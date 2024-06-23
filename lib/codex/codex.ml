open Caqti_request.Infix
module type DB = Caqti_lwt.CONNECTION
module R = Caqti_request
module T = Caqti_type
module E = Caqti_error
module Ccal = Caqti_type_calendar

(* lwt runner junk: move to a utils package *)
let run_caqti sql_uri callback =
        let%lwt result = Caqti_lwt_unix.with_connection (Uri.of_string sql_uri) callback in
        Caqti_lwt.or_fail result

(* Database Migration code *)
let to_caqti_query str = Caqti_query.of_string str |> Result.get_ok
let migrations = List.map to_caqti_query [
        "CREATE TABLE IF NOT EXISTS versions (
                codex_version INTEGER NOT NULL, 
                last_modified TEXT NOT NULL
        );";
        "CREATE TABLE IF NOT EXISTS meals (
                id INTEGER PRIMARY KEY ASC, 
                name TEXT UNIQUE,
                last_modified TEXT NOT NULL
        );";
        "CREATE TABLE IF NOT EXISTS plans (
                id INTEGER PRIMARY KEY ASC, 
                date TEXT UNIQUE NOT NULL,
                breakfast INTEGER references meals(id),
                lunch INTEGER references meals(id),
                dinner INTEGER references meals(id)
        );";
]

(* This was the only way I could figure out to capture this specific error enum *)
type no_version_err = [
        | `Request_failed of E.query_error
]
let migrate_target () =
        let max_db_version = List.length migrations - 1 in
        let table_exists_query =
                let open R.Infix in (T.string ->? T.string)
                "SELECT name from sqlite_schema WHERE type='table' and name=$1;" in
        let get_version_query =
                let open R.Infix in (T.unit ->! T.int)
                "SELECT codex_version FROM versions;" in
        fun (module Db: DB) ->
                let%lwt version_table_exists = Db.find_opt table_exists_query "versions" in
                let%lwt current_version = Db.find get_version_query () in
                let target = match Result.get_ok version_table_exists with None -> Some 0 (* special case for first migration step that creates this version table *)
                        | _ -> let version = Result.get_ok current_version in if version < max_db_version then (Some (version + 1)) else None 
                in
                Lwt.return target

(* Helper function to unwrap the result type that's the usual return of Caqti db functions and throw / terminate on any errors *)
let caqti_ok r =
        match%lwt r with 
                | Ok r_val -> r_val 
                | Error e -> E.show e |> failwith 
let caqti_ok_unit r = 
        match%lwt r with
                | Ok _ -> Lwt.return_unit
                | Error e -> E.show e |> failwith
       
let migrate_db version =
        let update_version_query =
                let open R.Infix in (T.(t2 int Ccal.ctime) -->. T.unit)
                (* This needs to be specalized for version 0 to insert into the table instead of updating *)
                (fun _ -> (match version with 
                                | 0 -> "INSERT INTO versions (codex_version, last_modified) VALUES ($1, $2);" 
                                | _ -> "UPDATE versions SET codex_version = $1, last_modified = $2;")
                           |> to_caqti_query)
        in
        let migrate_query =
                let open R.Infix in (T.unit -->. T.unit)
                (fun _ -> List.nth migrations version) in
        fun (module Db: DB) ->
                let%lwt _ = Db.exec migrate_query () |> caqti_ok_unit in
                let%lwt _ = Db.exec update_version_query (version, CalendarLib.Calendar.now ()) |> caqti_ok_unit in
                Lwt.return version

let migrate_to_latest () =
        let rec inner (module Db: DB) = 
                let%lwt target = (migrate_target ()) (module Db) in
                let _ = match target with Some i -> print_endline ("Upgrading to database version: " ^ (Int.to_string i))  | _ -> () in
                match target with 
                        | None -> print_endline "Database upgrade complete"; Lwt.return (Ok ())
                        | Some version -> 
                                let%lwt _ = (migrate_db version) (module Db) in inner (module Db);
        in
        inner

(* caqti meal index processing *)
let cmeal_index = T.(option string)
let cmeal_plan = 
        let encode (plan: Types.meal_plan) =
                (plan.date, plan.breakfast, plan.lunch, plan.dinner) in 
        let decode plan_tuple : Types.meal_plan = match plan_tuple with (date, breakfast, lunch, dinner) -> {date; breakfast; lunch; dinner}
        in
        T.(product decode @@ proj (t4 Ccal.cdate cmeal_index cmeal_index cmeal_index) encode @@ proj_end)

let get_meal_plan date = 
        let query = 
        let open R.Infix in (Ccal.cdate ->! cmeal_plan)
        "SELECT p.date, b.name, l.name, d.name 
         FROM plans as p 
         LEFT JOIN meals b ON b.id = p.breakfast 
         LEFT JOIN meals l ON l.id = p.lunch
         LEFT JOIN meals d ON d.id = p.dinner
         WHERE date = $1 LIMIT 1" in
        fun (module Db: DB) -> 
                let result = Db.find_opt query date in
                Lwt.bind result (fun u -> match u with
                                | Ok opt_u -> Lwt.return(Ok (Option.value opt_u ~default:{date=date; breakfast=None; lunch=None; dinner=None}))
                                | Error e -> Lwt.return(Error e))

let get_meal_plans dates = 
        fun (module Db: DB) ->
                (* TODO make custom query to increase speed if required *)
                let%lwt plans = Lwt_list.map_s (fun d -> (get_meal_plan d) (module Db)) dates in
                let rec inner l ret_l = 
                        match l with 
                                | [] -> Ok (List.rev ret_l)
                                | Error e :: tail -> Error e 
                                | Ok head :: tail -> inner tail (head :: ret_l)
                in
                Lwt.return (inner plans [])

let get_meal_id name = 
        let query =
        let open R.Infix in (T.(option string) ->? T.int)
        "SELECT id FROM meals WHERE name = $1;" in
        fun (module Db: DB) -> Db.find_opt query name 

let find_or_create_meal name =
        let query = 
        let open R.Infix in T.(t2 T.string Ccal.ctime ->. T.unit)
        "INSERT INTO meals (name, last_modified) VALUES ($1, $2);" in
        fun (module Db: DB) ->
                let%lwt meal_id = match%lwt (get_meal_id name) (module Db) with Ok id -> Lwt.return id | Error e -> failwith (E.show e) in
                match meal_id with
                        | Some id -> Lwt.return id (* early return of existing meal *)
                        | None -> (
                                        (* otherwise we're creating a new meal! *)
                                        let%lwt _ = Db.exec query (Option.get name, CalendarLib.Calendar.now ()) in 
                                        match%lwt (get_meal_id name) (module Db) with
                                                | Ok id -> Option.get id |> Lwt.return
                                                | Error e -> failwith (E.show e)
        )

let update_meal_plan (plan: Types.meal_plan) = 
        let query =
        let open R.Infix in (T.(t4 Ccal.cdate (option int) (option int) (option int)) ->. T.unit)
                "INSERT OR REPLACE INTO plans (date, breakfast, lunch, dinner) VALUES ($1, $2, $3, $4);" in
        fun (module Db: DB) ->
                let%lwt breakfast_id = (find_or_create_meal plan.breakfast) (module Db) in
                let%lwt lunch_id = (find_or_create_meal plan.lunch) (module Db) in
                let%lwt dinner_id = (find_or_create_meal plan.dinner) (module Db) in
                Db.exec query (plan.date, Some breakfast_id, Some lunch_id, Some dinner_id)

(* Todo: this could be smarter & search for fragment strings inside *)
let search_for_meals search_str =
        let query = 
        let open R.Infix in (T.string ->* T.string)
                "SELECT name from meals WHERE name LIKE $1"
        in
        fun (module Db: DB) -> Db.collect_list query (search_str ^ "%")

