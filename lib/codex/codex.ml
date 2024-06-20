let parse_str_op stmt column =
        Some (Sqlite3.column_text stmt column)

(* Simple fire and forget DB execution when only success / failure is needed *)
let rec checked_exec db sql =
        match Sqlite3.exec db sql with
                | Sqlite3.Rc.OK -> ()
                | Sqlite3.Rc.BUSY -> checked_exec db sql
                | err -> Sqlite3.Rc.check err

(* More complicated DB execution with overridable parsed return vals and bind data *)
let bind_exec db ?ret ?bind sql =
        let stmt = Sqlite3.prepare db sql in
        let rec checked_bind i bind_val = 
                match Sqlite3.bind stmt i bind_val with
                        | Sqlite3.Rc.OK -> ()
                        | Sqlite3.Rc.BUSY -> checked_bind i bind_val
                        | fault -> failwith ("Unable to bind " ^ (Int.to_string i) ^ "'th statement in " ^ sql ^ " (" ^ (Sqlite3.Rc.to_string fault) ^ ")")
        in
        let rec bind_all i l = 
                match l with 
                        | [] -> ()
                        | head :: tail -> checked_bind i head; bind_all (i+1) tail
        in 
        bind_all 1 (Option.value bind ~default:[]);
        let rec step stmt = match Sqlite3.step stmt with
                | Sqlite3.Rc.BUSY -> step stmt
                | r -> r in
        let rec build_results f r =
                match step stmt with
                        | Sqlite3.Rc.DONE -> (Sqlite3.Rc.DONE, r)
                        | Sqlite3.Rc.ROW -> build_results f ((f stmt) :: r)
                        | fault -> (fault, r)
        in 
        let return_values = match ret with None -> (step stmt, []) | Some f -> build_results f [] in
        let _ = Sqlite3.finalize stmt in  (* Finalize always succeeds: and the return code is the same as already packed ret_val *)
        return_values

let read_version db =
        try (
                let (rc, versions) = bind_exec db ?ret:(Some (fun stmt -> let i = Sqlite3.column_int stmt 0 in print_int i; i)) "SELECT codex_version FROM versions LIMIT 1;" in
                match rc with
                        |  Sqlite3.Rc.DONE -> Some (List.hd versions)
                        | _ -> None
        ) with 
                | Sqlite3.Error _ -> None

let timestamp_now () =
        let time = CalendarLib.Time.now () |> CalendarLib.Time.to_gmt |> CalendarLib.Printer.Time.to_string in
        let date = CalendarLib.Date.today () |> CalendarLib.Printer.Date.to_string in
        date ^ "T" ^ time ^ "Z"

let migrate_schema db version =
        print_endline ("Updating to version " ^ (Int.to_string version));
        let checked_exec = checked_exec db in
        let update_version num = checked_exec ("UPDATE versions SET codex_version = " ^ (Int.to_string num) ^ ", last_modified = \"" ^ (timestamp_now ()) ^ "\";") in
        let rec migrate v = 
                match v with
                        | 0 -> checked_exec "CREATE TABLE IF NOT EXISTS versions (
                                        codex_version INTEGER NOT NULL, 
                                        last_modified TEXT NOT NULL
                                );"; 
                                checked_exec ("INSERT INTO versions (codex_version, last_modified) VALUES (0, \"" ^ (timestamp_now ()) ^ "\");");
                                migrate 1 
                        | 1 -> checked_exec "CREATE TABLE IF NOT EXISTS meals (
                                        id INTEGER PRIMARY KEY ASC, 
                                        name TEXT UNIQUE,
                                        last_modified TEXT NOT NULL
                                );"; update_version 1; migrate 2
                        | 2 -> checked_exec "CREATE TABLE IF NOT EXISTS plans (
                                        id INTEGER PRIMARY KEY ASC, 
                                        date TEXT UNIQUE NOT NULL,
                                        breakfast INTEGER references meals(id),
                                        lunch INTEGER references meals(id),
                                        dinner INTEGER references meals(id)
                                );"; update_version 2; migrate 3
                        | _ -> ()
        in
        migrate version

let open_db () =
        let db = Sqlite3.db_open "/tmp/test.db" in
        (match read_version db with 
                | None -> migrate_schema db 0 
                | Some n -> migrate_schema db (n + 1));
        db

let find_meal_plan_index db date =
        let (_, indexes) = bind_exec db 
                ?ret:(Some (fun stmt -> Sqlite3.column_int64 stmt 0)) 
                ?bind:(Some [Sqlite3.Data.TEXT(CalendarLib.Printer.Date.to_string date)])
                "SELECT id FROM plans WHERE date = ? LIMIT 1" in
        match indexes with
                | [] -> None
                | index :: _ -> print_endline ("Index: " ^ (Int64.to_string index)); Some index 

let read_meal_plan db date = 
        let empty_meal_plan date = {Types.date=date; Types.breakfast=None; Types.lunch=None; Types.dinner=None} in
        let parse_meal_plan stmt = {Types.date=date; Types.breakfast=parse_str_op stmt 1; Types.lunch=parse_str_op stmt 2; Types.dinner=parse_str_op stmt 3;} in 
        let (_, meal_plans) = bind_exec db
                ?ret:(Some parse_meal_plan)
                ?bind:(Some [Sqlite3.Data.TEXT(CalendarLib.Printer.Date.to_string date)])
                "SELECT p.date, b.name, l.name, d.name 
                        FROM plans p
                        LEFT JOIN meals b ON b.id = p.breakfast
                        LEFT JOIN meals l ON l.id = p.lunch
                        LEFT JOIN meals d ON d.id = p.dinner
                        WHERE date = ? 
                        LIMIT 1" in
        match meal_plans with
                | [] -> empty_meal_plan date
                | meal_plan :: _ -> meal_plan

let create_meal db meal =
        let (rc, _) = bind_exec db 
                ?bind:(Some [Sqlite3.Data.TEXT meal; Sqlite3.Data.TEXT (timestamp_now ())])
                "INSERT INTO meals (name, last_modified) VALUES (?, ?);" in
        match rc with
                | Sqlite3.Rc.DONE -> Sqlite3.last_insert_rowid db
                | fault -> failwith ("Unable to create meal: " ^ meal ^ ": " ^ (Sqlite3.Rc.to_string fault))

let find_or_create_meal db meal =
        let (_, meals) = bind_exec db
                ?ret:(Some (fun stmt -> Sqlite3.column_int64 stmt 0))
                ?bind:(Some [Sqlite3.Data.TEXT meal])
                "SELECT id FROM meals WHERE name = ?;" in
        match meals with
                | [] -> create_meal db meal
                | meal_id :: _ -> meal_id 

let write_meal_plan db (plan:Types.meal_plan) =
        let existing_plan = find_meal_plan_index db plan.date in 
        let meal_bind meal_name =
                match meal_name with
                        | Some name -> Sqlite3.Data.INT (find_or_create_meal db name) 
                        | None -> Sqlite3.Data.NULL
        in
        let date_str = CalendarLib.Printer.Date.to_string plan.date in
        let (rc, _) = bind_exec db
                ?bind:(Some ([
                                Sqlite3.Data.TEXT date_str;
                                meal_bind plan.breakfast;
                                meal_bind plan.lunch;
                                meal_bind plan.dinner;
                ] @ (match existing_plan with Some p -> [Sqlite3.Data.INT p] | None -> [])))
                (match existing_plan with
                        | None -> "INSERT INTO plans (date, breakfast, lunch, dinner) VALUES (?, ?, ?, ?);"  
                        | Some _ -> "UPDATE plans SET date = ?, breakfast = ?, lunch = ?, dinner = ? WHERE id = ?;") in
        match rc with
                | Sqlite3.Rc.DONE -> ()
                | _ -> failwith ("Unable to write meal plan " ^ date_str)

(* TODO: use sqlite's FTS5 virtual table to accelerate this *)
let fuzzy_match_meals db query =
        let (_, meals) = bind_exec db
                ?ret:(Some (fun stmt -> Sqlite3.column_text stmt 0))
                ?bind:(Some [Sqlite3.Data.TEXT ("%" ^ query ^ "%")])
                "SELECT name FROM meals WHERE name LIKE ?;" in
        meals

let load_meal_plan date =
        let db = open_db () in
        let meal_plan = read_meal_plan db date in
        let _ = Sqlite3.db_close db in
        meal_plan
(*
let update_meal_plan plan =
        let db = open_db () in
        write_meal_plan db plan;
        let updated_plan = read_meal_plan db plan.date in
        let _ = Sqlite3.db_close db in
        updated_plan
*)
let search_meal search_str =
        let db = open_db () in
        let canidate_meals = fuzzy_match_meals db search_str in
        let _ = Sqlite3.db_close db in
        canidate_meals

let clear () = 
        let db = open_db () in
        let checked_exec = checked_exec db in
        checked_exec "DELETE FROM meals;";
        checked_exec "DELETE FROM plans;";
        let _ = Sqlite3.db_close db in
        ()

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
                print_endline ("Upgrading to target: " ^ (match target with Some i -> Int.to_string i | None -> "Done!!"));
                match target with 
                        | None -> Lwt.return_unit 
                        | Some version -> let%lwt _ = (migrate_db version) (module Db) in inner (module Db);
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
 
