let parse_str_op stmt column =
        Some (Sqlite3.column_text stmt column)

let bind_exec db ?ret ?bind sql =
        let stmt = Sqlite3.prepare db sql in
        let checked_bind i bind_val = 
                match Sqlite3.bind stmt i bind_val with
                        | Sqlite3.Rc.OK -> ()
                        | fault -> failwith ("Unable to bind " ^ (Int.to_string i) ^ "'th statement in " ^ sql ^ " (" ^ (Sqlite3.Rc.to_string fault) ^ ")")
        in
        let rec bind_all i l = 
                match l with 
                        | [] -> ()
                        | head :: tail -> checked_bind i head; bind_all (i+1) tail
        in 
        bind_all 1 (Option.value bind ~default:[]);
        let rec build_results f r =
                match Sqlite3.step stmt with
                        | Sqlite3.Rc.DONE -> (Sqlite3.Rc.DONE, r)
                        | Sqlite3.Rc.ROW -> build_results f ((f stmt) :: r)
                        | fault -> (fault, r)
        in 
        let return_values = match ret with None -> (Sqlite3.step stmt, []) | Some f -> build_results f [] in
        match Sqlite3.finalize stmt with
                | Sqlite3.Rc.OK -> return_values
                | fault -> failwith ("Failed to close " ^ sql ^ " (" ^ (Sqlite3.Rc.to_string fault) ^ ")") 

let read_version db =
        try (
                let (rc, versions) = bind_exec db ?ret:(Some (fun stmt -> let i = Sqlite3.column_int stmt 0 in print_int i; i)) "SELECT codex_version FROM versions LIMIT 1;" in
                match rc with
                        |  Sqlite3.Rc.DONE -> print_endline ("Version: " ^ (Int.to_string (List.length versions))); Some (List.hd versions)
                        | _ -> None
        ) with 
                | Sqlite3.Error _ -> None

let timestamp_now () =
        let time = CalendarLib.Time.now () |> CalendarLib.Time.to_gmt |> CalendarLib.Printer.Time.to_string in
        let date = CalendarLib.Date.today () |> CalendarLib.Printer.Date.to_string in
        date ^ "T" ^ time ^ "Z"

let migrate_schema db version =
        print_endline ("Updating to version " ^ (Int.to_string version));
        let checked_exec sql = Sqlite3.Rc.check (Sqlite3.exec db sql) in
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

let update_meal_plan plan =
        let db = open_db () in
        write_meal_plan db plan;
        let updated_plan = read_meal_plan db plan.date in
        let _ = Sqlite3.db_close db in
        updated_plan

let search_meal search_str =
        let db = open_db () in
        let canidate_meals = fuzzy_match_meals db search_str in
        let _ = Sqlite3.db_close db in
        canidate_meals

let clear () = 
        let db = open_db () in
        let checked_exec sql = Sqlite3.Rc.check (Sqlite3.exec db sql) in
        checked_exec "DELETE FROM meals;";
        checked_exec "DELETE FROM plans;";
        let _ = Sqlite3.db_close db in
        ()

