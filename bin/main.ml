open Tyxml

let%html render_meal_plan meal_plans = {|
  <html>
  <head>
    <title>The Meal Plan</title>
    <script src="static/htmx.1.9.10.min.js"></script>
    <link rel="stylesheet" href="static/main.css">
  </head>
  <body>
<h1>Meal Plan:</h1>|}
  [MealPlan.render_meal_plan_table meal_plans]
{|</body>
  </html>|}

let static_loader _root path _request =
  match Static.read path with
    | None -> Dream.empty `Not_Found
    | Some resource -> Dream.respond resource

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html

let elt_to_string elt =
  Format.asprintf "%a" (Tyxml.Html.pp_elt ()) elt 

let parse_date date_str = CalendarLib.Printer.Date.from_fstring "%Y-%m-%d" date_str

let parse_opt_date date = 
  match date with
    | None -> failwith "Date not specified."
    | Some date_str -> parse_date date_str

let db_uri = "sqlite3:/tmp/codex.sql" 

let () =
  Lwt_main.run (Codex.run_caqti db_uri (Codex.migrate_to_latest ()));
  Dream.run
  @@ Dream.logger
  @@ Dream.sql_pool db_uri
  @@ Dream.router [
    Dream.get "/static/**" (Dream.static ~loader:static_loader "");
    Dream.get "/meal-plan" (fun request ->
      let s = Dream.query request "start" |> parse_opt_date in
      let e = Dream.query request "end" |> parse_opt_date in
      match%lwt MealPlan.date_list s e |> Codex.get_meal_plans |> Dream.sql request with
        | Ok meal_plans -> render_meal_plan meal_plans |> html_to_string |> Dream.html
        | Error e -> failwith (Caqti_error.show e)
    );
    Dream.get "/meal-plan/meal-search" (fun request ->
      let date = Dream.query request "date" |> parse_opt_date in
      let rec parse_query query =
        match query with 
          | ( "date", _ ) :: tail -> parse_query tail
          | ( category, search) :: _ -> Some (category, search)
          | [] -> None
      in
      match parse_query (Dream.all_queries request) with
        | Some (category, search_req) -> (match%lwt Codex.search_for_meals search_req |> Dream.sql request with
          | Ok (canidate_meals)-> MealPlan.render_meal_search_result (Some date) category canidate_meals |> elt_to_string |> Dream.html
          | Error _ -> Dream.empty `Internal_Server_Error)
        | _ -> Dream.empty `Bad_Request
    ); 
    Dream.patch "/meal-plan/:date" (fun request ->
      let date = Dream.param request "date" |> parse_date in
      match%lwt Dream.form ?csrf:(Some false) request with
        | `Ok fields -> (match%lwt Dream.sql request (Codex.update_meal_plan (MealPlan.parse date fields)) with
          | Error _ -> Dream.empty `Internal_Server_Error
          | Ok _ -> (match%lwt Dream.sql request (Codex.get_meal_plan date) with
            | Error _ -> Dream.empty `Internal_Server_Error
            | Ok meal_plan -> MealPlan.render_meal_plan_row meal_plan |> elt_to_string |> Dream.html
            )
        )
        | _ -> Dream.empty `Bad_Request
      );
  ]
