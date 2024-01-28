open Tyxml

let%html render_meal_plan start_date end_date = {|
  <html>
  <head><title>The Meal Plan</title></head>
  <body>
<h1>Meal Plan:</h1>|}
  [MealPlan.meal_plan_table start_date end_date]
{|</body>
  </html>|}

let static_loader _root path _request =
  match Static.read path with
    | None -> Dream.empty `Not_Found
    | Some resource -> Dream.respond resource

let html_to_string html =
  Format.asprintf "%a" (Tyxml.Html.pp ()) html

let parse_date date = 
  match date with
    | None -> failwith "Date not specified."
    | Some date_str -> CalendarLib.Printer.Date.from_fstring "%Y-%m-%d" date_str

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/static/**" (Dream.static ~loader:static_loader "");
    Dream.get "/meal-plan" (fun request ->
      let s = Dream.query request "start" |> parse_date in
      let e = Dream.query request "end" |> parse_date in
      render_meal_plan s e |> html_to_string |> Dream.html);
  ]
