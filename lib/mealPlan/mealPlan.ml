open Tyxml

let%html render_date date =
        "<td>"[Html.txt (CalendarLib.Printer.Date.sprint "%A" date)]"</td><td>"[Html.txt (CalendarLib.Printer.Date.sprint "%d/%m/%Y" date)]"</td>"

let%html render_meal meal_type meal_name = {|
        <td>
                <input 
                        name=|}meal_type{| 
                        type="search"
                        value=|}(Option.value meal_name ~default:""){|
                        data-hx-post="meal/search"
                        data-hx-trigger="input changed delay:500ms, search"
                        data-hx-target="next .search-results"
                >
                <div class="search-results"></div>
        </td>
        |}

let meal_plan_url date = "/meal-plan/" ^ (CalendarLib.Printer.Date.to_string date) 

let%html render_edit_button date = {|
        <td>
                <button class="btn btn-danger"
                        data-hx-target="closest tr"
                        data-hx-swap="outerHTML"
                        data-hx-include="closest tr"
                        data-hx-patch=|} (meal_plan_url date) 
                {|>Save</button>
        </td>|}

let%html render_meal_plan_row (meal_plan:Types.meal_plan) =
                "<tr>" (render_date meal_plan.date 
                        @ [ 
                                render_meal "breakfast" meal_plan.breakfast; 
                                render_meal "lunch" meal_plan.lunch; 
                                render_meal "dinner" meal_plan.dinner;
                                render_edit_button meal_plan.date;
                        ]) "</tr>"

let%html render_meal_plan_table meal_plan_list = {|
	<table class="table">
		<thead>
			<tr>
                                <th><!-- Day of the Week --></th>
				<th>Date</th>
				<th>Breakfast</th>
				<th>Lunch</th>
				<th>Dinner</th>
				<th><!-- Editing Controls --></th>
			</tr>
		</thead>
                <tbody>|}
			(List.map render_meal_plan_row meal_plan_list)
                {|</tbody>
        </table>|}


let date_list start_date end_date =
        let rec inner inc stop dates =
               match dates with
                        | [] -> [stop] (* Without a start there's nowhere to stop *)
                        | head :: _ -> if CalendarLib.Date.equal head stop 
                                then dates
                                else inner inc stop (inc head `Day :: dates)
        in
        let range_length = CalendarLib.Date.sub end_date start_date in
        if (CalendarLib.Date.Period.nb_days range_length) > 0
                then inner CalendarLib.Date.next end_date [start_date]
                else inner CalendarLib.Date.prev end_date [start_date]

let meal_plan_table start_date end_date =
        let dates = date_list start_date end_date in
        render_meal_plan_table (List.map Codex.load_meal_plan dates)


let parse date fields =
        let validate_meal meal_str =
                match String.trim meal_str with
                | "" -> None
                | s -> Some s
        in
        let find_meal name =
                match List.find_opt (fun (meal_name, _) -> String.equal meal_name name) fields with 
                        | None -> None
                        | Some (_, meal_str) -> validate_meal meal_str
        in
        {
                Types.date=date; 
                Types.breakfast=find_meal "breakfast"; 
                Types.lunch=find_meal "lunch"; 
                Types.dinner=find_meal "dinner";
        }

let update plan =
        Codex.update_meal_plan plan |> render_meal_plan_row

