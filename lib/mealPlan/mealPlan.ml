open Tyxml

let date_str = CalendarLib.Printer.Date.to_string 

let meal_plan_url date = "/meal-plan/" ^ (date_str date)

let%html render_date date = {|
        <td>|} [Html.txt (CalendarLib.Printer.Date.sprint "%A" date)] {|</td><td>|} [Html.txt (CalendarLib.Printer.Date.sprint "%d/%m/%Y" date)] {|</td>|}

let%html render_meal_suggestion patch_url category meal_name = {|<div class="search-item"
                data-hx-target="closest tr"
                data-hx-trigger="click"
                data-hx-swap="outerHTML"
                data-hx-include="closest tr"
                data-hx-vals=|} ("{\"" ^ category ^ "\":\"" ^ meal_name ^ "\"}") {|
                data-hx-patch=|} patch_url {|
        >|}[Html.txt meal_name]{|</div>|}

let%html render_meal_search_result date category meals = {| 
        <div class=|} ((if List.is_empty meals then ["hidden"] else []) @ ["search-results"]) {|

        > |}
          (List.map (render_meal_suggestion (match date with None -> "INVALID" | Some d -> (meal_plan_url d)) category)  meals )
        {|</div>|}

let%html render_meal date meal_type meal_name= {|
        <td>
                <input 
                        name=|}meal_type{| 
                        class="search-input"
                        type="search"
                        value=|}(Option.value meal_name ~default:""){|
                        data-hx-get="meal-plan/meal-search"
                        data-hx-params="*"
                        data-hx-trigger="input changed delay:500ms, search"
                        data-hx-target="next .search-results"
                        data-hx-swap="outerHTML"
                        data-hx-vals=|} ("{\"date\":\"" ^ (date_str date) ^ "\"}") {|
                >
                |} [(render_meal_search_result None "" [])] {|
        </td>
        |}


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
                                render_meal meal_plan.date "breakfast" meal_plan.breakfast; 
                                render_meal meal_plan.date "lunch" meal_plan.lunch; 
                                render_meal meal_plan.date "dinner" meal_plan.dinner;
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

