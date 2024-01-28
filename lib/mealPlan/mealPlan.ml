open Tyxml

type meal_plan = {
	date: CalendarLib.Date.t;
	breakfast: string option;
	lunch: string option;
	dinner: string option;
}

let%html render_date date =
        "<td>"[Html.txt (CalendarLib.Printer.Date.sprint "%A" date)]"</td><td>"[Html.txt (CalendarLib.Printer.Date.sprint "%d/%m/%Y" date)]"</td>"

let%html render_meal meal_type meal_name =
        "<td><input name="meal_type" value="(Option.value meal_name ~default:"")"></td>"

let%html render_meal_plan_row meal_plan =
                "<tr>" (render_date meal_plan.date @ [ render_meal "breakfast" meal_plan.breakfast; render_meal "lunch" meal_plan.lunch; render_meal "dinner" meal_plan.dinner]) "</tr>"

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

let lookup_meal_plan date =
        {date; breakfast=None; lunch=None; dinner=None} 

let meal_plan_table start_date end_date =
        let dates = date_list start_date end_date in
        render_meal_plan_table (List.map lookup_meal_plan dates)

