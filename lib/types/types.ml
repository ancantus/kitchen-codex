
(* Meal plan for a specific day *)
type meal_plan = {
	date: CalendarLib.Date.t;
	breakfast: string option;
	lunch: string option;
	dinner: string option;
}

let rec all l = 
	match l with
		| [] -> true
		| false :: _ -> false
		| true :: tail -> all tail

let meal_plan_eq lhs rhs =
	let meal_eq = Option.equal String.equal in
	all(
		[
			CalendarLib.Date.equal lhs.date rhs.date;
			meal_eq lhs.breakfast rhs.breakfast;
			meal_eq lhs.lunch rhs.lunch;
			meal_eq lhs.dinner rhs.dinner;
		])


let meal_plan_to_str plan =
	let opt_to_str opt = match opt with Some s -> s | None -> "None" in
	let date_to_str date = CalendarLib.Printer.Date.sprint "%A" date in
	Printf.sprintf "{meal_plan date: %s, breakfast: %s, lunch: %s, dinner: %s}" (date_to_str plan.date) (opt_to_str plan.breakfast) (opt_to_str plan.lunch) (opt_to_str plan.dinner) 
