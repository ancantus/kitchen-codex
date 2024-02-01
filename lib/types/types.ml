
(* Meal plan for a specific day *)
type meal_plan = {
	date: CalendarLib.Date.t;
	breakfast: string option;
	lunch: string option;
	dinner: string option;
}
