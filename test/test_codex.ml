open OUnit2

(* OUnit2 equality assert for the custom meal_plan type *) 
let assert_meal_plan_equal = assert_equal ~cmp:Types.meal_plan_eq ~printer:Types.meal_plan_to_str

(* Empty plan specialization *)
let empty_meal_plan date = {Types.date=date; Types.breakfast=None; Types.lunch=None; Types.dinner=None}
let assert_meal_plan_empty date = assert_meal_plan_equal (empty_meal_plan date)


let tests_load_meal_plan = "meal plan test suite" >::: [
        "load empty plan" >:: (fun _ -> 
                Codex.clear (); 
                let date = CalendarLib.Date.make 2024 1 1 in 
                assert_meal_plan_empty date (Codex.load_meal_plan date));
        "add meal plan" >:: (fun _ -> 
                Codex.clear ();
                let date = CalendarLib.Date.make 2024 1 1 in
                let expected_meal_plan = {Types.date=date; Types.breakfast=Some "Groats"; Types.lunch=Some "Ham Sandwich"; Types.dinner=Some "Tacos"} in
                assert_meal_plan_equal expected_meal_plan (Codex.update_meal_plan expected_meal_plan));
]

let codex_tests = "codex test suite" >::: [
        tests_load_meal_plan
]
