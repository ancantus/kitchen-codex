open OUnit2
open OUnitLwt

(* OUnit2 equality assert for the custom meal_plan type *) 
let assert_meal_plan_equal = assert_equal ~cmp:Types.meal_plan_eq ~printer:Types.meal_plan_to_str

(* Empty plan specialization *)
let empty_meal_plan date = {Types.date=date; Types.breakfast=None; Types.lunch=None; Types.dinner=None}
let assert_meal_plan_empty date = assert_meal_plan_equal (empty_meal_plan date)


module type DB = Caqti_lwt.CONNECTION
let run_caqti_test callback =
        let uuid = (Uuidm.v4_gen (Random.State.make_self_init ())) () in
        let uri = Printf.sprintf "sqlite3:%s?mode=memory" (Uuidm.to_string uuid) in
        Codex.run_caqti uri (fun (module Db: DB) -> 
                let%lwt _ = (Codex.migrate_to_latest ()) (module Db) in
                callback (module Db : DB)
        ) 

let tests_caqti_codex = "Caqti codex refactor" >::: [
        "load empty meal plan" >:: (lwt_wrapper
                (fun _ -> 
                        let date = CalendarLib.Date.make 2024 1 1 in
                        let%lwt meal_plan = run_caqti_test (Codex.get_meal_plan date) in
                        Lwt.return (assert_meal_plan_empty date meal_plan)
                )
        );
        "add meal plan" >:: (lwt_wrapper
                (fun _ -> 
                        let date = CalendarLib.Date.make 2024 1 1 in
                        let expected_meal_plan = {Types.date=date; Types.breakfast=Some "Groats"; Types.lunch=Some "Ham Sandwich"; Types.dinner=Some "Tacos"} in
                        let%lwt added_meal_plan = run_caqti_test (fun (module Db: DB) -> 
                                let%lwt _ = (Codex.update_meal_plan expected_meal_plan) (module Db) in
                                (Codex.get_meal_plan expected_meal_plan.date) (module Db))
                        in
                        Lwt.return (assert_meal_plan_equal expected_meal_plan added_meal_plan)
                )
        );
]


let codex_tests = "codex test suite" >::: [
        tests_caqti_codex;
]
