open Tokenizer
open Parser

module T = Tokenizer
module P = Parser
module A = Alcotest

let pp_expr ppf exp = Fmt.pf ppf "expression %s" (P.exp2string exp)
let myexpr = Alcotest.testable pp_expr P.exp_compare

let parse_test inp_s =
        match exp_p.run {tokens= ref (Array.of_list (tokenize inp_s)) ; pos= 0} with
        | _, Ok expr -> expr
        | _, Error _ -> Num 1
let checkparse e result = A.(check myexpr) e result (parse_test e)

let test_numbers () =
   checkparse "3" (Num 3);
   checkparse "$1251356125" (Num 78671864101);
   checkparse "1203*2+5*10" (Sum ( Mul ( (Num 1203), (Num 2) ), Mul ((Num 5), (Num 10))))

let test_braces() =

   checkparse "2+5*(-%0001001010+3)-8" (Sum(Num 2, Sub(Mul(Num 5, Sum(Min (Num 74), Num 3)), Num 8)));
   checkparse "(2+(5*3))-8" (Sub(Sum(Num 2, Mul(Num 5, Num 3)), Num 8))

let quickcase (descr, case) = A.test_case descr `Quick case 
let test_suite = List.map quickcase [
    ("parse numbers", test_numbers);
    ("parse braces", test_braces);
  ] 

let () = Printexc.record_backtrace true; A.run "parser" [
    "stack", [
      (* A.test_case "" `Quick test_xxx; *)
    ];
    "parser", test_suite;

    (* "miscellanous programs", Testprogram.test_suite; *)
  ]


