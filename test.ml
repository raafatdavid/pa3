open Compile
open Runner
open Printf
open OUnit2
open MyTests

let forty_one = "sub1(42)"
let forty = "sub1(sub1(42))"
let def_x = "let x = 5 in x"
let def_x2 = "let x = 5 in sub1(x)"
let def_x3 = "let x = 5 in let x = 67 in sub1(x)"
let addnums = "5 + 10"
let single_nest = "(5 + 2) - 1"
let negatives = "(0 - 5) + (0 - 10)"
let negatives2 = "(0 - 1) + (0 - 1)"
let nested_add = "(5 + (10 + 20))"
let let_nested = "let x = (5 + (10 + 20)) in x * x"

let if_simple_true = "if true: 1 else: 2"
let if_simple_false = "if false: 1 else: 2"
let nested_if = "if if false: 1 else: true: 11 else: 2"

let greater_of_equal = "4 > 4"
let less_of_equal = "4 < 4"
let greater = "4 > 3"
let less = "3 < 4"
let not_greater = "2 > 3"
let not_less = "3 < 2"

let equal = "(0 - 2) == (0 - 2)"
let not_equal = "(0 - 1) == (0 - 2)"

let add_true_left   = "true + 4"
let add_true_right  = "1 + true"
let add_false_left  = "false + 4"
let add_false_right = "1 + false"

let overflow = "1073741823 + 2"
let underflow = "(0 - 1073741823) - 2"

let print = "print(add1(5))"

let err_if_simple_true = "if 0: 1 else: 2"
let err_if_simple_false = "if 54: 1 else: 2"
let err_nested_if = "if if 54: 1 else: 0: 11 else: 2"

let ibt = "isbool(true)"
let ibf = "isbool(false)"
let intr = "isnum(true)"
let infalse = "isnum(false)"
let ibz = "isbool(0)"
let ib1 = "isbool(1)"
let ibn1 = "isbool(-1)"
let inz = "isnum(0)"
let in1 = "isnum(1)"
let inn1 = "isnum(-1)"

let call1 = "
def f(x):
  x + 1 
f(1)
"

let call2 = "
def g(y):
  y
def f(x, y):
  g(x)
f(1, 2)
"

let fact1 = "
def fact(n):
  if n < 1: 1
  else: n * fact(n - 1)

fact(4)
"

let calls = [
  t "call1" call1 "2";
  t "call2" call2 "1";
  t "fact1" fact1 "24";
]


let reg =
 [t "def_x" def_x "5";
  t "def_x2" def_x2 "4";
  t "def_x3" def_x3 "66";
  t "addnums" addnums "15";
  t "single_nest" single_nest "6";
  t "negatives" negatives "-15";
  t "negatives2" negatives2 "-2";
  t "nested_add" nested_add "35";
  t "let_nested" let_nested "1225";
  t "if_simple_true" if_simple_true "1";
  t "if_simple_false" if_simple_false "2";
  t "nested_if" nested_if "11";

  t "greater_of_equal" greater_of_equal "true";
  t "less_of_equal" less_of_equal "true";
  t "greater" greater "true";
  t "less" less "true";
  t "not_greater" not_greater "false";
  t "not_less" not_less "false";

  t "equal" equal "true";
  t "not_equal" not_equal "false";

  t "print" print "6\n6";

  t "ibt" ibt "true";
  t "ibf" ibf "true";
  t "intrue" intr "false";
  t "infalse" infalse "false";
  t "ibz" ibz "false";
  t "ib1" ib1 "false";
  t "ibn1" ibn1 "false";
  t "inz" inz "true";
  t "in1" in1 "true";
  t "inn1" inn1 "true";

  terr "add_true_left"   add_true_left   "arithmetic operator got boolean";
  terr "add_true_right"  add_true_right  "arithmetic operator got boolean"; 
  terr "add_false_left"  add_false_left  "arithmetic operator got boolean";
  terr "add_false_right" add_false_right "arithmetic operator got boolean";

  terr "err_if_simple_true" err_if_simple_true "if condition got number";
  terr "err_if_simple_false" err_if_simple_false "if condition got number";
  terr "err_nested_if" err_nested_if "if condition got number";

  terr "overflow" overflow "overflow";
  terr "underflow" underflow "overflow";
  ]
;;

let errs = [
  terr "unbound" "x" "Unbound";

  terr "duplicate" "def f(x, x): x\n9" "Duplicate";

  terr "duplicate_let" "let x = 10, x = 5 in x" "Duplicate";
  terr "duplicate_let2" "let x = 10, y = 7, x = 5 in x" "Duplicate";
  terr "duplicate_let3" "let x = 10, y = 7, y = 5 in x" "Duplicate";

  terr "duplicate_fun" "def f(): 5\n def f(): 10\nf()" "Duplicate";

  terr "arity" "def f(): 5\nf(1)" "Arity";
]

let suite =
"suite">:::
  calls @ reg @ errs @ myTestList

let () =
  run_test_tt_main suite
;;

