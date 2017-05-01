open Runner
(** Fill in myTestList with your own tests.

    There are two ways to do so:
   
    1) To test that a program produces the desired result: 

       t <name of test> <code> <expected result>

    e.g.
      
      t "Add" "1+1" "2"
 
    2) To test that a program throws an expected error:
  
      terr "Error" "1+true" "arithmetic operator got boolean"
**)
let myTestList = 
 [ (* Fill in your tests here: *)
   terr "TestError" "1+true" "arithmetic operator got boolean"
 ]
;;
