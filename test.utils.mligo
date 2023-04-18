#include "schema.mligo"
#include "error.mligo"
#include "utils.mligo"

let print_checkmark (given, expected : bool * bool) = 
    Test.print (if given=expected then "  v_true" else "  x_false")

let print_step (toPrint : string) =
    Test.println(" : " ^ toPrint) 

let print_topic (toPrint : string) =
    Test.println (toPrint)

let test_entrypoint (name : string) (a : test_exec_result) (expected : bool) = 
    match a with
        | Success gas ->  let _ = print_checkmark (true, expected) in
                          let _ = print_step name in gas
        | Fail err  ->  let _ = print_checkmark (false, expected) in
                        let _ = print_step name in
                        let _ = Test.print "      error_detected The previous test shows the following error : " in
                        let _ = Test.log err in 0n
