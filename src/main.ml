let filename = "rpn_expression-5869168714501555882.txt"

(* read lines from file *)
let read_lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

(* Running stack to hold the numbers *)
let running_stack = Stack.create ()

(* iter_expr takes a char and pushes it onto the stack*)
let iter_expr ch =
  match ch with
  | ' ' -> ()
  | '-' ->
      Stack.push
        (Stack.pop running_stack - Stack.pop running_stack)
        running_stack
  | '+' ->
      Stack.push
        (Stack.pop running_stack + Stack.pop running_stack)
        running_stack
  | '*' ->
      Stack.push
        (Stack.pop running_stack * Stack.pop running_stack)
        running_stack
  | '/' ->
      Stack.push
        (Stack.pop running_stack / Stack.pop running_stack)
        running_stack
  | num -> Stack.push (int_of_char num - int_of_char '0') running_stack
;;

(* iterate over all lines *)
List.iter (fun expr -> String.iter iter_expr expr) @@ read_lines filename ;;

Printf.printf "\nAnswer: %d\n" @@ Stack.top running_stack
