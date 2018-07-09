open Assignment6
exception INVALID_INPUT_GOAL

let p x = match x with
  Node(a,b) -> a |
  V(a) -> a |
  _ -> raise INVALID_INPUT_GOAL;;

let rec printt l = match l with
  [] -> []
  | x::xs -> (p x)::(printt xs);;

let rec print_list  = function
  [] -> []
  | x::xs ->  (printt x)::(print_list xs);;

let rec mapxx l = match l with
    [] -> " "|
    x::xs -> let s = String.concat("") in s ([x;" ";(mapxx xs)]);;

let rec mapx l = match l with
    [] -> "" |
    x::xs -> let sx =  String.concat("") in sx ([(mapxx x);"\n";(mapx xs)]);; 

let rec mpx f l = match l with
  [] -> "" |
  x::[] -> let sx = String.concat("") in sx ([(f x)])|
  x::xs -> let sx = String.concat("") in sx ([(f x);";";(mpx f xs)]);;

let rec final a = match a with
  Node(s,l) -> let sx = String.concat("") in sx (["Node(";"\"";s;"\"";",[";(mpx final l);"])"])|
  V(a) -> let sx = String.concat("") in sx (["V(";"\"";a;"\"";")"])|  
  _ -> "Hala Madrid!";; 

let fin a = let sx = String.concat("") in sx ([(final a);"."]);;

let pr = 
  try 
    let input = Sys.argv.(1) in 
    let file = open_in input in
    let lexbuf = Lexing.from_channel file in
    let rec prog acc = 
      let claus = Parser.main Lexer.token lexbuf in
        match claus with (Node("file_end",[]),[]) -> acc
        | _ -> (prog (claus::acc)) 
      in 
     (prog [])
with Invalid_argument("index out of bounds") -> []
;;

let _ = if(List.length pr = 0) then (
     Printf.printf "?-"; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
    while true do
       let input = Parser.main Lexer.token lexbuf in
          match input with 
            (goal,[]) -> (match goal with
               Node(s,l) -> print_string (fin goal);Printf.printf "\n?-"; flush stdout;
               | _-> raise INVALID_INPUT_GOAL)
            | _-> raise INVALID_INPUT_GOAL
    done
  ) else (
    Printf.printf "?-"; flush stdout;
    let lexbuf = Lexing.from_channel stdin in
      while true do
        let input = Parser.main Lexer.token lexbuf in
          match input with 
          (goal,l) ->(
            match (execute (goal::l) pr ) with 
            (true,sl,li) ->   (if(List.length li > 0 && List.length (List.nth li 0) > 0) then print_string (mapx  (print_list li)) else (Printf.printf "true.\n")); Printf.printf "\n?-";  flush stdout;  
            |(false,sl,li) ->   Printf.printf "false.\n";Printf.printf "\n?-"; flush stdout; 
          ) 
          | _-> raise INVALID_INPUT_GOAL 
      done )
;;


