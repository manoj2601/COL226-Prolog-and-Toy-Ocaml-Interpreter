(* let rows = 10;;
let cols = 10;;

let rec arr_init  arr rows cols =
	for i=0  to (rows-1) do
			for j=0 to (cols-1) do
				Array.set arr.(i) j ( (float_of_int i) +. (float_of_int j));
			done;
			(* Array.set arr i irow; *)
done
;; *)

open Backend

let table =  (*Copied from rachit*)
    let filename = Sys.argv.(1) in 
    let file = open_in filename in
    let lexbuf = Lexing.from_channel file in
    let rec createTable acc = 
      let result = Parser.main Lexer.token lexbuf in
        match result with 
        (Node(("file_end",0),[]),[]) -> acc
        | _ -> (createTable (result::acc)) 
      in 
    (printTable(createTable []));
    (createTable [])
;;

let _ =  (*copied from rachit*)
  Printf.printf "?-"; flush stdout;
  let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
        match result with 
        (Node(("exit",0),[]),[]) -> Printf.printf "EXITING\n";flush stdout; exit 0
        |(goal,[]) ->( 
          let t = solution table (goal, []) in
          Printf.printf "EXITING2\n";flush stdout; exit 0
          (* let targets = (getTargets goal) in
          match (solve 100 table (true,[]) [goal] targets false) with 
          (true,sl) -> Printf.printf "\n?-"; flush stdout;
          |(false,sl) -> Printf.printf "false.\n";Printf.printf "\n?-"; flush stdout; *)
        ) 
        | _-> Printf.printf "INVALID INPUT GOAL\n";Printf.printf "\n?-"; flush stdout;
    done
;;



        (* let _ =
          Printf.printf "?-"; flush stdout;
          let lexbuf = Lexing.from_channel stdin in
          while true do
            let result = Parser.main Lexer.token lexbuf in
            match result with
            | Node(("halt", []), []) -> Printf.printf "exiting!!"; flush stdout; exit 0
            | (goal, []) -> (
              let targets = (getTargets goal) in
              match (solve 100 table (true,[]) [goal] targets false) with 
              (true,sl) -> Printf.printf "\n?-"; flush stdout;
              |(false,sl) -> Printf.printf "false.\n";Printf.printf "\n?-"; flush stdout;
            ) 

            
          try
          let in_stream = open_in Sys.argv.(4) in
            let lexbuf = Lexing.from_channel in_stream in
              while true do
              let input = Parser.main Lexer.token lexbuf in  (*Here input represents the integer value that is returned by Parser*)
                print_newline();
          	done
          with Lexer.Eof ->
            exit 0 *)
