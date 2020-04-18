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

        let _ =
          try
          let in_stream = open_in Sys.argv.(4) in
            let lexbuf = Lexing.from_channel in_stream in
              while true do
              let input = Parser.main Lexer.token lexbuf in  (*Here input represents the integer value that is returned by Parser*)
                print_newline();
          	done
          with Lexer.Eof ->
            exit 0
