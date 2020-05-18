(*This is the backend file of the program. All the functions are implemented here*)

(*Errors*)
exception InvalidInput
exception Error
exception Error1

type symbol = string*int;; (*prev - string, Now string*int(Basically pair of symbol, arity) *)
type variable = string;;
type arity = int;;
									type pair = (symbol*arity);;
type constant = string;; (*Also can be called atom*)
type term = V of variable
	| Node of symbol*(term list)
	| C of constant
	;;
	type clause = term * term list;; (*If term list size is zero, than clause is fact, else rule*)


	let rec printTable l = match l with x::xs -> (printClause x);Printf.printf "NEXT CLAUSE \n\n\n\n"; (printTable xs);
	| [] -> Printf.printf "EMPTY"
	and printClause c = match c with (ter, terl) -> Printf.printf "Clause Start XXXXXXXXXXX\n"; (printTerm ter); Printf.printf "\nXXXXXXXXX MAPPED TO \n"; (printTerms terl);
	and printTerms tl = match tl with [] -> Printf.printf ""; | x::xs -> (printTerm x);(Printf.printf "next term\n");(printTerms xs);
	and printTerm t = match t with V(str) ->  Printf.printf "Variable %s\n" str;
	|Node((str,integ),children) -> Printf.printf "Symbol %s,%d\n Children\n" str integ; (printTerms children);Printf.printf "ChildrenOver\n";
	;;

	let rec ithelement (i:int) l = (*ith element of the list*)
	match l with
	| [] -> raise InvalidInput
	| x::xs -> if i=0 then x else ithelement (i-1) xs
;;
let rec createVList (t:term) : term list =
		match t with
		| V(var) -> [V(var)]
		| Node(sym, tl) -> 
							(	List.fold_left (takeunion) [] (List.map createVList tl) )
		| _ -> []
		and takeunion (l1:term list) (l2:term list) = 
			(match l2 with
					| x::xs -> List.fold_left takeunion2 l1 l2
					(* if (takeunion2 l1 x) then takeunion l1 xs else takeunion (x::l1) xs *)
					| [] -> l1
			);
		and takeunion2 (l1:term list) (x:term) =
			(
				match l1 with 
				| y::ys -> if(x = y) then l1 else (y::(takeunion2 ys x));
				| [] -> [x]
			)
;;

type substitution = (variable*term) list;;
type substitutionR = bool*((variable*term) list);;
let rec subst (t:term) (s:substitution) : term =
	match t with
	| V (x) -> if (isvarinsub x s) then (termforvar x s) else V (x);
	| Node (sym, tl) -> let list = helper tl s in
						let t = Node (sym, list) in
						t
	and helper (tl:term list) (s:substitution) : term list =
	match tl with
	| [] -> []
	| x::xs -> (subst x s)::(helper xs s)

	and isvarinsub (v:variable) (s:substitution) : bool =
	match s with
	| [] -> false
	| x::xs -> match x with
			| (a,b) -> if a = v then true else isvarinsub v xs
	and termforvar (v:variable) (s:substitution) : term =
	match s with
	| [] -> raise Error1
	| x::xs -> match x with
				| (a,b) -> if a = v then b else termforvar v xs;
;;
let rec composition (subs: substitution list) : substitution =
	match subs with
	| [] -> []
	| x::xs ->
				let ret = ref [] in
				for i=0 to ((List.length subs) -1) do
					ret := compositionhelper2 (!ret) (ithelement i subs)
				done;

	generalizationsubs !ret

	and compositionhelper2 (s1:substitution) (s2:substitution) : substitution =
		let s = ref (compositionhelper s1 s2) in
		for i=0 to ((List.length s2) -1) do
			match (ithelement i s2) with
			| (var, t) -> if ((isvarinsub var !s) <> true) then s := (!s)@[(var, t)];
		done;
	!s
	and compositionhelper (s1: substitution) (s2: substitution) : substitution =
		match s1 with
		| [] -> []
		| x::xs -> match x with
				| (var, t) -> (var, subst t s2) :: compositionhelper xs s2

	and generalizationsubs (sub: substitution): substitution =
	match sub with
	| [] -> []
	| x::xs -> match x with
			| (var, t) -> if t <> V (var) then (var, t) :: generalizationsubs xs
							 else generalizationsubs xs
;;

let rec mguhelper (t1:term) (t2:term) (sub:substitution):substitutionR =
	let t11 = subst t1 sub in
	let t21 = subst t2 sub in
	if t11 <> t21 then begin
	match t11 with
	| V x -> 
			(match t21 with
			| V y -> 
			(if x <> y then begin
				if (isvarinsub x sub || isvarinsub y sub) then (false, []) else begin
				let p = (x, V y) in
				(true, (composition [sub;[p]]))
				end
				end
			else (true, sub)
			)

			| Node ((str2, l2), tl) -> (if (isvarinsub x sub) then (false,[]) else (
								let p = (x, Node ((str2, l2), tl)) in
								if (isvarinterm x t21) then (false, []) else 
								(true, (composition [sub;[p]]));)
									)
		)
	| Node ((str1,l1), tl) -> 
			(match t21 with
			| V y -> if (isvarinsub y sub) then (false, []) else (
					let p  = (y, Node ((str1, l1), tl)) in
					if (isvarinterm y t11) then (false, []) else
					(true, (composition [sub;[p]]));)
			| Node ((str2,l2), tl2) -> if (str1 <> str2 || l1 <> l2) then (false, []) else if
					 (List.length tl <> List.length tl2) then (false, []) else
									let ret = ref sub in
									let subR = ref true in
								  for i=0 to (List.length tl) -1 do
										let temp = mguhelper (ithelement i tl) (ithelement i tl2) (!ret) in
										match temp with (true, sub1) -> ret := sub1;
										| (false, sub1) -> subR := false; ret := sub1;
								done;
								if(!subR) then	(true, (!ret))
								else (false, [])
							)
	end
	else (true,sub)
	and isvarinterm (v:variable) (t:term) : bool =
		match t with
	| V a -> if v = a then true else false;
	| Node (sym, tl) -> 
			 (let isgot = ref false in 
			 for i=0 to (List.length tl-1) do
			 	let a = ithelement i tl in
			 	if (isvarinterm v a) then isgot := true;
			 	done;
				 if (!isgot = true) then true else false)
	| _ -> false
	;;

let rec unify (t1:term) (t2:term) (inisubs:substitutionR) : substitutionR =
		match inisubs with
		| (true, sl) -> mguhelper t1 t2 sl
		| _ -> (false, [])
	;;



let rec loopOnCList (inisubs:substitutionR) (table1: clause list) (g:term) (lgoals: term list) (table:clause list) =
	match table1 with
	| x::xs ->	(let ret = operationOnElement x g inisubs in
							match ret with (true, sub) -> (true, sub)
							| (false, sub) -> loopOnCList inisubs xs g lgoals table
							)
						(* call the function operationOnElement (x:clause) (g:term)
						if(return false) then loopOnCList inisubs xs g Lgoals;
						else return the updated inisubs *)
	| [] -> (false, [])
			(*Unify head(x) and g and return the bool result whether these are unifiable or not*)
and goalsSolve (lgoals:term list) (inisubs: subsitutionR) (table: clause list) (vset: term list) =
	match lgoals with
	| x::xs -> (
				let newsubs = loopOnCList inisubs table x xs in
				goalsSolve xs newsubs table vset;
		)
	| _ -> (true, inisubs)
	and operationOnElement (x:clause) (g:term) (inisubs: substitutionR) (table: clause list) : (substitutionR) =
		match x with
		| (hd, bdy) -> ( let newsub = unify hd g inisubs in
							match newsub with 
							| (true, sub) -> goalsSolve (List.append bdy lgoals) newsub table vset 
							| (false, sub) -> (false, [])
								(* if unifiable then update inisubs and update Lgoals with body of x, again solve for 1st element of Lgoals *)
		)
		;;
let solution (table: clause list) (goal : clause) = 
	(* Create a list of variables *)
	match goal with
	| (g, []) -> ( 
		let vset = createVList g in
		printTerms vset;
		(*List of variables vset created = [V(X); V(Y); V(Z)] *)
		let inisubs = (true, []) in
		let Lgoals = [g] in
		goalsSolve Lgoals inisubs table vset
		loopOnCList (inisubs:substitution) (table: clause list) (g:term) (Lgoals: term list);
		vset
	)
	| _ -> raise Error	
	;;
(* type atomicFormula = (symbol * term list);; *)

(* type body = atomicFormula list;; *)

(* type fact = term;;
type rule = term*term list;;           (atomicFormula*atomicFormula list);; *)


(*

type signature = pair list;;
type substitution = (variable*term) list;;
(* let rows = 10;;
let cols = 10;;

let arr = Array.make_matrix rows cols None;; *)


(*In case, if the output index is out of range, we can expand the sheet by this function.
it returns a sheet with extended dimenstions.
First it fills all the values of arr1 with None
Then it iterates to the all indices of the given sheet and copies all the values to the newly created sheet.
*)
let expandarray (arr: sheet) cols rows extracols extrarows : sheet =
	let arr1 = Array.make_matrix (rows+extrarows) (cols+extracols) None in
		for i=0 to (rows-1) do
		for j=0 to cols-1 do
			arr1.(i).(j) <- arr.(i).(j)
		done;
	done;
arr1;;

(* It initilize the sheet with the value of (i+j) in [i, j] index 
It iterates in the whole sheet and fills the corresponding values.
*)

let rec arr_init  (arr:sheet) (rows:int) (cols:int) =
	for i=0  to (rows-1) do
			for j=0 to (cols-1) do
				arr.(i).(j) <- Some ( (float_of_int i) +. (float_of_int j));
				(* print_string "Hello friends !!\n"; *)
				(* print_array arr; *)
			done;
	done
;;
(* It prints the sheet .
	It iterates in the whole sheet and print that value if it is not None.
	Else if prints "E" on this position.
 *)
let rec print_array (arr:sheet) =
	let rows = Array.length arr in
	let cols = Array.length arr.(0) in
	for i=0 to (rows-1) do
		for j=0 to cols-1 do
		if (is_none arr.(i).(j)) then print_string "E"
		else print_float (value arr.(i).(j)); 
		print_string " ";
	done;
	print_string "\n";
	done
;;

let rec print_sheet (arr:sheet) =
	let rows = Array.length arr in
	let cols = Array.length arr.(0) in
	for i=0 to (rows-1) do
		for j=0 to cols-1 do
		if (is_none arr.(i).(j)) then print_string ","
		else begin print_float (value arr.(i).(j)); print_string ","; end 
		(* print_string " "; *)
	done;
	print_string "\n";
	done
;;
(*Unary functions start*)
(* ROWCOUNT : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase count by 1. After interating each row, we assign the value of count to the corresponding output index.
 *)
let rec row_count (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput;
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1+c1-a1 > (rows - 1) then extrarows := i1 + c1-a1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=a1 to c1 do
			 	let count = ref 0.0 in
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then count := (!count +. 1.0);
			 	done;
			 	arr.(i1+ (!m)).(i2) <- Some (!count);
			 	count := 0.0;
			 	m := !m +1;
			 done;
arr ;;

(* FULLCOUNT : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase count by 1.
Finally we assign the value of count to arr.(i1).(i2);
 *)
let rec full_count (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
		(* 	 let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let count = ref 0.0 in
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then count := (!count +. 1.0)
			 		else count := !count
			 	done;
			 done;
			 arr.(i1).(i2) <- Some (!count);
arr ;;

(* COLCOUNT : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase count by 1. After interating in each column, we assign the value of count to the corresponding output index.
 *)
let rec col_count (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=b1 to d1 do
			 	let count = ref 0.0 in
			 	for j=a1 to c1 do
			 		if (is_some arr.(j).(i)) then count := (!count +. 1.0)
			 		else count := !count
			 	done;
			 	arr.(i1).(i2+ (!m)) <- Some (!count);
			 	count := 0.0;
			 	m := !m +1;
			 done;
arr ;;

(* FULLSUM : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value.
if no then we raise an exception.
Finally we assign the value of total to arr.(i1).(i2);
 *)
let rec full_sum (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let total = ref 0.0 in
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then total := (!total +. value arr.(i).(j))
			 	else raise InvalidInput;
			 	done;
			 done;
			 arr.(i1).(i2) <- Some (!total);
arr ;;

(* ROWSUM : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value. After interating each row, we assign the value of total to the corresponding output index.
If no, then we raise an exception.
 *)
let rec row_sum (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1+c1-a1 > (rows - 1) then extrarows := i1 + c1-a1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=a1 to c1 do
			 	let total = ref 0.0 in
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then total := (!total +. value arr.(i).(j))
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1+ (!m)).(i2) <- Some !total;
			 	total := 0.0;
			 	m := !m +1;
			 done;
arr ;;

(* COLSUM : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value. After interating each column, we assign the value of total to the corresponding output index.
If no, then we raise an exception.
 *)
let rec col_sum (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=b1 to d1 do
			 	let total = ref 0.0 in
			 	for j=a1 to c1 do
			 		if (is_some arr.(j).(i)) then total := !total +. (value arr.(j).(i))
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1).(i2+ (!m)) <- Some (!total);
			 	total := 0.0;
			 	m := !m +1;
			 done;
arr ;;


(* FULLAVG : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value and count by 1.
if no then we raise an exception.
Finally we assign the value of total/count to arr.(i1).(i2);
 *)
let rec full_avg (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let count = ref 0.0 in
			 let total = ref 0.0 in
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then begin count := (!count +. 1.0); total := (!total +. value arr.(i).(j)) end
			 		else raise InvalidInput;
			 	done;
			 done;
			 arr.(i1).(i2) <- Some ((!total)/.(!count));
arr ;;

(* ROWAVG : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value and count by 1. After interating each column, we assign the value of total/count to the corresponding output index.
If no, then we raise an exception.
 *)
let rec row_avg (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1+c1-a1 > (rows - 1) then extrarows := i1 + c1-a1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=a1 to c1 do
			 	let count = ref 0.0 in
			 	let total = ref 0.0 in
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then begin count := (!count +. 1.0); total := (!total +. value arr.(i).(j)) end
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1+ (!m)).(i2) <- Some ((!total)/.(!count));
			 	count := 0.0;
			 	total := 0.0;
			 	m := !m +1;
			 done;
arr ;;

(* COLAVG : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we increase total by its value and count by 1. After interating each column, we assign the value of total/count to the corresponding output index.
If no, then we raise an exception.
 *)
let rec col_avg (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=b1 to d1 do
			 	let count = ref 0.0 in
			 	let total = ref 0.0 in
			 	for j=a1 to c1 do
			 		if (is_some arr.(j).(i)) then begin count := !count +. 1.0; total := (!total +. value arr.(j).(i)) end
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1).(i2+ (!m)) <- Some ((!total)/.(!count));
			 	count := 0.0;
			 	total := 0.0;
			 	m := !m +1;
			 done;
arr ;;

(* FULLMIN : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign min to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with min, if it is less than min, we replace min with this value.
if arr.(i).(j) contains None then we raise an exception.
Finally we assign the value of min to arr.(i1).(i2);
 *)
let rec full_min (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let min = ref (value arr.(a1).(b1)) in
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then begin
			 			if value arr.(i).(j) < !min then min := value arr.(i).(j); end
			 		else raise InvalidInput;
			 	done;
			 done;
			 arr.(i1).(i2) <- Some (!min);
arr ;;

(* ROWMIN : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign min to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with min, if it is less than min, we replace min with this value.
After interating each row, we assign the value of min to the corresponding output index. and
if arr.(i).(j) contains None then we raise an exception.
 *)
let rec row_min (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1+c1-a1 > (rows - 1) then extrarows := i1 + c1-a1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=a1 to c1 do
			 	let min = ref (value arr.(i).(b1)) in
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then
			 			begin
			 				let a = value arr.(i).(j) in
							if (a <(!min)) then min := a;
							(* print_string "Manoj\n"; *)
			 			end
			 		else begin raise InvalidInput; end
			 	done;
			 	(* print_string "Manoj3\n"; *)
			 	arr.(i1+ (!m)).(i2) <- Some (!min);
			 	(* if i<>c1 then min := value arr.(i+1).(b1); *)
			 	(* print_string "manoj4\n"; *)
			 	m := !m +1;
			 done;
arr;;

(* COLMIN : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign min to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with min, if it is less than min, we replace min with this value.
After interating each column, we assign the value of min to the corresponding output index.
if arr.(i).(j) contains None then we raise an exception.
 *)
let rec col_min (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=b1 to d1 do
			 	let min = ref (value arr.(a1).(i)) in
			 	for j=a1 to c1 do
			 		if (is_some arr.(j).(i)) then begin
			 			if value arr.(j).(i) < !min then min := value arr.(j).(i);
			 		end
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1).(i2+ (!m)) <- Some !min;
			 	(* min := value arr.(i+1).(b1); *)
			 	m := !m +1;
			 done;
arr ;;

(* FULLMAX : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign max to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with max, if it is greater than max, we replace max with this value.
if arr.(i).(j) contains None then we raise an exception.
Finally we assign the value of max to arr.(i1).(i2);
 *)
let rec full_max (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput;
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			 (* let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let max = ref (value arr.(a1).(b1)) in
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then begin 
			 		if value arr.(i).(j) > !max then max := value arr.(i).(j); end
			 		else raise InvalidInput;
			 	done;
			 done;
			 arr.(i1).(i2) <- Some !max;
arr ;;


(* ROWMAX : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign max to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with max, if it is greater than max, we replace max with this value.
After interating each row, we assign the value of max to the corresponding output index. and
if arr.(i).(j) contains None then we raise an exception.
 *)

let rec row_max (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in	
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2 > (cols - 1) then extracols := i2 - cols + 1;
			 if i1+c1-a1 > (rows - 1) then extrarows := i1 + c1-a1 -rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=a1 to c1 do
			 	let max = ref (value arr.(i).(b1)) in
			 	for j=b1 to d1 do
				if (is_some arr.(i).(j)) then begin
					if((value arr.(i).(j)) > !max ) then max := value arr.(i).(j); end
			else raise InvalidInput;
			 	done;
			 	arr.(i1+ (!m)).(i2) <- Some !max;
			 	(* if i<>c1 then max := value arr.(i+1).(b1); *)
			 	m := !m +1;
			 done;
arr ;;


(* COLMAX : It takes sheet, range and index in arguments and returns an updated sheet.
first it checks if the number of rows are positive else raise error.
if range is invalid then it raises an exception
We can expand the sheet if we need extra indices.
First we assign max to arr.(a1).(b1)
we interates on the every index in the given range and check whether arr.(i).(j) contains some value or not, If yes,
then we compare value of that index with max, if it is greater than max, we replace max with this value.
After interating each column, we assign the value of max to the corresponding output index.
if arr.(i).(j) contains None then we raise an exception.
 *)
let rec col_max (arr:sheet) (rnze:(int*int*int*int)) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1 > (rows - 1) then extrarows := i1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 let m = ref 0 in
			 for i=b1 to d1 do
			 	let max = ref (value arr.(a1).(i)) in
			 	for j=a1 to c1 do
			 		if (is_some arr.(j).(i)) then begin
			 		if (value arr.(j).(i) > !max) then max := value arr.(j).(i); end
			 		else raise InvalidInput;
			 	done;
			 	arr.(i1).(i2+(!m)) <- Some !max;
			 	(* max := 0.0; *)
			 	m := !m +1;
			 done;
arr;;
(*Unary functions completed*)
(*Binary functions start*)

(* ADDCONST :  First we type match with rnze and then with indx.
We check whether given range exist in the sheet or not. If not, raise InvalidInput.
Then we iterate all the indices in the given range and add the constant value flt in each of them and place them on the corresponding output indices,
Finally we return the updated arr.
*)
let rec add_const (arr:sheet) (rnze:(int*int*int*int)) (flt:float) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in	
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1+c1-a1> (rows - 1) then extrarows := i1+c1-a1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			 if (i1+c1-a1) > (rows-1) || (i2+d1-b1) > (cols-1) then raise InvalidInput;
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then
			 		arr.(i1+i-a1).(i2+j-b1) <- Some ( value arr.(i).(j) +. flt)
			 		else raise InvalidInput;
			 	done;
			 done;
arr;;


(* SUBCONST :  First we type match with rnze and then with indx.
We check whether given range exist in the sheet or not. If not, raise InvalidInput.
Then we iterate all the indices in the given range and subtract the constant value flt in each of them and place them on the corresponding output indices,
Finally we return the updated arr.
*)
let rec subt_const (arr:sheet) (rnze:(int*int*int*int)) (flt:float) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
		(* 	 let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1+c1-a1> (rows - 1) then extrarows := i1+c1-a1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
			if (i1+c1-a1) > (rows-1) || (i2+d1-b1) > (cols-1) then raise InvalidInput;
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then arr.(i1+i-a1).(i2+j-b1) <- Some (value arr.(i).(j) -. flt)
			 		else raise InvalidInput;
			 	done;
			 done;
arr;;


(* MULTCONST :  First we type match with rnze and then with indx.
We check whether given range exist in the sheet or not. If not, raise InvalidInput.
Then we iterate all the indices in the given range and multiply the constant value flt with each of them and place them on the corresponding output indices,
Finally we return the updated arr.
*)
let rec mult_const (arr:sheet) (rnze:(int*int*int*int)) (flt:float) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) ->
	if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
		match indx with
			 (i1, i2) ->
			(*  let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1+c1-a1> (rows - 1) then extrarows := i1+c1-a1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in*)
			 if (i1+c1-a1) > (rows-1) || (i2+d1-b1) > (cols-1) then raise InvalidInput; 
			 for i=a1 to c1 do
			 	for j=b1 to d1 do
			 		if (is_some arr.(i).(j)) then arr.(i1+i-a1).(i2+j-b1) <- Some (value arr.(i).(j) *. flt)
			 		else raise InvalidInput;
			 	done;
			 done;
arr;;


(* DIVCONST :  First we type match with rnze and then with indx.
We check whether given range exist in the sheet or not. If not, raise InvalidInput.
Then we iterate all the indices in the given range and devide each of them with the given constant value flt and place them on the corresponding output indices,
Finally we return the updated arr.
*)
let rec div_const (arr:sheet) (rnze:(int*int*int*int)) (flt:float) (indx:(int*int)) : sheet = 
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	(* if flt == 0.0 then raise InvalidInput  *)
		match rnze with
		(a1, b1, c1, d1) ->
		if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput
		else
			match indx with
				 (i1, i2) ->
			(* 	 let extracols = ref 0 in
			 let extrarows = ref 0 in
			 if i2+d1-b1 > (cols - 1) then extracols := i2 + d1-b1 - cols + 1;
			 if i1+c1-a1> (rows - 1) then extrarows := i1+c1-a1-rows + 1;
			 let arr1 = expandarray arr cols rows !extracols !extrarows in *)
				 if (i1+c1-a1) > (rows-1) || (i2+d1-b1) > (cols-1) then raise InvalidInput;
				 for i=a1 to c1 do
				 	for j=b1 to d1 do
				 		if (is_some arr.(i).(j)) then arr.(i1+i-a1).(i2+j-b1) <- Some (value arr.(i).(j) /. flt)
				 		else raise InvalidInput;
				 	done;
				 done;
arr;;



(* ADDRANGE :  First we type match with rnze and rnze2 and then with indx.
We check whether given ranges exist in the sheet or not. If not, raise InvalidInput.
If dimensions of both ranges are not equal then raise InvalidInput.
Then we iterate all the indices in the given ranges and add the values of corresponding indices and place the result on the corresponding output indices,
Finally we return the updated arr.
*)

let rec add_range (arr:sheet) (rnze:(int*int*int*int)) (rnze2:(int*int*int*int)) (indx:(int*int)) : sheet=
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) -> 
		if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput;
		let rnzecols = d1-b1 in
		let rnzerows = c1-a1 in
		match rnze2 with
		(a2, b2, c2, d2) ->
			if a2 < 0 || b2 < 0 || c2 < 0 || d2 < 0 || a2 > rows-1 || c2 > rows-1 || b2 > cols-1 || d2 > cols-1 then raise InvalidInput;
			let rnze2cols = d2-b2 in
			let rnze2rows = c2-a2 in
			if rnzecols <> rnze2cols then raise InvalidInput;
			if rnzerows <> rnze2rows then raise InvalidInput;
			match indx with
				(i1, i2) ->
				(* let extracols = ref 0 in
					 let extrarows = ref 0 in
					 if( i1+rnzerows > rows-1) then extrarows := i1+rnzerows-rows + 1;
					 if (i2+rnzecols > cols-1) then extracols := i2+rnzecols-cols+1;
					let arr1 = expandarray arr cols rows !extracols !extrarows in *)
					for i=0 to rnzerows do
						for j=0 to rnzecols do
						if is_none arr.(a1+i).(b1+j) then raise InvalidInput;
						if is_none arr.(a2+i).(b2+j) then raise InvalidInput;
						arr.(i1+i).(i2+j) <- Some ( value arr.(a1+i).(b1+j) +. value arr.(a2+i).(b2+j))
					done;
				done;
arr;;



(* SUBTRANGE :  First we type match with rnze and rnze2 and then with indx.
We check whether given ranges exist in the sheet or not. If not, raise InvalidInput.
If dimensions of both ranges are not equal then raise InvalidInput.
Then we iterate all the indices in the given ranges and subtract the values of corresponding indices and place the result on the corresponding output indices,
Finally we return the updated arr.
*)
let rec subt_range (arr:sheet) (rnze:(int*int*int*int)) (rnze2:(int*int*int*int)) (indx:(int*int)) : sheet=
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) -> 
		if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput;
		let rnzecols = d1-b1 in
		let rnzerows = c1-a1 in
		match rnze2 with
		(a2, b2, c2, d2) ->
			if a2 < 0 || b2 < 0 || c2 < 0 || d2 < 0 || a2 > rows-1 || c2 > rows-1 || b2 > cols-1 || d2 > cols-1 then raise InvalidInput;
			let rnze2cols = d2-b2 in
			let rnze2rows = c2-a2 in
			if rnzecols <> rnze2cols then raise InvalidInput;
			if rnzerows <> rnze2rows then raise InvalidInput;
			match indx with
				(i1, i2) ->
				(* let extracols = ref 0 in
					 let extrarows = ref 0 in
					 if( i1+rnzerows > rows-1) then extrarows := i1+rnzerows-rows + 1;
					 if (i2+rnzecols > cols-1) then extracols := i2+rnzecols-cols+1;
					let arr1 = expandarray arr cols rows !extracols !extrarows in *)
					for i=0 to rnzerows do
						for j=0 to rnzecols do
						if is_none arr.(a1+i).(b1+j) then raise InvalidInput;
						if is_none arr.(a2+i).(b2+j) then raise InvalidInput;
						arr.(i1+i).(i2+j) <- Some ( value arr.(a1+i).(b1+j) -. value arr.(a2+i).(b2+j))
					done;
				done;
arr;;


(* MULTRANGE :  First we type match with rnze and rnze2 and then with indx.
We check whether given ranges exist in the sheet or not. If not, raise InvalidInput.
If dimensions of both ranges are not equal then raise InvalidInput.
Then we iterate all the indices in the given ranges and multiply the values of corresponding indices and place the result on the corresponding output indices,
Finally we return the updated arr.
*)
let rec mult_range (arr:sheet) (rnze:(int*int*int*int)) (rnze2:(int*int*int*int)) (indx:(int*int)) : sheet=
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) -> 
		if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput;
		let rnzecols = d1-b1 in
		let rnzerows = c1-a1 in
		match rnze2 with
		(a2, b2, c2, d2) ->
			if a2 < 0 || b2 < 0 || c2 < 0 || d2 < 0 || a2 > rows-1 || c2 > rows-1 || b2 > cols-1 || d2 > cols-1 then raise InvalidInput;
			let rnze2cols = d2-b2 in
			let rnze2rows = c2-a2 in
			if rnzecols <> rnze2cols then raise InvalidInput;
			if rnzerows <> rnze2rows then raise InvalidInput;
			match indx with
				(i1, i2) ->
				(* let extracols = ref 0 in
					 let extrarows = ref 0 in
					 if( i1+rnzerows > rows-1) then extrarows := i1+rnzerows-rows + 1;
					 if (i2+rnzecols > cols-1) then extracols := i2+rnzecols-cols+1;
					let arr1 = expandarray arr cols rows !extracols !extrarows in *)
					for i=0 to rnzerows do
						for j=0 to rnzecols do
						if is_none arr.(a1+i).(b1+j) then raise InvalidInput;
						if is_none arr.(a2+i).(b2+j) then raise InvalidInput;
						arr.(i1+i).(i2+j) <- Some ( value arr.(a1+i).(b1+j) *. value arr.(a2+i).(b2+j))
					done;
				done;
arr;;


(* DIVRANGE :  First we type match with rnze and rnze2 and then with indx.
We check whether given ranges exist in the sheet or not. If not, raise InvalidInput.
If dimensions of both ranges are not equal then raise InvalidInput.
Then we iterate all the indices in the given ranges and devide the values of corresponding indices and place the result on the corresponding output indices,
Finally we return the updated arr.
*)
let rec div_range (arr:sheet) (rnze:(int*int*int*int)) (rnze2:(int*int*int*int)) (indx:(int*int)) : sheet=
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	match rnze with
	(a1, b1, c1, d1) -> 
		if a1 < 0 || b1 < 0 || c1 < 0 || d1 < 0 || a1 > rows-1 || c1 > rows-1 || b1 > cols-1 || d1 > cols-1 then raise InvalidInput;
		let rnzecols = d1-b1 in
		let rnzerows = c1-a1 in
		match rnze2 with
		(a2, b2, c2, d2) ->
			if a2 < 0 || b2 < 0 || c2 < 0 || d2 < 0 || a2 > rows-1 || c2 > rows-1 || b2 > cols-1 || d2 > cols-1 then raise InvalidInput;
			let rnze2cols = d2-b2 in
			let rnze2rows = c2-a2 in
			if rnzecols <> rnze2cols then raise InvalidInput;
			if rnzerows <> rnze2rows then raise InvalidInput;
			match indx with
				(i1, i2) ->
				(* let extracols = ref 0 in
					 let extrarows = ref 0 in
					 if( i1+rnzerows > rows-1) then extrarows := i1+rnzerows-rows + 1;
					 if (i2+rnzecols > cols-1) then extracols := i2+rnzecols-cols+1;
					let arr1 = expandarray arr cols rows !extracols !extrarows in *)
					for i=0 to rnzerows do
						for j=0 to rnzecols do
						if is_none arr.(a1+i).(b1+j) then raise InvalidInput;
						if is_none arr.(a2+i).(b2+j) then raise InvalidInput;
						arr.(i1+i).(i2+j) <- Some ( value arr.(a1+i).(b1+j) /. value arr.(a2+i).(b2+j))
					done;
				done;
arr;;


(* ADDRANGE2 :  Similar to add_const.
	we pass the sheet, the float value flt as the value in indx1, the range rnze and the target index in the add_const.
*)
let add_range2 (arr:sheet) (rnze:(int*int*int*int)) (indx1:(int*int)) (indx:(int*int)) : sheet=
	match indx1 with
	(a,b) ->
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	if a>(rows-1) || b>(cols-1) then raise InvalidInput;
	if is_none arr.(a).(b) then raise InvalidInput
	else
add_const arr rnze (value arr.(a).(b)) indx;;


(* SUBTRANGE2 :  Similar to subt_const.
	we pass the sheet, the float value flt as the value in indx1, the range rnze and the target index in the subt_const.
*)
let subt_range2 (arr:sheet) (rnze:(int*int*int*int)) (indx1:(int*int)) (indx:(int*int)) : sheet=
	match indx1 with
	(a,b) ->
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	if a>rows-1 || b>cols-1 then raise InvalidInput;
	if is_none arr.(a).(b) then raise InvalidInput
	else
subt_const arr rnze (value arr.(a).(b)) indx;;



(* MULTRANGE2 :  Similar to mult_const.
	we pass the sheet, the float value flt as the value in indx1, the range rnze and the target index in the mult_const.
*)
let mult_range2 (arr:sheet) (rnze:(int*int*int*int)) (indx1:(int*int)) (indx:(int*int)) : sheet=
	match indx1 with
	(a,b) ->
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	if a>rows-1 || b>cols-1 then raise InvalidInput;
	if is_none arr.(a).(b) then raise InvalidInput
	else
mult_const arr rnze (value arr.(a).(b)) indx;;


(* DIVRANGE2 :  Similar to div_const.
	we pass the sheet, the float value flt as the value in indx1, the range rnze and the target index in the div_const.
*)
let div_range2 (arr:sheet) (rnze:(int*int*int*int)) (indx1:(int*int)) (indx:(int*int)) : sheet=
	match indx1 with
	(a,b) ->
	let rows = Array.length arr in
	if rows <= 0 then raise InvalidInput else
	let cols = Array.length arr.(0) in
	if a>rows-1 || b>cols-1 then raise InvalidInput;
	if is_none arr.(a).(b) then raise InvalidInput
	else
div_const arr rnze (value arr.(a).(b)) indx;;

(* let rows = 10;;
let cols = 10;;
let arr = Array.make_matrix rows cols None;;
arr_init arr rows cols;; *)


*)