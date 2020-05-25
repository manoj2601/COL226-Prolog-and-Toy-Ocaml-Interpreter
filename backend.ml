(*This is the backend file of the program. All the functions are implemented here*)

(*Errors*)
exception InvalidInput
exception ErrorInGoal
exception Error1
exception NotFound
exception NotFound1
exception NotFound2
exception NotFound3
exception WrongSubstitution

type symbol = string*int;;
type variable = string;;
type arity = int;;
type constant = string;; (*Also can be called atom*)
type term = V of variable
	| Node of symbol*(term list)
	| C of constant
	;;
	type clause = term * term list;; (*If term list size is zero, than clause is fact, else rule*)
	type substitution = (variable*term) list;;
	type substitutionR = bool*substitution;;
(* 
	let rec printTable l = match l with x::xs -> (printClause x);Printf.printf "NEXT CLAUSE \n\n\n\n"; (printTable xs);
	| [] -> Printf.printf "EMPTY"
	and printClause c = match c with (ter, terl) -> Printf.printf "Clause Start XXXXXXXXXXX\n"; (printTerm ter); Printf.printf "\nXXXXXXXXX MAPPED TO \n"; (printTerms terl);
	and printTerms tl = match tl with [] -> Printf.printf ""; | x::xs -> (printTerm x);(Printf.printf "next term\n");(printTerms xs);
	and printTerm t = match t with V(str) ->  Printf.printf "Variable %s\n" str;
	|Node((str,integ),children) -> Printf.printf "Symbol %s,%d\n Children\n" str integ; (printTerms children);Printf.printf "ChildrenOver\n";
	;; 

	let rec printSubstitution (sub: substitution) =
		match sub with
		| x::xs -> (match x with (var, term) -> Printf.printf"var is : %s " var; printTerm term; printSubstitution xs;)
		| _ -> Printf.printf "end!!";
		;; *)
	let rec ithelement (i:int) l = (*ith element of the list*)
	match l with
	| [] -> raise InvalidInput
	| x::xs -> if i=0 then x else ithelement (i-1) xs
;;
let rec createVList (t:term) : term list = (*Creating a list of variables in the given term*)
		match t with
		| V(var) -> [V(var)]
		| Node(sym, tl) -> 
							(	List.fold_left (takeunion) [] (List.map createVList tl) )
		| _ -> []
		and takeunion (l1:term list) (l2:term list) = 
			(match l2 with
					| x::xs -> List.fold_left takeunion2 l1 l2
					| [] -> l1
			);
		and takeunion2 (l1:term list) (x:term) =
			(
				match l1 with 
				| y::ys -> if(x = y) then l1 else (y::(takeunion2 ys x));
				| [] -> [x]
			)
;;

(*Copied from assignment 4 starts*)
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

(*Copied from assignment 4 ends*)

let rec unify (t1:term) (t2:term) (inisubs:substitutionR) : substitutionR =
		match inisubs with
		| (true, sl) -> mguhelper t1 t2 sl
		| _ -> (false, [])
	;;

let rec printSubTerm (t: term) = (*Printing the substituted term*)
	match t with
	| V(var) -> Printf.printf "%s" var;
	| Node(sym, tl) -> (match sym with (name, leng) -> Printf.printf "%s" name;)
;;
let rec printResult (subR:substitutionR) (vset: term list) = (*Printing the final result*)
	match subR with (true, sub) -> (if (List.length vset = 0) then Printf.printf "true." else printResult2 sub vset vset) 
				| _ -> raise NotFound
and printResult2 (sub:substitution) (vset: term list) (cvset: term list)=
		match vset with
		| y::xs -> (match y with 
							V(x) -> (
								let ter = searchInSub sub x in
								match ter with 
								| V(var) -> if (isPresent var cvset) then (Printf.printf "\n%s = " x; printSubTerm ter; printResult2 sub xs cvset;) else (printResult2 sub xs cvset;)
								| _ -> (Printf.printf "\n%s = " x; printSubTerm ter; printResult2 sub xs cvset;)
								)
							| _ -> raise WrongSubstitution;
								)
		| _ -> Printf.printf " ";
	and isPresent (var:variable) (vset: term list) : bool =
		match vset with x::xs -> if x = V(var) then true else isPresent var xs | [] -> false
	and searchInSub (sub:substitution) (var:variable) =
	match sub with
												x::xs -> (match x with (var2, t) -> if var = var2 then t else searchInSub xs var)
												| _ -> raise NotFound
											
								| _ -> raise NotFound
;;

let rec shouldloopnext (inisubs:substitutionR) (vset: term list) =
	match List.length vset with
	| 0 -> inisubs
	| _ -> (let rl = read_line() in
		match rl with 
		";" -> (false, [])
		| "." -> inisubs
		| _ -> Printf.printf "Invalid Symbol\n"; inisubs
	)
;;

let rec appendTerm (t:term) =
	match t with 
	V(var) -> V(var^"!")
	| Node(sym, tl) -> Node(sym, List.map appendTerm tl)
	| _ -> t
;;
let rec appendTable (table: clause list) =
	match table with
	| (hd, bdy)::xs -> ((appendTerm hd), (List.map appendTerm bdy)) :: (appendTable xs)
	| [] -> []
;;

let rec loopOnCList (inisubs:substitutionR) (table1: clause list) (g:term) (lgoals: term list) (table:clause list) (vset: term list) : substitutionR =
	match table1 with
	| x::xs ->	(
					let ret = operationOnElement vset x g inisubs table lgoals in (*Substitution after unifying goal and cullrent clause of the program*)
							match ret with
							|	(true,sub1) -> (true, sub1)
							| (false, sub) -> loopOnCList inisubs xs g lgoals table vset
							| _ -> raise WrongSubstitution
							)
	| _ -> (false, []) (*If program clauses ends*)
			
and goalsSolve (lgoals:term list) (inisubs: substitutionR) (table: clause list) (vset: term list) : substitutionR =
	let atable = appendTable table in (*differentiating variables of each depth by adding a "!" at the end of each variable on each depth.*)
	match lgoals with
	| x::xs -> (
				let newsubs = (loopOnCList inisubs atable x xs atable vset) in (*New updated substitution after solving a goal*)
				(newsubs)
		)
	| _ -> printResult inisubs vset; (shouldloopnext inisubs vset) (*Printing result after solving all goals and decides to continue backtracking or not*)

and operationOnElement (vset:term list) (x:clause) (g:term) (inisubs: substitutionR) (table: clause list) (lgoals: term list) : substitutionR =
	match x with
		| (hd, bdy) -> ( let newsub = unify hd g inisubs in (*Unify head(x) and g and return the bool result whether these are unifiable or not*)
							match newsub with 
							| (true, sub) -> goalsSolve (List.append bdy lgoals) newsub table vset 
							| (false, sub) ->(false, [])
								(* if unifiable then update inisubs and update Lgoals with body of x, again solve for 1st element of Lgoals *)
		)
;;


let solution (table: clause list) (goal : clause) = 
	match goal with
	| (g, []) -> ( 
		let vset = createVList g in (*Created a list of variables in goal ( element is like : V(var) ) that will be used in printing results.*)
		let inisubs = (true, []) in (*initial substitution*)
		let lgoals = [g] in (*initial list of goals*)
		let subR = goalsSolve lgoals inisubs table vset in (*entering in depth first traverse and backtracking*)
		match subR with (*Final substitution*)
		| (true, sub) -> Printf.printf "\n"; subR
		| (false, sub) -> Printf.printf "false.\n"; subR
	)
	| _ -> raise ErrorInGoal	
	;;
