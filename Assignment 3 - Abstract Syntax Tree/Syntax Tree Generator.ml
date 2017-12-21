(* let() = if Array.length Sys.argv <> 3                                                            *)
(* 	then let () = print_string "Correct format: cs5140279.ml <input_file> <output_file>" in exit 1 *)
(* 	else ();;                                                                                      *)

(* let in_file_name = Sys.argv.(1);;  *)
(* let out_file_name = Sys.argv.(2);; *)

let in_file_name="C:\Python27\COL 226 Ocaml\Assignment 3 - Abstract Syntax Tree\pt3.txt";;
let out_file_name="C:\Python27\COL 226 Ocaml\Assignment 3 - Abstract Syntax Tree\output3.txt";;

let rec clean l acc=
	match l with 
	| []->acc
	| x::xs->
		let a,b=x in 
		if a="" then
			clean xs acc
		else
			clean xs (acc@[x])

let create_parselist filename=
	let in_file = open_in filename in
	let in_stream = Stream.of_channel in_file in
	let rec read_file l bc strt buffer=
		try 
			let nxt=Stream.next in_stream in
			if strt=false then
				if nxt='[' then
					read_file l (bc+1) true ""
				else
					read_file l (bc) false ""
			else
				if nxt='[' then 
					read_file (l@[(buffer,(bc))]) (bc+1) true ""
				else if nxt=']' then 
					read_file (l@[(buffer,(bc))]) (bc-1) true ""
				else if nxt=',' then
					read_file (l@[(buffer,(bc))]) (bc) true ""
				else if nxt=' ' || nxt='\n' || nxt='\t' then
					read_file (l) (bc) true (buffer)
				else
					read_file (l) (bc) true (buffer^Char.escaped(nxt))					
		with Stream.Failure->
			close_in in_file;
			l				
	in clean (read_file [] 0 false "") [];;

type 'a tree= Leaf of ('a *int)| Node of ('a * int * 'a tree list);;

let rec cut_list l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		let a,b=x in
			if b>n then
				cut_list xs (acc@[x]) n
			else
				acc,l

let rec create_parsetree l curr=
	match l with
	| []->
		[curr]
	| x::xs->
		(match curr with
		| Leaf(p,q)->
			let l1,l2=cut_list l [] q in
			if l1=[] then
				(match l2 with
					| []->
						[Leaf(p,q)]
					| y::ys->
						let pp,qq=y in
						let l3=create_parsetree ys (Leaf(pp,qq)) in
						[Leaf(p,q)]@l3				
						)
			else
				let z::zs=l1 in
				let ppp,qqq=z in
				let ll=create_parsetree zs (Leaf(ppp,qqq)) in
				(match l2 with
					| []->
						[Node(p,q,ll)]
					| y::ys->
						let pp,qq=y in
						let l3=create_parsetree ys (Leaf(pp,qq)) in
						[Node(p,q,ll)]@l3				
						)
		| Node(p,q,r)->[]
		)

let get_parsetree filename=
	let l=create_parselist filename in
	match l with
	| []->[Leaf("",0)]
	| x::xs->
		let a,b=x in
		create_parsetree xs (Leaf(a,b))

let rec map f l acc=
	match l with
	| []->acc
	| x::xs->
		map f xs (acc@[f x])
		
let rec append l1 l2=
	l1@l2
	
let rec reduce l acc=
	match l with 
	| []->acc
	| x::xs->reduce xs (acc@x)	
						
let rec abstract tree=
	match tree with
	| Node(p,q,r)->
		(match p with
		| "Program"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "Block"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "DeclarationSeq"->[Node(p,q,(reduce (map abstract r [])	[]))]
		| "VarDecls"->(reduce (map abstract r [])	[])
		| "IntVarDecls"->[Node(p,q,(reduce (map abstract r [])	[]))]
		| "BoolVarDecls"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "ProcDecls"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "VarDef"->reduce (map abstract r [])	[]
		| "VarDef1"->reduce (map abstract r []) []
		| "Ident"->r
		| "IDENT"->r
		| "CommandSeq"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "Command"-> reduce (map abstract r []) []
		| "ReadCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "PrintCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "AssignmentCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "ConditionalCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "WhileCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "CallCmd"-> [Node(p,q,(reduce (map abstract r [])	[]))]
		| "Expression"->[Node(p,q,(reduce (map abstract r [])	[]))]
		| "IntExpression"->reduce (map abstract r []) []
		| "BoolExpression"->reduce (map abstract r []) []
		| "IntE"->reduce (map abstract r []) []
		| "IntT"->reduce (map abstract r []) []
		| "IntT1"->reduce (map abstract r []) []
		| "IntF"->reduce (map abstract r []) []
		| "IntF1"->reduce (map abstract r []) []
		| "BoolE"->reduce (map abstract r []) []
		| "BoolF"->reduce (map abstract r []) []
		| "BoolF1"->reduce (map abstract r []) []
		| "BoolG"->reduce (map abstract r []) []
		| "BoolG1"->reduce (map abstract r []) []
		| "BoolH"->reduce (map abstract r []) []
		| "BoolH1"->reduce (map abstract r []) []
		| "BoolJ"->reduce (map abstract r []) []
		| "BoolI"->reduce (map abstract r []) []
		| "IntLiteral"->reduce (map abstract r []) []
		| "BoolLiteral"->reduce (map abstract r []) []
		| _->[tree]
		 )
	| Leaf(p,q)->
		(match p with
		| "EPSILON"->[]
		| "INT"->[]
		| "BOOL"->[]
		| "PROC"->[]
		| "EOS"->[]
		| "LB"->[]
		| "RB"->[]
		| "LP"->[]
		| "RP"->[]
		| "READ"-> []
		| "PRINT"-> []
		| "CALL"-> []
		| "COMMA"->[]
		| "ASSIGN"->[]
		| "IF"->[]
		| "THEN"->[]
		| "ELSE"->[]
		| "WHILE"->[]
		| _->[Leaf(p,q)]
		 )
		
let printAbstractTree filename1 filename2= 
	let ptree=get_parsetree filename1 in
	let tree=(List.hd (abstract (List.hd ptree))) in
	let outfile = open_out (filename2) in
	let rec tab = function
		| 0 -> ()
		| n -> let () = output_char outfile '\t' in tab (n-1)
	in
	let rec help t = 
		match t with
		 | Node (p,q,r) -> 
			 	output_string (outfile) (p ^ "\n");
			 	tab (q+1);
			 	output_string (outfile) ("[ ");
			 	help2 r;
			 	output_string (outfile) ("]\n"); 
			 	tab q
		 | Leaf (s,l) -> output_string outfile (s)  
	and help2 l= 
		match l with
			| [] -> ()
			| [h] -> help h
			| h::hs -> 
				help h;
				output_string outfile (",\n");
				(match h with
					 | Node (p,q, r) ->
						tab q;
						help2 hs
					 | Leaf (p,q) ->
						tab q;
						help2 hs
			 		)
	in
	output_string outfile "[ ";
	help tree;
	output_string outfile "]";
	close_out outfile;;

printAbstractTree in_file_name out_file_name;

(* let in_file_name="C:\Python27\COL 226 Ocaml\Assignment 3 - Abstract Syntax Tree\input.txt"         *)
(* let out_file_name="C:\Python27\COL 226 Ocaml\Assignment 3 - Abstract Syntax Tree\output.txt"       *)
(* let in_file_name2="C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\TestCase2\Output_ParseTree.txt" *)		
		
		(* let a,b=x in                                                      *)
		(* (                                                                 *)
		(* 	let l1,l2=cut_list l [] b in                                    *)
		(* 	if l1=[] then                                                   *)
		(* 		(match curr with                                              *)
		(* 		| Node(p,q,r)->                                               *)
		(* 			(match l2 with                                              *)
		(* 			| []->                                                      *)
		(* 				[Node(p,q,r@[Leaf(a,b)])]                                 *)
		(* 			| y::ys->                                                   *)
		(* 				let pp,qq=y in                                            *)
		(* 				let l3=create_parsetree ys (Leaf(pp,qq)) in               *)
		(* 				[Node(p,q,(r@[Leaf(a,b)]))]@l3				                    *)
		(* 				)					                                               *)
		(* 		|Leaf(p,q)->                                                  *)
		(* 			(match l2 with                                              *)
		(* 			| []->                                                      *)
		(* 				[Node(p,q,[Leaf(a,b)])]                                   *)
		(* 			| y::ys->                                                   *)
		(* 				let pp,qq=y in                                            *)
		(* 				let l3=create_parsetree ys (Leaf(pp,qq)) in               *)
		(* 				[Node(p,q,[Leaf(a,b)])]@l3				                        *)
		(* 				)                                                         *)
		(* 			)				                                                   *)
		(* 	else                                                            *)
		(* 		let ll=create_parsetree l1 (Node(a,b,[])) in                  *)
		(* 		(match l2 with                                                *)
		(* 		| []->                                                        *)
		(* 			(match curr with                                            *)
		(* 			| Node(p,q,r)->                                             *)
		(* 				[Node(p,q,r@ll)]                                          *)
		(* 			|Leaf(p,q)->[Node(p,q,ll)]                                  *)
		(* 			)                                                           *)
		(* 		| y::ys->                                                     *)
		(* 			let ppp,qqq=List.hd l2 in                                   *)
		(* 			let lll=create_parsetree (List.tl l2) (Node(ppp,qqq,[])) in *)
		(* 			(match curr with                                            *)
		(* 			| Node(p,q,r)->                                             *)
		(* 				[Node(p,q,r@ll@lll)]                                      *)
		(* 			|Leaf(p,q)->[Node(p,q,ll@lll)]                              *)
		(* 			)                                                           *)
		(* 		)                                                             *)
		(* )		                                                             *)		
(* Pswdx123 *)
