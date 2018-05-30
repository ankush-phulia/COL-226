#load "str.cma";;
#use "BigInt.ml";;       
#use "State Machine.ml";;

let in_file_name1="ast2.txt";;
let in_file_name2="st2.txt";;
let out_file_name1="instructions.txt";;
let out_file_name2="output.txt";;

let isInteger a=
	if a="" then false
	else
	let rec isinteger y n=
	if n<String.length y then
		let x=y.[n] in
			if x>='0' && x<='9' then
				isinteger y (n+1)
			else 
			  false
	else
		true
	in isinteger a 0;;

let rec clean l acc=
	match l with 
	| []->acc
	| x::xs->
		let a,b=x in 
		if a="" then
			clean xs acc
		else
			clean xs (acc@[x])
;;
let create_aslist filename=
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

let rec concat l acc=
	match l with
	| []->acc
	| x::xs->concat xs (acc^x)

let rec symbolise s=
	let ss=Str.split (Str.regexp "[ \t]+") s in
	if List.nth ss 1="IDENT" then
		let [w;x;y;z]= ss in
		let sss=Str.split (Str.regexp ":") z in
		[(w,y,concat sss "")]
	else
		[]	

let create_symt filename=
	let in_file = open_in filename in
	let rec read_file l=
		try 
			let nxt=input_line in_file in
			if nxt="" then
				read_file (l) 
			else
			 let nxt_symb=symbolise nxt in
			read_file (l@nxt_symb) 
		with End_of_file->
			close_in in_file;
			l		
	in (read_file []);;

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
;;
let rec create_astree l curr=
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
						let l3=create_astree ys (Leaf(pp,qq)) in
						[Leaf(p,q)]@l3				
						)
			else
				let z::zs=l1 in
				let ppp,qqq=z in
				let ll=create_astree zs (Leaf(ppp,qqq)) in
				(match l2 with
					| []->
						[Node(p,q,ll)]
					| y::ys->
						let pp,qq=y in
						let l3=create_astree ys (Leaf(pp,qq)) in
						[Node(p,q,ll)]@l3				
						)
		| Node(p,q,r)->[]
		)
;;
let get_astree filename=
	let l=create_aslist filename in
	match l with
	| []->[Leaf("",0)]
	| x::xs->
		let a,b=x in
		create_astree xs (Leaf(a,b))
;;

let rec map f l acc=
	match l with
	| []->acc
	| x::xs->
		map f xs (acc@[f x])
;;		
let rec append l1 l2=
	l1@l2
;;	
let rec reduce l acc=
	match l with 
	| []->acc
	| x::xs->reduce xs (acc@x)
;;	

let rec search (x,y) st flag=
	match st with
	| []->
		if flag=0 then (0,0)
		else (1,0)
	| l::ls->
		let a,c,b=l in
		if a=x && b=y then 1,1
		else  
			if a=x && (not(b=y)) then search (x,y) ls 1
			else
				search (x,y) ls flag
;;
(* let rec fix_ast scope st flag tree=                                                                                                          *)
(* 	match tree with                                                                                                                            *)
(* 	| Leaf(p,q)->                                                                                                                              *)
(* 		if search (p,"global"^scope) st 0= (1,1) then                                                                                            *)
(* 			Leaf(p^scope,q)                                                                                                                        *)
(* 		else if search (p,"global"^scope) st 0= (1,0) then                                                                                       *)
(* 			Leaf(p,q)                                                                                                                              *)
(* 		else                                                                                                                                     *)
(* 			if p="IntVarDecls" || p="BoolVarDecls" || p="ProcDecls" || p="CommandSeq" || p="ProcDecls" ||p="tt"||p="ff" ||(isInteger(p)=true) then *)
(* 				Leaf(p,q)                                                                                                                            *)
(* 			else                                                                                                                                   *)
(* 				raise Not_Declared                                                                                                                   *)
(* 	| Node(p,q,r)->                                                                                                                            *)
(* 		if flag=0 then                                                                                                                           *)
(* 			if p="ProcDecls" then                                                                                                                  *)
(* 				Node(p,q,(map (fix_ast scope st 1) r []))                                                                                            *)
(* 			else                                                                                                                                   *)
(* 				Node(p,q,(map (fix_ast scope st flag) r []))                                                                                         *)
(* 		else                                                                                                                                     *)
(* 			Node(p,q,(map (fix_ast (scope^p) st 0) r []))                                                                                          *)

(* let rec search2 (x,y) st flag t=                     *)
(* 	match st with                                      *)
(* 	| []->                                             *)
(* 		if flag=0 then (0,0,t)                           *)
(* 		else (1,0,t)                                     *)
(* 	| l::ls->                                          *)
(* 		let a,c,b=l in                                   *)
(* 		if a=x && b=y then 1,1,c                         *)
(* 		else                                             *)
(* 			if a=x && (not(b=y)) then search2 (x,y) ls 1 c *)
(* 			else                                           *)
(* 				search2 (x,y) ls flag t                      *)

(* let typ_chk typ st scope tree=                                                                      *)
(* 	match ast with                                                                                    *)
(* 	| Leaf(p,q)->                                                                                     *)
(* 		if p="IntVarDecls" || p="BoolVarDecls" || p="ProcDecls" || p="CommandSeq" || p="ProcDecls" then *)
(* 				Leaf(p,q)                                                                                   *)
(* 		else if p="tt"||p="ff" then                                                                     *)
(* 			if typ="BOOL" then Leaf(p,q)                                                                  *)
(* 			else raise Type_Mismatch				                                                              *)
(* 		else if (isInteger(p)=true) then                                                                *)
(* 			if typ="INT" then Leaf(p,q)                                                                   *)
(* 			else raise Type_Mismatch                                                                      *)
(* 		else                                                                                            *)
(* 			let a,b,c=search2 (p,"global"^scope) st scope 0 "null" in                                     *)
(* 			if c=typ then Leaf(p,q)                                                                       *)
(* 			else raise Type_Mismatch                                                                      *)
(* 	| Node(p,q,r)->                                                                                   *)
(* 		(match p with                                                                                   *)
(* 		| "ASSIGN"->                                                                                    *)
(* 			let Leaf(ass,bss)=List.hd r in                                                                *)
(* 			let c,cc,cc=search2 ()                                                                        *)
(* 		| "BINADD"->                                                                                    *)
(* 		| "BINSUB"->                                                                                    *)
(* 		| "BINMUL"->                                                                                    *)
(* 		| "BINDIV"->                                                                                    *)
(* 		| "BINMOD"->                                                                                    *)
(* 		| "GTE"->                                                                                       *)
(* 		| "GT"->                                                                                        *)
(* 		| "LTE"->                                                                                       *)
(* 		| "LT"->                                                                                        *)
(* 		| "NE"->                                                                                        *)
(* 		| "EQ"->                                                                                        *)
(* 		| "AND"->                                                                                       *)
(* 		| "OR"->                                                                                        *)
(* 		| "UNMINUS"->                                                                                   *)
(* 		| "NEG"->                                                                                       *)
(* 		)                                                                                               *)

let rec var typ scope n :string list=
	match n with
	| Leaf(p,q)->[typ^" "^p^scope^" _ _"]
	| Node(p,q,r)->
		let ll=(reduce (map (low_lev [] (scope)) r []) []) in
		[typ^" "^p^" 2 _"]@["GOTO _ "^string_of_int(List.length ll+2)^" _"]@ll@["RETURN _ _ _"]
	
and oper_bin scope op r acc typ :string list=
	match r with
	| [Leaf(m,n);Leaf(mm,nn)]->
		let m1="NV"^m^m in
		let mm1="NV"^mm^mm in
		acc@["DECLARE_"^typ^" "^m1^" _ _"]@["DECLARE_"^typ^" "^mm1^" _ _"]@["ASSIGN "^m^" _ "^m1]@["ASSIGN "^mm^" _ "^mm1]@["DECLARE_"^typ^" "^"NV"^m^scope^mm^scope^" _ _"]@[op^" "^m1^scope^" "^mm1^scope^" "^"NV"^m^mm^scope]
	| [Leaf(m,n);Node(mm,nn,oo)]->
		let ll=(low_lev [] scope (Node(mm,nn,oo))) in		
		let xx=Str.split (Str.regexp "[ \t]+") (List.nth ll (List.length ll -1)) in
		let yy=(List.nth xx 1) in
		let m1="NV"^m^m in
		acc@ll@["DECLARE_"^typ^" "^m1^" _ _"]@["ASSIGN "^m^" _ "^m1]@["DECLARE_"^typ^" "^"NV"^m^scope^yy^scope^" _ _"]@[op^" "^m1^scope^" "^yy^scope^" "^"NV"^m^scope^yy^scope]
	| [Node(mm,nn,oo);Leaf(m,n)]->
		let ll=(low_lev [] scope (Node(mm,nn,oo))) in
		let xx=Str.split (Str.regexp "[ \t]+") (List.nth ll (List.length ll -1)) in
		let yy=(List.nth xx 1) in
		let m1="NV"^m^m in
		acc@ll@["DECLARE_"^typ^" "^m1^" _ _"]@["ASSIGN "^m^" _ "^m1]@["DECLARE_"^typ^" "^"NV"^yy^scope^m^scope^" _ _"]@[op^" "^yy^scope^" "^m1^scope^" "^"NV"^yy^scope^m^scope]
	| [Node(m,n,o);Node(mm,nn,oo)]->
		let ll=(low_lev [] scope (Node(m,n,o))) in
		let xx=Str.split (Str.regexp "[ \t]+") (List.nth ll (List.length ll -1)) in
		let yy=(List.nth xx 1) in
		let ll2=(low_lev [] scope (Node(mm,nn,oo))) in
		let xx2=Str.split (Str.regexp "[ \t]+") (List.nth ll2 (List.length ll2 -1)) in
		let yy2=(List.nth xx2 1) in
		acc@ll@ll2@["DECLARE_"^typ^" "^"NV"^yy^scope^yy2^scope^" _ _"]@[op^" "^yy^scope^" "^yy2^scope^" "^"NV"^yy^scope^yy2^scope]
	|_->acc
	
and oper_un scope op r acc typ :string list=
	match r with
	| [Leaf(m,n)]->
		let m1="NV"^m^m in
		if typ=1 then
			acc@["DECLARE_INT "^m1^" _ _"]@["ASSIGN "^m^" _ "^m1]@[op^" _ "^m1^scope^" "^"NV"^m^scope]
		else
			acc@["DECLARE_BOOL "^m1^" _ _"]@["ASSIGN "^m^" _ "^m1]@[op^" _ "^m1^scope^" "^"NV"^m^scope]
	| [Node(mm,nn,oo)]->
		let ll=(low_lev [] scope (Node(mm,nn,oo))) in		
		let xx=Str.split (Str.regexp "[ \t]+") (List.nth ll (List.length ll -1)) in
		let yy=(List.nth xx 1) in
		if typ=1 then
			acc@ll@["DECLARE_INT "^"NV"^yy^scope^" _ _"]@[op^" _ "^yy^scope^" "^"NV"^yy^scope]
		else
			acc@ll@["DECLARE_BOOL "^"NV"^yy^scope^" _ _"]@[op^" _ "^yy^scope^" "^"NV"^yy^scope]	
	|_->acc
	
and low_lev acc scope tree :string list=
	match tree with
	| Node(p,q,r)->
		(match p with
		| "Program"-> acc@(reduce (map (low_lev [] scope) r []) [])
		| "Block"-> acc@(reduce (map (low_lev [] scope) r []) [])
		| "DeclarationSeq"->acc@(reduce (map (low_lev [] scope) r []) [])
		| "VarDecls"->acc@(reduce (map (low_lev [] scope) r []) [])
		| "IntVarDecls"->acc@(reduce (map (var "DECLARE_INT" scope) r []) [])
		| "BoolVarDecls"-> acc@(reduce (map (var "DECLARE_BOOL" scope) r []) [])
		| "ProcDecls"-> acc@(reduce (map (var "DECLARE_PROC" scope) r []) [])
		| "CommandSeq"-> acc@(reduce (map (low_lev [] scope) r []) [])
		| "PRINT"->acc@(reduce (map (var "PRINT" scope) r []) [])
		| "CALL"->acc@(reduce (map (var "CALL" scope) r []) [])
		| "READ"->let Leaf(m,n)=List.hd r in acc@["READ _ _ "^m]	
		| "ASSIGN"->
			(match r with
			| [Leaf(m,n);Leaf(mm,nn)]->acc@["ASSIGN "^mm^" _ "^m]
			| [Leaf(m,n);Node(mm,nn,oo)]->
				let ll=(low_lev [] scope (Node(mm,nn,oo))) in
				let xx=Str.split (Str.regexp "[ \t]+") (List.nth ll (List.length ll -2)) in
				acc@ll@["ASSIGN "^(List.nth xx 1)^" _ "^m]				
			)
		| "ConditionalCmd"->
			(match (List.hd r) with
			| Node(pp,qq,rr)->			
				let ll=((map (low_lev [] scope) r [])) in
				let lll=List.hd ll in
				let xx=Str.split (Str.regexp "[ \t]+") (List.nth (lll) (List.length lll -1)) in
				let yy=(List.nth xx 1) in
				let aa=List.nth ll 1 in
				let bb=List.nth ll 2 in
				acc@(lll)@["IF "^yy^" "^string_of_int((List.length (aa)) +2)^" _"]@(aa)@["GOTO _ "^string_of_int(List.length (bb) +1)^" _"]@(bb)
			|	Leaf(pp,qq)->
				let ll=((map (low_lev [] scope) r [])) in
				let aa=List.nth ll 1 in
				let bb=List.nth ll 2 in
				acc@["IF "^pp^" "^string_of_int((List.length (aa)) +2)^" _"]@aa@["GOTO _ "^string_of_int(List.length (bb) +1)^" _"]@(bb)
				)
		| "WHILE"->
			(match (List.hd r) with
			| Node(pp,qq,rr)->
				let ll=((map (low_lev [] scope) r [])) in
				let lll=List.hd ll in
				let xx=Str.split (Str.regexp "[ \t]+") (List.nth (lll) (List.length lll -1)) in
				let yy=(List.nth xx 1) in
				let llll=reduce (List.tl ll) [] in
				acc@(lll)@["IF "^yy^" "^string_of_int((List.length (llll)) +2)^" _"]@(llll)@["GOTO _ "^string_of_int(0-(List.length (llll)) -1)^" _"]		
			|	Leaf(pp,qq)->
				let ll=(reduce (map (low_lev [] scope) r []) []) in
				acc@["IF "^pp^" "^string_of_int((List.length (ll)) +2)^" _"]@(ll)@["GOTO _ "^string_of_int(0-(List.length (ll)) -1)^" _"]
				)			
		| "BINADD"->acc@(oper_bin scope "PLUS" r [] "INT")
		| "BINSUB"->acc@(oper_bin scope "MINUS" r [] "INT")
		| "BINMUL"->acc@(oper_bin scope "MULT" r [] "INT")
		| "BINDIV"->acc@(oper_bin scope "DIV" r [] "INT")
		| "BINMOD"->acc@(oper_bin scope "MOD" r [] "INT")
		| "GTE"->acc@(oper_bin scope "GEQ" r [] "BOOL")
		| "GT"->acc@(oper_bin scope "GT" r [] "BOOL")
		| "LTE"->acc@(oper_bin scope "LEQ" r [] "BOOL")
		| "LT"->acc@(oper_bin scope "LT" r [] "BOOL")
		| "NE"->acc@(oper_bin scope "NEQ" r [] "BOOL")
		| "EQ"->acc@(oper_bin scope "EQ" r [] "BOOL")
		| "AND"->acc@(oper_bin scope "AND" r [] "BOOL")
		| "OR"->acc@(oper_bin scope "OR" r [] "BOOL")
		| "UNMINUS"->acc@(oper_un scope "UNMINUS" r [] 1)
		| "NEG"->acc@(oper_un scope "NOT" r [] 0)
		| _->acc
		 )
	| Leaf(p,q)->
		(match p with
		| _->acc
		 )
;;
						
let print_inslist in_file_name1 in_file_name2 out_file_name=
	let tree= List.hd (get_astree in_file_name1) in
	(* let st=create_symt in_file_name2 in *)
	(* let ttree=fix_ast "" st 0 tree in *)
	let l=low_lev [] "" tree in
	let rec clean2 ll acc n=
		match ll with
		| []->acc
		| x::xs->
			let instr=Str.split (Str.regexp "[ \t]+") x in
			(match instr with
			| ["DECLARE_PROC";x;y;z]->clean2 xs (acc@["DECLARE_PROC "^x^" "^(string_of_int(n+int_of_string(y)))^" _"]) (n+1)
			| ["IF";x;y;z]->clean2 xs (acc@["IF "^x^" "^(string_of_int(n+int_of_string(y)))^" _"]) (n+1)
			| ["GOTO";x;y;z]->clean2 xs (acc@["GOTO "^x^" "^(string_of_int(n+int_of_string(y)))^" _"]) (n+1)
			| _->clean2 xs (acc@[x]) (n+1)
			)
	in 
	let ll=clean2 l [] 0 in	
	let output=open_out out_file_name in
	let rec writef x=
		match x with
		| []->close_out output
	  | y::ys->
			Printf.fprintf output "%s\n" y; 
			writef ys	
	in writef (ll@["END_OF_CODE _ _ _"]);;
			
print_inslist in_file_name1 in_file_name2 out_file_name1;;
