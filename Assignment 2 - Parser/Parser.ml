(* let() = if Array.length Sys.argv <> 4                                                                                                          *)
(* 	then let () = print_string "Correct format: cs5140279.ml <input_file> <output_file for parse tree> <output_file for symbol table>" in exit 1 *)
(* 	else ();;                                                                                                                                    *)

(* let in_file_name = Sys.argv.(1);;                                                                                                              *)
(* let out_file_name1= Sys.argv.(2);;                                                                                                             *)
(* let out_file_name2 = Sys.argv.(3);;                                                                                                            *)

let in_file_name="C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\input3.txt";;
let out_file_name1="C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\output1.txt";;
let out_file_name2="C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\output2.txt";;

exception Invalid_Token

type token = UNMINUS of (int * int)
	| BINADD of (int * int)
	| BINSUB of (int * int)
	| BINDIV of (int * int)
	| BINMUL of (int * int)
	| BINMOD of (int * int)
	| NEG of (int * int)
	| AND of (int * int)
	| OR of (int * int)
	| ASSIGN of (int * int)
	| EQ of (int * int)
	| NE of (int * int)
	| LT of (int * int)
	| LTE of (int * int)
	| GT of (int * int)
	| GTE of (int * int)
	| LP of (int * int)
	| RP of (int * int)
	| LB of (int * int)
	| RB of (int * int)
	| EOS of (int * int)
	| COMMA of (int * int)
	| INT of (int * int)
	| BOOL of (int * int)
	| IF of (int * int)
	| THEN of (int * int)
	| ELSE of (int * int)
	| WHILE of (int * int)
	| PROC of (int * int)
	| PRINT of (int * int)
	| READ of (int * int)
	| CALL of (int * int)
	| ERROR of (int * int * int)
	| INTLIT of (int * int * int)
	| IDENT of (int * int * string)
	| BOOLVAL of (int * int * bool);;
		
let ptuple s r=
	let x=		
	let rec tuple k n acc buffer=	
		if k.[n]='(' then
			tuple k (n+1) acc buffer
		else if k.[n]=')' then
			acc@[buffer]
		else if k.[n]=',' then 
			tuple k (n+1) (acc@[buffer]) 0
		else 
			tuple k (n+1) (acc) (buffer*10 + int_of_string(Char.escaped k.[n]))
	in tuple s r [] 0 in
	let a=(List.hd x)in
	let b=(List.hd (List.tl x)) in 
		(a,b)
	
let ptupleid s r=
	let x=		
	let rec tuple k n acc buffer=	
		if k.[n]='(' then
			tuple k (n+1) acc buffer
		else if k.[n]=')' then
			acc@[buffer]
		else if k.[n]=',' then 
			tuple k (n+1) (acc@[buffer]) ""
		else 
			tuple k (n+1) (acc) (buffer ^ (Char.escaped k.[n]))
	in tuple s r [] "" in
	let a=int_of_string(List.hd x)in
	let b=int_of_string(List.hd (List.tl x)) in 
	let c=List.hd (List.tl (List.tl x)) in
		(a,b,c)
		
let ptupleint s r=
	let x=		
	let rec tuple k n acc buffer=
		if k.[n]='(' then
			tuple k (n+1) acc buffer
		else if k.[n]=')' then
			acc@[buffer]
		else if k.[n]=',' then 
			tuple k (n+1) (acc@[buffer]) ""
		else 
			tuple k (n+1) (acc) (buffer ^ (Char.escaped k.[n]))
	in tuple s r [] "" in
	let a=int_of_string(List.hd x)in
	let b=int_of_string(List.hd (List.tl x)) in 
	let c=int_of_string (List.hd (List.tl (List.tl x))) in
		(a,b,c)
		
let ptuplebool s r=
	let x=		
	let rec tuple k n acc buffer=	
		if k.[n]=')' then
			acc@[buffer]
		else if k.[n]=',' then 
			tuple k (n+1) (acc@[buffer]) ""
		else 
			tuple k (n+1) (acc) (buffer ^ (Char.escaped k.[n]))
	in tuple s r [] "" in
	let a=int_of_string(List.hd x)in
	let b=int_of_string(List.hd (List.tl x)) in 
	let c=List.hd (List.tl (List.tl x)) in
		if c="true" then
			(a,b,true)
		else
			(a,b,false)
	
let tokenise k=
	let kk=k.[0] in
	match kk with
	| 'U'->UNMINUS (ptuple k 8)
	| 'O'->OR(ptuple k 3)
	| 'C'->
		let kkk=k.[1] in
		if kkk='A' then
			CALL(ptuple k 5)
		else if kkk='O' then
			COMMA(ptuple k 6)
		else 
			raise Invalid_Token

	| 'W'->WHILE(ptuple k 6)
	| 'T'->THEN(ptuple k 5)
	| 'A'->
		let kkk=k.[1] in
		if kkk='S' then
			ASSIGN(ptuple k 7)
		else if kkk='N' then
			AND(ptuple k 4)
		else 
			raise Invalid_Token
	|'N'->
		let kkk=k.[2] in
		if kkk='G' then
			NEG(ptuple k 4)
		else if kkk='(' then
			NE(ptuple k 3)
		else 
			raise Invalid_Token
	|'P'->
		let kkk=k.[2] in
		if kkk='O' then
			PROC(ptuple k 5)
		else if kkk='I'then
			PRINT(ptuple k 6)	
		else 
			raise Invalid_Token		
	|'R'->
		let kkk=k.[1] in
		if kkk='E' then
			READ(ptuple k 5)
		else if kkk='P' then
			RP(ptuple k 3)
		else if kkk='B' then
			RB(ptuple k 3)	
		else 
			raise Invalid_Token	
	|'G'->
		let kkk=k.[2] in
		if kkk='E' then
			GTE(ptuple k 4)
		else if kkk='(' then
			GT(ptuple k 3)
		else 
			raise Invalid_Token
	|'L'->
		let kkk=k.[1] in
		if kkk='P' then
			LP(ptuple k 3)
		else if kkk='B' then
			LB(ptuple k 3)
		else if kkk='T' then
			let kkkk=k.[3] in
			if kkkk='E' then
				LTE(ptuple k 4)
			else if kkkk='(' then
				LT(ptuple k 3)
			else 
			raise Invalid_Token
		else 
			raise Invalid_Token
	|'I'->
		let kkk=k.[1] in
		if kkk='D' then
			IDENT(ptupleid k 6)
		else if kkk='F' then
			IF(ptuple k 3)
		else if kkk='N' then
			let kkkk=k.[3] in
			if kkkk='(' then
				INT(ptuple k 4)
			else if kkkk='L' then
				INTLIT(ptupleint k 7)
			else 
			raise Invalid_Token
		else 
			raise Invalid_Token
	|'E'->
		let kkk=k.[1] in
		if kkk='O' then
			EOS(ptuple k 4)
		else if kkk='Q' then
			EQ(ptuple k 3)
		else if kkk='L' then
			ELSE(ptuple k 5)
		else if kkk='R' then
			ERROR(ptupleint k 6)
		else 
			raise Invalid_Token
	|'B'->
		let kkk=k.[4] in
		if kkk='(' then
			BOOL(ptuple k 5)
		else if kkk='V' then
			BOOLVAL(ptuplebool k 8)
		else if kkk='D' then
			BINADD(ptuple k 7)
		else if kkk='I' then
			BINDIV(ptuple k 7)
		else if kkk='O' then
			BINMOD(ptuple k 7)
		else if kkk='U' then
			let kkkk=k.[5] in
			if kkkk='B' then
				BINSUB(ptuple k 7)
			else if kkkk='L' then
				BINMUL(ptuple k 7)
			else
			 raise Invalid_Token
		else 
			raise Invalid_Token
	|_->raise Invalid_Token
	
let create_toklist filename=
	let in_file = open_in filename in
	let rec read_file l=
		try 
			let nxt=input_line in_file in
			if nxt="" then
				read_file (l) 
			else
			 let nxt_tok=tokenise nxt in
			read_file ((nxt_tok)::l) 
		with End_of_file->
			close_in in_file;
			l		
	in List.rev (read_file []);;

type node=NONTERMINAL of (string*int)
|	INTLITERAL of (int * int)
| BOOLLITERAL of (string * int)
| ERROR of (int * string * int)

type symbol=KEYWORD of (string*string)
|	INTSYMBOL of string
| BOOLSYMBOL of string
| PROCSYMBOL of string

let rec intexp l acc n=
	let l1,l2=intT l [] (n+1) in
	let l3,l4= intE l2 [] (n+1) in
	(acc@[NONTERMINAL("IntT",n)]@l1@[NONTERMINAL("IntE",n)]@l3),l4
and intE l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| BINADD(p,q)->
				let l1,l2=intexp xs [] (n+1) in
				acc@[NONTERMINAL("BINADD",n);NONTERMINAL("cmma",n);NONTERMINAL("IntExpression",n)]@l1,l2
			| BINSUB(p,q)-> 
				let l1,l2=intexp xs [] (n+1) in
				acc@[NONTERMINAL("BINSUB",n);NONTERMINAL("cmma",n);NONTERMINAL("IntExpression",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)	
and intT l acc n=
	let l1,l2=intF l acc (n+1) in
	let l3,l4= intT1 l2 acc (n+1) in
	(acc@[NONTERMINAL("IntF",n)]@l1@[NONTERMINAL("IntT1",n)]@l3),l4
and intT1 l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| BINMOD(p,q)->
				let l1,l2=intT xs [] (n+1) in
				acc@[NONTERMINAL("BINMOD",n);NONTERMINAL("cmma",n);NONTERMINAL("IntT",n)]@l1,l2
			| BINDIV(p,q)-> 
				let l1,l2=intT xs [] (n+1) in
				acc@[NONTERMINAL("BINDIV",n);NONTERMINAL("cmma",n);NONTERMINAL("IntT",n)]@l1,l2
			| BINMUL(p,q)-> 
				let l1,l2=intT xs [] (n+1) in
				acc@[NONTERMINAL("BINMUL",n);NONTERMINAL("cmma",n);NONTERMINAL("IntT",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)			
and intF l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| UNMINUS(p,q)->
				let l1,l2=intF1 xs [] (n+1) in
				acc@[NONTERMINAL("UNMINUS",n);NONTERMINAL("cmma",n);NONTERMINAL("IntF1",n)]@l1,l2
			|_->
				let l1,l2=intF1 l [] (n+1) in
				acc@[NONTERMINAL("IntF1",n)]@l1,l2
			)
and intF1 l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| IDENT(p,q,r)->
				acc@[NONTERMINAL("IDENT ["^r^"]",n)],xs
			| INTLIT(p,q,r)->
				acc@[INTLITERAL(r,(n))],xs
			| LP(p,q)->
				let l1,l2=boolexp xs [] (n+1) in
				acc@[NONTERMINAL("LP",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@[NONTERMINAL("RP",n)],(List.tl l2)
			|_->
				acc@[ERROR(0,"IDENT or INTLIT or LP expected",n)],l
			)
and boolexp l acc n=
	let l1,l2=boolF l [] (n+1) in
	let l3,l4= boolE l2 [] (n+1) in
	(acc@[NONTERMINAL("BoolF",n)]@l1@[NONTERMINAL("BoolE",n)]@l3),l4
and boolE l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| OR(p,q)->
				let l1,l2=boolexp xs [] (n+1) in
				acc@[NONTERMINAL("OR",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)
and boolF l acc n=
	let l1,l2=boolG l acc (n+1) in
	let l3,l4= boolF1 l2 acc (n+1) in
	(acc@[NONTERMINAL("BoolG",n)]@l1@[NONTERMINAL("BoolF1",n)]@l3),l4
and boolF1 l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| AND(p,q)->
				let l1,l2=boolF xs [] (n+1) in
				acc@[NONTERMINAL("AND",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolF",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)
and boolG l acc n=
	let l1,l2=boolH l acc (n+1) in
	let l3,l4= boolG1 l2 acc (n+1) in
	(acc@[NONTERMINAL("BoolH",n)]@l1@[NONTERMINAL("BoolG1",n)]@l3),l4
and boolG1 l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| EQ(p,q)->
				let l1,l2=boolG xs [] (n+1) in
				acc@[NONTERMINAL("EQ",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolG",n)]@l1,l2
			| NE(p,q)->
				let l1,l2=boolG xs [] (n+1) in
				acc@[NONTERMINAL("NE",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolG",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)
and boolH l acc n=
	let l1,l2=boolI l acc (n+1) in
	let l3,l4= boolH1 l2 acc (n+1) in
	(acc@[NONTERMINAL("BoolI",n)]@l1@[NONTERMINAL("BoolH1",n)]@l3),l4
and boolH1 l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| GT(p,q)->
				let l1,l2=boolH xs [] (n+1) in
				acc@[NONTERMINAL("GT",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolH",n)]@l1,l2
			| LT(p,q)->
				let l1,l2=boolH xs [] (n+1) in
				acc@[NONTERMINAL("LT",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolH",n)]@l1,l2
			| GTE(p,q)->
				let l1,l2=boolH xs [] (n+1) in
				acc@[NONTERMINAL("GTE",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolH",n)]@l1,l2
			| LTE(p,q)->
				let l1,l2=boolH xs [] (n+1) in
				acc@[NONTERMINAL("LTE",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolH",n)]@l1,l2
			|_->acc@[NONTERMINAL("EPSILON",n)],l
			)
and boolI l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| NEG(p,q)->
				let l1,l2=boolJ xs [] (n+1) in
				acc@[NONTERMINAL("NEG",n);NONTERMINAL("cmma",n);NONTERMINAL("BoolJ",n)]@l1,l2
			|_->
				let l1,l2=boolJ l [] (n+1) in
				acc@[NONTERMINAL("BoolJ",n)]@l1,l2
			)
and boolJ l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with 
			| BOOLVAL(p,q,r)->
				if r=true then
					acc@[BOOLLITERAL("tt",(n))],xs
				else
					acc@[BOOLLITERAL("ff",(n))],xs
			|_->
				let l1,l2=intexp l acc (n+1) in
				acc@[NONTERMINAL("IntExpression",n)]@l1,l2
			)
		
let assigncmd l acc n=
		boolexp l (acc@[NONTERMINAL("BoolExpression",n)]) (n+1)
							
let rec whilecmd l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		let l1,l2=boolexp l ([]) (n+1) in
		let l3,l4=comm_seq l2 [] (n+1) in
		(acc@[NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@l3),l4
		
and ifcmd l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		let l1,l2=boolexp l ([]) (n+1) in
		(match List.hd l2 with
		| THEN(p,q)->
			let l3,l4=comm_seq (List.tl l2) [] (n+1) in
			(match (List.hd l4) with
			| ELSE(p,q)->
				let l5=(List.tl l4) in
				let l6,l7=comm_seq l5 [] (n+1) in
				(acc@[NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@[NONTERMINAL("cmma",n);NONTERMINAL("Then",n)]@l3@[NONTERMINAL("cmma",n);NONTERMINAL("Else",n)]@l6),l7
			|_->
				let l5=(l4) in
				let l6,l7=comm_seq l5 [] (n+1) in
				(acc@[NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@[NONTERMINAL("cmma",n);NONTERMINAL("Then",n)]@l3@[NONTERMINAL("cmma",n);ERROR(0,"Else Expected",n)]@l6),l7
				)
			|_->
			let l3,l4=comm_seq (l2) [] (n+1) in
			(match (List.hd l4) with
			| ELSE(p,q)->
				let l5=(List.tl l4) in
				let l6,l7=comm_seq l5 [] (n+1) in
				(acc@[NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@[NONTERMINAL("cmma",n);ERROR(0,"Then Expected",n)]@l3@[NONTERMINAL("cmma",n);NONTERMINAL("Else",n)]@l6),l7
			|_->
				let l5=(l4) in
				let l6,l7=comm_seq l5 [] (n+1) in
				(acc@[NONTERMINAL("cmma",n);NONTERMINAL("BoolExpression",n)]@l1@[NONTERMINAL("cmma",n);ERROR(0,"Then Expected",n)]@l3@[NONTERMINAL("cmma",n);ERROR(0,"Else Expected",n)]@l6),l7
				)
			)
				
and pcomm l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| EOS(p,q)->acc,l
		| PRINT(p,q)->
			let l1,l2=(pcomm xs [] (n+1)) in
			(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("PrintCmd",n);NONTERMINAL("Print",n+1);NONTERMINAL("cmma",n+1)]@l1@[NONTERMINAL("cmma",n-1)]),l2
		| READ(p,q)->
			let l1,l2=(pcomm xs [] (n+1)) in
				(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("ReadCmd",n);NONTERMINAL("Read",n+1);NONTERMINAL("cmma",n+1)]@l1@[NONTERMINAL("cmma",n-1)]),l2
		| CALL(p,q)->
			let l1,l2=(pcomm xs [] (n+1)) in
				(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("CallCmd",n);NONTERMINAL("Call",n+1);NONTERMINAL("cmma",n+1)]@l1@[NONTERMINAL("cmma",n-1)]),l2
		| LP(p,q)->
			let l1,l2=pcomm xs ([]) (n) in
			(acc@[NONTERMINAL("LP",n);NONTERMINAL("cmma",n)]@l1),l2
		| RP(p,q)->
			let l1,l2=pcomm xs ([]) (n) in
			(acc@[NONTERMINAL("RP",n)]@l1),l2
		| IDENT(p,q,r)->
			(match xs with
			| []->acc,[]
			| y::ys->
				(match y with
				| EOS(p,q)->
					let l1,l2=(pcomm xs [] (n+1)) in
					(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("CallCmd",n);NONTERMINAL("IDENT ["^r^"]",n+1);NONTERMINAL("cmma",n-1)]@l1),l2
				| ASSIGN(p,q)->
					let l1,l2=(assigncmd ys [] (n+2)) in
					(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("AssignmentCmd",n);NONTERMINAL("IDENT ["^r^"]",n+1);NONTERMINAL("cmma",n+1);NONTERMINAL("ASSIGN",n+1);NONTERMINAL("cmma",n+1);NONTERMINAL("Expression",(n+1))]@l1@[NONTERMINAL("cmma",n-1)]),l2
				| _->
					pcomm xs (acc@[NONTERMINAL("IDENT ["^r^"]",n);NONTERMINAL("cmma",n)]) n  ))		
		| IF(p,q)->
			let l1,l2=ifcmd xs [] (n+1) in
			(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("ConditionalCmd",n);NONTERMINAL("If",n+1)]@(l1)@[NONTERMINAL("cmma",n-1)]),l2
		| WHILE(p,q)->
			let l1,l2=(whilecmd xs [] (n+1)) in
			(acc@[NONTERMINAL("Command",(n-1));NONTERMINAL("WhileCmd",n);NONTERMINAL("While",n+1)]@l1@[NONTERMINAL("cmma",n-1)]),l2
		| _->acc,l
			)
				
and command l acc n flag bc f=
	if flag=false then
		match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| LB(p,q)->
			let l1,l2=command l ([]) (n) true (bc) true in
			acc@l1,l2
		| RB(p,q)->
			let l1,l2=command l ([]) (n) true (bc) false in
			acc@l1,l2
		|_->
			let l1,l2=command xs ([]) (n) true (bc) false in
			acc@l1,l2
			)
	else
		if bc=0 && f=false then acc,l
		else
		match l with
		| []->acc,[]
		| x::xs->
			(match x with
			|LB(p,q)->
				let l1,l2=pcomm xs [] (n+1) in
				let l3,l4=command l2 ([]) (n) true (bc+1) false in
				(acc@[NONTERMINAL("LB",n);NONTERMINAL("cmma",n);]@(l1)@l3),l4
			|EOS(p,q)->
				let l1,l2=pcomm xs [] (n+1) in
				let l3,l4=command l2 ([]) (n) true (bc) false in
				(acc@[NONTERMINAL("EOS",n);NONTERMINAL("cmma",n)]@(l1)@l3),l4 	
			|RB(p,q)->
				if bc<=1 then
					(acc@[NONTERMINAL("Command",n);NONTERMINAL("EPSILON",n+1);NONTERMINAL("cmma",n);NONTERMINAL("RB",n)]),xs
				else
					let l1,l2=pcomm xs [] (n+1) in
					let l3,l4=command l2 ([]) (n) true (bc-1) false in
					(acc@[NONTERMINAL("Command",n);NONTERMINAL("EPSILON",n+1);NONTERMINAL("cmma",n);NONTERMINAL("RB",n)]@(l1)@l3),l4 
			|_->
				let l1,l2=command xs ([]) (n) true (bc) false in
				acc@l1,l2
			)

and comm_seq l acc n=
	let l1,l2=command l [] (n+1) false (0) false in
	acc@[NONTERMINAL("CommandSeq",(n))]@(l1),l2

let rec int_decls l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| INT(p,q)->
			let l1,l2=
			int_decls2 xs ([]) (n+1) in
			acc@[NONTERMINAL("INT",n);NONTERMINAL("cmma",n);NONTERMINAL("VarDef",n)]@l1,l2
		|_->acc,l
			)
and int_decls2 l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| IDENT(p,q,r)->
			let l1,l2=
				int_decls3 xs ([]) (n+1) in
				acc@[NONTERMINAL("IDENT ["^r^"]",n);NONTERMINAL("cmma",n);NONTERMINAL("VarDef1",n)]@l1,l2
		|_->acc@[ERROR(0,"Error: IDENT Expected",n)],l
			)		
and int_decls3 l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| COMMA(p,q)->
			let l1,l2=
			int_decls2 xs ([]) (n+1) in
			acc@[NONTERMINAL("COMMA",n);NONTERMINAL("cmma",n);NONTERMINAL("VarDef",n)]@l1,l2
		| EOS(p,q)->
			acc@[NONTERMINAL("EOS",n)],xs
		|_->acc@[ERROR(0,"Error: , or ; Expected",n)],l
			)
		
let rec bool_decls l acc n=
	match l with
	| []->acc,[]
	| x::xs->
		(match x with
		| BOOL(p,q)->
			let l1,l2=
			int_decls2 xs ([]) (n+1) in
			acc@[NONTERMINAL("BOOL",n);NONTERMINAL("cmma",n);NONTERMINAL("VarDef",n)]@l1,l2
		|_->acc,l
			)
	
let var_decls l acc n=
	let l1,l2=int_decls l [] (n+2) in
	let l3,l4=bool_decls l2 [] (n+2) in
	if l1=[] then
		if l3=[] then
			acc@[NONTERMINAL("VarDecls",(n));NONTERMINAL("IntVarDecls",(n+1));NONTERMINAL("EPSILON",(n+2));NONTERMINAL("cmma",n);NONTERMINAL("BoolVarDecls",(n+1));NONTERMINAL("EPSILON",(n+2))],l4
		else
			acc@[NONTERMINAL("VarDecls",(n));NONTERMINAL("IntVarDecls",(n+1));NONTERMINAL("EPSILON",(n+2));NONTERMINAL("cmma",n);NONTERMINAL("BoolVarDecls",(n+1))]@(l3),l4
	else
		if l3=[] then
			acc@[NONTERMINAL("VarDecls",(n));NONTERMINAL("IntVarDecls",(n+1))]@(l1)@[NONTERMINAL("cmma",n);NONTERMINAL("BoolVarDecls",(n+1));NONTERMINAL("EPSILON",(n+2))],l4
		else
			acc@[NONTERMINAL("VarDecls",(n));NONTERMINAL("IntVarDecls",(n+1))]@(l1)@[NONTERMINAL("cmma",n);NONTERMINAL("BoolVarDecls",(n+1))]@(l3),l4

let rec proc_decls l acc n=
	match l with 
	| []->acc,[]
	| x::xs->
		(match x with
		| PROC(p,q)->
			proc_decls2 xs (acc@[NONTERMINAL("Proc",n)]) n
		|_->acc,l
		)
and proc_decls2 l acc n=
	match l with 
	| []->(acc@[ERROR(0,"IDENT expected",n)]),[]
	| x::xs->
		(match x with
		| IDENT(p,q,r)->
			proc_decls3 xs (acc@[NONTERMINAL("IDENT ["^r^"]",n)]) n
		|_->
			let l1,l2=proc_decls xs [] n in
			(acc@[ERROR(0,"IDENT expected",n)]@l1),l2
		)
and proc_decls3 l acc n=
	let l1,l2=program l [] (n+1) in
	match l2 with
	| []->
		 acc@l1,[]
	|x::xs->
		(match x with
		| EOS(p,q)->
			let l3,l4=proc_decls (List.tl l2) [] n in
			acc@l1@[NONTERMINAL("EOS",n)]@l3,l4
		|_->
			let l3,l4=proc_decls (List.tl l2) [] n in
			acc@l1@[ERROR(0,"; Expected",n)]@l3,l4
			)
	
and decl_seq l acc n=
	let l1,l2=var_decls l [] (n+1) in
	let l3,l4=proc_decls l2 [] (n+2) in
	if l3=[] then
		acc@[NONTERMINAL("DeclarationSeq",(n))]@(l1)@[NONTERMINAL("cmma",n);NONTERMINAL("ProcDecls",(n+1));NONTERMINAL("EPSILON",(n+2))],l4
	else
		acc@[NONTERMINAL("DeclarationSeq",(n))]@(l1)@[NONTERMINAL("cmma",n);NONTERMINAL("ProcDecls",(n+1))]@(l3),l4

and block l acc n=
	let l1,l2=decl_seq l [] (n) in
	let l3,l4=comm_seq l2 [] (n) in
	acc@(l1)@[NONTERMINAL("cmma",n);]@(l3),l4

and program l acc n=
	let l1,l2=block l [] (n+1) in
	acc@[NONTERMINAL("Block",n)]@l1,l2

let create_parsetree l acc n=
	let l1,l2=program l [] (n+1) in
	acc@[NONTERMINAL("Program",n)]@l1,l2
	
let parse filename=
	let l=create_toklist filename in
	create_parsetree l [] 1

let tostring node=
	match node with
	| NONTERMINAL("cmma",n)->",",n
	| NONTERMINAL(p,n)->p,n
	| INTLITERAL(p,n)->"IntLiteral ["^(string_of_int p)^"]",n
	| BOOLLITERAL(p,n)->"BoolLiteral ["^(p)^"]",n
	| ERROR(p,n,r)->n,r
						
let toString filename1 filename2=
	let nodelist,l2=parse filename1 in
	let output=open_out filename2 in
	let rec writef x cnt lst=
		match x with
		| []->
			if lst=0 then
				close_out output
			else
				let message=" ] " in
				Printf.fprintf output "%s \n" message; 
				writef x (cnt) (lst-1)
	  | y::ys->
			let message,n = tostring y in
			if n=cnt then
				let message,n=tostring y in
				Printf.fprintf output "%s " message; 
				writef ys	cnt n
			else if n<cnt then
				let message=" ] " in
				Printf.fprintf output "%s \n" message; 
				writef x (cnt-1) n
			else
				let message=" [ " in
				Printf.fprintf output "%s \n" message; 
				writef x (cnt+1) n
	in writef (nodelist) 0 0;;

let toSymbol filename1 filename2=
	let l=create_toklist filename1 in
	let output=open_out filename2 in
	let message="int     INT" in
	Printf.fprintf output "%s\n" message;
	let message="bool    BOOL" in
	Printf.fprintf output "%s\n" message; 
	let message="tt      BOOLVAL" in
	Printf.fprintf output "%s\n" message; 
	let message="ff      BOOLVAL" in
	Printf.fprintf output "%s\n" message; 
	let message="if      IF" in
	Printf.fprintf output "%s\n" message; 
	let message="then    THEN" in
	Printf.fprintf output "%s\n" message;
	let message="else    ELSE" in
	Printf.fprintf output "%s\n" message;
	let message="proc    PROC" in
	Printf.fprintf output "%s\n" message;
	let message="read    READ" in
	Printf.fprintf output "%s\n" message;
	let message="call    CALL" in
	Printf.fprintf output "%s\n" message; 
	let message="print    PRINT" in
	Printf.fprintf output "%s\n" message;
	let message="while   WHILE" in
	Printf.fprintf output "%s\n" message;
	let rec read_ident l typef globf decl flag=
		match l with
		| []->close_out output
		| x::xs->
			(match x with
			| INT(p,q)->read_ident xs "INT" globf decl true
			| BOOL(p,q)->read_ident xs "BOOL" globf decl true
			| PROC(p,q)->
				(match xs with
				| []->close_out output
				| y::ys->
					(match y with
					|IDENT(p,q,r)->
					let message=r^"     IDENT    PROC   "^globf in
					Printf.fprintf output "%s\n" message;
					read_ident ys "PROC" (globf^":"^r) (decl@[r]) true
					|_->close_out output
					)
					)
			|IDENT(p,q,r)->
			if flag=true then
				let message=r^"     IDENT    "^typef^"   "^globf in
				Printf.fprintf output "%s\n" message;
				read_ident xs typef globf (decl@[r]) true
			else read_ident xs typef globf decl false
			|COMMA(p,q)->read_ident xs typef globf decl true
			|_->read_ident xs typef globf decl false
			)


	in read_ident l ("INT") ("global") [] false;;

toString in_file_name out_file_name1;;
toSymbol in_file_name out_file_name2;;

(* let in_file_name= "C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\input2.txt"   *)
(* let out_file_name1= "C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\output.txt" *)
(* let out_file_name2= "C:\Python27\COL 226 Ocaml\Assignment 2 - Parser\output.txt" *)

(* let proc_decls l acc n=                             *)
(* 	match l with                                      *)
(* 	| []->acc,l                                       *)
(* 	| x::xs->                                         *)
(* 		(match x with                                   *)
(* 		| PROC(p,q)->                                   *)
(* 			let l1,l2=proc_decls xs [] (n) in             *)
(* 			acc@[NONTERMINAL("Proc",n)]@l1,l2             *)
(* 		| IDENT(p,q,r)->                                *)
(* 			let l1,l2=program l [] (n+1) in               *)
(* 			let l3,l4=proc_decls l2 [] (n) in             *)
(* 			acc@[NONTERMINAL("IDENT ["^r^"]",n)]@l1@l3,l2 *)
(* 		| EOS(p,q)->                                    *)
(* 			let l1,l2=proc_decls xs [] (n) in             *)
(* 			acc@[NONTERMINAL("EOS",n)]@l1,l2              *)
(* 		|_->acc,l                                       *)
(* 			)                                             *)

(* let rec isIntExp l=               *)
(* 		match l with                  *)
(* 		| []->true                    *)
(* 		| x::xs->                     *)
(* 			(match x with               *)
(* 			|IDENT(p,q,r)->isIntExp xs  *)
(* 			|INTLIT(p,q,r)->isIntExp xs *)
(* 			|UNMINUS(p,q)->isIntExp xs  *)
(* 			|BINADD(p,q)->isIntExp xs   *)
(* 			|BINSUB(p,q)->isIntExp xs   *)
(* 			|BINMUL(p,q)->isIntExp xs   *)
(* 			|BINDIV(p,q)->isIntExp xs   *)
(* 			|BINMOD(p,q)->isIntExp xs   *)
(* 			|LP(p,q)->isIntExp xs       *)
(* 			|RP(p,q)->isIntExp xs       *)
(* 			|EOS(p,q)->true             *)
(* 			| _->false                  *)
(* 			 )                          *)

(* let priority x=                   *)
(* 	match x with                    *)
(* 	|BINADD(p,q)->6                 *)
(* 	|BINSUB(p,q)->5                 *)
(* 	|BINMUL(p,q)->7                 *)
(* 	|BINDIV(p,q)->8                 *)
(* 	|BINMOD(p,q)->9                 *)
(* 	|UNMINUS(p,q)->10               *)
(* 	|LP(p,q)->8                     *)
(* 	|AND(p,q)->2                    *)
(* 	|OR(p,q)->1                     *)
(* 	|GT(p,q)->4                     *)
(* 	|LT(p,q)->4                     *)
(* 	|LTE(p,q)->4                    *)
(* 	|GTE(p,q)->4                    *)
(* 	|EQ(p,q)->3                     *)
(* 	|NE(p,q)->3                     *)
(* 	|NEG(p,q)->7                    *)
(* 	| _->0                          *)

(* let rec to_post inp stack out=                                                                  *)
(* 	match inp with                                                                                *)
(* 	| []->                                                                                        *)
(* 		out@stack,[]                                                                                *)
(* 	|x::xs->                                                                                      *)
(* 		(match x with                                                                               *)
(* 		| EOS(p,q)->out@stack,inp                                                                   *)
(* 		| LB (p,q)->out@stack,inp                                                                   *)
(* 		| RB(p,q)->out@stack,inp                                                                    *)
(* 		| THEN(p,q)->out@stack,inp                                                                  *)
(* 		| IDENT(p,q,r)->                                                                            *)
(* 			let l1,l2=to_post xs stack ([]) in                                                        *)
(* 			out@[IDENT(p,q,r)]@l1,l2                                                                  *)
(* 		| INTLIT(p,q,r)->                                                                           *)
(* 			let l1,l2=to_post xs stack ([]) in                                                        *)
(* 			out@[INTLIT(p,q,r)]@l1,l2                                                                 *)
(* 		| BOOLVAL(p,q,r)->                                                                          *)
(* 			let l1,l2=to_post xs stack ([]) in                                                        *)
(* 			out@[BOOLVAL(p,q,r)]@l1,l2                                                                *)
(* 		| _->                                                                                       *)
(* 			let p=priority x in                                                                       *)
(* 			(match stack with                                                                         *)
(* 			| []->to_post xs (x::(stack)) out                                                         *)
(* 			| y::ys->                                                                                 *)
(* 				let pp=priority y in                                                                    *)
(* 				if pp<=p then                                                                           *)
(* 					let l1,l2=to_post xs (x::(stack)) [] in                                               *)
(* 					out@l1,l2                                                                             *)
(* 				else                                                                                    *)
(* 					let l1,l2=to_post inp ys ([]) in                                                      *)
(* 					out@[y]@l1,l2                                                                         *)
(* 				)                                                                                       *)
(* 		)                                                                                           *)
						
(* let rec to_nodes x inp acc n=                                                                   *)
(* 	match inp with                                                                                *)
(* 	| []->                                                                                        *)
(* 		(match x with                                                                               *)
(* 		| IDENT(p,q,r)->                                                                            *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[NONTERMINAL("IDENT ["^r^"]",(n+1))]),[]            *)
(* 		| INTLIT(p,q,r)->                                                                           *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[INTLITERAL(r,(n+1))]),[]                           *)
(* 		|_->acc,[]                                                                                  *)
(* 		)		                                                                                       *)
(* 	| y::ys	->                                                                                   *)
(* 		(match x with                                                                               *)
(* 		| BINADD(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			let l4,l5=to_nodes (List.hd l3) (List.tl l3) [] (n+1) in                                  *)
(* 				(acc@[NONTERMINAL("AddExpression",n)]@(l4)@[NONTERMINAL("BINADD",(n+1))]@(l2)),l5			 *)
(* 		| BINSUB(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			let l4,l5=to_nodes (List.hd l3) (List.tl l3) [] (n+1) in                                  *)
(* 				(acc@[NONTERMINAL("SubExpression",n)]@(l4)@[NONTERMINAL("BINSUB",(n+1))]@(l2)),l5       *)
(* 		| BINMUL(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			let l4,l5=to_nodes (List.hd l3) (List.tl l3) [] (n+1) in                                  *)
(* 				(acc@[NONTERMINAL("MulExpression",n)]@(l4)@[NONTERMINAL("BINMUL",(n+1))]@(l2)),l5       *)
(* 		| BINDIV(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			let l4,l5=to_nodes (List.hd l3) (List.tl l3) [] (n+1) in                                  *)
(* 				(acc@[NONTERMINAL("DivExpression",n)]@(l4)@[NONTERMINAL("BINDIV",(n+1))]@(l2)),l5       *)
(* 		| BINMOD(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			let l4,l5=to_nodes (List.hd l3) (List.tl l3) [] (n+1) in                                  *)
(* 				(acc@[NONTERMINAL("ModExpression",n)]@(l4)@[NONTERMINAL("BINMOD",(n+1))]@(l2)),l5       *)
(* 		| UNMINUS(p,q)->                                                                            *)
(* 			let l2,l3=to_nodes y ys [] (n+1) in                                                       *)
(* 			(acc@[NONTERMINAL("MinExpression",n);NONTERMINAL("UNMINUS",(n+1))]@(l2)),l3               *)
(* 		| IDENT(p,q,r)->                                                                            *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[NONTERMINAL("IDENT ["^r^"]",(n+1))]),inp           *)
(* 		| INTLIT(p,q,r)->                                                                           *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[INTLITERAL(r,(n+1))]),inp                          *)
(* 		|_->acc,[]                                                                                  *)
(* 			)                                                                                         *)

(* let rec to_nodes2 x inp acc n=                                                                  *)
(* 	match inp with                                                                                *)
(* 	| []->                                                                                        *)
(* 		(match x with                                                                               *)
(* 		| IDENT(p,q,r)->                                                                            *)
(* 			(acc@[NONTERMINAL("Int/BoolExpression",n)]@[NONTERMINAL("IDENT ["^r^"]",(n+1))]),[]       *)
(* 		| INTLIT(p,q,r)->                                                                           *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[INTLITERAL(r,(n+1))]),[]                           *)
(* 		| BOOLVAL(p,q,r)->                                                                          *)
(* 			if r=true then                                                                            *)
(* 				(acc@[NONTERMINAL("BoolExpression",n)]@[BOOLLITERAL("tt",(n+1))]),[]                    *)
(* 			else                                                                                      *)
(* 				(acc@[NONTERMINAL("BoolExpression",n)]@[BOOLLITERAL("ff",(n+1))]),[]                    *)
(* 		|_->acc,[]                                                                                  *)
(* 		)		                                                                                       *)
(* 	| y::ys	->                                                                                   *)
(* 		(match x with                                                                               *)
(* 		| BINADD(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("AddExpression",n)]@(l4)@[NONTERMINAL("BINADD",(n+1))]@(l2)),l5			 *)
(* 		| BINSUB(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("SubExpression",n)]@(l4)@[NONTERMINAL("BINSUB",(n+1))]@(l2)),l5       *)
(* 		| BINMUL(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("MulExpression",n)]@(l4)@[NONTERMINAL("BINMUL",(n+1))]@(l2)),l5       *)
(* 		| BINDIV(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("DivExpression",n)]@(l4)@[NONTERMINAL("BINDIV",(n+1))]@(l2)),l5       *)
(* 		| BINMOD(p,q)->                                                                             *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("ModExpression",n)]@(l4)@[NONTERMINAL("BINMOD",(n+1))]@(l2)),l5       *)
(* 		|	GTE(p,q)->                                                                               *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("GTE",(n+1))]@(l2)),l5         *)
(* 		|	LTE(p,q)->                                                                               *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("LTE",(n+1))]@(l2)),l5         *)
(* 		|	GT(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("GT",(n+1))]@(l2)),l5          *)
(* 		|	LT(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("LT",(n+1))]@(l2)),l5          *)
(* 		|	AND(p,q)->                                                                               *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("AND",(n+1))]@(l2)),l5         *)
(* 		|	OR(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("OR",(n+1))]@(l2)),l5          *)
(* 		|	EQ(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("EQ",(n+1))]@(l2)),l5          *)
(* 		|	NE(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			let l4,l5=to_nodes2 (List.hd l3) (List.tl l3) [] (n+1) in                                 *)
(* 				(acc@[NONTERMINAL("CompExpression",n)]@(l4)@[NONTERMINAL("NE",(n+1))]@(l2)),l5          *)
(* 		| NEG(p,q)->                                                                                *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			(acc@[NONTERMINAL("NegExpression",n);NONTERMINAL("NEG",(n+1))]@(l2)),l3                   *)
(* 		| UNMINUS(p,q)->                                                                            *)
(* 			let l2,l3=to_nodes2 y ys [] (n+1) in                                                      *)
(* 			(acc@[NONTERMINAL("MinExpression",n);NONTERMINAL("UNMINUS",(n+1))]@(l2)),l3               *)
(* 		| IDENT(p,q,r)->                                                                            *)
(* 			(acc@[NONTERMINAL("Int/BoolExpression",n)]@[NONTERMINAL("IDENT ["^r^"]",(n+1))]),inp      *)
(* 		| INTLIT(p,q,r)->                                                                           *)
(* 			(acc@[NONTERMINAL("IntExpression",n)]@[INTLITERAL(r,(n+1))]),inp                          *)
(* 		|BOOLVAL(p,q,r)->                                                                           *)
(* 			if r=true then                                                                            *)
(* 				(acc@[NONTERMINAL("BoolExpression",n)]@[BOOLLITERAL("tt",(n+1))]),inp                   *)
(* 			else                                                                                      *)
(* 				(acc@[NONTERMINAL("BoolExpression",n)]@[BOOLLITERAL("ff",(n+1))]),inp                   *)
(* 		|_->acc,[]                                                                                  *)
(* 			)	                                                                                       *)
(* let rec intexp l acc n=			                                *)
(* 	match l with                                               *)
(* 	| []->acc,[]                                               *)
(* 	| x::xs->                                                  *)
(* 		let l1,l2=to_post l [] [] in                             *)
(* 		let l3=List.rev (l1) in                                  *)
(* 		let a,b = (to_nodes (List.hd l3) (List.tl l3) acc n) in  *)
(* 		a,l2                                                     *)
		
(* let rec boolexp l acc n=			                               *)
(* 	match l with                                               *)
(* 	| []->acc,[]                                               *)
(* 	| x::xs->                                                  *)
(* 		let l1,l2=to_post l [] [] in                             *)
(* 		let l3=List.rev (l1) in                                  *)
(* 		let a,b = (to_nodes2 (List.hd l3) (List.tl l3) acc n) in *)
(* 		a,l2                                                     *)

(*
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
*)