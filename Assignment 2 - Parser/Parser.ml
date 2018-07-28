let() = if Array.length Sys.argv <> 4                                                                                                          
then let () = print_string "Correct format: Parser.ml <input_file> <output_file for parse tree> <output_file for symbol table>\n" in exit 1 
else ();;                                                                                                                                    

let in_file_name = Sys.argv.(1);;
let out_file_name1= Sys.argv.(2);;  
let out_file_name2 = Sys.argv.(3);; 

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