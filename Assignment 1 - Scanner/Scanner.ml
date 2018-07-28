let() = if Array.length Sys.argv <> 3
then let () = print_string "Correct format: Scanner.ml <input_file> <output_file>\n" in exit 1
else ();;                                                               

let in_file_name=Sys.argv(1);;
let out_file_name=Sys.argv(2);;

let create_inlist (filename:string)=
	let in_file = open_in filename in
	let in_stream = Stream.of_channel in_file in
	let rec read_file (l:((char * int * int) list)) (y:int) (x:int) (b1:bool) (s2:bool)=
		try
			let nxt=Stream.next in_stream in
			if nxt='\n' then
					read_file (l@[('\n',y,x)]) (y+1) 1 b1 s2
			else 
				if b1=false then
					if nxt='(' then 
						let look_ahead=Stream.next in_stream in
					 	if look_ahead='*' then
							read_file l y (x+1) true s2
						else if look_ahead='(' then
							read_file (l@[('(',y,x)]) y (x+1) true s2
						else 
							read_file (l@[('(',y,x);(look_ahead,y,x+1)]) y (x+2) false s2
					else 
						read_file (l@[nxt,y,x]) y (x+1) false s2
				else
					if s2=false then 
						if nxt='*' then
							let look_ahead=Stream.next in_stream in
							if look_ahead=')' then
								read_file l y (x+1) false false
							else if look_ahead='*' then
								read_file l y (x+1) true true
							else
								read_file l y (x+1) true false
						else
							read_file l y (x+1) true false
					else 
						if nxt='*' then
							read_file l y (x+1) true true
						else if nxt=')' then
							read_file l y (x+1) false false
						else
							read_file l y (x+1) true false								
		with Stream.Failure ->
			close_in in_file;
			l
	in read_file [] 1 1 false false;;

let rec create_lexlist (m:int) (n:int) (inp:((char * int * int) list)) (outp:((string * int * int) list)) (buffer:string)=
	match inp with
	| []->
		if buffer="" then
			outp 
		else
			outp@[buffer,m,n]
	| (x,p,q)::xs->
		(match x with
		|' '->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]) ("")
		|'\n'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]) ("")
		|'\t'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]) ("")
		|'+'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["+",p,q]) ("")
		|'-'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["-",p,q]) ("")
		|'/'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["/",p,q]) ("")
		|'*'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["*",p,q]) ("")
		|'%'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["%",p,q]) ("")
		|'('->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["(",p,q]) ("")
		|')'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@[")",p,q]) ("")
		|'['->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["[",p,q]) ("")
		|']'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["]",p,q]) ("")
		|'{'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["{",p,q]) ("")
		|'}'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["}",p,q]) ("")
		|'='->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["=",p,q]) ("")
		|','->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@[",",p,q]) ("")
		|';'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@[";",p,q]) ("")
		|'!'->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["!",p,q]) ("")
		|'&'->
			(match xs with
			| []->create_lexlist m n xs (outp) (buffer^(Char.escaped x))
			|(y,mm,nn)::ys->
				if y='&' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@["&&",p,q]) ("")
				else
					create_lexlist m n xs (outp) (buffer^(Char.escaped x))
				)
		|'|'->
			(match xs with
			| []->create_lexlist m n xs (outp) (buffer^(Char.escaped x))
			|(y,mm,nn)::ys->
				if y='|' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@["||",p,q]) ("")
				else
					create_lexlist m n xs (outp) (buffer^(Char.escaped x))
				)
		|':'->
			(match xs with
			| []->create_lexlist m n xs (outp) (buffer^(Char.escaped x))
			|(y,mm,nn)::ys->
				if y='=' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@[":=",p,q]) ("")
				else
					create_lexlist m n xs (outp) (buffer^(Char.escaped x))
				)
		|'>'->
			(match xs with
			| []->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@[">",p,q]) ("")
			|(y,mm,nn)::ys->
				if y='=' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@[">=",p,q]) ("")
				else
					create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@[">",p,q]) ("")
				)
		|'<'->
			(match xs with
			| []->create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["<",p,q]) ("")
			|(y,mm,nn)::ys->
				if y='=' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@["<=",p,q]) ("")
				else if y='>' then
					create_lexlist p (q+2) ys (outp@[buffer,p,q-String.length buffer]@["<>",p,q]) ("")
				else
					create_lexlist p (q+1) xs (outp@[buffer,p,q-String.length buffer]@["<",p,q]) ("")
				)
		|_->create_lexlist m n xs (outp) (buffer^(Char.escaped x))
		)			;;

type token = UNMINUS of int * int
	| BINADD of int * int
	| BINSUB of int * int
	| BINDIV of int * int
	| BINMUL of int * int
	| BINMOD of int * int
	| NEG of int * int
	| AND of int * int
	| OR of int * int
	| ASSIGN of int * int
	| EQ of int * int
	| NE of int * int
	| LT of int * int
	| LTE of int * int
	| GT of int * int
	| GTE of int * int
	| LP of int * int
	| RP of int * int
	| LB of int * int
	| RB of int * int
	| EOS of int * int
	| COMMA of int * int
	| INT of int * int
	| BOOL of int * int
	| IF of int * int
	| THEN of int * int
	| ELSE of int * int
	| WHILE of int * int
	| PROC of int * int
	| PRINT of int * int
	| READ of int * int
	| ERROR of int * int * int
	| INTLIT of int * int * int
	| IDENT of int * int * string
	| BOOLVAL of int * int * bool;;

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
		
let isID a=
	if isInteger (Char.escaped a.[0]) then
		false
	else
		let rec isid y n=
			if n<String.length y then
			let x=y.[n] in
				if (x>='a' && x<='z')||(x>='A' && x<='Z')||(x>='0' && x<='9') then
					isid y (n+1)
				else 
			 	 false
			else
				true
	in isid a 0;;
												
let rec scan (l:((string * int * int) list)) (outp:token list)=
	match l with
	| []->outp
	| x::xs->
		match x with
		| ("",p,q)->scan xs outp
		| ("~",p,q)->scan xs (outp@[UNMINUS(p,q)])
		| ("+",p,q)->scan xs (outp@[BINADD(p,q)])
		| ("-",p,q)->scan xs (outp@[BINSUB(p,q)])
		| ("/",p,q)->scan xs (outp@[BINDIV(p,q)])
		| ("*",p,q)->scan xs (outp@[BINMUL(p,q)])
		| ("%",p,q)->scan xs (outp@[BINMOD(p,q)])
		| ("!",p,q)->scan xs (outp@[NEG(p,q)])
		| ("=",p,q)->scan xs (outp@[EQ(p,q)])
		| ("(",p,q)->scan xs (outp@[LP(p,q)])
		| (")",p,q)->scan xs (outp@[RP(p,q)])
		| ("{",p,q)->scan xs (outp@[LB(p,q)])
		| ("}",p,q)->scan xs (outp@[RB(p,q)])
		| (";",p,q)->scan xs (outp@[EOS(p,q)])
		| (",",p,q)->scan xs (outp@[COMMA(p,q)])
		| ("<>",p,q)->scan xs (outp@[NE(p,q)])
		| ("<=",p,q)->scan xs (outp@[LTE(p,q)])
		| (">=",p,q)->scan xs (outp@[GTE(p,q)])
		| ("<",p,q)->scan xs (outp@[LT(p,q)])
		| (">",p,q)->scan xs (outp@[GT(p,q)])
		| ("&&",p,q)->scan xs (outp@[AND(p,q)])
		| ("||",p,q)->scan xs (outp@[OR(p,q)])
		| (":=",p,q)->scan xs (outp@[ASSIGN(p,q)])
		| ("if",p,q)->scan xs (outp@[IF(p,q)])
		| ("then",p,q)->scan xs (outp@[THEN(p,q)])
		| ("else",p,q)->scan xs (outp@[ELSE(p,q)])
		| ("int",p,q)->scan xs (outp@[INT(p,q)])
		| ("bool",p,q)->scan xs (outp@[BOOL(p,q)])
		| ("while",p,q)->scan xs (outp@[WHILE(p,q)])
		| ("read",p,q)->scan xs (outp@[READ(p,q)])
		| ("print",p,q)->scan xs (outp@[PRINT(p,q)])
		| ("proc",p,q)->scan xs (outp@[PROC(p,q)])
		| ("tt",p,q)->scan xs (outp@[BOOLVAL(p,q,true)])
		| ("ff",p,q)->scan xs (outp@[BOOLVAL(p,q,false)])
		| (y,p,q)->
			(*if y.[0]='i' then
				scan xs (outp@(accept_i y p q 1 "i"))
			else if y.[0]='t' then
				scan xs (outp@(accept_t y p q 1 "t"))
			else if y.[0]='e' then
				scan xs (outp@(accept_e y p q 1 "e"))
			else if y.[0]='f' then
				scan xs (outp@(accept_f y p q 1 "f"))
			else if y.[0]='b' then
				scan xs (outp@(accept_b y p q 1 "b"))
			else if y.[0]='p' then
				scan xs (outp@(accept_p y p q 1 "p"))
			else if y.[0]='r' then
				scan xs (outp@(accept_r y p q 1 "r"))
			else if y.[0]='w' then
				scan xs (outp@(accept_w y p q 1 "w"))
		 else if y.[0]='b' then
				scan xs (outp@(accept_b y p q 1 "b"))
		 else	
			scan xs (scan_lex y p q (y.[0]) 0 [])*)
			let chkint=isInteger y in
				if chkint=true then
					scan xs (outp@[INTLIT(p,q,int_of_string y)])
				else
					let chkid=isID y in
						if chkid=false then
							scan xs (outp@[ERROR(p,q,String.length y)])
						else
							scan xs (outp@[IDENT(p,q,y)]);;
	
let tostring token =
match token with
UNMINUS(a,b) -> "UNMINUS(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BINADD(a,b) -> "BINADD(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BINSUB(a,b) -> "BINSUB(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BINDIV(a,b) -> "BINDIV(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BINMUL(a,b) -> "BINMUL(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BINMOD(a,b) -> "BINMOD(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| NEG(a,b) -> "NEG(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| AND(a,b) -> "AND(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| OR(a,b) -> "OR(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| ASSIGN(a,b) -> "ASSIGN(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| EQ(a,b) -> "EQ(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| NE(a,b) -> "NE(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| LT(a,b) -> "LT(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| LTE(a,b) -> "LTE(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| GT(a,b) -> "GT(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| GTE(a,b) -> "GTE(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| LP(a,b) -> "LP(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| RP(a,b) -> "RP(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| LB(a,b) -> "LB(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| RB(a,b) -> "RB(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| EOS(a,b) -> "EOS(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| COMMA(a,b) -> "COMMA(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| INT(a,b) -> "INT(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| BOOL(a,b) -> "BOOL(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| IF(a,b) -> "IF(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| THEN(a,b) -> "THEN(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| ELSE(a,b) -> "ELSE(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| WHILE(a,b) -> "WHILE(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| PROC(a,b) -> "PROC(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| PRINT(a,b) -> "PRINT(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| READ(a,b) -> "READ(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")\n"
| ERROR(a,b,c)->"ERROR(" ^ string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_int c ^")\n"
| INTLIT(a,b,c)->"INTLIT(" ^ string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_int c ^")\n"
| IDENT(a,b,c)->"IDENT(" ^ string_of_int a ^ "," ^ string_of_int b ^ "," ^ c ^ ")\n"
| BOOLVAL(a,b,c)->"BOOLVAL("^ string_of_int a ^ "," ^ string_of_int b ^ "," ^ string_of_bool c ^")\n"
;;

let in_file_name = Sys.argv.(1);;
let out_file_name = Sys.argv.(2);;

let scanner (filename:string)=
	let inlist=create_inlist filename in
	let lexlist=create_lexlist 1 1 inlist [] "" in
	scan lexlist [];;
	
let toString filename1 filename2=
	let tokenlist=scanner filename1 in
	let output=open_out filename2 in
	let rec writef x=
		match x with
		| []->close_out output
	  | y::ys->
			let message = tostring y in
			Printf.fprintf output "%s" message; 
			writef ys	
	in writef tokenlist;;

toString in_file_name out_file_name;;