let() = if Array.length Sys.argv <> 3                                                                                                         
	then let () = print_string "Correct format: StateMachine.ml <input_file> <output_file for parse tree> <output_file for symbol table>\n" in exit 1
	else ();;                                                                                                                                   
let in_file_name = Sys.argv.(1);;                                                                                                             
let out_file_name= Sys.argv.(2);;                                                                                                             

#load "str.cma";;
#use "BigInt.ml";;

open BigInt;;
exception Empty_Stack_Exception;;

let create_inslist filename=
	let in_file = open_in filename in
	let rec read_file l=
		try 
			let nxt=input_line in_file in
			if nxt="" then
				read_file (l) 
			else
				read_file ((nxt)::l) 
		with End_of_file->
			close_in in_file;
			l		
	in List.rev (read_file [])
	
let pop l=
	match l with
	| []->raise Empty_Stack_Exception
	| x::xs->x,xs
	;;
		
let rec search y l=
	match l with
	| []->false
	| (x,x1,x2)::xs->
		if x=y && x2=0 then true
		else 
			search y xs
	;;

let rec get y l=
	match l with
	| []->raise Empty_Stack_Exception
	| (x,x1,x2)::xs->
		if x=y && x2=0 then x1
		else 
			get y xs
	;;
			
let rec search_proc l=
	match l with
	| []->false
	| (x,x1,x2)::xs->
		if x2=1 then true
		else 
			search_proc xs
	;;			
			
let rec get_proc l=
	match l with
	| []->raise Empty_Stack_Exception
	| (x,x1,x2)::xs->
		if x2=1 then x1,xs
		else 
			get_proc xs
	;;

let rec update z y l=
	match l with
	| []->[(y,z,0)]
	| (x,x1,x2)::xs->
		if x=y && x2=0 then (x,z,x2)::xs
		else 
			(x,x1,x2)::(update z y xs)
	;;

(* let in_file_name="C:\Python27\COL 226 Ocaml\Assignment 5 - Transitions\input.txt";;   *)
(* let out_file_name="C:\Python27\COL 226 Ocaml\Assignment 5 - Transitions\output.txt";; *)

(* let input= ["3";"ff"];; *)

let rec read_ins inslist pc symbol_table output=
	let instruction=Array.get inslist pc in
	let instr=Str.split (Str.regexp "[ \t]+") instruction in
	match instr with
	| w::x::y::[z]->
		(match w with 
		| "DECLARE_INT"->read_ins inslist (pc+1) ([x,"0",0]@symbol_table) output
		| "DECLARE_BOOL"->read_ins inslist (pc+1) ([x,"ff",0]@symbol_table) output
		| "DECLARE_PROC"->read_ins inslist (pc+1) ([x,y,0]@symbol_table) output
		| "PRINT"->
			let xx=get x symbol_table in
			read_ins inslist (pc+1) (symbol_table) (output@[xx])
		| "READ"->
			let inp=read_line() in
			(* let input=in2 in *)
			read_ins inslist (pc+1) (update inp z symbol_table) output
		| "CALL"->
			let xx=get x symbol_table in
			read_ins inslist (int_of_string xx) ([x,string_of_int (pc+1),1]@symbol_table) (output)
		| "IF"->
			let xx=get x symbol_table in
			if xx="tt" then
				read_ins inslist (pc+1) (symbol_table) (output)
			else
				read_ins inslist (int_of_string y) (symbol_table) (output)
		| "GOTO"->read_ins inslist (int_of_string y) symbol_table output
		| "RETURN"->
			(* let chk=search_proc symbol_table in *)
			(* if chk=true then                    *)
				let xx,ll=get_proc symbol_table in
				read_ins inslist (int_of_string xx) ll output
			(* else                                              *)
			(* 	read_ins inslist (pc+1) (symbol_table) (output) *)
		| "ASSIGN"->
			let chk=search x symbol_table in 
			if chk=false then
				read_ins inslist (pc+1) (update x z symbol_table) (output)
			else
				let xx=get x symbol_table in
				read_ins inslist (pc+1) (update xx z symbol_table) (output)
		| "PLUS"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=bi2str(add(str2bi xx,str2bi yy)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "MINUS"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=bi2str(sub(str2bi xx,str2bi yy)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "MULT"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=bi2str(mul(str2bi xx,str2bi yy)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "DIV"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=bi2str(div4bigint(str2bi xx,str2bi yy)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "MOD"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=bi2str(mod4bigint(str2bi xx,str2bi yy)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "GT"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=(gt(str2bi xx,str2bi yy)) in
			if xx="ff" then 
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
			else if xx="tt" then
				if yy="tt" then 
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
				else 
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)									
			else if zz=true then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "GEQ"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx=yy then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else if xx="tt" then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else if xx="ff" then
				if y="tt" then
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				let zz=(eq(str2bi xx,str2bi yy)) in
				if zz=true then
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "LT"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			let zz=(lt(str2bi xx,str2bi yy)) in
			if xx="tt" then 
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
			else if xx="ff" then
				if yy="tt" then 
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
				else 
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)									
			else if zz=true then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "LEQ"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx=yy then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else if xx="ff" then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else if xx="tt" then
				if y="ff" then
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				let zz=(leq(str2bi xx,str2bi yy)) in
				if zz=true then
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "EQ"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx=yy then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				let zz=(eq(str2bi xx,str2bi yy)) in
				if zz=true then
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "NEQ"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx=yy then
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
			else
				let zz=(neq(str2bi xx,str2bi yy)) in
				if zz=true then
					read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
				else
					read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		| "OR"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx="ff" then
				read_ins inslist (pc+1) (update yy z symbol_table) (output)
			else
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
		| "AND"->
			let xx=get x symbol_table in
			let yy=get y symbol_table in
			if xx="ff" then
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
			else
				read_ins inslist (pc+1) (update yy z symbol_table) (output)
		| "UMINUS"->
			let xx=get x symbol_table in
			let zz=bi2str(unminus(str2bi xx)) in
			read_ins inslist (pc+1) (update zz z symbol_table) (output)
		| "NOT"->
			let xx=get x symbol_table in
			if xx="ff" then
				read_ins inslist (pc+1) (update "tt" z symbol_table) (output)
			else
				read_ins inslist (pc+1) (update "ff" z symbol_table) (output)
		|"END_OF_CODE"->output
		| _->output
		 )	                                                                        
	|_->output

let read_prog input_file output_file=
	let inlist=create_inslist input_file in
	let inslist=Array.of_list inlist in
	let o=read_ins inslist 0 [] [] in	
	let output=open_out output_file in
	let rec writef x=
		match x with
		| []->close_out output
	  | y::ys->
			Printf.fprintf output "%s\n" y; 
			writef ys	
	in writef o;;

read_prog in_file_name out_file_name;;                                                                                                                                                                                                                                              