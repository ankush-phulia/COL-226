module type Big_Int =
sig 
	type bigint
	val getbigint: int -> bigint
	val bi2str : bigint -> string
	val str2bi : string -> bigint
	val lt : bigint * bigint -> bool
	val leq : bigint * bigint -> bool
	val gt : bigint * bigint -> bool
	val geq : bigint * bigint -> bool
	val eq : bigint * bigint -> bool
	val neq : bigint * bigint -> bool
	val div4bigint : bigint * bigint -> bigint
	val mul : bigint * bigint -> bigint
	val add : bigint * bigint -> bigint
	val sub : bigint * bigint -> bigint
	val mod4bigint : bigint * bigint -> bigint
	val unminus : bigint -> bigint 
	exception Division_by_zero
end;;

module BigInt : Big_Int = struct
	
type sign=Plus|Minus	
type bigint = (sign*int list)

exception Division_by_zero;;

let getbigint (x):bigint=
	let rec tobigint x acc sign=
			if x=0 then (sign,acc)
			else tobigint (x/10) (acc@[(x mod 10)]) sign
	in
	if (x>0) then
		tobigint x [] Plus
	else if x=0 then
		Plus,[0]
	else
		tobigint (0-x) [] Minus
		
let bi2str (x:bigint)=
	let rec bigtostr y acc=
			match y with
			| []->acc
			| z::zs->
				bigtostr zs (acc^(string_of_int(z)))
	in
	match x with
	| (Plus,p)->
		bigtostr (List.rev p) ""
	| (minus,n)->
		bigtostr (List.rev n) "~"
	
let rec remz l n=
		match l with
		| []->[]
		| x::xs->
			if x>0 || n=1 then l
			else remz xs (n-1)

let str2bi x:bigint=
	let rec strtobig y acc n=
			if n>(String.length y)-1 then
				acc
			else
				let z=(y.[n]) in
				if ((int_of_char z)<58 && (int_of_char z)>47) then
					strtobig y ((int_of_string (Char.escaped z) )::acc) (n+1)
				else
					strtobig y acc (n+1)
	in
	let n=strtobig x [] 0 in
	let m=remz (List.rev n) (List.length n) in
	if x.[0]='~' && (not (m=[0])) then		
		(Minus,List.rev m)
	else
		(Plus,List.rev m)

let rec cmprl x1 x2 bool=
	let x3,x4=(remz (x1) (List.length x1)),(remz (x2) (List.length x2)) in
	if List.length x3<List.length x4 then true
	else if List.length x3>List.length x4 then false
	else
		match x3,x4 with
		| [],[]->bool
		| z::zs,w::ws->
			if z<w then true
			else if z=w then cmprl zs ws bool
			else false		
															
let lt ((x:bigint),(y:bigint))=
	match (x,y) with
	| (Plus,l1),(Plus,l2)->		 
		cmprl (List.rev l1) (List.rev l2) false
	| (Minus,l1),(Minus,l2)->		 
		cmprl (List.rev l1) (List.rev l2) false
	| (Plus,l1),(Minus,l2)->	
		false
	| (Minus,l1),(Plus,l2)->		 
		true
		
let leq ((x:bigint),(y:bigint))=
	match (x,y) with
	| (Plus,l1),(Plus,l2)->		 
		cmprl (List.rev l1) (List.rev l2) true
	| (Minus,l1),(Minus,l2)->		 
		cmprl (List.rev l1) (List.rev l2) true
	| (Plus,l1),(Minus,l2)->	
		false
	| (Minus,l1),(Plus,l2)->		 
		true
		
let gt ((x:bigint),(y:bigint))=
	not(leq (x,y))	
	
let geq ((x:bigint),(y:bigint))=
	not(lt (x,y))	

let eq (x,y)=
	x=y
	
let neq (x,y)=
	not (eq (x,y))

let unminus (x:bigint):bigint=
	match x with
	| (Plus,y)->(Minus,y)
	| (Minus,y)->(Plus,y)

let rec addh a b s cin=
	match (a,b) with
	| [],[]->
			if cin=0 then s
			else s@[cin]
	| x::xs,[]->addh a [cin] s 0
	| [],y::ys->addh [cin] b s 0
	| x::xs,y::ys->addh xs ys (s@[(x+y+cin) mod 10]) ((x+y+cin)/10)

let rec complement x=
	match x with
	| []->[]
	| y::ys->
		(9-y)::(complement ys)
		
let rec pad bb n=
	if n=0 then bb
	else bb@(pad [9] (n-1))				
														
let rec subh a b=
	let bb=complement b in
	let bbb=pad bb (List.length a-List.length b) in
	let c=addh a bbb [] 1	in
	let cc=(List.tl (List.rev c)) in
	let ccc=remz cc (List.length cc) in
	List.rev ccc
												
let add ((x1:bigint),(x2:bigint)):bigint=
	let s1,l3=x1 in
	let s2,l4=x2 in
	let l1,l2=List.rev (remz (List.rev l3) (List.length l3)),List.rev (remz (List.rev l4) (List.length l4)) in
	match (s1,s2) with
	| (Plus,Plus)->		 
		let z=addh l1 l2 [] 0 in
		(Plus,z)
	| (Minus,Minus)->		 
		let z=addh l1 l2 [] 0 in
		(Minus,z)
	| (Plus,Minus)->
		let chk=geq ((Plus,l1),(Plus,l2))	in	
		if chk=true then 
			let z=subh l1 l2 in
			(Plus,z)
		else
			let z=subh l2 l1 in
			(Minus,z)		
	| (Minus,Plus)->		 
		let chk=geq ((Plus,l1),(Plus,l2))	in	
		if chk=true then 
			let z=subh l1 l2 in
			(Minus,z)
		else
			let z=subh l2 l1 in
			(Plus,z)			

let sub ((x1:bigint),(x2:bigint)):bigint=
	add(x1,unminus(x2))

let rec bmul l x acc cin=
	match l with
	| []->
		if cin=0 then acc
		else acc@[cin]
	| y::ys->
		bmul ys x (acc@[((y*x)+cin)mod 10]) (((x*y)+cin)/10)

let rec shift l n acc=
	if n=0 then 
		acc@l
	else
		shift l (n-1) (acc@[0])
				
let mul ((x1:bigint),(x2:bigint)):bigint=
	let s1,l1=x1 in
	let s2,l2=x2 in
	let rec mulh y1 y2 acc n=
		match y2 with
		| []->acc
		| z::zs->
			let zz=bmul y1 z [] 0 in
			let zzz=shift zz n [] in
			mulh y1 zs (addh acc zzz [] 0) (n+1)
	in let l3=mulh l1 l2 [] 0 in
	let l4=List.rev (remz (List.rev l3) (List.length l3)) in
	if s1=s2 then
		Plus,l4
	else 
		Minus,l4

let rec bdiv l1 l2 x=
	if (cmprl (List.rev l1) (List.rev l2) false) then x,l1
	else bdiv (subh l1 l2) l2 (x+1)

let divbyten l=
	match l with 
	| []->[]
	| [0]->[0]
	| x::xs->xs
		
let division ((x1:bigint),(x2:bigint)):(bigint*bigint)=
	if x2=(Plus,[0]) then
		raise Division_by_zero
	else
		let s1,l1=x1 in
		let s2,l2=x2 in
		let l3,l4=List.rev (remz (List.rev l1) (List.length l1)),List.rev (remz (List.rev l2) (List.length l2)) in
		if l4=([0]) then
			raise Division_by_zero
		else
			let l5=shift l4 (List.length l3-List.length l4) [] in
			let rec divh y1 y2 quot n=
				if n<0 then quot,y1
				else
					let (q),r=bdiv y1 y2 0 in
					divh (r) (divbyten y2) (q::quot) (n-1)
			in
			let qq,rr=divh l3 l5 [0] (List.length l3 - List.length l4) in
			let qqq,rrr=List.rev (remz (List.rev qq) (List.length qq)),List.rev (remz (List.rev rr) (List.length rr)) in
			if s1=s2 then
				(Plus,qqq),(s1,rrr)
			else 
				(Minus,qqq),(s1,rrr)

let div4bigint ((x1:bigint),(x2:bigint)):bigint= 	
	let a,b=division (x1,x2) in a	
		
let mod4bigint ((x1:bigint),(x2:bigint)):bigint=
	let a,b=division (x1,x2) in b	

end ;;