fun is_older(date1 : int*int*int, date2 : int*int*int) =
    (#1 date1) < (#1 date2)
    orelse (#1 date1)=(#1 date2) andalso (#2 date1) < (#2 date2)
    orelse (#1 date1)=(#1 date2) andalso (#2 date1)=(#2 date2) andalso (#3 date1)<(#3 date2)

fun number_in_month(listdates : (int*int*int) list, month: int) =
    if(null listdates) then 0
    else
	let
	 val day = (hd listdates)
	 val temp = if((#2 day)=month) then 1 else 0
	in	    
	 temp+number_in_month((tl listdates), month)
	end

fun number_in_months(listdates : (int*int*int) list, listmonths: int list) =
    if(null listmonths) then 0
    else
	number_in_month(listdates, hd listmonths)+number_in_months(listdates, tl listmonths)

fun dates_in_month(listdates: (int*int*int) list, month: int) =
    if(null listdates) then []
    else
	if((#2 (hd listdates))=month) then (hd listdates)::dates_in_month(tl listdates, month)
	else dates_in_month(tl listdates, month)

fun dates_in_months(listdates: (int*int*int) list, listmonths: int list) =
    if(null listmonths) then []
    else
	dates_in_month(listdates, hd listmonths)@dates_in_months(listdates, tl listmonths)

fun get_nth(alist: string list, n: int) =
    if(n=1) then hd alist
    else get_nth(tl alist, n-1)


fun date_to_string(adate: int*int*int) =
    let
	val alist=["January", "February", "March", "April", "May", "June", "July", "August", "September","October","November", "December"]
	fun date_to_stringhelper(bdate: int*int*int) =
	    get_nth(alist, (#2 bdate))^" "^Int.toString((#3 bdate))^", "^Int.toString((#1 bdate))
    in
	date_to_stringhelper(adate)
    end

fun number_before_reaching_sum(sum: int, alist: int list) =
    let
	fun  number_before_reaching_sum_helper(sum: int, alist: int list, idx: int) =
	     if((sum-(hd alist))<=0 orelse null alist) then idx
	     else
		 number_before_reaching_sum_helper(sum-(hd alist), tl alist, idx+1)
    in
	number_before_reaching_sum_helper(sum, alist, 0)
    end

fun what_month(day: int) =
    let
	val alist = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    in
	1+number_before_reaching_sum(day, alist)
    end


fun month_range(day1: int, day2: int) =
    if(day1>day2) then []
    else what_month(day1)::month_range(day1+1, day2)
	

fun oldest(alist: (int*int*int) list) =
    if(null alist) then NONE
    else
	let
	    fun find_oldest(alist: (int*int*int) list, adate: (int*int*int)) =
		if(null alist) then adate
		else if(is_older(hd alist, adate)) then find_oldest(tl alist, hd alist)
		else find_oldest(tl alist, adate)
	in
	    SOME (find_oldest(alist, hd alist))
	end


datatype mytype = TwoInts of int * int
		| Str of string
		| Pizza

fun f x =
    case x of
	Pizza => 3
      | TwoInts(i1, i2) => i1+i2
      | Str s => String.size s

datatype suit = Club | Diamond | Heart | Spade
datatype card_value = Jack | Queen | King | Ace | Num of int

datatype exp = Constant of int
	     | Negate of exp
	     | Add of exp*exp
	     | Multiply of exp*exp

fun eval e =
    case e of
	Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add(e1, e2) => (eval e1) + (eval e2)
      | Multiply(e1, e2) => (eval e1) * (eval e2)

datatype my_int_list = Empty | Cons of int * my_int_list

val x = Cons(4, Cons(23, Cons(2008, Empty)))

fun append_my_list(xs, ys) =
    case xs of
	Empty => ys
      | Cons(x, xs') => Cons(x, append_my_list(xs',ys))

fun inc_or_zero intoption =
    case intoption of
	NONE => 0
      | SOME i => i+1

exception ListLengthMismatch
		  
fun zip3 lists =
    case lists of
	([],[],[]) => []
      | (hd1::t11, hd2::t12, hd3::t13) =>
	(hd1, hd2, hd3)::zip3(t11, t12, t13)
      | _ => raise ListLengthMismatch

fun unzip3 triples =
    case triples of
	[] => ([], [], [])
      | (a,b,c)::t1 =>
	let val (l1, l2, l3) = unzip3 t1
	in
	    (a::l1, b::l2, c::l3)
	end

fun fact n =
    let fun aux(n, acc) =
	    if n =0
	    then acc
	    else aux(n-1, acc*n)
    in
	aux(n,1)
    end

val x = fact 3

fun sum xs =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs', x+acc)
    in
	 aux(xs, 0)
    end

fun rev xs =
    let fun aux(xs, acc) =
	    case xs of
		[] => acc
	      | x::xs' => aux(xs', x::acc)
    in
	aux(xs, [])
    end
    
    
			
			    
		  
		    
    
	
    
	     
	
												