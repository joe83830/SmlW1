fun is_older (pr1 : int*int*int , pr2 : int*int*int) =
  if (#1 pr1) < (#1 pr2)
  then true
  else if (((#1 pr1) = (#1 pr2)) andalso ((#2 pr1) < (#2 pr2)))
  then true 
  else if (((#2 pr1) = (#2 pr2)) andalso ((#3 pr1) < (#3 pr2)))
  then true
  else false

fun number_in_month (xs : (int*int*int) list, y : int) =
  
  if null xs
  then 0	   
  else if (#2 (hd xs) = y)
  then 1 + number_in_month (tl xs, y)
  else 0 + number_in_month (tl xs, y)

fun number_in_months (xs : (int*int*int) list, ys : int list) =
  if ((xs = []) orelse (ys = []))
  then 0
  else number_in_month (xs, hd ys) + number_in_months (xs, tl ys)

fun dates_in_month (xs : (int*int*int) list, y : int) =
  if null xs
  then []
  else if ((#2 (hd xs)) = y)
  then (hd xs) :: dates_in_month (tl xs, y)
  else dates_in_month (tl xs, y)
		      
fun dates_in_months (xs : (int*int*int) list, ys : int list) =
  if ((xs = []) orelse (ys = []))
  then []
  else dates_in_month (xs, hd ys) @ dates_in_months (xs, tl ys)


fun get_nth (xs : string list, y : int) =
  if y = 1
  then hd xs
  else get_nth (tl xs, y - 1)

fun date_to_string (pr : int*int*int) =
  get_nth (["January ", "February ", "March ", "April ", "May ", "June ", "July ", "August ", "September ", "October ", "November ", "December "], #2 pr) ^ Int.toString(#3 pr) ^ ", " ^ Int.toString(#1 pr)

fun number_before_reaching_sum (x : int, ys : int list) =
  if hd ys >= x
  then 0
  else 1 + number_before_reaching_sum (x - hd ys, tl ys)

fun what_month (days : int) =
  let val month_list = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum (days, month_list) + 1
  end

fun month_range (d1 : int, d2 : int) =
  if (d1 > d2)
  then []
  else what_month (d1) :: month_range (d1 + 1, d2)

				      

fun oldest (dates : (int * int * int) list) =
  if null dates then NONE
  else
      let fun oldest_assist (dates: (int * int * int) list) =
            if null (tl dates) then hd dates
            else
		let val tail_oldest = oldest_assist (tl dates)
		in
                    if is_older (hd dates, tail_oldest) then hd dates
                    else tail_oldest
		end
      in
          SOME (oldest_assist (dates))
      end
	  
      

      
			   
      
      
  


						    
  

				 
					      
								    
								    
				      

      
      
      
			   
	       
	       
		
	      
		     
	     
	     
	     
      

      
      
      
      
