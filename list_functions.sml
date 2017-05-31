fun sum_list (xs : int list) =
  if null xs
  then 0
  else hd xs + sum_list(tl xs)


fun list_product (a : int list) =
  if null a
  then 1
  else hd a * list_product(tl a)

fun countdown (x : int) =
  if x = 0
  then []
  else x :: countdown (x - 1)

fun append (xs : int list, ys : int list) =
  if null xs
  then ys
  else (hd xs) :: append((tl xs), ys)

fun sum_pair_list (xs : (int * int) list) =
  if null xs
  then 0
  else #1 (hd xs) + #2 (hd xs) + sum_pair_list (tl xs)
(* sum_pair_list [(3,4),(5,6)]*)


					       
					       

		      

		      
      

			
      
	  
