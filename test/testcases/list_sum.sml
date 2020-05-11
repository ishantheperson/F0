datatype 'a list = Empty | Cons of 'a * ('a list)

fun sum l = 
  case l of 
    Empty () => 0 
  | Cons l2 => 
      let val (hd, tl) = l2 in 
      hd + (sum tl) 
      end 

fun tabulate f n = 
  if n == 0 
    then Empty ()
    else Cons (f n, tabulate f (n - 1))

fun compose f g = fn x => f (g x)

fun tabulateIdx f n = tabulate (fn x => (x, f x)) n  

fun id x = x 

val main = sum (tabulate id 53)

