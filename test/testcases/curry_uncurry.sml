fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y 

fun eq a b = a == b 
fun bimap f (x, y) = (f x, f y) 

fun fact a = if (uncurry eq) (a, 0) then 1 else a * fact (a - 1) 

val main = 
  let 
    val (x, y) = curry (bimap fact) 5 7 
  in 
    printint x;
    print "\n";
    printint y;
    print "\n";
    0
  end 
