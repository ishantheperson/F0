fun eq_if a b c d = if a == b then c true else d true (* Using true to represent unit*)
fun ifz a c d = eq_if a 0 c d 

fun fact n = ifz n (fn b => 1) (fn b => n * fact (n-1))
val main = fact 10 
