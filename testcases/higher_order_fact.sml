fun eq_if a b c d = if a == b then c () else d ()
fun ifz a c d = eq_if a 0 c d 

fun fact n = ifz n (fn () => 1) (fn () => n * fact (n-1))
val main = fact 10 
