fun package a b = (a, b)

val packaged = package 1 "hello"
val complex= package (fn x => x) (1, true, "hello")

val (a, b) = ("first", "second")
fun add4 (x, y) (a, b) = x + y + a + b 

val add2 = fn (a, b) => a + b 

val main = add4 (1, 2) (3, 4) + add2 (5 ,6)
