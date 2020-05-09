fun fib a b i stop = 
  if i == stop then () 
  else print "fib(";
       printint i;
       print ") = ";
       printint (a + b);
       print "\n";
       fib b (a + b) (i + 1) stop 

val main = fib 0 1 2 20 ; 0
