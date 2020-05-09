fun loop f n = 
  if n == 0 then () 
  else 
      f n;  
      loop f (n - 1)  

val print_loop = loop (fn i => 
  print "Iteration ";
  printint i;
  println ""
)

val main = 
    print_loop 10 ;
    0
  
