datatype ('a, 'b) either = Left of 'a | Right of 'b 

fun bind x f = 
  case x of 
    Left e => Left e  
  | Right x => f x

fun return x = Right x 

fun validate_int x = 
  if x <= 0
    then Left "Numbers must be positive"
    else 
      print "Validated ";
      printint x;
      print "\n";
      Right x 

fun try_add x y =
  bind (validate_int x) $ fn a => 
  bind (validate_int y) $ fn b => 
  return $ string_fromint $ a + b 

val main = 
  case try_add (4) (-3) of 
    Left e => println e ; 1
  | Right a => print a ; println "" ; 0
