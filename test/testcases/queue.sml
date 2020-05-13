(* List functions *)
datatype 'a list = Empty of unit | Cons of 'a * 'a list 

fun tabulate f n = 
  if n == 0 
    then Empty ()
    else Cons (f n, tabulate f (n - 1))

fun list_toString elemToString L = 
  let 
    fun go L2 = 
      case L2 of 
        Empty () => ""
      | Cons (x, xs) => 
          case xs of 
            Empty () => elemToString x 
          | Cons _ => string_join (elemToString x, string_join (", ", go xs)) 
  in  
    string_join ("[", string_join (go L, "]"))
  end 

fun append (L1, L2) = 
  case L1 of 
    Empty () => L2 
  | Cons (x, xs) => Cons (x, append (xs, L2)) 

fun reverse L = 
  let
    fun reverse' L L2 = 
      case L of 
        Empty () => L2 
      | Cons (x, xs) => reverse' xs (Cons (x, L2))
  in
    reverse' L (Empty ())
  end 

fun null L = 
  case L of 
    Empty () => true 
  | Cons _ => false 

(* Queues *)
datatype 'a queue = Queue of 'a list * 'a list 

val emptyQueue = Queue (Empty (), Empty ())
fun fromList L = Queue (Empty (), L)

fun queue_toString elem_toString Q = case Q of 
  Queue (inStack, outStack) => list_toString elem_toString $ append (inStack, reverse outStack)

fun empty Q = case Q of 
  Queue (inStack, outStack) => null inStack && null outStack 

(*@ensures !(empty result) @*)
fun enqueue Q x = case Q of 
  Queue (inStack, outStack) => Queue (Cons (x, inStack), outStack)

(*@requires !(empty Q) @*)  
fun dequeue Q = case Q of 
  Queue (inStack, outStack) => 
    case outStack of 
      Cons (x, xs) => (x, Queue (inStack, outStack))
      (* will loop if the precondition is not met *)
    | Empty () => dequeue $ Queue (Empty (), reverse inStack) 

fun queue_all Q f = case Q of 
  Queue (inStack, outStack) => 
    let
      fun list_all L f = 
        case L of 
          Empty () => true 
        | Cons (x, xs) => f x && list_all xs f  
    in
      list_all inStack f && list_all outStack f  
    end 

val main = 
  let
    val evens = tabulate (fn x => x * 2) 10  
    val evenQ = fromList evens 
  in
    println $ queue_toString string_fromint evenQ ;
    if queue_all (fromList evens) (fn x => x % 2 == 0)
      then println "Test passed"
      else println "Test failed"
  end ;
  0