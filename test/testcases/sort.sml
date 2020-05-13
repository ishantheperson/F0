datatype 'a list = Empty of unit | Cons of 'a * 'a list 

fun filter p L = 
  case L of 
    Empty () => Empty () 
  | Cons (x, xs) => 
      if p x 
        then Cons (x, filter p xs)
        else filter p xs

fun partition p L = 
  case L of 
    Empty () => (Empty (), Empty ())
  | Cons (x, xs) =>
      let 
        val (l, r) = partition p xs 
      in 
        if p x 
          then (Cons (x, l), r)
          else (l, Cons (x, r))
      end 

fun null L = 
  case L of 
    Empty () => true 
  | Cons _ => false 

fun append (L1, L2) = 
  case L1 of 
    Empty () => L2 
  | Cons (x, xs) => Cons (x, append (xs, L2)) 

fun toString elem_toString L =
  let
    fun go L2 =
      case L2 of
        Empty () => ""
      | Cons (x, xs) =>
          case xs of
            Empty () => elem_toString x
          | Cons _ => string_join (elem_toString x, string_join (", ", go xs))
  in
    string_join ("[", string_join (go L, "]"))
  end

fun less a b = a < b 
fun greater a b = !(a < b)

fun sort L = 
  case L of 
    Empty () => Empty () 
  | Cons (x, xs) => 
      if null xs 
        then L 
        else 
          let 
            val (small, big) = partition (greater x) xs 
          in 
            append (sort small, append (Cons (x, Empty ()), sort big))
          end 

val main = 
  let
    val L = Cons (24, Cons (10, Cons (0, Cons (4, Cons (99, Cons (44, Empty ())))))) 
  in 
    println (toString string_fromint (sort L));
    0
  end 
