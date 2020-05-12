datatype 'a list = Empty of unit | Cons of 'a * 'a list 

fun filter p L = 
  case L of 
    Empty () => Empty () 
  | Cons c => 
      let 
        val (hd, tl) = c 
      in 
        if p hd 
          then Cons (hd, filter p tl)
          else filter p tl 
      end 

fun null L = 
  case L of 
    Empty () => true 
  | Cons _ => false 

fun append (L1, L2) = 
  case L1 of 
    Empty () => L2 
  | Cons c => 
      let 
        val (hd, tl) = c 
      in
        Cons (hd, append (tl, L2)) 
      end 

fun less a b = a < b 
fun greater a b = !(a < b)
fun sort L = 
  case L of 
    Empty () => Empty () 
  | Cons c => 
      let 
        val (hd, tl) = c 
      in
        if null tl 
          then L 
          else 
            let 
              val small = filter (greater hd) tl 
              val big = filter (less hd) tl 
            in 
              append (sort small, append (Cons (hd, Empty ()), sort big))
            end 
      end 

fun toString elemToString L =
  let
    fun go L2 =
      case L2 of
        Empty () => ""
      | Cons items =>
          let
            val (hd, tl) = items
          in
            case tl of
              Empty () => elemToString hd
            | Cons _ => string_join (elemToString hd, string_join (", ", go tl))
          end
  in
    string_join ("[", string_join (go L, "]"))
  end

val main = 
  let
    val L = Cons (24, Cons (10, Cons (0, Cons (4, Cons (99, Cons (44, Empty ())))))) 
  in 
    println (toString string_fromint (sort L));
    0
  end 
