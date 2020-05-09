fun find p n = 
  if n == 0 then 0
  else if p (4 * n) then n 
  else find p (n - 1)

fun eq a b = a == b

val main = find (eq 16) 10 
