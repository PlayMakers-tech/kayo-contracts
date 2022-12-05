let rec _sqrt (n:int) (s:int) : int = if s*s > n then s-1 else _sqrt n (s+1)
let sqrt (n: int) : int = _sqrt n 1

let random (s0: nat) (s1: nat) = 
  let s1 =  s1 lxor (s1 lsl 23n) in 
  let s1 =  s1 lxor (s1 lsr 17n) in 
  let s1 =  s1 lxor s0 in 
  let s1 =  s1 lxor (s0 lsr 26n) in 
  s0 + s1