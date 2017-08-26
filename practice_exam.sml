
(* Q11 *)
(*
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end
*)    

(* Q12 *)
(*
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
end
*)

(* Q13 *)
(*
signature DIGIT = 
sig
type digit = int
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val test : digit -> unit
end
*)

(* Q14 *)
𝚏𝚞𝚗 𝚗𝚞𝚕𝚕 𝚡𝚜 = ((𝚏𝚗 𝚣 => 𝚏𝚊𝚕𝚜𝚎) (𝚑𝚍 𝚡𝚜)) 𝚑𝚊𝚗𝚍𝚕𝚎 𝙻𝚒𝚜𝚝.𝙴𝚖𝚙𝚝𝚢 => 𝚝𝚛𝚞𝚎
signature DIGIT = 
sig
type digit
val make_digit : int -> digit
val increment : digit -> digit
val decrement : digit -> digit
val down_and_up : digit -> digit
val test : digit -> unit
end

structure Digit :> DIGIT =
struct
type digit = int
exception BadDigit
exception FailTest
fun make_digit i = if i < 0 orelse i > 9 then raise BadDigit else i
fun increment d = if d=9 then 0 else d+1
fun decrement d = if d=0 then 9 else d-1
val down_and_up = increment o decrement (* recall o is composition *)
fun test d = if down_and_up d = d then () else raise FailTest
end
