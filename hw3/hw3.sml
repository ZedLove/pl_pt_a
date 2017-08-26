(* Zak Nitsch, Homework 3 *)

exception NoAnswer

(**** you can put all your code here ****)

fun only_capitals strs =
  let val f = List.filter(fn s => Char.isUpper(String.sub(s, 0)))
  in f strs
  end

fun longest_string1 strs =
  let
      fun compare_size (s1, s2) =
	if String.size s1 > String.size s2
	then s1
	else s2
  in
      List.foldl compare_size "" strs
  end
      
fun longest_string2 strs =
  let
      fun compare_size (s1, s2) =
	if String.size s1 >= String.size s2
	then s1
	else s2
  in
      List.foldl compare_size "" strs
  end

fun longest_string_helper f =
  let fun helper (s1, s2) =
	if f (String.size s1, String.size s2)
	then s1
	else s2
  in
      List.foldl helper ""
  end

val longest_string3 = longest_string_helper (fn (s1, s2) => s1 > s2)

val longest_string4 = longest_string_helper (fn (s1, s2) => s1 >= s2)

val longest_capitalized = longest_string3 o only_capitals

val rev_string = String.implode o List.rev o String.explode

fun first_answer f l =
  case l of
      [] => raise NoAnswer
    | x::xs' => case f x of
		    SOME v => v
		  | _ => first_answer f xs'

fun all_answers f l =
  let fun h lst acc =
	case lst of
	    [] => acc
	  | x::xs' => case f x of
			  NONE => raise NoAnswer
			| SOME y => (h xs' y@acc)
  in SOME (h l [])
     handle NoAnswer => NONE
  end

(* provided types *)
datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu
      
(* Provided g function *)
fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

val count_wildcards = g (fn x => 1) (fn _ => 0)

val count_wild_and_variable_lengths = g (fn x => 1) (fn x => String.size x)

fun count_some_var (s, p) =
  g (fn x => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
  let fun get_strings pat =
        case pat of
            Variable s => [s]
          | TupleP ps => List.foldl (fn (p,i) => (get_strings p)@i) [] ps
          | ConstructorP (s,p) => s::get_strings p
	  | _ => []
      fun is_unique slist =
        case slist of
            [] => true
          | x::xs' => if List.exists (fn y => y = x) xs' then false else is_unique xs'
  in is_unique(get_strings (p)) end

fun match (v, p) =
  case p of
      Wildcard => SOME []
    | Variable s => SOME [(s, v)]
    | UnitP => (case v of 
                    Unit => SOME [] 
                  | _ => NONE)
    | ConstP i => (case v of 
                       Const j => if i = j then SOME [] else NONE
                     | _ => NONE)
    | TupleP lst => (case v of
			 Tuple tlist => (all_answers (fn (x,y) => match(x,y)) (ListPair.zipEq(tlist, lst)) handle UnequalLengths => NONE)
                       | _ => NONE)
      | ConstructorP (s, pat) => (case v of
                                      Constructor (a, b) => if s = a then match(b, pat) else NONE
                                    | _ => NONE)

fun first_match v plist =
  SOME (first_answer (fn x => match(v, x)) plist) handle NoAnswer => NONE
									 
(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

