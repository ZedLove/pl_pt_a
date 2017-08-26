(* Zak Nitsch, Homework 2 *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* Problem #1 *)

fun all_except_option (str, slist) =
  let fun filter_match (str, slist) =
	case slist of
	    [] => []
	  | s::slist' =>
	    if same_string(str, s)
	    then filter_match(str, slist')
	    else s::filter_match(str, slist')
      val filtered = filter_match(str, slist)
  in
      if filtered = slist then NONE else SOME filtered
  end
		   
fun get_substitutions1 (subs, str) =
  case subs of
      [] => []
    | s::subs' => let val ans = all_except_option(str, s)
		  in
		      case ans of
			  NONE => get_substitutions1(subs', str)
			| SOME slist => slist @ get_substitutions1(subs', str) 
		  end

fun get_substitutions2 (subs, str) =
  let fun get_subs (subs, matches) =
	case subs of
	    [] => matches
	  | s::subs' => let val ans = all_except_option(str, s)
			in
			    case ans of
				NONE => get_subs(subs', matches)
			      | SOME slist => get_subs(subs', matches@slist)
			end
  in
      get_subs(subs, [])
  end

fun similar_names (subs, {first=first, middle=middle, last=last}) =
  let val variants = first::get_substitutions2(subs, first)
      fun create_names (variants) =
	case variants of
	    [] => []
	  | v::variants' => {first=v, middle=middle, last=last}::create_names(variants') 
  in
      create_names(variants)
  end

      
      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* Problem #2 *)

fun card_color (s, _) =
  case s of
      Clubs => Black
    | Spades => Black
    | _ => Red
		
fun card_value (_, r) =
  case r of
      Num v => v
   |  Ace => 11
   |  _ => 10

fun remove_card (cs, c_to_remove, e) =
  let fun filter_card (cs) =
	case cs of
	    [] => raise e
	  | c::cs' => if c = c_to_remove
		      then cs'
		      else c::filter_card(cs')
      val filtered = filter_card(cs)
  in
      if filtered = cs then raise e else filtered
  end

fun all_same_color (cs) =
  case cs of
      [] => false
    | c::[] => true 
    | c1::c2::cs' => card_color c1 = card_color c2 andalso all_same_color(c2::cs')

fun sum_cards (cs) =
  let fun result (cs, total) =
	case cs of
	    [] => total
	  | c::cs' => result(cs', card_value c + total)				      
  in
      result(cs, 0)
  end
					    
fun score (cs, g) =
  case cs of
      [] => 0
    | c::cs' => let fun calc_prelim_score (cs, g) =
		      let val sum = sum_cards cs
		      in
			  if sum > g
			  then 3 * (sum - g)
			  else g - sum
		      end
		    val prelim_score = calc_prelim_score(cs, g)
		in
		    if all_same_color cs
		    then prelim_score div 2
		    else prelim_score
		end

fun officiate (cs, ms, g) =
  let fun turn (cards, moves, held_cards) =
	case (moves, cards) of
	    ([],_) => score(held_cards, g)
	  | (Discard c::ms, _) => turn(cards, ms, remove_card(held_cards, c, IllegalMove))
	  | (Draw::ms, []) => score(held_cards, g)
	  | (Draw::ms, c::cs) => let val new_hand = c::held_cards
				 in
				     if sum_cards(new_hand) > g
				     then score(new_hand, g)
				     else turn(cs, ms, new_hand)
				 end
  in
      turn(cs, ms, [])
  end
