
(* kinds of things *)
(* focus, id, suggestion, constraint, result *)

type freq = { value : int; partial : bool }
	      
class virtual ['lis,'focus,'extent,'suggestion] place (lis : 'lis) (focus : 'focus) =
object
  method lis = lis
  method focus = focus

  method virtual eval : ('extent -> unit) -> ('suggestion list -> unit) -> unit
  method virtual activate : 'suggestion -> ('lis,'focus,'extent,'suggestion) place option
  method virtual abort : unit (* abort any ongoing computation in that place *)
end

class virtual ['place] lis =
object
  method virtual initial_place : 'place
				    
end

       
