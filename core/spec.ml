
class virtual ['transition] view =
  object
    method virtual refresh : unit
    method virtual trigger_transition : 'transition -> unit (* triggered by view *)
  end

module type T =
  sig
    type state
    type transition
    type semantics
    type suggestions

    (* navigation *)
    val init : unit -> state
    val apply : transition -> state -> state option

    (* asynchronous computations *)
    val eval : state -> (semantics -> unit) -> unit
    val suggestions : state -> semantics -> (suggestions -> unit) -> unit
									  
    (* view contents *)
    val view : state -> semantics -> suggestions -> transition view
									  
  end
