
open Js
open Jsutils

class ['place] history (p0 : 'place) =
object (self)
  constraint 'place = ('lis,'focus,'extent,'suggestion) Lis.place
							
  val mutable past : 'place list = []
  val mutable present : 'place = p0
  val mutable future : 'place list = []

  method present : 'place = present

  method push (p : 'place) : unit =
    present#abort;
    past <- present::past;
    present <- p;
    future <- []

  method replace (p : 'place) : unit =
    present <- p
		
  method home : unit =
    present#abort;
    past <- [];
    present <- p0;
    future <- []

  method back : bool =
    match past with
      | [] -> false
      | p::lp ->
	 present#abort;
	 future <- present::future;
	 present <- p;
	 past <- lp;
	 true
	   
  method forward : bool =
    match future with
      | [] -> false
      | p::lp ->
	 present#abort;
	 past <- present::past;
	 present <- p;
	 future <- lp;
	 true
end
    
open Js
open Jsutils
       
let start
      ~(make_lis : (string * string) list -> 'place #Lis.lis)
      ~(render_place : 'place -> (push_in_history:bool -> 'place -> unit) -> unit)
    : unit =
  firebug "Starting!";
  Dom_html.window##onload <-
    Dom.handler
      (fun ev ->
       let args = Url.Current.arguments in
       let lis = make_lis args in
       let p0 = lis#initial_place in
       let hist = new history p0 in
       (* rendering-navigation loop *)
       let rec render_hist p =
	 render_place p callback_hist
       and callback_hist ~push_in_history p =
	 if push_in_history then hist#push p else hist#replace p;
	 render_hist p
       in
       (* navigation controls *)
       let refresh () = render_hist hist#present in
       jquery "#button-home"
	      (onclick (fun elt ev -> hist#home; refresh ()));
       jquery "#button-back"
	      (onclick (fun elt ev -> if hist#back then refresh ()));
       jquery "#button-forward"
	      (onclick (fun elt ev -> if hist#forward then refresh ()));
       jquery "#button-refresh"
	      (onclick (fun elt ev -> hist#present#abort; refresh ()));
       (* initial rendering *)
       refresh ();
       bool true)
