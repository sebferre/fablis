(** Template for defining and starting a web application, running in the browser.

    Compiles to JavaScript file that goes with [index.html] and [index.css] files.
    @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

open Js_of_ocaml
       
open Js
open Jsutils

(** {1 Navigation History} *)
   
(** The class of navigation histories, made of LIS places. An history is created from a LIS instance, and an initial place. *)
class ['lis,'place] history (lis : 'place Lis.lis) (p0 : 'place) =
object (self)
  constraint 'place = ('lis,'focus,'extent,'suggestion) Lis.place

  (** backward places *)
  val mutable past : 'place list = []
  (** current place *)
  val mutable present : 'place = p0
  (** forward places *)
  val mutable future : 'place list = []

  (** Returns the current place. *)
  method present : 'place = present

  (** Pushes a new place [p] on top of visited places. The current place is pushed in backward place, while [p] becomes the current place. The set of forward places becomes empty. *)
  method push (p : 'place) : unit =
    present#abort;
    past <- present::past;
    present <- p;
    future <- []

  (** Sets a new place [p] as the new current place. The previous current place is forgotten. *)
  method replace (p : 'place) : unit =
    present <- p

  (** Resets history. The current place is the initial place. The sets of backward and forward places become empty. *)
  method home : unit =
    present#abort;
    past <- [];
    present <- p0;
    future <- []

  (** Moves backward in history. The current place is pushed in the forward places, and the top backward place becomes the current place. Does nothing if there is no backward place. Returns true if the move backward was effective. *)
  method back : bool =
    match past with
      | [] -> false
      | p::lp ->
	 present#abort;
	 future <- present::future;
	 present <- p;
	 past <- lp;
	 true

  (** Moves forward in history. The current place is pushed in the backward places, and the top forward place becomes the current place. Does nothing if there is no forward place. Returns true if the move forward was effective. *)
  method forward : bool =
    match future with
      | [] -> false
      | p::lp ->
	 present#abort;
	 past <- present::past;
	 present <- p;
	 future <- lp;
	 true

  (** Serialize the current place to JSON, and triggers its download. *)
  method save_place : unit =
    let json = present#json in
    let contents = Yojson.Safe.to_string json in
    let _ = Jsutils.trigger_download ~mime:"application/x-json-stream" contents in
    ()

  (** Pushes the deserialization of the given JSON as the new current place. *)
  method open_place (json : Yojson.Safe.t) : unit =
    let p = lis#place_of_json json in
    self#push p

  (** Triggers the download of the current place results. *)
  method save_results : unit =
    let mime, contents = present#results in
    let _ = Jsutils.trigger_download ~mime contents in
    ()

end

  
(** {1 Defining and Starting a Web Application} *)

(** Starts a LIS instance as a web application, defining it with 4 components:
- [make_lis args] must return the LIS instance, given a list of arguments (possibly empty) passed from the web application URL
- [render_place p k] must render place [p] using various widgets, and may use the continuation [k] to navigate to another place, specifying whether the current place should be pushed into the history. [k] is typically called in event handlers, defined while rendering the place.
- [handle_document_keydown event p k] may use the continuation [k] on a new place, depending on the document-level keydown event and the current place [p].
- [error_message exn] must return a string representation of an exception. This is used to display message errors in the web application.

*)
let start
      ~(make_lis : (string * string) list -> (('lis,'focus,'extent,'suggestion) #Lis.place as 'place) #Lis.lis)
      ~(render_place : 'place -> (push_in_history:bool -> 'place -> unit) -> unit)
      ~(handle_document_keydown : Dom_html.keyboardEvent Js.t -> 'place -> (push_in_history:bool -> 'place -> unit) -> bool) (* returns true if event was handled *)
      ~(error_message : exn -> string)
    : unit =
  firebug "Starting!";
  Dom_html.window##.onload :=
    Dom.handler
      (fun ev ->
       firebug "Loaded";
       let args = Url.Current.arguments in
       let lis = make_lis args in
       let p0 = lis#initial_place in
       let hist = new history lis p0 in
       (* rendering-navigation loop *)
       let rec render_hist p =
	 render_place p callback_hist
       and callback_hist ~push_in_history p =
	 try
	   render_hist p;
	   if push_in_history then hist#push p else hist#replace p
	 with
	 | Error js_error -> Firebug.console##error js_error
	 | exn ->
	    alert (error_message exn);
	    render_hist hist#present (* recovering from error *)
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
       (* query save-load controls *)
       jquery "#button-save"
	      (onclick (fun elt ev -> hist#save_place; true));
       jquery "#button-open"
	      (onclick (fun elt ev -> jquery_click "#input-open"; true));
       jquery_input
	 "#input-open"
	 (onchange (fun input ev ->
		    Jsutils.file_string_of_input
		      input
		      (fun (filename,contents) ->
		       let json = Yojson.Safe.from_string contents in
		       hist#open_place json;
		       refresh ())));
       (* results save control *)
       jquery "#button-save-results"
	      (onclick (fun elt ev -> hist#save_results; true));
       (* shortcut keys for navigation *)
       jquery_document
         (onkeydown (fun elt ev ->
              if handle_document_keydown ev hist#present callback_hist then ()
              else if to_bool ev##.ctrlKey then
                ( match ev##.keyCode with
                | 36 (* Home *) ->
                   hist#home; refresh ()
                | 37 (* ArrowLeft *) ->
                   if hist#back then refresh ()
                | 39 (* ArrowRight *) ->
                   if hist#forward then refresh ()
                | _ -> () )
              else ()
         ));
       (* initial rendering *)
       firebug "initial refresh";
       refresh ();
       bool true)
