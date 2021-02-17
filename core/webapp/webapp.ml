
open Js_of_ocaml
       
open Js
open Jsutils

class ['lis,'place] history (lis : 'lis) (p0 : 'place) =
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

  method save_place : unit =
    let json = present#json in
    let contents = Yojson.Safe.to_string json in
    let _ = Jsutils.trigger_download ~mime:"application/x-json-stream" contents in
    ()
      
  method open_place (json : Yojson.Safe.t) : unit =
    let p = lis#place_of_json json in
    self#push p

  method save_results : unit =
    let mime, contents = present#results in
    let _ = Jsutils.trigger_download ~mime contents in
    ()

end
   
open Js
open Jsutils

type 'place handler = push_in_history:bool -> 'place -> unit
   
let start
      ~(make_lis : (string * string) list -> 'place #Lis.lis)
      ~(render_place : 'place -> 'place handler -> unit)
      ~(handle_document_keydown : Dom_html.keyboardEvent Js.t -> 'place -> 'place handler -> bool) (* returns true if event was handled *)
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
