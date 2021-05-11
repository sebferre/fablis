(**
   Utilities related to JavaScript for accessing the DOM and browser.
   @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

open Js_of_ocaml
open Js_of_ocaml_lwt

open Js
open XmlHttpRequest

(** Displays an alert message in the browser. *)
let alert msg = Dom_html.window##alert (string msg)

let prompt msg text = Dom_html.window##prompt (string msg) (string text)

let firebug msg = Firebug.console##log (string msg)

let make_data_url mime contents =
  "data:" ^ mime ^ "," ^ Url.urlencode contents				       
let trigger_download ~mime contents : unit =
  let data_url = make_data_url mime contents in
  let _w_opt = Dom_html.window##open_ (string data_url) (string "_blank") null in
  ()

let jquery_document k =
  k Dom_html.document
				     
let jquery_from (root : #Dom_html.nodeSelector Js.t) s k =
  Opt.iter (root##querySelector (string s)) (fun elt ->
    k elt)
let jquery s k = jquery_from Dom_html.document s k

let jquery_input_from (root : #Dom_html.nodeSelector Js.t) s k =
  Opt.iter (root##querySelector (string s)) (fun elt ->
    Opt.iter (Dom_html.CoerceTo.input elt) (fun input ->
      k input))
let jquery_input s k = jquery_input_from Dom_html.document s k

let jquery_select_from (root : #Dom_html.nodeSelector Js.t) s k =
  Opt.iter (root##querySelector (string s)) (fun elt ->
    Opt.iter (Dom_html.CoerceTo.select elt) (fun select ->
      k select))
let jquery_select s k = jquery_select_from Dom_html.document s k

let jquery_all_from (root : #Dom_html.nodeSelector Js.t) s k =
  let nodelist = root##querySelectorAll (string s) in
  let n = nodelist##.length in
  for i=0 to n-1 do
    Opt.iter (nodelist##item i) k
  done
let jquery_all s k = jquery_all_from Dom_html.document s k

let jquery_all_input_from (root : #Dom_html.nodeSelector Js.t) s k =
  let nodelist = root##querySelectorAll (string s) in
  let n = nodelist##.length in
  for i=0 to n-1 do
    Opt.iter (nodelist##item i) (fun elt ->
      Opt.iter (Dom_html.CoerceTo.input elt) (fun input ->
	k input))
  done

let jquery_all_select_from (root : #Dom_html.nodeSelector Js.t) s k =
  let nodelist = root##querySelectorAll (string s) in
  let n = nodelist##.length in
  for i=0 to n-1 do
    Opt.iter (nodelist##item i) (fun elt ->
      Opt.iter (Dom_html.CoerceTo.select elt) (fun input ->
	k input))
  done

let jquery_parent elt k =
  Opt.iter elt##.parentNode (fun node ->
    Opt.iter (Dom.CoerceTo.element node) (fun parent ->
      let elt = Dom_html.element parent in
      k elt))

let jquery_ancestor ~classe elt k = (* finding first ancestor with some class *)
  let rec find_ancestor elt =
    if to_bool (elt##.classList##contains (string classe))
    then k elt
    else jquery_parent elt (fun parent -> find_ancestor parent)
  in
  find_ancestor elt

	   
let jquery_get_innerHTML sel =
  let res = ref "" in
  jquery sel (fun elt -> res := to_string elt##.innerHTML);
  !res
let jquery_set_innerHTML sel html =
  jquery sel (fun elt -> elt##.innerHTML := string html)
let jquery_toggle_innerHTML sel (s1 : string) (s2 : string) : string =
  let new_s = ref "" in
  jquery sel (fun elt ->
    new_s := (if to_string elt##.innerHTML = s1 then s2 else s1);
    elt##.innerHTML := string !new_s);
  !new_s

let jquery_show sel = jquery sel (fun elt -> elt##.style##.display := string "block")
let jquery_hide sel = jquery sel (fun elt -> elt##.style##.display := string "none")
let jquery_toggle sel = jquery sel (fun elt ->
  if to_string elt##.style##.display = "none"
  then elt##.style##.display := string "block"
  else elt##.style##.display := string "none")    

let jquery_disable_all sel =
  jquery_all
    sel
    (fun elt ->
     let classes = elt##.classList in
     classes##add (string "disabled");
     classes##add (string "disabledClick"))
let jquery_enable_all sel =
  jquery_all
    sel
    (fun elt ->
     let classes = elt##.classList in
     classes##remove (string "disabled");
     classes##remove (string "disabledClick"))

let jquery_click_from elt sel = jquery_from elt sel (fun elt -> Unsafe.(meth_call elt "click" [||]))
let jquery_click sel = jquery_click_from Dom_html.document sel

let onclick k elt =
  elt##.onclick := Dom.handler (fun ev -> k elt ev; bool true)

let ondblclick k elt =
  elt##.ondblclick := Dom.handler (fun ev -> k elt ev; bool true)

let onhover k elt =
  elt##.onmouseover := Dom.handler (fun ev -> k elt ev; bool true)

let onhover_out k elt =
  elt##.onmouseout := Dom.handler (fun ev -> k elt ev; bool true)

let oninput k elt =
  elt##.oninput := Dom.handler (fun ev -> k elt ev; bool true)

let onchange k elt =
  elt##.onchange := Dom.handler (fun ev -> k elt ev; bool true)

let onkeypress k elt =
  elt##.onkeypress := Dom.handler (fun ev -> k elt ev; bool true)
let onkeydown k elt =
  elt##.onkeydown := Dom.handler (fun ev -> k elt ev; bool true)

let onenter k elt =
  onkeypress
    (fun elt ev -> if ev##.keyCode = 13 then begin k elt ev; bool true end else bool false)
    elt

let stop_propagation_from elt sel =
  jquery_all_from elt sel
    (onclick (fun elt ev -> Dom_html.stopPropagation ev))
let stop_links_propagation_from elt = stop_propagation_from elt "a"

let toggle_class (elt : Dom_html.element t) (cl : string) : bool =
  let clList = elt##.classList in
  to_bool (clList##toggle (string cl))
							    
(* prepare a string for safe insertion in HTML code *)
let escapeHTML (str : string) : string =
  let div = Dom_html.createDiv Dom_html.document in
  ignore (div##appendChild ((Dom_html.document##createTextNode(string str) :> Dom.node t)));
  to_string (div##.innerHTML)

let integer_of_input ?(min = min_int) ?(max = max_int) input : int option =
  try
    let n = int_of_string (to_string input##.value) in
    if n < min then None
    else if n > max then None
    else Some n
  with _ -> None

let float_of_input input : float option =
  try Some (float_of_string (to_string input##.value))
  with _ -> None

let string_of_input input : string =
  to_string input##.value

let file_string_of_input (input : #Dom_html.inputElement t) (k : (string * string) -> unit) : unit =
  let open File in
  Optdef.iter
    input##.files
    (fun files ->
     Opt.iter
       (files##item 0)
       (fun file ->
	let reader = new%js fileReader in
	reader##.onloadend :=
	  Dom.handler
	    (fun ev ->
	     ( match reader##.readyState with
	       | EMPTY -> alert "The file is empty"
	       | LOADING -> assert false
	       | DONE ->
		  Opt.case
		    (CoerceTo.string reader##.result)
		    (fun () -> alert "The file is not a valid JSON file")
		    (fun s -> k (to_string (filename file), to_string s)) (* Json.unsafe_input s)) *)
	     );
	     bool true);
	reader##.onerror :=
	  Dom.handler
	    (fun ev ->
	     alert ("Error while reading file: error code " ^ string_of_int reader##.error##.code);
	    bool true);
	(* TODO: add progression bar *)
	reader##readAsText file))

let string_of_select (select : Dom_html.selectElement Js.t) : string =
  to_string select##.value

    
(* DOM utilities *)

let getElementsByTagNameNS (elt : Dom.element t) (ns : js_string t) (name : js_string t) : Dom.element Dom.nodeList t =
(*elt##getElementsByTagName (name)*)
  Unsafe.coerce (Unsafe.meth_call elt "getElementsByTagNameNS" [|Unsafe.inject ns; Unsafe.inject name|])

let lookupPrefix (elt : Dom.element t) (ns : js_string t) : js_string t opt =
  (* not working in Internet Explorer *)
  some (Unsafe.coerce (Unsafe.meth_call elt "lookupPrefix" [|Unsafe.inject ns|]))


(* helping injection of OCaml values to JSON values *)

module Inject =
struct
  let bool b = Unsafe.inject (bool b)
  let int i = Unsafe.inject i
  let float f = Unsafe.inject f
  let string s = Unsafe.inject (string s)
  let array ar = Unsafe.inject (array ar)
  let obj ar = Unsafe.obj ar
end
