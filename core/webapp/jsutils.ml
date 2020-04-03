(*
  Copyright 2013-2017 Sébastien Ferré, IRISA, Université de Rennes 1

  This file is part of Sparklis.
*)

open Js_of_ocaml
open Js_of_ocaml_lwt
       
open Js
open XmlHttpRequest

let alert msg = Dom_html.window##alert (string msg)

let prompt msg text = Dom_html.window##prompt (string msg) (string text)

let firebug msg = Firebug.console##log (string msg)

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

  
(* YASGUI bindings *)

let opt_iter (opt : 'a option) (k : 'a -> unit) : unit =
  match opt with
  | Some x -> k x
  | None -> ()
    
let yasgui =
object (self)
  val mutable this_opt = None

  method init =
    try
      let constr_YASGUI = Unsafe.global##.YASGUI in
      this_opt <- Some (new%js constr_YASGUI (Dom_html.getElementById "sparklis-yasgui"))
    with exn ->
      firebug ("yasgui#init: " ^ Printexc.to_string exn);
      alert ("Warning: YASGUI could not be initialized for some reason. SPARQL queries will be displayed only as text.");
      this_opt <- None

  method set_corsProxy (url_opt : string option) : unit =
    match this_opt with
    | None -> ()
    | Some yasgui -> yasgui##.options##.api##.corsProxy := (match url_opt with None -> null | Some url -> some (string url))

  method private yasqe yasgui = (yasgui##current ())##.yasqe
  method private yasr yasgui = (yasgui##current ())##.yasr

  method set_endpoint (endpoint : string) : unit =
    match this_opt with
    | None -> ()
    | Some yasgui ->
      let yasqe = self#yasqe yasgui in
      yasqe##.options##.sparql##.endpoint := string endpoint;
      jquery_set_innerHTML ".yasgui .endpointText .item" endpoint

  method set_requestMethod (meth : [`GET | `POST]) : unit =
    match this_opt with
    | None -> ()
    | Some yasgui ->
      let yasqe = self#yasqe yasgui in
      yasqe##.options##.sparql##.requestMethod := string (match meth with `GET -> "GET" | `POST -> "POST")

  method set_query (sparql : string) : unit =
    match this_opt with
    | None ->
      let html = "<xmp>" ^ sparql ^ "</xmp>" in
      jquery_set_innerHTML "#sparql-query" html
    | Some yasgui ->
      let yasqe = self#yasqe yasgui in
      yasqe##setValue (string sparql)

  method set_response (resp : string) : unit =
    match this_opt with
    | None -> ()
    | Some yasgui ->
      let yasr = self#yasr yasgui in
      yasr##setResponse (string resp)

  method refresh : unit =
    match this_opt with
    | None -> ()
    | Some yasgui ->
      let yasqe = self#yasqe yasgui in
      let yasr = self#yasr yasgui in
      yasqe##setValue (yasqe##getValue ());
      yasr##draw ()
end

(* Google charts bindings *)
  
let google =
object
  method set_on_load_callback : 'a. (unit -> 'a) -> 'a = fun k ->
    let g = Unsafe.global##.google in
    if g = Js.undefined
    then k ()
    else (
      let k () =
	firebug "Loaded document and google charts";
	k () in
      g##.charts##setOnLoadCallback (wrap_callback k)
    )
      
  method draw_map (points : (float * float * string) list) (elt_map : Dom_html.element t) : unit =
    let g = Unsafe.global##.google in
    if g = Js.undefined
    then ()
    else (
    firebug "Drawing map";
    let constr_Map = Unsafe.global##.Map in
    let map = new%js constr_Map elt_map in
    let table =
      let data =
	let col t = Inject.(obj [| "type", string t |]) in
	let row lat long name =
	  let cell v = Inject.(obj [| "v", float v |]) in
	  Inject.(obj [| "c", array [| cell lat; cell long; cell (string name) |] |]) in
	Inject.(obj [| 
	  "cols", array [| col "number"; col "number"; col "string" |];
	  "rows", array (Array.of_list (List.map (fun (lat,long,name) -> row lat long name) points)) |]) in
      let constr_DataTable = Unsafe.global##.DataTable in
      new%js constr_DataTable data in
    let options = Inject.(obj [|
      "showTooltip", bool true;
      "showInfoWindow", bool true;
      "useMapTypeControl", bool true;
      "icons",
      obj [| "default",
	     obj [| "normal",
		    string "http://maps.google.com/mapfiles/ms/icons/red-dot.png";

		    "selected",
		    string "http://maps.google.com/mapfiles/ms/icons/blue-dot.png" |] |] |]) in
    let _ = map##draw table options in
    firebug "Drawed the map"
    )

end

(* Wikidata services *)
module Wikidata =
  struct
    let entities_of_json ojson : string list option =
      try
	let oquery = Unsafe.get ojson (string "query") in
	let osearch = Unsafe.get oquery (string "search") in
	let n = truncate (to_float (Unsafe.get osearch (string "length"))) in
	let le = ref [] in
	for i = n-1 downto 0 do
	  let oresult = Unsafe.get osearch (string (string_of_int i)) in
	  let otitle = Unsafe.get oresult (string "title") in
	  le := (Js.to_string otitle)::!le
	done;
	firebug (string_of_int n ^ " wikidata entities found");
	Some !le
      with _ ->
	None

    let ajax_entity_search (query : string) (limit : int) (k : string list option -> unit) : unit =
      if String.length query < 3
      then k None
      else
	let _ = firebug ("Wikidata search: " ^ query) in
	let query_url =
	  Printf.sprintf
	    "https://www.wikidata.org/w/api.php?action=query&list=search&format=json&srlimit=%d&srsearch=%s"
	    (*"https://www.wikidata.org/w/api.php?action=wbsearchentities&format=json&language=en&limit=%d&search=%s" (* type=item|property *) NOTE: less flexible search *)
	    limit
	    (Url.urlencode query) in
	Lwt.ignore_result
	  (Lwt.bind
	     (Jsonp.call_custom_url (*~timeout:0.5*)
		(fun name -> query_url ^ "&callback=" ^ name))
	     (fun json ->
	      k (entities_of_json json);
	      Lwt.return ()))
	
  end
