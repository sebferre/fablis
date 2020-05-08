
open Js_of_ocaml
       
open Js
open Jsutils

class ['suggestion] widget
  ~(id : Html.id)
  ~(html_of_suggestion : input_dico:Html.input_dico -> 'suggestion -> Html.t)
  =
object
  val sugg_dico : 'suggestion Html.dico = new Html.dico (id ^ "-suggestion")
  val input_dico : Html.input_dico = new Html.dico (id ^ "-suggestion-input")

  val mutable on_suggestion_selection : 'suggestion -> unit = fun sugg -> ()
  method on_suggestion_selection f = on_suggestion_selection <- f
								  
  method set_suggestions (lcol : string list) (lfsugg : 'suggestion Lis.forest list) : unit =
    let cpt = ref 0 in (* counter for collapsable dirs *)
    let select_suggestion elt =
      let key = to_string elt##.id in
      let sugg = sugg_dico#get key in
      on_suggestion_selection sugg in
    let rec html_forest fsugg =
      Html.ul
	(List.map
	   (fun tree ->
	    match tree with
	    | `Sugg sugg ->
	       let key : string = sugg_dico#add sugg in
	       let html_sugg = html_of_suggestion ~input_dico sugg in
	       let html =
		 "<label style=\"visibility:hidden;\">►&nbsp;</label>"
		 ^ html_sugg in
	       (Some key, Some "suggestion", None, html)
	    | `Dir (dir,children) ->
	       let check_id =
		 incr cpt; "collapse-suggdir-" ^ string_of_int !cpt in
	       let html =
		 "<input class=\"input-treeview\" type=\"checkbox\" id=\"" ^ check_id ^ "\">"
		 ^ "<label for=\"" ^ check_id ^ "\" class=\"label-checked\">▼&nbsp;</label>"
		 ^ "<label for=\"" ^ check_id ^ "\" class=\"label-unchecked\">►&nbsp;</label>"
		 ^ Html.span ~classe:"suggestion-dir" dir
		 ^ html_forest children in
	       (None, None, None, html))
	   fsugg)
    in
    assert (List.length lcol = List.length lfsugg);
    sugg_dico#clear;
    input_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
       let lhtml =
	 List.map2
	   (fun col fsugg ->
	    Html.div
	      ~classe:(col ^ " column-suggestions")
	      (Html.div
		 ~classe:"panel panel-default panel-suggestions"
		 (Html.div
		    ~classe:"panel-body list-suggestions css-treeview"
		    (html_forest fsugg))))
	   lcol lfsugg in
       let html =
	 Html.div ~classe:"row"
		  (String.concat "\n" lhtml) in
       elt##.innerHTML := string html;
       stop_propagation_from elt ".suggestion-input, .suggestion-file-label, .suggestion-select";
       (* input validation *)
       jquery_all_input_from elt ".suggestion-input"
	  (oninput (fun elt_input ev ->
	     let key = to_string elt_input##.id in
	     let input_info = input_dico#get key in
	     match input_info with
	     | InputElt { input_update } ->
		(try
		    elt_input##.style##.color := string "black";
		    input_update#call elt_input (fun () -> ())
		  (* TODO: block suggestion selection until updated has terminated *)
		  with _ ->
		    elt_input##.style##.color := string "red";
		    ())
	     | _ -> assert false));
       (* suggestion selection by clicking *)
       jquery_all_from elt ".suggestion"
	  (onclick (fun elt_sugg ev ->
	     select_suggestion elt_sugg));
       (* suggestion selection by enter on input *)
       jquery_all_from elt ".suggestion-input"
	  (onenter (fun input_elt ev ->
		    jquery_ancestor ~classe:"suggestion" input_elt
				    (fun ancestor_elt -> select_suggestion ancestor_elt)));
       (* suggestion selection by selecting *)
       jquery_all_select_from elt ".suggestion-select"
	  (oninput (fun (select_elt : Dom_html.selectElement Js.t) ev ->
		     let key = to_string select_elt##.id in
		     match input_dico#get key with
		     | SelectElt { input_update } ->
			input_update#call select_elt (fun () -> ())
		     | _ -> assert false));
       (* suggestion selection by file choosing *)
       jquery_all_input_from elt ".suggestion-file-input"
	  (onchange (fun elt_input ev ->
	      let key = to_string elt_input##.id in
	      let input_info = input_dico#get key in
	      match input_info with
	      | FileElt { input_update } ->
		 input_update#call elt_input
		   (fun () ->
		    jquery_ancestor ~classe:"suggestion" (elt_input :> Dom_html.element Js.t)
                       (fun ancestor_elt ->
			select_suggestion ancestor_elt))
	      | _ -> assert false )))

end
  
