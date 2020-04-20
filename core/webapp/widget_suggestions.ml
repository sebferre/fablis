
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
								  
  method set_suggestions (lcol : string list) (llsugg : 'suggestion list list) : unit =
    let select_suggestion elt =
      let key = to_string elt##.id in
      let sugg = sugg_dico#get key in
      on_suggestion_selection sugg
    in
    assert (List.length lcol = List.length llsugg);
    sugg_dico#clear;
    input_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
       let lhtml =
	 List.map2
	   (fun col lsugg ->
	    Html.div
	      ~classe:(col ^ " column-suggestions")
	      (Html.div
		 ~classe:"panel panel-default panel-suggestions"
		 (Html.div
		    ~classe:"panel-body list-suggestions css-treeview"
		    (Html.ul
		       (List.map
			  (fun sugg ->

			   let key : string = sugg_dico#add sugg in
			   let html_sugg = html_of_suggestion ~input_dico sugg in
			   (Some key, Some "suggestion", None, html_sugg))
			  lsugg)))))
	   lcol llsugg in
       let html =
	 Html.div ~classe:"row"
		  (String.concat "\n" lhtml) in
       elt##.innerHTML := string html;
       stop_propagation_from elt ".suggestion-input, .suggestion-file-input";
       (* input validation *)
       jquery_all_input_from elt ".suggestion-input"
	  (oninput (fun elt_input ev ->
	     let key = to_string elt_input##.id in
	     let input_update = input_dico#get key in
	     try
	       elt_input##.style##.color := string "black";
	       input_update#call elt_input (fun () -> ())
		  (* TODO: block suggestion selection until updated has terminated *)
	     with _ ->
	       elt_input##.style##.color := string "red";
	       ()));
       (* suggestion selection by clicking *)
       jquery_all_from elt ".suggestion"
	  (onclick (fun elt_sugg ev ->
	     select_suggestion elt_sugg));
       (* suggestion selection by enter on input *)
       jquery_all_from elt ".suggestion-input"
	  (onenter (fun input_elt ev ->
	      jquery_ancestor ~classe:"suggestion" input_elt (fun ancestor_elt ->
		select_suggestion ancestor_elt)));
       (* suggestion selection by file choosing *)
       jquery_all_input_from elt ".suggestion-file-input"
	  (onchange (fun elt_input ev ->
	      let key = to_string elt_input##.id in
	      let input_update = input_dico#get key in
	      input_update#call elt_input
		 (fun () ->
		  jquery_ancestor ~classe:"suggestion" (elt_input :> Dom_html.element Js.t) (fun ancestor_elt ->
		    select_suggestion ancestor_elt)))))

end
  
