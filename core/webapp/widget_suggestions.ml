
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
       stop_propagation_from elt ".suggestion-input";
       jquery_all_input_from elt ".suggestion-input"
	  (oninput (fun elt_input ev ->
	     let key = to_string elt_input##.id in
	     let input_update = input_dico#get key in
	     try
	       elt_input##.style##.color := string "black";
	       input_update (to_string elt_input##.value)
	     with _ ->
	       elt_input##.style##.color := string "red";
	       ()));
       jquery_all_from elt ".suggestion"
	  (onclick (fun elt_sugg ev ->
	     let key = to_string elt_sugg##.id in
	     let sugg = sugg_dico#get key in
	     on_suggestion_selection sugg));
       jquery_all_from elt ".suggestion-input"
	  (onenter (fun input_elt ev ->
	      jquery_ancestor ~classe:"suggestion" input_elt (fun ancestor_elt ->
		let key = to_string ancestor_elt##.id in
		let sugg = sugg_dico#get key in
		on_suggestion_selection sugg))))

end
  
