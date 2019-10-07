
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
								  
  method set_suggestions (lsugg : 'suggestion list) : unit =
    sugg_dico#clear;
    input_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
       let html =
	 Html.div
	   ~classe:"list-suggestions css-treeview"
	   (Html.ul
	      (List.map
		 (fun sugg ->
		  let key = sugg_dico#add sugg in
		  (Some key, Some "suggestion", None, html_of_suggestion ~input_dico sugg))
		 lsugg)) in
       elt##innerHTML <- string html;
       stop_propagation_from elt ".suggestion-input";
       jquery_all_input_from elt ".suggestion-input"
	  (oninput (fun elt_input ev ->
	     let key = to_string elt_input##id in
	     let input_update = input_dico#get key in
	     try
	       elt_input##style##color <- string "black";
	       input_update (to_string (elt_input##value))
	     with _ ->
	       elt_input##style##color <- string "red";
	       ()));
       jquery_all_from elt ".suggestion"
	  (onclick (fun elt_sugg ev ->
	     let key = to_string elt_sugg##id in
	     let sugg = sugg_dico#get key in
	     on_suggestion_selection sugg));
       jquery_all_from elt ".suggestion-input"
	   (onenter (fun input_elt ev ->
	      jquery_parent input_elt (fun parent_elt ->
		let key = to_string parent_elt##id in
		let sugg = sugg_dico#get key in
		on_suggestion_selection sugg))))

end
  
