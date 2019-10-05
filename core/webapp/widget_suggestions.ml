
open Js
open Jsutils

class ['suggestion] widget
  ~(id : Html.id)
  ~(html_of_suggestion : 'suggestion -> Html.t)
  =
object
  val sugg_dico : 'suggestion Html.dico = new Html.dico (id ^ "-suggestion")

  val mutable on_suggestion_selection : 'suggestion -> unit = fun sugg -> ()
  method on_suggestion_selection f = on_suggestion_selection <- f
								  
  method set_suggestions (lsugg : 'suggestion list) : unit =
    sugg_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
       let html =
	 Html.div
	   ~classe:"list-suggestions css-treeview"
	   (Html.ul
	      (List.map
		 (fun sugg ->
		  let key = sugg_dico#add sugg in
		  (Some key, Some "suggestion", None, html_of_suggestion sugg))
		 lsugg)) in
       elt##innerHTML <- string html;
       jquery_all_from elt ".suggestion"
	  (onclick (fun elt_sugg ev ->
	     let key = to_string elt_sugg##id in
	     let sugg = sugg_dico#get key in
	     on_suggestion_selection sugg)))

end
  
