


open Js
open Jsutils

class ['word,'input,'focus] widget
  ~(id : Html.id) (* where to insert the widget in the DOM *)
  ~(html_of_word : 'word -> Html.t) (* rendering words *)
  =
object
  val focus_dico : 'focus Html.dico = new Html.dico (id ^ "-focus")

  val mutable on_focus_change : 'focus -> unit = fun foc -> ()
  method on_focus_change f = on_focus_change <- f

  val mutable on_focus_up : unit -> unit = fun () -> ()
  method on_focus_up f = on_focus_up <- f

  val mutable on_focus_delete : unit -> unit = fun () -> ()
  method on_focus_delete f = on_focus_delete <- f
					  
  method set_syntax (xml : ('word,'input,'focus) Syntax.xml) : unit =
    focus_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
        let html = Html.syntax ~focus_dico ~html_of_word xml in
	elt##innerHTML <- string html;
	stop_links_propagation_from elt;
	jquery_all_from elt ".focus"
	   (onclick (fun elt_foc ev ->
	      Dom_html.stopPropagation ev;
	      let key = to_string (elt_foc##id) in
	      let foc = focus_dico#get key in
	      on_focus_change foc));
	jquery_from elt "#focusup-current-focus"
	   (onclick (fun elt_button ev ->
	      Dom_html.stopPropagation ev;
	      on_focus_up ()));
	jquery_from elt "#delete-current-focus"
	   (onclick (fun elt_button ev ->
	      Dom_html.stopPropagation ev;
	      on_focus_delete ())))
       
end

