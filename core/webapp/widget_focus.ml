(**
   Widget for displaying, moving, and deleting a focus.
   @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

open Js_of_ocaml

open Js
open Jsutils

(** The class of focus widgets.

    [new widget ~id ~html_of_word] creates a new focus widget in the HTML element identified by [id], and where function [html_of_word] is used to generate HTML elements for custom words. 

    The widget has control buttons to move the focus up, or to delete the part under focus. Each part of the displayed syntax can be clciked to change/move the focus.
*)
class ['word,'input,'focus] widget
  ~(id : Html.id) (* where to insert the widget in the DOM *)
  ~(html_of_word : 'word -> Html.t) (* rendering words *)
  =
object
  val focus_dico : 'focus Html.dico = new Html.dico (id ^ "-focus")

  val mutable on_focus_change : 'focus -> unit = fun foc -> ()
  (** Defines the function to be called on the focus of the part that has been clicked. *)
  method on_focus_change (f : 'focus -> unit) : unit = on_focus_change <- f

  val mutable on_focus_up : unit -> unit = fun () -> ()
  (** Defines the function to be called when the button for moving the focus up has been clicked. *)
  method on_focus_up f = on_focus_up <- f

  val mutable on_focus_delete : unit -> unit = fun () -> ()
  (** Defines the function to be called when the button for deleting the part under focus has been clicked. *)
  method on_focus_delete f = on_focus_delete <- f

  val mutable on_focus_delete_constr : unit -> unit = fun () -> ()
  (** Defines the function to be called when the button for deleting the constructor under focus has been clicked. *)
  method on_focus_delete_constr f = on_focus_delete_constr <- f

  (** Displays the given syntax in the widget after converting it into HTML. The class argument [html_of_word] is used for the conversion of custom words (see {!Syntax}). *)
  method set_syntax (xml : ('word,'input,'focus) Syntax.xml) : unit =
    focus_dico#clear;
    jquery (Html.selector_id id)
      (fun elt ->
        let html = Html.syntax ~focus_dico ~html_of_word xml in
	elt##.innerHTML := string html;
	stop_links_propagation_from elt;
	jquery_all_from elt ".focus"
	   (onclick (fun elt_foc ev ->
	      Dom_html.stopPropagation ev;
	      let key = to_string elt_foc##.id in
	      let foc = focus_dico#get key in
	      on_focus_change foc));
	jquery_from elt "#focusup-current-focus"
	   (onclick (fun elt_button ev ->
	      Dom_html.stopPropagation ev;
	      on_focus_up ()));
	jquery_from elt "#delete-current-focus"
	   (onclick (fun elt_button ev ->
	      Dom_html.stopPropagation ev;
	      on_focus_delete ()));
	jquery_from elt "#delete-current-constr"
	   (onclick (fun elt_button ev ->
	      Dom_html.stopPropagation ev;
	      on_focus_delete_constr ())))
       
end
