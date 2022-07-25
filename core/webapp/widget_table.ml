(**
   Widget for displaying results as a table.
   @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

open Js_of_ocaml
       
open Js
open Jsutils

(** The class of table widgets, parametrized by the type of columns (id and info per column) and cells.

[new widget ~id ~html_of_column ~html_of_cell] creates a new table widget in the HTML element identified by [id], where:
- [html_of_column (col_id,col_info)] returns a tuple [(id_opt,class_opt,title_opt,html)], where [html] is the HTML presentation of the column header, [id_opt] is an optional element identifier, [class_opt] an optional element CSS class, and [title_opt] an optional element title,
- [html_of_cell cell] returns the HTML presentation of a cell.
*)
class ['column_id,'column_info,'cell] widget
  ~(id : Html.id) (* where to insert the widget in the DOM *)
  ~(html_of_column : 'column_id * 'column_info -> string option * string option * string option * Html.t) (* id, class, title, html *)
  ~(html_of_cell : 'cell -> Html.t)
  =
object
  (** [sets_contents columns rows] defines the table contents by a list of columns and a list of rows. Each row is a (possibly partial) mapping from column ids to cells. *)
  method set_contents (cols : ('column_id * 'column_info) list) (rows : ('column_id * 'cell) list list) : unit =
    let html =
      Html.div
	~classe:"table-responsive"
	(Html.table
	   ~classe:"table table-bordered table-condensed table-hover"
	   (List.map html_of_column cols)
	   (List.map
	      (fun row ->
	       List.map
		 (fun (col_id,_) ->
		  try html_of_cell (List.assoc col_id row)
		  with Not_found -> "")
		 cols)
	      rows)) in
    jquery_set_innerHTML (Html.selector_id id) html
end
  
