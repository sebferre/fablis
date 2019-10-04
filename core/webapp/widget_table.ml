
open Js
open Jsutils

class ['column,'cell] widget
  ~(id : Html.id) (* where to insert the widget in the DOM *)
  ~(html_of_column : 'column -> string option * string option * string option * Html.t) (* id, class, title, html *)
  ~(html_of_cell : 'cell -> Html.t)
  =
object
  method set_contents (cols : 'column list) (rows : ('column * 'cell) list list) : unit =
    let html =
      Html.div
	~classe:"table-responsive"
	(Html.table
	   ~classe:"table table-bordered table-condensed table-hover"
	   (List.map html_of_column cols)
	   (List.map
	      (fun row ->
	       List.map
		 (fun col ->
		  try html_of_cell (List.assoc col row)
		  with Not_found -> "")
		 cols)
	      rows)) in
    jquery_set_innerHTML (Html.selector_id id) html
end
  
