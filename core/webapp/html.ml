
type t = string

type id = string

let selector_id id = "#" ^ id

(* utilities for generating HTML *)
			     
let pre text =
  let text = Regexp.global_replace (Regexp.regexp "<") text "&lt;" in
  let text = Regexp.global_replace (Regexp.regexp ">") text "&gt;" in  
  "<pre>" ^ text ^ "</pre>"

let span ?id ?classe ?title text =
  "<span" ^
    (match id with None -> "" | Some id -> " id=\"" ^ id ^ "\"") ^
    (match classe with None -> "" | Some cl -> " class=\"" ^ cl ^ "\"") ^
    (match title with None -> "" | Some tit -> " title=\"" ^ tit ^ "\"") ^
    ">" ^ text ^ "</span>"

let div ?id ?classe ?title text =
  "<div" ^
    (match id with None -> "" | Some id -> " id=\"" ^ id ^ "\"") ^
    (match classe with None -> "" | Some cl -> " class=\"" ^ cl ^ "\"") ^
    (match title with None -> "" | Some tit -> " title=\"" ^ tit ^ "\"") ^
    ">" ^ text ^ "</div>"

let a url html =
  "<a target=\"_blank\" href=\"" ^ url ^ "\">" ^ html ^ "</a>"

let img ?id ?classe ?height ~alt ~title url =
  "<img" ^
    (match id with None -> "" | Some i -> " id=\"" ^ i ^ "\"") ^
    (match classe with None -> "" | Some c -> " class=\"" ^ c ^ "\"") ^
    " src=\"" ^ url ^ "\"" ^
    (match height with None -> "" | Some h -> " height=\"" ^ string_of_int h ^ "\"") ^
    " alt=\"" ^ alt ^ "\" title=\"" ^ title ^ "\">"

let option ~value label =
  "<option value=\"" ^ value ^ "\">" ^ label ^ "</option>"						
let video url mime =
  "<video width=\"320\" height=\"240\" controls>\
   <source src=\"" ^ url ^ "\" type=\"" ^ mime ^ "\">\
   Your browser does not support the video tag.\
   </video>"

let audio url mime =
  "<audio controls>\
   <source src=\"" ^ url ^ "\" type=\"" ^ mime ^ "\">\
   Your browser does not support this audio format.\
   </audio>"

let glyphicon name = "<span class=\"glyphicon glyphicon-" ^ name ^ "\"></span>"

(*let open_new_window lis ~height uri =
  html_a uri (html_img ~classe:"open-new-window" ~height ~alt:"Open" "icon-open-new-window.png")*)

let table ?id ?classe ?title headers rows =
  assert (headers <> []);
  let buf = Buffer.create 1000 in
  let add s = Buffer.add_string buf s in
  add "<table";
  add (match id with None -> "" | Some id -> " id=\"" ^ id ^ "\"");
  add (match classe with None -> "" | Some cl -> " class=\"" ^ cl ^ "\"");
  add (match title with None -> "" | Some tit -> " title=\"" ^ tit ^ "\"");
  add "><tr>";
  headers |> List.iter (fun h ->
    add "<th class=\"header\">"; add h; add "</th>");
  add "</tr>";
  rows |> List.iter (fun row ->
    add "<tr>";
    row |> List.iter (fun cell ->
      add "<td>"; add cell; add "</td>");
    add "</tr>");
  add "</table>";
  Buffer.contents buf

								     
(* generic dictionary with automatic generation of keys *)

class ['a] dico (prefix : string) =
object
  val mutable cpt = 0
  val ht : (string,'a) Hashtbl.t = Hashtbl.create 101
  val rev_ht : ('a,string) Hashtbl.t = Hashtbl.create 101

  method add (x : 'a) : string =
    try Hashtbl.find rev_ht x
    with Not_found ->
      let k = cpt <- cpt + 1; prefix ^ string_of_int cpt in
      Hashtbl.add ht k x;
      Hashtbl.add rev_ht x k;
      k

  method add_key (key : string) (x : 'a) : unit =
    Hashtbl.add ht key x;
    Hashtbl.add rev_ht x key

  method get (key : string) : 'a =
    try Hashtbl.find ht key
    with _ ->
      Jsutils.firebug ("Missing element in dico: " ^ key);
      failwith "Html.dico#get"

  method get_key (x : 'a) : string option =
    try Some (Hashtbl.find rev_ht x)
    with Not_found -> None

  method clear : unit =
    cpt <- 0;
    Hashtbl.clear ht;
    Hashtbl.clear rev_ht
end


(* generating HTML for Syntax.xml *)
  
let focus_highlight h xml =
  if h
  then span ~classe:"highlighted" xml
  else xml

let icon_delete ?id ~title () =
  span ?id ~title (glyphicon "remove")

let focus_dropdown =
  span ~id:"focus-dropdown"
	    (glyphicon "menu-hamburger" ^
	       "<div id=\"focus-dropdown-content\" style=\"display:none\"></div>")
	 
let append_node_to_xml node xml =
  List.rev (node :: List.rev xml)
let append_node_to_xml_list node lxml =
  match List.rev lxml with
  | [] -> [[node]]
  | last::rest -> List.rev (append_node_to_xml node last :: rest)

let syntax ~(dico : 'focus dico)
	   ~(html_of_word : 'word -> t)
	   ~(html_of_input : 'input -> t)
	   (xml : ('word,'input,'focus) Syntax.xml) : t =
  let rec aux_xml ~highlight xml =
    let open Syntax in
    match xml with
    | Enum (sep,lxml) :: DeleteCurrentFocus :: xml ->
       aux_xml ~highlight (Enum (sep, append_node_to_xml_list DeleteCurrentFocus lxml) :: xml)
    | Coord (coord,lxml) :: DeleteCurrentFocus :: xml ->
       aux_xml ~highlight (Coord (coord, append_node_to_xml_list DeleteCurrentFocus lxml) :: xml)
    | Block lxml :: DeleteCurrentFocus :: xml ->
       aux_xml ~highlight (Block (append_node_to_xml_list DeleteCurrentFocus lxml) :: xml)
    | Focus (foc,xml1) :: DeleteCurrentFocus :: xml ->
       aux_xml ~highlight (Focus (foc, append_node_to_xml DeleteCurrentFocus xml1) :: xml)
    | Highlight xml1 :: DeleteCurrentFocus :: xml ->
       aux_xml ~highlight (Highlight (append_node_to_xml DeleteCurrentFocus xml1) :: xml)
    | Focus (foc1, xml1) :: Focus (foc2, xml2) :: xml when foc1 = foc2 -> aux_xml ~highlight (Focus (foc1, xml1 @ xml2) :: xml)
    | Highlight xml1 :: Highlight xml2 :: xml -> aux_xml ~highlight (Highlight (xml1 @ xml2) :: xml)
    | node :: xml -> aux_node ~highlight node ^ (if xml=[] then "" else " " ^ aux_xml ~highlight xml)
    | [] -> ""
  and aux_node ~highlight node = 
    let open Syntax in
    match node with
    | Kwd s -> s
    | Word w -> html_of_word w
    | Input dt -> html_of_input dt
    | Selection xml_selop -> aux_xml ~highlight xml_selop
    | Suffix (xml,suf) -> aux_xml ~highlight xml ^ suf
    | Enum (sep,lxml) -> String.concat sep (List.map (aux_xml ~highlight) lxml)
    | Quote (left, xml, right) -> left ^ aux_xml ~highlight xml ^ right
    | Coord (coord,lxml) ->
      "<ul class=\"coordination\"><li>"
      ^ String.concat ("</li><li> " ^ focus_highlight highlight (aux_xml ~highlight coord ^ " "))
	(List.map (fun xml -> focus_highlight highlight (aux_xml ~highlight xml)) lxml)
      ^ "</li></ul>"
    | Block lxml ->
       String.concat "<br>"
		     (List.map
			(fun xml -> focus_highlight highlight (aux_xml ~highlight xml))
			lxml)
    | Focus (focus,xml) ->
      let id = dico#add focus in
      let html = aux_xml ~highlight xml in
      span ~id ~classe:"focus" html
    | Highlight xml ->
       focus_highlight true
	 (aux_xml ~highlight:true xml)
    | Suspended xml ->
      span ~classe:"suspended" (aux_xml ~highlight xml)
    | DeleteCurrentFocus ->
       icon_delete ~id:"delete-current-focus" ~title:"Delete current focus" () ^
	 focus_dropdown
    | DeleteIncr ->
       icon_delete ~title:"Remove element at focus" ()
  in
  aux_xml ~highlight:false xml

