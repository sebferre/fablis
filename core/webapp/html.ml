
type t = string

type id = string

let selector_id id = "#" ^ id

(* general utilities *)

let list_map_first (f : first:bool -> 'a -> 'b) (l : 'a list) : 'b list =
  let rec aux ~first = function
    | [] -> []
    | x::r -> f ~first x :: aux ~first:false r
  in
  aux ~first:true l
			     
(* utilities for generating HTML *)

let attr_opt name = function
  | None -> ""
  | Some id -> " " ^ name ^ "=\"" ^ id ^ "\""
			     
let pre text =
  let text = Regexp.global_replace (Regexp.regexp "<") text "&lt;" in
  let text = Regexp.global_replace (Regexp.regexp ">") text "&gt;" in  
  "<pre>" ^ text ^ "</pre>"

let span ?id ?classe ?title text =
  "<span"
  ^ attr_opt "id" id
  ^ attr_opt "class" classe
  ^ attr_opt "title" title
  ^ ">" ^ text ^ "</span>"

let div ?id ?classe ?title text =
  "<div"
  ^ attr_opt "id" id
  ^ attr_opt "class" classe
  ^ attr_opt "title" title
  ^ ">" ^ text ^ "</div>"

let a url html =
  "<a target=\"_blank\" href=\"" ^ url ^ "\">" ^ html ^ "</a>"

let img ?id ?classe ?height ~alt ~title url =
  "<img"
  ^ attr_opt "id" id
  ^ attr_opt "class" classe
  ^ " src=\"" ^ url ^ "\""
  ^ (match height with None -> "" | Some h -> " height=\"" ^ string_of_int h ^ "\"")
  ^ " alt=\"" ^ alt ^ "\" title=\"" ^ title ^ "\">"

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
  add (attr_opt "id" id);
  add (attr_opt "class" classe);
  add (attr_opt "title" title);
  add "><tr>";
  headers |> List.iter (fun (id_opt,classe_opt,title_opt,h) ->
    add "<th";
    add (attr_opt "id" id_opt);
    add (attr_opt "class" (match classe_opt with None -> Some "header" | Some cl -> Some ("header " ^ cl)));
    add (attr_opt "title" title_opt);
    add ">";
    add h;
    add "</th>");
  add "</tr>";
  rows |> List.iter (fun row ->
    add "<tr>";
    row |> List.iter (fun cell ->
      add "<td>"; add cell; add "</td>");
    add "</tr>");
  add "</table>";
  Buffer.contents buf


let ul ?id ?classe ?title items =
  let buf = Buffer.create 1000 in
  let add s = Buffer.add_string buf s in
  add "<ul";
  add (attr_opt "id" id);
  add (attr_opt "class" classe);
  add (attr_opt "title" title);
  add ">";
  items |> List.iter (fun (id_opt,classe_opt,title_opt,html) ->
    add "<li";
    add (attr_opt "id" id_opt);
    add (attr_opt "class" classe_opt);
    add (attr_opt "title" title_opt);
    add ">";
    add html;
    add "</li>");
  add "</ul>";
  Buffer.contents buf

let input ?id ?classe ?title ?placeholder input_type =
  "<input"
  ^ attr_opt "id" id
  ^ attr_opt "class" classe
  ^ attr_opt "title" title
  ^ attr_opt "type" (Some input_type)
  ^ attr_opt "placeholder" placeholder
  ^ ">"
		  
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

type input_update = string -> unit
type input_info =
  { input_type : string; (* text, checkbox, ... *)
    placeholder : string;
    input_update : input_update;
  }
type input_dico = input_update dico
				
let focus_highlight h xml =
  if h
  then span ~classe:"highlighted" xml
  else xml

let icon_focusup ?id ~title () =
  span ?id ~title (glyphicon "fullscreen" (*"chevron-up"*))
let icon_delete ?id ~title () =
  span ?id ~title (glyphicon "remove")

let focus_dropdown =
  span ~id:"focus-dropdown"
	    (glyphicon "menu-hamburger" ^
	       "<div id=\"focus-dropdown-content\" style=\"display:none\"></div>")

let focus_controls =
  icon_focusup ~id:"focusup-current-focus" ~title:"Move focus up" ()
  ^ icon_delete ~id:"delete-current-focus" ~title:"Delete current focus" ()
(* ^ focus_dropdown *) (* TODO *)
	    
let append_node_to_xml node xml =
  List.rev (node :: List.rev xml)
let append_node_to_xml_list node lxml =
  match List.rev lxml with
  | [] -> [[node]]
  | last::rest -> List.rev (append_node_to_xml node last :: rest)

let syntax ?(focus_dico : 'focus dico option)
	   ?(input_dico : input_dico option)
	   ~(html_of_word : 'word -> t)
	   ?(html_info_of_input : ('input -> input_info) option)
	   (xml : ('word,'input,'focus) Syntax.xml) : t =
  let rec aux_xml ~highlight ~linestart xml =
    let open Syntax in
    match xml with
    | Enum (sep,lxml) :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Enum (sep, append_node_to_xml_list ControlCurrentFocus lxml) :: xml)
    | Coord (coord,lxml) :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Coord (coord, append_node_to_xml_list ControlCurrentFocus lxml) :: xml)
    | Block lxml :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Block (append_node_to_xml_list ControlCurrentFocus lxml) :: xml)
    | Indent xml1 :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Indent (xml1 @ [ControlCurrentFocus]) :: xml)
    | Focus (foc,xml1) :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Focus (foc, append_node_to_xml ControlCurrentFocus xml1) :: xml)
    | Highlight xml1 :: ControlCurrentFocus :: xml ->
       aux_xml ~highlight ~linestart (Highlight (append_node_to_xml ControlCurrentFocus xml1) :: xml)
    | Focus (foc1, xml1) :: Focus (foc2, xml2) :: xml when foc1 = foc2 ->
       aux_xml ~highlight ~linestart (Focus (foc1, xml1 @ xml2) :: xml)
    | Highlight xml1 :: Highlight xml2 :: xml ->
       aux_xml ~highlight ~linestart (Highlight (xml1 @ xml2) :: xml)
    | node :: xml ->
       aux_node ~highlight ~linestart node ^ (if xml=[] then "" else " " ^ aux_xml ~highlight ~linestart:false xml)
    | [] -> ""
  and aux_node ~highlight ~linestart node = 
    let open Syntax in
    match node with
    | Kwd s -> s
    | Word w -> html_of_word w
    | Input i ->
       ( match input_dico, html_info_of_input with
	 | None, _
	 | _, None -> failwith "Html.syntax: unexpected input"
	 | Some input_dico, Some html_info_of_input ->
	    let info = html_info_of_input i in
	    let key = input_dico#add info.input_update in
	    input ~id:key ~classe:"suggestion-input" ~placeholder:info.placeholder info.input_type )
    | Selection xml_selop -> aux_xml ~highlight ~linestart xml_selop
    | Suffix (xml,suf) ->
       aux_xml ~highlight ~linestart xml ^ suf
    | Enum (sep,lxml) ->
       String.concat sep
		     (list_map_first
			(fun ~first xml -> aux_xml ~highlight ~linestart:(linestart && first) xml)
			lxml)
    | Quote (left, xml, right) ->
       left ^ aux_xml ~highlight ~linestart:false xml ^ right
    | Coord (coord,lxml) ->
       let lxml_coord =
	 list_map_first
	   (fun ~first xml -> if first then xml else coord @ xml)
	   lxml in
       let node = Block lxml_coord in
       let node = if linestart then node else Indent [node] in
       aux_node ~highlight ~linestart node
    | Block lxml ->
       String.concat ""
		     (List.map
			(fun xml -> div (focus_highlight highlight (aux_xml ~highlight ~linestart:true xml)))
			lxml)
    | Indent xml ->
       div ~classe:"indented" (focus_highlight highlight (aux_xml ~highlight ~linestart:true xml))
    | Focus (focus,xml) ->
      let html = aux_xml ~highlight ~linestart xml in
      ( match focus_dico with
	| None -> html
	| Some focus_dico ->
	   let id = focus_dico#add focus in
	   span ~id ~classe:"focus" html )
    | Highlight xml ->
       focus_highlight true
	 (aux_xml ~highlight:true ~linestart xml)
    | Suspended xml ->
      span ~classe:"suspended" (aux_xml ~highlight ~linestart xml)
    | ControlCurrentFocus ->
       focus_controls
    | DeleteIncr ->
       icon_delete ~title:"Remove element at focus" ()
  in
  div (aux_xml ~highlight:false ~linestart:true xml)

