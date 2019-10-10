
type ('word,'input,'focus) xml = ('word,'input,'focus) node list
and ('word,'input,'focus) node =
  | Kwd of string
  | Word of 'word
  | Input of 'input
  | Selection of ('word,'input,'focus) xml (* [xml] represents the selection operator *)
  | Suffix of ('word,'input,'focus) xml * string (* suffix: eg. !, 's *)
  | Enum of string * ('word,'input,'focus) xml list (* separator: eg. commas *)
  | Quote of string * ('word,'input,'focus) xml * string (* quoted xml *)
  | Coord of ('word,'input,'focus) xml * ('word,'input,'focus) xml list (* coordination: eg. 'and' *)
  | Block of ('word,'input,'focus) xml list
  | Indent of ('word,'input,'focus) xml
  | Focus of 'focus * ('word,'input,'focus) xml
  | Highlight of ('word,'input,'focus) xml
  | Suspended of ('word,'input,'focus) xml
  | ControlCurrentFocus
  | DeleteIncr

class type ['word,'input] lexicon =
  object
    method word_text : 'word -> string
    method input_text : 'input -> string
  end
      
let rec xml_text_content (lexicon : ('word,'input) lexicon) (l : ('word,'input,'focus) xml) : string =
  String.concat " " (List.map (xml_node_text_content lexicon) l)
and xml_node_text_content lexicon = function
  | Kwd s -> s
  | Word w -> lexicon#word_text w
  | Input i -> lexicon#input_text i
  | Selection xml_selop -> ""
  | Suffix (x,suf) -> xml_text_content lexicon x ^ suf
  | Enum (sep, xs) -> String.concat sep (List.map (xml_text_content lexicon) xs)
  | Quote (left, x, right) -> left ^ xml_text_content lexicon x ^ right
  | Coord (xsep,xs) -> String.concat (" " ^ xml_text_content lexicon xsep ^ " ") (List.map (xml_text_content lexicon) xs)
  | Block xs -> String.concat "\n" (List.map (xml_text_content lexicon) xs)
  | Indent x -> xml_text_content lexicon x
  | Focus (foc,x) -> xml_text_content lexicon x
  | Highlight x -> xml_text_content lexicon x
  | Suspended x -> xml_text_content lexicon x
  | ControlCurrentFocus -> ""
  | DeleteIncr -> ""

let xml_list_ctx (f : 'a -> 'a Focus.list_ctx -> ('word,'input,'focus) xml)
		 (x : 'a) (ll_rr : 'a Focus.list_ctx)
		 (xml_x : ('word,'input,'focus) xml)
    : ('word,'input,'focus) xml list =
  Focus.list_of_ctx
    xml_x
    (Focus.ctx_of_list_ctx x ll_rr
     |> Focus.map_list_ctx
	  (fun (x1,ll_rr1) -> f x1 ll_rr1))

		       
    
