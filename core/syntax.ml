(** Utilities for the syntactic representation of focuses.
    @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

(** The type for the XML-like representation of the syntax of focuses and suggestions.
- ['word] is the type of custom words
- ['input] is the type of user-editable parts
- ['focus] is the type of focuses (that are representeed by this syntax

 *)
type ('word,'input,'focus) xml = ('word,'input,'focus) node list
and ('word,'input,'focus) node =
  | Kwd of string (** keyword, a word with no special function *)
  | Word of 'word (** a custom word *)
  | Input of 'input (** a user-editable part *)
  | Selection of ('word,'input,'focus) xml (** a suggestion selection operator *)
  | Suffix of ('word,'input,'focus) xml * string (** adding a suffix to some syntax (e.g., punctuation) *)
  | Enum of string * ('word,'input,'focus) xml list (** enumeration of syntaxes, separated by a given string (e.g., comma), on a single line *)
  | Quote of string * ('word,'input,'focus) xml * string (** quoted syntax, with opening and closing quotation (e.g., double quotes, brackets) *)
  | Coord of ('word,'input,'focus) xml * ('word,'input,'focus) xml list (** coordination of a list of syntaxes, with a given operator syntax (e.g., 'and', 'or'), multiline *)
  | Block of ('word,'input,'focus) xml list (** block of syntaxes, one per line *)
  | Indent of ('word,'input,'focus) xml (** indented syntax *)
  | Focus of 'focus * ('word,'input,'focus) xml (** tagging a syntax with a focus, pointing into this syntax moves to this focus *)
  | Highlight of ('word,'input,'focus) xml (** highlight somehow (e.g., yellow background) this syntax to show that it belongs to the current focus *)
  | Suspended of ('word,'input,'focus) xml (** suspends somehow (e.g., grey text color) this syntax to show that it is inactive at the current focus *)
  | ControlCurrentFocus (** control widget that can contain on-focus suggestions *)
  | DeleteIncr (** control for deleting the part under focus *)

(** The class type specifying lexicons, which provide mappings from syntax words and inputs to strings. *)
class type ['word,'input] lexicon =
  object
    method word_text : 'word -> string
    method input_text : 'input -> string
  end

(** Returns a string representation of a syntax, given an application-specific lexicon. *)
let rec xml_text_content (lexicon : ('word,'input) lexicon) (l : ('word,'input,'focus) xml) : string =
  String.concat " " (List.map (xml_node_text_content lexicon) l)
(** The same for nodes. *)
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

(** Builds the syntax of a list from a list focus.

    [xml_list_ctx f x ([...; y], [...; z]) xml_x] returns the list of syntaxes [[xml_y; ...; xml_x; ...; xml_z]], a mapping from the list zipped from [x] and [([...; y], [...; z])] to syntaxes, i.e. [xml] value. Apart from [xml_x], each syntax element is obtained by applying function [f] to the corresponding element and its list context.

   For example, [xml_list_ctx f x2 ([x1; x0], [x3; x4]) xml_x2] returns the list [[f x0 ([],[x1;x2;x3;x4]); f x1 ([x0]; [x2;x3;x4]); xml_x2; f x3 ([x2;x1;x0],[x4]); f x4 ([x3;x2;x1;x0],[])]]. *)
let xml_list_ctx (f : 'a -> 'a Focus.list_ctx -> ('word,'input,'focus) xml)
		 (x : 'a) (ll_rr : 'a Focus.list_ctx)
		 (xml_x : ('word,'input,'focus) xml)
    : ('word,'input,'focus) xml list =
  Focus.list_of_ctx
    xml_x
    (Focus.ctx_of_list_ctx x ll_rr
     |> Focus.map_list_ctx
	  (fun (x1,ll_rr1) -> f x1 ll_rr1))

		       
    
