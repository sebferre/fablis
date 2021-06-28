(** Utilities relative to the definition of a focus as a Huet's zipper.
    A focus is composed of an element and a context.
    @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

(** {1 List Contexts} *)

(** The type of list contexts, for use in focuses. The first part is the
   sublist of elements on the left, in reverse order, and the second
   part is the sublist of elements on the right.

   For example, given a list [[0;1;2;3;4]], the list context of element [2] is the pair [([1;0],[3;4])], and the list context of element [3] is  [([2;1;0],[4])]. *)
type 'a list_ctx = 'a list * 'a list

(** The type of list focuses. A list focus is made of a list element and its list context. *)
type 'a list_focus = 'a * 'a list_ctx
                 
(** Zips a list context [(ll,rr)] with an element [x] in the middle. It returns [List.rev ll @ x :: rr].

    If [ll] is [[y1; ...; yM]], and [rr] is [[z1; ...; zN]], the result is [[yM; ...; y1; x; z1; ...; zN]]. *)
let list_of_ctx (x : 'a) (ll,rr : 'a list_ctx) : 'a list = List.rev_append ll (x :: rr)

(** Zips a list context [(ll,rr)] with elements [lx] in the middle. It returns [List.rev ll @ lx @ rr]*)
let list_of_ctx_many (lx : 'a list) (ll, rr : 'a list_ctx) : 'a list = List.rev_append ll (lx @ rr)

(** Zips a list context [(ll,rr)] with no element in the middle. It returns [List.rev ll @ rr].*)
let list_of_ctx_none (ll, rr : 'a list_ctx) : 'a list = List.rev_append ll rr

(** Unzips a list [lr] in all possible positions, returning a list of list focuses, one for each element of [lr].

   If [lr] is [\[x1;x2;x3\]], the result is [[(x1,([],[x2;x3]); (x2,([x1],[x3])); (x3,([x2;x1],[]))]]. *)
let focus_list_of_list (lr : 'a list) : 'a list_focus list =
  let rec aux ll = function
    | [] -> []
    | x::rr -> (x,(ll,rr)) :: aux (x::ll) rr
  in
  aux [] lr

(** Given a list focus [x, (ll,rr)], returns a list
   context composed of the left element focuses, and the right element
   focuses. 

   If [ll] is [[y1;y2]], and [rr] is [[z1]], the result is
   [([(y1,([y2],[x;z1])); (y2,([],[y1;x;z1]))], [(z1,([x;y1;y2],[]))])]. *)
let ctx_of_list_focus (x, (ll,rr) : 'a list_focus) : 'a list_focus list_ctx =
  let rec aux_left rr = function
    | [] -> []
    | x1::ll1 -> (x1, (ll1,rr)) :: aux_left (x1::rr) ll1
  in
  let rec aux_right ll = function
    | [] -> []
    | x1::rr1 -> (x1, (ll,rr1)) :: aux_right (x1::ll) rr1
  in
  aux_left (x::rr) ll, aux_right (x::ll) rr

(** The map functional on list contexts, applying function [f] on each left and right element. *)
let map_list_ctx (f : 'a -> 'b) (ll,rr : 'a list_ctx) : 'b list_ctx = (List.map f ll, List.map f rr)

(** Returns a list with size [arity], filled with element [x]. *)
let rec make_list (arity : int) (x : 'a) : 'a list =
  if arity = 0
  then []
  else x :: make_list (arity-1) x

  
(** {1 Focus Paths} *)

(** Type of path elements, which are navigation steps in an algebraic
   data structure.  

   [DOWN] is moving down to the leftmost child, and
   [RIGHT] is moving to the first sibling on the right. *)
type step = DOWN | RIGHT [@@deriving yojson]
(** (De)Serializing functions between steps and JSON. *)

(** Type of focus paths, which are lists of steps, starting from the
   root. *)
type path = step list [@@deriving yojson]
(** (De)Serializing functions between paths and JSON. *)

(** Exception raised when a path is not compatible with a data
   structure. *)
exception Invalid_path of path

(** Returns a focus path that navigates to a child data structure at some position. 

    [down_rights n] consists of one [DOWN] step followed by [n] [RIGHT] steps.
    @raise Invalid_arg if [n] is negative. *)
let down_rights (n : int) : path =
  let rec rights n =
    if n < 0 then invalid_arg "Focus.down_rights"
    else if n = 0 then []
    else RIGHT :: rights (n-1) in
  DOWN :: rights n

(** Returns a path navigating to the list context (a number of [RIGHT] steps), and continuing with the given path.
    
    [path_of_list_ctx (ll,rr) path] returns the path [RIGHT::...::RIGHT::path], prefixing [path] with as many [RIGHT] steps as there are elements in [ll]. *)
let path_of_list_ctx (ll,rr : 'a list_ctx) (path : path) : path =
  List.fold_left
    (fun path _ -> RIGHT::path)
    path ll

(** Returns a list focus by navigating into a list according to a given path. Also return the remaining path.

    [list_focus_of_path_list path lr] returns a triple [(path',(ll,rr),x)] where [x] is an element of [lr] at position K, where [(ll,rr)] is the list context of [x] in [lr], where the length of [ll] is hence K, where [path] starts with exactly K [RIGHT] steps, and where [path'] is the suffix of [path] starting after the K [RIGHT] steps.

   Raise [Invalid_path path'] if K is greater or equal to the length N of [ll], where [path'] is the remainder path after removing the N first [RIGHT] steps. *)
let list_focus_of_path_list (path : path) (lr : 'a list) : path * 'a list_focus =
  let rec aux path (ll,rr) x =
    match path, rr with
    | RIGHT::_, [] -> raise (Invalid_path path)
    | RIGHT::path1, y::rr1 -> aux path1 (x::ll,rr1) y
    | _ -> path, (x,(ll,rr))
  in
  match lr with
  | [] -> raise (Invalid_path path)
  | x::rr -> aux path ([],rr) x

           
(** {1 Focus Inputs} *)

(** A generic class for user-editable parts of focus and suggestions, initialized with a default value. At this stage, it behaves like a reference. *)
class ['a] input (default : 'a) =
object
  val mutable v : 'a = default
  method set (new_v : 'a) = v <- new_v
  method get : 'a = v
end

