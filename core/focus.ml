
type 'a list_ctx = 'a list * 'a list 

let list_of_ctx (x : 'a) (ll,rr : 'a list_ctx) : 'a list = List.rev ll @ x :: rr

let list_of_ctx_many (lx : 'a list) (ll, rr : 'a list_ctx) : 'a list = List.rev ll @ lx @ rr
								
let ctx_of_list (lr : 'a list) : ('a * 'a list_ctx) list =
  let rec aux ll = function
    | [] -> []
    | x::rr -> (x,(ll,rr)) :: aux (x::ll) rr
  in
  aux [] lr

let ctx_of_list_ctx (x : 'a) (ll,rr : 'a list_ctx) : ('a * 'a list_ctx) list_ctx =
  let rec aux_left rr = function
    | [] -> []
    | x1::ll1 -> (x1, (ll1,rr)) :: aux_left (x1::rr) ll1
  in
  let rec aux_right ll = function
    | [] -> []
    | x1::rr1 -> (x1, (ll,rr1)) :: aux_right (x1::ll) rr1
  in
  aux_left (x::rr) ll, aux_right (x::ll) rr
    
let map_list_ctx (f : 'a -> 'b) (ll,rr : 'a list_ctx) : 'b list_ctx = (List.map f ll, List.map f rr)

									
type step = DOWN | RIGHT
type path = step list

let path_of_list_ctx (ll,rr) path =
  List.fold_left
    (fun path _ -> RIGHT::path)
    path ll

let list_focus_of_path_list path lr =
  let rec aux path (ll,rr) x =
    match path, rr with
    | RIGHT::_, [] -> assert false
    | RIGHT::path1, y::rr1 -> aux path1 (x::ll,rr1) y
    | _ -> path, (ll,rr), x
  in
  match lr with
  | [] -> assert false
  | x::rr -> aux path ([],rr) x
			     
