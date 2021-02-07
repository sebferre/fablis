
open Js_of_ocaml

open Js
open Jsutils

type command = string
type score = float (* in [0,1] *)
   
class ['suggestion] widget
        ~(id : Html.id)
        ~(html_of_suggestion : input_dico:Html.input_dico -> 'suggestion -> Html.t)
        ~(score_of_suggestion : 'suggestion -> command -> score)
        (* how much does the suggestion matches the command *)
        ~(command_of_suggestion : 'suggestion -> command)
        (* a standard command for that suggestion *)
  =
object (self)

  val mutable initialized = false
  val mutable on_suggestion_selection : 'suggestion -> unit = fun sugg -> ()
  val mutable current_suggestions : 'suggestion Lis.forest list = []

  method private best_match_suggestions (cmd : string) : score * 'suggestion list =
    let rec aux res fsugg =
      List.fold_left
        (fun (best_score,lsugg as res) ->
          function
          | `Sugg sugg ->
             let score = score_of_suggestion sugg cmd in
             if score > best_score then
               (score, [sugg])
             else if score > 0. && score = best_score then
               (best_score, sugg::lsugg)
             else res
          | `Dir (dir,children) ->
             aux res children)
        res fsugg
    in
    List.fold_left aux (0.,[]) current_suggestions
                                                                
  method private init =
    if not initialized then
      jquery (Html.selector_id id)
        (fun elt ->
          let html =
            Html.input
              ~title:"Enter a short command for selecting a suggestion"
              "text" in
          elt##.innerHTML := string html;
          jquery_input_from elt "input"
            (onenter
               (fun input ev ->
                 let cmd = to_string input##.value in
                 let best_score, lsugg =
                   self#best_match_suggestions cmd in
                 match lsugg with
                 | [] -> Jsutils.alert "The command was not understood"
                 | [sugg] -> on_suggestion_selection sugg
                 | _ -> Jsutils.alert "The command is ambiguous"));
          initialized <- true)

  method selected_suggestion sugg =
    jquery_input (Html.selector_id id ^ " input")
      (fun input ->
          input##.value := string "";
          input##.placeholder := string (command_of_suggestion sugg))
    
  method on_suggestion_selection f =
    on_suggestion_selection <- f

  method set_suggestions (lfsugg : 'suggestion Lis.forest list) : unit =
    self#init;
    current_suggestions <- lfsugg
        
end
  
