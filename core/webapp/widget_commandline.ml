(**
   Widget selecting suggestions through a commandline.
   @author Sébastien Ferré (ferre AT irisa DOT fr)
 *)

open Js_of_ocaml

open Js
open Jsutils

(** An alias type for commands as strings. *)
type command = string

(** An alias type for matching scores as floats. *)
type score = float (* in [0,1] *)

(** The class of commandline widgets.

[new widget ~id ~html_of_suggestion ~score_of_suggestion ~command_of_suggestion] creates a new commandline widget in the HTML element identified by [id], where:
- [html_of_suggestion ~input_dico sugg] must return the HTML representation of the suggestion [sugg], using an input dictionary,
- [score_of_suggestion sugg cmd] returns the matching score of suggestion [sugg] against command [cmd],
- [command_of_suggestion sugg] returns a command that selects suggestion [sugg].

 *)
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
              ~classe:"commandline-input"
              "text" in
          elt##.innerHTML := string html;
          jquery_input_from elt "input"
            (onenter
               (fun input ev ->
                 let cmd = to_string input##.value in
                 let best_score, lsugg =
                   self#best_match_suggestions cmd in
                 match lsugg with
                 | [] -> (* The command was not understood *)
                    input##.style##.color := string "red"
                 | [sugg] ->
                    on_suggestion_selection sugg
                 | _ -> (* The command is ambiguous *)
                    input##.style##.color := string "orange"
            ));
          jquery_input_from elt "input"
            (oninput
               (fun input ev ->
                 input##.style##.color := string "black"));
          initialized <- true)

  (** Sets the set of available suggestions. *)
  method set_suggestions (lfsugg : 'suggestion Lis.forest list) : unit =
    self#init;
    current_suggestions <- lfsugg

  (** Defines the function to be called when a suggestion is selected by the input command. *)
  method on_suggestion_selection (f : 'suggestion -> unit) : unit =
    on_suggestion_selection <- f

  (** Sets the placeholder of the commandline input as the command of the given suggestion [sugg]. *)
  method selected_suggestion sugg =
    jquery_input (Html.selector_id id ^ " input")
      (fun input ->
          input##.value := string "";
          input##.placeholder := string (command_of_suggestion sugg))
    
end
  
