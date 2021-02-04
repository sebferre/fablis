
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
  val dico_sugg : (string, 'suggestion) Hashtbl.t = Hashtbl.create 103

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
                 try
                   let sugg = Hashtbl.find dico_sugg cmd in
                   on_suggestion_selection sugg
                 with Not_found ->
                   Jsutils.alert "Command not understood"));
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
    Hashtbl.clear dico_sugg;
    let rec process_forest fsugg =
      List.iter
        (function
         | `Sugg sugg ->
            let cmd = command_of_suggestion sugg in
            if cmd <> "" then
              Hashtbl.add dico_sugg cmd sugg
         | `Dir (dir,children) ->
            process_forest children)
        fsugg
    in
    List.iter process_forest lfsugg
        
end
  
