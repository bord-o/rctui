(** Data tables with columns, headers, and styling. *)

open Mosaic_tea

type running = Yes | No | Unknown
type service_data = { service : string; enabled : bool; running : running }
type box_style = Rounded | Heavy | Double | Ascii | Minimal
type model = { style : box_style; selected : int; data : service_data list; count : int }
type direction = Up | Down
type msg = Cycle_style | Quit | Cycle_selected of direction * int

let style_to_prop = function
  | Rounded -> Table.Rounded
  | Heavy -> Heavy
  | Double -> Double
  | Ascii -> Ascii
  | Minimal -> Minimal

let style_name = function
  | Rounded -> "Rounded"
  | Heavy -> "Heavy"
  | Double -> "Double"
  | Ascii -> "ASCII"
  | Minimal -> "Minimal"

let enabled () =
  let ic = Unix.open_process_in "service -e" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'
  |> List.map (fun s -> Filename.basename s)

let services () =
  let ic = Unix.open_process_in "service -l" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'

let is_running name =
  let ic =
    Unix.open_process_in (Printf.sprintf "service %s onestatus 2>&1" name)
  in
  let output = try input_line ic with End_of_file -> "" in
  let _ = Unix.close_process_in ic in
  try
    let _ = Str.search_forward (Str.regexp "unknown directive") output 0 in
    Unknown
  with Not_found -> (
    try
      let _ = Str.search_forward (Str.regexp "is running") output 0 in
      Yes
    with Not_found -> No)

let with_status () =
  let services = services () in
  let enabled = enabled () in
  services
  |> List.map @@ fun service ->
     let enabled = List.mem service enabled in
     let running = is_running service in
     { service; enabled; running }


let update msg model =
  match msg with
  | Cycle_style ->
      let style =
        match model.style with
        | Rounded -> Heavy
        | Heavy -> Double
        | Double -> Ascii
        | Ascii -> Minimal
        | Minimal -> Rounded
      in
      ({ model with style }, Cmd.none)
  | Cycle_selected (Up, n) ->
      let selected =
        if pred model.selected < 0 then pred n else pred model.selected
      in
      ({ model with selected }, Cmd.none)
  | Cycle_selected (Down, n) ->
      let selected =
        if succ model.selected >= n then 0 else succ model.selected
      in
      ({ model with selected }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let columns =
  [
    Table.column ~header:(Table.cell "Service") ~width:`Auto ~justify:`Left
      "service";
    Table.column ~header:(Table.cell "Running") ~width:`Auto ~justify:`Left
      "running";
    Table.column ~header:(Table.cell "Status") ~width:`Auto ~justify:`Right
      "status";
  ]

let string_of_running = function
  | Yes -> "Yes"
  | No -> "No"
  | Unknown -> "Unknown"

let rec rows_of_data (sd : service_data list) =
  match sd with
  | [] -> []
  | d :: ds ->
      [
        Table.cell d.service;
        Table.cell (d.enabled |> string_of_bool);
        Table.cell (string_of_running d.running);
      ]
      :: rows_of_data ds

let data = with_status () 
let init () =
  ({ style = Rounded; selected = 0; data; count = List.length data }, Cmd.none)

let rows = 
    (rows_of_data data) 

(* Palette *)
let footer_bg = Ansi.Color.grayscale ~level:3
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let w, h = Terminal.size @@ Terminal.open_terminal ()

let view model =

  let rows =
    rows |> List.mapi @@ fun idx a ->
       if idx = model.selected then
         Table.row

           ~style:(Ansi.Style.make ~bold:true ~bg:Ansi.Color.Magenta ())
           a
       else Table.row a
  in

  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_grow:1.
        ~size:{ width = pct 100; height = pct 100 }
        [
          box ~flex_direction:Column ~gap:(gap 2)
            ~size:{ width = pct 100; height = pct 100 }
            [
              (* Table with current style *)
              table ~columns ~rows
                (* ~key:(string_of_int model.selected) *)
                ~flex_grow:1.0 ~table_width:w
                ~box_style:(style_to_prop model.style)
                ~show_header:true ~show_edge:true ~show_lines:true
                ~table_padding:(1, 1, 1, 1)
                ~expand:true (* ~size:{width=pct 100; height=pct 100} *)
                ~header_style:(Ansi.Style.make ~bold:true ())
                (* ~row_styles: *)
                (*   [ *)
                (*     Ansi.Style.default; *)
                (*     Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) (); *)
                (*   ] *)
                ();
              (* Style indicator *)
              text ~text_style:hint
                (Printf.sprintf "Style: %s" (style_name model.style));
              text ~text_style:hint (Printf.sprintf "idx: %d" model.selected);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~text_style:hint "s cycle style  •  q quit" ];
    ]

let subscriptions model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Cycle_style
      | Char c when Uchar.equal c (Uchar.of_char 'j') ->
          Some (Cycle_selected (Down, model.count))
      | Char c when Uchar.equal c (Uchar.of_char 'k') ->
          Some (Cycle_selected (Up, model.count))
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
