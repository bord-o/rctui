(** Data tables with columns, headers, and styling. *)

open Mosaic_tea

type running = Yes | No | Unknown
type service_data = { service : string; enabled : bool; running : running }

type box_style = Rounded | Heavy | Double | Ascii | Minimal

type model = { style : box_style; selected : int; data : service_data list; count : int ;dims : (int * int); tick : int}
type direction = Up | Down
type msg = Cycle_style | Quit | Cycle_selected of direction * int | Resize of (int * int)

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
  | Resize dims -> print_endline "resize"; ({model with dims},Cmd.none)
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
      ({ model with selected; tick=model.tick+1 }, Cmd.none)
  | Cycle_selected (Down, n) ->
      let selected =
        if succ model.selected >= n then 0 else succ model.selected
      in
      ({ model with selected; tick=model.tick-1 }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let w, h = Terminal.size @@ Terminal.open_terminal ()
let columns =
  [
    Table.column ~header:(Table.cell "Service") ~width:(`Fixed (w/4)) ~justify:`Left
      "service";
    Table.column ~header:(Table.cell "Enabled") ~width:(`Fixed (w/4)) ~justify:`Right
      "running";
    Table.column ~header:(Table.cell "Running") ~width:(`Fixed (w/4)) ~justify:`Right
      "status";
  ]

let string_of_running = function
  | Yes -> "Yes"
  | No -> "No"
  | Unknown -> "Unknown"

let rows_of_data selected (sd : service_data list) =
  sd |> List.mapi @@ fun idx d ->
    let marker = if idx = selected then "► " else "  " in
    [
      Table.cell (marker ^ d.service);
      Table.cell (if d.enabled then "yes" else "no");
      Table.cell (string_of_running d.running);
    ]

let data = with_status ()  |> List.fast_sort (fun a b -> compare b.enabled a.enabled)


let init () =
    ({ style = Rounded; selected = 0; data; count = List.length data; dims=(w, h); tick=0 }, Cmd.none)


(* Palette *)
let footer_bg = Ansi.Color.grayscale ~level:3
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  (* let visible_count = 20 in (* adjust based on header/footer space *) *)
  let visible_count = max 1 ((snd model.dims - 6)/2) in
  let scroll_offset =
    if model.selected < visible_count / 2 then 0
    else if model.selected > model.count - visible_count / 2 then
      max 0 (model.count - visible_count)
    else model.selected - visible_count / 2
  in

  let visible_data =
    model.data
    |> List.filteri (fun i _ -> i >= scroll_offset && i < scroll_offset + visible_count)
  in

  let rows =
    (rows_of_data (model.selected - scroll_offset) visible_data) 
    |> List.mapi @@ fun idx a ->
       if idx = model.selected - scroll_offset then
         Table.row ~style:(Ansi.Style.make ~bold:true ~bg:Ansi.Color.Magenta ()) a
       else Table.row a
  in

  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_grow:1.
        ~size:{ width = pct 100; height = pct 100 }
        [
          box ~flex_direction:Column 
          (* box ~flex_direction:Column ~gap:(gap 2) *)
            ~size:{ width = pct 100; height = pct 100 }
            [
              table ~columns ~rows
                (* ~header_style:(Ansi.Style.make ~bold:true ~fg:(Ansi.Color.grayscale ~level:(23 + (model.tick mod 2))) ()) *)
                ~flex_grow:1.0 ~table_width:w
                ~box_style:(style_to_prop model.style)
                ~show_header:true ~show_edge:true ~show_lines:true
                (* ~table_padding:(1, 1, 1, 1) *)
                ~expand:true
                ~table_min_width:(w - (model.selected mod 2))
                ();
            ];
        ];
      box ~padding:(padding 1) ~background:footer_bg ~flex_direction:Row
        ~justify_content:Space_between
        ~size:{ width = pct 100; height = auto }
        [ 
          (* text ~text_style:hint "j/k navigate • q quit"; *)
            (* text ~text_style:hint (Printf.sprintf "h=%d vis=%d rows=%d" (snd model.dims) visible_count (List.length visible_data)); *)
          (* text ~text_style:hint (Printf.sprintf "%d/%d" (model.selected + 1) model.count); *)
        ];
    ]
let subscriptions model =
    Sub.batch [
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Cycle_style
          | Char c when Uchar.equal c (Uchar.of_char 'j') ->
              Some (Cycle_selected (Down, model.count))
          | Char c when Uchar.equal c (Uchar.of_char 'k') ->
              Some (Cycle_selected (Up, model.count))
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
        Sub.on_resize (fun ~width ~height -> Resize (width, height))
    ]

let () = run { init; update; view; subscriptions }
