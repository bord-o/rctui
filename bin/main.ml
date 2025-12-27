(** Data tables with columns, headers, and styling. *)

open Mosaic_tea

type running = Yes | Unknown | No
type service_data = { service : string; enabled : bool; running : running }
type direction = Up | Down

type sort_by =
  | Enabled of direction
  | Alpha of direction
  | Running of direction

type model = {
  selected : int;
  data : service_data list;
  count : int;
  dims : int * int;
  tick : int;
  sort : sort_by;
}

type msg =
  | Quit
  | Cycle_selected of direction * int
  | Cycle_sort
  | Resize of (int * int)
  | Refresh

let enabled () =
  let ic = Unix.open_process_in "service -e" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'
  |> List.map (fun s -> Filename.basename s)

let services () =
  let ic = Unix.open_process_in "service -l" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'

let contains sub s =
  let len_sub = String.length sub in
  let len_s = String.length s in
  if len_sub > len_s then false
  else
    let rec check i =
      if i > len_s - len_sub then false
      else if String.sub s i len_sub = sub then true
      else check (i + 1)
    in
    check 0

let is_running env name =
  let proc_mgr = Eio.Stdenv.process_mgr env in
  let null = Eio.Path.(Eio.Stdenv.fs env / "/dev/null") in
  Eio.Path.with_open_out null ~create:`Never @@ fun null_out ->
  let output =
    Eio.Process.parse_out ~stderr:null_out ~is_success:(Fun.const true) proc_mgr
      Eio.Buf_read.take_all
      [ "service"; name; "onestatus" ]
  in
  if contains "is running" output then Yes
  else if contains "is not running" output then No
  else Unknown

let with_status () =
  let services = services () in
  let enabled = enabled () in
  Eio_main.run @@ fun env ->
  Eio.Fiber.List.map
    (fun service ->
      {
        service;
        enabled = List.mem service enabled;
        running = is_running env service;
      })
    services

let next_sort = function
  | Alpha Up -> Alpha Down
  | Alpha Down -> Enabled Up
  | Enabled Up -> Enabled Down
  | Enabled Down -> Running Up
  | Running Up -> Running Down
  | Running Down -> Alpha Up

let update msg model =
  match msg with
  | Resize dims ->
      print_endline "resize";
      ({ model with dims }, Cmd.none)
  | Cycle_selected (Up, n) ->
      let selected =
        if pred model.selected < 0 then pred n else pred model.selected
      in
      ({ model with selected; tick = model.tick + 1 }, Cmd.none)
  | Cycle_selected (Down, n) ->
      let selected =
        if succ model.selected >= n then 0 else succ model.selected
      in
      ({ model with selected; tick = model.tick - 1 }, Cmd.none)
  | Quit -> (model, Cmd.quit)
  | Refresh ->
      let data =
        with_status ()
        |> List.fast_sort (fun a b -> compare b.enabled a.enabled)
      in
      ({ model with data; count = List.length data }, Cmd.none)
  | Cycle_sort ->
      let sort = next_sort model.sort in
      let data =
        model.data
        |> List.fast_sort (fun a b ->
            match sort with
            | Enabled Up -> compare b.enabled a.enabled
            | Enabled Down -> compare a.enabled b.enabled
            | Alpha Up -> compare b.service a.service
            | Alpha Down -> compare a.service b.service
            | Running Up -> compare b.running a.running
            | Running Down -> compare a.running b.running)
      in
      ({ model with sort; data }, Cmd.none)

let w, h = Terminal.size @@ Terminal.open_terminal ()

let string_of_running = function
  | Yes -> "Yes"
  | No -> "No"
  | Unknown -> "Unknown"

let rows_of_data selected (sd : service_data list) =
  sd
  |> List.mapi @@ fun idx d ->
     let marker = if idx = selected then "► " else "  " in
     [
       Table.cell (marker ^ d.service);
       Table.cell (if d.enabled then "yes" else "no");
       Table.cell (string_of_running d.running);
     ]

let data = []
(* with_status () |> List.fast_sort (fun a b -> compare b.enabled a.enabled) *)

let init () =
  ( {
      selected = 0;
      data;
      count = List.length data;
      dims = (w, h);
      tick = 0;
      sort = Enabled Down;
    },
    Cmd.none )

(* Palette *)
let footer_bg = Ansi.Color.grayscale ~level:3

let view model =
  let w = fst model.dims in
  let columns =
    [
      Table.column ~header:(Table.cell "Service")
        ~width:(`Fixed (w / 4))
        ~justify:`Left "service";
      Table.column ~header:(Table.cell "Enabled")
        ~width:(`Fixed (w / 4))
        ~justify:`Right "running";
      Table.column ~header:(Table.cell "Running")
        ~width:(`Fixed (w / 4))
        ~justify:`Right "status";
    ]
  in
  (* let visible_count = 20 in (* adjust based on header/footer space *) *)
  let visible_count = max 1 ((snd model.dims - 6) / 2) in
  let scroll_offset =
    if model.selected < visible_count / 2 then 0
    else if model.selected > model.count - (visible_count / 2) then
      max 0 (model.count - visible_count)
    else model.selected - (visible_count / 2)
  in

  let visible_data =
    model.data
    |> List.filteri (fun i _ ->
        i >= scroll_offset && i < scroll_offset + visible_count)
  in

  let rows =
    rows_of_data (model.selected - scroll_offset) visible_data
    |> List.mapi @@ fun idx a ->
       if idx = model.selected - scroll_offset then
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
          box
            ~flex_direction:Column (* box ~flex_direction:Column ~gap:(gap 2) *)
            ~size:{ width = pct 100; height = pct 100 }
            [
              table ~columns
                ~key:(string_of_int (model.count + model.selected))
                ~rows ~flex_grow:1.0 ~table_width:w ~box_style:Minimal
                ~show_header:true ~show_edge:true
                ~show_lines:true (* ~table_padding:(1, 1, 1, 1) *)
                ~expand:true
                (* ~table_min_width:(w - (model.selected mod 2)) *)
                ();
            ];
        ];
      box ~padding:(padding 1) ~background:footer_bg ~flex_direction:Row
        ~justify_content:Space_between
        ~size:{ width = pct 100; height = auto }
        [
          text "j/k navigate • q quit • e enable • d disable • r refresh";
          text (Printf.sprintf "%d/%d" (model.selected + 1) model.count);
        ];
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Cycle_sort
          | Char c when Uchar.equal c (Uchar.of_char 'j') ->
              Some (Cycle_selected (Down, model.count))
          | Char c when Uchar.equal c (Uchar.of_char 'k') ->
              Some (Cycle_selected (Up, model.count))
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Char c when Uchar.equal c (Uchar.of_char 'r') -> Some Refresh
          | Escape -> Some Quit
          | _ -> None);
      Sub.on_resize (fun ~width ~height -> Resize (width, height));
    ]

let () = run { init; update; view; subscriptions }
