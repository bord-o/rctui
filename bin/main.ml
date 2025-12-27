open Mosaic_tea

type running = Yes | Unknown | No [@@deriving show { with_path = false }]

type service_data = { service : string; enabled : bool; running : running }
[@@deriving show { with_path = false }]

type key = service_data list [@@deriving show { with_path = false }]
type direction = Up | Down [@@deriving show { with_path = false }]
type sort_dir = Ascending | Decending [@@deriving show { with_path = false }]

type sort_by = Enabled of sort_dir | Alpha of sort_dir | Running of sort_dir
[@@deriving show { with_path = false }]

type mode = Normal | Filtering [@@deriving show { with_path = false }]

type model = {
  selected : int;
  all_data : service_data list;
  data : service_data list;
  count : int;
  dims : int * int;
  tick : int;
  sort : sort_by;
  last_error : string option;
  filter : string;
  mode : mode;
}
[@@deriving show { with_path = false }]

type msg =
  | Quit
  | Cycle_selected of direction * int
  | Cycle_sort
  | Resize of (int * int)
  | Refresh
  | Enable_service
  | Disable_service
  | Start_filter
  | Clear_filter
  | Exit_filter
  | Filter_input of string
[@@deriving show { with_path = false }]

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

let run_service_cmd s action =
  let cmd = Format.sprintf "service %s %s 2>&1" s action in
  let ic = Unix.open_process_in cmd in
  let output = In_channel.input_all ic in
  let _ = Unix.close_process_in ic in
  if
    String.length output > 0
    && (contains "Permission denied" output
       || contains "cannot" output
       || contains "error" (String.lowercase_ascii output))
  then Error output
  else Ok ()

let enable_service s = run_service_cmd s "enable"
let disable_service s = run_service_cmd s "disable"

let enabled () =
  let ic = Unix.open_process_in "service -e" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'
  |> List.map (fun s -> Filename.basename s)

let services () =
  let ic = Unix.open_process_in "service -l" in
  let output = In_channel.input_all ic in
  output |> String.trim |> String.split_on_char '\n'

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
  | Alpha Ascending -> Alpha Decending
  | Alpha Decending -> Enabled Ascending
  | Enabled Ascending -> Enabled Decending
  | Enabled Decending -> Running Ascending
  | Running Ascending -> Running Decending
  | Running Decending -> Alpha Ascending

let update msg model =
  match msg with
  | Resize dims ->
      print_endline "resize";
      ({ model with dims }, Cmd.none)
  | Cycle_selected (Up, n) ->
      let selected =
        if pred model.selected < 0 then pred n else pred model.selected
      in
      ( { model with selected; tick = model.tick + 1; last_error = None },
        Cmd.none )
  | Cycle_selected (Down, n) ->
      let selected =
        if succ model.selected >= n then 0 else succ model.selected
      in
      ( { model with selected; tick = model.tick - 1; last_error = None },
        Cmd.none )
  | Quit -> (model, Cmd.quit)
  | Refresh ->
      let all_data =
        with_status ()
        |> List.fast_sort (fun a b -> compare b.enabled a.enabled)
      in
      let data =
        if model.filter = "" then all_data
        else all_data |> List.filter (fun d -> contains model.filter d.service)
      in
      ({ model with all_data; data; count = List.length data }, Cmd.none)
  | Cycle_sort ->
      let sort = next_sort model.sort in
      let data =
        model.data
        |> List.fast_sort (fun a b ->
            match sort with
            | Enabled Ascending -> compare b.enabled a.enabled
            | Enabled Decending -> compare a.enabled b.enabled
            | Alpha Ascending -> compare b.service a.service
            | Alpha Decending -> compare a.service b.service
            | Running Ascending -> compare b.running a.running
            | Running Decending -> compare a.running b.running)
      in
      ({ model with sort; data }, Cmd.none)
  | Enable_service -> (
      let selected_service = List.nth model.data model.selected in
      if selected_service.enabled then
        ({ model with last_error = Some "Already enabled" }, Cmd.none)
      else
        match enable_service selected_service.service with
        | Ok () ->
            let data =
              model.data
              |> List.map @@ fun d ->
                 if d.service = selected_service.service then
                   { d with enabled = true; running = Unknown }
                 else d
            in
            ({ model with data }, Cmd.none)
        | Error reason ->
            ( {
                model with
                last_error = Some (Format.sprintf "Reason: %s" reason);
              },
              Cmd.none ))
  | Disable_service -> (
      let selected_service = List.nth model.data model.selected in
      if not selected_service.enabled then
        ({ model with last_error = Some "Already disabled" }, Cmd.none)
      else
        match disable_service selected_service.service with
        | Ok () ->
            let data =
              model.data
              |> List.map @@ fun d ->
                 if d.service = selected_service.service then
                   { d with enabled = false; running = No }
                 else d
            in
            ({ model with data }, Cmd.none)
        | Error reason ->
            ( {
                model with
                last_error = Some (Format.sprintf "Reason: %s" reason);
              },
              Cmd.none ))
  | Start_filter -> ({ model with mode = Filtering; filter = "" }, Cmd.none)
  | Exit_filter -> ({ model with mode = Normal }, Cmd.none)
  | Clear_filter ->
      ( {
          model with
          mode = Normal;
          filter = "";
          selected = 0;
          data = model.all_data;
          count = List.length model.all_data;
        },
        Cmd.none )
  | Filter_input s ->
      let filtered_data =
        if s = "" then model.all_data
        else model.all_data |> List.filter (fun d -> contains s d.service)
      in
      let count = List.length filtered_data in
      ( { model with filter = s; selected = 0; data = filtered_data; count },
        Cmd.none )

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

let init () =
  ( {
      selected = 0;
      all_data = [];
      data = [];
      count = 0;
      dims = (w, h);
      tick = 0;
      sort = Enabled Decending;
      last_error = None;
      mode = Normal;
      filter = "";
    },
    Cmd.perform @@ fun disp -> disp Refresh )

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
              table ~columns ~key:(show_model model) ~rows ~flex_grow:1.0
                ~table_width:w ~box_style:Minimal ~show_header:true
                ~show_edge:true
                ~show_lines:true (* ~table_padding:(1, 1, 1, 1) *)
                ~expand:true
                (* ~table_min_width:(w - (model.selected mod 2)) *)
                ();
            ];
        ];
      box ~padding:(padding 1) ~background:footer_bg ~flex_direction:Row
        ~justify_content:Space_between
        ~size:{ width = pct 100; height = auto }
        (if model.mode = Filtering then
           [ text (Format.sprintf "Filter: %s" model.filter) ]
         else
           [
             text
               "j/k navigate • s cycle sort • q quit • e enable • d disable • \
                r refresh";
             text
               (Printf.sprintf "Last Error: %s"
                  (model.last_error |> Option.value ~default:"None"));
             text
               (Printf.sprintf "%d/%d %s" (model.selected + 1) model.count
                  (model.sort |> show_sort_by));
           ]);
    ]

let subscriptions model =
  Sub.batch
    [
      Sub.on_key (fun ev ->
          match model.mode with
          | Filtering -> (
              match (Mosaic_ui.Event.Key.data ev).key with
              | Escape -> Some Clear_filter
              | Enter -> Some Exit_filter
              | Backspace ->
                  let len = String.length model.filter in
                  if len > 0 then
                    Some (Filter_input (String.sub model.filter 0 (len - 1)))
                  else Some Clear_filter
              | Char c ->
                  let ch = Uchar.to_char c in
                  if ch >= ' ' && ch <= '~' then
                    Some (Filter_input (model.filter ^ String.make 1 ch))
                  else None
              | _ -> None)
          | Normal -> (
              match (Mosaic_ui.Event.Key.data ev).key with
              | Char c when Uchar.equal c (Uchar.of_char '/') ->
                  Some Start_filter
              | Char c when Uchar.equal c (Uchar.of_char 's') -> Some Cycle_sort
              | Char c when Uchar.equal c (Uchar.of_char 'e') ->
                  Some Enable_service
              | Char c when Uchar.equal c (Uchar.of_char 'd') ->
                  Some Disable_service
              | Char c when Uchar.equal c (Uchar.of_char 'j') ->
                  Some (Cycle_selected (Down, model.count))
              | Char c when Uchar.equal c (Uchar.of_char 'k') ->
                  Some (Cycle_selected (Up, model.count))
              | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
              | Char c when Uchar.equal c (Uchar.of_char 'r') -> Some Refresh
              | Escape -> Some Clear_filter
              | _ -> None));
      Sub.on_resize (fun ~width ~height -> Resize (width, height));
    ]

let () = run { init; update; view; subscriptions }
