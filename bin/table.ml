open! Core

type t =
  { times : Time_ns.t array option
  ; columns : (string * float array) list
  }

let time_names = [ "time"; "timestamp"; "date"; "ts" ]
let is_time_name name = List.mem time_names (String.lowercase name) ~equal:String.equal

let time_of_int i =
  let a = Int.abs i in
  (* the unit follows from the magnitude: seconds until about 5138, then ms, us, ns *)
  let ns =
    if a < 100_000_000_000
    then i * 1_000_000_000
    else if a < 100_000_000_000_000
    then i * 1_000_000
    else if a < 100_000_000_000_000_000
    then i * 1_000
    else i
  in
  Time_ns.of_int_ns_since_epoch ns
;;

let time_of_string s =
  let s = String.strip s in
  match Int.of_string_opt s with
  | Some i -> Ok (time_of_int i)
  | None ->
    (match Date.of_string s with
     | d ->
       Ok (Time_ns.of_date_ofday ~zone:Time_float.Zone.utc d Time_ns.Ofday.start_of_day)
     | exception _ ->
       (match Time_ns.of_string_with_utc_offset s with
        | t -> Ok t
        | exception _ ->
          (match Time_ns.of_string_with_utc_offset (s ^ "Z") with
           | t -> Ok t
           | exception _ -> Or_error.error_s [%message "cannot parse time" (s : string)])))
;;

let split_csv_line line = String.split line ~on:',' |> List.map ~f:String.strip

let of_csv path =
  let lines =
    In_channel.read_lines path
    |> List.filter ~f:(fun l -> not (String.is_empty (String.strip l)))
  in
  match lines with
  | [] -> Or_error.error_s [%message "empty CSV file" (path : string)]
  | header :: rows ->
    let names = split_csv_line header |> Array.of_list in
    let width = Array.length names in
    let rows = List.map rows ~f:(fun l -> Array.of_list (split_csv_line l)) in
    let%bind.Or_error () =
      match List.findi rows ~f:(fun _ r -> Array.length r <> width) with
      | Some (i, _) ->
        Or_error.error_s
          [%message "CSV row has the wrong number of fields" ~line:(i + 2 : int)]
      | None -> Ok ()
    in
    let rows = Array.of_list rows in
    let time_index =
      Array.findi names ~f:(fun _ n -> is_time_name n) |> Option.map ~f:fst
    in
    let%bind.Or_error times =
      match time_index with
      | None -> Ok None
      | Some j ->
        Array.to_list rows
        |> List.map ~f:(fun r -> time_of_string r.(j))
        |> Or_error.all
        |> Or_error.map ~f:(fun l -> Some (Array.of_list l))
    in
    let%map.Or_error columns =
      Array.to_list names
      |> List.filter_mapi ~f:(fun j name ->
        if [%equal: int option] (Some j) time_index
        then None
        else
          Some
            (Array.to_list rows
             |> List.mapi ~f:(fun i r ->
               match Float.of_string_opt r.(j) with
               | Some x -> Ok x
               | None ->
                 Or_error.error_s
                   [%message "not a number" (name : string) ~line:(i + 2 : int) r.(j)])
             |> Or_error.all
             |> Or_error.map ~f:(fun l -> name, Array.of_list l)))
      |> Or_error.all
    in
    { times; columns }
;;

let of_parquet path =
  Parquet.Reader.with_file path ~f:(fun r ->
    let schema = Parquet.Reader.columns r in
    let names =
      List.map schema ~f:(fun (c : Parquet.Metadata.Schema_element.t) -> c.name)
    in
    let%bind.Or_error rows =
      Parquet.Reader.fold_rows r ~init:[] ~f:(fun acc row -> row :: acc)
      |> Or_error.map ~f:(fun l -> Array.of_list (List.rev l))
    in
    let value_to_float name (v : Parquet.Reader.Value.t) =
      match v with
      | Float x -> Ok x
      | Int i -> Ok (Float.of_int i)
      | Null | Bool _ | String _ ->
        Or_error.error_s
          [%message "not a number" (name : string) (v : Parquet.Reader.Value.t)]
    in
    (* An integer time column's unit comes from its logical (or legacy converted) type;
       without one, from the size of the numbers. *)
    let int_to_time (c : Parquet.Metadata.Schema_element.t) i =
      let ns k = Time_ns.of_int_ns_since_epoch (i * k) in
      match c.logical_type, c.converted_type with
      | Some Date, _ | _, Some Date -> ns 86_400_000_000_000
      | Some (Timestamp { unit = Millis; _ }), _ | _, Some Timestamp_millis ->
        ns 1_000_000
      | Some (Timestamp { unit = Micros; _ }), _ | _, Some Timestamp_micros -> ns 1_000
      | Some (Timestamp { unit = Nanos; _ }), _ -> ns 1
      | _ -> time_of_int i
    in
    let value_to_time c (v : Parquet.Reader.Value.t) =
      match v with
      | Int i -> Ok (int_to_time c i)
      | String s -> time_of_string s
      | Null | Bool _ | Float _ ->
        Or_error.error_s [%message "not a time" (v : Parquet.Reader.Value.t)]
    in
    let indexed = List.mapi names ~f:(fun j n -> j, n) in
    let time_col = List.find indexed ~f:(fun (_, n) -> is_time_name n) in
    let%bind.Or_error times =
      match time_col with
      | None -> Ok None
      | Some (j, _) ->
        Array.to_list rows
        |> List.map ~f:(fun row -> value_to_time (List.nth_exn schema j) row.(j))
        |> Or_error.all
        |> Or_error.map ~f:(fun l -> Some (Array.of_list l))
    in
    let%map.Or_error columns =
      List.filter indexed ~f:(fun (j, _) ->
        Option.value_map time_col ~default:true ~f:(fun (tj, _) -> tj <> j))
      |> List.map ~f:(fun (j, name) ->
        Array.to_list rows
        |> List.map ~f:(fun row -> value_to_float name row.(j))
        |> Or_error.all
        |> Or_error.map ~f:(fun l -> name, Array.of_list l))
      |> Or_error.all
    in
    { times; columns })
;;

let load path =
  Or_error.try_with_join (fun () ->
    if String.is_suffix (String.lowercase path) ~suffix:".parquet"
    then of_parquet path
    else of_csv path)
  |> Or_error.tag ~tag:path
;;

let column t name =
  match List.Assoc.find t.columns name ~equal:String.equal with
  | Some c -> Ok c
  | None ->
    Or_error.error_s
      [%message
        "no such column"
          (name : string)
          ~available:(List.map t.columns ~f:fst : string list)]
;;

let series t returns =
  match t.times with
  | Some times -> Backtest_stats.Series.create ~times ~returns
  | None -> Or_error.try_with (fun () -> Backtest_stats.Series.of_returns returns)
;;
