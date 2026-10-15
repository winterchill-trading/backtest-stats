open! Core
open Backtest_stats

let print_json j = print_endline (Yojson.Safe.pretty_to_string j)

let ok_or_exit = function
  | Ok x -> x
  | Error e ->
    eprintf "backtest-stats: %s\n" (Error.to_string_hum e);
    exit 1
;;

let file_arg = Command.Param.(anon ("FILE" %: string))

let samples_flag =
  Command.Param.(
    flag
      "-samples"
      (optional_with_default 1000 int)
      ~doc:"N bootstrap resamples (default 1000)")
;;

let seed_flag =
  Command.Param.(
    flag "-seed" (optional_with_default 0 int) ~doc:"N random seed (default 0)")
;;

let benchmark_flag =
  Command.Param.(
    flag
      "-benchmark-column"
      (optional string)
      ~doc:"NAME column of benchmark returns (excluded from the candidates)")
;;

(* Strategy columns: every numeric column but the benchmark. *)
let strategies (table : Table.t) ~benchmark =
  List.filter table.columns ~f:(fun (name, _) ->
    not (Option.value_map benchmark ~default:false ~f:(String.equal name)))
;;

let benchmark_values table = function
  | None -> Ok None
  | Some name -> Table.column table name |> Or_error.map ~f:Option.some
;;

let report_param ~policy_required =
  let%map_open.Command file = file_arg
  and column =
    flag
      "-column"
      (optional string)
      ~doc:
        "NAME the strategy to report (default: the best in-sample Sharpe ratio among the \
         strategy columns, which is what a selection would pick)"
  and benchmark = benchmark_flag
  and periods_per_year =
    flag
      "-periods-per-year"
      (optional float)
      ~doc:"F periods per year (default: from the time column's spacing, else 252)"
  and oos_periods =
    flag "-oos-periods" (optional int) ~doc:"N the last N periods are out of sample"
  and oos_fraction =
    flag
      "-oos-fraction"
      (optional float)
      ~doc:"F the last fraction F of the periods is out of sample"
  and trials =
    flag
      "-trials"
      (optional int)
      ~doc:"N configurations tried (overrides the column count)"
  and trials_variance =
    flag
      "-trials-sharpe-variance"
      (optional float)
      ~doc:"V variance of the trials' per-period Sharpe ratios (with -trials)"
  and policy =
    match policy_required with
    | true ->
      flag "-policy" (required string) ~doc:"FILE acceptance policy (sexp)"
      >>| Option.some
    | false -> flag "-policy" (optional string) ~doc:"FILE acceptance policy (sexp)"
  and pbo_splits = flag "-pbo-splits" (optional int) ~doc:"S blocks for the PBO (even)"
  and samples = samples_flag
  and seed = seed_flag in
  fun () ->
    let open Or_error.Let_syntax in
    let%bind table = Table.load file in
    let%bind bench = benchmark_values table benchmark in
    let candidates = strategies table ~benchmark in
    let%bind () =
      if List.is_empty candidates
      then Or_error.error_s [%message "no strategy columns"]
      else Ok ()
    in
    let n = Array.length (snd (List.hd_exn candidates)) in
    let out_of_sample_periods =
      match oos_periods, oos_fraction with
      | Some k, _ -> k
      | None, Some f -> Float.iround_down_exn (f *. Float.of_int n)
      | None, None -> 0
    in
    let%bind name =
      match column with
      | Some c -> Ok c
      | None ->
        let in_sample = n - out_of_sample_periods in
        if in_sample < 2
        then Or_error.error_s [%message "in-sample part too short to select a column"]
        else
          Ok
            (List.max_elt candidates ~compare:(fun (_, a) (_, b) ->
               Float.compare
                 (Metrics.sharpe (Array.sub a ~pos:0 ~len:in_sample))
                 (Metrics.sharpe (Array.sub b ~pos:0 ~len:in_sample)))
             |> Option.value_exn
             |> fst)
    in
    let%bind returns = Table.column table name in
    let%bind series = Table.series table returns in
    let periods_per_year =
      match periods_per_year, table.times with
      | Some p, _ -> p
      | None, Some _ when n >= 2 -> Series.infer_periods_per_year series
      | None, _ -> 252.
    in
    let%bind policy =
      match policy with
      | None -> Ok None
      | Some path -> Gate.Policy.load path |> Or_error.map ~f:Option.some
    in
    let trials =
      match trials with
      | None -> None
      | Some count ->
        Some
          { Report.Trials.count
          ; sharpe_variance =
              Option.value
                trials_variance
                ~default:
                  (Metrics.trials_sharpe_variance (Array.of_list_map candidates ~f:snd))
          }
    in
    let candidates =
      if List.length candidates >= 2
      then Some (Array.of_list_map candidates ~f:snd)
      else None
    in
    Report.create
      ~name
      ~out_of_sample_periods
      ?trials
      ?candidates
      ?benchmark:bench
      ?pbo_splits
      ~bootstrap_samples:samples
      ~seed
      ?policy
      ~periods_per_year
      series
;;

let report_command =
  Command.basic
    ~summary:"Every statistic for one strategy, as JSON"
    ~readme:(fun () ->
      "FILE is CSV or Parquet: an optional time column and one column of per-period \
       returns per strategy. With two or more strategy columns the report includes the \
       PBO, the reality check and SPA p-values and the deflated Sharpe ratio over them.")
    (let%map.Command run = report_param ~policy_required:false in
     fun () -> run () |> ok_or_exit |> Report.to_json |> print_json)
;;

let gate_command =
  Command.basic
    ~summary:"Report plus verdict; exit status 2 when the policy rejects"
    (let%map.Command run = report_param ~policy_required:true in
     fun () ->
       let r = run () |> ok_or_exit in
       print_json (Report.to_json r);
       match r.verdict with
       | Some Accept | None -> ()
       | Some (Reject _) -> exit 2)
;;

let scheme_param =
  let%map_open.Command block =
    flag
      "-block-length"
      (optional float)
      ~doc:
        "L mean block length of the stationary bootstrap (default: cube root of the \
         length)"
  in
  Option.map block ~f:(fun l -> Bootstrap.Scheme.Stationary { mean_block_length = l })
;;

let spa_command =
  Command.basic
    ~summary:"Reality check and SPA test: does the best strategy beat the benchmark?"
    (let%map_open.Command file = file_arg
     and benchmark = benchmark_flag
     and samples = samples_flag
     and seed = seed_flag
     and scheme = scheme_param in
     fun () ->
       (let open Or_error.Let_syntax in
        let%bind table = Table.load file in
        let%bind bench = benchmark_values table benchmark in
        let cols = strategies table ~benchmark in
        let rows = Array.of_list_map cols ~f:snd in
        let rows =
          match bench with
          | None -> rows
          | Some b -> Multiple_testing.excess_over ~benchmark:b rows
        in
        let%map rc, spa =
          Or_error.try_with (fun () ->
            let rng = Random.State.make [| seed |] in
            let rc = Multiple_testing.Reality_check.run ?scheme ~samples ~rng rows in
            let spa = Multiple_testing.Spa.run ?scheme ~samples ~rng rows in
            rc, spa)
        in
        let names = List.map cols ~f:fst in
        `Assoc
          [ "strategies", `List (List.map names ~f:(fun s -> `String s))
          ; "best", `String (List.nth_exn names spa.best)
          ; "reality_check", Json.reality_check rc
          ; "spa", Json.spa spa
          ])
       |> ok_or_exit
       |> print_json)
;;

let pbo_command =
  Command.basic
    ~summary:
      "Probability of backtest overfitting by combinatorially symmetric cross-validation"
    (let%map_open.Command file = file_arg
     and splits =
       flag "-splits" (optional_with_default 16 int) ~doc:"S blocks, even (default 16)"
     and metric =
       flag
         "-metric"
         (optional_with_default
            Multiple_testing.Pbo.Metric.Sharpe
            (Arg_type.create (fun s ->
               Multiple_testing.Pbo.Metric.t_of_sexp
                 (Sexp.of_string (String.capitalize s)))))
         ~doc:"sharpe|mean performance measure (default sharpe)"
     and benchmark = benchmark_flag in
     fun () ->
       (let open Or_error.Let_syntax in
        let%bind table = Table.load file in
        let rows = strategies table ~benchmark |> Array.of_list_map ~f:snd in
        let%map r =
          Or_error.try_with (fun () -> Multiple_testing.Pbo.run ~splits ~metric rows)
        in
        Json.pbo r)
       |> ok_or_exit
       |> print_json)
;;

let var_command =
  Command.basic
    ~summary:"Kupiec and Christoffersen VaR backtests and the traffic-light zone"
    (let%map_open.Command file = file_arg
     and rate =
       flag "-rate" (required float) ~doc:"P expected exception rate (0.01 for a 99% VaR)"
     and hits_column =
       flag "-hits-column" (optional string) ~doc:"NAME column of 0/1 exceptions"
     and returns_column =
       flag "-returns-column" (optional string) ~doc:"NAME column of realized returns"
     and var_column =
       flag
         "-var-column"
         (optional string)
         ~doc:"NAME column of VaR forecasts (positive losses)"
     and exact_limit =
       flag
         "-exact-limit"
         (optional int)
         ~doc:"N largest sample for exact Christoffersen p-values (default 500)"
     in
     fun () ->
       (let open Or_error.Let_syntax in
        let%bind table = Table.load file in
        let%bind hits =
          match hits_column, returns_column, var_column with
          | Some h, None, None ->
            Table.column table h
            |> Or_error.map ~f:(Array.map ~f:(fun x -> Float.( <> ) x 0.))
          | None, Some r, Some v ->
            let%bind returns = Table.column table r in
            let%map var = Table.column table v in
            Var_backtest.hits ~returns ~var
          | _ ->
            Or_error.error_s
              [%message "give -hits-column, or -returns-column with -var-column"]
        in
        let%map r =
          Or_error.try_with (fun () ->
            Var_backtest.run ?exact_limit ~expected_rate:rate hits)
        in
        Json.var_backtest r)
       |> ok_or_exit
       |> print_json)
;;

let adjust_command =
  Command.basic
    ~summary:"Bonferroni, Holm and Benjamini-Hochberg adjusted p-values"
    (let%map_open.Command file = file_arg
     and column =
       flag
         "-column"
         (optional_with_default "p_value" string)
         ~doc:"NAME p-values (default p_value)"
     in
     fun () ->
       (let open Or_error.Let_syntax in
        let%bind table = Table.load file in
        let%bind ps = Table.column table column in
        let%map adjusted =
          Or_error.try_with (fun () ->
            [ "bonferroni", Multiple_testing.Adjust.bonferroni ps
            ; "holm", Multiple_testing.Adjust.holm ps
            ; "benjamini_hochberg", Multiple_testing.Adjust.benjamini_hochberg ps
            ])
        in
        let arr a = `List (Array.to_list a |> List.map ~f:Json.float) in
        `Assoc (("p_value", arr ps) :: List.map adjusted ~f:(fun (k, a) -> k, arr a)))
       |> ok_or_exit
       |> print_json)
;;

let () =
  Command_unix.run
    (Command.group
       ~summary:"Backtest statistics over return series in CSV or Parquet files"
       [ "report", report_command
       ; "gate", gate_command
       ; "spa", spa_command
       ; "pbo", pbo_command
       ; "var", var_command
       ; "adjust", adjust_command
       ])
;;
