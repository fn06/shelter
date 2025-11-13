(* A lot of logging infrastructure for Shelter's
   various outputs *)

let pp_colored c pp fmt v = Fmt.pf fmt "%a" (Fmt.styled (`Fg c) pp) v

let pp_rolling_window ?(window = 10) ?(pp_line = Fmt.string) ?(delay = 0.02) fmt
    flow =
  let clear n = if n > 0 then Fmt.pf fmt "\027[%dA\027[J%!" n in
  let r = Eio.Buf_read.of_flow ~max_size:max_int flow in
  let window_q = Queue.create () in
  let lines = Eio.Buf_read.lines r in
  let max_length = Terminal.Size.get_columns () |> Option.get in
  let printed = ref 0 in
  Seq.iter
    (fun line ->
      Queue.push line window_q;
      if Queue.length window_q > window then ignore (Queue.pop window_q);
      clear !printed;
      printed := 0;
      Queue.iter
        (fun qline ->
          let len = Int.min (String.length qline) max_length in
          Fmt.pf fmt "%a\n%!" pp_line (String.sub qline 0 len);
          incr printed)
        window_q;
      Eio_unix.sleep delay)
    lines
