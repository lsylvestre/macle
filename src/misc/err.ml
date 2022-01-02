type loc = Lexing.position * Lexing.position

let pp_loc fmt (Lexing.{pos_lnum=l1;pos_cnum=c1},
                       Lexing.{pos_lnum=l2;pos_cnum=c2}) =
  if l1 = l2
  then Format.fprintf fmt "line %d, characters %d-%d" l1 c1 c2
  else Format.fprintf fmt "from line %d, characters %d, to line %d characters %d" l1 c1 l2 c2

type 'a located = 'a * loc
let mk_loc loc a = (a,loc)

let error x loc pp_msg =
  let open Format in
  fprintf err_formatter
    "in circuit %s [%a]\nError: %a" x pp_loc loc pp_msg ();
  exit 0

let syntax_error ?msg loc =
  let open Format in
  (match msg with
  | None ->
      fprintf err_formatter "%a\nsyntax error" pp_loc loc
  | Some s ->
      fprintf err_formatter "%a\nsyntax error: %s" pp_loc loc s);
  exit 0
