open Batteries
let (|?) = Option.(|?)
let (/@) a b = List.map b a
let (//@) a b = List.filter_map b a

type 'b unparsed = int * 'b list (* position so far, list of things left to parse *)

type 'b error = string, 'b unparsed (* reason for the failure, what was left to parse *)

let no_error = "", (0, [])

let combine_errors (_, (p1, _) as e1) (_, (p2, _) as e2) =
  (* we merely keep the best one *)
  if p1 >= p2 then e1 else e2

(* NOTE: naming 'a*'b unparsed would helop! *)
type ('a, 'b) parzer_result = ('a * 'b unparsed) list * 'b error
type ('a, 'b) parzer = 'b unparsed (* tokens to add *) -> ('a, 'b) parzer_result (* possible results *)

let print_abbrev max_len print_tok fmt lst =
  let lst_ab, ellipsis =
    if List.length lst > max_len then
      List.sub max_len lst, "..."
    else
      lst, "" in
  Printf.fprintf fmt "%a%s" (List.print print_tok lst_ab) ellipsis

let parzer_result_printer print_res print_tok = function
  | [], (reason, (pos, rest)) ->
    let where =
      match rest with
        []     -> "end of input"
      | Some s -> Printf.sprintf "pos %d, before %a"
                    pos ((print_abbrev 8 print_tok) rest) in
    Printf.fprintf fmt "%s at %s" reason rest
  | lst, _ ->
    let single_result_printer fmt = function
      | res, (_, []) -> print_res fmt res
      | res, (n, rest) ->
        Printf.fprintf fmt "%a until %d, rest %a"
          print_res res n (List.print print_tok) rest in
    List.print single_result_printer lst

let is_error = function
  | [], _ -> true
  | _     -> false

(* unconditionally returns a result - useful for bind *)
let return res unp = [ res, unp ], ("", unp)
(*$= return & ~printer:(IO.to_string (parzer_result_printer String.print Int.print))
  ["glop", (0, [1; 2]), ("", (0, [1;2]))] (return "glop" (0, [ 1; 2 ]))
 *)

(* fails unconditionally *)
let fail reason unp = [], (reason, unp)

(* Matches the end of input *)
let eof = function
  | _pos, [] as unp -> [ (), unp ], ("", unp)
  | unp -> fail "EOF expected" unp
(*$= eof & ~printer:(IO.to_string (parzer_result_printer Unit.print Int.print))
  [] (eof (0,['X']))
  [(),(0,[])] (eof (0,[]))
*)

(* Accept 'nothing' *)
let nothing unp = return () unp

(* Fail like p or consume nothing if p match *)
let check p unp =
  match p unp with
  | [], _ e -> e
  | _ -> nothing unp
(*$T check
  not @@ is_error (check eof (0,[]))
  is_error        (check eof (0,['X']))
  is_error        (check (item 2) (0,[1;2]))
  not @@ is_error (check (item 1) (0,[1;2]))
*)

(* invert the result of p - useful with check *)
let no reason p unp =
  if is_error (p unp) then nothing unp else fail reason unp
(*$T no
  is_error        (check (no "no 1" (item 1)) (0,[1;2]))
  not @@ is_error (check (no "no 1" (item 1)) (0,[0;2]))
*)

let upto print_tok delim_orig unp =
  let rec aux past_delim delim past_bs unp =
    match delim, unp with
    | [], _ -> return (List.rev past_bs) unp
    | d::_, (_pos, []) -> fail (Printf.sprintf "Expecting %a" print_tok d) unp
    | d::delim', (pos, (b::bs' as bs)) ->
      if d = b then aux (d::past_delim) delim' (b::past_bs) (pos+1, bs') else
      (match past_delim with
      | [] ->
        aux [] delim_orig (b::past_bs) (pos+1, bs')
      | [_] -> (* stay in place *)
        aux [] delim_orig past_bs unp
      | _::p' -> (* rollback all chars but one *)
        let rb = List.length p' in
        let rbs, past_bs' = List.split_at rb past_bs in
        aux [] delim_orig past_bs' (pos-rb, List.rev_append rbs bs)) in
  aux [] delim_orig [] unp
(*$= upto & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  ([], ("Expecting 0 at end of input", (1, []))) (upto Int.print [0;0] (0,[0]))
  ([], ("Expecting 0 at end of input", (0, []))) (upto Int.print [0;0] (0,[]))
  ([], ("Expecting 0 at end of input", (2, []))) (upto Int.print [0;0] (0,[1;0]))
  ([], ("Expecting 0 at pos 0 before '1'"))      (upto Int.print [0;0] (0,[1]))
  ([[0;0], (2,[])], ("", (2,[])))                (upto Int.print [0;0] (0,[0;0]))
  ([[1;2;3;0;0], (5,[])], ("", (5,[])))          (upto Int.print [0;0] (0,[1;2;3;0;0]))
  ([[1;2;3;0;0], (5,[4;5])], ("", (5,[4;5])))    (upto Int.print [0;0] (0,[1;2;3;0;0;4;5]))
  ([[1;2;0;3;0;0], (6,[4;5])], ("", (6,[4;5])))  (upto Int.print [0;0] (0,[1;2;0;3;0;0;4;5]))
 *)

let cond reason c = function
  | pos, b::bs when c b -> return b (pos+1, bs)
  | unp -> fail reason unp

let item print_tok i = cond (Printf.sprintf "Expecting %a" print_tok i) ((=) i)
(*$= item & ~printer:(IO.to_string (parzer_result_printer Int.print Int.print))
  [1, (1,[1;2;2])] (item Int.print 1 (0,[1;1;2;2]))
 *)

let range print_tok mi ma =
  let reason = Printf.sprintf "Expecting %a..%a" print_tok mi print_tok_ma in
  cond reason (fun c -> c >= mi && c <= ma)
(*$= range & ~printer:(IO.to_string (parzer_result_printer Int.print Int.print))
  [1, (1,[2;3])] (range Int.print 1 5 (0,[1;2;3]))
  [5, (1,[2;3])] (range Int.print 1 5 (0,[5;2;3]))
  [] (range 1 5 (0,[6;2;3]))
 *)

let take n (pos, bs as unp) =
  try let res, rem = List.split_at n bs in
    return res (pos+n, rem)
  with Invalid_argument _ ->
    fail (Printf.sprintf "Expecting %d more tokens" n) unp
(*$= take & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;2], (2,[3;4;5])] (take 2 (0,[1;2;3;4;5]))
  [[1;2;3], (3,[])] (take 3 (0,[1;2;3]))
  [] (take 3 (0,[1;2]))
*)

(* equivalent to [take 1 >>: List.hd], only faster *)
let one = function
  | _pos, [] as unp -> fail "Expecting more" unp
  | pos, b::bs      -> return b (pos+1, bs)

(* Change value returned by p through f *)
let map_res p f unp =
  let lst, err = p unp in
  List.map (fun (res, rem) -> (f res, rem)) lst, err

let (>>:) = map_res

(* whenever [p] returns a result, replace this result with [x] *)
let replace p x = p >>: fun _ -> x

let (>>>) = replace

(* Filter_map every results returned by p through f *)
type 'a mapf_result = Ok of 'a | Fail of 'b error | FailSilent
let map_filter p f unp =
  let lst, err = p unp in
  let lst, err = List.fold_left (fun (lst, err as same) (res, rem) ->
    match f (res, rem) with
    | Ok x       -> x::lst, err
    | Fail err'  -> lst, combine_errors err err'
    | FailSilent -> same) ([], err) lst in
  List.rev lst, err

let (>>?) = map_filter

let some p = p >>: (fun (res, rem) -> Some res, rem)
let none p = p >>: (fun (_, rem) -> None, rem)
let ign p = p >>: ignore

(* combinators *)

(* sequence of 2 parsers *)
let cons p1 p2 unp =
  (* for each result of p1 unp, continue parsing the rest with p2 and return the product of all results *)
  let lst, err = p1 unp in
  let lst, err = List.fold_left (fun (lst, err as same) (res1, rem1) ->
    let lst2, err2 = p2 rem1 in
    let lst2 = List.fold_left (fun lst (res2, rem2) -> ((res1, res2), rem2)::lst) lst in
    lst2, combine_errors err err2) ([], err) lst in
  List.rev lst, err

(* Same but keeps only the first or second value *)
let cons1 p1 p2 = (cons p1 p2) >>: fst
let cons2 p1 p2 = (cons p1 p2) >>: snd

let (++) = cons
let (+-) = cons1
let (-+) = cons2
let (--) p1 p2 = p1 ++ p2 >>: ignore

let (|~) p1 p2 unp =
  (* Take care oftentimes alternatives are exclusives *)
  let r1, e1 = p1 unp and r2, e2 = p2 unp in
  List.rev_append r1 r2, combine_errors e1 e2

(* Record [msg] as an error message if parser [p] fails. Useful to
 * overwrite the generic error messages with something more specific. *)
let with_err msg p unp =
  match p unp with
  | [], _ -> fail msg
  | x -> x

let optional p =
  some p |~ return None
(*$= optional & ~printer:(IO.to_string (parzer_result_printer (Option.print Int.print) Int.print))
  [None,(0,[1;2]); Some 1,(1,[2])] (optional (item 1) (0,[1;2]) |> List.sort compare)
*)

let optional_greedy p =
  some p |~ none (check (no p))
(*$= optional_greedy & ~printer:(IO.to_string (parzer_result_printer (Option.print Int.print) Int.print))
  [Some 1,(1,[2])] (optional_greedy (item 1) (0,[1;2]) |> List.sort compare)
  [None,(0,[1;2])] (optional_greedy (item 3) (0,[1;2]) |> List.sort compare)
*)

let optional_default def p =
  p |~ return def

let rec repeat ?sep ?min ?max p unp =
  let pred_opt = Option.map pred in
  if max = Some 0 then (
    match min with
    | None | Some 0 -> return [] unp
    | Some mi -> fail (Printf.sprintf "Expecting %d more tokens" mi) (* TODO: instead of "token" pass a name *)
  ) else (
    match sep with
    | Some sep ->
      (match min with
      | Some mi when mi > 1 ->
        (p +- sep ++ repeat ~sep ?min:(pred_opt min) ?max:(pred_opt max) p >>: fun (v,vs) -> v::vs) unp
      | Some mi when mi == 1 ->
        (p ++ optional (sep -+ repeat ~sep ~min:1 ?max:(pred_opt max) p) >>: fun (v,vs_o) -> v::(Option.default [] vs_o)) unp
      | _ ->
        (optional (repeat ~sep ~min:1 ?max p) >>: Option.default []) unp)
    | None ->
      (match min with
      | Some mi when mi >= 1 ->
        (p ++ repeat ?min:(pred_opt min) ?max:(pred_opt max) p >>: fun (v,vs) -> v::vs) unp
      | _ ->
        (optional (repeat ~min:1 ?max p) >>: Option.default []) unp)
  )
(*$= repeat & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[],(0,[3;1;2])] (repeat (item 1) (0,[3;1;2]) |> List.sort compare)
  [[],(0,[1;1;1;2]); [1],(1,[1;1;2]); [1;1],(2,[1;2]); [1;1;1],(3,[2])] (repeat (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[],(0,[1;1;1;2]); [1],(1,[1;1;2])] (repeat ~max:1 (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[],(0,[1;1;1;2]); [1],(1,[1;1;2]); [1;1],(2,[1;2])] (repeat ~max:2 (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[],(0,[1;1;1;2]); [1],(1,[1;1;2]); [1;1],(2,[1;2]); [1;1;1],(3,[2])] (repeat ~max:3 (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [] (repeat ~min:2 ~max:4 (item 1) (0,[]) |> List.sort compare)
  [] (repeat ~min:2 ~max:4 (item 1) (0,[1]) |> List.sort compare)
  [[1;1],(2,[])] (repeat ~min:2 ~max:4 (item 1) (0,[1;1]) |> List.sort compare)
  [[1;1],(2,[2])] (repeat ~min:2 ~max:4 (item 1) (0,[1;1;2]) |> List.sort compare)
  [[1;1],(2,[1;2]); [1;1;1],(3,[2])] (repeat ~min:2 ~max:4 (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[1;1],(2,[1;1;2]); [1;1;1],(3,[1;2]); [1;1;1;1],(4,[2])] (repeat ~min:2 ~max:4 (item 1) (0,[1;1;1;1;2]) |> List.sort compare)
  [[1;1],(2,[1;1;1;2]); [1;1;1],(3,[1;1;2]); [1;1;1;1],(4,[1;2])] (repeat ~min:2 ~max:4 (item 1) (0,[1;1;1;1;1;2]) |> List.sort compare)
  [[1],(1,[0;1;0;1;2]); [1;1],(3,[0;1;2])] (repeat ~min:1 ~max:2 ~sep:(item 0) (item 1) (0,[1;0;1;0;1;2]) |> List.sort compare)
*)

let several ?sep p unp = repeat ?sep ~min:1 p unp
(*$= several & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1],(1,[1;1;2]); [1;1],(2,[1;2]); [1;1;1],(3,[2])] (several (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [] (several (item 1) (0,[3;2;1]) |> List.sort compare)
*)

let any ?sep p unp = repeat ?sep p unp
(*$= any & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[],(0,[1;1;2]); [1],(1,[1;2]); [1;1],(2,[2])] (any (item 1) (0,[1;1;2]) |> List.sort compare)
  [[],(0,[3;2;1])] (any (item 1) (0,[3;2;1]) |> List.sort compare)
*)

let rec several_greedy ?sep p =
  p ++ optional_greedy (several_greedy (sep -+ p)) >>: function
    | r, None -> r
    | r, Some rs -> r::rs
(*$= several_greedy & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;1;1],(3,[2])] (several_greedy (item 1) (0,[1;1;1;2]))
  [] (several_greedy (item 1) (0,[3;2;1]))
  [[1;1;1],(5,[2])] (several_greedy ~sep:(item 0) (item 1) (0,[1;0;1;0;1;2]))
  [[1;1;1],(5,[0;2])] (several_greedy ~sep:(item 0) (item 1) (0,[1;0;1;0;1;0;2]))
*)

(* same as several_greedy but allow for no match *)
let all ?sep p unp =
  match several_greedy ?sep p unp with
  | [], _ -> return [] unp (* from no result to a single result which is an empty list *)
  | l -> l
(*$= all & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;1;1],(3,[2])] (all (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[],(0,[2])] (all (item 1) (0,[2]) |> List.sort compare)
*)

let times ?sep n = repeat ?sep ~min:n ~max:n
(*$= times & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [ [1;1],(2,[1;3]) ] (times 2 (item 1) (0,[1;1;1;3]))
  [ [],(0,[1;1;1;3]) ] (times 0 (item 1) (0,[1;1;1;3]))
*)

(* Run the results of p through f, that will return a new parser.
   Once this new parser got it's result, give new items to first parser and so on *)
let bind p f unp =
  let lst, err = p unp in
  List.fold_left (fun (lst, err) (res, rem) ->
      let p' = f res in
      let lst', err' = p' rem in
      lst @ lst', combine_errors err err')
    err lst
(*$T bind
  let positive = cond (fun n -> n >= 0) in \
  let p = bind positive (fun i -> \
    assert (i >= 0) ; \
    (* match a sequence of i zeros *) \
    times i (item 0)) in \
  p (0,[1;0;5]) = [[0],(2,[5])] && \
  p (0,[1;0;0]) = [[0],(2,[0])] && \
  p (0,[3;0;0;0;5]) = [[0;0;0],(4,[5])] && \
  p (0,[3;0;0;5]) = []
*)

let (>>=) = bind (* as usual *)

(* Use the results of the first parser as the input elements of the second.
   Return the first result of p2.
   To help reasoning, let's consider we want to parse a mail header, with p1 reading
   chars and outputting lines (maybe taking care of wrapped lines), and p2 consuming those lines and outputting field,value
   pairs. Notice p1 result must be a list of something that will be tokens read by p2. So in our
   example p1 would output a line in the form of a list of chars or words, with p2 parsing chars or words. *)
let pipe (p1 : ('a,'b) parzer)
         (p2 : ('c,'a) parzer)
         (unp : 'b unparsed) : ('c,'b) parzer_result =
  (* Here we deal with two parsers acting at different depth and it's
   * doubtful how to combine errors here. Let's forget about this for now. *)
  let lst1, err1 = p1 unp in
  if lst1 = [] then [], err1 else
  (* Remember res is all the possible result of applying p1
  once. In our example, it would be a singleton with the first line (since
  there is only one way to parse a single line). This is not the list of all lines
  (unless p is .< all line >. instead of just .< line >., of course).
  So now we want to pass this result (all possible results actually) to p2 and
  see what it gives, unless p1 yield no result at all in which case we return the error.
  If p2 yield a result, then that's our result. If it returns an error for some possible
  result of p1, we just combine the error with the ones we already had. *)
  List.fold_left (fun (lst, err) (res1, unp1) ->
      let lst', err' = p2 (0, res1) in  (* notice that err will be computed with a starting position of 0 (in res1), so not easy to combine with err1 (combined with starting position on the outer layer stream) *)
      (* Now, lst' is a list of (result, unparsed tokens).
       * We do not care about the unparsed tokens (if called cared he would have used a parsed
       * that check end of input for p2 (in our example, p2 would check end of line).
       * We cannot keep those because we must return to the "outer" layer, in our
       * example we must go back to reading chars to form new lines. We must return the
       * result of p2 but the unparsed stream of p1: *)
      let lst' = List.map (fun (res', _unp') -> res', unp1) lst' in
      lst @ lst', combine_errors err err')  (* FIXME: this list concat can be made faster *)
    ([], ("", (0, res1))) lst2
(*$T pipe
  let p = pipe (upto [0]) (item [2;1;0]) in \
  p (0,[2;1;0;5;0;42]) = [[2;1;0],(3,[5;0;42])] && \
  several p (0,[2;1;0;5;0;42]) = [[[2;1;0]],(3,[5;0;42])] && \
  p (0,[5;0;42]) = []
 *)

(* Try all of the given parsers in turn *)
let rec either ?msg = function
  | [] -> fail ?msg
  | p::p' -> p |~ either ?msg p'


(* Various useful parsers, mostly on chars *)

let char ?(case_sensitive=true) c =
  if case_sensitive then item c
  else item (Char.uppercase c) |~ item (Char.lowercase c)

(* Special seq when you wait for a list of chars: *)
let string ?case_sensitive s =
  let strlen = String.length s in
  assert (strlen > 0) ;
  let rec loop i strlen s =
    if i = strlen - 1 then (* last char *) (
      ign (char ?case_sensitive s.[i])
    ) else (
      ign (char ?case_sensitive s.[i] ++ loop (succ i) strlen s)
    ) in
  loop 0 strlen s >>: fun () -> s

let alphabetic = cond Char.is_letter
let numeric = cond Char.is_digit
let alphanum = alphabetic |~ numeric
let uppercase = range 'A' 'Z'
let lowercase = range 'a' 'z'

let crlf = ign (item '\r' ++ item '\n')

let c2i c =
  if c >= '0' && c <= '9' then
    int_of_char c - int_of_char '0'
  else if c >= 'a' && c <= 'z' then
    int_of_char c - int_of_char 'a' + 10
  else if c >= 'A' && c <= 'Z' then
    int_of_char c - int_of_char 'A' + 10
  else
    invalid_arg (Printf.sprintf "c2i: Cannot convert char '%c' to int" c)

(*$= c2i & ~printer:String.of_int
  5 (c2i '5')
  15 (c2i 'f')
  15 (c2i 'F')
*)

let i2c i =
  if i >= 0 && i <= 9 then char_of_int (int_of_char '0' + i)
  else if i >= 10 && i <= 35 then char_of_int (int_of_char 'a' + i - 10)
  else
    invalid_arg (Printf.sprintf "i2c: Cannot convert int '%d' to digit" i)
(*$= i2c & ~printer:String.of_char
  '5' (i2c 5)
  'f' (i2c 15)
*)

let digit base =
  cond ~errmsg:"Number expected" (fun c ->
    (c >= '0' && c < (char_of_int (int_of_char '0' + min 10 base))) ||
    (base > 10 && (
      (c >= 'a' && c < (char_of_int (int_of_char 'a' + (base - 10)))) ||
      (c >= 'A' && c < (char_of_int (int_of_char 'A' + (base - 10))))
    ))) >>: c2i
(*$= digit & ~printer:(IO.to_string (parzer_result_printer Int.print Int.print))
  [0,(1,[])] ((digit  2 +- eof) (0,['0']))
  [1,(1,[])] ((digit  2 +- eof) (0,['1']))
  [] ((digit  2 +- eof) (0,['2']))
  [10,(1,[])] ((digit 16 +- eof) (0,['a']))
  [15,(1,[])] ((digit 16 +- eof) (0,['F']))
*)

module Number (N : BatNumber.NUMERIC_BASE) =
struct
  let num base =
    let base' = N.of_int base in
    several (digit base) >>: fun ds ->
      let rec aux n = function
        | [] -> n
        | d :: d' ->
          aux (N.add (N.of_int d) (N.mul n base')) d' in
      aux N.zero ds

  let binary_number = num 2
  let octal_number = num 8
  let decimal_number = num 10
  let hexadecimal_number = num 16

  let c_like_number_prefix c =
    item '0' -- char ~case_sensitive:false c

  let c_like_hex_number =
    c_like_number_prefix 'x' -+ hexadecimal_number

  let c_like_octal_number =
    c_like_number_prefix 'o' -+ octal_number

  let c_like_binary_number =
    c_like_number_prefix 'b' -+ binary_number

  let c_like_number =
    c_like_hex_number |~
    c_like_octal_number |~
    c_like_binary_number |~
    decimal_number

  let in_range min max n =
    if N.compare n min >= 0 && N.compare n max <= 0 then
      return n
    else
      fail ~msg:(Printf.sprintf "Integer not in [%s:%s]" (N.to_string min) (N.to_string max))

  let number_in_range min max = decimal_number >>= in_range min max

end

include Number (Int)

module Number32 = Number (Int32)
module Number64 = Number (Int64)

(*$= num & ~printer:(IO.to_string (parzer_result_printer Int.print Char.print))
  [1,(1,['2']); 12,(2,[])] (num 10 (0,['1';'2']) |> List.sort compare)
  [1,(1,[])] (num 10 (0,['1']) |> List.sort compare)
  [0,(1,[])] (num 10 (0,['0']) |> List.sort compare)
  [0,(1,['x'])] (num 10 (0,['0';'x']) |> List.sort compare)
*)

(*$= c_like_number & ~printer:(IO.to_string (parzer_result_printer Int.print Char.print))
  [1,(1,['2';'3']);    12,(2,['3']); 123,(3,[])] (c_like_number (0,['1';'2';'3']) |> List.sort compare)
  [0,(1,['x';'1';'2']); 1,(3,['2']);  18,(4,[])] (c_like_number (0,['0';'x';'1';'2']) |> List.sort compare)
  [0,(1,['b';'1';'0']); 1,(3,['0']);   2,(4,[])] (c_like_number (0,['0';'b';'1';'0']) |> List.sort compare)
  [0,(1,['x'])] (c_like_number (0,['0';'x']) |> List.sort compare)
*)
