open Batteries
let (|?) = Option.(|?)
let (/@) a b = List.map b a
let (//@) a b = List.filter_map b a

type 'b unparsed = int * 'b list (* position * list of things left to parse *)

(* All parsers are stateless *)
type error = string * int (* msg and where we failed (length of what was parsed) *)
type ('a, 'b) parzer_result = ('a * 'b unparsed) list
type ('a, 'b) parzer = 'b unparsed (* tokens to add *) -> ('a, 'b) parzer_result (* possible results *)

let parzer_result_printer print_res print_tok =
  let single_result_printer fmt = function
    | res, (_, []) -> print_res fmt res
    | res, (n, rest) -> Printf.fprintf fmt "%a until %d, rest %a" print_res res n (List.print print_tok) rest in
  List.print single_result_printer

let max_error = ref None
let reset_parse_error () = max_error := None
let failure msg (pos,_rest) =
    (match !max_error with
    | Some (_, max_pos) when max_pos >= pos -> ()
    | _ ->
        max_error := Some (msg, pos)) ;
    []

exception Parse_error of error
let parse_error () =
    match !max_error with
    | None -> raise (Parse_error ("unknown error", 0))
    | Some err ->
        raise (Parse_error err)

let string_of_error ?input (msg, pos) =
    let abbrev l s =
        if String.length s < l+3 then s else String.sub s 0 l ^"..." in
    let where =
        (match input with None -> "pos "^ string_of_int pos
                        | Some s -> "pos "^ string_of_int pos ^" ('"^ abbrev 84 String.(right s (length s - pos)) ^"')") in
    msg ^" at "^ where

let () = Printexc.register_printer (function Parse_error e -> Some (string_of_error e) | _ -> None)

let get_error ?input () =
    match !max_error with
    | None -> "unknown error"
    | Some err -> string_of_error ?input err

(* unconditionally returns something - useful for bind *)
let return res bs = [ res, bs ]
(*$= return & ~printer:(IO.to_string (parzer_result_printer String.print Int.print))
  ["glop", (0, [1; 2])] (return "glop" (0, [ 1; 2 ]))
 *)

(* fails unconditionally *)
let fail ?msg bs = match msg with
    | Some msg -> failure ("Failure: "^msg) bs
    | None -> []

(* Matches the end of input *)
let eof = function
    | _pos, [] as bs -> [ (), bs ]
    | bs -> failure "EOF expected" bs
(*$= eof & ~printer:(IO.to_string (parzer_result_printer Unit.print Int.print))
  [] (eof (0,['X']))
  [(),(0,[])] (eof (0,[]))
*)

(* Accept 'nothing' *)
let nothing bs = return () bs

(* Fail like p or consume nothing if p match *)
let check p bs =
    let e = p bs in
    if e = [] then []
    else [ (), bs ]
(*$T check
  not @@ List.is_empty (check eof (0,[]))
  List.is_empty (check eof (0,['X']))
*)

let upto delim_orig bs =
    let rec aux past_delim delim past_bs bs =
        match delim, bs with
        | [], _ -> [ List.rev past_bs, bs ]
        | _, (_pos, []) -> []
        | d::delim', (pos, b::bs') ->
            if d = b then aux (d::past_delim) delim' (b::past_bs) (pos+1, bs') else
            (match past_delim with
            | [] ->
                aux [] delim_orig (b::past_bs) (pos+1, bs')
            | [_] -> (* stay in place *)
                aux [] delim_orig past_bs bs
            | _::p' -> (* rollback all chars but one *)
                let rb = List.length p' in
                let rbs, past_bs' = List.split_at rb past_bs in
                aux [] delim_orig past_bs' (pos-rb, List.rev_append rbs (snd bs))) in
    aux [] delim_orig [] bs
(*$= upto & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[0;0], (2,[])] (upto [0;0] (0,[0;0]))
  [] (upto [0;0] (0,[0]))
  [] (upto [0;0] (0,[]))
  [] (upto [0;0] (0,[1;0]))
  [] (upto [0;0] (0,[1]))
  [[1;2;3;0;0], (5,[])] (upto [0;0] (0,[1;2;3;0;0]))
  [[1;2;3;0;0], (5,[4;5])] (upto [0;0] (0,[1;2;3;0;0;4;5]))
  [[1;2;0;3;0;0], (6,[4;5])] (upto [0;0] (0,[1;2;0;3;0;0;4;5]))
 *)

let cond ?errmsg c = function
    | pos, b::bs when c b -> [ b, (pos+1, bs) ]
    | rest -> match errmsg with None -> []
                            | Some m -> failure m rest

let item ?errmsg i = cond ?errmsg ((=) i)
(*$= item & ~printer:(IO.to_string (parzer_result_printer Int.print Int.print))
  [1, (1,[1;2;2])] (item 1 (0,[1;1;2;2]))
 *)

let range mi ma = cond (fun c -> c >= mi && c <= ma)
(*$= range & ~printer:(IO.to_string (parzer_result_printer Int.print Int.print))
  [1, (1,[2;3])] (range 1 5 (0,[1;2;3]))
  [5, (1,[2;3])] (range 1 5 (0,[5;2;3]))
  [] (range 1 5 (0,[6;2;3]))
 *)

let take n (pos,bs) =
    try let res, rem = List.split_at n bs in
        [ res, (pos+n, rem) ]
    with Invalid_argument _ -> []
(*$= take & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;2], (2,[3;4;5])] (take 2 (0,[1;2;3;4;5]))
  [[1;2;3], (3,[])] (take 3 (0,[1;2;3]))
  [] (take 3 (0,[1;2]))
*)

(* equivalent to [take 1 >>: List.hd], only faster *)
let one = function
    | _pos, [] -> []
    | pos, b::bs -> [ b, (pos+1, bs) ]

(* Change value returned by p through f *)
let map_res p f bs =
    p bs /@ fun (res, rem) -> (f res, rem)

let (>>:) = map_res

(* Record [msg] as an error message if parser [p] fails to parse anything
 * from [bs]. This is different from [p |~ fail msg] in that the later
 * would always set an error message ([|~] is not exclusive) *)
let with_err msg p bs =
  match p bs with
  | [] -> failure msg bs
  | res -> res

(* Filter_map every results returned by p through f *)
type 'a map_filter_res = Ok of 'a | Fail of string | FailSilent
let map_filter p f bs =
    p bs //@ fun (res, rem) ->
        match f res with
        | Ok x -> Some (x, rem)
        | Fail msg -> ignore (failure msg rem) ; None
        | FailSilent -> None

let (>>?) = map_filter

let map_filter p f bs =
    p bs //@ fun (res, rem) ->
        Option.map (fun x -> x, rem) (f res)

let some p = p >>: (fun res -> Some res)
let none p = p >>: (fun _ -> None)
let ign p = p >>: ignore

(* combinators *)

(* sequence of 2 parsers *)
let cons p1 p2 bs =
    (* for each result of p1 bs, continue parsing the rest with p2 and return the product of all results *)
    p1 bs /@ (fun (res1, rem1) ->
        p2 rem1 /@ (fun (res2, rem2) ->
            (res1, res2), rem2)) |>
    List.flatten

(* Same but keeps only the first or second value *)
let cons1 p1 p2 =
    (cons p1 p2) >>: fst
let cons2 p1 p2 =
    (cons p1 p2) >>: snd

let (++) = cons
let (+-) = cons1
let (-+) = cons2
let (--) p1 p2 = p1 ++ p2 >>: ignore

let (|~) p1 p2 bs =
    (* Take care that most of the time alternatives are exclusives *)
    let r1 = p1 bs and r2 = p2 bs in
    if r2 = [] then r1 else
    List.rev_append r1 r2

let optional p =
    some p |~ return None

let optional_greedy p bs =
    p bs /@ fun (res,rem) -> Some res, rem

let optional_default def p =
    p |~ return def

let rec repeat ?sep ?min ?max p bs =
    let pred_opt = Option.map pred in
    if max = Some 0 then (
        if min = None || min = Some 0 then [ [], bs ] else []
    ) else
    match sep with
    | Some sep ->
        (match min with
        | Some mi when mi > 1 ->
            (p +- sep ++ repeat ~sep ?min:(pred_opt min) ?max:(pred_opt max) p >>: fun (v,vs) -> v::vs) bs
        | Some mi when mi == 1 ->
            (p ++ optional (sep -+ repeat ~sep ~min:1 ?max:(pred_opt max) p) >>: fun (v,vs_o) -> v::(Option.default [] vs_o)) bs
        | _ ->
            (optional (repeat ~sep ~min:1 ?max p) >>: Option.default []) bs)
    | None ->
        (match min with
        | Some mi when mi >= 1 ->
            (p ++ repeat ?min:(pred_opt min) ?max:(pred_opt max) p >>: fun (v,vs) -> v::vs) bs
        | _ ->
            (optional (repeat ~min:1 ?max p) >>: Option.default []) bs)
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

let several ?sep p bs = repeat ?sep ~min:1 p bs
(*$= several & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1],(1,[1;1;2]); [1;1],(2,[1;2]); [1;1;1],(3,[2])] (several (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [] (several (item 1) (0,[3;2;1]) |> List.sort compare)
*)

let any ?sep p bs = repeat ?sep p bs
(*$= any & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[],(0,[1;1;2]); [1],(1,[1;2]); [1;1],(2,[2])] (any (item 1) (0,[1;1;2]) |> List.sort compare)
  [[],(0,[3;2;1])] (any (item 1) (0,[3;2;1]) |> List.sort compare)
*)

(* FIXME: simplify using cons ? *)
let rec several_greedy ?sep p bs =
    p bs |>
    List.map (fun (res, rest) ->
        match several_greedy (match sep with Some sep -> sep -+ p | None -> p) rest with
        | [] -> [[res], rest]
        | l' -> List.map (fun (r,rs) -> (res::r), rs) l') |>
    List.flatten
(*$= several_greedy & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;1;1],(3,[2])] (several_greedy (item 1) (0,[1;1;1;2]))
  [] (several_greedy (item 1) (0,[3;2;1]))
  [[1;1;1],(5,[2])] (several_greedy ~sep:(item 0) (item 1) (0,[1;0;1;0;1;2]))
  [[1;1;1],(5,[0;2])] (several_greedy ~sep:(item 0) (item 1) (0,[1;0;1;0;1;0;2]))
*)

let all ?sep p bs =
    match several_greedy ?sep p bs with
    | [] -> [[],bs]
    | l -> l
(*$= all & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [[1;1;1],(3,[2])] (all (item 1) (0,[1;1;1;2]) |> List.sort compare)
  [[],(0,[2])] (all (item 1) (0,[2]) |> List.sort compare)
*)

let times ?sep n p bs = repeat ?sep ~min:n ~max:n p bs
(*$= times & ~printer:(IO.to_string (parzer_result_printer (List.print Int.print) Int.print))
  [ [1;1],(2,[1;3]) ] (times 2 (item 1) (0,[1;1;1;3]))
  [ [],(0,[1;1;1;3]) ] (times 0 (item 1) (0,[1;1;1;3]))
*)

(* Run the results of p through f, that will return a new result.
   Once this new parser got it's result, give new items to first parser and so on *)
let bind p f bs =
    p bs /@
    (fun (res, rem) -> (f res) rem) |>
    List.flatten
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

(* invert the result of p - useful with check *)
let no p bs =
    if List.is_empty (p bs) then [ (), bs ] else []

(* Use the results of the first parser as the input elements of the second.
   Return the first result of p2.
   For the reader, we make types explicit: 'inp is the input type, tok1 is the token
   returned by p1 and tok2 those returned by p2. *)
let pipe (p1 : ('a,'b) parzer)
         (p2 : ('c,'a) parzer)
         (bs : 'b unparsed) : ('c,'b) parzer_result =
    let rec aux prev1 bs = (* prev1 is the list of previous p1 results - reverted *)
        p1 bs /@ (fun (res1, (pos1,rem1)) ->
            let prev1 = res1 :: prev1 in
            (* p2 must be able to parse every tokens so far (prev1) *)
            ((p2 (pos1,(List.rev prev1)) //@
              (fun (res2, (_pos2,rem2)) -> if rem2 = [] then Some res2 else None)) /@
             (fun res2 -> res2, (pos1,rem1))) @
            aux prev1 (pos1,rem1)) |>
        List.flatten
    in aux [] bs
(*$T pipe
  let p = pipe (upto [0]) (item [2;1;0]) in \
  p (0,[2;1;0;5;0;42]) = [[2;1;0],(3,[5;0;42])] && \
  several p (0,[2;1;0;5;0;42]) = [[[2;1;0]],(3,[5;0;42])] && \
  p (0,[5;0;42]) = []
 *)

(* Try all of the given parser in turn *)
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
