let kLPar = '('
let kRPar = ')'
let kQuote = '\''

type obj =
  Nil
| Num of int
| Sym of string
| Error of string
| Cons of (obj ref) * obj ref
| Subr of (obj -> obj)
| Expr of obj * obj * obj

let safeCar obj =
  match obj with
    Cons(a, d) -> !a
  | _ -> Nil

let safeCdr obj =
  match obj with
    Cons(a, d) -> !d
  | _ -> Nil

let symTable = ref [("nil", Nil)]
let rec lookupSym str tbl =
  match tbl with
    [] -> None
  | (k, v)::rest ->
      if k = str then Some v
      else lookupSym str rest
let makeSym str =
  match lookupSym str !symTable with
    Some sym -> sym
  | None ->
      let ret = Sym str in
        symTable := (str, ret)::(!symTable);
        ret

let makeCons a d = Cons(ref a, ref d)

let rec nreconc lst tail =
  match lst with
    Cons(a, d) ->
      let tmp = !d in
        d := tail;
        nreconc tmp lst
  | _ -> tail
let nreverse lst = nreconc lst Nil

let isSpace c =
  c = '\t' || c = '\r' || c = '\n' || c = ' '

let isDelimiter c =
  c = kLPar || c = kRPar || c = kQuote || isSpace c

let skipSpaces str =
  let rec doit i =
    if i = String.length str then ""
    else if isSpace str.[i] then doit (i + 1)
    else String.sub str i (String.length str - i)
  in doit 0

let makeNumOrSym str =
  try Num (int_of_string str) with
    Failure "int_of_string" -> makeSym str

let position f str =
  let rec doit i =
    if i = String.length str then None
    else if f str.[i] then Some i
    else doit (i + 1)
  in doit 0

let readAtom str =
  match position isDelimiter str with
    Some n ->
      (makeNumOrSym (String.sub str 0 n),
       String.sub str n (String.length str - n))
  | None -> (makeNumOrSym str, "")

let lookAhead str =
   let str1 = skipSpaces str in
   let c = if str1 = "" then '_' else str.[0] in
   let rest = if str1 = "" then ""
              else (String.sub str1 1 (String.length str1 - 1)) in
     (str1, c, rest)

let rec read str =
  let (str1, c, rest) = lookAhead str in
    if str1 = "" then (Error "empty input", "")
    else if c = kRPar then (Error ("invalid syntax: " ^ str1), "")
    else if c = kLPar then readList rest Nil
    else if c = kQuote then readQuote rest
    else readAtom str1
and readQuote str =
  let (elm, next) = read str in
    (makeCons (makeSym "quote") (makeCons elm Nil), next)
and readList str acc =
  let (str1, c, rest) = lookAhead str in
    if str1 = "" then (Error "unfinished parenthesis", "")
    else if c = kRPar then (nreverse acc, rest)
    else
      match read str1 with
        (Error e, next) -> (Error e, next)
      | (elm, next) -> readList next (makeCons elm acc)

let rec printObj obj =
  match obj with
    Nil -> "nil"
  | Num n -> string_of_int n
  | Sym s -> s
  | Error s -> "<error: " ^ s ^ ">"
  | Cons(a, d) -> "(" ^ (printList obj "" "") ^ ")"
  | Subr _ -> "<subr>"
  | Expr _ -> "<expr>"
and printList obj delimiter acc =
  match obj with
    Cons(a, d) ->
      printList (!d) " " (acc ^ delimiter ^ printObj (!a))
  | Nil -> acc
  | _ -> acc ^ " . " ^ printObj obj

let first (x, y) = x

let rec repl prompt =
  print_string prompt;
  print_string (printObj (first (read (read_line ()))));
  print_newline ();
  repl prompt

let () = try repl "> " with End_of_file -> ()
