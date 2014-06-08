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

let read str =
   let str1 = skipSpaces str in
   let c = if str1 = "" then '_' else str.[0] in
     if str1 = "" then (Error "empty input", "")
     else if c = kRPar then (Error ("invalid syntax: " ^ str), "")
     else if c = kLPar then (Error "noimpl", "")
     else if c = kQuote then (Error "noimpl", "")
     else readAtom str1

let printObj _ = ()

let rec repl prompt =
  print_string prompt;
  printObj (read (read_line ()));
  repl prompt

let () = try repl "> " with End_of_file -> ()
