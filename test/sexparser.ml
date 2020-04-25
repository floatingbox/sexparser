type sexp = Atom of string | List of sexp list

let rec sexp_to_string = function
  | Atom s -> s
  | List sexps ->
      let s =
        List.fold_left (fun a b -> a ^ " " ^ sexp_to_string b) "(" sexps
      in
      s ^ ")"

exception Failed

module Option = Core.Option

let list_of_stack stack =
  let rec list_of_stack' stack result =
    if Stack.is_empty stack then result
    else
      let head = Stack.pop stack in
      let tail = list_of_stack' stack result in
      head :: tail
  in
  list_of_stack' stack [] |> List.rev

let parse text =
  let rec parse' text pos stack sstack =
    match text.[pos] with
    | ' ' -> parse' text (pos + 1) stack sstack
    | '(' ->
        Option.iter stack ~f:(fun stack -> Stack.push stack sstack);
        let stack = Stack.create () in
        parse' text (pos + 1) (Some stack) sstack
    | ')' ->
        let stack = Option.value_exn stack in
        let items = list_of_stack stack in
        let item = List items in
        if Stack.is_empty sstack then item
        else
          let stack = Stack.pop sstack in
          Stack.push item stack;
          parse' text (pos + 1) (Some stack) sstack
    | '"' ->
        let stack = Option.value_exn stack in
        let end_pos = String.index_from text (pos + 1) '"' in
        let len = end_pos - pos + 1 in
        let item = String.sub text pos len in
        Stack.push (Atom item) stack;
        parse' text (end_pos + 1) (Some stack) sstack
    | _ ->
        let stack = Option.value_exn stack in
        let splits = [ ' '; '\t'; '('; ')'; '"' ] in
        let end_pos =
          List.fold_left
            (fun end_pos split ->
              match (end_pos, String.index_from_opt text (pos + 1) split) with
              | Some end_pos, Some new_end_pos -> Some (min end_pos new_end_pos)
              | Some end_pos, None -> Some end_pos
              | None, Some new_end_pos -> Some new_end_pos
              | _ -> None)
            None splits
        in
        let end_pos =
          match end_pos with
          | Some end_pos -> end_pos
          | None -> String.length text
        in
        let end_pos = end_pos - 1 in
        let len = end_pos - pos + 1 in
        let item = String.sub text pos len in
        if end_pos = String.length text - 1 then Atom item
        else (
          Stack.push (Atom item) stack;
          parse' text (end_pos + 1) (Some stack) sstack )
  in
  parse' text 0 None (Stack.create ())

let () =
  let text = input_line stdin in
  let item = parse text in
  Printf.printf "parsed: %s\n" (sexp_to_string item)
