open Hashtbl
type _unary =
  | Inc of int
  | Dec of int

type _compare =
  | Greater of int * int
  | Less of int * int
  | GreaterOrEqual of int * int
  | LessOrEqual of int * int
  | Equal of int * int

type _expression =
  | CompileUnit of _expression list
  | While of string list * _expression list
  | If of string list * _expression list * _expression list
  | ElseIf of string list * _expression list
  | Read
  | Else of _expression list
  | Return of string list
  | Ensure of string list
  | For of string * _compare * _unary * _expression list

let _compare_integers (cmp : _compare) : bool =
  match cmp with
  | Greater (x, y) -> x > y
  | Less (x, y) -> x < y
  | GreaterOrEqual (x, y) -> x >= y
  | LessOrEqual (x, y) -> x <= y
  | Equal (x, y) -> x = y

let _unary_operation (op : _unary) : int =
  match op with
  | Inc x -> x + 1
  | Dec x -> x - 1

let rec evaluate_expression expression hash_map minimal =
  let hashmap_length = Hashtbl.length hash_map in
  match expression with
  | CompileUnit exps ->
    let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
    CompileUnit exps
  | While (strings, exps) ->
    let transition = create 10 in
if minimal then (
   (*Hashtbl.add hash_map hashmap_length transition;*)
                   let key_exists1 = Hashtbl.mem hash_map (hashmap_length - 1) in
      if key_exists1 then (
             let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
                let _hash_length = Hashtbl.length hash_map in
              let tran= Hashtbl.find hash_map ( _hash_length - (1  +  (_hash_length - hashmap_length)))in
               let _tran2= Hashtbl.find hash_map ( _hash_length - 1    )in
               if (hashmap_length + 1) = _hash_length then (
                List.iter (fun s -> add tran s (hashmap_length - 1 )) strings;
               ) else 
                    (
                List.iter (fun s -> add tran s (hashmap_length )) strings;
               );
              (*  List.iter (fun s -> add tran s (hashmap_length - 1 )) strings;*)
(*Hashtbl.remove tran "E" ;         *)   
   Hashtbl.add _tran2 "E" (hashmap_length - 1  ) ;
    print_string "oooo";
               )
              else (
        let tran = create 10 in
                       let _tran2= Hashtbl.find hash_map ( hashmap_length - 1 )in
   Hashtbl.add _tran2 "E" (hashmap_length - 1  );
        Hashtbl.add hash_map (hashmap_length) tran ;
              ; );
              print_string "here";
)
else(
   (* Hashtbl.add hash_map hashmap_length transition;*)
   (* Hashtbl.add hash_map hashmap_length transition;*)
        let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
    let _hash_length = Hashtbl.length hash_map in
    let _tran2 = Hashtbl.find hash_map (_hash_length - 1 ) in
 Hashtbl.add hash_map _hash_length transition;
    List.iter (fun s -> add transition s (hashmap_length )) strings;
   Hashtbl.add _tran2 "E" (_hash_length + 1 );
    let _hash_length2 = Hashtbl.length hash_map in
    let _tran3 = Hashtbl.find hash_map (_hash_length - 1) in
     let _tran4 = Hashtbl.find hash_map (hashmap_length - 1) in
     Hashtbl.remove _tran4 "E" ;
     Hashtbl.add _tran4 "E" (_hash_length );
    Hashtbl.remove _tran3 "E" ;
    (*Hashtbl.add _tran2 "E" (hashmap_length );*)
    Hashtbl.add transition "$" (_hash_length2 ) ;
);
    While (strings, exps)
  | If (strings, exps, _else) ->
let transition = create 10 in
    if minimal then (
      (*Hashtbl.add hash_map (hashmap_length) transition;*)
      let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
      let _hash_length = Hashtbl.length hash_map in
      List.iter (fun s -> add transition s (_hash_length  )) strings;
      let _hash_len1 = Hashtbl.length hash_map in
       Hashtbl.add hash_map (_hash_len1 + 1) transition;
      let _evaluated_inner_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) _else in
      let _hash_len = Hashtbl.length hash_map in
           let key_exists1 = Hashtbl.mem hash_map (hashmap_length - 1) in
      if key_exists1 then (
              let tran= Hashtbl.find hash_map ( hashmap_length - 1 )in
              Hashtbl.remove tran "E" ;
               Hashtbl.add tran "E" (_hash_len );
               )
              else (
        let tran = create 10 in
        Hashtbl.add hash_map (hashmap_length) tran ;
        Hashtbl.add tran "E" (_hash_len + 1)
        ; );
           let key_exists = Hashtbl.mem hash_map _hash_len1 in
      if key_exists then (
              let tran= Hashtbl.find hash_map (_hash_len  )in
               Hashtbl.add tran "E" (_hash_len + 1);)
              else (
        let tran = create 10 in
        Hashtbl.add hash_map (hashmap_length) tran ;
        Hashtbl.add tran "E" (_hash_len + 1)
        ; );
    )
    else (
            Hashtbl.add hash_map hashmap_length transition;
      let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
      let _hash_length = Hashtbl.length hash_map in
      List.iter (fun s -> add transition s (hashmap_length + 1)) strings;
      let _hash_len1 = Hashtbl.length hash_map in
      let _evaluated_inner_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) _else in
      let _hash_len = Hashtbl.length hash_map in
      Hashtbl.add transition "$" (_hash_length);
    );
If (strings, exps, _else)
  | For (num, cond, update, exps) ->
    let transition = create 10 in
    Hashtbl.add hash_map hashmap_length transition;
    let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
    let _hash_length = Hashtbl.length hash_map in
    Hashtbl.add transition "$" (_hash_length);
    For (num, cond, update, exps)
  | ElseIf (strings, ifExps) ->
    let transition = create 10 in
    Hashtbl.add hash_map hashmap_length transition;
    List.iter (fun s -> add transition s hashmap_length) strings;
    let _evaluated_ifExps = List.map (fun exp -> evaluate_expression exp hash_map minimal) ifExps in
    ElseIf (strings, ifExps)
  | Else exps ->
    let transition = create 10 in
    let _hash_length = Hashtbl.length hash_map in
    Hashtbl.add transition "E" hashmap_length;
    let _evaluated_exps = List.map (fun exp -> evaluate_expression exp hash_map minimal) exps in
    Else exps
  | Return strings ->
    let transition = create 10 in
    if minimal then (
    let key_exists1 = Hashtbl.mem hash_map (hashmap_length - 1 ) in
      if key_exists1 then (
              let tran= Hashtbl.find hash_map ( hashmap_length - 1  )in
              List.iter (fun s -> add tran s 1000) strings; 
              )
              else (
                      print_string "s";
      );
    )
    else ( 
        Hashtbl.add hash_map hashmap_length transition;
    List.iter (fun s -> add transition s 1000) strings;
    );
    Return strings
  | Ensure strings ->
    let transition = create 10 in
    
    if minimal then (
            Hashtbl.remove hash_map (hashmap_length - 1 ) ;
            let tran2 = Hashtbl.find hash_map (hashmap_length - 2 )in
    List.iter (fun s -> Hashtbl.add tran2 s (hashmap_length - 1 )) strings;
        
    )
    else
            ( 
                    Hashtbl.add hash_map hashmap_length transition;
                    List.iter (fun s -> Hashtbl.add transition s (hashmap_length + 1 )) strings;);
    Ensure strings
  | Read ->
    let transition = create 10 in
     let hash_len = Hashtbl.length hash_map in
if hash_len = 0 then (
    if minimal then (
      Hashtbl.add hash_map hashmap_length transition ;
       let tran = create 10 in
 Hashtbl.add hash_map (hashmap_length + 1) tran ; 
 Hashtbl.add transition "E" (hashmap_length + 1 )  ;)
    else (
            Hashtbl.add hash_map hashmap_length transition;
    )
  )
  else (
    if minimal then (
            print_string "\n hii";
            Hashtbl.add hash_map hashmap_length  transition;
    ) else (
      Hashtbl.add hash_map hashmap_length transition;
      print_string "\n yoooo ";
    Hashtbl.add transition "E" (hashmap_length + 1);)
  );
   Read
;;

let my_nfa = create 10
let _expression1 = CompileUnit [Read; While (["T"], [Read]); Ensure ["0"]; Read;  While (["T"], [Read ;  While (["T"], [Read]);While (["T"], [Read]) ]) ; Ensure ["0"];Read ;Return ["EOF"]]
let _expression2 = CompileUnit [Read; If (["T"], [Read], []); Ensure ["0"]; Read; Return ["EOF"]]
let _tm_exp = CompileUnit [For ("i", Greater (1, 2), Inc 1, [])]

let _result = evaluate_expression _expression1 my_nfa true

let concatenate_nested_hashmap hashmap =
  let concatenated_string = ref "digraph finite_state_machine {\n" in
  concatenated_string := !concatenated_string ^ "\tfontname=\"Helvetica,Arial,sans-serif\"\n";
  concatenated_string := !concatenated_string ^ "\tnode [fontname=\"Helvetica,Arial,sans-serif\"]\n";
  concatenated_string := !concatenated_string ^ "\tedge [fontname=\"Helvetica,Arial,sans-serif\"]\n";
  concatenated_string := !concatenated_string ^ "\trankdir=LR;\n";
  concatenated_string := !concatenated_string ^ "\tnode [shape = doublecircle]; 1000 ; \n";
  concatenated_string := !concatenated_string ^ "\tnode [shape = circle];\n";
  Hashtbl.iter (fun key1 inner_hashmap ->
    Hashtbl.iter (fun key2 value ->
      concatenated_string := !concatenated_string ^ "\t" ^ string_of_int key1 ^ " -> " ^ string_of_int value ^ " [label = \"" ^ key2 ^ "\"];\n"
    ) inner_hashmap
  ) hashmap;
  concatenated_string := !concatenated_string ^ "}";

  !concatenated_string
;;

let result = concatenate_nested_hashmap my_nfa in
let oc = open_out "tm1.dot" in
output_string oc result;
close_out oc;;
let _ = Sys.command "dot -Tpdf tm1.dot -o tm1.pdf" ;
