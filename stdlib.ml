(* stdlib.ml
   Standard library of necessary things.

*)

open Translator

exception RuntimeError of string

let runtime message =
  raise (RuntimeError( message ))
;;

(*
(* Arg handling and verification *)
let arg1 x =
  List.nth x 0
;;

let arg2 x =
  List.nth x 1
;;

let arg3 x =
  List.nth x 2
;;

let arg4 x =
  List.nth x 3
;;

let arg5 x =
  List.nth x 4
;;

let argn x i =
  List.nth x i
;;

let numArgs x = 
  List.length x
;;

let isInt = function
    Int( _ ) -> true
  | _ -> false
;;

let isSymbol = function
    Int( _ ) -> true
  | _ -> false
;;

let isTuple = function
    Int( _ ) -> true
  | _ -> false
;;

let isArray = function
    Int( _ ) -> true
  | _ -> false
;;

let isFunc = function
    Int( _ ) -> true
  | _ -> false
;;

let isBuiltin = function
    Int( _ ) -> true
  | _ -> false
;;

let isCallable = function
    Int( _ ) -> true
  | _ -> false
;;


let extractValue = function
    Value( v ) -> v
  | _ -> Null
;;


let getInt = function
    Int( a ) -> a
  | _ -> runtime "Int expected!"
;;

let getSymbol = function
    Symbol( a ) -> a
  | _ -> runtime "Symbol expected!"
;;

let getTuple = function
    Tuple( a ) -> a
  | _ -> runtime "Tuple expected!"
;;

let getArray = function
    Array( a ) -> a
  | _ -> runtime "Array expected!"
;;



let sym2bool x =
  let b = getSymbol x in
    b = "true"
;;

let bool2sym b =
  if b then Symbol( "true" )
  else Symbol( "false" )
;;


(* Math and logic *)
let add x = 
  let vals = List.map getInt x in
    Int( List.fold_left (+) 0 vals )
;;

let sub x = 
  let vals = List.map getInt x in
    Int( List.fold_left (-) (List.hd vals) (List.tl vals) )
;;

let mul x = 
  let vals = List.map getInt x in
    Int( List.fold_left ( * ) 1 vals )
;;

let div x = 
  let vals = List.map getInt x in
    Int( List.fold_left (/) (List.hd vals) (List.tl vals) )
;;

let modulus x = 
  if (numArgs x) <> 2 then
    runtime "Mod expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      Int( a1 mod a2 )
;;
      



let gt x =
  if (numArgs x) <> 2 then
    runtime "> expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 > a2)
;;


let lt x =
  if (numArgs x) <> 2 then
    runtime "< expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 < a2)
;;


let gte x =
  if (numArgs x) <> 2 then
    runtime ">= expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 >= a2)
;;


let lte x =
  if (numArgs x) <> 2 then
    runtime "<= expects 2 args!"
  else
    let a1 = getInt (arg1 x)
    and a2 = getInt (arg2 x) in
      bool2sym (a1 <= a2)
;;


let printNum x =
  let a = arg1 x in
  let n = getInt a in
    Printf.printf "%d\n" n;
    Null
;;


let eq x =
  if (numArgs x) <> 2 then
    runtime "= expects 2 args!"
  else
    let a1 = (arg1 x)
    and a2 = (arg2 x) in
      bool2sym (a1 = a2)
;;

let neq x =
  if (numArgs x) <> 2 then
    runtime "/= expects 2 args!"
  else
    let a1 = (arg1 x)
    and a2 = (arg2 x) in
      bool2sym (a1 <> a2)
;;



(* Arrays *)
let arrayGet x =
  let array = List.nth x 0
  and index = List.nth x 1 in
    match array with
	Array( arr ) ->
	  (match index with
	      Int( idx ) -> extractValue arr.(idx)
	    | _ -> Null)
      | _ -> Null
;;

let arraySet x =
  let array = List.nth x 0
  and index = List.nth x 1
  and itm = List.nth x 2 in
    match array with
	Array( arr ) ->
	  (match index with
	      Int( idx ) -> arr.(idx) <- (Value( itm )); Null;
	    | _ -> Null)
      | _ -> Null
;;

let arrayMake x =
  match x with
      [Int( x )] -> Array( Array.create x (Value( Null )) )
    | _ -> Null
;;

let arrayLength x = 
  match x with
      [Array( x )] -> Int( (Array.length x) )
    | _ -> Null
;;

let arrayConcat x =
  ()
;;

let arrayExpand x =
  ()
;;

let arraySlice x =
  ()
;;




(* File I/O *)
let openFile x =
  ()
;;

let closeFile x =
  ()
;;

let read x =
  ()
;;

let readLine x =
  ()
;;

let readChar x =
  ()
;;

let readInt x =
  ()
;;

let writeString x =
  ()
;;

let writeChar x =
  ()
;;

let writeInt x =
  ()
;;

let seek x =
  ()
;;


(* String handling *)

let char2str x =
  ()
;;

let strGet x =
  ()
;;

let strSet x =
  ()
;;

let strMake x =
  ()
;;

let strLength x =
  ()
;;

let strSlice x =
  ()
;;

(* Other stuff *)
let evalString x =
  ()
;;
*)


let initLib stbl =
(*
  setVar "user" "+"   (Builtin( add )) stbl;
  setVar "user" "-"   (Builtin( sub )) stbl;
  setVar "user" "*"   (Builtin( mul )) stbl;
  setVar "user" "/"   (Builtin( div )) stbl;
  setVar "user" "mod" (Builtin( modulus )) stbl;

  setVar "user" ">"  (Builtin( gt )) stbl;
  setVar "user" "<"  (Builtin( lt )) stbl;
  setVar "user" ">=" (Builtin( gte )) stbl;
  setVar "user" "<=" (Builtin( lte )) stbl;
  setVar "user" "="  (Builtin( eq )) stbl;
  setVar "user" "/=" (Builtin( neq )) stbl;

  setVar "user" "print" (Builtin( printNum )) stbl;


  setVar "user" "aget" (Builtin( arrayGet )) stbl;
  setVar "user" "aset" (Builtin( arraySet )) stbl;
  setVar "user" "array" (Builtin( arrayMake )) stbl;
  setVar "user" "arrLen" (Builtin( arrayLength )) stbl;
*)
  ();
;;
