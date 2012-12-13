(* vm.ml
   The bytecode interpreter.
   Or what will one day be it, maybe.

*)


type instr =
      Const of value
    | Var of value
    | Return of value
    | CallMethod of value
    | Return
    | Compare of value * value
	
    | Jump of int
    | JumpIfEqual of int
    | JumpIfNotEqual of int
    | JumpIfGreater of int
    | JumpIfLessThan of int

(* Math, logic, etc *)

and value =
      Int of int
    | Instance of value array
;;

type classdef = string * string list;;



type vmInstance = {
    mutable instructions : instr array;
    mutable ip : int;
    mutable return : value;
    mutable stack : unit;
  }
;;

let makeVMInstance () = {
    instructions = [||];
    ip = 0;
    return = Int( 0 );
    stack = ();
  }
;;

let addInstruction vm =
  ()
;;
