(* 
   Mayhem backend

   Simon Heath
   28/9/2006
*)

exception Fubar of string;;
exception TypeError of string;;
exception UnboundVar of string;;
exception MatchError of string;;
exception NamespaceError of string;;


type syntree =
    Literal of valuetype
  | Var of label
  | VarDef of label * syntree
  | Apply of syntree * syntree * syntree list
  | If of syntree * syntree list * syntree list
  | MethodDef of label * label * string list * syntree list
  | ClassDef of label * label * string list

  | Comment of string

and label = string

and valuetype =
    Int of int
      (* Class, members *)
  | Instance of valuetype * (string, valuetype) Hashtbl.t
      (* Class, args, body *)
  | Method of string list * syntree list
  | Null

(*  | Array of syntree array
  | Char of char
  | String of string

  (* Closures! They need to know their environment.  *)
  | Func of string list * syntree list * (string * valuetype) list
  | Builtin of (valuetype list -> valuetype)
  | Null
*)
;;



let rec vallist2str lst =
  let rec loop lst accm =
    match lst with
	[] -> accm
      | hd :: tl ->
	  match hd with
	      Value( v ) ->
		loop tl (accm ^ (value2str v) ^ " ")
	    | _ -> raise (Fubar( "Tuple is not awake!" ))
  in
    (loop lst "")

and value2str = function
    Int( i ) -> Printf.sprintf "%d" i
  | Var( s ) ->  s
(*
  | Null -> "Null"
  | Tuple( x ) -> "[" ^ (vallist2str x) ^  "\b]"  (* \b = Funny kludge *)
  | Func( _ ) -> "Some func"
  | Builtin( _ ) -> "Some builtin"
  | Array( x ) -> "{" ^ (vallist2str (Array.to_list x)) ^ "\b}"
  | Char( x ) -> Printf.sprintf "'%c'" x
  | String( x ) -> "\"" ^ x ^ "\""
*)
;;

type symtable = {
    mutable stack : ((string, valuetype) Hashtbl.t) list;
    mutable globals : (string, valuetype) Hashtbl.t
  }

let makeSymtbl () = 
  let defaultNamespace = Hashtbl.create 1 in
  let t = {
    stack = [];
    globals = Hashtbl.create 8;
    } in
    t
;;




let pushScope stbl =
(*  Printf.printf "Pushing scope %d\n" ((List.length stbl) + 1); *)
  stbl.stack <- (Hashtbl.create 8) :: stbl.stack;
;;

let popScope stbl =
  match stbl.stack with
(*      x :: [] -> raise (Fubar( "Tried to ditch toplevel scope!" ))
    | *) [] -> raise (Fubar( "I have no scope and I must bind." ))
    | hd :: tl -> stbl.stack <- tl
;;



let getLocal v symtbl = 
  let rec loop stack =
    match stack with
	[] ->
	  raise (UnboundVar( v ))
      | hd :: tl -> 
	  try
	    Hashtbl.find hd v
	  with
	      Not_found -> 
		loop tl
  in
    loop symtbl.stack
;;

let getGlobal v symtbl =
  try
    Hashtbl.find symtbl.globals v
  with
      Not_found -> raise (UnboundVar( ns ^ ":" ^ v ))
;;

(* A bit of exception-wanking happens here to make the local
   and globals cooperate.
   I don't like it, but...  Oh well.
*)
let getVar v symtbl =
  match v with
      Var( ns, s ) ->
	(try
	    getGlobal ns s symtbl
	  with
	      UnboundVar( _ ) -> getLocal s symtbl)

    | _ -> raise (Fubar( "getVar: Var required!" ))

;;


let rec setVar name value symtbl =
(*  Printf.printf "Adding %s:%s\n" namespace name; *)
  if symtbl.stack = [] then
    Hashtbl.replace symtbl.globals name value
  else 
    Hashtbl.replace (List.hd symtbl.stack) name value
;;

let setSlot instance slotname value =
  match instance with
    Instance( cls, slots ) ->
      Hashtbl.replace slots slotname value;
    | _ -> ()
;;





let rec eval tree tbl =
    Literal( v ) ->
      (match v with
	  Int( x ) -> v)


  | Var( s ) -> getVar v tbl
  | VarDef( label, tree ) -> setVar label (eval tree tbl) tbl;
      
  | Apply( name, obj, args ) ->
      let f = eval name tbl
      and o = eval obj tbl
      and a = List.map (fun x -> eval x tbl) args in
	apply f o a tbl
  | If( cond, ifpart, elsepart ) -> Int( 0 )
  | MethodDef( name, cls, args, body ) ->
      let theclass = eval cls tbl in
	setSlot theclass name (Method( args, body ))
  | ClassDef( name, super, members ) -> 
      let cls = Instance( Null, (Hashtbl.create 8) ) in
	...

  | Comment( s ) -> Int( 0 )




and apply meth obj args tbl =
  ()
;;



(*




(* We split the globals and stack up... *)
type symtable = {
    mutable stack : ((string, valuetype) Hashtbl.t) list;
    namespaces : (string, (string, valuetype) Hashtbl.t) Hashtbl.t;
    mutable currentNamespaceName : string;
    mutable globals : (string, valuetype) Hashtbl.t
  }


let addNamespace stbl name =
  Hashtbl.add stbl.namespaces name (Hashtbl.create 16);
;;

(* This will still preserve the current namespace until you leave it.
   I think this is the right behavior anyway.  ^_^
*)
let delNamespace stbl name =
  Hashtbl.remove stbl.namespaces name
;;


let setNamespace stbl name =
  try
    stbl.globals <- Hashtbl.find stbl.namespaces name;
    stbl.currentNamespaceName <- name
  with
      Not_found -> raise (NamespaceError( "Namespace '" ^ name ^ "' DNE!" ))
;;


let makeSymtbl () = 
  let defaultNamespace = Hashtbl.create 1 in
  let t = {
    stack = [];
    namespaces = Hashtbl.create 8;
    currentNamespaceName = "user";
    globals = defaultNamespace;
    } in
    addNamespace t "user";
    setNamespace t "user";
    t
;;




let pushScope stbl =
(*  Printf.printf "Pushing scope %d\n" ((List.length stbl) + 1); *)
  stbl.stack <- (Hashtbl.create 8) :: stbl.stack;
;;

(*
let addScope scope stbl =
  scope :: stbl
;;
*)

let popScope stbl =
  match stbl.stack with
(*      x :: [] -> raise (Fubar( "Tried to ditch toplevel scope!" ))
    | *) [] -> raise (Fubar( "I have no scope and I must bind." ))
    | hd :: tl -> stbl.stack <- tl
;;



let getLocal v symtbl = 
  let rec loop stack =
    match stack with
	[] ->
	  raise (UnboundVar( v ))
      | hd :: tl -> 
	  try
	    Hashtbl.find hd v
	  with
	      Not_found -> 
		loop tl
  in
    loop symtbl.stack
;;

let currentNamespace = "";;

let getGlobal ns v symtbl =
  try
    if ns = currentNamespace then (
	Hashtbl.find symtbl.globals v
      ) else (
	if Hashtbl.mem symtbl.namespaces ns then (
	    let namespace = Hashtbl.find symtbl.namespaces ns in
	      Hashtbl.find namespace v
	  ) else (
	    (* Printf.printf "Blople?  %s\n" ns; Hashtbl.iter (fun x y ->
	       Printf.printf "%s -> ...\n" x) symtbl.namespaces;
	    *)
	    raise (NamespaceError( "Namespace '" ^ ns ^ "' does not exist!" ));
	  )
      )
  with
      Not_found -> raise (UnboundVar( ns ^ ":" ^ v ))
;;

(* A bit of exception-wanking happens here to make the local
   and globals cooperate.
   I don't like it, but...  Oh well.
*)
let getVar v symtbl =
  match v with
      Var( ns, s ) ->
	(try
	    getGlobal ns s symtbl
	  with
	      UnboundVar( _ ) -> getLocal s symtbl)

    | _ -> raise (Fubar( "getVar: Var required!" ))

;;


let rec setVar namespace name value symtbl =
(*  Printf.printf "Adding %s:%s\n" namespace name; *)
  if namespace = "" then
    if symtbl.stack = [] then
      Hashtbl.replace symtbl.globals name value
    else 
      Hashtbl.add (List.hd symtbl.stack) name value

  else
    let ns = Hashtbl.find symtbl.namespaces namespace in
      Hashtbl.add ns name value



(* Closures work.
   They may abruptly stop working if values become mutable, like this:

   defun accm x = 
   let v = ref x in
   fun = (set! v (add v 1))
   end

   let f = (accm 10) in
   (f) -> 11
   (f) -> 11, NOT 12.

   Since (set!) doesn't exist, it doesn't matter.  For now.
   But when we add objects by reference, we have to make sure it works.

*)
and extractClosure (arglst : string list) funbody symtbl =
  let env = ref [] in  (* A list of name/value pairs of bound vars *)
  let args = ref [] in (* A list of lists of argument names *)
  let pushSym sym vl =
    env := (sym,vl) :: !env
      
  and pushArgs arglst =
    args := arglst :: !args
  and popArgs () =
    args := List.tl !args
  and isArg term =
    let rec loop itm arglst =
      if List.mem term itm then (
	  (*	  Printf.printf "%s is an arg!\n" term; *)
	  true
	)
      else if arglst = [] then (
	  (*	  Printf.printf "%s is not an arg!\n" term; *)
	  false
	) else
	loop (List.hd arglst) (List.tl arglst)
    in
      loop (List.hd !args) (List.tl !args)
  in

    pushArgs arglst;
    (*    Printf.printf "# of args: %d\n" (List.length arglst); *)
    (*    List.iter print_endline (List.hd !args); *)
    
    let rec extractTerm = function
	Value( t ) ->
	  (match t with
	      Var( namespace, name ) ->
		if isArg name then
		  ()
		    (*		  Printf.printf "Is arg: %s\n" name *)
		else (
		    (*		    Printf.printf "Term extracted: %s\n" name; *)
		    pushSym name (getVar t symtbl);
		  )
	    | Tuple( itms ) -> List.iter extractTerm itms
	    | _ -> ())

      | Let( vals ) -> 
	  List.iter (fun x -> let _, term = x in extractTerm term) vals
      | Apply( func, args ) -> List.iter extractTerm args


      | Lambda( a, body ) -> 
	  pushArgs a;
	  List.iter extractTerm body;
	  popArgs ();


      (* Args here? 
	 Well, if something in a pattern isn't recognized,
	 the pattern will bind it... which is sorta the point.
	 Hmmm.  We need...  a different way to handle pattern-vars.
	 Right now, it looks at the pattern and says "Oh, a var.
	 Is it bound?"  If it is, everything's cozy.  If not, it's like
	 "Fuck, this var doesn't exist.  DIE!"  Instead of saying
	 "Oh well, nevermind".
	 Basically, we need a new case for Value( Var( ... ) )
	 which knows that it's in a pattern, unbound vars aren't
	 an error.

	 Ooog, this is ugly.
      *)
      | Match( term, patterns ) -> 
	  let rec extractPattern p =
	    match p with
		(*		Value( m ) -> 
				(match m with *)
		Var( ns, name ) ->
		  (if isArg name then ()
		    else 

		      try pushSym name (getVar p symtbl)
		      with UnboundVar( _ ) -> pushArgs [name]
		  )
	      | Tuple( itms ) ->
		  let stuff = List.map 
		    (fun x -> match x with Value( x ) -> x 
		      | _ -> Null) itms in
		    List.iter extractPattern stuff 
	      | _ -> ()
	  in
	    extractTerm term;

	    List.iter 
	      (fun x -> 
		let pattern, bodies = x in 
		  extractPattern pattern;
		  List.iter extractTerm bodies;)		  
	      patterns

	      
      | If( cond, i, e ) -> extractTerm cond;
	  List.iter extractTerm i;
	  List.iter extractTerm e;
	  (*      | TableGet( t, k ) -> extractTerm t; extractTerm k; *)

      | Comment( _ ) -> ()

      | NamespaceChange( _ ) -> ()
    in
      List.iter extractTerm funbody;
      !env


and eval tree tbl =
  match tree with
      Value( t ) -> 
	(match t with
	    Var( _ ) ->
	      getVar t tbl
	  | Tuple( lst ) ->
	      let res = List.map (fun x -> Value( eval x tbl )) lst in
		Tuple( res )
	  | Array( arr ) -> 
	      let res = Array.map (fun x -> Value( eval x tbl )) arr in
		Array( res )
	  | _ -> t)


    | Let( vals ) ->
	let rec loop x =
	  let (namespace, name), value = x in
	  let result = eval value tbl in
	    setVar namespace name result tbl
	in
	  List.iter loop vals;
	  Null



    | Apply( func, arglst ) ->
	let f = eval func tbl
	and a = List.map (fun x -> eval x tbl) arglst in
	  apply f a tbl

    | Lambda( args, body ) ->
	let env = extractClosure args body tbl in
	  (*	  print_endline "Schloooorp!"; *)
	  Func( args, body, env )

    | Match( expr, cases ) ->
	matchCases expr cases tbl

    | If( cond, ifpart, elsepart ) ->
	let c = eval cond tbl in
	  if c = Symbol( "true" ) then
	    evalList ifpart tbl
	  else if c = Symbol( "false" ) then
	    evalList elsepart tbl
	  else
	    raise (TypeError( "If's only take 'true and 'false!" ))

    | NamespaceChange( newns ) -> 
	(try
	  setNamespace tbl newns;
	  Null
	with 
	    (* Namespace doesn't exist, make it *)
	    NamespaceError( _ ) -> 
	      addNamespace tbl newns; setNamespace tbl newns; Null)


    | Comment( _ ) -> Null


(* Ah!  It's recursive!
   First, make symbols and variables and such happen.
   Then, for pattern matching:
   If lhs is a variable, then it matches rhs
   If lhs is a constant, and it's = to rhs, it matches rhs
   If lhs is a tuple and rhs is a tuple, decompose both
   and match recursively across them.
   W00t!

   Match statement = new scope
*)

and matchCases cond matchlst tbl =
  let valToMatch = eval cond tbl in
  let rec loop fst rest =
    let pattern, body = fst in

      (* We wanna know if a pattern matches, but we don't want to
	 partially-bind vars if it doesn't.  So, we throw efficiency
	 out the window.
	 This has the kinda-nice side-effect that each body match
	 statement has its own scope.  Wish we didn't have to throw
	 away however many scopes beforehand to get it.
      *)
      pushScope tbl;
      if matchValue pattern valToMatch tbl then
	evalList body tbl
      else if rest = [] then (
	  popScope tbl;
	  raise (MatchError( "No match!  Bad programmer!" ))
	)
      else (
	  popScope tbl;
	  loop (List.hd rest) (List.tl rest)
	)

  in
    loop (List.hd matchlst) (List.tl matchlst)

      
      

and doMatch pattern term tbl =
  match pattern with
      Value( x ) ->
	let value = eval term tbl in 
	  matchValue x value tbl
    | _ -> raise (TypeError( "Match pattern not given a value!" ))

and matchValue pattern value tbl =

  match pattern with
      (* If it's an unbound var, bind it. *)
      Var( ns, s ) -> (* Printf.printf "Matching var %s\n" s;*)
	(try 
	    matchValue (getVar pattern tbl) value tbl;
	  (*	    matchValue (getVar pattern tbl) (getVar value tbl) tbl;*)
          with
	      UnboundVar( _ ) -> setVar ns s value tbl; 
                true
	)


    | Tuple( _ ) ->
	matchTuple pattern value tbl

    (* A literal *)
    | _ -> (value = pattern)

and matchTuple lval rval tbl =
  match lval with
      Tuple( itms ) ->
	(match rval with
	    Tuple( ritems ) ->
	      
	      if (List.length itms) = (List.length ritems) then (
                  try
                    let res = List.map2 (fun x y -> doMatch x y tbl) itms ritems in
		      List.fold_left (fun x y -> x && y) true res
                  with
		      Invalid_argument( _ ) -> raise (MatchError( "Tuples are different lengths!" ))
		    | _ -> raise (MatchError( "Match was expecting a tuple"))
		)
	      else
		false

	  | _ -> raise (MatchError( "Match was expecting a tuple"))
	)
    | _ -> raise (Fubar( "The impossible happened." ))


and apply f args tbl =
  match f with
      Func( formals, body, env ) ->
	(try
(*	    Printf.printf "Scope is %d\n" (List.length tbl.stack); *)
	    pushScope tbl;

	  (* Add args *)
(*	  print_endline "Adding args..."; *)
	  List.iter2 (fun x y -> setVar currentNamespace x y tbl) formals args;
	  (* Add closure environment *)
(*	  print_endline "Adding closure..."; *)
	  List.iter 
	    (fun x -> let sym, vl = x in setVar currentNamespace sym vl tbl) env;

(*	  print_endline "Evaluating body..."; *)
	  let r = evalList body tbl in
	    popScope tbl;
	    r
	  with 
	      Invalid_argument( "List.iter2" ) -> 
		popScope tbl;
		let str = Printf.sprintf 
		  "Invalid number of args, expected %d got %d\n" 
		  (List.length formals) (List.length args) in
		  raise (MatchError( str )))

    | Builtin( func ) ->
	func args
    | _ -> raise (TypeError( "apply: Function required!" ))




and evalList lst stbl =
  let lst = (List.rev (List.map (fun x -> eval x stbl) lst)) in
    if lst = [] then
      Null
    else
      List.hd lst
;;


*)
