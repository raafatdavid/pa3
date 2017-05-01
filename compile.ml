open Expr
open Instruction
open Printf

type 'a envt = (string * 'a) list

let count         = ref 0
let gen_temp base =
  count := !count + 1;
  sprintf "temp_%s_%d" base !count

let rec find ls x =
  match ls with
    | []          -> None
    | (y,v)::rest -> if y = x then Some(v) else find rest x

let const_true  = HexConst(0xffffffff)
let const_false = HexConst(0x7fffffff)

let throw_err code = 
  [
    IPush(Sized(DWORD_PTR, Const(code)));
    ICall("error");
  ]

let check_overflow = IJo("overflow_check")
let error_non_int  = "error_non_int"
let error_non_bool = "error_non_bool"

let check_num =
  [
    IAnd(Reg(EAX), Const(0x00000001));
    ICmp(Reg(EAX), Const(0x00000000));
  ]

let max n m = if n > m then n else m

let check_eax_num =
  check_num @ [ IJne(error_non_int) ]

let stackloc i      = RegOffset (-4 * i, ESP)  
let save_to_stack i = [ IMov (stackloc i, Reg(EAX)) ]
let restore_stack i = [ IMov (Reg(EAX), stackloc i) ]

let rec compile_prim1 (o : prim1) (e : expr) (si : int) (env : int envt) : instruction list =
  let prelude = compile_expr e si env in
  let op_is   = match o with
    | Add1   -> [ IAdd(Reg(EAX), Const(2)) ]
    | Sub1   -> [ IAdd(Reg(EAX), Const(-2)) ]
    | IsNum  -> [ IAnd(Reg(EAX), Const(0x00000001)) 
                ; IShl(Reg(EAX), Const(31))
                ; IXor(Reg(EAX), Const(0xFFFFFFFF))
                ]
    | IsBool -> [ IAnd(Reg(EAX), Const(0x00000001))
                ; IShl(Reg(EAX), Const(31))
                ; IOr(Reg(EAX), Const(0x7FFFFFFF))
                ]
    | Print  -> failwith "TODO: Print"

  in prelude @ op_is

and compile_prim2 (o : prim2) (e1 : expr) (e2 : expr) (si : int) (env : int envt) : instruction list =
  let rhs_loc = stackloc (si+1) in
  let e1_is   = compile_expr e1 si env     @ save_to_stack si     @ check_eax_num in
  let e2_is   = compile_expr e2 (si+1) env @ save_to_stack (si+1) @ check_eax_num in
  let op_is   = match o with
    | Plus    -> [ IAdd (Reg(EAX), rhs_loc); check_overflow ]
    | Minus   -> [ ISub (Reg(EAX), rhs_loc); check_overflow ]
    | Times   -> [ ISar (Reg(EAX), Const(1))
                 ; IMul (Reg(EAX), rhs_loc)
                 ; check_overflow
                 ]
    | Less    -> [ ISub (Reg(EAX), rhs_loc)
                 ; ISub (Reg(EAX), Const(1))
                 ; IAnd(Reg(EAX), HexConst(0x80000000))
                 ; IOr( Reg(EAX), HexConst(0x7FFFFFFF))
                 ]
    | Greater -> [ ISub(Reg(EAX), rhs_loc)
                 ; IAnd(Reg(EAX), HexConst(0x80000000))
                 ; IXor(Reg(EAX), HexConst(0xFFFFFFFF))
                 ]
    | Equal   -> let leave_false = gen_temp "equals" in
                 [ ICmp (Reg(EAX), rhs_loc)
                 ; IMov (Reg(EAX), const_false)
                 ; IJne (leave_false)
                 ; IMov (Reg(EAX), const_true)
                 ; ILabel(leave_false)
                 ]
  in     e1_is
       @ e2_is
       @ restore_stack si
       @ op_is

and compile_let (bs : (string *expr) list) (body : expr) (si : int) (env : int envt) : instruction list =
  match bs with
  | []         -> compile_expr body si env
  | (x,e)::bs' ->
     let e_is   = compile_expr e si env in
       e_is
     @ save_to_stack si
     @ compile_let bs' body (si+1) ((x,si)::env) 

and compile_if (cond : expr) (e_then : expr) (e_else : expr) (si : int) (env : int envt) : instruction list =
  let cond_is    = compile_expr cond si env   in
  let then_is    = compile_expr e_then si env in
  let else_is    = compile_expr e_else si env in
  let label_then = gen_temp "then" in
  let label_else = gen_temp "else" in
  let label_end  = gen_temp "end"  in
    cond_is
  @ [ ICmp(Reg(EAX), const_true);
      IJe(label_then);
      ICmp(Reg(EAX), const_false);
      IJe(label_else);
      IJmp(error_non_bool);
      ILabel(label_then)
    ]
  @ then_is
  @ [ IJmp(label_end); ILabel(label_else) ]
  @ else_is
  @ [ ILabel(label_end) ]

and compile_app (f : string) (args : expr list) (si : int) (env : int envt) : instruction list 
  = failwith "TODO: compile_app"
  
and compile_expr (e : expr) (si : int) (env : int envt) : instruction list = 
  match e with
  | ENumber (n ) -> [ IMov (Reg(EAX), Const((n lsl 1))) ]
  | EBool   (b ) -> [ IMov (Reg(EAX), if b then const_true else const_false) ]
  | EId     (rx) ->
     let arg = begin match find env rx with
               | Some(i) -> RegOffset(-4 * i, ESP)
               | None    -> failwith (sprintf "Unbound identifier: %s" rx)
               end
     in [ IMov (Reg(EAX), arg) ]
  | EPrim1 (op, e')          -> compile_prim1 op e' si env
  | EPrim2 (op, e1, e2)      -> compile_prim2 op e1 e2 si env
  | ELet   (bs, body)        -> compile_let bs body si env
  | EIf    (c, ethen, eelse) -> compile_if c ethen eelse si env
  | EApp   (f, args)         -> compile_app f args si env

let compile_decl (d : decl) : instruction list = failwith "TODO: compile_decl"

let rec find_one (l : 'a list) (elt : 'a) : bool =
  match l with
    | []    -> false
    | x::xs -> (elt = x) || (find_one xs elt)

let rec find_dup (l : 'a list) : 'a option =
  match l with
    | []    -> None
    | [x]   -> None
    | x::xs ->
      if find_one xs x then Some(x) else find_dup xs

let rec well_formed_e (e : expr) (ds : decl list) (env : bool envt) =
  match e with
    | ENumber(_)
    | EBool(_) -> []
    | EId(x)   ->
      begin match find env x with
        | None    -> ["Unbound identifier: " ^ x]
        | Some(_) -> []
      end
    | EPrim1(op, e)           ->
       well_formed_e e ds env
    | EPrim2(op, left, right) ->
      (well_formed_e left ds env) @ 
      (well_formed_e right ds env)
    | EIf(cond, thn, els)     ->
      (well_formed_e cond ds env) @
      (well_formed_e thn ds env ) @
      (well_formed_e els ds env )
    | EApp(name, args)        -> failwith "TODO: well_formed_e EApp"
    | ELet(binds, body)       ->
      let names          = List.map fst binds                           in
      let env_from_binds = List.map (fun a -> (a, true)) names          in
      let from_body      = well_formed_e body ds (env_from_binds @ env) in
      begin match find_dup names with
        | None       -> from_body
        | Some(name) -> ("Duplicate name in let: " ^ name)::from_body
      end

let well_formed_d (d : decl) (ds : decl list) : string list =
  failwith "TODO: well_formed_d"

let well_formed_p (p : program) : string list =
  match p with
    | Program(ds, maine) ->
      let names        = List.map (fun (DFun(name, _, _)) -> name) ds in
      let subexpr_errs = 
        (well_formed_e maine ds []) @
        (List.flatten (List.map (fun d -> well_formed_d d ds) ds))    in
      begin match find_dup names with
        | None    -> subexpr_errs
        | Some(v) -> ("Duplicate function definition: " ^ v)::subexpr_errs
      end

let compile_to_string prog =
  match well_formed_p prog with
    | x::rest ->
      let errstr = (List.fold_left (fun x y -> x ^ "\n" ^ y) "" (x::rest)) in
      failwith errstr
    | []      ->
      match prog with
        | Program(decls, main) ->
          let compiled_decls = List.map compile_decl decls in
          let compiled_main  = (compile_expr main 1 []) in
          let prelude        = "
section .text
extern error
extern print
global our_code_starts_here" in
          let main_start = [
            IComment("Our Code Begins Here!");
            ILabel("our_code_starts_here");
          ] in
          let postlude = [
            IRet
          ]
          @ [ILabel(error_non_int)]    @ (throw_err 1)
          @ [ILabel(error_non_bool)]   @ (throw_err 2)
          @ [ILabel("overflow_check")] @ (throw_err 3)
          in
          let as_assembly_string = (to_asm (
            (List.flatten compiled_decls) @
            main_start @
            compiled_main @
            postlude)) in
          sprintf "%s%s\n" prelude as_assembly_string

