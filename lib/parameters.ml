type ('a, 'b, 'c) inner =
  { output_type: 'c Ir.ValueType.t
  ; old_params: 'b Ir.VarList.t Ir.Var.t -> 'a Ir.VarList.t Ir.Var.t
  ; initial_values: ('b Ir.VarList.t, Runtime.Value.host) Runtime.Value.t
  ; f: 'b Ir.VarList.t Ir.Var.t -> 'c Ir.Var.t }

type ('a, 'b, 'c) t =
  ('a Ir.VarList.t, Runtime.Value.host) Runtime.Value.t -> ('a, 'b, 'c) inner

let return : type a b. a Ir.Var.t -> (b, b, a) t =
 fun x initial_values ->
  { old_params= Fun.id
  ; output_type= Ir.ValueType.of_var x
  ; initial_values
  ; f= (fun _ -> x) }

let bind :
    type a b c d e. (a, b, c) t -> (c Ir.Var.t -> (b, d, e) t) -> (a, d, e) t =
 fun x f initial_values ->
  let dummy_output = f (Ir.ValueType.to_arg (x initial_values).output_type) in
  let dummy_inner = dummy_output (x initial_values).initial_values in
  { dummy_inner with
    f=
      (fun z ->
        let b_param = dummy_inner.old_params z in
        let x_inner = x initial_values in
        let c = x_inner.f b_param in
        let output = f c in
        let output_inner = output x_inner.initial_values in
        output_inner.f z )
  ; old_params=
      (fun z ->
        let b_param = dummy_inner.old_params z in
        (x initial_values).old_params b_param ) }

let ( let* ) = bind

let new_param :
    type a b.
    (a, Runtime.Value.host) Runtime.Value.t -> (b, a Ir.Var.t -> b, a) t =
 fun t xs ->
  { initial_values= t :: xs
  ; old_params= (fun (_ :: xs) -> xs)
  ; output_type= Runtime.Value.value_type t
  ; f= (fun (x :: _) -> x) }

let apply : type a b. (unit, a, b) t -> a Ir.VarList.t Ir.Var.t -> b Ir.Var.t =
 fun x -> (x []).f

let initial :
    type a b c.
       a Ir.ValueType.t
    -> (a Ir.Var.t -> (unit, b, c) t)
    -> (b Ir.VarList.t, Runtime.Value.host) Runtime.Value.t =
 fun t f ->
  let dummy_x = Ir.ValueType.to_arg t in
  let dummy_inner = Random.dummy_handler (fun () -> f dummy_x []) in
  dummy_inner.initial_values

let param_type :
    type a b c.
       a Ir.ValueType.t
    -> (a Ir.Var.t -> (unit, b, c) t)
    -> b Ir.VarList.t Ir.ValueType.t =
 fun t f -> initial t f |> Runtime.Value.value_type

let grad_and_value :
    type a b c.
       (a, b, c) t
    -> (a, b, (b Ir.VarList.t Ir.Var.t -> c Ir.Var.t -> unit) Ir.VarList.t) t =
 fun x initial_values ->
  let x = x initial_values in
  { x with
    output_type=
      List_type [Runtime.Value.value_type x.initial_values; x.output_type]
  ; f= Backpropagate.diff Var x.f }

let params : type a. (a, a, a Ir.VarList.t) t =
 fun initial_values ->
  { initial_values
  ; old_params= Fun.id
  ; output_type= Runtime.Value.value_type initial_values
  ; f= Fun.id }

let create_func t f =
  let param_type = param_type t f in
  let input_types = Ir.ValueType.List_type [param_type; t; Random.seed_type] in
  Ir.create_func input_types (fun [params; x; seed] ->
      Random.handler
        (fun () ->
          let y = apply (f x) params in
          let seed = Random.current_seed () in
          Ir.Var.[y; seed] )
        seed )
