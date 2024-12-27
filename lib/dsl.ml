let fn = Ir.create_func

module Var = Ir.Var

let matmul a b =
  let a_shape = Ir.shape_of_var a in
  let b_shape = Ir.shape_of_var b in
  let rec prefix = function
    | [_; _] ->
        []
    | x :: xs ->
        x :: prefix xs
    | [] ->
        failwith "matmul requires at least 2 dims"
  in
  let rec common_prefix a b =
    match (a, b) with
    | [], _ | _, [] ->
        []
    | a :: a_tl, b :: b_tl when a = b ->
        a :: common_prefix a_tl b_tl
    | _ ->
        []
  in
  let batching_dims =
    List.init
      (List.length (common_prefix (prefix a_shape) (prefix b_shape)))
      Fun.id
  in
  Ir.Var.DotProduct
    ( a
    , b
    , [List.length a_shape - 1]
    , [List.length b_shape - 2]
    , batching_dims
    , batching_dims )

let ( +@ ) a b = Var.Add (a, b)

let ( -@ ) a b = Var.Subtract (a, b)

let ( *@ ) a b = Var.Multiply (a, b)

let ( /@ ) a b = Var.Divide (a, b)

let ( <<@ ) a b = Var.LeftShift (a, b)

let ( >>@ ) a b = Var.RightShift (a, b)

let ( |@ ) a b = Var.Or (a, b)

let exp a = Var.Exponential a

let pow a b = Var.Pow (a, b)

let ( **@ ) = pow

let abs a = Var.Abs a

let ln a = Var.Ln a

let compare dir a b = Var.Compare (a, dir, b)

let min a b = Var.Min (a, b)

let max a b = Var.Max (a, b)

let ( =@ ) a = compare Ir.Eq a

let ( <>@ ) a = compare Ir.Ne a

let ( >=@ ) a = compare Ir.Ge a

let ( >@ ) a = compare Ir.Gt a

let ( <=@ ) a = compare Ir.Le a

let ( <@ ) a = compare Ir.Lt a

let broadcast_scalar op shape = Ir.Var.BroadcastInDim (op, shape)

let broadcast_scalar_like op var = broadcast_scalar op (Ir.shape_of_var var)

let full value shape =
  Ir.Var.BroadcastInDim (Ir.Tensor.full value [] |> Ir.Tensor.to_ir, shape)

let full_f32 value = full (F32 value)

let full_i1 value = full (I1 value)

let full_like value var = Ir.shape_of_var var |> full value

let var_float_op op a b = op a (full_like (F32 b) a)

let float_var_op op a b = op (full_like (F32 a) b) b

let var_u64_op op a b = op a (full_like (U64 b) a)

let u64_var_op op a b = op (full_like (U64 a) b) b

let ( +.> ) = var_float_op ( +@ )

let ( -.> ) = var_float_op ( -@ )

let ( *.> ) = var_float_op ( *@ )

let ( /.> ) = var_float_op ( /@ )

let ( **.> ) = var_float_op ( **@ )

let ( =.> ) = var_float_op ( =@ )

let ( <>.> ) = var_float_op ( <>@ )

let ( >=.> ) = var_float_op ( >=@ )

let ( >.> ) = var_float_op ( >@ )

let ( <=.> ) = var_float_op ( <=@ )

let ( <.> ) = var_float_op ( <@ )

let ( <<.> ) = var_u64_op ( <<@ )

let ( >>.> ) = var_u64_op ( >>@ )

let ( |.> ) = var_u64_op ( |@ )

let ( +.< ) = float_var_op ( +@ )

let ( -.< ) = float_var_op ( -@ )

let ( *.< ) = float_var_op ( *@ )

let ( /.< ) = float_var_op ( /@ )

let ( **.< ) = float_var_op ( **@ )

let ( =.< ) = float_var_op ( =@ )

let ( <>.< ) = float_var_op ( <>@ )

let ( >=.< ) = float_var_op ( >=@ )

let ( >.< ) = float_var_op ( >@ )

let ( <=.< ) = float_var_op ( <=@ )

let ( <.< ) = float_var_op ( <@ )

let ( <<.< ) = u64_var_op ( <<@ )

let ( >>.< ) = u64_var_op ( >>@ )

let ( |.< ) = u64_var_op ( |@ )

let sqrt a = a **.> 0.5

let tanh a = Var.Tanh a

let ones : type a. a Ir.tensor Ir.ValueType.t -> a Ir.tensor Ir.Var.t = function
  | Tensor_type (shape, F32) ->
      full (F32 1.) shape
  | Tensor_type (shape, F64) ->
      full (F64 1.) shape
  | Tensor_type (shape, I1) ->
      full (I1 false) shape
  | Tensor_type (shape, I64) ->
      full (I64 1) shape
  | Tensor_type (shape, U32) ->
      full (U32 "1") shape
  | Tensor_type (shape, U64) ->
      full (U64 "1") shape

let ones_like t = ones (Ir.ValueType.of_var t)

let zeros : type a. a Ir.tensor Ir.ValueType.t -> a Ir.tensor Ir.Var.t =
  function
  | Tensor_type (shape, F32) ->
      full (F32 0.) shape
  | Tensor_type (shape, F64) ->
      full (F64 0.) shape
  | Tensor_type (shape, I1) ->
      full (I1 false) shape
  | Tensor_type (shape, I64) ->
      full (I64 0) shape
  | Tensor_type (shape, U32) ->
      full (U32 "0") shape
  | Tensor_type (shape, U64) ->
      full (U64 "0") shape

let zeros_like t = zeros (Ir.ValueType.of_var t)

let norm mean stddev shape =
  Var.Random
    ( Ir.ValueType.Tensor_type (shape, F32)
    , mean
    , stddev
    , Ir.Tensor.from_int_list shape |> Ir.Tensor.to_ir
    , Normal )

let uniform low high shape =
  Var.Random
    ( Ir.ValueType.Tensor_type (shape, F32)
    , low
    , high
    , Ir.Tensor.from_int_list shape |> Ir.Tensor.to_ir
    , Uniform )

let sum axes x = Var.Sum (x, axes)

let mean axes x =
  let (Tensor_type (shape, _)) = Ir.ValueType.of_var x in
  let size =
    List.filteri (fun i _ -> List.mem i axes) shape |> List.fold_left ( * ) 1
  in
  sum axes x /.> float_of_int size

let transpose var permutation =
  let shape = Ir.shape_of_var var in
  if Stdlib.(List.length permutation <> List.length shape) then
    failwith "Permutation length must match tensor rank" ;
  if
    not
      Stdlib.(
        List.sort compare permutation = List.init (List.length shape) Fun.id )
  then failwith "Invalid permutation" ;
  Var.Transpose (var, permutation)

let scalar_f32 = Fun.compose Ir.Tensor.to_ir Ir.Tensor.scalar_f32

let scalar_u64 = Fun.compose Ir.Tensor.to_ir Ir.Tensor.scalar_u64

let assert_float_fn (f : Ir.f32 Ir.tensor Ir.Var.t -> Ir.f32 Ir.tensor Ir.Var.t)
    : Ir.Var.map_fn =
  let fn : type a. a Ir.tensor Ir.Var.t -> a Ir.tensor Ir.Var.t =
   fun x ->
    match Ir.ValueType.of_var x with
    | Ir.ValueType.Tensor_type (_, F32) ->
        f x
    | _ ->
        failwith "assert_float_map: unsupported type"
  in
  {fn}

let assert_float2_fn
    (f :
         Ir.f32 Ir.tensor Ir.Var.t
      -> Ir.f32 Ir.tensor Ir.Var.t
      -> Ir.f32 Ir.tensor Ir.Var.t ) : Ir.Var.map2_fn =
  let fn :
      type a.
      a Ir.tensor Ir.Var.t -> a Ir.tensor Ir.Var.t -> a Ir.tensor Ir.Var.t =
   fun x y ->
    match Ir.ValueType.of_var x with
    | Ir.ValueType.Tensor_type (_, F32) ->
        f x y
    | _ ->
        failwith "assert_float_map: unsupported type"
  in
  {fn}

let float_map f = Ir.Var.map (assert_float_fn f)

let float_map2 f = Ir.Var.map2 (assert_float2_fn f)

let bitcast dtype var = Var.Bitcast (var, dtype)

let convert dtype var = Var.Convert (var, dtype)

let iota n var = Var.Iota (n, var)

let reshape shape var = Var.Reshape (var, shape)

let no_grad x = Ir.Var.NoGrad x

let sin x = Var.Sin x

let cos x = Var.Cos x
