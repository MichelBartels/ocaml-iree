open C.Type
open C.Functions
open Ctypes
open C_utils
open Device_api
type any_kind = Any : ('a, 'b) Tensor.kind -> any_kind

type any_tensor = Any : ('a, 'b) Device_api.Tensor.t -> any_tensor

type any_carray = Any : 'a CArray.t -> any_carray

type t = {
  view: buffer_view structure Ctypes_static.ptr
  ; device: Device.t }

let assert_no_error status =
  if not @@ status_ok status then (
    let msg_ptr = allocate_n (ptr char) ~count:1 in
    let len_ptr = allocate_n size_t ~count:1 in
    let allocator = allocator_system () in
    status_to_string status (addr allocator) msg_ptr len_ptr ;
    failwith @@ Ctypes_std_views.string_of_char_ptr !@msg_ptr )

let create_out_param c_type = create_out_param assert_no_error c_type

let buffer_params =
  let p = make buffer_params in
  setf p buffer_usage buffer_usage_default ;
  setf p buffer_access memory_access_all ;
  setf p buffer_memory_type memory_type_device_local ;
  p

let iree_element_type : type a b. (a, b) Device_api.Tensor.kind -> Unsigned.uint32 = function
  | F32 ->
      element_type_float_32
  | F64 ->
      element_type_float_64
  | I1 ->
      element_type_bool_8 (* TODO: need to check if this is correct *)
  | I64 ->
      element_type_int_64
  | U32 ->
      element_type_uint_32
  | U64 ->
      element_type_uint_64

let collect buffer =
  buffer_view_release buffer.view

let make device view =
  {view; device}

let of_carray device shape kind arr =
  let element_type = Tensor.ctype_of_kind kind in
  let data_ptr = CArray.start arr |> to_voidp in
  let size = List.fold_left ( * ) 1 shape in
  let shape_arr =
    List.map Unsigned.Size_t.of_int shape |> CArray.of_list size_t
  in
  let rank = Unsigned.Size_t.of_int (CArray.length shape_arr) in
  let shape_ptr = CArray.start shape_arr in
  let size = size * sizeof element_type |> Unsigned.Size_t.of_int in
  make device
  @@
        create_out_param (ptr buffer_view)
        @@ buffer_view_allocate_buffer_copy device.device
             device.allocator rank shape_ptr (iree_element_type kind)
             encoding_type_dense_row_major buffer_params
        @@ make_const_byte_span data_ptr size

let of_float_list device shape data =
  let arr = CArray.of_list float data in
  of_carray device shape F32 arr

let size shape = List.fold_left ( * ) 1 shape

let to_carray device shape kind buffer =
  let c_type = Tensor.ctype_of_kind kind in
  let data = size shape |> CArray.make c_type in
  let data_ptr = CArray.start data |> to_voidp in
  let size = CArray.length data * sizeof c_type |> Unsigned.Size_t.of_int in
  device_transfer_d2h device.Device.device
    (buffer_view_buffer buffer.view)
    (Unsigned.Size_t.of_int 0) data_ptr size transfer_buffer_flag_default
    (infinite_timeout ())
  |> assert_no_error ;
  data

let of_tensor : type a b. Device.t -> (a, b) Tensor.t -> t =
  fun device tensor ->
  let arr = tensor.data in
  let shape = tensor.shape in
  of_carray device shape (Device_api.Tensor.kind tensor) arr

let move device shape kind buffer =
  let arr = to_carray device shape kind buffer in
  of_carray device shape kind arr

let to_tensor device shape kind buffer =
  let arr = to_carray device shape kind buffer in
  Tensor.make kind shape arr
