open C.Functions
open C.Type
open Ctypes
open C_utils

type kind = Local_task | Cuda

type t =
  { device: device structure Ctypes_static.ptr
  ; allocator: device_allocator structure Ctypes_static.ptr
  ; instance: instance structure Ctypes_static.ptr
  ; session: runtime_session structure Ctypes_static.ptr
  ; kind: kind }

let instance_options () =
  create_out_param (Fun.const ()) instance_options instance_options_initialize

let session_options () =
  create_out_param (Fun.const ()) session_options session_options_initialize

let assert_no_error status =
  if not @@ status_ok status then (
    let msg_ptr = allocate_n (ptr char) ~count:1 in
    let len_ptr = allocate_n size_t ~count:1 in
    let allocator = allocator_system () in
    status_to_string status (addr allocator) msg_ptr len_ptr ;
    failwith @@ Ctypes_std_views.string_of_char_ptr !@msg_ptr )

let call session name =
  create_out_param assert_no_error call
  @@ call_initialize_by_name session
  @@ make_string_view name
  
let create_out_param c_type = create_out_param assert_no_error c_type


let instance options =
  protect instance_release
  @@ create_out_param (ptr instance)
  @@ instance_create (addr options)
  @@ allocator_system ()

let device instance device_str =
  protect device_release
  @@ create_out_param (ptr device)
  @@ instance_try_create_default_device instance (make_string_view device_str)

let session instance options device =
  protect session_release
  @@ create_out_param (ptr runtime_session)
  @@ session_create_with_device instance (addr options) device
  @@ instance_host_allocator instance

let device_str = function
  | Local_task ->
      "local-task"
  | Cuda ->
      "cuda"

let compile_device_str = function
  | Local_task ->
      "vmvx"
  | Cuda ->
      "cuda"

let make kind =
  let options = instance_options () in
  instance_options_use_all_available_drivers (addr options) ;
  let instance = instance options in
  let device = device instance @@ device_str kind in
  let session_options = session_options () in
  let session = session instance session_options device in
  let allocator = session_device_allocator session in
  {device; allocator; instance; session; kind}