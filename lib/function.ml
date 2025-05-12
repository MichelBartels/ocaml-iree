open C.Functions
open C.Type
open Ctypes
open C_utils

type t =
  {call: call structure; device: Device.t}

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

let make device file_name function_name =
  let session = device.Device.session in
  assert_no_error
  @@ session_append_bytecode_module_from_file session file_name ;
  let call = call session @@ "module." ^ function_name in
  {call; device}

let pop_output call =
  create_out_param (ptr buffer_view)
  @@ call_outputs_pop_front_buffer_view @@ addr call

let push_input t buffer = 
  assert_no_error
  @@ call_inputs_push_back_buffer_view (addr t.call) buffer.Buffer.view

let push_inputs t buffers = List.iter (push_input t) buffers

let collect_inputs = List.iter Buffer.collect

let pop_output t =
  let buffer = pop_output t.call in
  Buffer.make t.device buffer

let pop_outputs t n =
  List.init n (fun _ -> pop_output t)

let call t buffers num_outputs =
  push_inputs t buffers ;
  assert_no_error @@ call_invoke (addr t.call) (Unsigned.UInt32.of_int 0) ;
  collect_inputs buffers ;
  let outputs = pop_outputs t num_outputs in
  call_reset (addr t.call) ;
  outputs