module Make (M : sig
  val kind : Device.kind
end) = struct
  let device = Device.make M.kind

  type program = Function.t

  type buffer = Buffer.t

  let compile ?path source =
    let compiled_model = Compile.get_compiled_model ?path (Device.device_str device.kind) source in
    Function.make device compiled_model "main"

  let tensor_to_buffer t =
    let buffer = Buffer.of_tensor device t in
    Buffer.move device t.shape t.kind buffer

  let execute program ~num_outputs buffers =
    Function.call program buffers num_outputs
    
  let buffer_to_tensor ~shape kind buffer =
    Buffer.to_tensor device shape kind buffer

  let identifier = "IREE"

  let collect_buffer = Buffer.collect
end

let make kind = (module Make (struct
  let kind = kind
end) : Device_api.S)

