(lang dune 3.17)

(using ctypes 0.3)

(name iree_bindings)

(generate_opam_files true)

(source
 (github MichelBartels/ocaml-iree))

(authors "Michel Bartels")

(maintainers "Michel Bartels")

(package
 (name iree_bindings)
 (depends ocaml ctypes device_api)
 )
