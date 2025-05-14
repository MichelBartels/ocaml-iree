# IREE-bindings
To install this library, you first need to build IREE.

To build IREE, follow the instruction [on their website](https://iree.dev/building-from-source/getting-started/).
Afterwards, search for the files `libcpuinfo.a`, `libflatcc_parsing.a`,  `libIREECompiler.so` and `libiree_runtime_unified.a` in the directory where you have built IREE. Copy the files to a folder and make the enviroment variable `PJRT_PATH` point to that path.

Then, you can install this library using the following command:

``` shell
opam pin add iree_bindings https://github.com/MichelBartels/ocaml-iree.git

```
