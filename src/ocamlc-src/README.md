# Updating ocamlc-js

The actual version of ocamlc-js is 4.00

The compiler uses its own stdlib, which allows the compilation of nativeint
correctly. It means that the compiler must be compiled using this modify
stdlib and should use it to compile Ocaml sources. It is necessary due to
js_of_ocaml which cannot differentiate nativeints and ints (and also floats and
ints).

To update to a new version of OCaml :

1) Modify the Nativeint module (if it has changed since the previous version) :
   - Nativeint must only use a type t like this one : 
    
    ```ocaml
    type t = { i : nativeint } 
    ```
   - Those values can only be created by using a function :

   ```ocaml
   (* val of_nativeint : nativeint -> t *)
   let of_nativeint i = 
       let v = { i } in
       Obj.set_tag (Obj.repr v) 245;
       v
   ```

   This tagged value will be correctly recognized by caml_output_value and so
   correctly marshalled (the tag 245 is important, another value and marshalling
   won't work at all).

   - Every function from Nativeint should use type t instead of nativeint, they
     should be overwritten and use the old function. For example :
     
     ```ocaml
     external neg: nativeint -> nativeint = "%nativeint_neg"

     let neg n = of_nativeint (neg n.i)
     ```

2) Modify also the module Int64 to use Nativeint.t instead of nativeint

3) Modify the type constant in asttypes.mli (in parsing) : 

```ocaml
(* replace : *) 
| Const_nativeint of nativeint
(* by : *) 
| Const_nativeint of Nativeint.t 
```

4) Modify then every part of the compiler to use your modified constant (don't
worry, the typer will tell you where when you'll try to compile)


Normally, that should be OK, except if there has been any change in runtime,
meaning that ocp-runtime.js has to be updated as well (marshal_data_size
if the marshalling magic number has changed).
If floats primitives has to be rewritten, don't forget that it should be
encapsulated in an array, whose first value is 253 (or Obj.double_tag if it has
changed, but then you should modify caml_output_value to recognize this new
value instead of 253) and the second the float itself. It allows the compiler to
differentiate floats and ints (another limitation from js_of_ocaml).

If you just replace the sources by the updated one and don't touch the .ocp or
makefiles, it should work correctly. In case of any new file in the compiler
sources, just update the .ocp, and for stdlib just add the .cmo in the dir's

Don't forget to use the main.ml provided, the functions and types in it are
essential to communicate with the calling program. 


# Ocamlc side-effects

To prevent side-effects problem, you must add the functions called by
*Bytecomp_common.reset_bytecomp ()*. Just copy/paste them in the new bytecomp sources. 

# Auxiliary programs


progmagic.ml : this little program takes a cm[iao] or bytecode file (actually
any file, but it is particularely useful for binary files) and writes every byte
in its decimal representation, to avoid any encoding issue. It can be used to
"read" bytecode, and its result will be in "progmagic_out". 
However, if you use it on a file with the *-js* option, it will make a .js file
containing a variable named filename_ext which is a string containing your
file with every binary value encoded into its decimal representation. This way,
it can be used by the compiler to add a new .cm[iao] by default, you just need
to link filename_ext.js at js_of_ocaml during ocamlc.byte compilation (in
the Makefile).
To decode it in javascript, simply use *read_encoded_binary_file* from
ocp-filesystem.js. Since there is a "Provides" field in the .js generated, you
can ensure it is available at js compilation using the "Requires" field in your
javascript runtime.
It is primary used to add the modified stdlib.cma to the compiler filesystem. 
It can also prints values as hexa values with the *-hex* option.

diff.ml : Takes two files (encoded by progmagic without the -js option) and
write their differences into two files named filename.ext_diff. A diff is
actually a new line followed by the byte which is different in each file. 
