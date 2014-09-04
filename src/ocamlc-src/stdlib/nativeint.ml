
external neg: nativeint -> nativeint = "%nativeint_neg"
external add: nativeint -> nativeint -> nativeint = "%nativeint_add"
external sub: nativeint -> nativeint -> nativeint = "%nativeint_sub"
external mul: nativeint -> nativeint -> nativeint = "%nativeint_mul"
external div: nativeint -> nativeint -> nativeint = "%nativeint_div"
external rem: nativeint -> nativeint -> nativeint = "%nativeint_mod"
external logand: nativeint -> nativeint -> nativeint = "%nativeint_and"
external logor: nativeint -> nativeint -> nativeint = "%nativeint_or"
external logxor: nativeint -> nativeint -> nativeint = "%nativeint_xor"
external shift_left: nativeint -> int -> nativeint = "%nativeint_lsl"
external shift_right: nativeint -> int -> nativeint = "%nativeint_asr"
external shift_right_logical: nativeint -> int -> nativeint = "%nativeint_lsr"
external of_int: int -> nativeint = "%nativeint_of_int"
external to_int: nativeint -> int = "%nativeint_to_int"
external of_float : float -> nativeint = "caml_nativeint_of_float"
external to_float : nativeint -> float = "caml_nativeint_to_float"
external of_int32: int32 -> nativeint = "%nativeint_of_int32"
external to_int32: nativeint -> int32 = "%nativeint_to_int32"


type t = { i : nativeint }

let of_nativeint i =
  let t = { i } in
  Obj.set_tag (Obj.repr t) 245;
  t

let neg n = of_nativeint (neg n.i)
let add n1 n2 = of_nativeint (add n1.i n2.i)
let sub n1 n2 = of_nativeint (sub n1.i n2.i)
let mul n1 n2 = of_nativeint (mul n1.i n2.i)
let div n1 n2 = of_nativeint (div n1.i n2.i)
let logand n1 n2 = of_nativeint (rem n1.i n2.i)
let logor n1 n2 = of_nativeint (logor n1.i n2.i)
let logxor n1 n2 = of_nativeint (logxor n1.i n2.i)
let shift_left n1 n2 = of_nativeint (shift_left n1.i n2)
let shift_right n1 n2 = of_nativeint (shift_right n1.i n2)
let shift_right_logical n1 n2 = of_nativeint (shift_right_logical n1.i n2)
let of_int n = of_nativeint (of_int n)
let to_int n = to_int n.i
let of_float n = of_nativeint (of_float n)
let to_float n = to_float n.i
let of_int32 n = of_nativeint (of_int32 n)
let to_int32 n = to_int32 n.i


let zero = of_nativeint 0n
let one = of_nativeint 1n
let minus_one = of_nativeint (-1n)
let succ n = add n (of_nativeint 1n)
let pred n = sub n (of_nativeint 1n)
let abs n = if n.i >= 0n then n else neg n
let size = Sys.word_size
let min_int = shift_left (of_nativeint 1n) (size - 1)
let max_int = sub min_int (of_nativeint 1n)
let lognot n = logxor n (of_nativeint (-1n))


external format : string -> nativeint -> string = "caml_nativeint_format"
let to_string n = format "%d" n

let format s n =
  format s n.i

external of_string: string -> nativeint = "caml_nativeint_of_string"

let of_string s =
  of_nativeint (of_string s)

let compare (x: t) (y: t) = Pervasives.compare x.i y.i
