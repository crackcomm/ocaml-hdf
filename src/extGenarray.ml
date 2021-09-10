open Batteries

(** {5 ExtGenarray} *)

open Bigarray.Genarray

(** This module extends the Bigarray module somewhat. *)

(** [elems b] will return the number of elements in the whole Bigarray. *)
let elems ba = Array.reduce ( * ) (dims ba)

(** [fold f accum b]
    Like Array.fold_left and List.fold_left, but over an entire bigarray.
    Note: The Bigarray is flattened, so this function works over all of
    the elements, regardless of the Bigarray dimensions. *)
let fold f initial b =
  let new_dim = elems b in
  let b' = Bigarray.reshape b [| new_dim |] in
  let accum = ref initial in
  for i = 0 to new_dim - 1 do
    accum := f !accum (get b' [|i|])
  done;
  !accum

(** [find f b]
    This is just a wrapper around [fold], where [accum] is taken the be the
    first element in the (flattened) Bigarray.
    Purely for convenience: [find max b] to get the maximum element in [b]. *)
let find f b =
  let b_0 = get (Bigarray.reshape b [| elems b |]) [|0|] in
  fold f b_0 b

external _c_cast : ('a, 'b, 'c) Bigarray.Genarray.t ->
  ('d, 'e, 'c) Bigarray.Genarray.t -> unit = "ml_ba_cast"

(** [cast kind ba] returns a Bigarray with the same memory content as [ba],
    but cast such that elements of of type [kind].  The contents of [ba] are
    copied, so there is no memory sharing between the new and old array.
    This can be used to, for example, cast a Bigarray of int8_unsigned
    elements to float32 elements.  The result would be a Bigarray with 1/4
    the elements of [ba]. *)
let cast kind ba =
  let new_size =
    elems ba
    * (Bigarray.Genarray.kind ba |> Bigarray.kind_size_in_bytes)
    / Bigarray.kind_size_in_bytes kind
  in
  let new_ba =
    Bigarray.Genarray.create kind (Bigarray.Genarray.layout ba) [|new_size|]
  in
  (* This does the actual copying *)
  _c_cast ba new_ba;
  new_ba

