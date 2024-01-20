open Base

type 'a t = {
    mutable i: int;
    data: 'a array;
    len: int;
}

let length ({len;_}: 'a t) : int = len 

let inc (x: 'a t) : unit =
    let {i;len;_} = x in
    let i' = (i + 1) % len in
    x.i <- i'

let peek (x: 'a t) : 'a =
    let {i;data;_} = x in
    data.(i)

let read_and_shift (x: 'a t) : 'a =
    let v = peek x in
    inc x;
    v

let of_list (l: 'a list) : 'a t =
    let data = Array.of_list l in
    let len = Array.length data in
    let i = 0 in
    {i;data;len}
