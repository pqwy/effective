(**
 Direct transliteration of Oleg's code, including the intermetiate [view]
 type, which we do away with in the actual implementation.
*)

open Higher

type ('a, 'e) t =
  | Pure   : 'a -> ('a, 'e) t
  | Impure : ('b, 'e) app * ('b, 'a, 'e) arr -> ('a, 'e) t

and ('a, 'b, 'e) arr =
  | Fun  : ('a -> ('b, 'e) t) -> ('a, 'b, 'e) arr
  | Comp : ('a, 'b, 'e) arr * ('b, 'c, 'e) arr -> ('a, 'c, 'e) arr

type ('a, 'b, 'e) view =
  | VFun  : ('a -> ('b, 'e) t) -> ('a, 'b, 'e) view
  | VComp : ('a -> ('b, 'e) t) * ('b, 'c, 'e) arr -> ('a, 'c, 'e) view

let (|+) a f = Comp (a, Fun f)

let return x = Pure x

let rec view : type a b e. (a, b, e) arr -> (a, b, e) view = function
  | Fun f       -> VFun f
  | Comp (f, g) ->
      let rec split : type a b x e. (a, x, e) arr -> (x, b, e) arr -> (a, b, e) view =
        fun q1 q2 -> match q1 with
          | Fun f           -> VComp (f, q2)
          | Comp (q1a, q1b) -> split q1a (Comp (q1b, q2)) in
      split f g

let rec app : type a b e. (a, b, e) arr -> a -> (b, e) t =
  fun arr a -> match view arr with
  | VFun f       -> f a
  | VComp (f, g) ->
      match f a with
      | Pure x          -> app g x
      | Impure (eff, k) -> Impure (eff, Comp (k, g))

let after g h = Fun (fun x -> h (app g x))

let (>>=) a fb = match a with
  | Pure x          -> fb x
  | Impure (eff, k) -> Impure (eff, k |+ fb)

let (>|=) a f = a >>= fun x -> return (f x)

let aret = Fun (fun x -> Pure x)
let req eff = Impure (eff, aret)
