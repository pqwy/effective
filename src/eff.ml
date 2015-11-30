open Higher

type ('a, 'e) t =
  | Pure   : 'a -> ('a, 'e) t
  | Impure : ('b, 'e) app * ('b, 'a, 'e) arr -> ('a, 'e) t

and ('a, 'b, 'e) arr =
  | Id   : ('a, 'a, 'e) arr
  | Fun  : ('a -> ('b, 'e) t) -> ('a, 'b, 'e) arr
  | Comp : ('a, 'b, 'e) arr * ('b, 'c, 'e) arr -> ('a, 'c, 'e) arr

let arr f = Fun f

let (><) : type a b c e. (a, b, e) arr -> (b, c, e) arr -> (a, c, e) arr =
  fun a b -> match (a, b) with
  | (Id, _) -> b
  | (_, Id) -> a
  | _       -> Comp (a, b)

let rec twist : type a b e. (a, b, e) arr -> (a, b, e) arr = function
  | Comp (Id, g)            -> twist g
  | Comp (Comp (f1, f2), g) -> twist (Comp (f1, f2 >< g))
  | f                       -> f

let rec app : type a b e. (a, b, e) arr -> a -> (b, e) t =
  fun arr a -> match arr with
  | Id    -> Pure a
  | Fun f -> f a
  | Comp (Fun f, g) ->
    ( match f a with
      | Pure x          -> app g x
      | Impure (eff, k) -> Impure (eff, k >< g) )
  | Comp _ -> app (twist arr) a

let return x = Pure x

let (>>=) a fb = match a with
  | Pure x          -> fb x
  | Impure (eff, k) -> Impure (eff, k >< arr fb)

let (>|=) a f = a >>= fun x -> return (f x)

let req eff = Impure (eff, Id)

let after g h = arr (fun x -> h (app g x))
