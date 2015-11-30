open Eff
open Higher

type ('a, 'eff) t = ('a, 'eff) Eff.t

let rebind m k run = m >>= fun x -> run (app k x)

module Id = struct
  type 'a req
  module Req = Newtype1 (struct type 'a t = 'a req end)
  type eff = Req.t
  let run : ('a, eff) t -> 'a = function
    | Pure x        -> x
    | Impure (_, _) -> assert false
end

module Ground (M : S.Monad) = struct
  type 'a req = Lift : 'a M.t -> 'a req
  module Req = Newtype1 (struct type 'a t = 'a req end)
  type eff = Req.t
  let rec run : ('a, eff) t -> 'a M.t = function
    | Pure x          -> M.return x
    | Impure (req, k) -> match Req.prj req with
        Lift m -> M.(m >>= fun x -> run (app k x))
  let lift m = req (Req.inj (Lift m))
end

module Result = struct
  type (_, _, _) req =
    | Error : 'e -> ('a, 'e, 'base) req
  module Req = Newtype3 (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) req end)
  type ('e, 'base) eff = ('e, ('base, Req.t) app) app
  let run = function
    | Pure x -> return (`Ok x)
    | Impure (req, _) -> match Req.prj req with
        Error e -> return (`Error e)
  let fail e = req (Req.inj (Error e))
end

module State = struct
  type (_, _, _) req =
    | Set  : 's -> (unit, 's, 'base) req
    | Get  : ('s, 's, 'base) req
    | Lift : ('a, 'base) t -> ('a, 's, 'base) req
  module Req = Newtype3 (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) req end)
  type ('s, 'base) eff = ('s, ('base, Req.t) app) app
  let rec run : type s. s -> ('a, (s, 'base) eff) t -> (('a * s), 'base) t =
    fun s -> function
    | Pure x          -> Pure (x, s)
    | Impure (req, k) -> match Req.prj req with
      | Set x  -> run x (app k ())
      | Get    -> run s (app k s)
      | Lift m -> rebind m k (run s)
  let get () = req (Req.inj Get)
  let set x  = req (Req.inj (Set x))
  let lift m = req (Req.inj (Lift m))
end

module Reader = struct
  type (_, _, _) req =
    | Ask  : ('e, 'e, 'base) req
    | Lift : ('a, 'base) t -> ('a, 'e, 'base) req
  module Req = Newtype3 (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) req end)
  type ('e, 'base) eff = ('e, ('base, Req.t) app) app
  let rec run : type e. e -> ('a, (e, 'base) eff) t -> ('a, 'base) t =
    fun e -> function
    | Pure x          -> Pure x
    | Impure (req, k) -> match Req.prj req with
      | Ask    -> run e (app k e)
      | Lift m -> rebind m k (run e)
  let ask () = req (Req.inj Ask)
  let lift m = req (Req.inj (Lift m))
end

module Nondet = struct
  type (_, _) req =
    | Amb  : 'a list -> ('a, 'base) req
    | Lift : ('a, 'base) t -> ('a, 'base) req
  module Req = Newtype2 (struct type ('a, 'b) t = ('a, 'b) req end)
  type 'base eff = ('base, Req.t) app
  let rec run f z = function
    | Pure x -> f x z
    | Impure (req, k) -> match Req.prj req with
      | Amb xs ->
          let rec unamb = function
            | []    -> Lazy.force z
            | x::xs -> run f (lazy (unamb xs)) (app k x) in
          unamb xs
      | Lift m -> rebind m k (run f z)
    let amb xs = req (Req.inj (Amb xs))
    let lift m = req (Req.inj (Lift m))
end

module Cc = struct
  type (_, _, _) req = ..
  module Req = Newtype3 (struct type ('a, 'b, 'c) t = ('a, 'b, 'c) req end)
  type ('r, 'base) eff = ('r, ('base, Req.t) app) app
  type (_, _, _) req +=
    | Reset : ('a, ('a, 'base) eff) t -> ('a, 'r, 'base) req
    | Shift : (('a -> ('r, ('r, 'base) eff) t) -> ('r, ('r, 'base) eff) t) -> ('a, 'r, 'base) req
    | Lift  : ('a, 'base) t -> ('a, 'r, 'base) req
  let lift m  = req (Req.inj (Lift m))
  let shift f = req (Req.inj (Shift f))
  let reset t = req (Req.inj (Reset t))
  let rec run : type a. (a, (a, 'base) eff) t -> (a, 'base) t = function
    | Pure x          -> Pure x
    | Impure (req, k) -> match Req.prj req with
      | Reset t -> rebind (run t) k run
      | Shift f -> run (f (app k))
      | Lift m  -> rebind m k run
      | _       -> assert false
end

