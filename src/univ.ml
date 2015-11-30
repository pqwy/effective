
open Eff

type 'a effect = ..

include Local.Make (struct type 'a t = 'a effect end)

let lower = function
  | Pure x   -> Some x
  | Impure _ -> None

let lower_exn = function
  | Pure x   -> x
  | Impure _ -> invalid_arg "Universal.lower_exn: unhandled effect"

module Result (Err : sig type t end) = struct
  type 'a effect += Fail : Err.t -> 'a effect
  let fail e = req (Fail e)
  let rec run = function
    | Pure x          -> Pure (`Ok x)
    | Impure (req, k) ->
        match Req.prj req with
        | Fail e -> Pure (`Error e)
        | _      -> Impure (req, after k run)
end

module Reader (Env : sig type t end) = struct
  type 'a effect += Ask : Env.t effect
  let ask = req Ask
  let rec run : type a. Env.t -> a t -> a t = fun e -> function
    | Pure x          -> Pure x
    | Impure (req, k) ->
        match Req.prj req with
        | Ask -> run e (app k e)
        | _   -> Impure (req, after k (run e))
end

module State (St : sig type t end) = struct
  type 'a effect +=
    | Get : St.t effect
    | Set : St.t -> unit effect
  let get    = req Get
  and set x  = req (Set x)
  let rec run : type a. St.t -> a t -> (a * St.t) t = fun s -> function
    | Pure x          -> Pure (x, s)
    | Impure (req, k) ->
        match Req.prj req with
        | Get    -> run s (app k s)
        | Set x  -> run x (app k ())
        | _      -> Impure (req, after k (run s))
end

module Yield (T : sig type t end) = struct
  type 'a effect += Yield : T.t -> unit effect
  type 'a res = [ `Done of 'a | `Cont of T.t * (unit -> 'a t) ]
  let yield x = req (Yield x)
  let rec run : 'a t -> 'a res t = function
    | Pure x          -> Pure (`Done x)
    | Impure (req, k) ->
        match Req.prj req with
        | Yield x -> Pure (`Cont (x, app k))
        | _       -> Impure (req, after k run)
end

module Nondet = struct
  type 'a effect +=
    | Amb : ('a -> ('b * 'a) option) * 'a -> 'b effect
  let amb f s = req (Amb (f, s))
  let ambl xs = amb (function [] -> None | x::xs -> Some (x, xs)) xs
  let rec run f z = function
    | Pure x          -> f x z
    | Impure (req, k) ->
        match Req.prj req with
        | Amb (g, s) ->
            let rec unamb s = match g s with
              | None -> Lazy.force z
              | Some (x, s) -> run f (lazy (unamb s)) (app k x) in
            unamb s
        | _ -> Impure (req, after k (run f z))
  let to_list t =
    let nil = lazy (return []) in
    run (fun x (lazy txs) -> txs >|= fun xs -> x::xs) nil t
end

module Ground (M : S.Monad) = struct
  type 'a effect += Lift : 'a M.t -> 'a effect
  let lift m = req (Lift m)
  let rec run : 'a t -> 'a t M.t = function
    | Pure _ as r     -> M.return r
    | Impure (req, k) ->
        match Req.prj req with
        | Lift m -> M.(m >>= fun x -> run (app k x))
        | _      -> invalid_arg "Commute.run: unhandled effect"
end
