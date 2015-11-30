type 'a effect = ..

module Req : Higher.Newtype1 with type 'a s = 'a effect

type 'a t = ('a, Req.t) Eff.t

include S.Monad with type 'a t := 'a t

val lower : 'a t -> 'a option
val lower_exn : 'a t -> 'a

module Result (Err : sig type t end) : sig
  val run : 'a t -> [ `Ok of 'a | `Error of Err.t ] t
  val fail : Err.t -> 'a t
end

module Reader (Env : sig type t end) : sig
  val run : Env.t -> 'a t -> 'a t
  val ask : Env.t t
end

module State (St : sig type t end) : sig
  val run  : St.t -> 'a t -> ('a * St.t) t
  val get : St.t t
  val set : St.t -> unit t
end

module Yield (T : sig type t end) : sig
  val run : 'a t -> [ `Done of 'a | `Cont of T.t * (unit -> 'a t) ] t
  val yield : T.t -> unit t
end

module Nondet : sig
  val run : ('a -> 'b t Lazy.t -> 'b t) -> 'b t Lazy.t -> 'a t -> 'b t
  val to_list : 'a t -> 'a list t
  val amb : ('a -> ('b * 'a) option) -> 'a -> 'b t
  val ambl : 'a list -> 'a t
end

module Ground (M : S.Monad) : sig
  val run : 'a t -> 'a t M.t
  val lift : 'a M.t -> 'a t
end
