
type ('a, 'eff) t =
  | Pure   : 'a -> ('a, 'eff) t
  | Impure : ('b, 'eff) Higher.app * ('b, 'a, 'eff) arr -> ('a, 'eff) t
(** [('a, 'eff) t] is a computation yielding ['a] and possibly invoking
    effects ['eff].
    - [Pure] is just the result value.
    - [Impure] is [('b 'eff) * ('b -> ('a, 'eff) t)] with ['b 'eff] encoded
      via Higher. *)

and ('a, 'b, 'eff) arr
(** [('a, 'b, 'eff) arr] is morally a function ['a -> 'b], except:
  - The result ['b] is lifted into [('b, 'eff) t], so [arr] corresponds to
    ['a -> ('b, 'eff) t], a kleisly arrow over [(_, 'eff) t]; and
  - The type is abstract and internally represented as a type-aligned
    sequence. *)

val arr : ('a -> ('b, 'eff) t) -> ('a, 'b, 'eff) arr
(** Lift a function into an [arr]. *)

val app : ('a, 'b, 'eff) arr -> 'a -> ('b, 'eff) t
(** [arr] application. *)

val after : ('a, 'b, 'eff) arr -> (('b, 'eff) t -> ('c, 'eff2) t) -> ('a, 'c, 'eff2) arr
(** [after arr f] is a new [arr] which invokes [arr] on the input, and then [f]
    on the entire monadic result of [arr]. *)

val req : ('a, 'eff) Higher.app -> ('a, 'eff) t
(** Effect injection. *)

include S.Monad2 with type ('a, 'eff) t := ('a, 'eff) t
(** A "Warm Fuzzy Thing". *)
