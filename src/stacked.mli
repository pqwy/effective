type ('a, 'eff) t = ('a, 'eff) Eff.t

module Id : sig
  type eff
  val run : ('a, eff) t -> 'a
end

module Ground (M : S.Monad) : sig
  type eff
  val run : ('a, eff) t -> 'a M.t
  val lift : 'a M.t -> ('a, eff) t
end

module Result : sig
  type ('e, 'base) eff
  val run : ('a, ('e, 'base) eff) t -> ([ `Ok of 'a | `Error of 'e ], 'base) t
  val fail : 'e -> ('a, ('e, 'base) eff) t
end

module State : sig
  type ('s, 'base) eff
  val run  : 's -> ('a, ('s, 'base) eff) t -> (('a * 's), 'base) t
  val lift : ('a, 'base) t -> ('a, ('s, 'base) eff) t
  val get  : unit -> ('s, ('s, 'base) eff) t
  val set  : 's -> (unit, ('s, 'base) eff) t
end

module Reader : sig
  type ('e, 'base) eff
  val run : 'e -> ('a, ('e, 'base) eff) t -> ('a, 'base) t
  val lift : ('a, 'base) t -> ('a, ('e, 'base) eff) t
  val ask  : unit -> ('e, ('e, 'base) eff) t
end

module Nondet : sig
  type 'base eff
  val run : ('a -> ('b, 'base) t Lazy.t -> ('b, 'base) t)
              -> ('b, 'base) t Lazy.t -> ('a, 'base eff) t -> ('b, 'base) t
  val amb : 'a list -> ('a, 'base eff) t
  val lift : ('a, 'base) t -> ('a, 'base eff) t
end

module Cc : sig
  type ('r, 'base) eff
  val run : ('a, ('a, 'base) eff) t -> ('a, 'base) t
  val shift : (('a -> ('r, ('r, 'base) eff) t) -> ('r, ('r, 'base) eff) t)
                -> ('a, ('r, 'base) eff) t
  val reset : ('a, ('a, 'base) eff) t -> ('a, ('r, 'base) eff) t
  val lift : ('a, 'base) t -> ('a, ('a, 'base) eff) t
end
