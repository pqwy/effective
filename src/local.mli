module Make (E : sig type 'a t end) : sig

  module Req : Higher.Newtype1 with type 'a s = 'a E.t

  type 'a t = ('a, Req.t) Eff.t
  include S.Monad with type 'a t := 'a t

  val req : 'a E.t -> 'a t
end
