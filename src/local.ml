module Make (E : sig type 'a t end) = struct

  module Req = Higher.Newtype1 (struct type 'a t = 'a E.t end)

  type 'a t = ('a, Req.t) Eff.t

  open Eff

  let return = return
  and (>>=)  = (>>=)
  and (>|=)  = (>|=)

  let req r = req (Req.inj r)
end
