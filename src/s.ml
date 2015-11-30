module type Monad1 = sig
  type 'a t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val (>|=) : 'a t -> ('a -> 'b) -> 'b t
end

module type Monad2 = sig
  type ('a, 'x) t
  val return : 'a -> ('a, 'x) t
  val (>>=) : ('a, 'x) t -> ('a -> ('b, 'x) t) -> ('b, 'x) t
  val (>|=) : ('a, 'x) t -> ('a -> 'b) -> ('b, 'x) t
end

module type Monad = Monad1
