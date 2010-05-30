type 'a cell = Nil | Cons of 'a * 'a t
and 'a t = 'a cell Lazy.t

val cons : 'a -> 'a t -> 'a cell
val nil  : 'a t

val hd   : 'a t -> 'a
val tl   : 'a t -> 'a t

val (++)   : 'a t -> 'a t -> 'a t
val append : 'a t -> 'a t -> 'a t

val take : int -> 'a t -> 'a t
val drop : int -> 'a t -> 'a t
val rev  : 'a t -> 'a t

val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
