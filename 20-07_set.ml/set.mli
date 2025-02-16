type 'a t (* 要素の型が'aの集合の型 *)
val empty : 'a t (* 空集合 *)
val singleton : 'a -> 'a t (* 要素ひとつからなる集合 *)
val union : 'a t -> 'a t -> 'a t (* 和集合 *)
val inter : 'a t -> 'a t -> 'a t (* 共通部分 *)
val diff : 'a t -> 'a t -> 'a t (* 差集合 *)
val mem : 'a -> 'a t -> bool
val length : 'a t -> int
