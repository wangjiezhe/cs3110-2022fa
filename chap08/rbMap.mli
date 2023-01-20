type ('k, 'v) t

val empty : ('k, 'v) t
val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
val find : 'k -> ('k, 'v) t -> 'v option
