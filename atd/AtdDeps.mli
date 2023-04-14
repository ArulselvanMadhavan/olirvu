val equal_string : string -> string -> bool

val equal_list : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool

val equal_option : ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

val equal_int : int -> int -> bool

val equal_bool : bool -> bool -> bool
