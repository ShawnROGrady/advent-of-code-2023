module Input : sig
  type t

  val of_in_channel : in_channel -> t
  val of_string : string -> t
end

module Part1 : sig
  val run : Input.t -> int
end

module Part2 : sig
  val run : Input.t -> int
end
