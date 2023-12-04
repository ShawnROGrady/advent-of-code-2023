#! /bin/sh

interface_tmpl=$(cat <<EOF
module Input : sig
	type t

	val of_in_channel : in_channel -> t
	val of_string : string -> t
end

module Part1 : sig
	val run : Input.t -> {{ANSWER}}
end

module Part2 : sig
	val run : Input.t -> {{ANSWER}}
end
EOF
)

implementation_tmpl=$(cat <<EOF
module Input = struct
	type t = unit

	let of_in_channel _ = failwith "unimplemented"
	let of_string _ = failwith "unimplemented"
end

module Part1 = struct
	let run _ = failwith "unimplemented"
end

module Part2 = struct
	let run _ = failwith "unimplemented"
end
EOF
)

dune_tmpl=$(cat <<EOF
(library
 (name day{{DAY}})
 (modules day{{DAY}})
 (libraries
  fmt))

(executable
 (name main)
 (modules main)
 (libraries
  day{{DAY}}
  runner))
EOF
)

main_tmpl=$(cat <<EOF
module Main = Runner.Make (struct
  let day = {{DAY}}

  module Answer = {{ANSWER_MOD}}
  include Day{{DAY}}
end)

let () = Main.run ()
EOF
)

OPTIND=1

answer_typ="int"
answer_mod="Int"

while getopts "h?string" opt; do
  case "$opt" in
	  h|\?)
		  exit 0
		  ;;
	  string)
		  answer_typ="string"
		  answer_mod="Runner.String"
		  ;;
  esac
done

shift $((OPTIND-1))

[ "${1:-}" = "--" ] && shift

day=$@

mkdir -p day${day}/src
echo "$interface_tmpl" | sed "s/{{ANSWER}}/$answer_typ/g" > ./day${day}/src/day${day}.mli
echo "$implementation_tmpl" > ./day${day}/src/day${day}.ml
echo "$dune_tmpl" | sed "s/{{DAY}}/$day/g" > ./day${day}/src/dune
echo "$main_tmpl" | sed "s/{{DAY}}/$day/g" | sed "s/{{ANSWER_MOD}}/$answer_mod/g" > ./day${day}/src/main.ml
dune fmt 2> /dev/null
exit 0
