module Main = Runner.Make (struct
  let day = 19

  module Answer = Z
  include Day19
end)

let () = Main.run ()
