module Main = Runner.Make (struct
  let day = 11

  module Answer = Int
  include Day11
end)

let () = Main.run ()
