module Main = Runner.Make (struct
  let day = 18

  module Answer = Int
  include Day18
end)

let () = Main.run ()
