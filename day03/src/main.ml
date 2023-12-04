module Main = Runner.Make (struct
  let day = 3

  module Answer = Int
  include Day3
end)

let () = Main.run ()
