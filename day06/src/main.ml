module Main = Runner.Make (struct
  let day = 6

  module Answer = Int
  include Day6
end)

let () = Main.run ()
