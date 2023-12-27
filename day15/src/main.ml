module Main = Runner.Make (struct
  let day = 15

  module Answer = Int
  include Day15
end)

let () = Main.run ()
