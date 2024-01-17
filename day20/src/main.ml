module Main = Runner.Make (struct
  let day = 20

  module Answer = Int
  include Day20
end)

let () = Main.run ()
