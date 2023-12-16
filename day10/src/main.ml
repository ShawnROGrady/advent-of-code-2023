module Main = Runner.Make (struct
  let day = 10

  module Answer = Int
  include Day10
end)

let () = Main.run ()
