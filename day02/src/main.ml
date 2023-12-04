module Main = Runner.Make (struct
  let day = 2

  module Answer = Int
  include Day2
end)

let () = Main.run ()
