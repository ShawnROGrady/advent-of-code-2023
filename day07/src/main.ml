module Main = Runner.Make (struct
  let day = 7

  module Answer = Int
  include Day7
end)

let () = Main.run ()
