module Main = Runner.Make (struct
  let day = 1

  module Answer = Int
  include Day1
end)

let () = Main.run ()
