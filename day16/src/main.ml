module Main = Runner.Make (struct
  let day = 16

  module Answer = Int
  include Day16
end)

let () = Main.run ()
