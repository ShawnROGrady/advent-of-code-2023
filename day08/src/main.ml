module Main = Runner.Make (struct
  let day = 8

  module Answer = Int64
  include Day8
end)

let () = Main.run ()
