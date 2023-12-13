module Main = Runner.Make (struct
  let day = 9

  module Answer = Z
  include Day9
end)

let () = Main.run ()
