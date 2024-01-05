module Main = Runner.Make (struct
  let day = 18

  module Answer = Z
  include Day18
end)

let () = Main.run ()
