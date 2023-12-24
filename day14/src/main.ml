module Main = Runner.Make (struct
  let day = 14

  module Answer = Z
  include Day14
end)

let () = Main.run ()
