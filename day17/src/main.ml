module Main = Runner.Make (struct
  let day = 17

  module Answer = Z
  include Day17
end)

let () = Main.run ()
