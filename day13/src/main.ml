module Main = Runner.Make (struct
  let day = 13

  module Answer = Z
  include Day13
end)

let () = Main.run ()
