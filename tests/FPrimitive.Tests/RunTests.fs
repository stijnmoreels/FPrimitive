namespace FPrimitive.Tests

open Expecto

module RunTests =

  [<EntryPoint>]
  let main args =
    Tests.runTestsInAssemblyWithCLIArgs [] args

