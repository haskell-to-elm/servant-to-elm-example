module MainTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "arithmetics"
        [ test "2 + 2 = 4" <|
            \() ->
                2
                    + 2
                    |> Expect.equal 4
        ]
