module Test.HUnit.Tools2 where
import Test.Framework.Providers.HUnit (testCase)
import Text.Regex.TDFA ((=~))

(>@?=) monadic expected = do
    value <- monadic
    value @?= expected

(@=?<) expected monadic = do
    value <- monadic
    expected @=? value

(@?~) received expectedRE = (received =~ expectedRE :: Bool) @?
    ("expected match: " ++ expectedRE ++ "\nbut got: " ++ received ++ "\n")

(@?!~) received unexpectedRE = (not $ received =~ unexpectedRE :: Bool) @?
    ("should not match: " ++ unexpectedRE ++ "\nbut got: " ++ received ++ "\n")

(@~?) = flip (@?~)

(@!~?) = flip (@?!~)

(>@?~) monadic expectedRE = do
    value <- monadic
    value @?~ expectedRE

(@~?<) = flip (>@?~)

(>@?!~) monadic expectedRE = do
    value <- monadic
    value @?!~ expectedRE

(@!~?<) = flip (>@?!~)
