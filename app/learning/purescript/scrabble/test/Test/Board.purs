module Test.Board where

import Test.QuickCheck (assertEquals)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)

all :: TestSuite
all =
  suite "module Board" do
    test "setWord" do
      quickCheck \i -> assertEquals i 0
