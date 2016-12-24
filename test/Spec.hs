{-# LANGUAGE OverloadedStrings #-}

import Pid
import MPlayer

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "parseOutput" $ do
    it "extracts path from answer" $ do
      case parseOutput "ANS_path=A\n" of
        Right [x] ->
          case x of
            Answer Path p -> p `shouldBe` "A"

    it "ignores logs as noise" $ do
      case parseOutput "Audio only file format detected.\n" of
        Right [x] ->
          x `shouldBe` Noise

    it "ignores the first line as noise and detects playback start" $ do
      case parseOutput "Video: no video\nStarting playback...\n" of
        Right [x1, x2] ->
          x2 `shouldBe` StartingPlayback

    it "extracts paths from links" $ do
      case linkToPath "l-wx------ 1 amir amir 64 Dec 24 15:21 3 -> /run/user/1000/i3/errorlog" of
        Right x ->
          x `shouldBe` "/run/user/1000/i3/errorlog"
