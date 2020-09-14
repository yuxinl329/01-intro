module SecretCode where

import Data.Char
import Data.Maybe
import Test.HUnit

code :: [(Char, Char)]
code = zip ['a' .. 'z'] cypher ++ zip ['A' .. 'Z'] (map toUpper cypher)
  where
    cypher :: String
    cypher = "thequickbrownfxjmpsvlazydg"

encodeChar :: Char -> Char
encodeChar c = fromMaybe c (lookup c code)

testEncodeChar :: IO Counts
testEncodeChar =
  runTestTT $
    TestList
      [ encodeChar 'a' ~?= 't',
        encodeChar '.' ~?= '.'
      ]

encodeLine :: String -> String
encodeLine = map encodeChar

testEncodeLine = runTestTT $ TestList [encodeLine "abc defgh" ~?= "the quick"]

encodeContent :: String -> String
encodeContent str = (unlines . reverse . map encodeLine . lines) str

testEncodeContent =
  runTestTT $
    encodeContent "abc\n defgh\n" ~?= " quick\nthe\n"

encodeFile :: FilePath -> IO ()
encodeFile f = do
  let outFile = f ++ ".code"
  contents <- readFile f
  writeFile outFile (encodeContent contents)

main :: IO ()
main = do
  putStrLn "What file shall I encode?"
  fn <- getLine
  encodeFile fn
  putStrLn "All done!"
