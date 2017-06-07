{-# LANGUAGE OverloadedStrings #-}
module ParserCombinators (tests) where

import Test.Hspec (Spec, describe, it)
import Test.HUnit (assertBool, assertEqual, Assertion)
import qualified Data.Attoparsec.Text as P
import Data.Text
--import Data.Ato

tests :: Spec
tests = describe "ParserCombinators" $ do
    testDigitParser
    testDigitsParser
    testSymbolParser
    testAtomParser
    testListParser

failParser :: String -> P.Parser a
failParser parserName =
  fail $ "\n\n\t\x1B[32;1mCheck documentation\x1B[0m of \x1B[33;1m"
         ++ parserName
         ++ "\x1B[0m on:\n\t"
         ++ "http://hackage.haskell.org/packages/archive/attoparsec/latest/doc/html/Data-Attoparsec-Text.html"
         ++ "\n\tand http://hackage.haskell.org/packages/archive/attoparsec/latest/doc/html/Data-Attoparsec-Combinator.html"

assertParse :: (Eq a, Show a) => a -> Either String a -> Assertion
assertParse _ (Left e) = assertBool e False
assertParse expected (Right answer) =
  assertEqual "wrong parser" expected answer

testDigitParser :: Spec
testDigitParser = it "digit parser" $ do
    -- Change parser with the correct parser to use
    let parser = P.digit :: P.Parser Char
    let result = P.parseOnly parser "5"
    assertParse '5' result

testDigitsParser :: Spec
testDigitsParser = it "sequence of digits parser" $ do
    -- Change parser with the correct parser to use
    let parser = P.many' P.digit :: P.Parser String
    let result = P.parseOnly parser "54321"
    assertParse "54321" result

testSymbolParser :: Spec
testSymbolParser = it "symbol parser" $ do
    -- Change parser with the correct parser to use
    --
    -- Here we say symbol is a sequence of characters that doesn't have
    -- parenthes or spaces.
    let parser = P.many' (P.satisfy (\c -> c `notElem` ['(', ')', ' '])) :: P.Parser String
    assertParse "ab" $ P.parseOnly parser "ab"
    assertParse "a/b" $ P.parseOnly parser "a/b"
    assertParse "a/b" $ P.parseOnly parser "a/b c"

data Atom = AInt Int | ASym Text deriving (Eq, Show)
parseAtom :: P.Parser Atom
parseAtom = let symbolParser = fmap ASym (P.takeWhile1 (\ c -> c `notElem` ['(', ')', ' '] && not (c >= '0' && c <= '9'))) :: P.Parser Atom
                digitParser = fmap (AInt . read) (P.many' P.digit) :: P.Parser Atom
            in P.choice [symbolParser, digitParser]
testAtomParser :: Spec
testAtomParser = it "atom parser" $ do
    -- Change parser with the correct parser to use
    let parser = parseAtom
    assertParse (ASym "ab") $ P.parseOnly parser "ab"
    assertParse (ASym "a/b") $ P.parseOnly parser "a/b"
    assertParse (ASym "a/b") $ P.parseOnly parser "a/b c"
    assertParse (AInt 54321) $ P.parseOnly parser "54321 123"

data List = Nil | LAtom Atom | Cons List List deriving (Eq, Show)

parseListContents :: P.Parser List
parseListContents = do
  ret <- P.choice [parseList, fmap LAtom parseAtom, return Nil]
  P.skipSpace
  fmap (Cons ret) parseListContents


parseList :: P.Parser List
parseList = do
  P.char '('
  ret <- parseListContents
  P.char ')'
  return ret

testListParser :: Spec
testListParser = it "list parser" $ do
    -- Change parser with the correct parser to use
    let parser = parseList
    assertParse Nil $ P.parseOnly parser "()"
    assertParse (Cons (LAtom (AInt 12)) Nil) $ P.parseOnly parser "(12)"
    assertParse (Cons (LAtom (ASym "a")) (Cons (LAtom (ASym "b")) Nil)) $ P.parseOnly parser "(a (b))"
