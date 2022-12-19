https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
-- | This module defines a simple command line interface for the Fast
-- interpreter.  If your solution is correct, this module should work.
module Main
       (main)
where

import FastParser
import FastInterpreter

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
            [file] -> do
              s <- readFile file
              case parseString s of
                Left e -> error $ show e
                Right prog ->
                  case runProg prog of
                    Left e -> error $ show e
                    Right output -> putStr output
            _ ->
              error "Give me a (single) argument!"
