{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser
import Lexer
import Interpret
import DrawMats

import Control.Monad.State
import System.IO
import System.Environment

main = do
    args <- getArgs
    script <- readFile (head args)
    let cmds = parse . lexString $ script
    render cmds

