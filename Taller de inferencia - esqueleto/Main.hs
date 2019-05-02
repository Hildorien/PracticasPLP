module Main where

import ParserLC
import PrettyPrintLC
import TypeInference

plainExpr :: String -> Doc
plainExpr = ppExpr . parseLC

inferExpr :: String -> Doc
inferExpr = ppTypingResult . inferType . parseLC
