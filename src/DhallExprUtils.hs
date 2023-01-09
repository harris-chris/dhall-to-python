-- {-# LANGUAGE ScopedTypeVariables              #-}
-- {-# LANGUAGE FlexibleInstances              #-}

module DhallExprUtils where

import Dhall.Parser ( ParseError, Src, exprFromText )
import Dhall.Core ( Expr, denote )
import Data.Char ( isSpace )
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

cleanSrc :: Bool -> Expr s a -> Expr s a
cleanSrc shouldDenote expr = if shouldDenote then denote expr else expr

data ProcessState = ProcessState {
    processed::String
    , indentLevel::Int
}

getIndent :: Int -> String -> String
getIndent 0 acc = acc
getIndent x acc = getIndent (x - 1) ("    " ++ acc)

adjustIndent :: ProcessState -> String -> String
adjustIndent (ProcessState done ind) (c:rest)
    | Set.member c indSet = let
        newInd = ind + 1
        indent = getIndent newInd ""
        newDone = done ++ c:"\n" ++ indent
        in adjustIndent (ProcessState newDone newInd) rest
    | Set.member c outSet = let
        newInd = ind - 1
        indent = getIndent newInd ""
        newDone = done ++ "\n" ++ indent ++ [c] ++ "\n" ++ indent
        newRest = dropWhile isSpace rest
        in adjustIndent (ProcessState newDone newInd) newRest
    | c == ',' = let
        indent = getIndent ind ""
        newDone = done ++ "\n" ++ indent ++ [c]
        in adjustIndent (ProcessState newDone ind) rest
    | True = adjustIndent (ProcessState (done ++ [c]) ind) rest
    where
        indSet = Set.fromList ['(', '{']
        outSet = Set.fromList [')', '}']
adjustIndent (ProcessState done ind) [] = done

indent :: T.Text -> String
indent txt = let
    startingState = (ProcessState "" 0)
    in adjustIndent startingState (T.unpack txt)

reduceToFirstLast :: (Int, Int) -> T.Text -> T.Text
reduceToFirstLast (first, last) exprTxt =
    let txtLines = lines $ T.unpack exprTxt
        firstLines = take first txtLines
        lastLines = drop (length txtLines - last) txtLines
    in T.pack $ unlines $ firstLines ++ lastLines

data ShowOptions = ShowOptions {
    showDenoted :: Bool
    , showFirstLast :: Maybe (Int, Int)
}

showExpr :: (Show a) => ShowOptions -> Expr Src a -> T.Text
showExpr opts expr =
    let exprTxt = T.pack . show . (cleanSrc (showDenoted opts)) $ expr
        exprTxt' = indent exprTxt
    in case (showFirstLast opts) of
        (Just fstlst) -> reduceToFirstLast fstlst $ T.pack exprTxt'
        Nothing -> T.pack exprTxt'
