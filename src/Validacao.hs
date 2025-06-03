module Validacao (getStringValid, getValidInt) where

import System.IO (hFlush, stdout)
import Data.Char (isDigit)



getStringValid :: String -> IO String
getStringValid prompt = do
    putStr prompt
    hFlush stdout
    str <- getLine

    if null str 
        then do
            putStrLn "Entrada inválida! O campo não pode estar vazio."
            getStringValid prompt
        else return str

getValidInt :: String -> IO Int
getValidInt prompt = do
    putStr prompt
    hFlush stdout
    input <- getLine
    if all isDigit input  -- Verifica se todos os caracteres são dígitos
        then return (read input :: Int)
        else do
            putStrLn "Entrada inválida. Digite um número inteiro."
            getValidInt prompt