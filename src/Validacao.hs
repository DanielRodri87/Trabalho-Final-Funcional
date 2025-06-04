{-| Este módulo fornece funções utilitárias para validação de entrada
de dados no terminal, garantindo que os valores recebidos sejam válidos.

- 'getStringValid' : Solicita e valida uma string não vazia.
- 'getValidInt'    : Solicita e valida um número inteiro.

-}

module Validacao (getStringValid, getValidInt) where

import System.IO (hFlush, stdout)
import Data.Char (isDigit)

{-| Solicita ao usuário uma string não vazia.

Exibe o 'prompt' e repete até que uma entrada válida seja fornecida.

Parâmetro:
- 'prompt': Mensagem a ser exibida ao usuário.

Retorna:
- String não vazia digitada pelo usuário.
-}
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

{-| Solicita ao usuário um número inteiro válido.

Exibe o 'prompt' e repete até que uma entrada válida seja fornecida.

Parâmetro:
- 'prompt': Mensagem a ser exibida ao usuário.

Retorna:
- Número inteiro digitado pelo usuário.
-}
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