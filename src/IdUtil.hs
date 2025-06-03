module IdUtil where

import Tipos(Produto (..), Identificavel(..))
import Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente)

-- Verifica se um ID já está em uso tanto para produtos, quanto para clientes
idExiste :: Identificavel a => Int -> [a] -> Bool
idExiste id lista = any (\x -> obterID x == id) lista

gerarIdUnico :: Identificavel a => [a] -> (Int, Int) -> Int
gerarIdUnico lista (inicio, fim) = 
    let existeIds = map obterID lista 
        novoId = head [x| x <- [inicio..fim], x `notElem` existeIds] 
    in novoId


-- Produtos: IDs entre 100-999
gerarIdUnicoProduto :: [Produto] -> Int
gerarIdUnicoProduto produtos = gerarIdUnico produtos (100, 999)


-- Clientes: IDs entre 1000-9999
gerarIdUnicoCliente :: [Cliente] -> Int
gerarIdUnicoCliente clientes = gerarIdUnico clientes (1000, 9999)

