module Pedidos where

import System.IO
import Control.Exception (catch, IOException)
import Tipos (Pedido(..))
import Validacao (getStringValid, getValidInt)


novoPedido :: [Pedido] -> IO Pedido
novoPedido pedidos = do
    idCliente <- getValidInt "Digite o ID do cliente:" 
 
    nomeProduto <- getStringValid "Digite o nome do produto:"
  
    qtd <- getValidInt "Digite a quantidade:"  

    let pedido = Pedido idCliente nomeProduto qtd

    putStrLn "Pedido cadastrado na fila com sucesso!"
    return pedido

listarPedidos :: [Pedido] -> IO ()
listarPedidos pedidos = do
    putStrLn "Fila de Pedidos:"
    putStrLn "ID Cliente\tProduto\tQuantidade"
    putStrLn "--------------------------------------"
    mapM_ (\p -> putStrLn $ show (idClientePedido p) ++ "\t" ++ nomeProdutoPedido p++ "\t" ++ show (quantidadePedido p)) pedidos

salvarPedidos :: [Pedido] -> FilePath -> IO ()
salvarPedidos pedidos arquivo = do
    writeFile arquivo (show pedidos)
    putStrLn $ "Pedidos salvos com sucesso em " ++ arquivo

carregarPedidos :: FilePath -> IO [Pedido]
carregarPedidos arquivo = do
    conteudo <- readFile arquivo `catch` (\(_ :: IOException) -> return "[]")
    let pedidos = read conteudo :: [Pedido]
    return pedidos

removerPedido :: Pedido -> [Pedido] -> [Pedido]
removerPedido pedido = filter (/= pedido)
