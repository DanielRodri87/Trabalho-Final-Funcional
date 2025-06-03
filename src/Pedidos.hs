module Pedidos where

import System.IO
import Control.Exception (catch, IOException)

type Pedido = (Int, String, Int) -- (IdCliente, NomeProduto, Quantidade)

novoPedido :: [Pedido] -> IO Pedido
novoPedido pedidos = do
    putStrLn "Digite o ID do cliente:"  
    idClienteStr <- getLine
    let idCliente = read idClienteStr :: Int

    putStrLn "Digite o nome do produto:"  
    nomeProduto <- getLine

    putStrLn "Digite a quantidade:"  
    qtdStr <- getLine
    let quantidade = read qtdStr :: Int

    let pedido = (idCliente, nomeProduto, quantidade)
    putStrLn "Pedido cadastrado na fila com sucesso!"
    return pedido

listarPedidos :: [Pedido] -> IO ()
listarPedidos pedidos = do
    putStrLn "Fila de Pedidos:"
    putStrLn "ID Cliente\tProduto\tQuantidade"
    putStrLn "--------------------------------------"
    mapM_ (\(idC, prod, qtd) -> putStrLn $ show idC ++ "\t" ++ prod ++ "\t" ++ show qtd) pedidos

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
