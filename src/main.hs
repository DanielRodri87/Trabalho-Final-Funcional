module Main where

import CadastrarClientes
import Produtos 
import Pedidos (Pedido, novoPedido, listarPedidos, removerPedido)
import System.IO

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nMenu Principal"
    putStrLn "1. Opcoes Clientes"
    putStrLn "2. Opcoes Produtos"
    putStrLn "3. Fila de Pedidos"
    putStrLn "4. Sair"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> menuClientes []
        "2" -> menuProdutos []  
        "3" -> menuPedidos []
        "4" -> putStrLn "Programa encerrado."
        _   -> do
            putStrLn "Opção inválida!"
            menuPrincipal

menuClientes :: [Cliente] -> IO ()
menuClientes clientes = do
    putStrLn "\nMenu de Clientes"
    putStrLn "1. Cadastrar Cliente"
    putStrLn "2. Editar Cliente"
    putStrLn "3. Exibir Clientes Cadastrados"
    putStrLn "4. Deletar Cliente"
    putStrLn "5. Voltar ao Menu Principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoCliente <- cadastrarCliente clientes  -- Pass current clients list
            menuClientes (clientes ++ [novoCliente])
        "2" -> do
            putStr "Digite o ID do cliente que deseja editar: "
            idStr <- getLine
            let id = read idStr :: Int
            clientesAtualizados <- editarCliente id clientes
            menuClientes clientesAtualizados
        "3" -> do
            listarClientes clientes
            menuClientes clientes
        "4" -> do
            putStr "Digite o ID do cliente que deseja excluir: "
            idStr <- getLine
            let id = read idStr :: Int
            clientesAtualizados <- excluirCliente id clientes
            menuClientes clientesAtualizados
        "5" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuClientes clientes

menuProdutos :: [Produto] -> IO ()
menuProdutos produtos = do
    putStrLn "\nMenu de Produtos"
    putStrLn "1. Cadastrar Produto"
    putStrLn "2. Editar Produto"
    putStrLn "3. Exibir Produtos Cadastrados"
    putStrLn "4. Deletar Produto"
    putStrLn "5. Voltar ao Menu Principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoProd <- novoProduto produtos
            let produtosAtualizados = produtos ++ [novoProd]
            menuProdutos produtosAtualizados
        "2" -> do
            putStr "Digite o ID do produto que deseja editar: "
            idStr <- getLine
            let id = read idStr :: Int
            produtosAtualizados <- editarProduto id produtos
            menuProdutos produtosAtualizados
        "3" -> do
            listarProdutos produtos
            menuProdutos produtos
        "4" -> do
            putStr "Digite o ID do produto que deseja excluir: "
            idStr <- getLine
            let id = read idStr :: Int
            produtosAtualizados <- excluirProduto id produtos
            menuProdutos produtosAtualizados
        "5" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuProdutos produtos

menuPedidos :: [Pedido] -> IO ()
menuPedidos pedidos = do
    putStrLn "\nMenu de Pedidos"
    putStrLn "1. Cadastrar Pedido"
    putStrLn "2. Exibir Fila de Pedidos"
    putStrLn "3. Remover Pedido da Fila (Pagamento)"
    putStrLn "4. Voltar ao Menu Principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoPed <- novoPedido pedidos
            menuPedidos (pedidos ++ [novoPed])
        "2" -> do
            listarPedidos pedidos
            menuPedidos pedidos
        "3" -> do
            putStrLn "Digite o ID do cliente do pedido a remover (pagamento):"
            idStr <- getLine
            let idCliente = read idStr :: Int
            let (removidos, filaRestante) = case pedidos of
                    [] -> ([], [])
                    xs -> let (r, f) = span (\(idC, _, _) -> idC /= idCliente) xs in
                          if null f then ([], xs) else ([head f], r ++ tail f)
            if null removidos
                then putStrLn "Pedido não encontrado na fila!"
                else putStrLn "Pedido removido da fila (pagamento realizado)!"
            menuPedidos filaRestante
        "4" -> menuPrincipal
        _   -> do
            putStrLn "Opção inválida!"
            menuPedidos pedidos

main :: IO ()
main = menuPrincipal

