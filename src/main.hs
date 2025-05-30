module Main where

import CadastrarClientes
import Produtos 
import System.IO

menuPrincipal :: IO ()
menuPrincipal = do
    putStrLn "\nMenu Principal"
    putStrLn "1. Cadastrar Clientes"
    putStrLn "2. Cadastrar Produtos"
    putStrLn "3. Sair"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> menuClientes []
        "2" -> menuProdutos []  
        "3" -> putStrLn "Programa encerrado."
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
            novoCliente <- cadastrarCliente
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

main :: IO ()
main = menuPrincipal

