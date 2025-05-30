module CadastrarClientes where

import System.IO

type Cliente = (Int, String, String)

-- Função para cadastrar um cliente
cadastrarCliente :: IO Cliente
cadastrarCliente = do
    putStrLn "Digite o ID do cliente:"
    idStr <- getLine
    let idCliente = read idStr :: Int

    putStrLn "Digite o Nome do cliente:"
    nome <- getLine

    putStrLn "Digite o Telefone do cliente:"
    telefone <- getLine

    let cliente = (idCliente, nome, telefone)
    putStrLn "Cliente cadastrado com sucesso!"
    return cliente


-- Função para listar todos os clientes
listarClientes :: [Cliente] -> IO ()
listarClientes clientes = do
    putStrLn "Lista de Clientes:"
    putStrLn "ID\tNome\t\tTelefone"
    putStrLn "-----------------------------------"
    mapM_ (\(idCliente, nome, telefone) -> 
        putStrLn $ show idCliente ++ "\t" ++ nome ++ "\t\t" ++ telefone) clientes


-- Função para editar um cliente pelo ID
editarCliente :: Int -> [Cliente] -> IO [Cliente]
editarCliente idEditar clientes = do
    let clienteExistente = filter (\(idCliente, _, _) -> idCliente == idEditar) clientes
    if null clienteExistente
        then do
            putStrLn "Cliente não encontrado!"
            return clientes
        else do
            putStrLn "Digite o novo Nome do cliente:"
            novoNome <- getLine

            putStrLn "Digite o novo Telefone do cliente:"
            novoTelefone <- getLine

            let clientesAtualizados = map (\(idCliente, nome, telefone) ->
                    if idCliente == idEditar
                        then (idCliente, novoNome, novoTelefone)
                        else (idCliente, nome, telefone)) clientes

            putStrLn "Cliente editado com sucesso!"
            return clientesAtualizados

-- Função para excluir um cliente pelo ID
excluirCliente :: Int -> [Cliente] -> IO [Cliente]
excluirCliente idExcluir clientes = do
    let clientesAtualizados = filter (\(idCliente, _, _) -> idCliente /= idExcluir) clientes
    if length clientesAtualizados == length clientes
        then do
            putStrLn "Cliente não encontrado!"
            return clientes
        else do
            putStrLn "Cliente excluído com sucesso!"
            return clientesAtualizados