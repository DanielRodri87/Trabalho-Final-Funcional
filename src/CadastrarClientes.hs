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
            let (_, nomeAtual, telefoneAtual) = head clienteExistente
            putStrLn "\nInformações atuais do cliente:"
            putStrLn $ "ID: " ++ show idEditar
            putStrLn $ "Nome: " ++ nomeAtual
            putStrLn $ "Telefone: " ++ telefoneAtual
            
            putStrLn "\nO que você deseja editar?"
            putStrLn "1. Nome"
            putStrLn "2. Telefone"
            putStrLn "3. Ambos"
            putStrLn "4. Cancelar"
            putStr "Escolha uma opção: "
            opcao <- getLine
            
            case opcao of
                "1" -> do
                    putStrLn "Digite o novo Nome do cliente:"
                    novoNome <- getLine
                    let clientesAtualizados = map (\(id, nome, tel) ->
                            if id == idEditar
                                then (id, novoNome, tel)
                                else (id, nome, tel)) clientes
                    putStrLn "Nome editado com sucesso!"
                    return clientesAtualizados
                    
                "2" -> do
                    putStrLn "Digite o novo Telefone do cliente:"
                    novoTelefone <- getLine
                    let clientesAtualizados = map (\(id, nome, tel) ->
                            if id == idEditar
                                then (id, nome, novoTelefone)
                                else (id, nome, tel)) clientes
                    putStrLn "Telefone editado com sucesso!"
                    return clientesAtualizados
                    
                "3" -> do
                    putStrLn "Digite o novo Nome do cliente:"
                    novoNome <- getLine
                    putStrLn "Digite o novo Telefone do cliente:"
                    novoTelefone <- getLine
                    let clientesAtualizados = map (\(id, nome, tel) ->
                            if id == idEditar
                                then (id, novoNome, novoTelefone)
                                else (id, nome, tel)) clientes
                    putStrLn "Cliente editado com sucesso!"
                    return clientesAtualizados
                    
                "4" -> do
                    putStrLn "Operação cancelada."
                    return clientes
                    
                _ -> do
                    putStrLn "Opção inválida!"
                    return clientes

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