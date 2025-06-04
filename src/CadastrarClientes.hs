module CadastrarClientes where
-- Importação de módulos auxiliares para validação e tipos
import Validacao (getStringValid, getValidInt)
import Tipos (Identificavel(..))

import Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente)


import System.IO
import IdUtil (gerarIdUnicoCliente)


{- Função para cadastrar um cliente.
Descrição: Cadastra um novo cliente solicitando nome e telefone ao usuário, gerando um ID único com base nos clientes já existentes.
Entrada: Lista de clientes já cadastrados.
Saída: Um novo cliente criado.
-}
cadastrarCliente :: [Cliente] -> IO Cliente
cadastrarCliente clientes = do
    -- Gera um ID único para o novo cliente
    let idCliente = gerarIdUnicoCliente clientes
    putStrLn $ "ID gerado: " ++ show idCliente

    -- Solicita nome e telefone válidos ao usuário
    nome <- getStringValid "Digite o nome do cliente: "

    telefone <- getStringValid "Digite o telefone do cliente: "

    -- Cria o novo Cliente
    let cliente = novoCliente idCliente nome telefone

    putStrLn "Cliente cadastrado com sucesso!"
    return cliente

{- Função para listar todos os clientes
Descrição: Exibe todos os clientes da lista formatados.
Entrada: Lista de clientes.
Saída: Impressão formatada no console.
-}
listarClientes :: [Cliente] -> IO ()
listarClientes clientes = do
    putStrLn "Lista de Clientes:"
    putStrLn "ID\tNome\t\tTelefone"
    putStrLn "-----------------------------------"
    -- Imprime cada cliente formatado
    mapM_ (\c -> putStrLn $ show (obterIdCliente c) ++ "\t" ++ obterNomeCliente c ++ "\t\t" ++ obterTelefoneCliente c) clientes




{- Função para editar um cliente pelo ID
Descrição: Permite editar um cliente existente com base no seu ID.
-- Entrada: ID do cliente a ser editado e lista de clientes.
-- Saída: Lista atualizada de clientes.
-}
editarCliente :: Int -> [Cliente] -> IO [Cliente]
editarCliente idEditar clientes = do
    -- Filtra o cliente pelo ID
    let clienteExistente = filter (\c -> obterIdCliente c == idEditar) clientes

    if null clienteExistente
        then do
            putStrLn "Cliente não encontrado!"
            return clientes
        else do
            -- Obtém o cliente e suas informações atuais
            let cliente = head clienteExistente  -- Pegamos o primeiro elemento com segurança
            let nomeAtual = obterNomeCliente cliente
            let telefoneAtual = obterTelefoneCliente cliente

            -- Exibe as informações atuais
            putStrLn "\nInformações atuais do cliente:"
            putStrLn $ "ID: " ++ show idEditar
            putStrLn $ "Nome: " ++ nomeAtual
            putStrLn $ "Telefone: " ++ telefoneAtual
            
            -- Menu de edição
            putStrLn "\nO que você deseja editar?"
            putStrLn "1. Nome"
            putStrLn "2. Telefone"
            putStrLn "3. Ambos"
            putStrLn "4. Cancelar"
            putStr "Escolha uma opção: "
            opcao <- getLine
            
            -- Processa a escolha do usuário
            case opcao of
                "1" -> do
                    novoNome <- getStringValid "Digite o novo nome do cliente: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c novoNome (obterTelefoneCliente c)
                                else c) clientes
                    putStrLn "Nome editado com sucesso!"
                    return clientesAtualizados
                    
                "2" -> do  
                    novoTelefone <- getStringValid "Digite o novo telefone do cliente: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c (obterNomeCliente c) novoTelefone
                                else c) clientes
                    putStrLn "Telefone editado com sucesso!"
                    return clientesAtualizados
                    
                "3" -> do
                    novoNome <- getStringValid "Digite o nome do cliente: "

                    novoTelefone <- getStringValid "Digite o telefone do cliente: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c novoNome novoTelefone
                                else c) clientes
                    putStrLn "Cliente editado com sucesso!"
                    return clientesAtualizados
                    
                "4" -> do
                    putStrLn "Operação cancelada."
                    return clientes
                    
                _ -> do
                    putStrLn "Opção inválida!"
                    return clientes

{- Função para excluir um cliente pelo ID
Descrição: Remove um cliente da lista com base no ID informado.
Entrada: ID do cliente a ser excluído e lista de clientes.
Saída: Lista atualizada de clientes.
-}
excluirCliente :: Int -> [Cliente] -> IO [Cliente]
excluirCliente idExcluir clientes = do
    -- Remove o cliente com o ID informado
    let clientesAtualizados = filter (\c -> obterIdCliente c /= idExcluir) clientes
    if length clientesAtualizados == length clientes
        then do
            putStrLn "Cliente não encontrado!"
            return clientes
        else do
            putStrLn "Cliente excluído com sucesso!"
            return clientesAtualizados