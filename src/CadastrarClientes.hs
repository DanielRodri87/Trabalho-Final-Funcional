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
    let idCliente = gerarIdUnicoCliente clientes
    putStrLn $ "ID généré: " ++ show idCliente
    nome <- getStringValid "Entrez le nom du client: "
    telefone <- getStringValid "Entrez le téléphone du client: "
    -- Create new client with separate arguments
    let cliente = novoCliente idCliente nome telefone
    putStrLn "Client enregistré avec succès!"
    return cliente

{- Função para listar todos os clientes
Descrição: Exibe todos os clientes da lista formatados.
Entrada: Lista de clientes.
Saída: Impressão formatada no console.
-}
listarClientes :: [Cliente] -> IO ()
listarClientes clientes = do
    putStrLn "Liste des Clients:"
    putStrLn "ID\tNom\t\tTéléphone"
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
            putStrLn "Client introuvable!"
            return clientes
        else do
            -- Obtém o cliente e suas informações atuais
            let cliente = head clienteExistente  -- Pegamos o primeiro elemento com segurança
            let nomeAtual = obterNomeCliente cliente
            let telefoneAtual = obterTelefoneCliente cliente

            -- Exibe as informações atuais
            putStrLn "\nInformations actuelles du client:"
            putStrLn $ "ID: " ++ show idEditar
            putStrLn $ "Nom: " ++ nomeAtual
            putStrLn $ "Téléphone: " ++ telefoneAtual
            
            -- Menu de edição
            putStrLn "\nQue souhaitez-vous modifier?"
            putStrLn "1. Nom"
            putStrLn "2. Téléphone"
            putStrLn "3. Les deux"
            putStrLn "4. Annuler"
            putStr "Choisissez une option: "
            opcao <- getLine
            
            -- Processa a escolha do usuário
            case opcao of
                "1" -> do
                    novoNome <- getStringValid "Entrez le nouveau nom du client: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c novoNome (obterTelefoneCliente c)
                                else c) clientes
                    putStrLn "Nom modifié avec succès!"
                    return clientesAtualizados
                    
                "2" -> do  
                    novoTelefone <- getStringValid "Entrez le nouveau numéro de téléphone du client: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c (obterNomeCliente c) novoTelefone
                                else c) clientes
                    putStrLn "Téléphone modifié avec succès!"
                    return clientesAtualizados
                    
                "3" -> do
                    novoNome <- getStringValid "Entrez le nom du client: "
                    novoTelefone <- getStringValid "Entrez le téléphone du client: "
                    let clientesAtualizados = map (\c ->
                            if obterIdCliente c == idEditar
                                then atualizarCliente c novoNome novoTelefone
                                else c) clientes
                    putStrLn "Client modifié avec succès!"
                    return clientesAtualizados
                    
                "4" -> do
                    putStrLn "Opération annulée."
                    return clientes
                    
                _ -> do
                    putStrLn "Option invalide!"
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
            putStrLn "Client introuvable!"
            return clientes
        else do
            putStrLn "Client supprimé avec succès!"
            return clientesAtualizados