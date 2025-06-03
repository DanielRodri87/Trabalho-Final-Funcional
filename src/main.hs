module Main where

import CadastrarClientes
import Produtos 
import Pedidos (novoPedido, listarPedidos, removerPedido)
import System.IO
import System.Process (callCommand)
import IdUtil (idExiste)

import Validacao (getStringValid, getValidInt)
import Tipos (Produto(..), Pedido(..), Identificavel(..))
import Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente)



limparTela :: IO ()
limparTela = callCommand "clear"

pausaTerminal :: IO ()
pausaTerminal = do
    putStrLn "\nPressione Enter para continuar..."
    _ <- getLine
    return ()

menuPrincipal :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuPrincipal clientes produtos pedidos = do
    limparTela
    putStrLn "\nMenu Principal"
    putStrLn "1. Menu clientes"
    putStrLn "2. Menu produtos"
    putStrLn "3. Fila de Pedidos"
    putStrLn "4. Sair"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> menuClientes clientes produtos pedidos
        "2" -> menuProdutos clientes produtos pedidos
        "3" -> menuPedidos clientes produtos pedidos
        "4" -> putStrLn "Programa encerrado."
        _   -> do
            putStrLn "Opção inválida!"
            pausaTerminal
            menuPrincipal clientes produtos pedidos

menuClientes :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuClientes clientes produtos pedidos = do
    limparTela
    putStrLn "\nMenu de Clientes"
    putStrLn "1. Cadastrar cliente"
    putStrLn "2. Editar cliente"
    putStrLn "3. Exibir clientes cadastrados"
    putStrLn "4. Deletar cliente"
    putStrLn "5. Voltar ao menu principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoCliente <- cadastrarCliente clientes  -- Pass current clients list
            pausaTerminal
            menuClientes (clientes ++ [novoCliente]) produtos pedidos
        "2" -> do
            
            id <- getValidInt "Digite o ID do cliente que deseja editar: " 
            clientesAtualizados <- editarCliente id clientes
            pausaTerminal
            menuClientes clientesAtualizados produtos pedidos
        "3" -> do
            listarClientes clientes
            pausaTerminal
            menuClientes clientes produtos pedidos
        "4" -> do
            id <- getValidInt "Digite o ID do cliente que deseja excluir: "
            clientesAtualizados <- excluirCliente id clientes
            pausaTerminal
            menuClientes clientesAtualizados produtos pedidos
        "5" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Opção inválida!"
            pausaTerminal
            menuClientes clientes produtos pedidos

menuProdutos :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuProdutos clientes produtos pedidos = do
    limparTela
    putStrLn "\nMenu de Produtos"
    putStrLn "1. Cadastrar produto"
    putStrLn "2. Editar produto"
    putStrLn "3. Exibir produtos cadastrados"
    putStrLn "4. Deletar produto"
    putStrLn "5. Voltar ao menu principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoProd <- novoProduto produtos
            let produtosAtualizados = produtos ++ [novoProd]
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "2" -> do
            id <- getValidInt "Digite o ID do produto que deseja editar: "
            produtosAtualizados <- editarProduto id produtos
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "3" -> do
            listarProdutos produtos
            pausaTerminal
            menuProdutos clientes produtos pedidos
        "4" -> do
            
            id <- getValidInt "Digite o ID do produto que deseja excluir: "

            produtosAtualizados <- excluirProduto id produtos
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "5" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Opção inválida!"
            pausaTerminal
            menuProdutos clientes produtos pedidos

novoPedidoSeguro :: [Cliente] -> [Produto] -> [Pedido] -> IO Pedido
novoPedidoSeguro clientes produtos pedidos = do
    -- Perguntar se deseja ver a lista de clientes
    putStr "Deseja visualizar a lista de clientes cadastrados? (S/N): "
    verClientes <- getLine
    if verClientes `elem` ["S", "s"]
        then listarClientes clientes else return ()
    putStr "Digite o ID do cliente: "
    idClienteStr <- getLine
    let idCliente = read idClienteStr :: Int
    if not (idExiste idCliente clientes) 
        then do
            putStrLn "ID de cliente inválido! Pedido não cadastrado."
            return $ Pedido (-1) "" 0
        else do
            -- Perguntar se deseja ver a lista de produtos
            putStr "Deseja visualizar a lista de produtos cadastrados? (S/N): "
            verProdutos <- getLine
            if verProdutos `elem` ["S", "s"]
                then listarProdutos produtos else return ()
            
            idProdutoEscolhido <- getValidInt "Digite o ID do produto: "
           
            if not (idExiste idProdutoEscolhido produtos) 
                then do
                    putStrLn "ID de produto inválido! Pedido não cadastrado."
                    return $ Pedido (-1) "" 0
                else do
                    putStr "Digite a quantidade: "
                    qtdStr <- getLine
                    let quantidade = read qtdStr :: Int
                    -- Buscar nome do produto pelo ID
                    let nomeProdutoPedido = case filter (\p -> idProduto p == idProdutoEscolhido) produtos of
                            (p:_) -> nomeProduto p
                            _     -> ""
                    putStrLn "Pedido cadastrado na fila com sucesso!"
                    return $ Pedido idCliente nomeProdutoPedido quantidade

menuPedidos :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuPedidos clientes produtos pedidos = do
    limparTela
    putStrLn "\nMenu de Pedidos"
    putStrLn "1. Cadastrar pedido"
    putStrLn "2. Exibir fila de pedidos"
    putStrLn "3. Remover pedido da fila (pagamento)"
    putStrLn "4. Voltar ao menu principal"
    putStr "Escolha uma opção: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoPed <- novoPedidoSeguro clientes produtos pedidos
            if novoPed == Pedido (-1) "" 0
                then pausaTerminal >> menuPedidos clientes produtos pedidos
                else pausaTerminal >> menuPedidos clientes produtos (pedidos ++ [novoPed])
        "2" -> do
            listarPedidos pedidos
            pausaTerminal
            menuPedidos clientes produtos pedidos
        "3" -> do
            putStrLn "Digite o ID do cliente do pedido a remover (pagamento):"
            idStr <- getLine
            let idCliente = read idStr :: Int
            let (removidos, filaRestante) = case pedidos of
                    [] -> ([], [])
                    xs -> let (r, f) = span (\p -> idClientePedido p /= idCliente) xs in
                          if null f then ([], xs) else ([head f], r ++ tail f)
            if null removidos
                then putStrLn "Pedido não encontrado na fila!"
                else putStrLn "Pedido removido da fila (pagamento realizado)!"
            pausaTerminal
            menuPedidos clientes produtos filaRestante
        "4" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Opção inválida!"
            pausaTerminal
            menuPedidos clientes produtos pedidos

main :: IO ()
main = menuPrincipal [] [] []