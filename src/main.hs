{-| Módulo principal do sistema.

Responsável por inicializar o programa, exibir os menus principais e
delegar as operações para os módulos de clientes, produtos e pedidos.

-}

module Main where

import CadastrarClientes
import Produtos 
import Pedidos (novoPedido, listarPedidos, removerPedido)
import System.IO
import System.Process (callCommand)
import IdUtil (idExiste)
import Control.Concurrent (threadDelay)
import Validacao (getStringValid, getValidInt)
import Tipos (Produto(..), Pedido(..), Identificavel(..))
import Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente)

{-| Limpa a tela do terminal. -}
limparTela :: IO ()
limparTela = callCommand "clear"


{-| Pausa a execução até o usuário pressionar Enter. -}
pausaTerminal :: IO ()
pausaTerminal = do
    putStrLn "\nAppuyez sur Entrée pour continuer..."
    _ <- getLine
    return ()

{-| Verifica se há estoque suficiente para um produto específico.

Parâmetros:
- 'idProduto': ID do produto a verificar
- 'quantidadeDesejada': Quantidade que se deseja comprar
- 'produtos': Lista de produtos para verificar o estoque

Retorna:
- True se há estoque suficiente, False caso contrário
-}
verificarEstoque :: Int -> Int -> [Produto] -> Bool
verificarEstoque idProd qtdDesejada produtos = 
    case filter (\p -> idProduto p == idProd) produtos of
        [] -> False  -- Produto não encontrado
        (p:_) -> quantidadeProduto p >= qtdDesejada

{-| Atualiza o estoque de um produto específico, decrementando a quantidade.

Parâmetros:
- 'idProd': ID do produto a ser atualizado
- 'qtdVendida': Quantidade a ser decrementada
- 'produtos': Lista de produtos a ser atualizada

Retorna:
- Nova lista de produtos com estoque atualizado
-}
atualizarEstoque :: Int -> Int -> [Produto] -> [Produto]
atualizarEstoque idProd qtdVendida produtos = 
    map (\p -> if idProduto p == idProd 
               then p { quantidadeProduto = quantidadeProduto p - qtdVendida }
               else p) produtos

{-| Busca um produto pelo nome na lista de produtos.

Parâmetros:
- 'nomeBusca': Nome do produto a ser buscado
- 'produtos': Lista de produtos onde buscar

Retorna:
- Maybe Produto - Just produto se encontrado, Nothing caso contrário
-}
buscarProdutoPorNome :: String -> [Produto] -> Maybe Produto
buscarProdutoPorNome nomeBusca produtos = 
    case filter (\p -> nomeProduto p == nomeBusca) produtos of
        [] -> Nothing
        (p:_) -> Just p

{-| Exibe arte ASCII temática do restaurante. -}
mostrarLogoRestaurante :: IO ()
mostrarLogoRestaurante = do
    putStrLn "    🐀 Bienvenue au LaRatatouille Bistro 🐀"
    putStrLn "         'Anyone Can Cook!' - Gusteau       "
    putStrLn "    =====================================    "

{-| Exibe decoração para seções do menu. -}
mostrarDecoracao :: String -> IO ()
mostrarDecoracao titulo = do
    putStrLn $ "         " ++ titulo
    putStrLn "    ⚜️  ========================== ⚜️\n"

{-| Exibe o menu principal e direciona para os submenus. -}
menuPrincipal :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuPrincipal clientes produtos pedidos = do
    limparTela
    mostrarLogoRestaurante
    putStrLn "\n    🍷 Menu Principal du Restaurant 🍷"
    putStrLn "    1. 👤 Gestion des clients"
    putStrLn "    2. 🍽️  Menu et Cuisine"
    putStrLn "    3. 📝 Commandes"
    putStrLn "    4. 🚪 Au Revoir"
    putStr "\n    🤵 Votre choix, s'il vous plaît: "
    opcao <- getLine
    case opcao of
        "1" -> menuClientes clientes produtos pedidos
        "2" -> menuProdutos clientes produtos pedidos
        "3" -> menuPedidos clientes produtos pedidos
        "4" -> putStrLn "Programme fermé."
        _   -> do
            putStrLn "Option invalide!"
            pausaTerminal
            menuPrincipal clientes produtos pedidos

{-| Exibe o menu de clientes e executa as operações relacionadas. -}
menuClientes :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuClientes clientes produtos pedidos = do
    limparTela
    mostrarDecoracao "✨ Nos Chers Clients ✨"
    putStrLn "    1. 📝 Nouveau Client"
    putStrLn "    2. ✏️  Modifier Client"
    putStrLn "    3. 📋 Liste des Clients"
    putStrLn "    4. ❌ Supprimer Client"
    putStrLn "    5. 🔙 Retour au Menu Principal"
    putStr "\n    Votre choix: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoCliente <- cadastrarCliente clientes  -- Pass current clients list
            pausaTerminal
            menuClientes (clientes ++ [novoCliente]) produtos pedidos
        "2" -> do
            id <- getValidInt "Entrez l'ID du client que vous souhaitez modifier: " 
            clientesAtualizados <- editarCliente id clientes
            pausaTerminal
            menuClientes clientesAtualizados produtos pedidos
        "3" -> do
            listarClientes clientes
            pausaTerminal
            menuClientes clientes produtos pedidos
        "4" -> do
            id <- getValidInt "Saisissez l'ID du client que vous souhaitez supprimer: "
            clientesAtualizados <- excluirCliente id clientes
            pausaTerminal
            menuClientes clientesAtualizados produtos pedidos
        "5" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Option invalide!"
            pausaTerminal
            menuClientes clientes produtos pedidos

{-| Exibe o menu de produtos e executa as operações relacionadas. -}
menuProdutos :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuProdutos clientes produtos pedidos = do
    limparTela
    mostrarDecoracao "🍲 La Carte du Chef 🍲"
    putStrLn "    1. 🆕 Ajouter un Plat"
    putStrLn "    2. ✏️  Modifier un Plat"
    putStrLn "    3. 📜 Voir le Menu"
    putStrLn "    4. ❌ Retirer un Plat"
    putStrLn "    5. 🔙 Retour au Menu Principal"
    putStr "\n    Votre choix: "
    opcao <- getLine
    case opcao of
        "1" -> do
            novoProd <- novoProduto produtos
            let produtosAtualizados = produtos ++ [novoProd]
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "2" -> do
            id <- getValidInt "Entrez l'ID du produit que vous souhaitez modifier: "
            produtosAtualizados <- editarProduto id produtos
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "3" -> do
            listarProdutos produtos
            pausaTerminal
            menuProdutos clientes produtos pedidos
        "4" -> do
            id <- getValidInt "Entrez l'ID du produit que vous souhaitez supprimer: "
            produtosAtualizados <- excluirProduto id produtos
            pausaTerminal
            menuProdutos clientes produtosAtualizados pedidos
        "5" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Option invalide!"
            pausaTerminal
            menuProdutos clientes produtos pedidos

{-| Solicita e cadastra um novo pedido, validando IDs de cliente e produto.

Retorna um pedido válido ou um pedido inválido (-1, "", 0) caso haja erro.
-}
novoPedidoSeguro :: [Cliente] -> [Produto] -> [Pedido] -> IO Pedido
novoPedidoSeguro clientes produtos pedidos = do
    -- Perguntar se deseja ver a lista de clientes
    putStr "Souhaitez-vous afficher la liste des clients? (O/N): "
    verClientes <- getLine
    if verClientes `elem` ["O", "o"]
        then listarClientes clientes else return ()
    putStr "Entrez l'ID du client: "
    idClienteStr <- getLine
    let idCliente = read idClienteStr :: Int
    if not (idExiste idCliente clientes) 
        then do
            putStrLn "ID client invalide ! Commande non enregistrée."
            return $ Pedido (-1) "" 0
        else do
            -- Perguntar se deseja ver a lista de produtos
            putStr "Voulez-vous consulter la liste des produits enregistrés? (O/N): "
            verProdutos <- getLine
            if verProdutos `elem` ["O", "o"]
                then listarProdutos produtos else return ()
            
            idProdutoEscolhido <- getValidInt "Entrez l'identifiant du produit: "
           
            if not (idExiste idProdutoEscolhido produtos) 
                then do
                    putStrLn "ID produit invalide ! Commande non enregistrée."
                    return $ Pedido (-1) "" 0
                else do
                    quantidade <- getValidInt "Entrez la quantité: "
                    
                    -- Verificar se há estoque suficiente ANTES de criar o pedido
                    if not (verificarEstoque idProdutoEscolhido quantidade produtos)
                        then do
                            putStrLn "Quantité indisponible en stock ! Commande non enregistrée."
                            return $ Pedido (-1) "" 0
                        else do
                            -- Buscar nome do produto pelo ID
                            let nomeProdutoPedido = case filter (\p -> idProduto p == idProdutoEscolhido) produtos of
                                    (p:_) -> nomeProduto p
                                    _     -> ""
                            putStrLn "Demande enregistrée avec succès dans la file d'attente!"
                            return $ Pedido idCliente nomeProdutoPedido quantidade

{-| Processa o pagamento de acordo com o método escolhido.

Parâmetros:
- 'valorTotal': Valor total a ser pago
Retorna:
- True se o pagamento foi bem sucedido, False caso contrário
-}
realizarPagamento :: Float -> IO Bool
realizarPagamento valorTotal = do
    mostrarDecoracao "💶 Mode de Paiement 💶"
    putStrLn $ "    Total à payer: " ++ show valorTotal ++ " €"
    putStrLn "\n    1. 💳 Carte Bancaire"
    putStrLn "    2. 📱 PIX"
    putStrLn "    3. 💵 Espèces"
    putStr "\n    Votre choix: "
    metodo <- getLine
    case metodo of
        "1" -> do
            putStrLn "\nTraitement du paiement par carte..."
            putStrLn "Paiement approuvé!"
            return True
        "2" -> do
            putStrLn "\nClé PIX: 123.456.789-00"
            putStrLn "En attente de confirmation du paiement..."
            putStr "Paiement reçu? (O/N): "
            confirmacao <- getLine
            return $ confirmacao `elem` ["O", "o"]
        "3" -> do
            putStr "\nMontant reçu en espèces: € "
            valorRecebidoStr <- getLine
            let valorRecebido = read valorRecebidoStr :: Float
            if valorRecebido < valorTotal
                then do
                    putStrLn "Montant insuffisant!"
                    return False
                else do
                    let troco = valorRecebido - valorTotal
                    putStrLn $ "Monnaie: € " ++ show troco
                    return True
        _ -> do
            putStrLn "Option invalide!"
            return False

{-| Processa o pagamento de um pedido, removendo da fila e atualizando estoque.

Parâmetros:
- 'idCliente': ID do cliente cujo pedido será processado
- 'pedidos': Lista atual de pedidos
- 'produtos': Lista atual de produtos

Retorna:
- Tupla contendo (pedidos atualizados, produtos com estoque atualizado, sucesso)
-}
processarPagamento :: Int -> [Pedido] -> [Produto] -> IO ([Pedido], [Produto], Bool)
processarPagamento idCliente pedidos produtos = do
    case pedidos of
        [] -> do
            putStrLn "La file d'attente est vide!"
            return (pedidos, produtos, False)
        _ -> do
            let (antes, depois) = span (\p -> idClientePedido p /= idCliente) pedidos
            case depois of
                [] -> do
                    putStrLn "Commande introuvable dans la file d'attente!"
                    return (pedidos, produtos, False)
                (pedido:resto) -> do
                    -- Buscar o produto correspondente ao pedido
                    case buscarProdutoPorNome (nomeProdutoPedido pedido) produtos of
                        Nothing -> do
                            putStrLn "Produit non trouvé dans le stock!"
                            return (pedidos, produtos, False)
                        Just produto -> do
                            let qtdPedido = quantidadePedido pedido
                            let idProd = idProduto produto
                            let valorTotal = precoProduto produto * fromIntegral qtdPedido
                            
                            -- Verificar novamente se há estoque (por segurança)
                            if not (verificarEstoque idProd qtdPedido produtos)
                                then do
                                    putStrLn "Stock insuffisant pour effectuer le paiement!"
                                    return (pedidos, produtos, False)
                                else do
                                    putStrLn $ "\nRésumé de la commande:"
                                    putStrLn $ "Produit: " ++ nomeProdutoPedido pedido
                                    putStrLn $ "Quantité: " ++ show qtdPedido
                                    putStrLn $ "Prix unitaire: € " ++ show (precoProduto produto)
                                    putStrLn $ "Montant total: € " ++ show valorTotal
                                    
                                    -- Processar pagamento
                                    pagamentoOk <- realizarPagamento valorTotal
                                    
                                    if not pagamentoOk
                                        then do
                                            putStrLn "Paiement non effectué!"
                                            return (pedidos, produtos, False)
                                        else do
                                            -- Atualizar estoque
                                            let produtosAtualizados = atualizarEstoque idProd qtdPedido produtos
                                            -- Remover pedido da fila
                                            let pedidosAtualizados = antes ++ resto
                                            
                                            putStrLn "Paiement effectué avec succès!"
                                            return (pedidosAtualizados, produtosAtualizados, True)

{-| Exibe o menu de pedidos e executa as operações relacionadas. -}
menuPedidos :: [Cliente] -> [Produto] -> [Pedido] -> IO ()
menuPedidos clientes produtos pedidos = do
    limparTela
    mostrarDecoracao "📋 Les Commandes 📋"
    putStrLn "    1. 🆕 Nouvelle Commande"
    putStrLn "    2. 📜 Liste des Commandes"
    putStrLn "    3. 💶 Paiement"
    putStrLn "    4. 🔙 Retour au Menu Principal"
    putStr "\n    Votre choix: "
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
            idCliente <- getValidInt "Saisissez l'identifiant client de la commande pour effectuer le paiement: "
            (pedidosAtualizados, produtosAtualizados, sucesso) <- processarPagamento idCliente pedidos produtos
            pausaTerminal
            menuPedidos clientes produtosAtualizados pedidosAtualizados
        "4" -> menuPrincipal clientes produtos pedidos
        _   -> do
            putStrLn "Option invalide!"
            pausaTerminal
            menuPedidos clientes produtos pedidos

{-| Exibe o texto de carregamento em francês com animação. -}
mostrarCarregando :: IO ()
mostrarCarregando = do
    let ratatouilleArt = unlines [
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡤⠖⠂⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣄⣮⣠⠦⠄⡀⠀⠀⠀⠀⠀⢀⣤⠟⣡⡴⠒⠈⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⣿⡿⠛⠛⢷⣶⣮⢯⡿⣽⣦⣤⣜⡿⢿⠦⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣐⣿⣿⠀⣴⣦⢼⣼⣻⣿⣿⣿⣿⣿⡝⡼⢩⠆⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣰⠖⠶⠵⣪⣕⣺⣿⣿⣿⣧⣽⣾⣾⣶⣿⣿⣿⣽⣿⣿⣿⣿⡖⠇⠊⣠⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠘⢍⠢⡑⠤⣉⠥⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⠀⠀⡸⢡⡣⢤⣀⠄",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⠷⡼⣰⢢⣏⣿⣿⣿⣿⣿⣿⣿⣮⣛⣡⡀⣀⠀⠀⢀⣸⣷⣬⠆⠉⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢦⣿⣿⣿⣿⣿⣿⣮⣛⣡⡀⣀⠀⠀⢀⣸⣷⣬⠆⠉⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣢⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⣻⢷⣳⣛⣿⣿⡯⠃⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⢢⣾⣿⣿⣿⣿⣿⣿⣿⣿⢯⣟⣷⢯⢿⣹⣟⣾⣿⡿⢿⠑⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢂⣶⣿⣿⣿⣿⣿⣿⣿⣿⣯⣟⣿⢫⣟⣻⡞⣿⠻⠅⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡠⢳⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⢯⡧⢉⠆⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢰⣸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⣿⢾⠁⡃⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⣴⣿⣿⣿⣿⣿⣿⣿⣯⡙⣿⣿⣿⡻⡯⡀⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡎⣿⣿⣿⣿⣿⣿⣿⣿⣷⢡⡇⣿⣯⡷⣣⠓⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⣷⣿⣿⣿⣿⣿⣿⡗⡯⣟⡼⣽⣓⢽⣜⣋⡜⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠐⣎⣿⣿⣿⣿⣿⡗⡯⣟⡼⣽⣓⢽⣜⣋⡜⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢸⣿⣿⣿⣿⣿⡗⡯⣟⡼⣽⣓⢽⣜⣋⡜⠚⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠠⢹⣿⣿⣿⣿⣯⣯⢧⡿⢺⡿⣽⣞⣯⡽⣎⡝⠄⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠀⠀⠀⡀⢀⠠⡄⣤⣴⣲⠶⠷⠿⣿⣿⣿⣿⣷⣻⣜⣹⣿⣿⣻⣾⣯⣿⣿⣿⠪⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⢴⡠⠵⠒⠚⠉⠉⠈⠁⠀⠀⠀⠐⠈⠻⣿⣿⣿⣿⣿⣿⡿⠳⠿⠿⠿⠿⣻⣉⣪⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀",
            "⠁⠒⠡⠄⠐⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠊⠛⢭⣹⢆⣩⠀⠈⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀"
            ]
    
    mapM_ (\i -> do
        limparTela
        putStrLn ratatouilleArt
        putStrLn $ "\n    Chargement du système" ++ replicate (i `mod` 4) '.'
        putStrLn "\n    (Loading system...)"
        threadDelay 1000000  -- Pausa de 1 segundo
        ) [1..13]  -- Repete 13 vezes

{-| Função principal do programa. Inicia o menu principal com listas vazias. -}
main :: IO ()
main = do
    limparTela
    mostrarCarregando  -- Mostra a tela de carregamento animada
    limparTela
    mostrarLogoRestaurante
    putStrLn "\n    🎭 Bienvenue au Système de Gestion LaRatatouille 🎭"
    putStrLn     "    ============================================="
    putStrLn "    Le meilleur bistro de Paris!"
    putStrLn "    'Anyone can cook!' - Auguste Gusteau"
    putStrLn "\n    Appuyez sur Entrée pour continuer..."
    _ <- getLine
    menuPrincipal [] [] []