{-| Este módulo fornece funcionalidades para criar, listar,
editar, excluir, salvar e carregar produtos. Ele interage com o
usuário via terminal e faz persistência dos dados em arquivos.

O módulo depende dos seguintes módulos auxiliares:

- 'Tipos'     : Define o tipo 'Produto' e a classe 'Identificavel'.
- 'Validacao' : Fornece funções de validação de entrada.
- 'IdUtil'    : Geração de IDs únicos para produtos.

-}

module Produtos where

import Validacao (getStringValid, getValidInt)
import Tipos (Identificavel(..), Produto(..))


import System.IO
import Control.Exception (catch, IOException)
import IdUtil (gerarIdUnicoProduto)


{-| Cadastra um novo produto, solicitando dados ao usuário.

Recebe a lista atual de produtos apenas para contexto,
mas não altera essa lista.

Retorna o produto criado.
-}
novoProduto :: [Produto] -> IO Produto
novoProduto produtos = do
    nome <- getStringValid "Entrez le nom du produit: "
     
    qtd <- getValidInt "Entrez la quantité du produit: "

    precoStr <- getStringValid "Entrez le prix du produit:"
    let preco = read precoStr :: Float

    controle <- getStringValid "Entrez la catégorie du produit:"

    -- Gera um ID único entre 100 e 999
    let id = gerarIdUnicoProduto produtos
    
    let produto = Produto id nome qtd preco controle
    putStrLn $ "Produit enregistré avec succès! ID: " ++ show id
    return produto

{-| Lista todos os produtos presentes.

Exibe no terminal uma tabela formatada contendo:
ID, nome, quantidade, preço e controle/categoria.
-}
listarProdutos :: [Produto] -> IO ()
listarProdutos produtos = do
    putStrLn "\n    🍽️  Menu du LaRatatouille Bistro 🍽️"
    putStrLn "    =================================="
    putStrLn "    ID   |  Plat          |  Prix   | Stock  | Catégorie"
    putStrLn "    ------------------------------------------------"
    mapM_ (\p -> 
        putStrLn $ "    " ++ show (idProduto p) ++ 
                   "  |  " ++ padRight 12 (nomeProduto p) ++ 
                   "  |  €" ++ padLeft 5 (show (precoProduto p)) ++ 
                   "  |   " ++ padLeft 3 (show (quantidadeProduto p)) ++ 
                   "   | " ++ controleProduto p) produtos
    putStrLn "    ------------------------------------------------"
    where
        padRight n str = take n (str ++ repeat ' ')
        padLeft n str = take n (reverse (take n (reverse str ++ repeat ' ')))

{-| Busca um produto pelo ID na lista.

Parâmetros:
- 'idBusca': ID do produto a ser buscado
- 'produtos': Lista de produtos onde buscar

Retorna:
- Maybe Produto - Just produto se encontrado, Nothing caso contrário
-}
buscarProdutoPorId :: Int -> [Produto] -> Maybe Produto
buscarProdutoPorId idBusca produtos = 
    case filter (\p -> idProduto p == idBusca) produtos of
        [] -> Nothing
        (p:_) -> Just p

{-| Verifica se um produto tem estoque suficiente.

Parâmetros:
- 'idProd': ID do produto
- 'qtdDesejada': Quantidade desejada
- 'produtos': Lista de produtos

Retorna:
- True se há estoque suficiente, False caso contrário
-}
temEstoqueSuficiente :: Int -> Int -> [Produto] -> Bool
temEstoqueSuficiente idProd qtdDesejada produtos =
    case buscarProdutoPorId idProd produtos of
        Nothing -> False
        Just produto -> quantidadeProduto produto >= qtdDesejada

{-| Atualiza a quantidade de um produto específico.

Parâmetros:
- 'idProd': ID do produto a ser atualizado
- 'novaQtd': Nova quantidade do produto
- 'produtos': Lista de produtos

Retorna:
- Nova lista de produtos com quantidade atualizada
-}
atualizarQuantidadeProduto :: Int -> Int -> [Produto] -> [Produto]
atualizarQuantidadeProduto idProd novaQtd produtos =
    map (\p -> if idProduto p == idProd 
               then p { quantidadeProduto = novaQtd }
               else p) produtos

{-| Decrementa a quantidade de um produto específico.

Parâmetros:
- 'idProd': ID do produto
- 'qtdDecremento': Quantidade a ser decrementada
- 'produtos': Lista de produtos

Retorna:
- Nova lista de produtos com quantidade decrementada
-}
decrementarQuantidadeProduto :: Int -> Int -> [Produto] -> [Produto]
decrementarQuantidadeProduto idProd qtdDecremento produtos =
    map (\p -> if idProduto p == idProd 
               then p { quantidadeProduto = max 0 (quantidadeProduto p - qtdDecremento) }
               else p) produtos

{-| Edita um produto existente na lista, identificado pelo ID.

Solicita novos dados ao usuário. Se o produto não for encontrado,
a lista permanece inalterada.

Parâmetros:
- 'idEditar': ID do produto a ser editado.
- Lista de produtos.

Retorna:
- Nova lista de produtos, com o produto editado.
-}
editarProduto :: Int -> [Produto] -> IO [Produto]
editarProduto idEditar produtos = do
    let produtoExistente = filter (\p -> obterID p == idEditar) produtos
    if null produtoExistente
        then do
            putStrLn "Produit introuvable!"
            return produtos
        else do
            let produto = head produtoExistente
            let nomeAtual = nomeProduto produto
            let qtdAtual = quantidadeProduto produto
            let precoAtual = precoProduto produto
            let controleAtual = controleProduto produto

            putStrLn "\n═════════════════════════════════════════════════"
            putStrLn "         🟢 Produit trouvé!"
            putStrLn "═════════════════════════════════════════════════"
            putStrLn $ "ID:         " ++ show idEditar
            putStrLn $ "Nom:        " ++ nomeAtual
            putStrLn $ "Quantité:   " ++ show qtdAtual
            putStrLn $ "Prix:       € " ++ show precoAtual
            putStrLn $ "Catégorie:  " ++ controleAtual
            putStrLn "═════════════════════════════════════════════════\n"

            -- Solicita novos dados, permitindo manter os atuais
            putStrLn "Appuyez sur Entrée pour garder la valeur actuelle."
            putStr $ "Nouveau nom du produit [" ++ nomeAtual 
            ++ "]: "
            hFlush stdout
            novoNome <- getLine
            let nomeFinal = if null novoNome then nomeAtual else novoNome

            putStr $ "Nouvelle quantité du produit [" ++ show qtdAtual ++ "]: "
            hFlush stdout
            qtdStr <- getLine
            let qtdFinal = if null qtdStr then qtdAtual else (read qtdStr :: Int)

            putStr $ "Nouveau prix du produit [" ++ show precoAtual ++ "]: "
            hFlush stdout
            precoStr <- getLine
            let precoFinal = if null precoStr then precoAtual else (read precoStr :: Float)

            putStr $ "Nouvelle catégorie du produit [" ++ controleAtual ++ "]: "
            hFlush stdout
            novoControle <- getLine
            let controleFinal = if null novoControle then controleAtual else novoControle

            let produtosAtualizados = map (\p ->
                    if idProduto p == idEditar
                        then p {
                            nomeProduto = nomeFinal, quantidadeProduto = qtdFinal,
                            precoProduto = precoFinal, controleProduto = controleFinal
                            }
                        else p) produtos

            putStrLn "Produit modifié avec succès!"
            return produtosAtualizados

{-| Exclui um produto da lista, identificado pelo ID.

Solicita confirmação ao usuário antes de excluir.

Parâmetros:
- 'idExcluir': ID do produto a ser excluído.
- Lista de produtos.

Retorna:
- Nova lista de produtos, sem o produto excluído.
-}
excluirProduto :: Int -> [Produto] -> IO [Produto]
excluirProduto idExcluir produtos = do
    let produtoParaExcluir = filter (\p -> obterID p == idExcluir) produtos
    if null produtoParaExcluir
        then do
            putStrLn "Produit introuvable!"
            return produtos
        else do
            let produto = head produtoParaExcluir
            putStrLn $ "Produit à supprimer: " ++ nomeProduto produto
            
            putStr "Confirmer la suppression (O/N)? "
            confirmacao <- getLine
            
            if confirmacao == "O" || confirmacao == "o"
                then do
                    let produtosAtualizados = filter (\p -> idProduto p /= idExcluir) produtos
                    putStrLn "Produit supprimé avec succès!"
                    return produtosAtualizados
                else do
                    putStrLn "Suppression annulée."
                    return produtos

{-| Salva a lista de produtos em um arquivo.

Os produtos são serializados usando a representação 'show'.

Parâmetros:
- 'produtos': Lista de produtos a ser salva.
- 'arquivo': Caminho do arquivo onde os dados serão salvos.
-}
salvarProdutos :: [Produto] -> FilePath -> IO ()
salvarProdutos produtos arquivo = do
    writeFile arquivo (show produtos)
    putStrLn $ "Produits enregistrés avec succès dans " ++ arquivo

{-| Carrega a lista de produtos de um arquivo.

Caso o arquivo não exista ou ocorra um erro de leitura,
retorna uma lista vazia.

Parâmetro:
- 'arquivo': Caminho do arquivo a ser lido.

Retorna:
- Lista de produtos carregados.
-}
carregarProdutos :: FilePath -> IO [Produto]
carregarProdutos arquivo = do
    conteudo <- readFile arquivo `catch` (\(_ :: IOException) -> return "[]")
    let produtos = read conteudo :: [Produto]
    return produtos