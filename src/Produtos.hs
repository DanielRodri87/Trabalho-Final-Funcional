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
    nome <- getStringValid "Digite o nome do produto: "
     
    qtd <- getValidInt "Digite a quantidade do produto: "

    precoStr <- getStringValid "Digite o preço do produto:"
    let preco = read precoStr :: Float

    controle <- getStringValid "Digite o controle/categoria do produto:"

    -- Gera um ID único entre 100 e 999
    let id = gerarIdUnicoProduto produtos
    
    let produto = Produto id nome qtd preco controle
    putStrLn $ "Produto cadastrado com sucesso! ID: " ++ show id
    return produto

{-| Lista todos os produtos presentes.

Exibe no terminal uma tabela formatada contendo:
ID, nome, quantidade, preço e controle/categoria.
-}
listarProdutos :: [Produto] -> IO ()
listarProdutos produtos = do
    putStrLn "Lista de Produtos:"
    putStrLn "ID\tNome\t\tQuantidade\tPreço\t\tControle"
    putStrLn "--------------------------------------------------------------"
    mapM_ (\p -> 
        putStrLn $ show (idProduto p) ++ "\t" ++ nomeProduto p ++ "\t\t" ++ 
        show (quantidadeProduto p) ++ "\t\t" ++ show (precoProduto p) ++ "\t\t" ++ controleProduto p) produtos

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
            putStrLn "Produto não encontrado!"
            return produtos
        else do
            let produto = head produtoExistente
            let nomeAtual = nomeProduto produto
            let qtdAtual = quantidadeProduto produto
            let precoAtual = precoProduto produto
            let controleAtual = controleProduto produto
            
            putStrLn $ "Produto atual: ID=" ++ show idEditar ++ 
                      ", Nome=" ++ nomeAtual ++ 
                      ", Quantidade=" ++ show qtdAtual ++
                      ", Preço=" ++ show precoAtual ++
                      ", Controle=" ++ controleAtual
            
             
            novoNome <- getStringValid "Digite o novo nome do produto (ou enter para manter):"
            
            novaQtd <- getValidInt "Digite a nova quantidade do produto (ou enter para manter):"
            
            novoPrecoStr <- getStringValid "Digite o novo preco do produto (ou enter para manter):"
            let novoPreco = read novoPrecoStr :: Float
            
            novoControle <- getStringValid "Digite o novo controle do produto (ou enter para manter):"

            let produtosAtualizados = map (\p ->
                    if idProduto p == idEditar
                        then p {
                            nomeProduto = novoNome, quantidadeProduto = novaQtd, 
                            precoProduto = novoPreco, controleProduto = novoControle
                            }
                        else p) produtos

            putStrLn "Produto editado com sucesso!"
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
            putStrLn "Produto não encontrado!"
            return produtos
        else do
            let produto = head produtoParaExcluir
            putStrLn $ "Produto a ser excluído: " ++ nomeProduto produto
            
            putStr "Confirmar exclusão (S/N)? "
            confirmacao <- getLine
            
            if confirmacao == "S" || confirmacao == "s"
                then do
                    let produtosAtualizados = filter (\p -> idProduto p /= idExcluir) produtos
                    putStrLn "Produto excluído com sucesso!"
                    return produtosAtualizados
                else do
                    putStrLn "Exclusão cancelada."
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
    putStrLn $ "Produtos salvos com sucesso em " ++ arquivo

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

