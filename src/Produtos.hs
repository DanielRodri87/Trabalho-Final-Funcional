module Produtos where

import System.IO
import Control.Exception (catch, IOException)
import IdUtil (gerarIdUnicoProduto)

type Produto = (Int, String, Int, Float, String)

novoProduto :: [Produto] -> IO Produto
novoProduto produtos = do
    putStrLn "Digite o Nome do produto:"
    nome <- getLine

    putStrLn "Digite a Quantidade do produto:"
    qtdStr <- getLine
    let qtd = read qtdStr :: Int

    putStrLn "Digite o Preço do produto:"
    precoStr <- getLine
    let preco = read precoStr :: Float

    putStrLn "Digite o Controle/Categoria do produto:"
    controle <- getLine

    -- Gera um ID único entre 100 e 999
    let id = gerarIdUnicoProduto produtos
    
    let produto = (id, nome, qtd, preco, controle)
    putStrLn $ "Produto cadastrado com sucesso! ID: " ++ show id
    return produto

listarProdutos :: [Produto] -> IO ()
listarProdutos produtos = do
    putStrLn "Lista de Produtos:"
    putStrLn "ID\tNome\t\tQuantidade\tPreço\t\tControle"
    putStrLn "--------------------------------------------------------------"
    mapM_ (\(id, nome, qtd, preco, controle) -> 
        putStrLn $ show id ++ "\t" ++ nome ++ "\t\t" ++ 
        show qtd ++ "\t\t" ++ show preco ++ "\t\t" ++ controle) produtos

editarProduto :: Int -> [Produto] -> IO [Produto]
editarProduto idEditar produtos = do
    let produtoExistente = filter (\(id, _, _, _, _) -> id == idEditar) produtos
    if null produtoExistente
        then do
            putStrLn "Produto não encontrado!"
            return produtos
        else do
            let (_, nomeAtual, qtdAtual, precoAtual, controleAtual) = head produtoExistente
            
            putStrLn $ "Produto atual: ID=" ++ show idEditar ++ 
                      ", Nome=" ++ nomeAtual ++ 
                      ", Quantidade=" ++ show qtdAtual ++
                      ", Preço=" ++ show precoAtual ++
                      ", Controle=" ++ controleAtual
            
            putStrLn "Digite o novo Nome do produto (ou Enter para manter):"
            novoNome <- getLine
            let nomeAtualizado = if null novoNome then nomeAtual else novoNome
            
            putStrLn "Digite a nova Quantidade (ou Enter para manter):"
            novaQtdStr <- getLine
            let qtdAtualizada = if null novaQtdStr then qtdAtual else read novaQtdStr :: Int
            
            putStrLn "Digite o novo Preço (ou Enter para manter):"
            novoPrecoStr <- getLine
            let precoAtualizado = if null novoPrecoStr then precoAtual else read novoPrecoStr :: Float
            
            putStrLn "Digite o novo Controle (ou Enter para manter):"
            novoControle <- getLine
            let controleAtualizado = if null novoControle then controleAtual else novoControle

            let produtosAtualizados = map (\(id, nome, qtd, preco, controle) ->
                    if id == idEditar
                        then (id, nomeAtualizado, qtdAtualizada, precoAtualizado, controleAtualizado)
                        else (id, nome, qtd, preco, controle)) produtos

            putStrLn "Produto editado com sucesso!"
            return produtosAtualizados

excluirProduto :: Int -> [Produto] -> IO [Produto]
excluirProduto idExcluir produtos = do
    let produtoParaExcluir = filter (\(id, _, _, _, _) -> id == idExcluir) produtos
    if null produtoParaExcluir
        then do
            putStrLn "Produto não encontrado!"
            return produtos
        else do
            let (_, nome, _, _, _) = head produtoParaExcluir
            putStrLn $ "Produto a ser excluído: " ++ nome
            
            putStr "Confirmar exclusão (S/N)? "
            confirmacao <- getLine
            
            if confirmacao == "S" || confirmacao == "s"
                then do
                    let produtosAtualizados = filter (\(id, _, _, _, _) -> id /= idExcluir) produtos
                    putStrLn "Produto excluído com sucesso!"
                    return produtosAtualizados
                else do
                    putStrLn "Exclusão cancelada."
                    return produtos

salvarProdutos :: [Produto] -> FilePath -> IO ()
salvarProdutos produtos arquivo = do
    writeFile arquivo (show produtos)
    putStrLn $ "Produtos salvos com sucesso em " ++ arquivo

carregarProdutos :: FilePath -> IO [Produto]
carregarProdutos arquivo = do
    conteudo <- readFile arquivo `catch` (\(_ :: IOException) -> return "[]")
    let produtos = read conteudo :: [Produto]
    return produtos

