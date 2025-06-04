{-| Este módulo fornece funcionalidades para criar, listar,
salvar, carregar e remover pedidos. Ele interage com o
usuário via terminal e faz persistência dos dados em arquivos.

O módulo depende dos seguintes módulos auxiliares:

- 'Tipos'     : Define o tipo 'Pedido'.
- 'Validacao' : Fornece funções de validação de entrada.

-}

module Pedidos where

import System.IO
import Control.Exception (catch, IOException)
import Tipos (Pedido(..))
import Validacao (getStringValid, getValidInt)


{-| Cadastra um novo pedido, solicitando dados ao usuário.
Recebe a lista atual de pedidos apenas para contexto,
mas não altera essa lista.

Retorna o pedido criado.
-}
novoPedido :: [Pedido] -> IO Pedido
novoPedido pedidos = do
    idCliente <- getValidInt "Digite o ID do cliente:" 
 
    nomeProduto <- getStringValid "Digite o nome do produto:"
  
    qtd <- getValidInt "Digite a quantidade:"  

    let pedido = Pedido idCliente nomeProduto qtd

    putStrLn "Pedido cadastrado na fila com sucesso!"
    return pedido

{-| Lista todos os pedidos presentes na fila.

Exibe no terminal uma tabela formatada contendo:
ID do cliente, nome do produto e quantidade.
-}
listarPedidos :: [Pedido] -> IO ()
listarPedidos pedidos = do
    putStrLn "Fila de Pedidos:"
    putStrLn "ID Cliente\tProduto\tQuantidade"
    putStrLn "--------------------------------------"
    mapM_ (\p -> putStrLn $ show (idClientePedido p) ++ "\t" ++ nomeProdutoPedido p++ "\t" ++ show (quantidadePedido p)) pedidos


{-| Salva a lista de pedidos em um arquivo.

Os pedidos são serializados usando a representação 'show'.

Parâmetros:
- 'pedidos': Lista de pedidos a ser salva.
- 'arquivo': Caminho do arquivo onde os dados serão salvos.
-}
salvarPedidos :: [Pedido] -> FilePath -> IO ()
salvarPedidos pedidos arquivo = do
    writeFile arquivo (show pedidos)
    putStrLn $ "Pedidos salvos com sucesso em " ++ arquivo


{-| Carrega a lista de pedidos de um arquivo.

Caso o arquivo não exista ou ocorra um erro de leitura,
retorna uma lista vazia.

Parâmetro:
- 'arquivo': Caminho do arquivo a ser lido.

Retorna:
- Lista de pedidos carregados.
-}
carregarPedidos :: FilePath -> IO [Pedido]
carregarPedidos arquivo = do
    conteudo <- readFile arquivo `catch` (\(_ :: IOException) -> return "[]")
    let pedidos = read conteudo :: [Pedido]
    return pedidos


{-| Remove um pedido específico da lista.

Parâmetros:
- 'pedido': O pedido que deve ser removido.
- Lista de pedidos onde será feita a remoção.

Retorna:
- Nova lista de pedidos, sem o pedido especificado.
-}
removerPedido :: Pedido -> [Pedido] -> [Pedido]
removerPedido pedido = filter (/= pedido)
