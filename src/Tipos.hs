{-| Este módulo define os tipos principais utilizados no sistema,
incluindo produtos, pedidos e a classe de identificação genérica.

- 'Produto' : Representa um produto no estoque.
- 'Pedido'  : Representa um pedido realizado por um cliente.
- 'Identificavel' : Classe para tipos que possuem um identificador inteiro.

-}

module Tipos where


{- | Classe para tipos que possuem um identificador inteiro. -}
class Identificavel a where
    obterID :: a -> Int

{-| Representa um produto no estoque.

Campos:
- 'idProduto'        : Identificador único do produto.
- 'nomeProduto'      : Nome do produto.
- 'quantidadeProduto': Quantidade disponível.
- 'precoProduto'     : Preço unitário.
- 'controleProduto'  : Categoria ou controle do produto.
-}
data Produto = Produto
    { idProduto :: Int
    , nomeProduto :: String
    , quantidadeProduto :: Int
    , precoProduto :: Float
    , controleProduto :: String
    } deriving (Show, Read, Eq)


instance Identificavel Produto where
    obterID produto = idProduto produto

{-| Representa um pedido realizado por um cliente.

Campos:
- 'idClientePedido'   : ID do cliente que fez o pedido.
- 'nomeProdutoPedido' : Nome do produto pedido.
- 'quantidadePedido'  : Quantidade solicitada.
-}
data Pedido = Pedido {
    idClientePedido   :: Int,
    nomeProdutoPedido :: String,
    quantidadePedido  :: Int
} deriving (Show, Read, Eq)