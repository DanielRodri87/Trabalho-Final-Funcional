module Tipos where



class Identificavel a where
    obterID :: a -> Int


data Produto = Produto
    { idProduto :: Int
    , nomeProduto :: String
    , quantidadeProduto :: Int
    , precoProduto :: Float
    , controleProduto :: String
    } deriving (Show, Read, Eq)

instance Identificavel Produto where
    obterID produto = idProduto produto

data Pedido = Pedido {
    idClientePedido   :: Int,
    nomeProdutoPedido :: String,
    quantidadePedido  :: Int
} deriving (Show, Read, Eq)
