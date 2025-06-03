module Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente) where
import Tipos (Identificavel(..))

data Cliente = ClientePrivado Int String String

instance Identificavel Cliente where
    obterID = obterIdCliente


novoCliente :: Int -> String -> String -> Cliente
novoCliente id nome telefone = ClientePrivado id nome telefone

-- Funções de acesso (getters)

obterIdCliente :: Cliente -> Int
obterIdCliente (ClientePrivado id _ _) = id

obterNomeCliente :: Cliente -> String
obterNomeCliente (ClientePrivado _ nome _) = nome

obterTelefoneCliente :: Cliente -> String
obterTelefoneCliente (ClientePrivado _ _ telefone) = telefone

atualizarCliente :: Cliente -> String -> String -> Cliente
atualizarCliente (ClientePrivado id _ _) novoNome novoTelefone = ClientePrivado id novoNome novoTelefone