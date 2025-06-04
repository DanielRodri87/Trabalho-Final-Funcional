{-| Este módulo define o tipo 'Cliente' e funções utilitárias para manipulação
de clientes, incluindo criação, acesso e atualização de dados.

- 'Cliente'             : Tipo abstrato para clientes.
- 'novoCliente'         : Cria um novo cliente.
- 'obterIdCliente'      : Obtém o ID do cliente.
- 'obterNomeCliente'    : Obtém o nome do cliente.
- 'obterTelefoneCliente': Obtém o telefone do cliente.
- 'atualizarCliente'    : Atualiza nome e telefone do cliente.

-}

module Cliente (Cliente, novoCliente, obterIdCliente, obterNomeCliente, obterTelefoneCliente, atualizarCliente) where
import Tipos (Identificavel(..))


{-| Tipo abstrato para clientes.

Construtor interno:
- 'ClientePrivado id nome telefone'
-}
data Cliente = ClientePrivado Int String String

instance Identificavel Cliente where
    obterID = obterIdCliente

{-| Cria um novo cliente a partir de ID, nome e telefone. -}
novoCliente :: Int -> String -> String -> Cliente
novoCliente id nome telefone = ClientePrivado id nome telefone

-- Funções de acesso (getters)

{-| Obtém o ID do cliente. -}
obterIdCliente :: Cliente -> Int
obterIdCliente (ClientePrivado id _ _) = id

{-| Obtém o nome do cliente. -}
obterNomeCliente :: Cliente -> String
obterNomeCliente (ClientePrivado _ nome _) = nome

{-| Obtém o telefone do cliente. -}
obterTelefoneCliente :: Cliente -> String
obterTelefoneCliente (ClientePrivado _ _ telefone) = telefone

{-| Atualiza o nome e telefone de um cliente existente. -}
atualizarCliente :: Cliente -> String -> String -> Cliente
atualizarCliente (ClientePrivado id _ _) novoNome novoTelefone = ClientePrivado id novoNome novoTelefone