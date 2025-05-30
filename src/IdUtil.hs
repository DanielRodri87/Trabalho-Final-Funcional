module IdUtil where

-- Verifica se um ID já está em uso para produtos
idExisteProduto :: Int -> [(Int, a, b, c, d)] -> Bool
idExisteProduto id produtos = any (\(pid, _, _, _, _) -> pid == id) produtos

-- Verifica se um ID já está em uso para clientes
idExisteCliente :: Int -> [(Int, a, b)] -> Bool
idExisteCliente id clientes = any (\(cid, _, _) -> cid == id) clientes

-- Produtos: IDs entre 100-999
gerarIdUnicoProduto :: [(Int, b, c, d, e)] -> Int
gerarIdUnicoProduto produtos = 
    let existingIds = map (\(id, _, _, _, _) -> id) produtos
        newId = head [x | x <- [100..999], x `notElem` existingIds]
    in newId

-- Clientes: IDs entre 1000-9999
gerarIdUnicoCliente :: [(Int, b, c)] -> Int
gerarIdUnicoCliente clientes = 
    let existingIds = map (\(id, _, _) -> id) clientes
        newId = head [x | x <- [1000..9999], x `notElem` existingIds]
    in newId
