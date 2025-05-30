module IdUtil where

-- Verifica se um ID já está em uso para produtos
idExisteProduto :: Int -> [(Int, a, b, c, d)] -> Bool
idExisteProduto id produtos = any (\(pid, _, _, _, _) -> pid == id) produtos

-- Verifica se um ID já está em uso para clientes
idExisteCliente :: Int -> [(Int, a, b)] -> Bool
idExisteCliente id clientes = any (\(cid, _, _) -> cid == id) clientes

-- Gera um ID sequencial entre 100 e 999 que não esteja em uso para produtos
gerarIdUnicoProduto :: [(Int, a, b, c, d)] -> Int
gerarIdUnicoProduto [] = 100  -- Começa com ID 100 se lista estiver vazia
gerarIdUnicoProduto produtos = 
    let idsUsados = [id | (id, _, _, _, _) <- produtos]
        candidatos = [100..999] 
        idsDisponiveis = filter (\id -> not (id `elem` idsUsados)) candidatos
    in if null idsDisponiveis 
       then error "Todos os IDs de 100 a 999 estão em uso!"
       else head idsDisponiveis

-- Gera um ID sequencial entre 100 e 999 que não esteja em uso para clientes
gerarIdUnicoCliente :: [(Int, a, b)] -> Int
gerarIdUnicoCliente [] = 100  -- Começa com ID 100 se lista estiver vazia
gerarIdUnicoCliente clientes = 
    let idsUsados = [id | (id, _, _) <- clientes]
        candidatos = [100..999] 
        idsDisponiveis = filter (\id -> not (id `elem` idsUsados)) candidatos
    in if null idsDisponiveis 
       then error "Todos os IDs de 100 a 999 estão em uso!"
       else head idsDisponiveis
