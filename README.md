# 🌟 Gerenciador de Uma Cafeteria

Bem-vindo ao **Gerenciador de Uma Cafeteria**, uma solução prática e eficiente para gerenciar clientes, produtos, pedidos e estoque em cafeterias! Este projeto foi desenvolvido para simplificar o dia a dia de negócios pequenos, oferecendo funcionalidades essenciais em um único sistema.

---

## 📜 Histórico

| Data            | Versão | Responsável   | Alteração                     |
|-----------------|--------|---------------|-------------------------------|
| 30/05/2025      | 1.0    | Daniel        | Criação da estrutura inicial  |
| 30/05/2025      | 1.1    | Francinaldo   | CRUD Produtos Iniciado        |
| 30/05/2025      | 1.2    | Daniel        | CRUD Clientes Iniciado        |
| 03/06/2025      | 1.3    | Iago          | Fila de pedidos finalizado    |

---

## ✨ Funcionalidades

### 1. CRUD (Create, Read, Update, Delete) de Clientes e Produtos
- Gerencie facilmente os dados de clientes e produtos com operações completas de criação, leitura, atualização e exclusão.
  - **Clientes**: 
    - ID
    - Nome
    - Telefone
  - **Produtos**: 
    - ID
    - Nome
    - Quantidade
    - Preço
    - Controle de Estoque

### 2. Fila de Pedidos para Consumo
- Organize os pedidos dos clientes em uma fila eficiente.
  - O cliente faz o pedido, que entra na fila.
  - O pedido só é removido da fila após a realização do pagamento.

### 3. Efetuar um Pedido com Pagamento
- Finalize os pedidos com um processo simplificado.
  - Recebe os produtos e a quantidade escolhida.
  - Calcula o valor total do pedido.
  - Remove o pedido da fila.
  - Atualiza o estoque de produtos automaticamente.

### 4. Menu de Opções
- Navegue pelas funcionalidades com um menu intuitivo.
  - Inclui opções para cada funcionalidade listada, facilitando o uso do sistema.

---

## 🚀 Como Rodar

1. Clone o repositório:
   ```bash
   git clone https://github.com/DanielRodri87/Trabalho-Final-Funcional.git
   ```
2. Rode o Projeto por meio do MakeFile:
   ```bash
   make
   ```

