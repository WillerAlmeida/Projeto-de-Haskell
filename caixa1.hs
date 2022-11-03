module Main (main) where -- Tarefa 1: Definição do módulo Main, exportando a variável main.
import System.IO (stdout, hSetBuffering, BufferMode(NoBuffering)) -- Tarefa 3.1: Necessário pro cancelamento da bufferização.
import System.Environment

type Estado = Double -- Tarefa 6: Definindo o tipo Estado como sinônimo para Double.

main :: IO() -- Tarefa 2: Anotar o tipo de main, que quando executada interage com o mundo e retorna a tupla vazia.
main = do hSetBuffering stdout NoBuffering -- Tarefa 3.1: Cancela a bufferização de saída padrão.
  putStrLn "Simulação de Caixa Automático" -- Tarefa 3.2: Exibição do título do programa.
  args <- getArgs
  let file = (args !! 0)
  contents <- readFile file
  if contents == ""
    then menu 0.0 -- Tarefa 5 e Tarefa 9: Ação menu / Modificando o menu para receber o saldo inicial da conta corrente quando a aplicação é executada.
      else do let estadoInicio = read(contents)::Estado
        menu estadoInicio
  return () -- Tarefa 3.3: Retorna a tupla vazia.
  
menu :: Estado -> IO Estado -- Tarefa 4 e Tarefa 7: Definindo a ação de Menu / Modificando o menu.
menu estado = do putStrLn "=============================" -- Tarefa 4: Exibição do Menu.
  putStrLn "    Banco Haskell S.A."
  putStrLn "============================="
  putStrLn "Opções:\n"
  putStrLn "1. Saldo.\n"
  putStrLn "2. Depósito.\n"
  putStrLn "3. Saque.\n"
  putStrLn "4. Fim.\n"
  putStrLn "Escolha uma opção:"
  opcao <- getLine -- Entrada da opção escolhida pelo usuário.
 
  case opcao of -- Tarefa 4: Definindo case para analisar as diferentes opções de entrada.
  
  "1" -> saldo estado -- Tarefa 13: Utilizando a função Saldo.
         menu estado -- Ação recursiva para retornar o menu ao usuário.
         
  "2" -> estado <- deposito estado -- Tarefa 13: Utilizando a função Depósito.
         menu estado -- Ação recursiva para retornar o menu ao usuário.
         
  "3" -> estado <- saque estado -- Tarefa 13: Utilizando a função Saque.
         menu estado -- Ação recursiva para retornar o menu ao usuário.
        
  "4" -> putStrLn "Agradecemos por usar o nosso banco!"
         args <- getArgs
         writeFile (args !! 0) (show estado)
         return 0
  
  _   -> putStrLn "Opção inválida! Escolha entre 1 e 4."
         menu estado -- Ação recursiva para retornar o menu ao usuário.

saldo :: Estado -> IO Estado -- Tarefa 10: Função Saldo recebe o estado atual, exibe o estado atual e retorna o estado atual, já que não modifica nada no saldo da conta.
saldo estado = do putStr "Saldo:" 
  print estado -- Exibe o estado atual sem modificações.
  return estado -- Retorna o estado atual sem modificações.
  
deposito :: Estado -> IO Estado -- Tarefa 11: Função Depósito recebe o estado atual, lê o valor a ser depositado, verifica se não é negativo e retorna o estado com o saldo atualizado pelo depósito, caso o valor seja positivo.
deposito estado = do putStr "Informe o valor a ser depositado:" 
  valorDeposito <- readLn -- Lê o valor inserido pelo usuário.
  let deposito = valorDeposito :: Estado
  if deposito < 0.0 -- Testa se o valor inserido é negativo.
    then do putStrLn "Valor inválido. Por favor insira um valor acima de 0."
    return estado -- Retorna o estado atual sem modificações.
      else return (estado + deposito) -- Retorna o estado atual da conta somado do valor a ser depositado.
      
saque :: Estado -> IO Estado -- Tarefa 12: Função Saque recebe o estado atual, lê o valor a ser sacado, verifica se não é negativo e retorna o estado com o saldo atualizado pelo saque, caso o valor seja positivo.
saque estado = do putStr "Informe o valor a ser sacado:" 
  valorSaque <- readLn -- Lê o valor inserido pelo usuário.
  let saque = valorSaque :: Estado
  if saque < 0.0 -- Testa se o valor inserido é negativo.
  then do putStrLn "Valor inválido. Por favor insira um valor acima de 0."
  return estado -- Retorna o estado atual sem modificações.
    else 
    if estado - saque < 0.0 -- Testa se o valor inserido subtraído do estado atual do saldo é menor do que 0.
      then do putStrLn "Valor inválido. Por favor insira um valor menor ou igual ao saldo atual da conta."
      return estado -- Retorna o estado atual sem modificações.
    else return (estado - saque) -- Retorna o estado subtraído do valor do saque.
