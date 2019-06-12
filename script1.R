# Site do CURSO https://github.com/Rsimetti/IntroRCTM



# Remover todos os dados da memoria do R
rm(list = ls())

# Para saber a pasta de trabalho atual
getwd()

# Para definirmos a parta de trabalho 
setwd("C:/Users/rodri/Dropbox/Projetos R/CursoNEAPQuiM") #Esse caminho é diferente para cada pessoa!


# Vamos ver algumas operações básicas e entender como o R funciona --------

x <- 5 # "Xis" minusculo recebe o valor 5
X <- c("a", 4, "r") # "Xis" maiusculo recebe os valores "a", 4 e "r"


x+x # soma de numeros
x+X # Não soma pois é caracter

# comando c() concatena




#  Leitura de arquivos externos para o R ----------------------------------
# Lendo uma planilha do excel
# install.packages("readxl", dependencies = T) # Tirar o "#" do inicio para instalar
library(readxl) # Carrega o pacote que iremos precisar
dados_e <- read_excel("data/dados mecanica.xlsx")
str(dados_e)

# Como os dados numericos originalmente utilizam "." como separador decimal
# o R não reconheceu as duas ultimas colunas como números, portanto precisamos
# declarar para ele que essas variaveis são númericas, fazemos isso adicionando
# o comando col_types e dizendo qual o tipo de variavel de cada coluna 
dados_e <- read_excel("data/dados mecanica.xlsx", 
                    col_types = c("numeric", "numeric", "text", "numeric", 
                                  "numeric", "numeric"))
str(dados_e)

# Os mesmo dados estão disponíveis no formato csv, portanto podemos usar 
# o comando read.csv do pacote básico (base) do R, somente indicando que o
# separador é o ";"  com o comando sep 
dados_c <- read.csv("data/dados mecanica.csv", sep = ";")
str(dados_c)

plot(RC ~ PLANO, dados_c)
plot(RC ~ PLANO, dados_e)

# Se os dados são os mesmo, por que não foi feito o gráfico?
# O pacote read_excel não converte para um fator, podemos ver a diferença usando
# o comando str() para cada conjunto de dados. Para resolver isso vamos transformar
# a coluna PLANO do arquivo dados_e em um fator

dados_e$PLANO <- as.factor(dados_e$PLANO)
plot(RC ~ PLANO, dados_e)

# Um grafico basico para duas variaveis númericas 
plot(RC ~ Clone, data = dados_e)

# Agora vamos dar formato e cores diferentes para os pontos em função de 
# algumas variaveis 
# pch vai mudar a forma em função do PLANO
# col vai mudar a cor em função de Temp
plot(RC ~ Clone, 
     pch = c(19, 8)[as.numeric(dados_e$PLANO)],
     col = c("black", "red", "green", "blue")[as.numeric(as.factor(dados_e$Temp))],
     data = dados_e)

# Agora RC x MOE
plot(RC ~ MOE, # Quais as variaveis que vamos plotar y ~ x
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))], # A cor
     las = 1, # Gira os rotulos do y
     data = dados_e)

# Outra forma é declarar as variaveis diretamente, sem precisar indicar data = dados
plot(dados_e$MOE ~ dados_e$RC) 
# Como ambos os arquivos são iguais teremos o mesmo resultado
plot(dados_e$MOE ~ dados_c$RC) 

# Podemos visualizar todos os dados juntos
plot(dados_e)
# Separando utilizando as cores e formatos
plot(dados_e,
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor

# Plotar somente os dados das colunas 4, 5 e 6
plot(dados_e[,-1:-3], # REMOVEMOS as colunas 1 até 3
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor
# Mesmo resultado que o anterior porem ao inves de remover, declaramos quais usaremos
plot(dados_e[, 4:6], # USAMOS as colunas 4 até 6
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor

# jpeg("boxplot.jpg", width = 2500, height = 1500, res = 300)
# dev.off()


summary(dados_e)
dados_e$MOE[dados_e$Clone == "2"] # Somente os dados do MOE do CLONE 2
dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"] # dados do MOE do clone 2 e no plano PAR

# Usando esses dados para obter a média
mean(dados_e$MOE[dados_e$Clone == "2"])
mean(dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"])

# Usando esses dados para obter o desvio padrão
sd(dados_e$MOE[dados_e$Clone == "2"])
sd(dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"])

# Média dos clones 2 e 3 juntos
mean(dados_e$MOE[dados_e$Clone == "2" | dados_e$Clone == "3"])
mean(dados_e$MOE[dados_e$Clone == "2" & dados_e$Clone == "3"]) # Não existe uma mostra dos dos clones ao mesmo tempo. portanto não retorna resultado númerico



# PARA A SEGUNDA PARTE ----------------------------------------------------
# Instalar os pacotes abaixo
install.packages("tidyverse", dependencies = T)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
library(tidyverse)
