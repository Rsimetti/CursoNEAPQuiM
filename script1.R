# Site do CURSO https://github.com/Rsimetti/IntroRCTM



# Remover todos os dados da memoria do R
rm(list = ls())

# Para saber a pasta de trabalho atual
getwd()

# Para definirmos a parta de trabalho 
setwd("C:/Users/rodri/Dropbox/Projetos R/CursoNEAPQuiM") #Esse caminho � diferente para cada pessoa!


# Vamos ver algumas opera��es b�sicas e entender como o R funciona --------

x <- 5 # "Xis" minusculo recebe o valor 5
X <- c("a", 4, "r") # "Xis" maiusculo recebe os valores "a", 4 e "r"


x+x # soma de numeros
x+X # N�o soma pois � caracter

# comando c() concatena




#  Leitura de arquivos externos para o R ----------------------------------
# Lendo uma planilha do excel
# install.packages("readxl", dependencies = T) # Tirar o "#" do inicio para instalar
library(readxl) # Carrega o pacote que iremos precisar
dados_e <- read_excel("data/dados mecanica.xlsx")
str(dados_e)

# Como os dados numericos originalmente utilizam "." como separador decimal
# o R n�o reconheceu as duas ultimas colunas como n�meros, portanto precisamos
# declarar para ele que essas variaveis s�o n�mericas, fazemos isso adicionando
# o comando col_types e dizendo qual o tipo de variavel de cada coluna 
dados_e <- read_excel("data/dados mecanica.xlsx", 
                    col_types = c("numeric", "numeric", "text", "numeric", 
                                  "numeric", "numeric"))
str(dados_e)

# Os mesmo dados est�o dispon�veis no formato csv, portanto podemos usar 
# o comando read.csv do pacote b�sico (base) do R, somente indicando que o
# separador � o ";"  com o comando sep 
dados_c <- read.csv("data/dados mecanica.csv", sep = ";")
str(dados_c)

plot(RC ~ PLANO, dados_c)
plot(RC ~ PLANO, dados_e)

# Se os dados s�o os mesmo, por que n�o foi feito o gr�fico?
# O pacote read_excel n�o converte para um fator, podemos ver a diferen�a usando
# o comando str() para cada conjunto de dados. Para resolver isso vamos transformar
# a coluna PLANO do arquivo dados_e em um fator

dados_e$PLANO <- as.factor(dados_e$PLANO)
plot(RC ~ PLANO, dados_e)

# Um grafico basico para duas variaveis n�mericas 
plot(RC ~ Clone, data = dados_e)

# Agora vamos dar formato e cores diferentes para os pontos em fun��o de 
# algumas variaveis 
# pch vai mudar a forma em fun��o do PLANO
# col vai mudar a cor em fun��o de Temp
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

# Outra forma � declarar as variaveis diretamente, sem precisar indicar data = dados
plot(dados_e$MOE ~ dados_e$RC) 
# Como ambos os arquivos s�o iguais teremos o mesmo resultado
plot(dados_e$MOE ~ dados_c$RC) 

# Podemos visualizar todos os dados juntos
plot(dados_e)
# Separando utilizando as cores e formatos
plot(dados_e,
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor

# Plotar somente os dados das colunas 4, 5 e 6
plot(dados_e[,-1:-3], # REMOVEMOS as colunas 1 at� 3
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor
# Mesmo resultado que o anterior porem ao inves de remover, declaramos quais usaremos
plot(dados_e[, 4:6], # USAMOS as colunas 4 at� 6
     pch = c(20, 18)[as.numeric(dados_e$PLANO)], # A forma
     col = c("black", "red", "green", "coral3")[as.numeric(as.factor(dados_e$Temp))]) # A cor

# jpeg("boxplot.jpg", width = 2500, height = 1500, res = 300)
# dev.off()


summary(dados_e)
dados_e$MOE[dados_e$Clone == "2"] # Somente os dados do MOE do CLONE 2
dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"] # dados do MOE do clone 2 e no plano PAR

# Usando esses dados para obter a m�dia
mean(dados_e$MOE[dados_e$Clone == "2"])
mean(dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"])

# Usando esses dados para obter o desvio padr�o
sd(dados_e$MOE[dados_e$Clone == "2"])
sd(dados_e$MOE[dados_e$Clone == "2" & dados_e$PLANO == "PAR"])

# M�dia dos clones 2 e 3 juntos
mean(dados_e$MOE[dados_e$Clone == "2" | dados_e$Clone == "3"])
mean(dados_e$MOE[dados_e$Clone == "2" & dados_e$Clone == "3"]) # N�o existe uma mostra dos dos clones ao mesmo tempo. portanto n�o retorna resultado n�merico



# PARA A SEGUNDA PARTE ----------------------------------------------------
# Instalar os pacotes abaixo
install.packages("tidyverse", dependencies = T)
install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)
library(tidyverse)
