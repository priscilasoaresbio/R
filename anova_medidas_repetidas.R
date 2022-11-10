#####ANOVA de uma via com medidas repetidas ###########


# Carregar os pacotes

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(ez)) install.packages("ez") 
library(ez)
if(!require(reshape)) install.packages("reshape") 
library(reshape) 
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)

# abrir o banco de dados


armazen=read.csv2(file.choose(), header=TRUE, dec=".",strip.white=TRUE) 
View(armazen)                               
glimpse(armazen)                             

# Alterar o formato do banco de dados de "wide" para "long" (pacote: reshape)

# Reestruturando o banco de dados

armazen1 <- melt(armazen,
                 id = "Tratamento",
                 measured = c("30 dias", "60 dias", "120 dias", "180 dias"))

View(armazen1)

# Renomeando as colunas do novo banco
colnames(armazen1) = c("Tratamento", "Tempo", "Sementes")

# Ordenando as colunas pelo sujeito experimental
armazen1 <- sort_df(armazen1, vars = "Tratamento")


glimpse(armazen1)

# Transformando a variável ID em fator
armazen1$Tratamento <- factor(armazen1$Tratamento)


# Checar os pressupostos de normalidade e ausência de outliers 

# Verificando a presença de outliers por grupo
armazen1 %>% group_by(Tempo) %>% 
  identify_outliers(Sementes)


# Verificando a normalidade por grupo
armazen1 %>% group_by(Tempo) %>% 
  shapiro_test(Sementes)


# Passo 5: Construção do modelo da ANOVA com medidas repetidas (pacote: ez)

mod.ANOVA <- ezANOVA(data = armazen1,
                     dv = Sementes,
                     wid = Tratamento,
                     within = Tempo,
                     detailed = TRUE,
                     type = 3)

# dv = variável dependente
# wid = variável de identificação do sujeito
# within = variável independente de medidas repetidas
# type = tipo da soma dos quadrados (default é o tipo II, tipo III pelo desbalanço nos outliers)


# Analisando os resultados do modelo
mod.ANOVA


# Testes de post-hoc para ver onde está a diferença (neste caso nao tem, mas fiz o test)
pairwise.t.test(armazen1$Sementes, armazen1$Tempo, paired = TRUE,
                p.adjust.method = "bonferroni")


#  Análise descritiva dos dados 
armazen1 %>% group_by(Tempo) %>% 
  get_summary_stats(Sementes, type = "mean_sd")

boxplot(Sementes~ Tempo, data = armazen1, ylab = "Número de sementes germinadas", xlab="Tempo de armazenamento (dias)")

######################### Teste de Friedman ANOVA não paramétrica #########################


# Passo 1: Carregar os pacotes que serão usados

if(!require(dplyr)) install.packages("dplyr")
library(dplyr)                                
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix) 
if(!require(reshape)) install.packages("reshape") 
library(reshape) 
if(!require(PMCMRplus)) install.packages("PMCMRplus") 
library(PMCMRplus)   
if(!require(ggplot2)) install.packages("ggplot2") 
library(ggplot2)    

# Carregar o banco de dados


armazen=read.csv2(file.choose(), header=TRUE, dec=".",strip.white=TRUE) 
View(armazen)                               
glimpse(armazen)   

# Passo 3: Alterar o formato do banco de dados de "wide" para "long" (pacote: reshape)

# Reestruturando o banco de dados

armazenl <- melt(armazen,
                 id = "Tratamento",
                 measured = c("X30d", "X60d", "X120d", "X180d"))

View(armazenl)

# Renomeando as colunas do novo banco
colnames(armazenl) = c("Tratamento", "Tempo", "Sementes")


# Ordenando as colunas pelo sujeito experimental
armazenl <- sort_df(armazenl, vars = "Tratamento")


glimpse(armazenl)


# Transformando a variável ID em fator
armazenl$Tratamento <- factor(armazenl$Tratamento)


#Realização do teste de Friedman

friedman.test(Sementes ~ Tempo | Tratamento, data = armazenl)


# Passo 5: Análise descritiva dos dados
armazenl %>% group_by(Tempo) %>% 
  get_summary_stats(Sementes, type = "median_iqr")


# Passo 6: Visualização dos dados
boxplot(Sementes ~ Tempo, data = armazenl, xlab = "Tempo de armazenamento", ylab="Número de sementes")


# Passo 7: Análise da distribuição

par(mfrow=c(2,2))

hist(armazenl$Sementes[armazenl$Tempo == "X30d"],
     ylab = "Frequência", xlab = "Sementes", main="30 dias")

hist(armazenl$Sementes[armazenl$Tempo == "X60d"],
     ylab = "Frequência", xlab = "Sementes", main="60 dias") 

hist(armazenl$Sementes[armazenl$Tempo == "X120d"],
     ylab = "Frequência", xlab = "Sementes", main="120 dias")

hist(armazenl$Sementes[armazenl$Tempo == "X180d"],
     ylab = "Frequência", xlab = "Sementes", main="180 dias")

# Histograma com todos os grupos, separados por cor
ggplot(armazenl, aes(x = Sementes)) +
  geom_histogram(aes(color = Tempo, fill = Tempo),
                 alpha = 0.3, position = "stack", binwidth = 1)


