rm(list = ls())
# pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
# install.packages(pkgs)

# keras: Library that ports Keras from Python enabling deep learning in R.
# lime: Used to explain the predictions of black box classifiers. Deep Learning falls into this category.
# tidyquant: Loads the tidyverse (dplyr, ggplot2, etc) and has nice visualization functions with theme_tq(). 
# rsample: New package for generating resamples.
# recipes: New package for preprocessing machine learning data sets.
# yardstick: Tidy methods for measuring model performance.
# corrr: Tidy methods for correlation.

# Load libraries
library(keras)
library(tidyr)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)
library(tidyverse)
# Install Keras if you have not installed before
#install_keras()
library (readr)
packages <- c("openxlsx", "readxl", "magrittr", "purrr", "ggplot2")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)


setwd("E:\\Mestrado\\2� semestre\\Ciencia de Dados para Processos de Negocio\\Trabalho final\\Projeto\\")
balancos <- read.xlsx("labels_balanceado.xlsx")


glimpse(balancos)
length(balancos)

# Remove unnecessary data e colunas desnecess�rias
# GERAL
balancos_tbl <- balancos %>%
  select(-Ticker,-Ano,-Empresa,-Concatenado,-Ag�ncia,-Ratings,
         -Aplicacoes_financeiras,-Obrigacoes_Trabalhistas,-Outras_Despesas,
         -Resultado_Equivalencia_Patrimonial,-Receita_Financeira,-Despesa_Financeira,
         -Aquisi��es_Intangivel,-Total_Impostos,-Juros_Sobre_Capital_Proprio,
         -Tributos_a_pagar,-Intangivel,-Prazo_Medio_Contas_a_Pagar) %>%
  drop_na() %>%
  select(Rating.Label, everything())
length(balancos_tbl)
glimpse(balancos_tbl)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
for (i in 2:length(balancos_tbl[1,])){
  balancos_tbl[,i]=normalize(balancos_tbl[,i])
}

# Split test/training sets
set.seed(100)
train_test_split <- initial_split(balancos_tbl, prop = 0.70)
train_test_split


# Retrieve train and test sets
train_tbl <- training(train_test_split)
test_tbl  <- testing(train_test_split) 

# Create recipe
rec_obj <- recipe(Rating.Label ~ ., data = train_tbl) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(), -all_outcomes()) %>%
  prep(data = train_tbl)

# Print the recipe object
rec_obj

# Predictors
x_train_tbl <- bake(rec_obj, new_data = train_tbl) %>% select(-Rating.Label)
x_test_tbl  <- bake(rec_obj, new_data = test_tbl) %>% select(-Rating.Label)


glimpse(x_train_tbl)

# Response variables for training and testing sets
y_train_vec <- ifelse(pull(train_tbl, Rating.Label) == "EXTREMLY STRONG", 5,
                      ifelse(pull(train_tbl, Rating.Label) == "STRONG", 4,
                             ifelse(pull(train_tbl, Rating.Label) == "REGULAR", 3,
                                    ifelse(pull(train_tbl, Rating.Label) == "WEAK", 2,1))))
length(which(y_train_vec ==1))
length(which(y_train_vec ==2))
length(which(y_train_vec ==3))
length(which(y_train_vec ==4))
length(which(y_train_vec ==5))

y_test_vec <- ifelse(pull(test_tbl, Rating.Label) == "EXTREMLY STRONG", 5,
                     ifelse(pull(test_tbl, Rating.Label) == "STRONG", 4,
                            ifelse(pull(test_tbl, Rating.Label) == "REGULAR", 3,
                                   ifelse(pull(test_tbl, Rating.Label) == "WEAK", 2,1))))
length(which(y_test_vec ==1))
length(which(y_test_vec ==2))
length(which(y_test_vec ==3))
length(which(y_test_vec ==4))
length(which(y_test_vec ==5))

divisao_dados=data.frame("Treinamento"=c(length(which(y_train_vec ==1)),
                                         length(which(y_train_vec ==2)),
                                         length(which(y_train_vec ==3)),
                                         length(which(y_train_vec ==4)),
                                         length(which(y_train_vec ==5))),
                         "Treino"=c(length(which(y_test_vec ==1)),
                                    length(which(y_test_vec ==2)),
                                    length(which(y_test_vec ==3)),
                                    length(which(y_test_vec ==4)),
                                    length(which(y_test_vec ==5))))
library(class)
i=1
qnt=150
matriz_treino=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
matriz_teste=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
acuracia_treino=data.frame("Vizinho"=rep(NA,qnt/2),"Acur�cia"=rep(NA,qnt/2))
acuracia_teste=data.frame("Vizinho"=rep(NA,qnt/2),"Acur�cia"=rep(NA,qnt/2))
precisao_treino=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
cobertura_treino=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
precisao_teste=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
cobertura_teste=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
f_measure_treino=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))
f_measure_teste=data.frame("Vizinho"=rep(NA,qnt/2),"1"=rep(NA,qnt/2),"2"=rep(NA,qnt/2),"3"=rep(NA,qnt/2),"4"=rep(NA,qnt/2),"5"=rep(NA,qnt/2))

for (vizinhos in 1:qnt){
  if (!vizinhos %% 2){
    next
  }
  ##run knn function
  pr_treino <- knn(x_test_tbl,x_train_tbl,cl=y_test_vec,k=vizinhos)
  pr_teste <- knn(x_train_tbl,x_test_tbl,cl=y_train_vec,k=vizinhos)
  
  length(pr_treino)
  length(pr_teste)
  length(y_train_vec)
  length(y_test_vec)
  
  ##create confusion matrix
    #Teste
  tab_treino <- table(pr_treino,y_train_vec)
  colnames(tab_treino)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
  rownames(tab_treino)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
  tab_treino
  #Treino
  tab_teste <- table(pr_teste,y_test_vec)
  colnames(tab_teste)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
  rownames(tab_teste)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
  tab_teste
  
  accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
  #Estat�sticas
  #Precis�o e Cobertura
  matriz_treino$Vizinho[i]=vizinhos
  matriz_teste$Vizinho[i]=vizinhos
  acuracia_treino$Vizinho[i]=vizinhos
  acuracia_teste$Vizinho[i]=vizinhos
  precisao_treino$Vizinho[i]=vizinhos
  precisao_teste$Vizinho[i]=vizinhos
  cobertura_treino$Vizinho[i]=vizinhos
  cobertura_teste$Vizinho[i]=vizinhos
  f_measure_treino$Vizinho[i]=vizinhos
  f_measure_teste$Vizinho[i]=vizinhos
  for (k in 1:length(tab_treino[1,])){
    #matriz_treino[i,(k+1)]=paste(tab_treino[k,],"")
    matriz_treino[i,(k+1)]=paste(tab_treino[k,1],tab_treino[k,2],tab_treino[k,3],tab_treino[k,4],tab_treino[k,5])
    matriz_teste[i,(k+1)]=paste(tab_teste[k,1],tab_teste[k,2],tab_teste[k,3],tab_teste[k,4],tab_teste[k,5])
    
    precisao_treino[i,(k+1)]=tab_treino[k,k]/(sum(tab_treino[k,]))*100
    cobertura_treino[i,(k+1)]=tab_treino[k,k]/(sum(tab_treino[,k]))*100
    
    precisao_teste[i,(k+1)]=tab_teste[k,k]/(sum(tab_teste[k,]))*100
    cobertura_teste[i,(k+1)]=tab_teste[k,k]/(sum(tab_teste[,k]))*100
    
    
    f_measure_treino[i,(k+1)]=2*(precisao_treino[i,(k+1)]*cobertura_treino[i,(k+1)])/(precisao_treino[i,(k+1)]+cobertura_treino[i,(k+1)])
    f_measure_teste[i,(k+1)]=2*(precisao_teste[i,(k+1)]*cobertura_teste[i,(k+1)])/(precisao_teste[i,(k+1)]+cobertura_teste[i,(k+1)])
    #F1-score = 2 � (precision � recall)/(precision + recall)
  }
  #Acur�cia
  acuracia_treino$Acur�cia[i]=accuracy(tab_treino)
  
  acuracia_teste$Acur�cia[i]=accuracy(tab_teste)
  
  cobertura=1
  f_measure=1
  ##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.
  
  i=i+1
}

#Plota acur�cia Treino e Teste
plot(acuracia_treino, type="b" , bty="l" , xlab="N� de Vizinhos", 
     ylab="Acur�cia %", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,
     ylim=c(min(acuracia_treino$Acur�cia,acuracia_teste$Acur�cia),
            max(acuracia_treino$Acur�cia,acuracia_teste$Acur�cia)))
lines(acuracia_teste, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=19 , type="b" )
legend("topright", 
       legend = c("Acur�cia Teste","Acur�cia Treinamento"), 
       col = c(rgb(0.2,0.4,0.1,0.7), 
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(17,19), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = F , 
       inset = c(0, -0.1))

#Plota precis�o do Treino
plot(precisao_treino$Vizinho,precisao_treino$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="Precis�o %", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='Precis�o Treino')
lines(precisao_treino$Vizinho,precisao_treino$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(precisao_treino$Vizinho,precisao_treino$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(precisao_treino$Vizinho,precisao_treino$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(precisao_treino$Vizinho,precisao_treino$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("bottomright", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))

#Plota precis�o do Teste
plot(precisao_teste$Vizinho,precisao_teste$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="Precis�o %", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='Precis�o Teste')
lines(precisao_teste$Vizinho,precisao_teste$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(precisao_teste$Vizinho,precisao_teste$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(precisao_teste$Vizinho,precisao_teste$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(precisao_teste$Vizinho,precisao_teste$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("bottomright", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))

#Plota Cobertura do Treino
plot(cobertura_treino$Vizinho,cobertura_treino$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="Cobertura Treino", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='Cobertura Treino')
lines(cobertura_treino$Vizinho,cobertura_treino$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(cobertura_treino$Vizinho,cobertura_treino$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(cobertura_treino$Vizinho,cobertura_treino$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(cobertura_treino$Vizinho,cobertura_treino$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("right", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))

#Plota Cobertura do Teste
plot(cobertura_teste$Vizinho,cobertura_teste$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="Cobertura Teste", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='Cobertura Teste')
lines(cobertura_teste$Vizinho,cobertura_teste$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(cobertura_teste$Vizinho,cobertura_teste$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(cobertura_teste$Vizinho,cobertura_teste$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(cobertura_teste$Vizinho,cobertura_teste$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("right", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))


#Plota F1-Measure do Treino
plot(f_measure_treino$Vizinho,f_measure_treino$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="F1-Measure Treino", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='F1-Measure Treino')
lines(f_measure_treino$Vizinho,f_measure_treino$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(f_measure_treino$Vizinho,f_measure_treino$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(f_measure_treino$Vizinho,f_measure_treino$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(f_measure_treino$Vizinho,f_measure_treino$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("topright", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))

#Plota F1-Measure do Teste
plot(f_measure_teste$Vizinho,f_measure_teste$X4,type="b", ylim=c(0,100), bty="l" , xlab="N� de Vizinhos", ylab="F1-Measure Teste", col=rgb(0.2,0.4,0.1,0.7) , lwd=3 , pch=17,main='F1-Measure Teste')
lines(f_measure_teste$Vizinho,f_measure_teste$X1, col=rgb(0.8,0.4,0.1,0.5) , lwd=3 , pch=18 , type="b" )
lines(f_measure_teste$Vizinho,f_measure_teste$X2, col=rgb(0.4,0.4,0,0.3) , lwd=3 , pch=19 , type="b" )
lines(f_measure_teste$Vizinho,f_measure_teste$X3, col=rgb(0.2,1,1,1) , lwd=3 , pch=15 , type="b" )
lines(f_measure_teste$Vizinho,f_measure_teste$X5, col=rgb(0.8,0.4,0.1,0.7) , lwd=3 , pch=3 , type="b" )
legend("topright", 
       legend = c("1","2","3","4","5"), 
       col = c(rgb(0.8,0.4,0.1,0.5),
               rgb(0.4,0.4,0,0.3),
               rgb(0.2,1,1,1),
               rgb(0.2,0.4,0.1,0.7),
               rgb(0.8,0.4,0.1,0.7)), 
       pch = c(18,19,15,17,3), 
       bty = "n", 
       pt.cex = 2, 
       cex = 0.8, 
       text.col = "black", 
       horiz = T , 
       inset = c(0, -0.1))

#Exportando
metricas_knn <-
  list("Matriz de Confus�o" = matriz_teste,
       "Acur�cia" = acuracia_teste,
       "Precis�o" = precisao_teste,
       "Cobertura"=cobertura_teste,
       "F1-Measure"=f_measure_teste)

write.xlsx(metricas_knn, "Metricas KNN.xlsx")
