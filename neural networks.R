rm(list = ls())
# pkgs <- c("keras", "lime", "tidyquant", "rsample", "recipes", "yardstick", "corrr")
# install.packages(pkgs)


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
library(stats)
# Install Keras if you have not installed before
#install_keras()
library (readr)
library(mgcv)
packages <- c("openxlsx", "readxl", "magrittr", "purrr", "ggplot2")

if (!require(install.load)) {
  install.packages("install.load")
}

install.load::install_load(packages)

#contador de tempo
sleep_for_a_minute <- function() { Sys.sleep(60) }
start_time <- Sys.time()


setwd("E:\\Mestrado\\2� semestre\\Ciencia de Dados para Processos de Negocio\\Trabalho final\\Projeto\\")
balancos <- read.xlsx("labels.xlsx")



#Tirando os indicadores
balancos_tbl <- balancos %>%
  select(-Ticker,-Ano,-Empresa,-Concatenado,-Agência,-Ratings,
         -Aplicacoes_financeiras,-Obrigacoes_Trabalhistas,-Outras_Despesas,
         -Resultado_Equivalencia_Patrimonial,-Receita_Financeira,-Despesa_Financeira,
         -Aquisiçõees_Intangivel,-Total_Impostos,-Juros_Sobre_Capital_Proprio,
         -Tributos_a_pagar,-Intangivel,-Indice_Liquidez_Corrente,-Indice_Liquidez_Seco,
         -Indice_Liquidez_Imediata,-Endividamento_Geral,-Composição_Endividamento,
         -Grau_Endividamento,-Cobertura_Juros,-Divida_Liquida_Sobre_Ebitda,
         -Passivo_Oneroso_Sobre_Ativo,-Imobilizacao_PL,-Imobilizacao_Recursos_nao_Correntes,
         -Rotacao_de_Estoques,-Prazo_Medio_Estoque,-Rotacao_Contas_a_Receber,
         -Prazo_Medio_Contas_a_Receber,-Rotacao_Contas_a_Pagar,-Prazo_Medio_Contas_a_Pagar,
         -Rotacao_Ativo_Imobilizado,-Rotacao_Capital_Acionistas,-Margem_Bruta,-Margem_Atividade,
         -Margem_EBITDA,-Margem_Operacional,-Margem_Liquida,-Retorno_Sobre_Ativo_Total,-Retorno_Sobre_PL,
         -Payout,-Retention_Rate) %>%
  drop_na() %>%
  select(Rating.Label, everything())
# length(balancos_tbl)
# glimpse(balancos_tbl)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
for (i in 2:length(balancos_tbl[1,])){
  balancos_tbl[,i]=normalize(balancos_tbl[,i])
}

# Split test/training sets
set.seed(100)
train_test_split <- initial_split(balancos_tbl, prop = 0.7)
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

# glimpse(x_train_tbl)

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


# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
  # First hidden layer,
  layer_dense(
    units              = 100, 
    kernel_initializer = "he_normal", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.025) %>%
  # Second hidden layer,
  layer_dense(
    units              = 100, 
    kernel_initializer = "he_normal", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.05) %>%
  # Third hidden layer,
  layer_dense(
    units              = 100, 
    kernel_initializer = "he_normal", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.075) %>%
  # Fourth hidden layer,
  layer_dense(
    units              = 100, 
    kernel_initializer = "he_normal", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  # Fifth hidden layer,
  layer_dense(
    units              = 100, 
    kernel_initializer = "he_normal", 
    activation         = "relu", 
    input_shape        = ncol(x_train_tbl)) %>% 
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.125) %>%
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "he_normal", 
    activation         = "relu") %>% 
  # Compile ANN
  compile(
    optimizer = 'rmsprop',
    loss      = 'mse',
    metrics   = 'mean_squared_error'
  )

# Fit the keras model to the training data
history <- fit(
  object           = model_keras, 
  x                = as.matrix(x_train_tbl), 
  y                = y_train_vec,
  batch_size       = 25, 
  epochs           = 100,
  validation_split = 0.30,
  verbose = 0
)

# Print a summary of the training history
print(history)

# Plot the training/validation history of our Keras model
plot(history) 

yhat_keras_class_vec <- model_keras %>% predict(as.matrix(x_test_tbl), batch_size = 32)
summary(yhat_keras_class_vec)
plot(y_test_vec,yhat_keras_class_vec)

#plot(yhat_keras_class_vec,dnorm(yhat_keras_class_vec,mean(yhat_keras_class_vec),sd(yhat_keras_class_vec)))
#normalizado
for (i in 1:length(yhat_keras_class_vec)){
  if (yhat_keras_class_vec[i]<=1.5){
    yhat_keras_class_vec[i]=1
  }
  else if (yhat_keras_class_vec[i]<=2.5 & yhat_keras_class_vec[i]>1.5){
    yhat_keras_class_vec[i]=2
  }
  else if (yhat_keras_class_vec[i]<=3.5 & yhat_keras_class_vec[i]>2.5){
    yhat_keras_class_vec[i]=3
  }
  else if (yhat_keras_class_vec[i]<=4.5 & yhat_keras_class_vec[i]>3.5){
    yhat_keras_class_vec[i]=4
  }
  else if (yhat_keras_class_vec[i]>4.5){
    yhat_keras_class_vec[i]=5
  }
}
summary(yhat_keras_class_vec)

##create confusion matrix
tab <- table(yhat_keras_class_vec[,1],y_test_vec)
linha=rownames(tab)
coluna=colnames(tab)
for (l in 1:5){
  if (l!=6){
    len=length(which(coluna==l))
    if (len==0 & length(which(colnames(tab)==l))==0){
      tab = cbind(tab[,1:(l-1)],0,tab[,-(1:(l-1))])
    }
  }
}
for (l in 1:5){
  if (l!=6){
    len=length(which(linha==l))
    if (len==0 & length(which(rownames(tab)==l))==0){
      tab = rbind(tab[1:(l-1),],0,tab[-(1:(l-1)),])
    }
  }
}
colnames(tab)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
rownames(tab)<-c("EXTREMELY WEAK","WEAK","REGULAR","STRONG","EXTREMLY STRONG")
tab
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}

acuracia=accuracy(tab)
acuracia
matriz=data.frame("1"=rep(NA,1),"2"=rep(NA,1),"3"=rep(NA,1),"4"=rep(NA,1),"5"=rep(NA,1))
precisao=data.frame("1"=rep(NA,1),"2"=rep(NA,1),"3"=rep(NA,1),"4"=rep(NA,1),"5"=rep(NA,1))
cobertura=data.frame("1"=rep(NA,1),"2"=rep(NA,1),"3"=rep(NA,1),"4"=rep(NA,1),"5"=rep(NA,1))
f_measure=data.frame("1"=rep(NA,1),"2"=rep(NA,1),"3"=rep(NA,1),"4"=rep(NA,1),"5"=rep(NA,1))
for (k in 1:5){
  matriz[1,k]=paste('[',tab[k,1],tab[k,2],tab[k,3],tab[k,4],tab[k,5],']')
  precisao[1,(k)]=tab[k,k]/(sum(tab[k,]))*100
  cobertura[1,(k)]=tab[k,k]/(sum(tab[,k]))*100
  f_measure[1,(k)]=2*(precisao[1,(k)]*cobertura[1,(k)])/(precisao[1,(k)]+cobertura[1,(k)])
}
#Exportando

metricas_knn <-
  list("Resultados" = data.frame(matriz,acuracia,precisao,cobertura,f_measure))

write.xlsx(metricas_knn, "Metricas Redes Neurais.xlsx")

end_time <- Sys.time()
end_time - start_time

