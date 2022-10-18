library(readr)
library(gridExtra)
library(ggplot2)
library(ggeffects)
library(modelr)
library(car)
library(lmtest)
library(caret) ##trainControl e Confusion Matrix

data <- read_csv2("/Users/PedroGabriel/Documents/LIVROS/Data Science 17:11/R 03:11/Análise Macro/Módulo 5/Laboratório 5/default of credit card clients.csv")

### Retirar a primeira variável do dataset, visto que se trata de um
### index e o R já criou um index próprio ao importar os dados

data <- data[,-1]

### Mudar o nome das variáveis, visto que foram importadas de forma que
### os rótulos das variáveis foram considerados como parte dos próprios dados

for (i in 1:ncol(data)){
  
  colnames(data)[i] <- data[1,i]
  
}

colnames(data)[24] <- 'default'

### Eliminar a primeira linha, que originalmente incorporava o rótulo
### das variáveis

data <- data[-1,]

### Convertendo colunas de variável que originalmente são númericas,
### mas constam como character.

data[,c(1, 5, 12:23)] <- lapply(data[,c(1, 5, 12:23)], 
                                as.numeric)

### Adaptar variáveis dummy para melhor compreensão

data$SEX <- sapply(data$SEX, function(x) ifelse (x == '1', 'male',
                                                 'female'))

data$EDUCATION[data$EDUCATION == 1] <- 'graduate school'
data$EDUCATION[data$EDUCATION == 2] <- 'university'
data$EDUCATION[data$EDUCATION == 3] <- 'high school'
data$EDUCATION[data$EDUCATION == 0 | data$EDUCATION == 4 | data$EDUCATION == 5 | data$EDUCATION == 6] <- 'others'

data$MARRIAGE[data$MARRIAGE == as.factor(1)] <- 'married'
data$MARRIAGE[data$MARRIAGE == 2] <- 'single'
data$MARRIAGE[data$MARRIAGE == 3 | data$MARRIAGE == 0] <- 'others'

### Convertendo colunas de variáveis que originalmente são factor, 
### mas constam como character.

data[, c(2, 3, 4, 6:11, 24)] <- lapply(data[, c(2, 3, 4, 6:11, 24)],
                                       as.factor)

### Checar a existência de missing values (NAs) no dataset

data_NAs <- apply(data, 2, function(x) is.na(x))
sum(data_NAs)

### Realizar primeiro uma análise exploratória dos dados criando gráficos
### que nos permitam identificar a distribuição de cada variável preditora
### (assim como da própria variável prevista) do modelo.


g1 <- ggplot(data, aes(x = LIMIT_BAL, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g2 <- ggplot(data, aes(x = SEX, fill = default))+
  geom_bar(position = 'stack')

g3 <- ggplot(data, aes(x = EDUCATION, fill = default))+
  geom_bar(position = 'stack')+
  coord_flip()

g4 <- ggplot(data, aes(x = MARRIAGE, fill = default))+
  geom_bar(position = 'stack')

g5 <- ggplot(data, aes(x = AGE, fill = default, color = default))+
  geom_histogram(alpha = 0.5)

g6 <- ggplot(data, aes(x = PAY_0, fill = default))+
  geom_bar(position = 'stack')

g7 <- ggplot(data, aes(x = PAY_2, fill = default))+
  geom_bar(position = 'stack')

g8 <- ggplot(data, aes(x = PAY_3, fill = default))+
  geom_bar(position = 'stack')

g9 <- ggplot(data, aes(x = PAY_4, fill = default))+
  geom_bar(position = 'stack')

g10 <- ggplot(data, aes(x = PAY_5, fill = default))+
  geom_bar(position = 'stack')

g11 <- ggplot(data, aes(x = PAY_6, fill = default))+
  geom_bar(position = 'stack')

g12 <- ggplot(data, aes(x = BILL_AMT1, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g13 <- ggplot(data, aes(x = BILL_AMT2, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g14 <- ggplot(data, aes(x = BILL_AMT3, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g15 <- ggplot(data, aes(x = BILL_AMT4, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g16 <- ggplot(data, aes(x = BILL_AMT5, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g17 <- ggplot(data, aes(x = BILL_AMT6, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g18 <- ggplot(data, aes(x = PAY_AMT1, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g19 <- ggplot(data, aes(x = PAY_AMT2, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g20 <- ggplot(data, aes(x = PAY_AMT3, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g21 <- ggplot(data, aes(x = PAY_AMT4, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g22 <- ggplot(data, aes(x = PAY_AMT5, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g23 <- ggplot(data, aes(x = PAY_AMT6, fill = default, colour = default))+
  geom_histogram(alpha = 0.5)+
  coord_flip()

g24 <- ggplot(data, aes(x = default))+
  geom_bar()


### Separar gráficos por painéis

grid.arrange(g1, g2, g3, g4)
grid.arrange(g5, g6, g7, g8)
grid.arrange(g9, g10, g11, g12)
grid.arrange(g13, g14, g15, g16)
grid.arrange(g17, g18, g19, g20)
grid.arrange(g21, g22, g23)

### Rodar a regressão logística, procurando explicar a probabilidade
### de default em dívida dadas as características analisadas. Vamos
### rodar o modelo através da função train() e ajustar para o tipo
### glm e família binomial para já empregarmos uma k-fold cross
### validation

validation_cv <- trainControl(method = 'cv', number = 5,
                              savePredictions = T)

logit_default <- train(default ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6,
                   data = data,
                   method = 'glm',
                   family = 'binomial',
                   trControl = validation_cv)

summary(logit_default)

### Testar a existência de autocorrelação dos resíduos utilizando
### o teste de Ljung-Box e testar a existência de 
### heteroscedasticidade utilizando o teste de Breusch-Godfrey

Box.test(logit_default$finalModel$residuals, lag = 1, type = 'Ljung-Box', 
         fitdf = 0)

### Avaliar plots do modelo

plot(logit_default$finalModel)

### Avaliar percentual de erro do modelo: 82% de acurácia

logit_default

### Avaliar a matriz de confusão

confusionMatrix(table((logit_default$pred)$pred,
                      (logit_default$pred)$obs))

### Possui uma tendência maior a subestimar o default 


