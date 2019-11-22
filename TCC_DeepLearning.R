#CÓDIGO FONTE TCC Hellen Gleicy Martins, MBA em Anaytics em Big Data Turma 8

#install.packages("installr")
#library(installr)
#updateR()

# carregar bibliotecas importantes séries temporais
#install.packages("mafs")
library(mafs)
#install.packages("magrittr")
library(magrittr)
#install.packages("forecast")
library(forecast)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tseries")
library(tseries)
#install.packages("imputeTS")
library(imputeTS)

###função para calcular coeficiente de variação
cv <-function(x){coef<-sd(x)/mean(x, na.rm = TRUE)*100 
return(coef)}

estacionar<-function(x){
  a<-lengths(x)["MES"]# - 6
  for (i in 1:a)
  {x$metrica_est[i] <- (x[i,3]-x[i-1,3])/x[i-1,3]}
  x$metrica_est[1] <- 0
  x_est<- ts(x$metrica_est[1:a], start = c(2004, 5), frequency = 12)
  print(adf.test(x_est, k=12))
  return(x_est)}


### Carregando dados de origem

setwd('C:\\Users\\helle\\Desktop\\TCC_FIA\\fase2')
dados <- read.table('2004-2019.tsv',sep="\t",dec=".",na.strings = "-", header=TRUE)

#Renomeando colunas
names(dados) <- c("X","DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                  "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                  "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                  "MES","ANO")

str(dados)


dados_GASOLINA_TEMP_SP <- subset(dados,  PRODUTO == "GASOLINA COMUM" & REGIAO == "SUDESTE", select = c("DATA.INICIAL","DATA.FINAL",
                                                                        "PRECO.MEDIO.DISTRIBUICAO",
                                                                        "MES","ANO"))
head(dados_GASOLINA_TEMP_SP)

dados_GASOLINA_SP <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
          data=dados_GASOLINA_TEMP_SP,
          FUN = mean)

dados_ETANOL_TEMP_SP <- subset(dados, PRODUTO == "ETANOL HIDRATADO" & ESTADO == "SAO PAULO", select = c("DATA.INICIAL","DATA.FINAL",
                                                                        "PRECO.MEDIO.DISTRIBUICAO",
                                                                        "MES","ANO"))

head(dados_ETANOL_TEMP_SP)

dados_ETANOL_SP <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                               data=dados_ETANOL_TEMP_SP,
                               FUN = mean)


# transformar para série temporal

dados_GASOLINA_SP_TS <- ts(dados_GASOLINA_SP[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_SP_TS <- ts(dados_ETANOL_SP[, 3], start = c(2004, 5), frequency = 12) 



###Já sabemos pelas análises anteriores que as séries não são estacionárias, estacionaremos as mesmas.

dados_GASOLINA_SP_TS_EST<-estacionar(dados_GASOLINA_SP)
dados_ETANOL_SP_TS_EST<-estacionar(dados_ETANOL_SP) 

###Para LSTM temos que ter uma matriz, abaixo efetuaremos conversão
step = 2   # step is a window size

#To cover all elements in a vector, we'll add a 'step' into the last part of  'a' vector by replicating the last element.

a = c(dados_GASOLINA_SP_TS_EST, replicate(step, tail(dados_GASOLINA_SP_TS_EST, 1)))

#Creating x - input, and y - output data.
N = lengths(dados_GASOLINA_SP)["MES"] - 6
x = NULL
y = NULL

for(i in 1:N) #base treino
{
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

##vendo prévia matriz
cbind(head(x), head(y))

xt = NULL
yt = NULL


for(j in 177:182) #base teste
{
  t = j-1+step
  xt = rbind(xt,a[j:t])
  yt = rbind(yt,a[t+1])
}

cbind(head(xt), head(yt))

#Input data should be an array type, so we'll reshape it.

X = array(x, dim=c(N, step,1))
Xt = array(xt, dim=c(6, step,1))

#install.packages("keras")
library(keras)

#Next, we'll create Keras sequential model, add an LSTM layer, and compile it with defined metrics.

model = keras_model_sequential() %>%   
layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
layer_dense(units=64, activation = "relu") %>%  
layer_dense(units=32) %>%  
layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
optimizer = 'adam',
metrics = list("mean_absolute_error","mean_absolute_percentage_error") #para mape metric_mean_absolute_percentage_error(y_true, y_pred)

)

model %>% summary()

#Next, we'll train the model with X and y input data, predict X data, and check the errors.


model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(Xt)
 
scores = model %>% evaluate(Xt, yt, verbose = 0)
print(scores)


#Finally, we'll plot the results.

x_axes = seq(1:length(y_pred))
plot(x_axes, yt, type="l", col="red", lwd=2)
lines(x_axes, y_pred, col="blue",lwd=2)
legend("topleft", legend=c("y-original", "y-predicted"),
        col=c("red", "blue"), lty=1,cex=0.8) 

#Analisando erro de predição de Jan a Jun de 2019
dados_GASOLINA_SP_TS[177:182] #valores reais
y_pred[1:6] #valores previsto estacionados

#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
t <- lengths(dados_GASOLINA_SP)["MES"] - 6
dados_GASOLINA_SP[t,]$MES
dados_GASOLINA_SP[t,]$ANO
dados_GASOLINA_SP[t,3]


dados_GASOLINA_SP_Projecao = dados_GASOLINA_SP


dados_GASOLINA_SP_Projecao[t+1,3] = (y_pred[1,] * dados_GASOLINA_SP[t,3]) + dados_GASOLINA_SP[t,3]
dados_GASOLINA_SP_Projecao[t+2,3] = (y_pred[2,] * dados_GASOLINA_SP[t+1,3]) + dados_GASOLINA_SP_Projecao[t+1,3] 
dados_GASOLINA_SP_Projecao[t+3,3] = (y_pred[3,] * dados_GASOLINA_SP[t+2,3]) + dados_GASOLINA_SP_Projecao[t+2,3] 
dados_GASOLINA_SP_Projecao[t+4,3] = (y_pred[4,] * dados_GASOLINA_SP[t+3,3]) + dados_GASOLINA_SP_Projecao[t+3,3] 
dados_GASOLINA_SP_Projecao[t+5,3] = (y_pred[5,] * dados_GASOLINA_SP[t+4,3]) + dados_GASOLINA_SP_Projecao[t+4,3] 
dados_GASOLINA_SP_Projecao[t+6,3] = (y_pred[6,] * dados_GASOLINA_SP[t+5,3]) + dados_GASOLINA_SP_Projecao[t+5,3]

##Calculando o MAPE##
mape = abs((dados_GASOLINA_SP[(t+1):(t+6),3] - dados_GASOLINA_SP_Projecao[(t+1):(t+6),3])/dados_GASOLINA_SP[(t+1):(t+6),3] * 100)
mean(mape) #o arima foi 3,06 e o lstm 3,28
dados_GASOLINA_SP_Projecao[(t+1):(t+6),]
dados_GASOLINA_SP[(t+1):(t+6),]
