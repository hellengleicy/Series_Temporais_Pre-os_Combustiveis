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

###função para análise de Série Temporal 
analise <-function(serie){
#statsNA(serie)
#plot(serie)
#str(serie)
#summary(serie)
#sd(serie)
#cv(serie)
#boxplot(serie, 
#        col="dark red")
#hist(serie,breaks = 20, 
#     ylab="Frequencia", col="dark blue")
# testando se a série é estacionária pelo método de Dickey-Fuller (com 90% de confiança pvalor deve ser menor que 0.1 para ser estacionária)
print(adf.test(serie, k=12))
# Visualizar decomposição sazonal da série
serie %>% decompose %>% plot
# função ggmonthplot do pacote forecast
#ggmonthplot(serie)
#estratificação por mês
ggseasonplot(serie, year.labels = TRUE)
#aplicando modelo de rede neural para predição
#apply_selected_model(serie, "nnetar", horizon = 6) %>% forecast(h  = 6) %>% plot


}


projecao_auto_arima <-function(x){
  par(mfrow=c(2,2))
  treino <-window(x,,c(2018,12))
  teste <- window(x,2019)
  model_auto_arima = auto.arima(treino) 
  summary(model_auto_arima)
  auto_arima <- forecast(model_auto_arima,6)
  print(accuracy(auto_arima,teste))
  print(teste)
  #plot(auto_arima$residuals)
  #plot(forecast(model_auto_arima,6))
  tsdisplay(residuals(auto_arima), lag.max=45, main='Model Residuals')
  plot(forecast(model_auto_arima,6))
  auto_arima[["mean"]]}


projecao_linear_model <-function(x){
par(mfrow=c(1,2))
treino <-window(x,,c(2018,12))
teste <- window(x,2019)
model_tslm = tslm(treino ~ trend + season)
print(summary(model_tslm))
linear_model <- forecast(model_tslm)
print(accuracy(linear_model,teste))
print(teste)
plot(forecast(model_tslm))
plot(linear_model$residuals)
linear_model[["mean"]]}



estacionar<-function(x){
  a<-lengths(x)["MES"] - 6
  for (i in 1:a)
  {x$metrica_est[i] <- (x[i,3]-x[i-1,3])/x[i-1,3]}
  x$metrica_est[1] <- 0
  x_est<- ts(x$metrica_est[1:a], start = c(2004, 5), frequency = 12)
  print(adf.test(x_est, k=12))
  return(x_est)}


### Carregando dados de origem

setwd('C:\\Users\\helle\\Desktop\\TCC_FIA')
dados <- read.table('2004-2019.tsv',sep="\t",dec=".",na.strings = "-", header=TRUE)
head(dados)
dados$PRODUTO
names(dados)
#Renomeando colunas
names(dados) <- c("X","DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                  "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                  "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                  "MES","ANO")

str(dados)


#criando subsets por Produto
dados_GLP <- subset(dados, PRODUTO == "GLP", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                        "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                        "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                        "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                        "MES","ANO"))
head(dados_GLP)
summary(dados_GLP)


dados_GNV <- subset(dados, PRODUTO == "GNV", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                        "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                        "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                        "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                        "MES","ANO"))
head(dados_GNV)
summary(dados_GNV)

dados_GASOLINA <- subset(dados, PRODUTO == "GASOLINA COMUM", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                                        "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                                        "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                        "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                                        "MES","ANO"))
head(dados_GASOLINA)
summary(dados_GASOLINA)

dados_ETANOL <- subset(dados, PRODUTO == "ETANOL HIDRATADO", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                                        "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                                        "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                        "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                                        "MES","ANO"))

head(dados_ETANOL)
summary(dados_ETANOL)

dados_OLEO_DIESEL <- subset(dados, PRODUTO == "Ã"LEO DIESEL", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                                         "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                                         "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                         "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                                         "MES","ANO"))
head(dados_OLEO_DIESEL)
tail(dados_OLEO_DIESEL)
summary(dados_OLEO_DIESEL)

dados_OLEO_DIESEL_S10 <- subset(dados, PRODUTO == "Ã"LEO DIESEL S10", select = c("DATA.INICIAL","DATA.FINAL","REGIAO","ESTADO","PRODUTO","NUMERO.DE.POSTOS.PESQUISADOS",
                                                                                 "UNIDADE.DE.MEDIDA","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA",
                                                                                 "PRECO.MAXIMO.REVENDA","MARGEM.MEDIA.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                 "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO",
                                                                                 "MES","ANO"))
head(dados_OLEO_DIESEL_S10)
summary(dados_OLEO_DIESEL_S10)

### Descriptive analysis
# criando função - para realizar analises,lendo as diferentes tabelas de dados por região e produto que serão criadas a seguida
analise_descritiva <-function(x){
  
   # para as variáveis quantitativas calcular as medidas de posição (média, quartis),
  # de dispersão (desvio padrão e coeficiente de variação) e
  # análise gráfica (Box-plot e Histograma).
  
  par(mfrow=c(1,2))
  
  print('Numero Postos Pesquisados')
  print(summary(x$NUMERO.DE.POSTOS.PESQUISADOS))
  print(sd(x$NUMERO.DE.POSTOS.PESQUISADOS))
  print(cv(x$NUMERO.DE.POSTOS.PESQUISADOS))
  boxplot(x$NUMERO.DE.POSTOS.PESQUISADOS, 
          main="Boxplot - Numero Postos Pesquisados",
          ylab="Numero Postos Pesquisados", col="dark red")
  hist(x$NUMERO.DE.POSTOS.PESQUISADOS,breaks = 20, 
       main="Histograma - Numero Postos Pesquisados",
       xlab="Numero Postos Pesquisados", ylab="Frequencia", col="dark blue")
  
  
  print('Preco Medio Revenda')
  print(summary(x$PRECO.MEDIO.REVENDA))
  print(sd(x$PRECO.MEDIO.REVENDA))
  print(cv(x$PRECO.MEDIO.REVENDA))
  boxplot(x$PRECO.MEDIO.REVENDA, 
          main="Boxplot - Preço Médio de Revenda",
          ylab="Preço Médio de Revenda", col="dark red")
  hist(dados$PRECO.MEDIO.REVENDA,breaks = 20, 
       main="Histograma - Preço Médio de Revenda",
       xlab= "Preço Médio de Revenda", ylab="Frequencia", col="dark blue")
  
  print('Desvio Padrao Revenda')
  print(summary(x$DESVIO.PADRAO.REVENDA))
  print(sd(x$DESVIO.PADRAO.REVENDA))
  print(cv(x$DESVIO.PADRAO.REVENDA))
  boxplot(x$DESVIO.PADRAO.REVENDA, 
          main="Boxplot - Desvio Padrao Revenda",
          ylab="Desvio Padrao Revenda", col="dark red")
  hist(x$DESVIO.PADRAO.REVENDA,breaks = 20, 
       main="Histograma - Desvio Padrao Revenda",
       xlab= "Desvio Padrao Revenda", ylab="Frequencia", col="dark blue")
  
  print('Preco Minimo Revenda')
  print(summary(x$PRECO.MINIMO.REVENDA))
  print(sd(x$PRECO.MINIMO.REVENDA))
  print(cv(x$PRECO.MINIMO.REVENDA))
  boxplot(x$PRECO.MINIMO.REVENDA, 
          main="Boxplot - Preco Minimo Revenda",
          ylab="Preco Minimo Revenda", col="dark red")
  hist(x$PRECO.MINIMO.REVENDA,breaks = 20, 
       main="Histograma - Preco Minimo Revenda",
       xlab= "Preco Minimo Revenda", ylab="Frequencia", col="dark blue")
  
  print('Preço Maximo Revenda')
  print(summary(x$PRECO.MAXIMO.REVENDA))
  print(sd(x$PRECO.MAXIMO.REVENDA))
  print(cv(x$PRECO.MAXIMO.REVENDA))
  boxplot(x$PRECO.MAXIMO.REVENDA, 
          main="Boxplot - Preco Maximo Revenda",
          ylab="Preco Maximo Revenda", col="dark red")
  hist(x$PRECO.MAXIMO.REVENDA,breaks = 20, 
       main="Histograma - Preco Maximo Revenda",
       xlab= "Preco Maximo Revenda", ylab="Frequencia", col="dark blue")
  
  print('Margem Media Revenda')
  print(summary(x$MARGEM.MEDIA.REVENDA))
  print(sd(x$PRECO.MARGEM.MEDIA.REVENDA))
  print(cv(x$PRECO.MARGEM.MEDIA.REVENDA))
  boxplot(x$MARGEM.MEDIA.REVENDA, 
          main="Boxplot - Margem Media Revenda",
          ylab="Margem Media Revenda", col="dark red")
  hist(x$MARGEM.MEDIA.REVENDA,breaks = 20, 
       main="Histograma - Margem Media Revenda",
       xlab= "Margem Media Revenda", ylab="Frequencia", col="dark blue")
  
  print('Coef de Variacao Revenda')
  print(summary(x$COEF.DE.VARIACAO.REVENDA))
  print(sd(x$COEF.DE.VARIACAO.REVENDA))
  print(cv(x$COEF.DE.VARIACAO.REVENDA))
  boxplot(x$COEF.DE.VARIACAO.REVENDA, 
          main="Boxplot - Coef. de Variacao Revenda",
          ylab="Coef. de Variacao Revenda", col="dark red")
  hist(x$COEF.DE.VARIACAO.REVENDA,breaks = 20, 
       main="Histograma - Coef. de Variacao Revenda",
       xlab= "Coef. de Variacao Revenda", ylab="Frequencia", col="dark blue")
  
  print('Preco Medio Distribuicao')
  print(summary(x$PRECO.MEDIO.DISTRIBUICAO))
  print(sd(x$PRECO.MEDIO.DISTRIBUICAO))
  print(cv(x$PRECO.MEDIO.DISTRIBUICAO))
  boxplot(x$PRECO.MEDIO.DISTRIBUICAO, 
          main="Boxplot - Preco Medio Distribuicao",
          ylab="Preco Medio Distribuicao", col="dark red")
  hist(x$PRECO.MEDIO.DISTRIBUICAO,breaks = 20, 
       main="Histograma - Preco Medio Distribuicao",
       xlab= "Preco Medio Distribuicao", ylab="Frequencia", col="dark blue")
  
  print('Desvio Padrao Distribuicao')
  print(summary(x$DESVIO.PADRAO.DISTRIBUICAO))
  print(sd(x$DESVIO.PADRAO.DISTRIBUICAO))
  print(cv(x$DESVIO.PADRAO.DISTRIBUICAO))
  boxplot(x$DESVIO.PADRAO.DISTRIBUICAO, 
          main="Boxplot - Desvio Padrao Distribuicao",
          ylab="Desvio Padrao Distribuicao", col="dark red")
  hist(x$DESVIO.PADRAO.DISTRIBUICAO,breaks = 20, 
       main="Histograma - Desvio Padrao Distribuicao",
       xlab= "Desvio Padrao Distribuicao", ylab="Frequencia", col="dark blue")
  
  print('Preco Minimo Distribuicao')
  print(summary(x$PRECO.MINIMO.DISTRIBUICAO))
  print(sd(x$PRECO.MINIMO.DISTRIBUICAO))
  print(cv(x$PRECO.MINIMO.DISTRIBUICAO))
  boxplot(x$PRECO.MINIMO.DISTRIBUICAO, 
          main="Boxplot - Preco Minimo Distribuicao",
          ylab="Preco Minimo Distribuicao", col="dark red")
  hist(x$PRECO.MINIMO.DISTRIBUICAO,breaks = 20, 
       main="Histograma - Preco Minimo Distribuicao",
       xlab= "Preco Minimo Distribuicao", ylab="Frequencia", col="dark blue")
  
  print('Preco Maximo Distribuicao')
  print(summary(x$PRECO.MAXIMO.DISTRIBUICAO))
  print(sd(x$PRECO.MAXIMO.DISTRIBUICAO))
  print(cv(x$PRECO.MAXIMO.DISTRIBUICAO))
  boxplot(x$PRECO.MAXIMO.DISTRIBUICAO, 
          main="Boxplot - Preco Maximo Distribuicao",
          ylab="Preco Maximo Distribuicao", col="dark red")
  hist(x$PRECO.MAXIMO.DISTRIBUICAO,breaks = 20, 
       main="Histograma - Preco Maximo Distribuicao",
       xlab= "Preco Maximo Distribuicao", ylab="Frequencia", col="dark blue")
  
  print('Coef Variacao Distribuicao')
  print(summary(x$COEF.DE.VARIACAO.DISTRIBUICAO))
  print(sd(x$COEF.DE.VARIACAO.DISTRIBUICAO))
  print(cv(x$COEF.DE.VARIACAO.DISTRIBUICAO))
  boxplot(x$COEF.DE.VARIACAO.DISTRIBUICAO, 
          main="Boxplot - Coef. De Variacao Distribuicao",
          ylab="Coef. De Variacao Distribuicao", col="dark red")
  hist(x$COEF.DE.VARIACAO.DISTRIBUICAO,breaks = 20, 
       main="Histograma - Coef. De Variacao Distribuicao",
       xlab= "Coef. De Variacao Distribuicao", ylab="Frequencia", col="dark blue")
}

# Realizando análise descritiva das tabelas criadas Brasil

analise_descritiva(dados_GASOLINA)
analise_descritiva(dados_ETANOL)
analise_descritiva(dados_GLP)
analise_descritiva(dados_GNV)
analise_descritiva(dados_OLEO_DIESEL)
analise_descritiva(dados_OLEO_DIESEL_S10)

### Criando as tabelas por região e produto
## Região: NORTE, NORDESTE, CENTRO OESTE, SUDESTE, SUL
## Produtos: ÓLEO DIESEL, ÓLEO DIESEL S10, ETANOL HIDRATADO, GASOLINA COMUM, GLP, GNV
## Também serão criadas as respectivas Séries Temporais a partir destas tabelas por produto e região: "PRECO.MEDIO.REVENDA",
## "PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
## "PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO", "MARGEM.MEDIA.REVENDA"

##Região NORTE
#Gasolina
dados_GASOLINA_NORTE_TEMP <- subset(dados_GASOLINA,REGIAO == "NORTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GASOLINA_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                 data=dados_GASOLINA_NORTE_TEMP,
                                 FUN = mean)

dados_GASOLINA_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                       data=dados_GASOLINA_NORTE_TEMP,
                                       FUN = mean)
dados_GASOLINA_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                       data=dados_GASOLINA_NORTE_TEMP,
                                       FUN = mean)
dados_GASOLINA_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_GASOLINA_NORTE_TEMP,
                                       FUN = mean)
dados_GASOLINA_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_GASOLINA_NORTE_TEMP,
                                       FUN = mean)
dados_GASOLINA_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_GASOLINA_NORTE_TEMP,
                                            FUN = mean)
dados_GASOLINA_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                            data=dados_GASOLINA_NORTE_TEMP,
                                            FUN = mean)

# ETANOL HIDRATADO

dados_ETANOL_NORTE_TEMP <- subset(dados_ETANOL,REGIAO == "NORTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                            "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_ETANOL_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_NORTE_TEMP,
                                              FUN = mean)

dados_ETANOL_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_NORTE_TEMP,
                                              FUN = mean)
dados_ETANOL_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_NORTE_TEMP,
                                              FUN = mean)
dados_ETANOL_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_NORTE_TEMP,
                                            FUN = mean)
dados_ETANOL_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_NORTE_TEMP,
                                            FUN = mean)
dados_ETANOL_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_NORTE_TEMP,
                                            FUN = mean)
dados_ETANOL_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                 data=dados_ETANOL_NORTE_TEMP,
                                                 FUN = mean)
#GLP
dados_GLP_NORTE_TEMP <- subset(dados_GLP,REGIAO == "NORTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                      "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GLP_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORTE_TEMP,
                                            FUN = mean)

dados_GLP_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORTE_TEMP,
                                            FUN = mean)
dados_GLP_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORTE_TEMP,
                                            FUN = mean)
dados_GLP_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORTE_TEMP,
                                          FUN = mean)
dados_GLP_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORTE_TEMP,
                                          FUN = mean)
dados_GLP_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORTE_TEMP,
                                          FUN = mean)
dados_GLP_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                               data=dados_GLP_NORTE_TEMP,
                                               FUN = mean)
#GNV


dados_GNV_NORTE_TEMP <- subset(dados_GNV,REGIAO == "NORTE" & ANO>2012,select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                      "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GNV_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                         data=dados_GNV_NORTE_TEMP,
                                         FUN = mean)

dados_GNV_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                         data=dados_GNV_NORTE_TEMP,
                                         FUN = mean)
dados_GNV_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                         data=dados_GNV_NORTE_TEMP,
                                         FUN = mean)
dados_GNV_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_GNV_NORTE_TEMP,
                                       FUN = mean)
dados_GNV_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_GNV_NORTE_TEMP,
                                       FUN = mean)
dados_GNV_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_GNV_NORTE_TEMP,
                                       FUN = mean)
dados_GNV_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                            data=dados_GNV_NORTE_TEMP,
                                            FUN = mean)
#óLEO DIESEL

dados_OLEO_DIESEL_NORTE_TEMP <- subset(dados_OLEO_DIESEL,REGIAO == "NORTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                      "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_DIESEL_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                         data=dados_OLEO_DIESEL_NORTE_TEMP,
                                         FUN = mean)

dados_OLEO_DIESEL_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                         data=dados_OLEO_DIESEL_NORTE_TEMP,
                                         FUN = mean)
dados_OLEO_DIESEL_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                         data=dados_OLEO_DIESEL_NORTE_TEMP,
                                         FUN = mean)
dados_OLEO_DIESEL_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_OLEO_DIESEL_NORTE_TEMP,
                                       FUN = mean)
dados_OLEO_DIESEL_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_OLEO_DIESEL_NORTE_TEMP,
                                       FUN = mean)
dados_OLEO_DIESEL_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                       data=dados_OLEO_DIESEL_NORTE_TEMP,
                                       FUN = mean)
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                            data=dados_OLEO_DIESEL_NORTE_TEMP,
                                            FUN = mean)
#óLEO DIESEL s10

dados_OLEO_s10_NORTE_TEMP <- subset(dados_OLEO_DIESEL_S10,REGIAO == "NORTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                       "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_s10_NORTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORTE_TEMP,
                                                 FUN = mean)

dados_OLEO_s10_NORTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORTE_TEMP,
                                                 FUN = mean)
dados_OLEO_s10_NORTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORTE_TEMP,
                                                 FUN = mean)
dados_OLEO_s10_NORTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                    data=dados_OLEO_s10_NORTE_TEMP,
                                                    FUN = mean)
# Realizando análise descritiva das tabelas criadas

analise_descritiva(dados_GASOLINA_NORTE_TEMP)
analise_descritiva(dados_ETANOL_NORTE_TEMP)
analise_descritiva(dados_GLP_NORTE_TEMP)
analise_descritiva(dados_GNV_NORTE_TEMP)
analise_descritiva(dados_OLEO_DIESEL_NORTE_TEMP)
analise_descritiva(dados_OLEO_s10_NORTE_TEMP)

# transformar para série temporal

dados_GASOLINA_NORTE_MED_REVENDA_TS <- ts(dados_GASOLINA_NORTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORTE_MIN_REVENDA_TS <- ts(dados_GASOLINA_NORTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORTE_MAX_REVENDA_TS <- ts(dados_GASOLINA_NORTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORTE_MED_DISTR_TS <- ts(dados_GASOLINA_NORTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORTE_MIN_DISTR_TS <- ts(dados_GASOLINA_NORTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORTE_MAX_DISTR_TS <- ts(dados_GASOLINA_NORTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORTE_MARGEM_REVENDA_TS <- ts(dados_GASOLINA_NORTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MED_REVENDA_TS <- ts(dados_ETANOL_NORTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MIN_REVENDA_TS <- ts(dados_ETANOL_NORTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MAX_REVENDA_TS <- ts(dados_ETANOL_NORTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MED_DISTR_TS <- ts(dados_ETANOL_NORTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MIN_DISTR_TS <- ts(dados_ETANOL_NORTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_NORTE_MAX_DISTR_TS <- ts(dados_ETANOL_NORTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORTE_MARGEM_REVENDA_TS <- ts(dados_ETANOL_NORTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORTE_MED_REVENDA_TS  <- ts(dados_GLP_NORTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORTE_MIN_REVENDA_TS <- ts(dados_GLP_NORTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORTE_MAX_REVENDA_TS <- ts(dados_GLP_NORTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORTE_MED_DISTR_TS <- ts(dados_GLP_NORTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORTE_MIN_DISTR_TS <- ts(dados_GLP_NORTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORTE_MAX_DISTR_TS <- ts(dados_GLP_NORTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORTE_MARGEM_REVENDA_TS <- ts(dados_GLP_NORTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORTE_MED_REVENDA_TS <- ts(dados_GNV_NORTE_MED_REVENDA[, 3], start = c(2013, 1), frequency = 12) 
dados_GNV_NORTE_MIN_REVENDA_TS <- ts(dados_GNV_NORTE_MIN_REVENDA[, 3], start = c(2013, 1), frequency = 12) 
dados_GNV_NORTE_MAX_REVENDA_TS <- ts(dados_GNV_NORTE_MAX_REVENDA[, 3], start = c(2013, 1), frequency = 12) 
dados_GNV_NORTE_MIN_DISTR_TS <- ts(dados_GNV_NORTE_MIN_DISTR[, 3], start = c(2013, 1), frequency = 12)  #NÃO EXISTEM DADOS EM TODOS OS PERIODOS ANTERIORES A 2013 ALÉM DISSO FALTAM PERIODOS MES 5 EM 2018 e MESES 3 e 11 de 2016
dados_GNV_NORTE_MAX_DISTR_TS <- ts(dados_GNV_NORTE_MAX_DISTR[, 3], start = c(2013, 1), frequency = 12)  #NÃO EXISTEM DADOS EM TODOS OS PERIODOS ANTERIORES A 2013 ALÉM DISSO FALTAM PERIODOS MES 5 EM 2018 e MESES 3 e 11 de 2016

dados_GNV_NORTE_MARGEM_REVENDA[45,]
tail(dados_GNV_NORTE_MARGEM_REVENDA)

dados_GNV_NORTE_MARGEM_REVENDA[76,]<- dados_GNV_NORTE_MARGEM_REVENDA[45,]
dados_GNV_NORTE_MARGEM_REVENDA[76,1] <-11
dados_GNV_NORTE_MARGEM_REVENDA[77,]<- dados_GNV_NORTE_MARGEM_REVENDA[62,]
dados_GNV_NORTE_MARGEM_REVENDA[77,1]<- 5
dados_GNV_NORTE_MARGEM_REVENDA[78,]<- dados_GNV_NORTE_MARGEM_REVENDA[38,]
dados_GNV_NORTE_MARGEM_REVENDA[78,1]<- 3
dados_GNV_NORTE_MARGEM_REVENDA <- dados_GNV_NORTE_MARGEM_REVENDA[order(dados_GNV_NORTE_MARGEM_REVENDA$ANO, dados_GNV_NORTE_MARGEM_REVENDA$MES,decreasing=c(FALSE, FALSE)), ] 
dados_GNV_NORTE_MARGEM_REVENDA_TS <- ts(dados_GNV_NORTE_MARGEM_REVENDA[, 3], start = c(2013, 1), frequency = 12) #NÃO EXISTEM DADOS EM TODOS OS PERIODOS ANTERIORES A 2013 ALÉM DISSO FALTAM PERIODOS MES 5 EM 2018 e MESES 3 e 11 de 2016


dados_GNV_NORTE_MED_DISTR[76,]<- dados_GNV_NORTE_MED_DISTR[45,]
dados_GNV_NORTE_MED_DISTR[76,1] <-11
dados_GNV_NORTE_MED_DISTR[77,]<- dados_GNV_NORTE_MED_DISTR[62,]
dados_GNV_NORTE_MED_DISTR[77,1]<- 5
dados_GNV_NORTE_MED_DISTR[78,]<- dados_GNV_NORTE_MED_DISTR[38,]
dados_GNV_NORTE_MED_DISTR[78,1]<- 3
dados_GNV_NORTE_MED_DISTR <- dados_GNV_NORTE_MED_DISTR[order(dados_GNV_NORTE_MED_DISTR$ANO, dados_GNV_NORTE_MED_DISTR$MES,decreasing=c(FALSE, FALSE)), ]
dados_GNV_NORTE_MED_DISTR_TS <- ts(dados_GNV_NORTE_MED_DISTR[, 3], start = c(2013, 1), frequency = 12) #NÃO EXISTEM DADOS EM TODOS OS PERIODOS ANTERIORES A 2013 E ALÉM DISSO FALTAM PERIODOS MES 5 EM 2018 e MESES 3 e 11 de 2016



dados_OLEO_DIESEL_NORTE_MED_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORTE_MIN_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORTE_MAX_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORTE_MED_DISTR_TS <- ts(dados_OLEO_DIESEL_NORTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORTE_MIN_DISTR_TS <- ts(dados_OLEO_DIESEL_NORTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_OLEO_DIESEL_NORTE_MAX_DISTR_TS <- ts(dados_OLEO_DIESEL_NORTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_s10_NORTE_MED_REVENDA_TS <- ts(dados_OLEO_s10_NORTE_MED_REVENDA[, 3], start = c(2012, 12), frequency = 12)
dados_OLEO_s10_NORTE_MIN_REVENDA_TS <- ts(dados_OLEO_s10_NORTE_MIN_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORTE_MAX_REVENDA_TS <- ts(dados_OLEO_s10_NORTE_MAX_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORTE_MED_DISTR_TS <- ts(dados_OLEO_s10_NORTE_MED_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORTE_MIN_DISTR_TS <- ts(dados_OLEO_s10_NORTE_MIN_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORTE_MAX_DISTR_TS <- ts(dados_OLEO_s10_NORTE_MAX_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_s10_NORTE_MARGEM_REVENDA[, 3], start = c(2012, 12), frequency = 12)

#realizando análise exploratória das séries temporais de média

par(mfrow=c(1,3))
analise(dados_GASOLINA_NORTE_MED_REVENDA_TS)
analise(dados_GASOLINA_NORTE_MED_DISTR_TS) 
analise(dados_GASOLINA_NORTE_MARGEM_REVENDA_TS) 
analise(dados_ETANOL_NORTE_MED_REVENDA_TS) 
analise(dados_ETANOL_NORTE_MED_DISTR_TS) 
analise(dados_ETANOL_NORTE_MARGEM_REVENDA_TS) 
analise(dados_GLP_NORTE_MED_REVENDA_TS) 
analise(dados_GLP_NORTE_MED_DISTR_TS) 
analise(dados_GLP_NORTE_MARGEM_REVENDA_TS) 
analise(dados_GNV_NORTE_MED_REVENDA_TS) 
analise(dados_GNV_NORTE_MED_DISTR_TS) #Faltam periodos na série
analise(dados_GNV_NORTE_MARGEM_REVENDA_TS)  #Faltam periodos na série
analise(dados_OLEO_DIESEL_NORTE_MED_REVENDA_TS) 
analise(dados_OLEO_DIESEL_NORTE_MED_DISTR_TS) 
analise(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_s10_NORTE_MED_REVENDA_TS) #para estes o tamanho da serie é pequena, não sendo confiável teste de estacionaridade
analise(dados_OLEO_s10_NORTE_MED_DISTR_TS) 
analise(dados_OLEO_s10_NORTE_MARGEM_REVENDA_TS)


###Inicialmente efetuaremos projeção das séries considerando os modelos de regressão linear e ARIMA (esta através
### da função auto.arima, que não requer série temporal previamente estacionária
### Além da projeção auto.arima também estamos utilizando tslm (time series linear model), para assim termos a possibilidade de comparação.

projecao_auto_arima(dados_GASOLINA_NORTE_MED_REVENDA_TS)
projecao_linear_model(dados_GASOLINA_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GASOLINA_NORTE_MED_DISTR_TS) 
projecao_linear_model(dados_GASOLINA_NORTE_MED_DISTR_TS)
projecao_auto_arima(dados_GASOLINA_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GASOLINA_NORTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_ETANOL_NORTE_MED_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_ETANOL_NORTE_MED_DISTR_TS) 
projecao_linear_model(dados_ETANOL_NORTE_MED_DISTR_TS)
projecao_auto_arima(dados_ETANOL_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_NORTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GLP_NORTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GLP_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GLP_NORTE_MED_DISTR_TS) 
projecao_linear_model(dados_GLP_NORTE_MED_DISTR_TS)
projecao_auto_arima(dados_GLP_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GLP_NORTE_MARGEM_REVENDA_TS)


projecao_auto_arima(dados_GNV_NORTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GNV_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GNV_NORTE_MED_DISTR_TS)
projecao_linear_model(dados_GNV_NORTE_MED_DISTR_TS) 
projecao_auto_arima(dados_GNV_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GNV_NORTE_MARGEM_REVENDA_TS) 

projecao_auto_arima(dados_OLEO_DIESEL_NORTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_DIESEL_NORTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS)


projecao_auto_arima(dados_OLEO_s10_NORTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_NORTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_s10_NORTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_s10_NORTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_s10_NORTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_NORTE_MARGEM_REVENDA_TS)


###Anteriormente utilizamos a função auto-arima, que pode não trazer resultados totalmente satistatórios, abaixo utilizaremos
### em um exemplo o modelo ARIMA, seguindo passo a passa a metodologia de box-jenkins

###Após analisar as séries identificamos que as mesmas não são estacionárias, para utilizarmos ARIMA precisaremos estacionar as mesmas.

dados_GASOLINA_NORTE_MED_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORTE_MED_REVENDA)
dados_GASOLINA_NORTE_MIN_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORTE_MIN_REVENDA)
dados_GASOLINA_NORTE_MAX_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORTE_MAX_REVENDA) 
dados_GASOLINA_NORTE_MED_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORTE_MED_DISTR) 
dados_GASOLINA_NORTE_MIN_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORTE_MIN_DISTR) 
dados_GASOLINA_NORTE_MAX_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORTE_MAX_DISTR) 
dados_GASOLINA_NORTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORTE_MARGEM_REVENDA) 
dados_ETANOL_NORTE_MED_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORTE_MED_REVENDA) 
dados_ETANOL_NORTE_MIN_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORTE_MIN_REVENDA) 
dados_ETANOL_NORTE_MAX_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORTE_MAX_REVENDA) 
dados_ETANOL_NORTE_MED_DISTR_TS_EST<-estacionar(dados_ETANOL_NORTE_MED_DISTR) 
dados_ETANOL_NORTE_MIN_DISTR_TS_EST<-estacionar(dados_ETANOL_NORTE_MIN_DISTR) 
dados_ETANOL_NORTE_MAX_DISTR_TS_EST<-estacionar(dados_ETANOL_NORTE_MAX_DISTR) 
dados_ETANOL_NORTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORTE_MARGEM_REVENDA) 
dados_GLP_NORTE_MED_REVENDA_TS_EST<-estacionar(dados_GLP_NORTE_MED_REVENDA) 
dados_GLP_NORTE_MIN_REVENDA_TS_EST<-estacionar(dados_GLP_NORTE_MIN_REVENDA) 
dados_GLP_NORTE_MAX_REVENDA_TS_EST<-estacionar(dados_GLP_NORTE_MAX_REVENDA) 
dados_GLP_NORTE_MED_DISTR_TS_EST<-estacionar(dados_GLP_NORTE_MED_DISTR) 
dados_GLP_NORTE_MIN_DISTR_TS_EST<-estacionar(dados_GLP_NORTE_MIN_DISTR) 
dados_GLP_NORTE_MAX_DISTR_TS_EST<-estacionar(dados_GLP_NORTE_MAX_DISTR) 
dados_GLP_NORTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GLP_NORTE_MARGEM_REVENDA) 
dados_GNV_NORTE_MED_REVENDA_TS_EST<-estacionar(dados_GNV_NORTE_MED_REVENDA) 
dados_GNV_NORTE_MIN_REVENDA_TS_EST<-estacionar(dados_GNV_NORTE_MIN_REVENDA) 
dados_GNV_NORTE_MAX_REVENDA_TS_EST<-estacionar(dados_GNV_NORTE_MAX_REVENDA) 
dados_GNV_NORTE_MED_DISTR_TS_EST <-estacionar(dados_GNV_NORTE_MED_DISTR) 
dados_GNV_NORTE_MIN_DISTR_TS_EST <-estacionar(dados_GNV_NORTE_MIN_DISTR) 
dados_GNV_NORTE_MAX_DISTR_TS_EST <-estacionar(dados_GNV_NORTE_MAX_DISTR) 
dados_GNV_NORTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_GNV_NORTE_MARGEM_REVENDA) 
dados_OLEO_DIESEL_NORTE_MED_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MED_REVENDA) 
dados_OLEO_DIESEL_NORTE_MIN_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MIN_REVENDA) 
dados_OLEO_DIESEL_NORTE_MAX_REVENDA_TS_EST <- estacionar(dados_OLEO_DIESEL_NORTE_MAX_REVENDA) 
dados_OLEO_DIESEL_NORTE_MED_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MED_DISTR) 
dados_OLEO_DIESEL_NORTE_MIN_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MIN_DISTR) 
dados_OLEO_DIESEL_NORTE_MAX_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MAX_DISTR) 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA)


#atividades referentes à metodologia de box-jenkins: Identificação, Estimação e previsão
#obtendo as funçoes de auto-correlação e auto-correlação parcial -válido para series estacionárias 
par(mfrow=c(1,2))
acf(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS_EST) #varias correlaçoes diferentes de zero
pacf(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS_EST) #AR (1) <> 0 - para AR verifica as ordens do pacf
##PAra MA já utiliza as ordens do ACF então para este caso seria um MA de ordem(1)

#criando modelo MA neste caso MA (1) para testes apenas (deta0+deta1*erro de modelo t-1)
modelom <- arima(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS_EST,order=c(0,0,1),fixed = c(NA,NA),method = c("ML"))
modelom
#divide valor do ma e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes teta0 e teta1.
coeficientesm <-modelom$coef
coeficientesm #o deta0 da equação é o intercept

acf(residuals(modelom)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado)
pacf(residuals(modelom)) #todos igual a zero, seria modelo adequado..que não foi o caso..apresentou resíduo em 6

## o modelom  que é um modelo MA(1), na análise dos residuos, apresentou residuo no partial ACF na posição 6
## o que sugere criação do modelo ARMA(6,1), como segue:

modeloarma <- arima(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_TS_EST,order=c(6,0,1),fixed = c(0,0,0,0,0,NA,NA,NA),method = c("ML"))
#divide valor dos coeficientes  e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes 
modeloarma
coeficientesarma <-modeloarma$coef
coeficientesarma
acf(residuals(modeloarma)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado) ..
pacf(residuals(modeloarma)) #todos igual a zero, modelo adequado

# efetuando projeção 6 meses 95% de confiança
frct <- forecast(modeloarma,6) #como esse modelo é um ARMA(6,1), só projetou 1 mes utilizando o ma1 , os demais utilizou apenas os ars. 
frct #Portanto se atendar a este fato, não faz sentido projetar periodos não "atingidos" pelo modelo
residuals(modeloarma) #erro: observado - estimado)
frct[["mean"]]

#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
a <- lengths(dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA)["MES"] - 6  #seleciondo a partir de Dezembro 2018
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[a,]$MES
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[a,]$ANO
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[a,3]


dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao = dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA


dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+1,3] = (frct[["mean"]] [1] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[a,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[a,3]
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+2,3] = (frct[["mean"]] [2] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+1,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+1,3] 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+3,3] = (frct[["mean"]] [3] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+2,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+2,3] 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+4,3] = (frct[["mean"]] [4] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+3,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+3,3] 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+5,3] = (frct[["mean"]] [5] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+4,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+4,3] 
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+6,3] = (frct[["mean"]] [6] * dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+5,3]) + dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[a+5,3] 

##Calculando o MAPE##
abs((dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA - dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao)/dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA * 100)
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA_Projecao[(a+1):(a+6),]
dados_OLEO_DIESEL_NORTE_MARGEM_REVENDA[(a+1):(a+6),]



##Região NORDESTE
#Gasolina
dados_GASOLINA_NORDESTE_TEMP <- subset(dados_GASOLINA,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                      "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GASOLINA_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                 data=dados_GASOLINA_NORDESTE_TEMP,
                                                 FUN = mean)

dados_GASOLINA_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                 data=dados_GASOLINA_NORDESTE_TEMP,
                                                 FUN = mean)
dados_GASOLINA_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                 data=dados_GASOLINA_NORDESTE_TEMP,
                                                 FUN = mean)
dados_GASOLINA_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_GASOLINA_NORDESTE_TEMP,
                                               FUN = mean)
dados_GASOLINA_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_GASOLINA_NORDESTE_TEMP,
                                               FUN = mean)
dados_GASOLINA_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_GASOLINA_NORDESTE_TEMP,
                                               FUN = mean)
dados_GASOLINA_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                    data=dados_GASOLINA_NORDESTE_TEMP,
                                                    FUN = mean)

# ETANOL HIDRATADO

dados_ETANOL_NORDESTE_TEMP <- subset(dados_ETANOL,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_ETANOL_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                               data=dados_ETANOL_NORDESTE_TEMP,
                                               FUN = mean)

dados_ETANOL_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                               data=dados_ETANOL_NORDESTE_TEMP,
                                               FUN = mean)
dados_ETANOL_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                               data=dados_ETANOL_NORDESTE_TEMP,
                                               FUN = mean)
dados_ETANOL_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_ETANOL_NORDESTE_TEMP,
                                             FUN = mean)
dados_ETANOL_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_ETANOL_NORDESTE_TEMP,
                                             FUN = mean)
dados_ETANOL_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_ETANOL_NORDESTE_TEMP,
                                             FUN = mean)
dados_ETANOL_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                  data=dados_ETANOL_NORDESTE_TEMP,
                                                  FUN = mean)
#GLP
dados_GLP_NORDESTE_TEMP <- subset(dados_GLP,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                            "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GLP_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORDESTE_TEMP,
                                            FUN = mean)

dados_GLP_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORDESTE_TEMP,
                                            FUN = mean)
dados_GLP_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                            data=dados_GLP_NORDESTE_TEMP,
                                            FUN = mean)
dados_GLP_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORDESTE_TEMP,
                                          FUN = mean)
dados_GLP_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORDESTE_TEMP,
                                          FUN = mean)
dados_GLP_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GLP_NORDESTE_TEMP,
                                          FUN = mean)
dados_GLP_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                               data=dados_GLP_NORDESTE_TEMP,
                                               FUN = mean)
#GNV

dados_GNV_NORDESTE_TEMP <- subset(dados_GNV,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                            "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GNV_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                            data=dados_GNV_NORDESTE_TEMP,
                                            FUN = mean)

dados_GNV_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                            data=dados_GNV_NORDESTE_TEMP,
                                            FUN = mean)
dados_GNV_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                            data=dados_GNV_NORDESTE_TEMP,
                                            FUN = mean)
dados_GNV_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GNV_NORDESTE_TEMP,
                                          FUN = mean)
dados_GNV_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GNV_NORDESTE_TEMP,
                                          FUN = mean)
dados_GNV_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GNV_NORDESTE_TEMP,
                                          FUN = mean)
dados_GNV_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                               data=dados_GNV_NORDESTE_TEMP,
                                               FUN = mean)
#óLEO DIESEL

dados_OLEO_DIESEL_NORDESTE_TEMP <- subset(dados_OLEO_DIESEL,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                            "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_DIESEL_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                    data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                    FUN = mean)

dados_OLEO_DIESEL_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                    data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                    FUN = mean)
dados_OLEO_DIESEL_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                    data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                    FUN = mean)
dados_OLEO_DIESEL_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                  data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                  FUN = mean)
dados_OLEO_DIESEL_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                  data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                  FUN = mean)
dados_OLEO_DIESEL_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                  data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                  FUN = mean)
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                       data=dados_OLEO_DIESEL_NORDESTE_TEMP,
                                                       FUN = mean)
#óLEO DIESEL s10

dados_OLEO_s10_NORDESTE_TEMP <- subset(dados_OLEO_DIESEL_S10,REGIAO == "NORDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                             "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_s10_NORDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORDESTE_TEMP,
                                                 FUN = mean)

dados_OLEO_s10_NORDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORDESTE_TEMP,
                                                 FUN = mean)
dados_OLEO_s10_NORDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                 data=dados_OLEO_s10_NORDESTE_TEMP,
                                                 FUN = mean)
dados_OLEO_s10_NORDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORDESTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORDESTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                               data=dados_OLEO_s10_NORDESTE_TEMP,
                                               FUN = mean)
dados_OLEO_s10_NORDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                    data=dados_OLEO_s10_NORDESTE_TEMP,
                                                    FUN = mean)
# Realizando análise descritiva das tabelas criadas
analise_descritiva(dados_GASOLINA_NORDESTE_TEMP)
analise_descritiva(dados_ETANOL_NORDESTE_TEMP)
analise_descritiva(dados_GLP_NORDESTE_TEMP)
analise_descritiva(dados_GNV_NORDESTE_TEMP)
analise_descritiva(dados_OLEO_DIESEL_NORDESTE_TEMP)
analise_descritiva(dados_OLEO_s10_NORDESTE_TEMP)

# transformar para série temporal

dados_GASOLINA_NORDESTE_MED_REVENDA_TS <- ts(dados_GASOLINA_NORDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORDESTE_MIN_REVENDA_TS <- ts(dados_GASOLINA_NORDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORDESTE_MAX_REVENDA_TS <- ts(dados_GASOLINA_NORDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_NORDESTE_MED_DISTR_TS <- ts(dados_GASOLINA_NORDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORDESTE_MIN_DISTR_TS <- ts(dados_GASOLINA_NORDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORDESTE_MAX_DISTR_TS <- ts(dados_GASOLINA_NORDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_GASOLINA_NORDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MED_REVENDA_TS <- ts(dados_ETANOL_NORDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MIN_REVENDA_TS <- ts(dados_ETANOL_NORDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MAX_REVENDA_TS <- ts(dados_ETANOL_NORDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MED_DISTR_TS <- ts(dados_ETANOL_NORDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MIN_DISTR_TS <- ts(dados_ETANOL_NORDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_NORDESTE_MAX_DISTR_TS <- ts(dados_ETANOL_NORDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_ETANOL_NORDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORDESTE_MED_REVENDA_TS  <- ts(dados_GLP_NORDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORDESTE_MIN_REVENDA_TS <- ts(dados_GLP_NORDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORDESTE_MAX_REVENDA_TS <- ts(dados_GLP_NORDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORDESTE_MED_DISTR_TS <- ts(dados_GLP_NORDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_NORDESTE_MIN_DISTR_TS <- ts(dados_GLP_NORDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORDESTE_MAX_DISTR_TS <- ts(dados_GLP_NORDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_GLP_NORDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MED_REVENDA_TS <- ts(dados_GNV_NORDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MIN_REVENDA_TS <- ts(dados_GNV_NORDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MAX_REVENDA_TS <- ts(dados_GNV_NORDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MED_DISTR_TS <- ts(dados_GNV_NORDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MIN_DISTR_TS <- ts(dados_GNV_NORDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MAX_DISTR_TS <- ts(dados_GNV_NORDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_GNV_NORDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MED_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MIN_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MAX_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MED_DISTR_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MIN_DISTR_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_OLEO_DIESEL_NORDESTE_MAX_DISTR_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_s10_NORDESTE_MED_REVENDA_TS <- ts(dados_OLEO_s10_NORDESTE_MED_REVENDA[, 3], start = c(2012, 12), frequency = 12)
dados_OLEO_s10_NORDESTE_MIN_REVENDA_TS <- ts(dados_OLEO_s10_NORDESTE_MIN_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORDESTE_MAX_REVENDA_TS <- ts(dados_OLEO_s10_NORDESTE_MAX_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORDESTE_MED_DISTR_TS <- ts(dados_OLEO_s10_NORDESTE_MED_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORDESTE_MIN_DISTR_TS <- ts(dados_OLEO_s10_NORDESTE_MIN_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORDESTE_MAX_DISTR_TS <- ts(dados_OLEO_s10_NORDESTE_MAX_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_NORDESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_s10_NORDESTE_MARGEM_REVENDA[, 3], start = c(2012, 12), frequency = 12)

#realizando análise exploratória das séries temporais

par(mfrow=c(1,3))
analise(dados_GASOLINA_NORDESTE_MED_REVENDA_TS)
analise(dados_GASOLINA_NORDESTE_MED_DISTR_TS) 
analise(dados_GASOLINA_NORDESTE_MARGEM_REVENDA_TS) 
analise(dados_ETANOL_NORDESTE_MED_REVENDA_TS) 
analise(dados_ETANOL_NORDESTE_MED_DISTR_TS) 
analise(dados_ETANOL_NORDESTE_MARGEM_REVENDA_TS) 
analise(dados_GLP_NORDESTE_MED_REVENDA_TS) 
analise(dados_GLP_NORDESTE_MED_DISTR_TS) 
analise(dados_GLP_NORDESTE_MARGEM_REVENDA_TS) 
analise(dados_GNV_NORDESTE_MED_REVENDA_TS) 
analise(dados_GNV_NORDESTE_MED_DISTR_TS) 
analise(dados_GNV_NORDESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_DIESEL_NORDESTE_MED_REVENDA_TS) 
analise(dados_OLEO_DIESEL_NORDESTE_MED_DISTR_TS) 
analise(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_s10_NORDESTE_MED_REVENDA_TS)
analise(dados_OLEO_s10_NORDESTE_MED_DISTR_TS) 
analise(dados_OLEO_s10_NORDESTE_MARGEM_REVENDA_TS)


###Iniciamente efetuaremos projeção das séries considerando os modelos de regressão linear e ARIMA (esta através
### da função auto.arima, que não requer série temporal previamente estacionária
### Além da projeção auto.arima também estamos utilizando tslm (time series linear model), para assim termos a possibilidade de comparação.
projecao_auto_arima(dados_GASOLINA_NORDESTE_MED_REVENDA_TS)
projecao_linear_model(dados_GASOLINA_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GASOLINA_NORDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GASOLINA_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GASOLINA_NORDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GASOLINA_NORDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_ETANOL_NORDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_ETANOL_NORDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_ETANOL_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_ETANOL_NORDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_NORDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GLP_NORDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GLP_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GLP_NORDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GLP_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GLP_NORDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GLP_NORDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GNV_NORDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GNV_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GNV_NORDESTE_MED_DISTR_TS)
projecao_linear_model(dados_GNV_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GNV_NORDESTE_MARGEM_REVENDA_TS)
projecao_linear_model(dados_GNV_NORDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_DIESEL_NORDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_DIESEL_NORDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS)


projecao_auto_arima(dados_OLEO_s10_NORDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_NORDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_s10_NORDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_s10_NORDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_s10_NORDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_NORDESTE_MARGEM_REVENDA_TS)

###Após analisar as séries identificamos que as mesmas não são estacionárias, para utilizarmos ARIMA precisaremos estacionar as mesmas.

dados_GASOLINA_NORDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MED_REVENDA)
dados_GASOLINA_NORDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MIN_REVENDA)
dados_GASOLINA_NORDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MAX_REVENDA) 
dados_GASOLINA_NORDESTE_MED_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MED_DISTR) 
dados_GASOLINA_NORDESTE_MIN_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MIN_DISTR) 
dados_GASOLINA_NORDESTE_MAX_DISTR_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MAX_DISTR) 
dados_GASOLINA_NORDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GASOLINA_NORDESTE_MARGEM_REVENDA) 
dados_ETANOL_NORDESTE_MED_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MED_REVENDA) 
dados_ETANOL_NORDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MIN_REVENDA) 
dados_ETANOL_NORDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MAX_REVENDA) 
dados_ETANOL_NORDESTE_MED_DISTR_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MED_DISTR) 
dados_ETANOL_NORDESTE_MIN_DISTR_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MIN_DISTR) 
dados_ETANOL_NORDESTE_MAX_DISTR_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MAX_DISTR) 
dados_ETANOL_NORDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_ETANOL_NORDESTE_MARGEM_REVENDA) 
dados_GLP_NORDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GLP_NORDESTE_MED_REVENDA) 
dados_GLP_NORDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GLP_NORDESTE_MIN_REVENDA) 
dados_GLP_NORDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GLP_NORDESTE_MAX_REVENDA) 
dados_GLP_NORDESTE_MED_DISTR_TS_EST<-estacionar(dados_GLP_NORDESTE_MED_DISTR) 
dados_GLP_NORDESTE_MIN_DISTR_TS_EST<-estacionar(dados_GLP_NORDESTE_MIN_DISTR) 
dados_GLP_NORDESTE_MAX_DISTR_TS_EST<-estacionar(dados_GLP_NORDESTE_MAX_DISTR) 
dados_GLP_NORDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GLP_NORDESTE_MARGEM_REVENDA) 
dados_GNV_NORDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GNV_NORDESTE_MED_REVENDA) 
dados_GNV_NORDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GNV_NORDESTE_MIN_REVENDA) 
dados_GNV_NORDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GNV_NORDESTE_MAX_REVENDA) 
dados_GNV_NORDESTE_MED_DISTR_TS_EST <-estacionar(dados_GNV_NORDESTE_MED_DISTR) 
dados_GNV_NORDESTE_MIN_DISTR_TS_EST <-estacionar(dados_GNV_NORDESTE_MIN_DISTR) 
dados_GNV_NORDESTE_MAX_DISTR_TS_EST <-estacionar(dados_GNV_NORDESTE_MAX_DISTR) 
dados_GNV_NORDESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_GNV_NORDESTE_MARGEM_REVENDA) 
dados_OLEO_DIESEL_NORDESTE_MED_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MED_REVENDA) 
dados_OLEO_DIESEL_NORDESTE_MIN_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MIN_REVENDA) 
dados_OLEO_DIESEL_NORDESTE_MAX_REVENDA_TS_EST <- estacionar(dados_OLEO_DIESEL_NORDESTE_MAX_REVENDA) 
dados_OLEO_DIESEL_NORDESTE_MED_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MED_DISTR) 
dados_OLEO_DIESEL_NORDESTE_MIN_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MIN_DISTR) 
dados_OLEO_DIESEL_NORDESTE_MAX_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MAX_DISTR) 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA)


#atividades referentes à metodologia de box-jenkins: Identificação, Estimação e previsão
#obtendo as funçoes de auto-correlação e auto-correlação parcial -válido para series estacionárias 
par(mfrow=c(1,2))
acf(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS_EST) #varias correlaçoes diferentes de zero
pacf(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS_EST) #AR incompleto (1) <> 0 - para AR verifica as ordens do pacf
##PAra MA já utiliza as ordens do ACF então para este caso seria um MA de ordem(1)

#criando modelo MA neste caso MA (1) para testes apenas (deta0+deta1*erro de modelo t-1)
modelom <- arima(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS_EST,order=c(0,0,1),fixed = c(NA,NA),method = c("ML"))
modelom
#divide valor do ma e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes
coeficientesm <-modelom$coef
coeficientesm #o deta0 da equação é o intercept

acf(residuals(modelom)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado)
pacf(residuals(modelom)) #todos igual a zero, modelo adequado

## o modelom  que é um modelo MA(1), na análise dos residuos, apresentou residuo no partial ACF na posição 5
## o que sugere criação do modelo ARMA(5,1), como segue:

modeloarma <- arima(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_TS_EST,order=c(5,0,1),fixed = c(0,0,0,0,NA,NA,NA),method = c("ML"))
#divide valor dos coeficientes  e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes 
modeloarma
coeficientesarma <-modeloarma$coef
coeficientesarma
acf(residuals(modeloarma)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado) ..
pacf(residuals(modeloarma)) #todos igual a zero, modelo adequado



# efetuando projeção 6 meses 95% de confiança
frct <- forecast(modeloarma,6) #como esse modelo é um MA(1)... 
frct #Portanto se atendar a este fato, não faz sentido projetar periodos não "atingidos" pelo modelo
residuals(modelom) #erro: observado - estimado)
frct[["mean"]]

#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
a <- lengths(dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA)["MES"] - 6  #seleciondo a partir de Dezembro 2018
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[a,]$MES
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[a,]$ANO
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[a,3]


dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao = dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA


dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+1,3] = (frct[["mean"]] [1] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[a,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[a,3]
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+2,3] = (frct[["mean"]] [2] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+1,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+1,3] 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+3,3] = (frct[["mean"]] [3] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+2,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+2,3] 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+4,3] = (frct[["mean"]] [4] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+3,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+3,3] 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+5,3] = (frct[["mean"]] [5] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+4,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+4,3] 
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+6,3] = (frct[["mean"]] [6] * dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+5,3]) + dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[a+5,3]

##Calculando o MAPE##
abs((dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA - dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao)/dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA * 100)
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA_Projecao[(a+1):(a+6),]
dados_OLEO_DIESEL_NORDESTE_MARGEM_REVENDA[(a+1):(a+6),]



##Região SUL
#Gasolina
dados_GASOLINA_SUL_TEMP <- subset(dados_GASOLINA,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                            "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GASOLINA_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                            data=dados_GASOLINA_SUL_TEMP,
                                            FUN = mean)

dados_GASOLINA_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                            data=dados_GASOLINA_SUL_TEMP,
                                            FUN = mean)
dados_GASOLINA_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                            data=dados_GASOLINA_SUL_TEMP,
                                            FUN = mean)
dados_GASOLINA_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GASOLINA_SUL_TEMP,
                                          FUN = mean)
dados_GASOLINA_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GASOLINA_SUL_TEMP,
                                          FUN = mean)
dados_GASOLINA_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_GASOLINA_SUL_TEMP,
                                          FUN = mean)
dados_GASOLINA_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                               data=dados_GASOLINA_SUL_TEMP,
                                               FUN = mean)

# ETANOL HIDRATADO

dados_ETANOL_SUL_TEMP <- subset(dados_ETANOL,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                        "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_ETANOL_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                          data=dados_ETANOL_SUL_TEMP,
                                          FUN = mean)

dados_ETANOL_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                          data=dados_ETANOL_SUL_TEMP,
                                          FUN = mean)
dados_ETANOL_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                          data=dados_ETANOL_SUL_TEMP,
                                          FUN = mean)
dados_ETANOL_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                        data=dados_ETANOL_SUL_TEMP,
                                        FUN = mean)
dados_ETANOL_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                        data=dados_ETANOL_SUL_TEMP,
                                        FUN = mean)
dados_ETANOL_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                        data=dados_ETANOL_SUL_TEMP,
                                        FUN = mean)
dados_ETANOL_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                             data=dados_ETANOL_SUL_TEMP,
                                             FUN = mean)
#GLP
dados_GLP_SUL_TEMP <- subset(dados_GLP,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GLP_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                       data=dados_GLP_SUL_TEMP,
                                       FUN = mean)

dados_GLP_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                       data=dados_GLP_SUL_TEMP,
                                       FUN = mean)
dados_GLP_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                       data=dados_GLP_SUL_TEMP,
                                       FUN = mean)
dados_GLP_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GLP_SUL_TEMP,
                                     FUN = mean)
dados_GLP_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GLP_SUL_TEMP,
                                     FUN = mean)
dados_GLP_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GLP_SUL_TEMP,
                                     FUN = mean)
dados_GLP_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                          data=dados_GLP_SUL_TEMP,
                                          FUN = mean)
#GNV

dados_GNV_SUL_TEMP <- subset(dados_GNV,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GNV_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                       data=dados_GNV_SUL_TEMP,
                                       FUN = mean)

dados_GNV_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                       data=dados_GNV_SUL_TEMP,
                                       FUN = mean)
dados_GNV_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                       data=dados_GNV_SUL_TEMP,
                                       FUN = mean)
dados_GNV_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GNV_SUL_TEMP,
                                     FUN = mean)
dados_GNV_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GNV_SUL_TEMP,
                                     FUN = mean)
dados_GNV_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                     data=dados_GNV_SUL_TEMP,
                                     FUN = mean)
dados_GNV_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                          data=dados_GNV_SUL_TEMP,
                                          FUN = mean)
#óLEO DIESEL

dados_OLEO_DIESEL_SUL_TEMP <- subset(dados_OLEO_DIESEL,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                  "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_DIESEL_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                               data=dados_OLEO_DIESEL_SUL_TEMP,
                                               FUN = mean)

dados_OLEO_DIESEL_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                               data=dados_OLEO_DIESEL_SUL_TEMP,
                                               FUN = mean)
dados_OLEO_DIESEL_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                               data=dados_OLEO_DIESEL_SUL_TEMP,
                                               FUN = mean)
dados_OLEO_DIESEL_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_OLEO_DIESEL_SUL_TEMP,
                                             FUN = mean)
dados_OLEO_DIESEL_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_OLEO_DIESEL_SUL_TEMP,
                                             FUN = mean)
dados_OLEO_DIESEL_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                             data=dados_OLEO_DIESEL_SUL_TEMP,
                                             FUN = mean)
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                  data=dados_OLEO_DIESEL_SUL_TEMP,
                                                  FUN = mean)
#óLEO DIESEL s10

dados_OLEO_s10_SUL_TEMP <- subset(dados_OLEO_DIESEL_S10,REGIAO == "SUL",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                   "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_s10_SUL_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                            data=dados_OLEO_s10_SUL_TEMP,
                                            FUN = mean)

dados_OLEO_s10_SUL_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                            data=dados_OLEO_s10_SUL_TEMP,
                                            FUN = mean)
dados_OLEO_s10_SUL_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                            data=dados_OLEO_s10_SUL_TEMP,
                                            FUN = mean)
dados_OLEO_s10_SUL_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_OLEO_s10_SUL_TEMP,
                                          FUN = mean)
dados_OLEO_s10_SUL_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_OLEO_s10_SUL_TEMP,
                                          FUN = mean)
dados_OLEO_s10_SUL_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                          data=dados_OLEO_s10_SUL_TEMP,
                                          FUN = mean)
dados_OLEO_s10_SUL_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                               data=dados_OLEO_s10_SUL_TEMP,
                                               FUN = mean)
# Realizando análise descritiva das tabelas criadas
analise_descritiva(dados_GASOLINA_SUL_TEMP)
analise_descritiva(dados_ETANOL_SUL_TEMP)
analise_descritiva(dados_GLP_SUL_TEMP)
analise_descritiva(dados_GNV_SUL_TEMP)
analise_descritiva(dados_OLEO_DIESEL_SUL_TEMP)
analise_descritiva(dados_OLEO_s10_SUL_TEMP)


# transformar para série temporal

dados_GASOLINA_SUL_MED_REVENDA_TS <- ts(dados_GASOLINA_SUL_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUL_MIN_REVENDA_TS <- ts(dados_GASOLINA_SUL_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUL_MAX_REVENDA_TS <- ts(dados_GASOLINA_SUL_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUL_MED_DISTR_TS <- ts(dados_GASOLINA_SUL_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUL_MIN_DISTR_TS <- ts(dados_GASOLINA_SUL_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUL_MAX_DISTR_TS <- ts(dados_GASOLINA_SUL_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUL_MARGEM_REVENDA_TS <- ts(dados_GASOLINA_SUL_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MED_REVENDA_TS <- ts(dados_ETANOL_SUL_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MIN_REVENDA_TS <- ts(dados_ETANOL_SUL_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MAX_REVENDA_TS <- ts(dados_ETANOL_SUL_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MED_DISTR_TS <- ts(dados_ETANOL_SUL_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MIN_DISTR_TS <- ts(dados_ETANOL_SUL_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_SUL_MAX_DISTR_TS <- ts(dados_ETANOL_SUL_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUL_MARGEM_REVENDA_TS <- ts(dados_ETANOL_SUL_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUL_MED_REVENDA_TS  <- ts(dados_GLP_SUL_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUL_MIN_REVENDA_TS <- ts(dados_GLP_SUL_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUL_MAX_REVENDA_TS <- ts(dados_GLP_SUL_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUL_MED_DISTR_TS <- ts(dados_GLP_SUL_MED_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUL_MIN_DISTR_TS <- ts(dados_GLP_SUL_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUL_MAX_DISTR_TS <- ts(dados_GLP_SUL_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUL_MARGEM_REVENDA_TS <- ts(dados_GLP_SUL_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MED_REVENDA_TS <- ts(dados_GNV_SUL_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MIN_REVENDA_TS <- ts(dados_GNV_SUL_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MAX_REVENDA_TS <- ts(dados_GNV_SUL_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MED_DISTR_TS <- ts(dados_GNV_SUL_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MIN_DISTR_TS <- ts(dados_GNV_SUL_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MAX_DISTR_TS <- ts(dados_GNV_SUL_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUL_MARGEM_REVENDA_TS <- ts(dados_GNV_SUL_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MED_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUL_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MIN_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUL_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MAX_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUL_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MED_DISTR_TS <- ts(dados_OLEO_DIESEL_SUL_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MIN_DISTR_TS <- ts(dados_OLEO_DIESEL_SUL_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_OLEO_DIESEL_SUL_MAX_DISTR_TS <- ts(dados_OLEO_DIESEL_SUL_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_s10_SUL_MED_REVENDA_TS <- ts(dados_OLEO_s10_SUL_MED_REVENDA[, 3], start = c(2012,12), frequency = 12)
dados_OLEO_s10_SUL_MIN_REVENDA_TS <- ts(dados_OLEO_s10_SUL_MIN_REVENDA[, 3], start = c(2012,12), frequency = 12) 
dados_OLEO_s10_SUL_MAX_REVENDA_TS <- ts(dados_OLEO_s10_SUL_MAX_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUL_MED_DISTR_TS <- ts(dados_OLEO_s10_SUL_MED_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUL_MIN_DISTR_TS <- ts(dados_OLEO_s10_SUL_MIN_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUL_MAX_DISTR_TS <- ts(dados_OLEO_s10_SUL_MAX_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUL_MARGEM_REVENDA_TS <- ts(dados_OLEO_s10_SUL_MARGEM_REVENDA[, 3], start = c(2012, 12), frequency = 12)

#realizando análise exploratória das séries temporais

par(mfrow=c(1,3))
analise(dados_GASOLINA_SUL_MED_REVENDA_TS)
analise(dados_GASOLINA_SUL_MED_DISTR_TS) 
analise(dados_GASOLINA_SUL_MARGEM_REVENDA_TS) 
analise(dados_ETANOL_SUL_MED_REVENDA_TS) 
analise(dados_ETANOL_SUL_MED_DISTR_TS) 
analise(dados_ETANOL_SUL_MARGEM_REVENDA_TS) 
analise(dados_GLP_SUL_MED_REVENDA_TS) 
analise(dados_GLP_SUL_MED_DISTR_TS) 
analise(dados_GLP_SUL_MARGEM_REVENDA_TS) 
analise(dados_GNV_SUL_MED_REVENDA_TS) 
analise(dados_GNV_SUL_MED_DISTR_TS) 
analise(dados_GNV_SUL_MARGEM_REVENDA_TS) 
analise(dados_OLEO_DIESEL_SUL_MED_REVENDA_TS) 
analise(dados_OLEO_DIESEL_SUL_MED_DISTR_TS) 
analise(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS) 
analise(dados_OLEO_s10_SUL_MED_REVENDA_TS) #para estes o tamanho da serie é pequena, não sendo confiável teste de estacionaridade
analise(dados_OLEO_s10_SUL_MED_DISTR_TS) 
analise(dados_OLEO_s10_SUL_MARGEM_REVENDA_TS)


###Iniciamente efetuaremos projeção das séries considerando os modelos de regressão linear e ARIMA (esta através
### da função auto.arima, que não requer série temporal previamente estacionária
### Além da projeção auto.arima também estamos utilizando tslm (time series linear model), para assim termos a possibilidade de comparação.
projecao_auto_arima(dados_GASOLINA_SUL_MED_REVENDA_TS)
projecao_linear_model(dados_GASOLINA_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_GASOLINA_SUL_MED_DISTR_TS) 
projecao_linear_model(dados_GASOLINA_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_GASOLINA_SUL_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GASOLINA_SUL_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_ETANOL_SUL_MED_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_ETANOL_SUL_MED_DISTR_TS) 
projecao_linear_model(dados_ETANOL_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_ETANOL_SUL_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_SUL_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GLP_SUL_MED_REVENDA_TS) 
projecao_linear_model(dados_GLP_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_GLP_SUL_MED_DISTR_TS) 
projecao_linear_model(dados_GLP_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_GLP_SUL_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GLP_SUL_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GNV_SUL_MED_REVENDA_TS) 
projecao_linear_model(dados_GNV_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_GNV_SUL_MED_DISTR_TS)
projecao_linear_model(dados_GNV_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_GNV_SUL_MARGEM_REVENDA_TS)
projecao_linear_model(dados_GNV_SUL_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_DIESEL_SUL_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_DIESEL_SUL_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_s10_SUL_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_SUL_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_s10_SUL_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_s10_SUL_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_s10_SUL_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_SUL_MARGEM_REVENDA_TS)

###Após analisar as séries identificamos que as mesmas não são estacionárias, para utilizarmos ARIMA precisaremos estacionar as mesmas.

dados_GASOLINA_SUL_MED_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUL_MED_REVENDA)
dados_GASOLINA_SUL_MIN_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUL_MIN_REVENDA)
dados_GASOLINA_SUL_MAX_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUL_MAX_REVENDA) 
dados_GASOLINA_SUL_MED_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUL_MED_DISTR) 
dados_GASOLINA_SUL_MIN_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUL_MIN_DISTR) 
dados_GASOLINA_SUL_MAX_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUL_MAX_DISTR) 
dados_GASOLINA_SUL_MARGEM_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUL_MARGEM_REVENDA) 
dados_ETANOL_SUL_MED_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUL_MED_REVENDA) 
dados_ETANOL_SUL_MIN_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUL_MIN_REVENDA) 
dados_ETANOL_SUL_MAX_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUL_MAX_REVENDA) 
dados_ETANOL_SUL_MED_DISTR_TS_EST<-estacionar(dados_ETANOL_SUL_MED_DISTR) 
dados_ETANOL_SUL_MIN_DISTR_TS_EST<-estacionar(dados_ETANOL_SUL_MIN_DISTR) 
dados_ETANOL_SUL_MAX_DISTR_TS_EST<-estacionar(dados_ETANOL_SUL_MAX_DISTR) 
dados_ETANOL_SUL_MARGEM_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUL_MARGEM_REVENDA) 
dados_GLP_SUL_MED_REVENDA_TS_EST<-estacionar(dados_GLP_SUL_MED_REVENDA) 
dados_GLP_SUL_MIN_REVENDA_TS_EST<-estacionar(dados_GLP_SUL_MIN_REVENDA) 
dados_GLP_SUL_MAX_REVENDA_TS_EST<-estacionar(dados_GLP_SUL_MAX_REVENDA) 
dados_GLP_SUL_MED_DISTR_TS_EST<-estacionar(dados_GLP_SUL_MED_DISTR) 
dados_GLP_SUL_MIN_DISTR_TS_EST<-estacionar(dados_GLP_SUL_MIN_DISTR) 
dados_GLP_SUL_MAX_DISTR_TS_EST<-estacionar(dados_GLP_SUL_MAX_DISTR) 
dados_GLP_SUL_MARGEM_REVENDA_TS_EST<-estacionar(dados_GLP_SUL_MARGEM_REVENDA) 
dados_GNV_SUL_MED_REVENDA_TS_EST<-estacionar(dados_GNV_SUL_MED_REVENDA) 
dados_GNV_SUL_MIN_REVENDA_TS_EST<-estacionar(dados_GNV_SUL_MIN_REVENDA) 
dados_GNV_SUL_MAX_REVENDA_TS_EST<-estacionar(dados_GNV_SUL_MAX_REVENDA) 
dados_GNV_SUL_MED_DISTR_TS_EST <-estacionar(dados_GNV_SUL_MED_DISTR) 
dados_GNV_SUL_MIN_DISTR_TS_EST <-estacionar(dados_GNV_SUL_MIN_DISTR) 
dados_GNV_SUL_MAX_DISTR_TS_EST <-estacionar(dados_GNV_SUL_MAX_DISTR) 
dados_GNV_SUL_MARGEM_REVENDA_TS_EST <-estacionar(dados_GNV_SUL_MARGEM_REVENDA) 
dados_OLEO_DIESEL_SUL_MED_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MED_REVENDA) 
dados_OLEO_DIESEL_SUL_MIN_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MIN_REVENDA) 
dados_OLEO_DIESEL_SUL_MAX_REVENDA_TS_EST <- estacionar(dados_OLEO_DIESEL_SUL_MAX_REVENDA) 
dados_OLEO_DIESEL_SUL_MED_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MED_DISTR) 
ados_OLEO_DIESEL_SUL_MIN_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MIN_DISTR) 
dados_OLEO_DIESEL_SUL_MAX_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MAX_DISTR) 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA)


#atividades referentes à metodologia de box-jenkins: Identificação, Estimação e previsão
#obtendo as funçoes de auto-correlação e auto-correlação parcial -válido para series estacionárias 
par(mfrow=c(1,2))
acf(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS_EST) #varias correlaçoes diferentes de zero
pacf(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS_EST) #AR incompleto (1,6) <> 0 - para AR verifica as ordens do pacf
##PAra MA já utiliza as ordens do ACF então para este caso seria um MA de ordem(1)

#criando modelo MA neste caso MA (1) para testes apenas (deta0+deta1*erro de modelo t-1)
modelom <- arima(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS_EST,order=c(0,0,1),fixed = c(NA,NA),method = c("ML"))
modelom
#divide valor do ma e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes teta0 e teta1.
coeficientesm <-modelom$coef
coeficientesm #o deta0 da equação é o intercept

acf(residuals(modelom)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado)
pacf(residuals(modelom)) #todos igual a zero, seria modelo adequado..que não foi o caso..apresentou resíduo em 6

## o modelom  que é um modelo MA(1), na análise dos residuos, apresentou residuo no partial ACF nas posição 6
## o que sugere criação do modelo ARMA(6,1), como segue:

modeloarma <- arima(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_TS_EST,order=c(6,0,1),fixed = c(0,0,0,0,0,NA,NA,NA),method = c("ML"))
#divide valor dos coeficientes  e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes 
modeloarma
coeficientesarma <-modeloarma$coef
coeficientesarma
acf(residuals(modeloarma)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado) ..
pacf(residuals(modeloarma)) #todos igual a zero, modelo adequado

# efetuando projeção 6 meses 95% de confiança
frct <- forecast(modeloarma,6) #como esse modelo é um ARMA(6,1), só projetou 1 mes utilizando o ma1 , os demais utilizou apenas os ars. 
frct #Portanto se atendar a este fato, não faz sentido projetar periodos não "atingidos" pelo modelo
residuals(modeloarma) #erro: observado - estimado)
frct[["mean"]]

#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
a <- lengths(dados_OLEO_DIESEL_SUL_MARGEM_REVENDA)["MES"] - 6  #seleciondo a partir de Dezembro 2018
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[a,]$MES
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[a,]$ANO
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[a,3]


dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao = dados_OLEO_DIESEL_SUL_MARGEM_REVENDA


dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+1,3] = (frct[["mean"]] [1] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[a,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[a,3]
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+2,3] = (frct[["mean"]] [2] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+1,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+1,3] 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+3,3] = (frct[["mean"]] [3] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+2,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+2,3] 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+4,3] = (frct[["mean"]] [4] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+3,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+3,3] 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+5,3] = (frct[["mean"]] [5] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+4,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+4,3] 
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+6,3] = (frct[["mean"]] [6] * dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+5,3]) + dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[a+5,3]

##Calculando o MAPE##
abs((dados_OLEO_DIESEL_SUL_MARGEM_REVENDA - dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao)/dados_OLEO_DIESEL_SUL_MARGEM_REVENDA * 100)
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA_Projecao[(a+1):(a+6),]
dados_OLEO_DIESEL_SUL_MARGEM_REVENDA[(a+1):(a+6),]



##Região SUDESTE
#Gasolina
dados_GASOLINA_SUDESTE_TEMP <- subset(dados_GASOLINA,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                    "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GASOLINA_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                data=dados_GASOLINA_SUDESTE_TEMP,
                                                FUN = mean)

dados_GASOLINA_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                data=dados_GASOLINA_SUDESTE_TEMP,
                                                FUN = mean)
dados_GASOLINA_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                data=dados_GASOLINA_SUDESTE_TEMP,
                                                FUN = mean)
dados_GASOLINA_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GASOLINA_SUDESTE_TEMP,
                                              FUN = mean)
dados_GASOLINA_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GASOLINA_SUDESTE_TEMP,
                                              FUN = mean)
dados_GASOLINA_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GASOLINA_SUDESTE_TEMP,
                                              FUN = mean)
dados_GASOLINA_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                   data=dados_GASOLINA_SUDESTE_TEMP,
                                                   FUN = mean)

# ETANOL HIDRATADO

dados_ETANOL_SUDESTE_TEMP <- subset(dados_ETANOL,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_ETANOL_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_SUDESTE_TEMP,
                                              FUN = mean)

dados_ETANOL_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_SUDESTE_TEMP,
                                              FUN = mean)
dados_ETANOL_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                              data=dados_ETANOL_SUDESTE_TEMP,
                                              FUN = mean)
dados_ETANOL_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_SUDESTE_TEMP,
                                            FUN = mean)
dados_ETANOL_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_SUDESTE_TEMP,
                                            FUN = mean)
dados_ETANOL_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                            data=dados_ETANOL_SUDESTE_TEMP,
                                            FUN = mean)
dados_ETANOL_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                 data=dados_ETANOL_SUDESTE_TEMP,
                                                 FUN = mean)
#GLP
dados_GLP_SUDESTE_TEMP <- subset(dados_GLP,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                          "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GLP_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                           data=dados_GLP_SUDESTE_TEMP,
                                           FUN = mean)

dados_GLP_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                           data=dados_GLP_SUDESTE_TEMP,
                                           FUN = mean)
dados_GLP_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                           data=dados_GLP_SUDESTE_TEMP,
                                           FUN = mean)
dados_GLP_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GLP_SUDESTE_TEMP,
                                         FUN = mean)
dados_GLP_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GLP_SUDESTE_TEMP,
                                         FUN = mean)
dados_GLP_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GLP_SUDESTE_TEMP,
                                         FUN = mean)
dados_GLP_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                              data=dados_GLP_SUDESTE_TEMP,
                                              FUN = mean)
#GNV

dados_GNV_SUDESTE_TEMP <- subset(dados_GNV,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                          "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GNV_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                           data=dados_GNV_SUDESTE_TEMP,
                                           FUN = mean)

dados_GNV_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                           data=dados_GNV_SUDESTE_TEMP,
                                           FUN = mean)
dados_GNV_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                           data=dados_GNV_SUDESTE_TEMP,
                                           FUN = mean)
dados_GNV_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GNV_SUDESTE_TEMP,
                                         FUN = mean)
dados_GNV_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GNV_SUDESTE_TEMP,
                                         FUN = mean)
dados_GNV_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                         data=dados_GNV_SUDESTE_TEMP,
                                         FUN = mean)
dados_GNV_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                              data=dados_GNV_SUDESTE_TEMP,
                                              FUN = mean)
#óLEO DIESEL

dados_OLEO_DIESEL_SUDESTE_TEMP <- subset(dados_OLEO_DIESEL,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                          "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_DIESEL_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                   data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                   FUN = mean)

dados_OLEO_DIESEL_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                   data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                   FUN = mean)
dados_OLEO_DIESEL_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                   data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                   FUN = mean)
dados_OLEO_DIESEL_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                 FUN = mean)
dados_OLEO_DIESEL_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                 FUN = mean)
dados_OLEO_DIESEL_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                 FUN = mean)
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                      data=dados_OLEO_DIESEL_SUDESTE_TEMP,
                                                      FUN = mean)
#óLEO DIESEL s10

dados_OLEO_s10_SUDESTE_TEMP <- subset(dados_OLEO_DIESEL_S10,REGIAO == "SUDESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                           "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_s10_SUDESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                data=dados_OLEO_s10_SUDESTE_TEMP,
                                                FUN = mean)

dados_OLEO_s10_SUDESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                data=dados_OLEO_s10_SUDESTE_TEMP,
                                                FUN = mean)
dados_OLEO_s10_SUDESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                data=dados_OLEO_s10_SUDESTE_TEMP,
                                                FUN = mean)
dados_OLEO_s10_SUDESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_OLEO_s10_SUDESTE_TEMP,
                                              FUN = mean)
dados_OLEO_s10_SUDESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_OLEO_s10_SUDESTE_TEMP,
                                              FUN = mean)
dados_OLEO_s10_SUDESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_OLEO_s10_SUDESTE_TEMP,
                                              FUN = mean)
dados_OLEO_s10_SUDESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                   data=dados_OLEO_s10_SUDESTE_TEMP,
                                                   FUN = mean)
# Realizando análise descritiva das tabelas criadas
analise_descritiva(dados_GASOLINA_SUDESTE_TEMP)
analise_descritiva(dados_ETANOL_SUDESTE_TEMP)
analise_descritiva(dados_GLP_SUDESTE_TEMP)
analise_descritiva(dados_GNV_SUDESTE_TEMP)
analise_descritiva(dados_OLEO_DIESEL_SUDESTE_TEMP)
analise_descritiva(dados_OLEO_s10_SUDESTE_TEMP)


# transformar para série temporal

dados_GASOLINA_SUDESTE_MED_REVENDA_TS <- ts(dados_GASOLINA_SUDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUDESTE_MIN_REVENDA_TS <- ts(dados_GASOLINA_SUDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUDESTE_MAX_REVENDA_TS <- ts(dados_GASOLINA_SUDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_SUDESTE_MED_DISTR_TS <- ts(dados_GASOLINA_SUDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUDESTE_MIN_DISTR_TS <- ts(dados_GASOLINA_SUDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUDESTE_MAX_DISTR_TS <- ts(dados_GASOLINA_SUDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_GASOLINA_SUDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MED_REVENDA_TS <- ts(dados_ETANOL_SUDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MIN_REVENDA_TS <- ts(dados_ETANOL_SUDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MAX_REVENDA_TS <- ts(dados_ETANOL_SUDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MED_DISTR_TS <- ts(dados_ETANOL_SUDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MIN_DISTR_TS <- ts(dados_ETANOL_SUDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_SUDESTE_MAX_DISTR_TS <- ts(dados_ETANOL_SUDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_ETANOL_SUDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUDESTE_MED_REVENDA_TS  <- ts(dados_GLP_SUDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUDESTE_MIN_REVENDA_TS <- ts(dados_GLP_SUDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUDESTE_MAX_REVENDA_TS <- ts(dados_GLP_SUDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUDESTE_MED_DISTR_TS <- ts(dados_GLP_SUDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_SUDESTE_MIN_DISTR_TS <- ts(dados_GLP_SUDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUDESTE_MAX_DISTR_TS <- ts(dados_GLP_SUDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_GLP_SUDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MED_REVENDA_TS <- ts(dados_GNV_SUDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MIN_REVENDA_TS <- ts(dados_GNV_SUDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MAX_REVENDA_TS <- ts(dados_GNV_SUDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MED_DISTR_TS <- ts(dados_GNV_SUDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MIN_DISTR_TS <- ts(dados_GNV_SUDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MAX_DISTR_TS <- ts(dados_GNV_SUDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_GNV_SUDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MED_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MIN_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MAX_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MED_DISTR_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MIN_DISTR_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_OLEO_DIESEL_SUDESTE_MAX_DISTR_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_s10_SUDESTE_MED_REVENDA_TS <- ts(dados_OLEO_s10_SUDESTE_MED_REVENDA[, 3], start = c(2012, 12), frequency = 12)
dados_OLEO_s10_SUDESTE_MIN_REVENDA_TS <- ts(dados_OLEO_s10_SUDESTE_MIN_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUDESTE_MAX_REVENDA_TS <- ts(dados_OLEO_s10_SUDESTE_MAX_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUDESTE_MED_DISTR_TS <- ts(dados_OLEO_s10_SUDESTE_MED_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUDESTE_MIN_DISTR_TS <- ts(dados_OLEO_s10_SUDESTE_MIN_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUDESTE_MAX_DISTR_TS <- ts(dados_OLEO_s10_SUDESTE_MAX_DISTR[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_SUDESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_s10_SUDESTE_MARGEM_REVENDA[, 3], start = c(2012, 12), frequency = 12)

#realizando análise exploratória das séries temporais

par(mfrow=c(1,3))
analise(dados_GASOLINA_SUDESTE_MED_REVENDA_TS)
analise(dados_GASOLINA_SUDESTE_MED_DISTR_TS) 
analise(dados_GASOLINA_SUDESTE_MARGEM_REVENDA_TS) 
analise(dados_ETANOL_SUDESTE_MED_REVENDA_TS) 
analise(dados_ETANOL_SUDESTE_MED_DISTR_TS) 
analise(dados_ETANOL_SUDESTE_MARGEM_REVENDA_TS) 
analise(dados_GLP_SUDESTE_MED_REVENDA_TS) 
analise(dados_GLP_SUDESTE_MED_DISTR_TS) 
analise(dados_GLP_SUDESTE_MARGEM_REVENDA_TS) 
analise(dados_GNV_SUDESTE_MED_REVENDA_TS) 
analise(dados_GNV_SUDESTE_MED_DISTR_TS) 
analise(dados_GNV_SUDESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_DIESEL_SUDESTE_MED_REVENDA_TS) 
analise(dados_OLEO_DIESEL_SUDESTE_MED_DISTR_TS) 
analise(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_s10_SUDESTE_MED_REVENDA_TS) #para estes o tamanho da serie é pequena, não sendo confiável teste de estacionaridade
analise(dados_OLEO_s10_SUDESTE_MED_DISTR_TS) 
analise(dados_OLEO_s10_SUDESTE_MARGEM_REVENDA_TS)

###Iniciamente efetuaremos projeção das séries considerando os modelos de regressão linear e ARIMA (esta através
### da função auto.arima, que não requer série temporal previamente estacionária)
### Além da projeção auto.arima também estamos utilizando tslm (time series linear model), para assim termos a possibilidade de comparação.
projecao_auto_arima(dados_GASOLINA_SUDESTE_MED_REVENDA_TS)
projecao_linear_model(dados_GASOLINA_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GASOLINA_SUDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GASOLINA_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GASOLINA_SUDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GASOLINA_SUDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_ETANOL_SUDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_ETANOL_SUDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_ETANOL_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_ETANOL_SUDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_SUDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GLP_SUDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GLP_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GLP_SUDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GLP_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GLP_SUDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GLP_SUDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GNV_SUDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GNV_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GNV_SUDESTE_MED_DISTR_TS)
projecao_linear_model(dados_GNV_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GNV_SUDESTE_MARGEM_REVENDA_TS)
projecao_linear_model(dados_GNV_SUDESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_DIESEL_SUDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_DIESEL_SUDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS)


projecao_auto_arima(dados_OLEO_s10_SUDESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_SUDESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_s10_SUDESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_s10_SUDESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_s10_SUDESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_SUDESTE_MARGEM_REVENDA_TS)


###Após analisar as séries identificamos que as mesmas não são estacionárias, para utilizarmos ARIMA precisaremos estacionar as mesmas.

dados_GASOLINA_SUDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MED_REVENDA)
dados_GASOLINA_SUDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MIN_REVENDA)
dados_GASOLINA_SUDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MAX_REVENDA) 
dados_GASOLINA_SUDESTE_MED_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MED_DISTR) 
dados_GASOLINA_SUDESTE_MIN_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MIN_DISTR) 
dados_GASOLINA_SUDESTE_MAX_DISTR_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MAX_DISTR) 
dados_GASOLINA_SUDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GASOLINA_SUDESTE_MARGEM_REVENDA) 
dados_ETANOL_SUDESTE_MED_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MED_REVENDA) 
dados_ETANOL_SUDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MIN_REVENDA) 
dados_ETANOL_SUDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MAX_REVENDA) 
dados_ETANOL_SUDESTE_MED_DISTR_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MED_DISTR) 
dados_ETANOL_SUDESTE_MIN_DISTR_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MIN_DISTR) 
dados_ETANOL_SUDESTE_MAX_DISTR_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MAX_DISTR) 
dados_ETANOL_SUDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_ETANOL_SUDESTE_MARGEM_REVENDA) 
dados_GLP_SUDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GLP_SUDESTE_MED_REVENDA) 
dados_GLP_SUDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GLP_SUDESTE_MIN_REVENDA) 
dados_GLP_SUDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GLP_SUDESTE_MAX_REVENDA) 
dados_GLP_SUDESTE_MED_DISTR_TS_EST<-estacionar(dados_GLP_SUDESTE_MED_DISTR) 
dados_GLP_SUDESTE_MIN_DISTR_TS_EST<-estacionar(dados_GLP_SUDESTE_MIN_DISTR) 
dados_GLP_SUDESTE_MAX_DISTR_TS_EST<-estacionar(dados_GLP_SUDESTE_MAX_DISTR) 
dados_GLP_SUDESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GLP_SUDESTE_MARGEM_REVENDA) 
dados_GNV_SUDESTE_MED_REVENDA_TS_EST<-estacionar(dados_GNV_SUDESTE_MED_REVENDA) 
dados_GNV_SUDESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GNV_SUDESTE_MIN_REVENDA) 
dados_GNV_SUDESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GNV_SUDESTE_MAX_REVENDA) 
dados_GNV_SUDESTE_MED_DISTR_TS_EST <-estacionar(dados_GNV_SUDESTE_MED_DISTR) 
dados_GNV_SUDESTE_MIN_DISTR_TS_EST <-estacionar(dados_GNV_SUDESTE_MIN_DISTR) 
dados_GNV_SUDESTE_MAX_DISTR_TS_EST <-estacionar(dados_GNV_SUDESTE_MAX_DISTR) 
dados_GNV_SUDESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_GNV_SUDESTE_MARGEM_REVENDA) 
dados_OLEO_DIESEL_SUDESTE_MED_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MED_REVENDA) 
dados_OLEO_DIESEL_SUDESTE_MIN_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MIN_REVENDA) 
dados_OLEO_DIESEL_SUDESTE_MAX_REVENDA_TS_EST <- estacionar(dados_OLEO_DIESEL_SUDESTE_MAX_REVENDA) 
dados_OLEO_DIESEL_SUDESTE_MED_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MED_DISTR) 
ados_OLEO_DIESEL_SUDESTE_MIN_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MIN_DISTR) 
dados_OLEO_DIESEL_SUDESTE_MAX_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MAX_DISTR) 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA)


#atividades referentes à metodologia de box-jenkins: Identificação, Estimação e previsão
#obtendo as funçoes de auto-correlação e auto-correlação parcial -válido para series estacionárias 
par(mfrow=c(1,2))
acf(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS_EST) #varias correlaçoes diferentes de zero
pacf(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS_EST) #AR incompleto (1) <> 0 - para AR verifica as ordens do pacf
##PAra MA já utiliza as ordens do ACF então para este caso seria um MA de ordem(1)

#criando modelo MA neste caso MA (1) para testes apenas (deta0+deta1*erro de modelo t-1)
modelom <- arima(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_TS_EST,order=c(0,0,1),fixed = c(NA,NA),method = c("ML"))
modelom
#divide valor do ma e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes teta0 e teta1.
coeficientesm <-modelom$coef
coeficientesm #o deta0 da equação é o intercept

acf(residuals(modelom)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado)
pacf(residuals(modelom)) #todos igual a zero, modelo adequado


# efetuando projeção 6 meses 95% de confiança
frct <- forecast(modelom,6) #como esse modelo é um MA(1), só projetou 1 mes utilizando o ma1. 
frct #Portanto se atendar a este fato, não faz sentido projetar periodos não "atingidos" pelo modelo
residuals(modelom) #erro: observado - estimado)
frct[["mean"]]


#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
a <- lengths(dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA)["MES"] - 6  #seleciondo a partir de Dezembro 2018
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[a,]$MES
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[a,]$ANO
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[a,3]


dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao = dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA


dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+1,3] = (frct[["mean"]] [1] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[a,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[a,3]
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+2,3] = (frct[["mean"]] [2] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+1,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+1,3] 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+3,3] = (frct[["mean"]] [3] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+2,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+2,3] 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+4,3] = (frct[["mean"]] [4] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+3,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+3,3] 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+5,3] = (frct[["mean"]] [5] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+4,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+4,3] 
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+6,3] = (frct[["mean"]] [6] * dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+5,3]) + dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[a+5,3]

##Calculando o MAPE##
abs((dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA - dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao)/dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA * 100)
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA_Projecao[(a+1):(a+6),]
dados_OLEO_DIESEL_SUDESTE_MARGEM_REVENDA[(a+1):(a+6),]




##Região CENTRO OESTE
#Gasolina
dados_GASOLINA_CENTRO_OESTE_TEMP <- subset(dados_GASOLINA,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                              "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GASOLINA_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                     data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                     FUN = mean)

dados_GASOLINA_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                     data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                     FUN = mean)
dados_GASOLINA_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                     data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                     FUN = mean)
dados_GASOLINA_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_GASOLINA_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_GASOLINA_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                        data=dados_GASOLINA_CENTRO_OESTE_TEMP,
                                                        FUN = mean)

# ETANOL HIDRATADO

dados_ETANOL_CENTRO_OESTE_TEMP <- subset(dados_ETANOL,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                          "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_ETANOL_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                   data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                   FUN = mean)

dados_ETANOL_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                   data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_ETANOL_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                   data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_ETANOL_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                 FUN = mean)
dados_ETANOL_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                 FUN = mean)
dados_ETANOL_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                 data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                 FUN = mean)
dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                      data=dados_ETANOL_CENTRO_OESTE_TEMP,
                                                      FUN = mean)
#GLP
dados_GLP_CENTRO_OESTE_TEMP <- subset(dados_GLP,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                    "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GLP_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                data=dados_GLP_CENTRO_OESTE_TEMP,
                                                FUN = mean)

dados_GLP_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                data=dados_GLP_CENTRO_OESTE_TEMP,
                                                FUN = mean)
dados_GLP_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                data=dados_GLP_CENTRO_OESTE_TEMP,
                                                FUN = mean)
dados_GLP_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GLP_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GLP_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GLP_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GLP_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GLP_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GLP_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                   data=dados_GLP_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
#GNV

dados_GNV_CENTRO_OESTE_TEMP <- subset(dados_GNV,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                    "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_GNV_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                data=dados_GNV_CENTRO_OESTE_TEMP,
                                                FUN = mean)

dados_GNV_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                data=dados_GNV_CENTRO_OESTE_TEMP,
                                                FUN = mean)
dados_GNV_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                data=dados_GNV_CENTRO_OESTE_TEMP,
                                                FUN = mean)
dados_GNV_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GNV_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GNV_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GNV_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GNV_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                              data=dados_GNV_CENTRO_OESTE_TEMP,
                                              FUN = mean)
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                   data=dados_GNV_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
#óLEO DIESEL

dados_OLEO_DIESEL_CENTRO_OESTE_TEMP <- subset(dados_OLEO_DIESEL,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                                    "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                        data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                        FUN = mean)

dados_OLEO_DIESEL_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                        data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                        FUN = mean)
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                        data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                        FUN = mean)
dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                      data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                      FUN = mean)
dados_OLEO_DIESEL_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                      data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                      FUN = mean)
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                      data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                      FUN = mean)
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                           data=dados_OLEO_DIESEL_CENTRO_OESTE_TEMP,
                                                           FUN = mean)
#óLEO DIESEL s10

dados_OLEO_s10_CENTRO_OESTE_TEMP <- subset(dados_OLEO_DIESEL_S10,REGIAO == "CENTRO OESTE",select = c("DATA.INICIAL","DATA.FINAL","REGIAO","NUMERO.DE.POSTOS.PESQUISADOS","PRECO.MEDIO.REVENDA","DESVIO.PADRAO.REVENDA","PRECO.MINIMO.REVENDA","PRECO.MAXIMO.REVENDA","COEF.DE.VARIACAO.REVENDA","PRECO.MEDIO.DISTRIBUICAO",
                                                                                                     "DESVIO.PADRAO.DISTRIBUICAO","PRECO.MINIMO.DISTRIBUICAO","PRECO.MAXIMO.DISTRIBUICAO","COEF.DE.VARIACAO.DISTRIBUICAO","MARGEM.MEDIA.REVENDA", "MES","ANO"))
dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA <- aggregate(PRECO.MEDIO.REVENDA ~ MES + ANO,
                                                     data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                     FUN = mean)

dados_OLEO_s10_CENTRO_OESTE_MIN_REVENDA <- aggregate(PRECO.MINIMO.REVENDA ~ MES + ANO,
                                                     data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                     FUN = mean)
dados_OLEO_s10_CENTRO_OESTE_MAX_REVENDA <- aggregate(PRECO.MAXIMO.REVENDA ~ MES + ANO,
                                                     data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                     FUN = mean)
dados_OLEO_s10_CENTRO_OESTE_MED_DISTR <- aggregate(PRECO.MEDIO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_OLEO_s10_CENTRO_OESTE_MIN_DISTR <- aggregate(PRECO.MINIMO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_OLEO_s10_CENTRO_OESTE_MAX_DISTR <- aggregate(PRECO.MAXIMO.DISTRIBUICAO ~ MES + ANO,
                                                   data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                   FUN = mean)
dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA <- aggregate(MARGEM.MEDIA.REVENDA ~ MES + ANO,
                                                        data=dados_OLEO_s10_CENTRO_OESTE_TEMP,
                                                        FUN = mean)

# Realizando análise descritiva das tabelas criadas

analise_descritiva(dados_GASOLINA_CENTRO_OESTE_TEMP)
analise_descritiva(dados_ETANOL_CENTRO_OESTE_TEMP)
analise_descritiva(dados_GLP_CENTRO_OESTE_TEMP)
analise_descritiva(dados_GNV_CENTRO_OESTE_TEMP)
analise_descritiva(dados_OLEO_DIESEL_CENTRO_OESTE_TEMP)
analise_descritiva(dados_OLEO_s10_CENTRO_OESTE_TEMP)

# transformar para série temporal

dados_GASOLINA_CENTRO_OESTE_MED_REVENDA_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GASOLINA_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_CENTRO_OESTE_MAX_DISTR_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MED_REVENDA_TS <- ts(dados_ETANOL_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_ETANOL_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_ETANOL_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_ETANOL_CENTRO_OESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_ETANOL_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_ETANOL_CENTRO_OESTE_MAX_DISTR_TS <- ts(dados_ETANOL_CENTRO_OESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_CENTRO_OESTE_MED_REVENDA_TS  <- ts(dados_GLP_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_GLP_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_GLP_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_GLP_CENTRO_OESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_GLP_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_GLP_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_CENTRO_OESTE_MAX_DISTR_TS <- ts(dados_GLP_CENTRO_OESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_GLP_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_GLP_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_CENTRO_OESTE_MED_REVENDA_TS <- ts(dados_GNV_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_GNV_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_GNV_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_GNV_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_GNV_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12) 




#5 de 2011  copiar do id 84
#4 de 2016 copiar do id 142
tail(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA)

dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[181,]<- dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[84,]
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[181,1] <-5
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[182,]<- dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[142,]
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[182,1]<- 4

dados_GNV_CENTRO_OESTE_MARGEM_REVENDA <- dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[order(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA$ANO, dados_GNV_CENTRO_OESTE_MARGEM_REVENDA$MES,decreasing=c(FALSE, FALSE)), ] 
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 



tail(dados_GNV_CENTRO_OESTE_MED_DISTR)

dados_GNV_CENTRO_OESTE_MED_DISTR[181,]<- dados_GNV_CENTRO_OESTE_MED_DISTR[84,]
dados_GNV_CENTRO_OESTE_MED_DISTR[181,1] <-5
dados_GNV_CENTRO_OESTE_MED_DISTR[182,]<- dados_GNV_CENTRO_OESTE_MED_DISTR[142,]
dados_GNV_CENTRO_OESTE_MED_DISTR[182,1]<- 4

dados_GNV_CENTRO_OESTE_MED_DISTR <- dados_GNV_CENTRO_OESTE_MED_DISTR[order(dados_GNV_CENTRO_OESTE_MED_DISTR$ANO, dados_GNV_CENTRO_OESTE_MED_DISTR$MES,decreasing=c(FALSE, FALSE)), ] 
dados_GNV_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_GNV_CENTRO_OESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 


dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2004, 5), frequency = 12)
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_DISTR_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MAX_DISTR[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2004, 5), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA[, 3], start = c(2012, 12), frequency = 12)
dados_OLEO_s10_CENTRO_OESTE_MIN_REVENDA_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MIN_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MAX_REVENDA_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MAX_REVENDA[, 3], start = c(2012, 12), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MED_DISTR_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MED_DISTR[, 3], start = c(2013, 1), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MIN_DISTR_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MIN_DISTR[, 3], start = c(2013, 1), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MAX_DISTR_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MAX_DISTR[, 3], start = c(2013, 1), frequency = 12) 
dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA_TS <- ts(dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA[, 3], start = c(2013, 1), frequency = 12)

#realizando análise exploratória das séries temporais

par(mfrow=c(1,3))
analise(dados_GASOLINA_CENTRO_OESTE_MED_REVENDA_TS)
analise(dados_GASOLINA_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA_TS) 
analise(dados_ETANOL_CENTRO_OESTE_MED_REVENDA_TS) 
analise(dados_ETANOL_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA_TS) 
analise(dados_GLP_CENTRO_OESTE_MED_REVENDA_TS) 
analise(dados_GLP_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_GLP_CENTRO_OESTE_MARGEM_REVENDA_TS) 
analise(dados_GNV_CENTRO_OESTE_MED_REVENDA_TS) 
analise(dados_GNV_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA_TS) 
analise(dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS) 
analise(dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA_TS) #para estes o tamanho da serie é pequena, não sendo confiável teste de estacionaridade
analise(dados_OLEO_s10_CENTRO_OESTE_MED_DISTR_TS) 
analise(dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA_TS)

###Iniciamente efetuaremos projeção das séries considerando os modelos de regressão linear e ARIMA (esta através
### da função auto.arima, que não requer série temporal previamente estacionária)
### Além da projeção auto.arima também estamos utilizando tslm (time series linear model), para assim termos a possibilidade de comparação.
projecao_auto_arima(dados_GASOLINA_CENTRO_OESTE_MED_REVENDA_TS)
projecao_linear_model(dados_GASOLINA_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GASOLINA_CENTRO_OESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GASOLINA_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_ETANOL_CENTRO_OESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_ETANOL_CENTRO_OESTE_MED_DISTR_TS) 
projecao_linear_model(dados_ETANOL_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GLP_CENTRO_OESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GLP_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GLP_CENTRO_OESTE_MED_DISTR_TS) 
projecao_linear_model(dados_GLP_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GLP_CENTRO_OESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_GLP_CENTRO_OESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_GNV_CENTRO_OESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_GNV_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_GNV_CENTRO_OESTE_MED_DISTR_TS)
projecao_linear_model(dados_GNV_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA_TS)
projecao_linear_model(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS)

projecao_auto_arima(dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_CENTRO_OESTE_MED_REVENDA_TS)
projecao_auto_arima(dados_OLEO_s10_CENTRO_OESTE_MED_DISTR_TS) 
projecao_linear_model(dados_OLEO_s10_CENTRO_OESTE_MED_DISTR_TS)
projecao_auto_arima(dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA_TS) 
projecao_linear_model(dados_OLEO_s10_CENTRO_OESTE_MARGEM_REVENDA_TS)

###Após analisar as séries identificamos que as mesmas não são estacionárias, para utilizarmos ARIMA que não pela função auto.arima precisaremos estacionar as mesmas.

dados_GASOLINA_CENTRO_OESTE_MED_REVENDA_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MED_REVENDA)
dados_GASOLINA_CENTRO_OESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MIN_REVENDA)
dados_GASOLINA_CENTRO_OESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MAX_REVENDA) 
dados_GASOLINA_CENTRO_OESTE_MED_DISTR_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MED_DISTR) 
dados_GASOLINA_CENTRO_OESTE_MIN_DISTR_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MIN_DISTR) 
dados_GASOLINA_CENTRO_OESTE_MAX_DISTR_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MAX_DISTR) 
dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GASOLINA_CENTRO_OESTE_MARGEM_REVENDA) 
dados_ETANOL_CENTRO_OESTE_MED_REVENDA_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MED_REVENDA) 
dados_ETANOL_CENTRO_OESTE_MIN_REVENDA_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MIN_REVENDA) 
dados_ETANOL_CENTRO_OESTE_MAX_REVENDA_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MAX_REVENDA) 
dados_ETANOL_CENTRO_OESTE_MED_DISTR_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MED_DISTR) 
dados_ETANOL_CENTRO_OESTE_MIN_DISTR_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MIN_DISTR) 
dados_ETANOL_CENTRO_OESTE_MAX_DISTR_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MAX_DISTR) 
dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_ETANOL_CENTRO_OESTE_MARGEM_REVENDA) 
dados_GLP_CENTRO_OESTE_MED_REVENDA_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MED_REVENDA) 
dados_GLP_CENTRO_OESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MIN_REVENDA) 
dados_GLP_CENTRO_OESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MAX_REVENDA) 
dados_GLP_CENTRO_OESTE_MED_DISTR_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MED_DISTR) 
dados_GLP_CENTRO_OESTE_MIN_DISTR_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MIN_DISTR) 
dados_GLP_CENTRO_OESTE_MAX_DISTR_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MAX_DISTR) 
dados_GLP_CENTRO_OESTE_MARGEM_REVENDA_TS_EST<-estacionar(dados_GLP_CENTRO_OESTE_MARGEM_REVENDA) 
dados_GNV_CENTRO_OESTE_MED_REVENDA_TS_EST<-estacionar(dados_GNV_CENTRO_OESTE_MED_REVENDA) 
dados_GNV_CENTRO_OESTE_MIN_REVENDA_TS_EST<-estacionar(dados_GNV_CENTRO_OESTE_MIN_REVENDA) 
dados_GNV_CENTRO_OESTE_MAX_REVENDA_TS_EST<-estacionar(dados_GNV_CENTRO_OESTE_MAX_REVENDA) 
dados_GNV_CENTRO_OESTE_MED_DISTR_TS_EST <-estacionar(dados_GNV_CENTRO_OESTE_MED_DISTR) 
dados_GNV_CENTRO_OESTE_MIN_DISTR_TS_EST <-estacionar(dados_GNV_CENTRO_OESTE_MIN_DISTR) 
dados_GNV_CENTRO_OESTE_MAX_DISTR_TS_EST <-estacionar(dados_GNV_CENTRO_OESTE_MAX_DISTR) 
dados_GNV_CENTRO_OESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_GNV_CENTRO_OESTE_MARGEM_REVENDA) 
dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MED_REVENDA) 
dados_OLEO_DIESEL_CENTRO_OESTE_MIN_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MIN_REVENDA) 
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_REVENDA_TS_EST <- estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MAX_REVENDA) 
dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MED_DISTR) 
ados_OLEO_DIESEL_CENTRO_OESTE_MIN_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MIN_DISTR) 
dados_OLEO_DIESEL_CENTRO_OESTE_MAX_DISTR_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MAX_DISTR) 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST <-estacionar(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA) 

#atividades referentes à metodologia de box-jenkins: Identificação, Estimação e previsão
#obtendo as funçoes de auto-correlação e auto-correlação parcial -válido para series estacionárias
par(mfrow=c(1,2))
acf(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST) #varias correlaçoes diferentes de zero
pacf(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST) #AR incompleto (1,6) <> 0 - para AR verifica as ordens do pacf
##PAra MA já utiliza as ordens do ACF então para este caso seria um MA de ordem(1)

#criando modelo MA neste caso MA (1) para testes apenas (deta0+deta1*erro de modelo t-1)
modelom <- arima(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST,order=c(0,0,1),fixed = c(NA,NA),method = c("ML"))
modelom
#divide valor do ma e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes teta0 e teta1.
coeficientesm <-modelom$coef
coeficientesm #o deta0 da equação é o intercept

acf(residuals(modelom)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado)
pacf(residuals(modelom)) #todos igual a zero, seria modelo adequado..que não foi o caso..apresentou resíduo em 6

## o modelom  que é um modelo MA(1), na análise dos residuos, apresentou residuo no partial ACF na posição 6
## o que sugere criação do modelo ARMA(6,1), como segue:

modeloarma <- arima(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_TS_EST,order=c(6,0,1),fixed = c(0,0,0,NA,0,NA,NA,NA),method = c("ML"))
#divide valor dos coeficientes  e intercept pelos respectivos s.e, o ponto de confiança 90% é (+-) 1.64, portanto rejeitamos Ho e mantemos os coeficientes 
modeloarma
coeficientesarma <-modeloarma$coef
coeficientesarma
acf(residuals(modeloarma)) #todos igual a zero, e só o primeiro igual a 1 (modelo adequado) ..
pacf(residuals(modeloarma)) #todos igual a zero, modelo adequado

# efetuando projeção 6 meses 95% de confiança
frct <- forecast(modeloarma,6) #como esse modelo é um ARMA(6,1), só projetou 1 mes utilizando o ma1 , os demais utilizou apenas os ars. 
frct #Portanto se atendar a este fato, não faz sentido projetar periodos não "atingidos" pelo modelo
residuals(modeloarma) #erro: observado - estimado)
frct[["mean"]]

#retornando a série a sua forma normal, antes de estacionar para avaliarmos o modelo
a <- lengths(dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA)["MES"] - 6  #seleciondo a partir de Dezembro 2018
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[a,]$MES
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[a,]$ANO
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[a,3]


dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao = dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA


dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+1,3] = (frct[["mean"]] [1] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[a,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[a,3]
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+2,3] = (frct[["mean"]] [2] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+1,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+1,3] 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+3,3] = (frct[["mean"]] [3] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+2,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+2,3] 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+4,3] = (frct[["mean"]] [4] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+3,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+3,3] 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+5,3] = (frct[["mean"]] [5] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+4,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+4,3] 
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+6,3] = (frct[["mean"]] [6] * dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+5,3]) + dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[a+5,3]

##Calculando o MAPE##
abs((dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA - dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao)/dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA * 100)
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA_Projecao[(a+1):(a+6),]
dados_OLEO_DIESEL_CENTRO_OESTE_MARGEM_REVENDA[(a+1):(a+6),]






