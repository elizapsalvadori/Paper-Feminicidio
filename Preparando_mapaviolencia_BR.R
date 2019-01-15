
# Ativando Pacotes necessarios para a execucao do Script

library(readxl)
library(pander)
library(ggplot2)
library(car)
library(MASS)
library(tidyverse)


# Inserindo banco de dados

library(readxl)
MapaViolencia2012_mulheres <- read_excel("eliza_dados/MapaViolencia2012_mulheres.xlsx", 
                                         col_types = c("text", "numeric", "numeric", 
                                                       "numeric"))
View(MapaViolencia2012_mulheres)



# Gerando graficos de dispersao das variaveis

# Grafico 1 - Dispersao entre Feminicidio e IDH

violence <- MapaViolencia2012_mulheres
feminicidio <- violence$Taxa_Feminicidio
IDH <- violence$IDH_2010
plot1 <- ggplot(violence, aes(x=feminicidio, y= IDH, color= Estado)) +
  geom_jitter()
plot1

plot2 <- plot1 +
  labs( x = "Taxa de Feminicidio - 2012", y = "IDH - 2010")
plot2

# Grafico 2 - Dispersao entre Feminicidio e Desemprego

desemprego <- violence$Desemprego_2011
plot1 <- ggplot(violence, aes(x=feminicidio, y= desemprego, color= Estado)) +
  geom_jitter()
plot1

plot2 <- plot1 +
  labs( x = "Taxa de Feminicidio - 2012", y = "Desemprego - 2010")
plot2


# Rodando regressao

# Gerando grafico

# Gerando coeficientes de regressao sem o pander

regressao_1 <- lm (feminicidio ~ desemprego + IDH, data = violence)
summary(regressao_1)

plot(regressao_1)


# Gerando coeficientes de regressao com o pander 

pander (regressao_1 <- lm (feminicidio ~ desemprego + IDH, data = violence))
summary(regressao_1)

plot(regressao_1)



# Testando Pressupostos para a analise de regressao

# Analisando se ha outliers na amostra: 3 tipos de testes

# Ativando pacote para testes de outliers

library(car)

# Teste P- valor para observacoes mais extremas
outlierTest(regressao_1)


cooks.distance(regressao_1)


round(cooks.distance(regressao_1), digits = 2) # ajustando a quantidade de digitos


# Grafico de COOKS - mostra os valores e onde se encontram na dispersao cada caso

cooksd<-cooks.distance(regressao_1) # salvando os valores
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance") # plotando
abline(h = 4*mean(cooksd, na.rm=T), col="red") # adicionando a linha
text(x=1:length(cooksd)+1, y=cooksd, labels=feminicidio, col="red")


# Elaborando grafico do teste de outliers

# Qqplot para entender os entender os residuos 

qqPlot(regressao_1, main="QQ Plot")

# LeveragePlots - pontos mais extremos

leveragePlots(regressao_1)


# Testando Pressuposto nao-normalidade, normalidade dos residuos

library(MASS)
# Distribuicao de residuo grafico
distresid <- studres(regressao_1)


# elaborando grafico de histograma para residuos

hist(distresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(distresid),max(distresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# Testando Pressuposto da Homecedasticidade

#Avalindo homecedasticidade - teste de variancia
ncvTest(regressao_1)

#Gerando grafico do teste de homocedasticidade

spreadLevelPlot(regressao_1)

# Testando Pressuposto de Multicolinaridade
# Avaliando Colinearidade
library(pander)
vif(regressao_1)
pander(sqrt(vif(regressao_1)) > 2)
pander(sqrt(vif(regressao_1)))

# Testando Pressuposto Nao-linearidade

# Avaliando nao-linearidade

crPlots(regressao_1)

ceresPlots(regressao_1)


# Testando Pressuposto da nao independencia dos erros 

# Avaliando Teste para erros auto-correlacionados


durbinWatsonTest(regressao_1) # Gerou tabela de coeficientes atraves do summary da regressao



# Graficos para Analise Exploratoria dos Dados

