library(readxl)
dados2<- read_excel("C:/Users/Eliza/Downloads/MapaViolencia2015_mulheres.xlsx")
View(dados2)

# Carregando dados

# Gerando gráfico 

plot1 <- ggplot(dados2, aes(x=Feminicidio, y= IDHM2010, color= Municipio)) +
  geom_jitter() 
plot2 <- plot1 +
  labs( x = "Taxa de Feminicidio", y = "IDH") + 
  theme(legend.position = "none")
plot2

# Rodando regressao
# Gerando grafico

regres <- lm (Taxa_Feminicidio ~ Desemprego_2011 + IDH_2010, data = dados2)
summary(regres)

plot(regres)

# Testando Pressupostos
# Analisando se ha outliers na amostra 3 tipos

# ativando pacote para testes de outliers
library(car)
# Teste P- valor para observacoes mais extremas
outlierTest(regres)

# Elaborando grafico do teste de outliers
# Qqplot para entender os entender os resíduos 

qqPlot(regres, main="QQ Plot")

# LeveragePlots - pontos mais extremos

leveragePlots(regres)


# Testando Pressuposto não-normalidade, normalidade dos residuos

library(MASS)
# Distribuicao de residuo grafico
distresid <- studres(regres)

# elaborando grafico de histograma para residuos

hist(distresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(distresid),max(distresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)


# Testando Pressuposto da Homecedasticidade

#Avalindo homecedasticidade - teste de variancia
ncvTest(regres)

#Gerando grafico do teste de homocedasticidade

spreadLevelPlot(regres)

# Testando Pressuposto de Multicolinaridade
# Avaliando Colinearidade

vif(regres)
sqrt(vif(regres)) > 2
sqrt(vif(regres))


# Testando Pressuposto Não-linearidade

# Avaliando não-linearidade

crPlots(regres)

ceresPlots(regres)


# Testando Pressuposto da não independencia dos erros 

# Avaliando Teste para erros auto-correlacionados

durbinWatsonTest(regres)


