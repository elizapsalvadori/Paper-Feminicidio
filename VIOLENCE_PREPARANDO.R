# Script paper:
# t�tulo: "Feminic�dio: o pre�o pago por ser mulher"
# subt�tulo: "Estudo dos determinantes do Feminic�dio no Brasil"
# autora: "Eliza Salvador"


# Instalando pacotes


install.packages("haven")
install.packages("pander")

# Ativando pacotes

library(lmtest)
library(car)
library(tinytex)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(pander)
library(coefplot)
library(MASS)
library(haven)

# Abrir a base de dados

library(readxl) # carregar pacote para abrir a base

MapaViolencia2012_mulheres <- read_excel("C:/Users/Eliza/Documents/eliza_dados/MapaViolencia2012_mulheres.xlsx",
                                         col_types = c("text", "numeric", "numeric", 
                                                       "numeric"))

# Transformacao de variavel em numerica e atribuindo nomes

violence <- MapaViolencia2012_mulheres
desemprego <- violence$Desemprego_2011
feminicidio <- violence$Taxa_Feminicidio
IDH <- violence$IDH_2010

# Graficos da figura 1

# Carregar pacotes para os graficos

library(ggplot2)
library(tidyverse)
library(ggpubr)

# Formar data frame 

data1 <- data.frame( Estado=MapaViolencia2012_mulheres$Estado,
                    Taxa_Feminicidio=MapaViolencia2012_mulheres$Taxa_Feminicidio)

# Construir grafico sobre Feminicidio

Grafico1 <- data1 %>%
  mutate(Estado = fct_reorder(Estado, feminicidio)) %>%
  ggplot(aes(x=Estado, y=feminicidio)) +
  geom_bar(stat="identity", color = "black", fill = rgb(0.1,0.4,0.5,0.7)) +
  ggtitle("Taxa de Feminic�dios por Estado - 2012") +
  xlab("Estados") + 
  ylab("Taxa de Feminic�dio") +
  coord_flip()

# Formar data frame 

data2 <- data.frame( Estado=MapaViolencia2012_mulheres$Estado,
                     
                     IDH_2010=MapaViolencia2012_mulheres$IDH_2010)

# Construir grafico sobre IDH

Grafico2 <- data2 %>%
  mutate(Estado = fct_reorder(Estado, IDH)) %>%
  ggplot(aes(x=Estado, y=IDH)) +
  geom_bar(stat="identity", color = "black", fill = rgb(0.1,0.4,0.5,0.7)) +
  ggtitle("�ndice de Desenvolvimento Humano por Estado - 2010") +
  xlab("Estados") + 
  ylab("IDH - 2010") +
  coord_flip()

# Formar Data Frame

data3 <- data.frame( Estado=MapaViolencia2012_mulheres$Estado,
                    Desemprego_2011=MapaViolencia2012_mulheres$Desemprego_2011)
Grafico3<- data3 %>%
  mutate(Estado = fct_reorder(Estado, desemprego)) %>%
  ggplot(aes(x=Estado, y=desemprego)) +
  geom_bar(stat="identity", color = "black", fill = rgb(0.1,0.4,0.5,0.7)) +
  ggtitle("Taxa de Desemprego por Estado - 2011") +
  xlab("Estados") + 
  ylab("Taxa de Desemprego") +
  coord_flip()

# Inserindo comando para unir graficos

ggarrange(Grafico1,Grafico2,Grafico3)

figura1 <- ggarrange(Grafico1,Grafico2,Grafico3, common.legend = TRUE, 
                     legend = "bottom")
annotate_figure(figura1, top = text_grob("Figura 1", color = "black", 
                                         size = 14), 
                bottom = text_grob("Fonte: Elabora��o Pr�pria ", color = "black", hjust = 1, x = 1, size = 10))


# Analise Exploratoria das Variaveis 

# Carregar pacote para tabela descritiva

library(pander)

pander(summary(violence, headr=T), caption = "Estat�sticas Descritivas")


# Correlacao Linear entre as variaveis do modelo

# Ativar pacote

library(pander)


# Correlacao entre feminicidio vs desemprego

cor(feminicidio,desemprego)
pander(cor.test(feminicidio,desemprego), caption = "Feminic�dio e Desemprego")

# Ativar pacote

library(pander)

# Grafico dispersao das variaveis 

# Ativar pacote

library(ggplot2)

# Grafico 1 - Dispersao feminicidio vs desemprego

desemprego <- violence$Desemprego_2011
plot1 <- ggplot(violence, aes(x=feminicidio, y= desemprego, color= Estado)) +
  geom_jitter()

plot2 <- plot1 +
  labs( x = "Taxa de Feminicidio - 2012", y = "Desemprego - 2010")
plot2 + theme(legend.position = "none")

# Correlacao entre feminicido vs IDH

# Ativar pacote

library(pander)

# correlacao

cor(feminicidio,IDH)
pander(cor.test(feminicidio,IDH))

# Grafico 2 - Dispersao entre Feminicidio e IDH

# Ativar pacote

library(ggplot2)

# dispersao

plot1 <- ggplot(violence, aes(x=feminicidio, y= IDH, color= Estado)) +
  geom_jitter()

plot2 <- plot1 +
  labs( x = "Taxa de Feminicidio - 2012", y = "IDH - 2010")
plot2 + theme(legend.position = "none")


# Modelo de regressao 

regressao_1 <- lm (feminicidio ~ desemprego + IDH, data = violence)
summary(regressao_1)
par(mfrow=c(2,2))
plot(regressao_1)

# Gerando tabela de regressao 

# Ativando pacote
library(pander)

tabelaregressao <- data.frame(Observations = c(27),
                              Residual_Std._Error = c(1.561),
                              R2 = c(0.041),
                              Adjusted_R2 = c(-0.038),
                              p_value = c(0.599))
pander(tabelaregressao, caption = "Resultados Regressao")

# Gerando grafico coefplot para analise da regressao

# Ativar pacote
library(coefplot)
summary(regressao_1)
coefplot(regressao_1, parm = -1)


# Testando os pressupostos da regressao

# Ativando pacote

library(car)

# Elaborando grafico do teste de outliers

# Qqplot para entender os residuos 
par(mfrow=c(1, 1))

qqPlot(regressao_1, main="QQ Plot")


# LeveragePlots - pontos mais extremos

# Ativar pacotes

leveragePlots(regressao_1)


# Testando Pressuposto da Homecedasticidade

# Ativando pacote

library(ggplot2)
library(pander)
library(lmtest)
library(car)

# Avaliando homecedasticidade - teste de variancia


# Gerando grafico do teste de homocedasticidade

spreadLevelPlot(regressao_1)

bptest(regressao_1, varformula = NULL, studentize = TRUE, data = violence())


# Testando Pressuposto nao-normalidade, normalidade dos residuos

# Ativando pacote

library(MASS)

# Distribuicao de residuo no grafico HISTOGRAMA

distresid <- studres(regressao_1)

# Elaborando grafico de histograma para residuos

hist(distresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(distresid),max(distresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Testando Pressuposto de Multicolinaridade

# Ativando pacote necessario

library(pander)

# # Rodando comando para visualiza��o do pressuposto

vif(regressao_1)
pander(sqrt(vif(regressao_1)) > 2)
pander(sqrt(vif(regressao_1)))

# Testando Pressuposto da nao independencia dos erros 

# Ativando pacotes

library(pander)
library(car)

# Avaliando Teste para erros auto-correlacionados

durbinWatsonTest(regressao_1) # Gerou tabela de coeficientes atraves do summary da regressao



# Gerando Data Frame

durbinTabela <- data.frame(lag = c (1),
                           Autocorrelation = c(1.71),
                           Statistic = c(1.654),
                           p_value = c(0.338),
                           Alternative_hypothesis = c("rho#0"))
pander(durbinTabela)

# Testando Pressuposto da Homecedasticidade

# Ativando pacote

library(ggplot2)
library(pander)

