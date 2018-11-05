                    #Aula 18 - Modelos VAR(p)

pacotes <- c("quantmod","Quandl","forecast","dplyr","magritt",
             "highcharter","dygraphs","htmltools","ggplot2","MTS",
             "vars","urca","seasonal")

install.packages(pacotes)

suppressMessages(require(quantmod))
suppressMessages(require(Quandl))
suppressMessages(require(forecast))
suppressMessages(require(dplyr))
suppressMessages(require(highcharter))
suppressMessages(require(dygraphs))
suppressMessages(require(htmltools))
suppressMessages(require(ggplot2))
suppressMessages(require(MTS))
suppressMessages(require(vars))
suppressMessages(require(urca))
suppressMessages(require(seasonal))
suppressMessages(require(pwt8))

data("pwt8.0")                                 #Carrega os dados elencados "pwt8.0" dispoiníveis no pacote
View(pwt8.0)                                   #Visualiza os dados na tabela pwt8.0


br <- subset(pwt8.0, country=="Brazil", 
             select = c("rgdpna","emp","xr"))  #Cria a tabela "br" com dados das linhas que assumem o valor "country" (país) igual a "Brazil", selecionando as colunas cujas variáveis são "rgdpna" (PIB), "avh" (TRABALHO)  e "xr" (CÂMBIO)

colnames(br) <-  c("PIB","Emprego","Câmbio")   #Renomeia as colunas para PIB, Trabalho e Câmbio
BR <- br[45:62,1:3]


#Separando as variáveis
PIB <- diff(br$PIB)                    #Cria o vetor para variável PIB 
PIb <- ts(PIB, start = 1950, frequency = 1)
EMPREGO <- diff(br$Emprego)            #Cria o vetor para variável EMPREGO
Emprego <- ts(EMPREGO, start = 1950, frequency = 1)
CAMBIO <- diff(br$Câmbio)              #Cria o vetor para variável CAMBIO
Cambio <- ts(CAMBIO, start = 1950, frequency = 1)
Brasil <- cbind(PIB,EMPREGO,CAMBIO)
Anos <- seq(from=1950, to=2011, by=1)         #Cria um vetor para o tempo em anos de 1994 até 2011
BRA <- ts(Brasil, start = 1950, frequency = 1)
plot(BRA,main="Variação do PIB, Emprego e Cambio no Brasil", col="Blue")

#Criando Gráficos Dinâmicos

h1 = hchart(PIb, name = "PIB", color = "#B71C1C")
  hc_title(h1,text = "Variação do PIB do Brasil", margin = 10, style = list(fontSize= "14px")) #Gráfico do PIB

h2 = hchart(Emprego, name = "EMPREGO", color = "black")
  hc_title(h2,text = "Variação do Emprego", margin = 10, style = list(fontSize= "14px")) #Gráfico do Emprego

h3 = hchart(Cambio, name = "Câmbio", color = "green")
  hc_title(h3,text = "Variação do Câmbio (R$/U$$)", margin = 10, style = list(fontSize= "14px")) #Gráfico do Câmbio

lst = list(h1,h2,h3)
hw_grid(lst, ncol = 2, rowheight = 400)  %>% browsable()  #Todos Três Gráficos Juntos

#Lag òtimo

CriterioInformacao = vars::VARselect(y = Brasil, lag.max = 6, type = "const")
print(CriterioInformacao$criteria)

#Estimando um Var

modelobra = vars::VAR(y = Brasil, p = 1, type = "const")
summary(modelobra)
