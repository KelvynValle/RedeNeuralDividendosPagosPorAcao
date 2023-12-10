setwd("C:/Users/kelvy/Desktop/laboratorio ai/lab 1/5/")
# por exemplo: "c:/dados/"
meusDados <-read.csv("dividendinfo.csv")
attach(meusDados)
scaleddata <- scale(meusDados)
normalizar <- function (x) {
  return ((x-min (x))/(max (x)-min (x)))
}
maxmindf <-as.data.frame(lapply (meusDados, normalizar))
#Training e dados de teste
trainset <- maxmindf[1:160,]
testset <- maxmindf[161:200,]
# Rede Neural
install.packages("neuralnet")
library(neuralnet)
rna <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap +
                   current_ratio, data=trainset, hidden=c(2,1), linear.output=FALSE,
                 threshold=0.01)
plot(rna)
#Test the resulting output
temp_teste <- subset(testset, select = c("fcfps","earnings_growth",
                                         "de", "mcap", "current_ratio"))
head(temp_teste)
rna.resultados <- compute(rna, temp_teste)
resultados <- data.frame(actual = testset$dividend, prediction =
                           rna.resultados$net.result)
resultados

resultadofinal<-sapply(resultados,round,digits=0)
resultadofinaldf=data.frame(resultadofinal)
attach(resultadofinaldf)
table(actual,prediction)
