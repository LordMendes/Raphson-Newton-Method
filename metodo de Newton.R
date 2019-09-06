#FUNÇÃO A SER CALCULADA
library(mosaicCalc)
library(ggplot2)

pi <- 3.141592653589793238462643383279502884197169399375105820974944592307816406286208998628034825342117067982148086513282306647093844609550582231725359408128481
it <- 0
#EQUAÇÃO A SER PLOTADA
eq = function(x){
  
  sin(x)-0.75
  
}

#EQUAÇÃO A SER APROXIMADA
f <- makeFun((              sin(x)-0.75            ) ~ x)
fd<- D(f(x) ~ x)
e <- 10^(-10)
#INTERVALO DA RAÍZ
a <- 0
b <- 1
#FUNÇÃO PARA PLOT DO GRÁFICO NO INTERVALO
ggplot(data.frame(x=c(a, b)), aes(x=x)) + stat_function(fun=eq, geom="line") + xlab("x") + ylab("y")

ant <- b-a
prox <- ant - f(ant)/fd(ant)

while(abs(prox - ant) > e){
  
  ant <- prox
  prox <- ant - f(ant)/fd(ant)
  it<-it+1
  
}

#RESPOSTA :
prox
it
