# assaig de bernoulli

x<-c(0,1)
f<-c(0.68,0.32)

plot(x,f,type='h',ylim=c(0,1),col='red')
points(x,f,pch=16,col='red')

n<-43
mostra<-sample(x,n,f,replace=TRUE) #simular l'expermiment

table(mostra)
table(mostra)/n #veure la prob
pie(table(mostra)) #grafic rodona
mean(mostra) #mitja
sum(mostra)

bar<-barplot(table(mostra)/n,ylim=c(0,1)) #grafic de barres
lines(bar,f,type='h',col='red')
points(bar,f,pch=16,col='red')

Y<- function(i){sum(sample(x,n,f,replace=TRUE))}

set.seed(123)
m<-400000
enquesta<-(sapply(1:m, Y)) #repetir Y fins a m vegades

fr<-table(enquesta)/m     
fr["13"]     
xx<-names(fr)

dbinom(13,43,0.32)
br<-barplot(table(enquesta)/m)
lines(br,dbinom(2:29,43,0.32),type='h',col='red')
points(br,dbinom(2:29,43,0.32),pch=16,col='red')


dbinom(17,44,0.32) #prob de 17
plot(0:43,dbinom(0:43,44,0.32),type='h',col='red')
pbinom(16,44,0.32) #prob de menys de 17

####

n<-c(0,24)
x<-c(0,1)
f<-c(0,32,0.68) # al reves perque hem definit 0 com a 0,32

Xstar<-function(i){sum(sample(x,n,f,replace=TRUE))}

set.seed(123)
m<-400000
enquesta2<-sapply(1:m,Xstar) #repetir Y fins a m vegades
