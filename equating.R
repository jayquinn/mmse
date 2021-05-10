library(equate)
library("SNSequate")
tail(scoreframe)
eqeq<-equate(scoreframe$CTT,scoreframe$CFA,type="linear",method="tuck")

data("Math20EG", package = "SNSequate")
print(Math20EG)
x.scores<-rep(0:20,Math20EG[,1])
y.scores<-rep(0:20,Math20EG[,2])
eg.x<-freqtab(x.scores,0:20)
eg.y<-freqtab(y.scores,0:20)

data("Math20EG", package = "SNSequate")