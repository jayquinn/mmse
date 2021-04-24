install.packages("dplyr")
install.packages("mirt")
install.packages("lavaan")
library(dplyr)
library(mirt)
library(lavaan)
dat<-read.csv("C:/git/mmse/mmse.csv",header=T,sep=",")
dat<-read.csv("C:/Users/john/Dropbox/r/mmse.csv",header=T,sep=",")
dat<-read.csv("C:/Users/mjay8/Dropbox/mmse.csv",header=T,sep=",")
head(dat)
#### 데이터 전처리 ####
attach(dat)

#402
a402<-ifelse(C402==1,1,
             ifelse(C402==-8,NA,
                    ifelse(C402==-9,0,
                           ifelse(C402==5,0,NA))))
head(a402)
table(C402,a402,useNA='always')
length(C402);length(a402)
#403
a403<-ifelse(C403==1,1,
             ifelse(C403==-8,NA,
                    ifelse(C403==-9,0,
                           ifelse(C403==5,0,NA))))
table(C403,a403,useNA='always')
table(C403)
table(a403)
length(C403);length(a403)

#404
a404<-ifelse(C404==1,1,
             ifelse(C404==-8,NA,
                    ifelse(C404==-9,0,
                           ifelse(C404==5,0,NA))))
table(C404,a404,useNA='always')
table(C404)
table(a404)
length(C404);length(a404)

#407
a407<-ifelse(C407==1,1,
             ifelse(C407==-8,NA,
                    ifelse(C407==-9,0,
                           ifelse(C407==5,0,NA))))
table(C407,a407,useNA='always')
table(C407)
table(a407)
length(C407);length(a407)
#408
a408<-ifelse(C408==1,1,
             ifelse(C408==-8,NA,
                    ifelse(C408==-9,0,
                           ifelse(C408==5,0,NA))))
table(C408,a408,useNA='always')
table(C408)
table(a408)
length(C408);length(a408)
#409
a409<-ifelse(C409==1,1,
             ifelse(C409==-8,NA,
                    ifelse(C409==-9,0,
                           ifelse(C409==5,0,NA))))
table(C409,a409,useNA='always')
table(C409)
table(a409)
length(C409);length(a409)
#410
a410<-ifelse(C410==1,1,
             ifelse(C410==-8,NA,
                    ifelse(C410==-9,0,
                           ifelse(C410==5,0,NA))))
table(C410,a410,useNA='always')
table(C410)
table(a410)
length(C410);length(a410)
#411
a411<-ifelse(C411==1,1,
             ifelse(C411==-8,NA,
                    ifelse(C411==-9,0,
                           ifelse(C411==5,0,NA))))
table(C411,a411,useNA='always')
table(C411)
table(a411)
length(C411);length(a411)

#413
a413<-ifelse(C413==1,1,
             ifelse(C413==-8,NA,
                    ifelse(C413==-9,0,
                           ifelse(C413==5,0,NA))))
table(C413,a413,useNA='always')
table(C413)
table(a413)
length(C413);length(a413)
#414
a414<-ifelse(C414==1,1,
             ifelse(C414==-8,NA,
                    ifelse(C414==-9,0,
                           ifelse(C414==5,0,NA))))
table(C414,a414,useNA='always')
table(C414)
table(a414)
length(C414);length(a414)
#415
a415<-ifelse(C415==1,1,
             ifelse(C415==-8,NA,
                    ifelse(C415==-9,0,
                           ifelse(C415==5,0,NA))))
table(C415,a415,useNA='always')
table(C415)
table(a415)
length(C415);length(a415)

#418
a418<-ifelse(C418==1,1,
             ifelse(C418==-8,NA,
                    ifelse(C418==-9,0,
                           ifelse(C418==5,0,NA))))
table(C418,a418,useNA='always')
table(C418)
table(a418)
length(C418);length(a418)
#419
a419<-ifelse(C419==1,1,
             ifelse(C419==-8,NA,
                    ifelse(C419==-9,0,
                           ifelse(C419==5,0,NA))))
table(C419,a419,useNA='always')
table(C419)
table(a419)
length(C419);length(a419)
detach(dat)
<<<<<<< HEAD
response.raw<-data.frame(a402,a403,a404,a407,a408,a409,a410,a411,a413,a414,a415,a418,a419)
1 5 6 12 16 17
=======
response.raw.bi<-data.frame(a402,a403,a404,a407,a408,a409,a410,a411,a413,a414,a415,a418,a419)

>>>>>>> ed36bd550fdfc183b1efd637f83e2237b961e208
#모두 NA인 행제거
#library(dplyr)
response.clean.bi <- response.raw.bi %>% filter(!is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a418)&!is.na(a419))
response<-response.clean.bi

#### CTT 점수산출 ####
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:13],na.rm=T)}
table(score.CTT)
hist(score.CTT)
#### Rasch 점수산출 ####
#library(mirt)
model.rasch <- 'F1 = 1-13' 
results.rasch <- mirt(data=response, model=model.rasch, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.rasch <- coef(results.rasch, IRTpars=TRUE, simplify=TRUE)
items.rasch <- as.data.frame(coef.rasch$items)
print(items.rasch)
summary(results.rasch)
plot(results.rasch, type = 'trace', which.items = c(1:13))
plot(results.rasch, type = 'infotrace', which.items = c(1:13))
plot(results.rasch, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.rasch, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.rasch, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.rasch, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.rasch, type = 'rxx', theta_lim = c(-4,4), lwd=2)

score.rasch<-fscores(results.rasch,method = 'EAP')
hist(score.rasch)# EAP(default) MAP ML WLE EAPsum

#### 2pl 점수산출####
#library(mirt)
model.2pl <- 'F1 = 1-13' 
results.2pl <- mirt(data=response, model=model.2pl, itemtype="2PL", SE=TRUE, verbose=FALSE)
coef.2pl <- coef(results.2pl, IRTpars=TRUE, simplify=TRUE)
items.2pl <- as.data.frame(coef.2pl$items)
print(items.2pl)
summary(results.2pl)
plot(results.2pl, type = 'trace', which.items = c(1:13))
plot(results.2pl, type = 'infotrace', which.items = c(1:13))
plot(results.2pl, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.2pl, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.2pl, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.2pl, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.2pl, type = 'rxx', theta_lim = c(-4,4), lwd=2)
score.2pl<-fscores(results.2pl,method = 'EAP')
hist(score.2pl)# EAP(default) MAP ML WLE EAPsum
#### CFA 점수산출####
#library(lavaan)
model.cfa<-'F1=~+a402+a403+a404+a407+a408+a409+a410+a411+a413+a414+a415+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response, ordered = T)
summary(results.cfa)
score.CFA<-lavPredict(results.cfa)
hist(score.CFA)
#### 각 점수 데이터프레임화 ####
score.frame<-cbind(score.CTT,score.CFA,score.rasch,score.2pl)
colnames(score.frame)<-c("CTT","CFA","rasch","twopl")
head(score.frame)
score.frame.t<-as_tibble(score.frame)
#CTT 특정점수이하만 남기기
#undercut<-filter(score.frame.t,score.frame<='24')
#head(undercut)
##상관그림
plot(score.frame.t)
# 점수별 상관비교
attach(score.frame.t)
cor(CTT,CFA,method="spearman")
cor(CTT,rasch,method="spearman")
cor(CTT,twopl,method="spearman")
cor(CFA,rasch,method="spearman")
cor(CFA,twopl,method="spearman")
cor(rasch,twopl,method="spearman")
cor(CTT,CFA)
cor(CTT,rasch)
cor(CTT,twopl)
cor(CFA,rasch)
cor(CFA,twopl)
cor(rasch,twopl)
detach(score.frame.t)