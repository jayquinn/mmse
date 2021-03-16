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
response.raw<-data.frame(a402,a403,a404,a407,a408,a409,a410,a411,a413,a414,a415,a418,a419)

#모두 NA인 행제거
#library(dplyr)
response.clean <- response.raw %>% filter(!is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a418)&!is.na(a419))
response<-response.clean

#### CTT 점수산출 ####
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:13],na.rm=T)}
table(score.CTT)
hist(score.CTT)
#### PCM 점수산출 ####
#library(mirt)
model.pcm <- 'F1 = 1-13' 
results.pcm <- mirt(data=response, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
summary(results.pcm)
plot(results.pcm, type = 'trace', which.items = c(1:13))
plot(results.pcm, type = 'infotrace', which.items = c(1:13))
plot(results.pcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)

score.PCM<-fscores(results.pcm,method = 'EAP')
hist(score.PCM)# EAP(default) MAP ML WLE EAPsum

#### GPCM 점수 산출####
#library(mirt)
model.gpcm <- 'F1 = 1-13' 
results.gpcm <- mirt(data=response, model=model.gpcm, itemtype="2PL", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
items.gpcm <- as.data.frame(coef.gpcm$items)
print(items.gpcm)
summary(results.gpcm)
plot(results.gpcm, type = 'trace', which.items = c(1:13))
plot(results.gpcm, type = 'infotrace', which.items = c(1:13))
plot(results.gpcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
hist(score.GPCM)# EAP(default) MAP ML WLE EAPsum
#### CFA 점수산출####
#library(lavaan)
model.cfa<-'F1=~+a402+a403+a404+a407+a408+a409+a410+a411+a413+a414+a415+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response, ordered = T)
summary(results.cfa)
score.CFA<-lavPredict(results.cfa)
hist(score.CFA)
#### 각 점수 데이터프레임화 ####
score.frame<-cbind(score.CTT,score.CFA,score.PCM,score.GPCM)
colnames(score.frame)<-c("CTT","CFA","PCM","GPCM")
head(score.frame)
#CTT 특정점수이하만 남기기
#score.frame.t<-as_tibble(score.frame)
#undercut<-filter(score.frame.t,score.frame<='24')
#head(undercut)
# 점수별 상관비교 언더컷버전
attach(score.frame)
cor(CTT,CFA,method="spearman")
cor(CTT,PCM,method="spearman")
cor(CTT,GPCM,method="spearman")
cor(CFA,PCM,method="spearman")
cor(CFA,GPCM,method="spearman")
cor(PCM,GPCM,method="spearman")
cor(CTT,CFA)
cor(CTT,PCM)
cor(CTT,GPCM)
cor(CFA,PCM)
cor(CFA,GPCM)
cor(PCM,GPCM)
detach(undercut)
#### 각 점수별 상관비교 ####
cor(score.CTT,score.CFA,method="kendall")
cor(score.CTT,score.PCM,method="kendall")
cor(score.CTT,score.GPCM,method="kendall")
cor(score.CFA,score.PCM,method="kendall")
cor(score.CFA,score.GPCM,method="kendall")
cor(score.PCM,score.GPCM,method="kendall")
cor(score.CTT,score.CFA)
cor(score.CTT,score.PCM)
cor(score.CTT,score.GPCM)
cor(score.CFA,score.PCM)
cor(score.CFA,score.GPCM)
cor(score.PCM,score.GPCM)
tetrachoric(score.CTT,score.CFA)

### 하다 말았음 신뢰하지 말 것