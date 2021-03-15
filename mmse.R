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
#401
a401<-ifelse(C401==3,3,
             ifelse(C401==2,2,
                    ifelse(C401==1,1,
                           ifelse(C401==-8,NA,
                                  ifelse(C401==-9,0,
                           ifelse(C401==5,0,NA))))))
head(a401)
table(C401,a401,useNA='always')
length(C401);length(a401)
table(C401)
table(a401)
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
#405
a405<-ifelse(C405==4,4,
             ifelse(C405==3,3,
             ifelse(C405==2,2,
                    ifelse(C405==1,1,
                           ifelse(C405==-8,NA,
                                  ifelse(C405==-9,0,
                           ifelse(C405==5,0,NA)))))))
head(a405)
table(C405,a405,useNA='always')
table(C405)
table(a405)
length(C405);length(a405)
#406
a406<-ifelse(C406==3,3,
                    ifelse(C406==2,2,
                           ifelse(C406==1,1,
                                  ifelse(C406==-8,NA,
                                         ifelse(C406==-9,0,
                                  ifelse(C406==5,0,NA))))))
table(C406,a406,useNA='always')
table(C406)
table(a406)
length(C406);length(a406)
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
#412
a412<-ifelse(C412==3,3,
             ifelse(C412==2,2,
                    ifelse(C412==1,1,
                           ifelse(C412==-8,NA,
                                  ifelse(C412==-9,0,
                           ifelse(C412==5,0,NA))))))
table(C412,a412,useNA='always')
table(C412)
table(a412)
length(C412);length(a412)
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
#416
a416<-ifelse(C416==3,3,
             ifelse(C416==2,2,
                    ifelse(C416==1,1,
                           ifelse(C416==-8,NA,
                                  ifelse(C416==-9,0,
                           ifelse(C416==5,0,NA))))))
table(C416,a416,useNA='always')
table(C416)
table(a416)
length(C416);length(a416)
#417
a417<-ifelse(C417==3,2,
                    ifelse(C417==1,1,
                           ifelse(C417==-8,NA,
                                  ifelse(C417==-9,0,
                           ifelse(C417==5,0,NA)))))
table(C417,a417,useNA='always')
table(C417)
table(a417)
length(C417);length(a417)
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
raw_response<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419)

#모두 NA인 행제거
#library(dplyr)
response <-  raw_response %>% filter(!is.na(a401) & !is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a405)&!is.na(a406)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a412)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a416)&!is.na(a417)&!is.na(a418)&!is.na(a419))


#### CTT 점수산출 ####
scoreCTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
scoreCTT[[i]]<-sum(response[i,1:19],na.rm=T)}
table(scoreCTT)
hist(scoreCTT)
#### PCM 점수산출 ####
#library(mirt)
model.pcm <- 'F1 = 1-19' 
results.pcm <- mirt(data=response, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE)
coef.pcm <- coef(results.pcm, IRTpars=TRUE, simplify=TRUE)
items.pcm <- as.data.frame(coef.pcm$items)
print(items.pcm)
plot(results.pcm, type = 'trace', which.items = c(1:19))
plot(results.pcm, type = 'infotrace', which.items = c(1:19))
plot(results.pcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.pcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)

score.PCM<-fscores(results.pcm,method = 'EAP')
hist(score.PCM)# EAP(default) MAP ML WLE EAPsum

#### GPCM 점수 산출####
#library(mirt)
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE)
coef.gpcm <- coef(results.gpcm, IRTpars=TRUE, simplify=TRUE)
items.gpcm <- as.data.frame(coef.gpcm$items)
print(items.gpcm)
plot(results.gpcm, type = 'trace', which.items = c(1:19))
plot(results.gpcm, type = 'infotrace', which.items = c(1:19))
plot(results.gpcm, type = 'info', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'SE', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'score', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2)
plot(results.gpcm, type = 'rxx', theta_lim = c(-4,4), lwd=2)
score.GPCM<-fscores(results.gpcm,method = 'EAP')
hist(score.GPCM)# EAP(default) MAP ML WLE EAPsum
#### CFA 점수산출####
#library(lavaan)
model.cfa<-'F1=~a401+a402+a403+a404+a405+a406+a407+a408+a409+a410+a411+a412+a413+a414+a415+a416+a417+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response, ordered = T)
summary(results.cfa)
score.CFA<-lavPredict(results.cfa)
hist(score.CFA)
