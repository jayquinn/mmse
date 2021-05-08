library(dplyr)
library(mirt)
library(lavaan)
library(Epi)
dat<-read.csv("C:/git/mmse/mmse.csv",header=T,sep=",")
#### 데이터 전처리 ####
attach(dat)
#401
a401<-ifelse(C401==3,3,
             ifelse(C401==2,2,
                    ifelse(C401==1,1,
                           ifelse(C401==-8,NA,
                                  ifelse(C401==-9,0,
                                         ifelse(C401==5,0,NA))))))
#402
a402<-ifelse(C402==1,1,
             ifelse(C402==-8,NA,
                    ifelse(C402==-9,0,
                           ifelse(C402==5,0,NA))))
#403
a403<-ifelse(C403==1,1,
             ifelse(C403==-8,NA,
                    ifelse(C403==-9,0,
                           ifelse(C403==5,0,NA))))
#404
a404<-ifelse(C404==1,1,
             ifelse(C404==-8,NA,
                    ifelse(C404==-9,0,
                           ifelse(C404==5,0,NA))))
#405
a405<-ifelse(C405==4,4,
             ifelse(C405==3,3,
                    ifelse(C405==2,2,
                           ifelse(C405==1,1,
                                  ifelse(C405==-8,NA,
                                         ifelse(C405==-9,0,
                                                ifelse(C405==5,0,NA)))))))
#406
a406<-ifelse(C406==3,3,
             ifelse(C406==2,2,
                    ifelse(C406==1,1,
                           ifelse(C406==-8,NA,
                                  ifelse(C406==-9,0,
                                         ifelse(C406==5,0,NA))))))
#407
a407<-ifelse(C407==1,1,
             ifelse(C407==-8,NA,
                    ifelse(C407==-9,0,
                           ifelse(C407==5,0,NA))))
#408
a408<-ifelse(C408==1,1,
             ifelse(C408==-8,NA,
                    ifelse(C408==-9,0,
                           ifelse(C408==5,0,NA))))
#409
a409<-ifelse(C409==1,1,
             ifelse(C409==-8,NA,
                    ifelse(C409==-9,0,
                           ifelse(C409==5,0,NA))))
#410
a410<-ifelse(C410==1,1,
             ifelse(C410==-8,NA,
                    ifelse(C410==-9,0,
                           ifelse(C410==5,0,NA))))
#411
a411<-ifelse(C411==1,1,
             ifelse(C411==-8,NA,
                    ifelse(C411==-9,0,
                           ifelse(C411==5,0,NA))))
#412
a412<-ifelse(C412==3,3,
             ifelse(C412==2,2,
                    ifelse(C412==1,1,
                           ifelse(C412==-8,NA,
                                  ifelse(C412==-9,0,
                                         ifelse(C412==5,0,NA))))))
#413
a413<-ifelse(C413==1,1,
             ifelse(C413==-8,NA,
                    ifelse(C413==-9,0,
                           ifelse(C413==5,0,NA))))
#414
a414<-ifelse(C414==1,1,
             ifelse(C414==-8,NA,
                    ifelse(C414==-9,0,
                           ifelse(C414==5,0,NA))))
#415
a415<-ifelse(C415==1,1,
             ifelse(C415==-8,NA,
                    ifelse(C415==-9,0,
                           ifelse(C415==5,0,NA))))
#416
a416<-ifelse(C416==3,3,
             ifelse(C416==2,2,
                    ifelse(C416==1,1,
                           ifelse(C416==-8,NA,
                                  ifelse(C416==-9,0,
                                         ifelse(C416==5,0,NA))))))
#417
a417<-ifelse(C417==3,2,
             ifelse(C417==1,1,
                    ifelse(C417==-8,NA,
                           ifelse(C417==-9,0,
                                  ifelse(C417==5,0,NA)))))
#418
a418<-ifelse(C418==1,1,
             ifelse(C418==-8,NA,
                    ifelse(C418==-9,0,
                           ifelse(C418==5,0,NA))))
#419
a419<-ifelse(C419==1,1,
             ifelse(C419==-8,NA,
                    ifelse(C419==-9,0,
                           ifelse(C419==5,0,NA))))
response.raw<-data.frame(a401,a402,a403,a404,a405,a406,a407,a408,a409,a410,a411,a412,a413,a414,a415,a416,a417,a418,a419,dat$diag)
detach(dat)
response.raw$diag<-ifelse(dat$diag==5,1,
                          ifelse(dat$diag==3,0,
                                 ifelse(dat$diag==1,0,dat$diag))) #정상 1, 경도인지+치매 0
#내가 지금 하려는건, 경도인지 + 치매(0)인 사람이 올 NA면 0점으로 잡고 분석을 해보자는 것
#(1) diag가 0이고 (2) a401-419이 모두 NA인 사람은 (3) a401-419 값을 0으로 바꿔주기 
#(i) 행 추출하기 (ii) subset (iii). filter
response.raw[response.raw$diag==0&is.na(a401),]<-0
response.clean <- response.raw %>% filter(!is.na(a401) & !is.na(a402) &!is.na(a403)&!is.na(a404)&!is.na(a405)&!is.na(a406)&!is.na(a407)&!is.na(a408)&!is.na(a409)&!is.na(a410)&!is.na(a411)&!is.na(a412)&!is.na(a413)&!is.na(a414)&!is.na(a415)&!is.na(a416)&!is.na(a417)&!is.na(a418)&!is.na(a419))
response<-response.clean
clean.diag<-response[,21]
response<-response[,1:19]
#### CTT 점수산출 ####
score.CTT<-vector("double",nrow(response))
for ( i in 1:nrow(response) ){
  score.CTT[[i]]<-sum(response[i,1:19],na.rm=T)}
#### PCM 점수산출 ####
model.pcm <- 'F1 = 1-19' 
results.pcm <- mirt(data=response, model=model.pcm, itemtype="Rasch", SE=TRUE, verbose=FALSE,technical = list(removeEmptyRows=TRUE))
score.PCM<-fscores(results.pcm,method = 'EAP')
#### GPCM 점수 산출####
model.gpcm <- 'F1 = 1-19' 
results.gpcm <- mirt(data=response, model=model.gpcm, itemtype="gpcm", SE=TRUE, verbose=FALSE,technical = list(removeEmptyRows=TRUE))
score.GPCM<-fscores(results.gpcm,method = 'EAP')
#### CFA 점수산출####
model.cfa<-'F1=~a401+a402+a403+a404+a405+a406+a407+a408+a409+a410+a411+a412+a413+a414+a415+a416+a417+a418+a419'
results.cfa<-cfa(model=model.cfa,data = response)
score.CFA<-lavPredict(results.cfa)
#### 각 점수 데이터프레임화 ####
score.frame<-cbind(score.CTT,score.CFA,score.PCM,score.GPCM,clean.diag)
colnames(score.frame)<-c("CTT","CFA","PCM","GPCM","diag")
tail(score.frame)
score.frame.t<-as_tibble(score.frame)
siba<-ROC(score.frame.t$CTT, score.frame.t$diag) #AUC .909 AIC 631
siba2<-ROC(round(score.frame.t$CFA,2), score.frame.t$diag) #AUC .906 AIC 642.2
siba3<-ROC(round(score.frame.t$PCM,2), score.frame.t$diag) #AUC 8.479...? AIC 628.6
siba4<-ROC(round(score.frame.t$GPCM,2), score.frame.t$diag) #AUC .907 AIC 645.7
edit(score.frame.t)
