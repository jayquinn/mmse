#### 힛트다 힛트 CTT ####
# 17이하 치매의심, 18이상 23이하 인지기능 저하, 24이상 정상
#install.packages("Epi")
library(Epi)
attach(scoreframe)
detach(scoreframe)#도합 6548
#### 진단정보(diag)  1 치매, 3경도인지장애, 5 정상
#### 진단정보(diag) -> 5 -> 1, 1,3 -> 0
CTT_a<-filter(scoreframe,CTT>=24&diag==1) #4843
CTT_b<-filter(scoreframe,CTT>=24&diag==0) #9
CTT_c<-filter(scoreframe,CTT<=23&diag==1) #1620
CTT_d<-filter(scoreframe,CTT<=23&diag==0) #76 도합 6548
CTT_FPR<-(nrow(CTT_c)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_FNR<-(nrow(CTT_b)/(nrow(CTT_b)+nrow(CTT_d))) 
CTT_sens<-(nrow(CTT_d)/(nrow(CTT_b)+nrow(CTT_d)))
CTT_spec<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_PPP<-(nrow(CTT_d)/(nrow(CTT_d)+nrow(CTT_c)))
CTT_NPP<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_b)))
CTT_PCO<-((nrow(CTT_a)+nrow(CTT_b))/(nrow(scoreframe)))
CTT_Md<-sqrt((1-CTT_sens)^2+(1-CTT_spec)^2)

PCM_a<-filter(scoreframe,PCM>=-1.33386268&diag==1)
PCM_b<-filter(scoreframe,PCM>=-1.33386268&diag==0)
PCM_c<-filter(scoreframe,PCM<=-1.33386267&diag==1)
PCM_d<-filter(scoreframe,PCM<=-1.33386267&diag==0)#도합 6548
PCM_FPR<-(nrow(PCM_c)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_FNR<-(nrow(PCM_b)/(nrow(PCM_b)+nrow(PCM_d))) 
PCM_sens<-(nrow(PCM_d)/(nrow(PCM_b)+nrow(PCM_d)))
PCM_spec<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_PPP<-(nrow(PCM_d)/(nrow(PCM_d)+nrow(PCM_c)))
PCM_NPP<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_b)))
PCM_PCO<-((nrow(PCM_a)+nrow(PCM_b))/(nrow(scoreframe)))
PCM_Md<-sqrt((1-PCM_sens)^2+(1-PCM_spec)^2)

GPCM_a<-filter(scoreframe,GPCM>=-0.614769975&diag==1)
GPCM_b<-filter(scoreframe,GPCM>=-0.614769975&diag==0)
GPCM_c<-filter(scoreframe,GPCM<=-0.614769974&diag==1)
GPCM_d<-filter(scoreframe,GPCM<=-0.614769974&diag==0)#도합 6548
GPCM_FPR<-(nrow(GPCM_c)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_FNR<-(nrow(GPCM_b)/(nrow(GPCM_b)+nrow(GPCM_d))) 
GPCM_sens<-(nrow(GPCM_d)/(nrow(GPCM_b)+nrow(GPCM_d)))
GPCM_spec<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_PPP<-(nrow(GPCM_d)/(nrow(GPCM_d)+nrow(GPCM_c)))
GPCM_NPP<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_b)))
GPCM_PCO<-((nrow(GPCM_a)+nrow(GPCM_b))/(nrow(scoreframe)))
GPCM_Md<-sqrt((1-GPCM_sens)^2+(1-GPCM_spec)^2)


CFA_a<-filter(scoreframe,CFA>=-0.1532998851&diag==1)
CFA_b<-filter(scoreframe,CFA>=-0.1532998851&diag==0)
CFA_c<-filter(scoreframe,CFA<=-0.1532998850&diag==1)
CFA_d<-filter(scoreframe,CFA<=-0.1532998850&diag==0)#도합 6548
CFA_FPR<-(nrow(CFA_c)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_FNR<-(nrow(CFA_b)/(nrow(CFA_b)+nrow(CFA_d))) 
CFA_sens<-(nrow(CFA_d)/(nrow(CFA_b)+nrow(CFA_d)))
CFA_spec<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_PPP<-(nrow(CFA_d)/(nrow(CFA_d)+nrow(CFA_c)))
CFA_NPP<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_b)))
CFA_PCO<-((nrow(CFA_a)+nrow(CFA_b))/(nrow(scoreframe)))
CFA_Md<-sqrt((1-CFA_sens)^2+(1-CFA_spec)^2)


#퍼센타일로 찍어누르는게 아니라는 생각이 계속 드는데.. 다른 방법이 있을거같다. 이건 아닌거같아. 어디서부터 잘못된거지?



library(Epi)

siba<-ROC(scoreframe$CTT, scoreframe$diag) #AUC .909 AIC 631
siba2<-ROC(round(scoreframe$CFA,2), scoreframe$diag) #AUC .906 AIC 642.2
siba3<-ROC(round(scoreframe$PCM,2), scoreframe$diag) #AUC 8.479...? AIC 628.6
siba4<-ROC(round(scoreframe$GPCM,2), scoreframe$diag) #AUC .907 AIC 645.7
###https://stackoverflow.com/questions/23131897/how-can-i-get-the-optimal-cutoff-point-of-the-roc-in-logistic-regression-as-a-nu
opt <- which.max(rowSums(siba$res[, c("sens", "spec")])) #최적점수찾기
opt2 <- which.max(rowSums(siba2$res[, c("sens", "spec")])) #최적점수찾기
opt3 <- which.max(rowSums(siba3$res[, c("sens", "spec")])) #최적점수찾기
opt4 <- which.max(rowSums(siba4$res[, c("sens", "spec")])) #최적점수찾기
siba$res$`scoreframe$CTT`[opt]
siba2$res$`round(scoreframe$CFA, 2)`[opt2]
siba3$res$`round(scoreframe$PCM, 2)`[opt3]
siba4$res$`round(scoreframe$GPCM, 2)`[opt4]
