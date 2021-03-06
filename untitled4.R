#### 적중 CTT ####
# 17이하 치매의심, 18이상 23이하 인지기능 저하, 24이상 정상
#install.packages("Epi")
library(Epi)
attach(scoreframe)
detach(scoreframe)#도합 6548
#### 진단정보(diag)  1 치매, 3경도인지장애, 5 정상
#### 진단정보(diag) -> 5,3 -> 1, 1-> 0
quantile(scoreframe$CTT,probs = 0.1)
#
CTT_a<-filter(scoreframe,CTT>24&diag==1)
CTT_b<-filter(scoreframe,CTT>24&diag==0) 
CTT_c<-filter(scoreframe,CTT<=24&diag==1) 
CTT_d<-filter(scoreframe,CTT<=24&diag==0)
nrow(CTT_a)
nrow(CTT_b)
nrow(CTT_c)
nrow(CTT_d)
CTT_FPR<-(nrow(CTT_c)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_FNR<-(nrow(CTT_b)/(nrow(CTT_b)+nrow(CTT_d))) 
CTT_sens<-(nrow(CTT_d)/(nrow(CTT_b)+nrow(CTT_d)))
CTT_spec<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_PPP<-(nrow(CTT_d)/(nrow(CTT_d)+nrow(CTT_c)))
CTT_NPP<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_b)))
CTT_PCO<-((nrow(CTT_a)+nrow(CTT_d))/(nrow(scoreframe)))
CTT_Md<-sqrt((1-CTT_sens)^2+(1-CTT_spec)^2)

PCM_a<-filter(scoreframe,PCM>quantile(scoreframe$PCM,probs = 0.1)&diag==1)
PCM_b<-filter(scoreframe,PCM>quantile(scoreframe$PCM,probs = 0.1)&diag==0)
PCM_c<-filter(scoreframe,PCM<=quantile(scoreframe$PCM,probs = 0.1)&diag==1)
PCM_d<-filter(scoreframe,PCM<=quantile(scoreframe$PCM,probs = 0.1)&diag==0)#도합 6548
nrow(PCM_a)
nrow(PCM_b)
nrow(PCM_c)
nrow(PCM_d)
PCM_FPR<-(nrow(PCM_c)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_FNR<-(nrow(PCM_b)/(nrow(PCM_b)+nrow(PCM_d))) 
PCM_sens<-(nrow(PCM_d)/(nrow(PCM_b)+nrow(PCM_d)))
PCM_spec<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_PPP<-(nrow(PCM_d)/(nrow(PCM_d)+nrow(PCM_c)))
PCM_NPP<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_b)))
PCM_PCO<-((nrow(PCM_a)+nrow(PCM_d))/(nrow(scoreframe)))
PCM_Md<-sqrt((1-PCM_sens)^2+(1-PCM_spec)^2)

GPCM_a<-filter(scoreframe,GPCM>quantile(scoreframe$GPCM,probs = 0.1)&diag==1)
GPCM_b<-filter(scoreframe,GPCM>quantile(scoreframe$GPCM,probs = 0.1)&diag==0)
GPCM_c<-filter(scoreframe,GPCM<=quantile(scoreframe$GPCM,probs = 0.1)&diag==1)
GPCM_d<-filter(scoreframe,GPCM<=quantile(scoreframe$GPCM,probs = 0.1)&diag==0)#도합 6548
nrow(GPCM_a)
nrow(GPCM_b)
nrow(GPCM_c)
nrow(GPCM_d)
GPCM_FPR<-(nrow(GPCM_c)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_FNR<-(nrow(GPCM_b)/(nrow(GPCM_b)+nrow(GPCM_d))) 
GPCM_sens<-(nrow(GPCM_d)/(nrow(GPCM_b)+nrow(GPCM_d)))
GPCM_spec<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_PPP<-(nrow(GPCM_d)/(nrow(GPCM_d)+nrow(GPCM_c)))
GPCM_NPP<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_b)))
GPCM_PCO<-((nrow(GPCM_a)+nrow(GPCM_d))/(nrow(scoreframe)))
GPCM_Md<-sqrt((1-GPCM_sens)^2+(1-GPCM_spec)^2)


CFA_a<-filter(scoreframe,CFA>quantile(scoreframe$CFA,probs = 0.1)&diag==1)
CFA_b<-filter(scoreframe,CFA>quantile(scoreframe$CFA,probs = 0.1)&diag==0)
CFA_c<-filter(scoreframe,CFA<=quantile(scoreframe$CFA,probs = 0.1)&diag==1)
CFA_d<-filter(scoreframe,CFA<=quantile(scoreframe$CFA,probs = 0.1)&diag==0)#도합 6548
nrow(CFA_a)
nrow(CFA_b)
nrow(CFA_c)
nrow(CFA_d)
CFA_FPR<-(nrow(CFA_c)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_FNR<-(nrow(CFA_b)/(nrow(CFA_b)+nrow(CFA_d))) 
CFA_sens<-(nrow(CFA_d)/(nrow(CFA_b)+nrow(CFA_d)))
CFA_spec<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_PPP<-(nrow(CFA_d)/(nrow(CFA_d)+nrow(CFA_c)))
CFA_NPP<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_b)))
CFA_PCO<-((nrow(CFA_a)+nrow(CFA_d))/(nrow(scoreframe)))
CFA_Md<-sqrt((1-CFA_sens)^2+(1-CFA_spec)^2)


# 1.5 sd로 변경해서 분석
CTT_a<-filter(scoreframe,CTT>17&diag==1)
CTT_b<-filter(scoreframe,CTT>17&diag==0) 
CTT_c<-filter(scoreframe,CTT<=17&diag==1) 
CTT_d<-filter(scoreframe,CTT<=17&diag==0) 
CTT_FPR<-(nrow(CTT_c)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_FNR<-(nrow(CTT_b)/(nrow(CTT_b)+nrow(CTT_d))) 
CTT_sens<-(nrow(CTT_d)/(nrow(CTT_b)+nrow(CTT_d)))
CTT_spec<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_PPP<-(nrow(CTT_d)/(nrow(CTT_d)+nrow(CTT_c)))
CTT_NPP<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_b)))
CTT_PCO<-((nrow(CTT_a)+nrow(CTT_b))/(nrow(scoreframe)))
CTT_Md<-sqrt((1-CTT_sens)^2+(1-CTT_spec)^2)

PCM_a<-filter(scoreframe,PCM>-2.758286&diag==1)
PCM_b<-filter(scoreframe,PCM>-2.758286&diag==0)
PCM_c<-filter(scoreframe,PCM<=-2.758286&diag==1)
PCM_d<-filter(scoreframe,PCM<=-2.758286&diag==0)#도합 6548
PCM_FPR<-(nrow(PCM_c)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_FNR<-(nrow(PCM_b)/(nrow(PCM_b)+nrow(PCM_d))) 
PCM_sens<-(nrow(PCM_d)/(nrow(PCM_b)+nrow(PCM_d)))
PCM_spec<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_c)))
PCM_PPP<-(nrow(PCM_d)/(nrow(PCM_d)+nrow(PCM_c)))
PCM_NPP<-(nrow(PCM_a)/(nrow(PCM_a)+nrow(PCM_b)))
PCM_PCO<-((nrow(PCM_a)+nrow(PCM_b))/(nrow(scoreframe)))
PCM_Md<-sqrt((1-PCM_sens)^2+(1-PCM_spec)^2)

GPCM_a<-filter(scoreframe,GPCM>-1.370837&diag==1)
GPCM_b<-filter(scoreframe,GPCM>-1.370837&diag==0)
GPCM_c<-filter(scoreframe,GPCM<=-1.370837&diag==1)
GPCM_d<-filter(scoreframe,GPCM<=-1.370837&diag==0)#도합 6548
GPCM_FPR<-(nrow(GPCM_c)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_FNR<-(nrow(GPCM_b)/(nrow(GPCM_b)+nrow(GPCM_d))) 
GPCM_sens<-(nrow(GPCM_d)/(nrow(GPCM_b)+nrow(GPCM_d)))
GPCM_spec<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_c)))
GPCM_PPP<-(nrow(GPCM_d)/(nrow(GPCM_d)+nrow(GPCM_c)))
GPCM_NPP<-(nrow(GPCM_a)/(nrow(GPCM_a)+nrow(GPCM_b)))
GPCM_PCO<-((nrow(GPCM_a)+nrow(GPCM_b))/(nrow(scoreframe)))
GPCM_Md<-sqrt((1-GPCM_sens)^2+(1-GPCM_spec)^2)


CFA_a<-filter(scoreframe,CFA>-0.688565&diag==1)
CFA_b<-filter(scoreframe,CFA>-0.688565&diag==0)
CFA_c<-filter(scoreframe,CFA<=-0.688565&diag==1)
CFA_d<-filter(scoreframe,CFA<=-0.688565&diag==0)#도합 6548
CFA_FPR<-(nrow(CFA_c)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_FNR<-(nrow(CFA_b)/(nrow(CFA_b)+nrow(CFA_d))) 
CFA_sens<-(nrow(CFA_d)/(nrow(CFA_b)+nrow(CFA_d)))
CFA_spec<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_c)))
CFA_PPP<-(nrow(CFA_d)/(nrow(CFA_d)+nrow(CFA_c)))
CFA_NPP<-(nrow(CFA_a)/(nrow(CFA_a)+nrow(CFA_b)))
CFA_PCO<-((nrow(CFA_a)+nrow(CFA_b))/(nrow(scoreframe)))
CFA_Md<-sqrt((1-CFA_sens)^2+(1-CFA_spec)^2)


# 분포가 병신이면 백분위로 가야한다

siba<-ROC(scoreframe$CTT, scoreframe$diag)
siba2<-ROC(round(scoreframe$CFA,2), scoreframe$diag)
siba3<-ROC(round(scoreframe$PCM,2), scoreframe$diag)
siba4<-ROC(round(scoreframe$GPCM,2), scoreframe$diag)

###https://stackoverflow.com/questions/23131897/how-can-i-get-the-optimal-cutoff-point-of-the-roc-in-logistic-regression-as-a-nu
hist(scoreframe$CTT); abline(v=quantile(scoreframe$CTT,probs = 0.1),col="blue",lwd=2)
hist(scoreframe$CFA); abline(v=quantile(scoreframe$CFA,probs = 0.1),col="blue",lwd=2)
hist(scoreframe$PCM); abline(v=quantile(scoreframe$PCM,probs = 0.1),col="blue",lwd=2)
hist(scoreframe$GPCM,breaks=20); abline(v=quantile(scoreframe$GPCM,probs = 0.1),col="blue",lwd=2)
length(scoreframe$CTT[CTT<=17]); #643/6548 = 0.098
length(scoreframe$CFA[CFA<=-0.57]) #724/6548 = 0.110
length(scoreframe$PCM[PCM<=-2.46]) #561/6548 = 0.085
length(scoreframe$GPCM[GPCM<=-1.01]) #860/6548 = 0.131
#저 654,724,561,860명 중에 진짜 치매진단 사람의 비율은 어떻게 될까요?
#위에 식


opt <- which.max(rowSums(siba$res[, c("sens", "spec")])) #최적점수찾기
opt2 <- which.max(rowSums(siba2$res[, c("sens", "spec")])) #최적점수찾기
opt3 <- which.max(rowSums(siba3$res[, c("sens", "spec")])) #최적점수찾기
opt4 <- which.max(rowSums(siba4$res[, c("sens", "spec")])) #최적점수찾기
siba$res$`scoreframe$CTT`[opt]
siba2$res$`round(scoreframe$CFA, 2)`[opt2]
siba3$res$`round(scoreframe$PCM, 2)`[opt3]
siba4$res$`round(scoreframe$GPCM, 2)`[opt4]


et.gpcm<-expected.test(results.gpcm,as.matrix(scoreframe$GPCM))
et.pcm<-expected.test(results.pcm,as.matrix(scoreframe$PCM))
hist(scoreframe$GPCM)
hist(et.gpcm)
hist(scoreframe$PCM)
hist(et.pcm)



