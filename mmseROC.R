#### 힛트다 힛트 CTT ####
# 17이하 치매의심, 18이상 23이하 인지기능 저하, 24이상 정상
#install.packages("Epi")
#library(Epi)
attach(scoreframe)
detach(scoreframe)#도합 6548
CTT_a<-filter(scoreframe,CTT>=24&diag==5) #4843
CTT_b<-filter(scoreframe,CTT>=24&diag<=3) #9
CTT_c<-filter(scoreframe,CTT<=23&diag==5) #1620
CTT_d<-filter(scoreframe,CTT<=23&diag<=3) #76 도합 6548
CTT_FPR<-(nrow(CTT_c)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_FNR<-(nrow(CTT_b)/(nrow(CTT_b)+nrow(CTT_d))) 
CTT_sens<-(nrow(CTT_d)/(nrow(CTT_b)+nrow(CTT_d)))
CTT_spec<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_c)))
CTT_PPP<-(nrow(CTT_d)/(nrow(CTT_d)+nrow(CTT_c)))
CTT_NPP<-(nrow(CTT_a)/(nrow(CTT_a)+nrow(CTT_b)))
CTT_PCO<-((nrow(CTT_a)+nrow(CTT_b))/(nrow(scoreframe)))
CTT_Md<-sqrt((1-CTT_sens)^2+(1-CTT_spec)^2)

sipal<-cbind(CTT_sens,CTT_spec)

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


install.packages("ROCit")
library(ROCit)
ROCit_obj <- rocit(score=scoreframe$PCM,class=scoreframe$diag)
plot(ROCit_obj)

go<-measureit(scoreframe$CTT,scoreframe$diag)


#0  0.9975244 0.09411765

install.packages("pROC")
library(pROC)
pROC_obj <- roc(scoreframe$diag,scoreframe$CTT,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE)
