nrow(scoreframe[scoreframe$age>=70 & scoreframe$age<=79,])
nrow(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,])
nrow(scoreframe[scoreframe$age>=90 & scoreframe$age<=105,])

mh<-scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]

# (1) 여기 표본에서 IRT 점수2표준편차 바깥 마킹해서 CTT 변환후 합불합 확인하기
# (2) 여기 표본에서 IRT 점수 2표준편차 컷오프 점수 CTT로 변환후 민감도 확인하기
# (3) CTT에서 마킹 후 IRT 변환 vs IRT에서 마킹후 CTT 변환
hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CTT)
hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CFA)
hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$PCM)
hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$GPCM)

tail(table(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$PCM)) # 여기서 1.118인애들은 천장효과로 칩시다
tail(table(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$GPCM)) # 여기서 1.118인애들은 천장효과로 칩시다

hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89 & scoreframe$GPCM<=1.118,]$GPCM,breaks=20)

hist(scoreframe[scoreframe$age>=80 & scoreframe$age<=89 & scoreframe$PCM<=2.38,]$PCM,breaks=20)

ks.test(scoreframe$CTT,'pnorm',mean(scoreframe$CTT),sd(scoreframe$CTT))
ks.test(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CTT,'pnorm',mean(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CTT),sd(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CTT))
ks.test(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CFA,'pnorm',mean(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CFA),sd(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$CFA))
ks.test(scoreframe[scoreframe$age>=80 & scoreframe$age<=89& scoreframe$PCM<=2.36,]$PCM,'pnorm',mean(scoreframe[scoreframe$age>=80 & scoreframe$age<=89 & scoreframe$PCM<=2.36,]$PCM),sd(scoreframe[scoreframe$age>=80 & scoreframe$age<=89 & scoreframe$PCM<=2.36,]$PCM))
ks.test(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$GPCM,'pnorm',mean(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$GPCM),sd(scoreframe[scoreframe$age>=80 & scoreframe$age<=89,]$GPCM))
ks.test(scoreframe[scoreframe$GPCM<=1.118,]$GPCM,'pnorm',mean(scoreframe[scoreframe$GPCM<=1.118,]$GPCM),sd(scoreframe[scoreframe$GPCM<=1.118,]$GPCM))
hist(scoreframe[scoreframe$GPCM<=1.118,]$GPCM,breaks=20)
