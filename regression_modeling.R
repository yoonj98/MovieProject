install.packages('MASS')
install.packages('psych')
install.packages('car')
install.packages("Hmisc")
install.packages("dummies")
install.packages('ggplot2')

library(ggplot2)
library(dummies)
library(Hmisc)
library(MASS)
library(psych)
library(car)

data = read.csv('C:/Users/yoonj/Desktop/regression/data/finaldata.csv')

##변수간 상관관계 확인
#상관분석
data_select=data
pairs.panels(data_select)

#선형관계를 위해선 변수 변환 필요
par(mfrow=c(1,2))
plot((data_select$audiAcc)~(data_select$actorscore), main="rawdata:actorscore")
plot(log(data_select$audiAcc)~log(data_select$actorscore), main="transform_data:actorscore")
plot((data_select$directorscore),(data_select$audiAcc), main="rawdata:directorscore")
plot(log(data_select$directorscore),log(data_select$audiAcc), main="transform_data:directorscore")


plot(data_select$gen,log(data_select$audiAcc))
plot(data_select$nation,log(data_select$audiAcc))

plot((data_select$firstscrnCnt),(data_select$audiAcc), main="rawdata:firstscrnCnt")
plot(sqrt(data_select$firstscrnCnt),log(data_select$audiAcc), main="transform_data:firstscrnCnt")
plot((data_select$movielike),(data_select$audiAcc), main="rawdata:movielike")
plot(log(data_select$movielike),log(data_select$audiAcc), main="transform_data:movielike")

plot(data_select$movieScore,(data_select$audiAcc), main="rawdata")
plot(data_select$movieScore,log(data_select$audiAcc), main="transform_data") #-> 비선형

#더미변수화
data_dummy<-dummy.data.frame(data)
str(data_dummy)

#변환을 위해 더미인 변수와 아닌 변수 잠시 분리
tdata_nondummy = data_dummy[,c(1:7)]
pairs.panels(tdata_nondummy)

tdata_dummy= data_dummy[,-c(1:7)]

par(mfrow=c(2,2))
plot((data_dummy$audiAcc)~(data_dummy$actorscore), main="rawdata:actorscore")
plot(log(data_dummy$audiAcc)~log(data_dummy$actorscore), main="transform_data:actorscore")

plot((data_dummy$directorscore),(data_dummy$audiAcc), main="rawdata:directorscore")
plot(log(data_dummy$directorscore),log(data_dummy$audiAcc), main="transform_data:directorscore")

plot((data_dummy$firstscrnCnt),(data_dummy$audiAcc), main="rawdata:firstscrnCnt")
plot(sqrt(data_dummy$firstscrnCnt),log(data_dummy$audiAcc), main="transform_data:firstscrnCnt")

plot((data_dummy$like),(data_dummy$audiAcc), main="rawdata:movielike")
plot(log(data_dummy$like),log(data_dummy$audiAcc), main="transform_data:movielike")

plot((data_dummy$news),(data_dummy$audiAcc), main="rawdata:movienews")
plot(log(data_dummy$news),log(data_dummy$audiAcc), main="transform_data:movienews")

plot((data_dummy$audiSat),(data_dummy$audiAcc), main="rawdata:movieaudiSat")
plot(log(data_dummy$audiSat),log(data_dummy$audiAcc), main="transform_data:movieaudiSat")

plot((data_dummy$distributorscore),(data_dummy$audiAcc), main="rawdata:distributorscore")
plot(log(data_dummy$distributorscore),log(data_dummy$audiAcc), main="transform_data:distributorscore")

#변환
tdata_nondummy$audiAcc = log(data_dummy$audiAcc)
tdata_nondummy$actorscore = log(data_dummy$actorscore)
tdata_nondummy$directorscore = log(data_dummy$directorscore)
tdata_nondummy$like = log(data_dummy$like)
tdata_nondummy$audiSat = log(data_dummy$audiSat)
tdata_nondummy$firstscrnCnt = sqrt(data_dummy$firstscrnCnt)
tdata_nondummy$news = log(data_dummy$news)

pairs.panels(tdata_nondummy)

#변환 후 재 병합
tdata=cbind(tdata_nondummy,tdata_dummy)

fit <- lm(audiAcc~., data = tdata)
summary(fit)

#backward
back<-step(fit, direction = "backward", trace = T)
back$anova

summary(back)
anova(back)

#forward
forw<-step(fit, direction = "forward", trace = 0)
forw$anova

summary(forw)
anova(forw)

#both
both = stepAIC(fit, direction = "both")
both$anova

summary(both)
anova(both)

######back=both << 채택
backdata<-tdata[,c(1,2,3,5,6,7,11,12,23,24,29,30,36,38,40)]
fit <- lm(audiAcc~., data = backdata)
summary(fit)
vif(fit)

#audisat -
backdata<-backdata[,-c(5)]
outsatfit <- lm(audiAcc~., data = backdata)
summary(outsatfit)
vif(outsatfit)


###################이상치
outlierTest(outsatfit)

#395번 제거 
out395<-backdata[-c(395),]
rownames(out395) <-NULL
out395fit<-lm(audiAcc~., data=out395)
summary(out395fit)

out395back<-step(out395fit, direction = "backward", trace = T)
summary(out395back)
out395back$anova

outlierTest(out395back)
par(mfrow = c(2,2))
plot(out395back)

#25번 제거 
out25<-out395[-c(25),]
rownames(out25) <-NULL
out25fit<-lm(audiAcc~., data=out25)
summary(out25fit)

out25back<-step(out25fit, direction = "backward", trace = T)
summary(out25back)
out25back$anova

outlierTest(out25back)
par(mfrow = c(2,2))
plot(out25back)

#287번 제거 
out287<-out25[-c(287),]
rownames(out287) <-NULL
out287fit<-lm(audiAcc~., data=out287)
summary(out287fit)

out287back<-step(out287fit, direction = "backward", trace = T)
summary(out287back)
out287back$anova

outlierTest(out287back)
par(mfrow = c(2,2))
plot(out287back)


#427번 제거 
out427<-out287[-c(427),]
rownames(out427) <-NULL
out427fit<-lm(audiAcc~., data=out427)
summary(out427fit)

out427back<-step(out427fit, direction = "backward", trace = T)
summary(out427back)
out427back$anova

outlierTest(out427back)
par(mfrow = c(2,2))
plot(out427back)


#471번 제거 
out471<-out427[-c(471),]
rownames(out471) <-NULL
out471fit<-lm(audiAcc~., data=out471)
summary(out471fit)

out471back<-step(out471fit, direction = "backward", trace = T)
summary(out471back)
out471back$anova

outlierTest(out471back)
par(mfrow = c(2,2))
plot(out471back)

#3번 제거 
out3<-out471[-c(3),]
rownames(out3) <-NULL
out3fit<-lm(audiAcc~., data=out3)
summary(out3fit)

out3back<-step(out3fit, direction = "backward", trace = T)
summary(out3back)
out3back$anova

outlierTest(out3back)
par(mfrow = c(2,2))
plot(out3back)

#190번 제거 
out190<-out3[-c(190),]
rownames(out190) <-NULL
out190fit<-lm(audiAcc~., data=out190)
summary(out190fit)

out190back<-step(out190fit, direction = "backward", trace = T)
summary(out190back)
out190back$anova

outlierTest(out190back)
par(mfrow = c(2,2))
plot(out190back)

#369번 제거 
out369<-out190[-c(369),]
rownames(out369) <-NULL
out369fit<-lm(audiAcc~., data=out369)
summary(out369fit)

out369back<-step(out369fit, direction = "backward", trace = T)
summary(out369back)
out369back$anova

outlierTest(out369back)
par(mfrow = c(2,2))
plot(out369back)

#349번 제거 
out349<-out369[-c(349),]
rownames(out349) <-NULL
out349fit<-lm(audiAcc~., data=out349)
summary(out349fit)

out349back<-step(out349fit, direction = "backward", trace = T)
summary(out349back)
out349back$anova

outlierTest(out349back)
par(mfrow = c(2,2))
plot(out349back)

#16번 제거 
out16<-out349[-c(16),]
rownames(out16) <-NULL
out16fit<-lm(audiAcc~., data=out16)
summary(out16fit)

out16back<-step(out16fit, direction = "backward", trace = T)
summary(out16back)
out16back$anova

outlierTest(out16back)
par(mfrow = c(2,2))
plot(out16back)

#161번 제거 
out161<-out16[-c(161),]
rownames(out161) <-NULL
out161fit<-lm(audiAcc~., data=out161)
summary(out161fit)

out161back<-step(out161fit, direction = "backward", trace = T)
summary(out161back)
out161back$anova

outlierTest(out161back)
par(mfrow = c(2,2))
plot(out161back)

#682번 제거 
out682<-out161[-c(682),]
rownames(out682) <-NULL
out682fit<-lm(audiAcc~., data=out682)
summary(out682fit)

out682back<-step(out682fit, direction = "backward", trace = T)
summary(out682back)
out682back$anova

outlierTest(out682back)
par(mfrow = c(2,2))
plot(out682back)

##########################이상치 제거 끝
##최종 모델
lastfit<-step(out682fit, direction = "backward", trace = T)
summary(lastfit)

##잔차분석
install.packages('nortest')
library(nortest)

par(mfrow=c(2,2))
plot(lastfit)

pred<-predict(lastfit)
resid<-residuals(lastfit)

#등분산성
plot(pred,resid)
abline(c(0,0),col="red") #0을 기준으로 무작위 분포

#선형성
crPlots(lastfit)

#정규성
par(mfrow=c(1,2))
qqnorm((resid))
qqline((resid))



###################################
back_test=lm(audiAcc~actorscore+directorscore+news+firstscrnCnt+naverScoreA+naverScoreB, data=out682) #변수 그래프 그리기 위함. (범주형->수치형) 


relweights<-function(fit){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda^2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta^2)
  rawwgt <- lambdasq %*% beta ^2
  import <- (rawwgt/rsquare)*100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1,drop=FALSE]
  dotchart(import$Weights,labels=row.names(import),
           xlab="% of R-Square",pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=",round(rsquare,digits=3)))
  return(import)
}

result=relweights(back_test)

#변수의 상대적 중요성 plot_r^2 : 0.895
plotRelWeights=function(fit){
  data<-relweights(fit)
  data$Predictors<-rownames(data)
  p<-ggplot(data=data,aes(x=reorder(Predictors,Weights),y=Weights,fill=Predictors))+ 
    geom_bar(stat="identity",width=0.5)+
    ggtitle("Relative Importance of Predictor Variables")+
    ylab(paste0("% of R-square \n(Total R-Square=",attr(data,"R-square"),")"))+
    geom_text(aes(y=Weights-0.1,label=paste(round(Weights,1),"%")),hjust=1)+
    guides(fill=FALSE)+
    coord_flip()
  p
}

plotRelWeights(back_test)
summary(lastfit)


##################예측
predicted2019 = read.csv('C:/Users/yoonj/Desktop/regression/data/2019_predicted2.csv')

predicted2019<-predicted2019[,-c(9:11)]
trans2019 = predicted2019

trans2019$audiAcc = log(predicted2019$audiAcc)
trans2019$actorscore = log(predicted2019$actorscore)
trans2019$directorscore = log(predicted2019$directorscore)
trans2019$firstscrnCnt = sqrt(predicted2019$firstscrnCnt)
trans2019$news = log(predicted2019$news)

log_audiAcc = as.data.frame(predict(lastfit, trans2019))

names(log_audiAcc)[1] = c("predicted_audiAcc")


raw_audiAcc = trans2019[,c(1:2)]

result_reg = cbind(raw_audiAcc, log_audiAcc)












