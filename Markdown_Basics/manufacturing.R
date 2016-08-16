
# https://kbig.kr/edu_manual/html/car_update/basic/car_chapter_1.html

# 빅데이터 분석 : 제조편           

# 학습목표
# 자동차 부품의 생산데이터로서 공정변수들과 함께 생산된 oil gasket의 
# 탕구 두께가 추출되어 기록된 자료이다. 
# 기본적인 데이터 탐색과, 이를 바탕으로 생산품 품질에 주된 영향을 미치는 
# 공정변수 및 그 연관성 등을 파악하여 본다. 
# 또한, 불량품 탐지를 위한 모형들을 구성하여 보고 그 성능을 비교한다. 
# 여러 제품들 중 제품번호 45231-3B400에 해당하는 oil gasket에 
# 대한 분석을 실시한다. 
# 본 분석에서는 탕구두께에 영향을 줄 것으로 생각되는 공정변수로 
# 고정시간(fix_time), a속도(a_speed), b속도(b_speed), 
# 실압력(mpa), 하중시간(load_time), 고압시간(highpressure_time) 등 
# 6개의 변수를 고려하였다. 
# 탕구두께가 [21,27] 구간을 벗어나면 불량을 의심할 수 있는 것으로 보았다. (약 11%) 

# 필요패키지 및 필요데이터
# 필요 패키지 : ggplot2, psych, corrplot, MASS, car, leaps, 
# ROCR, tree, monmlp, randomForest, RSNNS, glmnet, caret

# 필요 데이터 : autoparts.csv


# 1.자료의 요약

# 자료를 불러들인다. 

car <- read.csv('./data/car_2015_data/autoparts.csv', header=T, fileEncoding="UTF-8")
head(car)
str(car)
car <- car[car$prod_no == "45231-3B400",]
car <- car[(car$c_thickness > 10)*(car$c_thickness < 50) ==1,]  

# read.csv() 함수를 이용하여 csv형태의 외부 데이터 화일을 읽어들임
# head(), str() 함수를 이용하여 데이터의 기본적인 형태 
# (변수의 개수 및 읽어들인 object의 특성 등)를 파악
# 탕구두께(c_thickness)중 자료의 범위를 크게 벗어나는 점들(10 미만, 50 초과)
# 이 탐지됨
# 이는 명백한 불량으로 볼 수 있으며 불량이 나타나는 프로세스 분석시 
# 의미있는 정보를 제공할 수도 있음
# 하지만, 통계적 모형화를 실시할 때 지나치게 큰 영향력을 발휘할 수 있다고 
# 판단되므로 제거하고 분석을 실시함

# 변수들 중 oil gasket의 탕구 두께와 관계되는 변수들의 특성을 파악하여 본다. 

# 1.1 변수들의 분포 파악

# 히스토그램을 이용하여 탕구 두께 및 그에 영향을 미칠 것으로 
# 생각되는 변수들에 대하여 시각화를 실행하여 보자. 

library(ggplot2)
library(psych)

# ggplot은 데이터 시각화를 위해 최근 가장 폭넓게 사용되는 패키지임
# psych은 자료의 요약을 위한 패키지 중 하나임. 
# describe() 함수의 사용을 위해 로딩함

ggplot(car, aes(x=fix_time)) + geom_histogram(, colour = "black")

# ggplot() 함수는 데이터 시각화를 위한 함수이며, 
# geom_histogram()과 함께 양적변수에 대한 히스토그램을 출력한다.
# 대부분이 80~90 사이에 몰려 있음을 알 수 있다.
# 하지만 일부 70 근처에서 형성되고 있는 값도 존재하며 
# 히스토그램 상으로는 잘 보이지 않지만 0 에 매우 가까운 값도 존재한다. 

ggplot(car, aes(x=a_speed)) + geom_histogram(, colour = "black")
ggplot(car, aes(x=b_speed)) + geom_histogram(, colour = "black")
ggplot(car, aes(x=mpa)) + geom_histogram(, colour = "black")
ggplot(car, aes(x=load_time)) + geom_histogram(, colour = "black")
ggplot(car, aes(x=highpressure_time)) + geom_histogram(, colour = "black")

# a속도, b속도, 실압력, 하중시간, 고압시간 등에 대해서도 위와 같이 
# 히스토그램을 출력해 보면, 값이 특정구간에 집중되는 경향이 있으나 
# 일부 자료들은 분포의 중심에서 떨어져 있다. 

describe(car$c_thickness)
ggplot(car, aes(x=c_thickness)) + geom_histogram(, fill="blue", colour = "black")


# describe() 함수는 자료의 개수, 중위수, 적률값 등 기초적인 
# 요약통계량을 제공해 줌
# 탕구두께에 대한 요약통계량 및 히스토그램을 출력하였다.
# 평균수준이 24.24로 유지되고 있으며, 히스토그램에서 비교적 두꺼운 
# 제품이 많이 발생하는 것을 볼 수 있다. 


# 1.2 공정변수의 영향력 분석 - 각 공정변수의 수준에 따른 탕구두께의 분포

# 공정변수들이 탕구두께에 미치는 영향을 파악하기 위해 간단한 시각화를 
# 통해 확인하여 본다.
# 변수들은 모두 연속형변수로 볼 수 있으며, 각 변수들을 임의로 
# 3개의 구간(low,middle,high = 1,2,3)으로 나누어 각 구간별로 탕구두께에 
# 대한 상자그림을 출력한다.
# 두께가 [21,27] 구간을 벗어나면 불량을 의심할 수 있다.
# 상자그림에 두께 21,27에 해당하는 지시선(reference line)을 그어 확인한다. 

car1 <- car[,c(8,9,10,14,15,16,17)]

# 탕구두께및 공정변수 6개만을 새로운 object로 저장

car1$fixtime_bin <- as.factor((car1$fix_time > 81.3) + (car1$fix_time > 81.1) + 1)

ggplot(car1, aes(x=fixtime_bin,c_thickness)) +
  geom_jitter(aes(colour = fixtime_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=fixtime_bin),outlier.colour = NA) + geom_hline(yintercept=c(21,27),color="red") 
##    + ggtitle("탕구두께 vs 고정시간")


car1$mpa_bin <- as.factor((car1$mpa > 76.4) + (car1$mpa > 75.4) + 1)
ggplot(car1, aes(x=mpa_bin,c_thickness)) + 
  geom_jitter(aes(colour = mpa_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=mpa_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") 
##  + ggtitle("탕구두께 vs 실압력")


car1$loadtime_bin <- as.factor((car1$load_time > 20.1) + (car1$load_time > 20.3) + 1)
ggplot(car1, aes(x=loadtime_bin,c_thickness)) + 
  geom_jitter(aes(colour = loadtime_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=loadtime_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") 
## + ggtitle("탕구두께 vs 하중시간")


car1$as_bin <- as.factor((car1$a_speed > 0.659) + (car1$a_speed > 0.667) + 1)
ggplot(car1, aes(x=as_bin,c_thickness)) + 
  geom_jitter(aes(colour = as_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=as_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") 
## + ggtitle("탕구두께 vs a속도")


car1$bs_bin <- as.factor((car1$b_speed > 1.632) + (car1$b_speed > 1.732) + 1)
ggplot(car1, aes(x=bs_bin,c_thickness)) + 
  geom_jitter(aes(colour = bs_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=bs_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red") 
## + ggtitle("탕구두께 vs b속도")


car1$hp_bin <- as.factor((car1$highpressure_time > 72) + (car1$highpressure_time > 69) + 1)
ggplot(car1, aes(x=hp_bin,c_thickness)) + 
  geom_jitter(aes(colour = hp_bin), position = position_jitter(width = .3), alpha = 0.5) + 
  geom_boxplot(aes(fill=hp_bin),outlier.colour = NA) + 
  geom_hline(yintercept=c(21,27),color="red")
## + ggtitle("탕구두께 vs 고압시간")


# geom_boxplot()은 상자그림을 출력해 주며, 
# geom_hline()은 출력된 그림 위에 수평 지시선을 그어줌
# geom_jitter()는 상자 바깥에 있는 점들을 겹치지 않게 흩뿌려서, 점들의 밀도를 보다 
# 시각적으로 쉽게 파악할 수 있도록 해 주는 함수임
# 이상점은 따로 구별하여 표시하지 않음(outlier.colour = NA)
# as.factor()를 이용하여 각 변수들을 3개의 범주를 가지는 범주형 변수로 변환
# ggtitle()로 그림의 main title을 설정

# 지시선을 벗어나는 점들이 다수 눈에 띄는 것으로 보아 
# 불량으로 의심할 수 있는 제품들이 많다.
# 고정시간이 보통 수준보다 길면, 탕구두께의 변동도 
# 커진다는 것을 확인할 수 있다.
# 이외의 변수에 대해서도 같은 방식으로 그림을 
#  그려 경향성을 확인할 수 있다.
# 가장 눈에 띄는 현상은 하중시간에 따른 두께의 변화이다.
# 하중시간이 길수록 두께가 전반적으로 얇아지는 경향이 보이며,
# 특히 하중시간이 가장 높은 수준에 속할 때는 두께의 변동이 
# 매우 크게 증가하는 것으로 보인다. 

#  1.3 상관분석

# 탕구두께 및 공정변수들간의 상관계수를 계산하여 선형적 연관성의 정도를 파악한다. 

car1 <- car[,c(8,9,10,14,15,16,17)]

library(corrplot)
corrplot.mixed(cor(car1),  upper = "ellipse" ,lower="number",tl.pos="lt",bg="gray")


# corrplot은 변수들간의 상관계수를 계산하여 다양한 형태의 그림으로 
# 시각화 시킬 수 있는 패키지임
# corrplot.mixed()를 이용하여 상관계수 뿐 아니라 상관계수의 크기를 
# 타원의 모양과 색깔로 시각화
# 타원의 모양이 원에 가까울 수록 약한 상관, 직선의 형태에 가까울 수록 강한 상관관계를 나타내며, 
# 색깔이 진할수록 상관관계가 강함
# 그림의 위쪽은 타원의 모양 및 색깔의 진하기로 상관의 정도를 표현하고, 
# 대각 아래쪽은 숫자로 표시함
# 실압력과 a속도간의 상관관계가 가장 강하게 나타난다.
# 탕구두께와 가장 강한 선형적 연관성을 가지는 변수는 a속도와 
# 실압력으로 파악되며 둘 모두 음의 상관이다.
# 즉, a속도나 실압력이 커질 수록 탕구두께는 얇아지는 경향성이 있는 것으로 
# 파악된다. 





# 2. 선형회귀모형 (Linear model)

# 2.1 선형회귀모형의 적합 및 변수선택

# 변수들간에 어느정도 연관성이 존재하는 것으로 파악되므로 선형회귀모형을 적합하여 본다. 


lm1 <- lm(c_thickness~.,data=car1)
summary(lm1)


# lm()은 선형회귀모형을 적합하여 주는 함수
# 적합된 회귀모형을 새로운 object로 저장후 
# summary함수를 이용하면 적합된 모형에서의 회귀계수 추정치와 
# 표준오차, 각 변수별 유의성 검정결과인 p-value를 확인할 수 있음
# 모든 변수가 유의수준 0.05에서 유의함


library(MASS)
library(leaps)

vs1 <- regsubsets(c_thickness~.,data=car1)
summary(vs1)
par(mfrow=c(1,3))
plot(vs1,scale="Cp")
plot(vs1,scale="adjr2")
plot(vs1,scale="bic")


# leaps는 regression subset selection 패키지임. 여기서는 regsubsets()함수를 위해 로딩
# regsubsets() 함수는 best subset selection을 수행하며, 모형의 크기(변수가 포함되는 개수) 순으로 가장 좋은 모형을 출력함
# plot()함수에 의해 AIC, 수정결정계수, Cp 통계량 등을 기준으로 한 최적의 모형 및 각 변수의 포함여부를 시각화


# 변수 1개만 모형에 포함하는 경우에는 실압력(mpa)이 선택된다.
# 즉, 가장 영향력이 큰 공정변수로 볼 수 있다.
# 두 개의 변수만 포함하는 경우에는 실압력 및 b속도(b_speed)가 포함된다.
# 이는, 탕구두께와 개별적인 상관계수를 계산했을 때(실압력과 a속도가 가장 큰 상관관계를 가짐) 와는 약간 다른 결과이다.
# 최적의 모형은 변수 6개를 모두 고려하는 것이다.
# Cp, 수정결정계수, bic를 기준으로 했을 때 모두 같은 결과를 준다. 


library(car)

par(mfrow=c(2,2))
vif(lm1)

# car는 Companion to Applied Regression의 약자로, 
# 회귀분석과 연관된 다양한 함수를 제공하는 패키지임
# 여기서는, vif() 함수를 사용하기 위해서 로딩
# vif()는 분산팽창계수를 출력해 주는 함수임
# 분산팽창계수가 전반적으로 4를 넘지 않으므로 다중공선성이 심각하게 
# 존재하지 않는 것으로 볼 수 있다.
# (보통 분산팽창계수가 10을 넘으면 심각한 것으로 보고 4~5를 넘으면 
# 의심해 볼 수 있는 것으로 알려져 있음) 



# 2.2 축소추정법

# 본 단원에서는 위에서 적용했던 선형모형에 축수추정법을 
# 적용해 볼 것이다.
# 이미 위 절에서 모든 변수들이 유의미한 것으로 판명되었고 다중공선성도 크게 의심할만 하지 않아 
# 필수적인 절차라고는 할 수 없으나, 사실의 확인 및 연습을 위해 소개한다. 

library(glmnet)

set.seed(1)
ind.train <- sample(1:nrow(car1),nrow(car1)*0.7)

car.train <- car1[ind.train,]
car.test <- car1[-ind.train,]
X <- as.matrix(car.train[,1:6])
Y <- as.matrix(car.train[,7])

nX <- as.matrix(car.test[,1:6])
nY <- as.matrix(car.test[,7])


# glmnet은 축소추정에 의한 선형모형을 적합할 수 있는 패키지임
# set.seed()는 모의실험의 동일 결과재현을 위해 seed number를 설정하는 함수임
# 자료를 7:3의 비율로 훈련(training)자료와 검증(test)자료로 분할함
# 훈련자료는 모형의 적합을 위해, 검증자료는 예측오차(prediction error)의 계산을 위해 사용됨

# 2.2.1 능형(Ridge)회귀

cv.ridge <- cv.glmnet(X,Y,alpha=0,lambda=10^seq(10,-2,length=100))
par(mfrow=c(1,1))
plot(cv.ridge)

ridge.pred <- predict(glmnet(X,Y,alpha=0,lambda=cv.ridge$lambda.min),newx=nX)
coef(glmnet(X,Y,alpha=0,lambda=cv.ridge$lambda.min))
mean((nY - ridge.pred)^2)


# cv.glmnet()은 교차검증(cross-validation)을 수행해줌
# Ridge 모형적합을 위해alpha=0으로 설정
# plot()함수에 의해 조절모수(로그변환)에 따른 평균제곱오차를 시각화
# cv에 의해 선택된 조절모수값에 대한 모형을 적합하고 예측오차를 계산함
# 그림에서 볼 수 있듯이 조절모수의 값은 작은 쪽에서 형성됨
# 실제 선택된 값은 0.0534, 예측오차는 2.7310임


# LASSO

cv.lasso <- cv.glmnet(X, Y, alpha=1, lambda=10^seq(10, -2, length=100))
plot(cv.lasso)


lasso.pred <- predict(glmnet(X,Y,alpha=1,lambda=cv.lasso$lambda.min),newx=nX)
coef(glmnet(X,Y,alpha=1,lambda=cv.lasso$lambda.min))
mean((nY - lasso.pred)^2)


# LASSO 모형적합을 위해alpha=1으로 설정
# cv에 의해 선택된 조절모수값에 대한 모형을 적합하고 예측오차를 계산함
# 그림에서 볼 수 있듯이 조절모수의 값은 작은 쪽에서 형성된다.
# 실제 선택된 값은 0.01로 현재 설정한 값들 중 최소에 해당하는 값이다.
# 추정된 값중 0은 없다. 즉, 모형에서 빠지는 변수는 없다.
# 예측오차는 2.7567이다.

lm.pred <- predict(lm(c_thickness~.,data=car.train),newdata=car.test[,1:6])
mean((car.test[,7] -lm.pred)^2)

# 선형회귀모형(축소추정법 X)에 의한 예측오차도 계산
# 축소추정법을 적용하지 않은 선형회귀모형의 경우 예측오차가 2.7276이다.
# 즉, Ridge(2.7310)나 LASSO(2.7567)에 비해 작은 값을 주었다.
# 앞에서 모두 변수가 유의하였던 점, 축소추정에 의해 변수가 모형에서 빠지지 않았던 점,
# 축소추정에 의해서 예측오차가 좋아지지 않았던 점,
# 분산팽창계수가 그리 크지 않았던 점 등을 종합하여 볼 때,
# 본 자료에서는 축소추정법이 필요하지 않은 것으로 보인다. 



# 3. 분류모형 (Classification) - 불량 vs 양품 분석

# 탕구두께가 [21,27] 구간에 속하는지 여부를 반응변수(불량품or양품)로 하여 다른 공정변수들이 
# 어떤 영향을 미치는지를 여러 분류모형을 고려한 후,
# 오분류율, ROC curve, KS(kolmogorov-smirnov)통계량 등을 
# 비교하여 모형의 성능을 비교해 본다.

car1$failure <- as.factor((car1$c_thickness > 27)+(car1$c_thickness < 21))
car2 <- car1[,-7]
car2.train <- car2[ind.train,]
car2.test <- car2[-ind.train,]


# 위와 같이 불량이 의심되는 경우 1, 그렇지 않은 경우 0의 값을 가지도록 변수를 재설정하고 기존 변수는 삭제
# 모형의 성능평가를 위해 훈련자료와 검증자료로 분류



# 3.1 로지스틱(Logistic) 회귀모형

# 로지스틱 회귀모형은 선형모형의 확장된 형태이며,
# 본 예에서는 불량발생확률의 logit 변환을 공정변수들의 선형결합으로 모형화한 것이다. 

lm2 <- glm(failure~.,data=car2.train, family=binomial)
vs3 <- stepAIC(lm2,direction="both")
vs3$anova

# AIC에 의한 변수선택결과 a속도를 제외한 모형이 최적으로 판명된다. 


library(ROCR)

fit.log <- predict.glm(vs3, newdata=car2.test,type="response")

pred.log <- prediction(fit.log, car2.test$failure)
perf.log <- performance(pred.log, "tpr", "fpr")


# ROCR 은 분류기의 성능을 시각화하기 위한 패키지임
# predict.glm()에 의해 적합된 로지스틱 모형에서 예측값(예측확률:불량이 일어날 확률)을 계산
# performace()함수에 의해 tpr(True positive rate:민감도)와 fpr(false positive rate:1-특이도)를 각각 계산

plot(perf.log, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0, 1, by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)

auc <- performance(pred.log,"auc")


library(caret)

confusionMatrix(1*(fit.log>0.5), car2.test$failure)


# ROC(Receiver Operating Characteristic) 곡선을 그림
# ROC 곡선은 cutoff 값의 변화에 따른 fpr과 tpr의 관계를 좌표평면상에 연결하여 나타낸 것으로 
# 분류기의 성능을 평가하기 위해 쓰이는 대표적인 방법 중 하나임
# 곡선의 아래 면적 (AUC:Area Under Curve)가 1에 가까울 수록 우수한 성능을 보임. 
# 즉, 분류가 잘 된 것으로 볼 수 있음
# performance() 함수에서 옵션을 auc로 설정하여 AUC값을 계산함
# 분류행렬을 출력하고 오분류율을 계산
# 로지스틱 모형에 의한 오분류율은 0.0777이다.
# 양품은 양품으로 대부분 잘 분류하였으나, 불량의심제품에 대한 분류성능이 약간 떨어지는 것으로 보인다.
# AUC값은 0.8095이다.


Label0.log <- fit.log[car2.test$failure==0]
Label1.log <- fit.log[car2.test$failure==1]


plot(ecdf(Label0.log), col="red",main="로지스틱")

plot(ecdf(Label1.log), col="blue",add=T)


ks.test(Label0.log, Label1.log)

# 양품과 불량의심제품의 두 그룹으로 나누어 각각 불량예측확률을 저장
# ecdf()함수를 이용하여 각 그룹의 예측확률의 분포함수를 추정 (Empirical distribution function)
#  ks.test()함수를 이용하여 KS(Kolmogorov-Smirnov) 통계량을 계산함
# KS 통계량 값이 클 수록 두 그룹이 잘 분리되었다는 의미이므로 분류기의 성능이 좋은 것으로 볼 수 있다.
# 여기서 KS 통계량 값은 0.537이다.
# 양품은 0 근처에 예측확률이 집중되어 있으나, 불량의심품은 0근처와 1근처에 고르게 예측확률이 분포한다.
# 즉, 불량의심품의 분류에 어려움을 겪는다는 뜻으로 이는, 분류행렬에서 확인했던 바와 일치한다.



# 3.2 의사결정나무 (Decision tree)

# 로지스틱 모형과 달리 비선형모형이다.
# 현 자료의 경우 공정변수들의 값이 이원화되어 있는 경우가 많았으므로 
# 로지스틱 모형보다는 의사결정나무 모형에 의한 분류성능이 더 좋을 것으로 기대해 볼 수 있다.


library(tree)

tree.car <- tree(failure~., data=car2.train)
summary(tree.car)
plot(tree.car)
text(tree.car)

# tree()를 이용하여 의사결정나무를 적합하고 결과를 그림
# text()는 분류규칙을 나무 위에 표시
# 의사결정나무 적합결과 실제 나무적합을 위해 쓰인 변수는 
# 실압력, b속도, 고압시간, 하중시간, 고정시간이다.
# a속도는 사용되지 않았다.
# 보통 큰 나무모형은 과적합(overfitting) 문제가 있는 것으로 알려져 있다.
# 적절한 가지치기(pruning)를 통해 나무의 크기를 줄여 예측 정확도를 향상시켜 볼 것이다. 

set.seed(3)
cv.car <- cv.tree(tree.car, FUN=prune.misclass)
cv.car
plot(cv.car$size,cv.car$dev, type="b")

# 가지치기를 위해 cross-validation에 의해 deviance를 최소로 하는 terminal node의 개수를 찾음
# 여기서 deviance는 평균제곱오차(MSE)의 확장된 개념으로 일종의 오차임
# 크기가 5인 경우 deviance가 최소가 됨. 따라서, 크기가 5인 모형을 선택함

prune.car <- prune.misclass(tree.car, best=5)
prune.car

plot(prune.car)
text(prune.car)


# 가지치기하여 단순화된 나무를 확인할 수 있으며, 분류를 위해 쓰인 변수는 실압력, 하중시간, b속도 세 개이다. 

fit.tree <- predict(prune.car, newdata=car2.test,type="vector")
pred.tree <- prediction(fit.tree[,2], car2.test$failure)
perf.tree <- performance(pred.tree, "tpr", "fpr")

plot(perf.tree, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1, text.adj=c(-0.5, 0.5), lwd=2)


perf.tree1 <- performance(pred.tree, "auc")
perf.tree1
confusionMatrix(1*(fit.tree[,2]>0.5), car2.test$failure)

# 오분류율은 0.067이다.
# 로지스틱 모형에 비해서, 불량의심품을 분류해 내는 성능이 
# 다소 개선되었음을 확인할 수 있다.
# AUC는 0.7718로 로지스틱 모형에 비해 작다.
# 즉, 양품과 불량의심품을 구분해 내는 데에는 상대적으로 성공적이었으나 
# 전체적인 분류성능이 매우 좋다고 할 수는 없다. 


Label0.tree <- fit.tree[,2][car2.test$failure==0]
Label1.tree <- fit.tree[,2][car2.test$failure==1]

plot(ecdf(Label0.tree),col="red",main="의사결정나무")
plot(ecdf(Label1.tree),col="blue",add=T)

ks.test(Label0.tree,Label1.tree)

# KS 통계량 값은 0.5353으로 로지스틱 모형과 큰 차이가 없다. 
# b속도는 커질 수록 불량으로 판명될 가능성이 전반적으로 크다 할 수 있다. 



# 3.3 앙상블(Ensemble) 모형

# 의사결정나무모형은 여러 장점을 가지고 있으나 분산이 크다는 단점이 있다.
# 이를 보완하기 위한 앙상블(Ensemble) 모형을 고려한다. 

# 3.3.1. Bagging (Bootstrap aggregating)

# B개의 부스트랩 표본에 대해 나무를 구성하고 종합하는 방식이다. 


library(randomForest)
bag.car <- randomForest(failure~., data=car2.train, mtry=6)
bag.car

# randomForest : 앙상블모형을 적합하는 패키지
# mtry=6, 즉 모든 변수를 개별나무모형 생성에 포함하여 Bagging을 수행

fit.bag <- predict(bag.car, newdata=car2.test,type="prob")

pred.bag <- prediction(fit.bag[,2], car2.test$failure)
perf.bag <- performance(pred.bag, "tpr", "fpr")

plot(perf.bag, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)


perf.bag1 <- performance(pred.bag,"auc")
perf.bag1
confusionMatrix(1*(fit.bag[,2]>0.5), car2.test$failure)


Label0.bag <- fit.bag[,2][car2.test$failure==0]
Label1.bag <- fit.bag[,2][car2.test$failure==1]

plot(ecdf(Label0.bag),col="red",main="의사결정나무")
plot(ecdf(Label1.bag),col="blue",add=T)

ks.test(Label0.bag,Label1.bag)


#  KS 통계량 또한 0.7025로 0.5353이었던 의사결정나무모형에 
# 비해 크게 좋아졌다. 



# 3.3.2 랜덤포레스트 (Random forest)

# 나무 생성시 모든 변수를 사용하지 않고 랜덤하게 일부분의 변수만을 선택하여 
# 사용한다는 점이 Bagging과 다르다. 

library(randomForest)

rf.car <- randomForest(failure~.,data=car2.train,mtry=3)
rf.car

# mtry=3, 6개의 변수 중 3개의 변수만 개별나무 생성에 기여하도록 설정

fit.rf <- predict(rf.car,newdata=car2.test,type="prob")
pred.rf <- prediction(fit.rf[,2], car2.test$failure)
perf.rf <- performance(pred.rf,"tpr","fpr")
plot(perf.rf, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)


perf.rf1 <- performance(pred.rf,"auc")
perf.rf1
confusionMatrix(1*(fit.rf[,2]>0.5),car2.test$failure)

# 오분류율은 0.0631로 bagging과 동일하였다.
# AUC는 0.9110으로 bagging(0.9034)에 비해 소폭 상승하였다. 


Label0.rf <- fit.rf[,2][car2.test$failure==0]
Label1.rf <- fit.rf[,2][car2.test$failure==1]

plot(ecdf(Label0.rf),col="red",main="랜덤포레스트")
plot(ecdf(Label1.rf),col="blue",add=T)


ks.test(Label0.rf,Label1.rf)

# KS 통계량 또한 0.7152로 bagging에 비해 소폭 상승하여, 
# 더 분류가 잘 되었다고 할 수 있다. 



# 3.4 신경망모형 (Neural network model)

# 신경망모형은 생물학의 신경망을 본따 만든 학습 알고리즘으로 비선형 모형이다.
# 굉장히 다양한 모형이 존재하지만 여기서는 가장 잘 알려져 있는 것 중 하나인 
# 다층퍼셉트론(Multilayer perceptron) 모형을 이용한 적합을 소개한다.
# 신경망모형의 적합을 위해서는 설명변수들을 표준화(Normalization) 하는 것이 좋은 것으로 알려져 있다. 

library(RSNNS)

Y <- decodeClassLabels(car2.train$failure)
X <- normalizeData(car2.train[,-7])

test.Y <- decodeClassLabels(car2.test$failure)
test.X <- normalizeData(car2.test[,-7])


# RSNNS : 신경망모형 적합을 위한 패키지
# normalizeData()를 이용하여 표준화
# 훈련자료와 검증자료 분류

nn.car <- mlp(X,Y,size=5,maxit=50)


fit.nn <- predict(nn.car,test.X)

pred.nn <- prediction(fit.nn[,2], car2.test$failure)
perf.nn <- performance(pred.nn,"tpr","fpr")
plot(perf.nn, colorize=T, colorkey.pos="top", print.cutoffs.at=seq(0,1,by=0.2), text.cex=1,text.adj=c(-0.5, 0.5), lwd=2)


perf.nn1 <- performance(pred.nn,"auc")
perf.nn1

library(caret)
confusionMatrix(1*(fit.nn[,2]>0.5),car2.test$failure)

# 은닉층의 개수를 5개로 설정 (size=5)
# 학습을 위한 최대 반복수를 50으로 설정
# 오분류율이 0.0696으로 로지스틱모형에 비해서는 좋으나 다른 모형들에 비해서는 다소 떨어진다.
# AUC가 0.8394으로 로지스틱 및 의사결정나무에 비해서 큰 값을 가진다.

Label0.nn <- fit.nn[,2][car2.test$failure==0]
Label1.nn <- fit.nn[,2][car2.test$failure==1]

plot(ecdf(Label0.nn),col="red",main="신경망모형")
plot(ecdf(Label1.nn),col="blue",add=T)


ks.test(Label0.nn,Label1.nn)

# KS 통계량이 0.6063으로 로지스틱과 의사결정나무에 비해서 크며, 
# 앙상블모형에 비해서는 작다. 


# 3.5 모형성능비교

par(mfrow=c(2,2))
boxplot(Label0.log,Label1.log,main="로지스틱")
boxplot(Label0.tree,Label1.tree,main="의사결정나무")
boxplot(Label0.rf,Label1.rf,main="랜덤포레스트")
boxplot(Label0.nn,Label1.nn,main="신경망모형")


# 위 그림은 양품(좌), 불량의심품(우) 그룹에서 각 모형에 의한 
# 예측확률을 상자그림으로 표현한 것이다.
# 즉, 좌측 상자는 0에 가깝고 우측상자는 1에 가까울 수록, 
# 또한 두 상자의 폭이 좁고 서로 멀리 떨어져 있을수록 분류가 잘 된 것으로 볼 수 있다.
# 약간씩의 차이가 있기는 하지만 양품은 불량확률을 0에 매우 가깝게 대부분 예측하고 있음을 알 수 있다.
# 다만, 양품을 양품으로 가장 잘 예측하는 것은 
# 의사결정나무모형인 것으로 보인다.
# 불량의심품의 경우에는 모든 분류모형이 저하된 성능을 보였다.
# 상대적으로는 로지스틱이 가장 좋지 않은 성능을 보이고 있으며, 
# 랜덤포레스트(Random Forest)가 가장 좋은 성능을 보여주고 있다.
# 앞서 보았듯이 오분류율, AUC, KS통계량의 관점에서도 비슷한 결론을 내릴 수 있었다. 



# https://www.ohio.edu/plantbio/staff/mccarthy/quantmet/lectures/Nonparm.pdf

# Nonparametric Two-Sample Tests

# Nonparametric Tests
# Recall, nonparametric tests are considered 
# “distribution-free” methods because they do not rely 
# on any underlying mathematical distribution.

# They do, however, have various assumptions that 
# must be met. 

# Do not be confused by not having the need to meet 
# an assumption of “normality” with the notion of 
# “assumptionless.”



# Sign Test
# Consider a simple example where 20 farmers are 
# given two fertilizers (A & B) by an extension agent 
# and asked to evaluate which one was “better”.
# In the end, 16 farmers reported that fertilizer A was 
# the better of the two (based on qualitative 
#  estimates of visual quality, greenness, yield, etc.).

#  If there were no difference between fertilizers, we 
# would expect fertilizer A to be binomially distributed 
# with P = 0.05 and N = 20

# To estimate our rejection region:
pbinom(16, size=20, prob=0.5)

# Intuitively, we would expect to subtract 1.0 to get the other 
# tail, but we must actually adjust for the fact that one tail is 16 
# or better and for counts, the other tail must be 15 or fewer:
1-pbinom(15,size=20,prob=0.5)

# If you wish a 2-tailed test, you need to add the probabilities 
# in each direction:

1-pbinom(15,20,0.5)+pbinom(4,20,0.5)


# Sign Test
# This is a bit of a confusing test to start out 
# with (we do so only because it is one of the oldest statistical tests*) 
# because we are assuming a binomialdistribution.
# If we are assuming a distribution, then are we not 
# doing a parametric test? Yes. BUT, in this case the 
# parametric binomial distribution and the C distribution 
# of the sign test are identical. You are only ever 
# working with two outcomes: A/B, dead/alive, 0/1, +/-, 
# etc. The latter is where the “sign test” originated from


# Mann-Whitney U-Test
# This is the nonparametric analog to the two-sample 
# t-test with equal variances.
# It is used primarily when the data have not met the 
# assumption of normality (or should be used when there is 
# sufficient doubt).

# Assumptions:
  # Independent samples
  # Continuous variable
  # Equal variances
  # Identical (non-normal) distributions


# Mann-Whitney U-Test
# This test is based on ranks.
# It has good efficiency, especially for symmetric distributions. 
# There are exact procedures for this test given small 
# samples with no ties, and there are large sample 
# approximations

# The Mann-Whitney test statistic, U, is defined as the 
# total number of times a Y1 precedes a Y2 in the 
# configuration of combined samples


# Mann-Whitney U -Test
# - Procedure -

# 1. Pool data together, sort data in ascending order, 
#    keep track of sample ID
# 2. Convert data to ranks (1, 2, 3,... Y)
# 3. Separate ranks back in to two samples
# 4. Compute the test statistic, U
# 5. Determine critical value of U from table
# 6. Formulate decision and conclusion


# Mann-Whitney U-Test
# - Example -
# Suppose you wished to determine if 
# there was a difference in the biomass 
# of male and female Juniper trees.
# Thus,  Ho: B_{male} = B_{female} (medians are equal)
#        Ha: B_{male} ≠ B_{female} (medians not equal)

# You randomly select 6 individuals of each gender 
# from the field, dry them to constant moisture, 
# chip them, and then weigh them to the nearest kg.

#Raw Data:
# Male   74 77 78 75 72 71
# Fem    80 83 73 84 82 79

# Preliminary analysis shows data to have  equal variances, but 
# normality tests are questionable given  small sample sizes.
# Mann-Whitney U-test is most appropriate...


# Mann-Whitney U-Test
# - Example -
# Raw Data:
# Male   74* 77* 78* 75* 72* 71*
# Fem    80 83 73 84 82 79

# Order & Rank, ID Sample by *:
# 71* 72* 73 74* 75* 77* 78* 79 80 82 83 84
# 1*  2*  3  4*  5*  6*  7*  8  9  10 11 12

# Sum the Ranks:
# Male: 1 + 2 + 4 + 5 + 6 + 7 = 25
# Fem:  3 + 8 + 9 + 10 + 11 + 12 = 53


# Mann-Whitney U-Test
# - Example, cont. -
# Compute test statistic using rank data.
# First calculate C, then compare to (n1n2 - C)
# Larger of two values becomes U statistic.

# C = n1n2 + n2*(n2+1)/2 - sum(R_i)
# n1 = N of larger sample
# n2 = N of smaller sample
# R = Ranks of smaller  sample

# In our example,
# C = (6)(6) + (6)(7)/2 - 25 = 32
# n1n2 - C = (6)(6) - 32 = 4
# Larger value becomes U_{calc} = 32
# U_{6,6, .025} = U_{table} = 31
# U_{calc} > U_{table}, therefore reject H0


# Mann-Whitney U-Test
# The only real hitch that can 
# arise with this test is when two (or more) ranks are tied. 
# When this happens, sum the ranks and give each the mean 
# rank value. 
# Example: 24, 24 tied at 3, 4 then 3.5, 3.5
# Note also that at N > 20,
# U begins to approximate t,
# so the test statistic 
# changes to a t-value:


# Mann-Whitney U-Test
# - Using R -
male<-c(74,77,78,75,72,71)
female<-c(80,83,73,84,82,79)

wilcox.test(male,female) #NB: = MWU

# What would have happened if one had “mis-applied” the 
# t-test instead of using the Mann-Whitney U-test?
# Both samples would pass a normality test 
# (but would be  questionable given the small sample size) and both 
# would pass a homogeneity of variance test. The result 
# will be the same, but note the difference in P-value.

t.test(male,female,var.equal=TRUE)


# Kolmogorov-Smirnov Test
# This is the nonparametric analog to the two-sample t-test 
# with unequal variances.
# It is often used  when the data have not met either
# the assumption of normality or the assumption of equal variances.
# Assumptions:
  # Variable at least ordinal
  # Two samples are independent
  # Both simple random samples 
  # Identical distributions


# Kolmogorov-Smirnov Test
# This test has poor statistical efficiency.
# Many nonparm stats are based on ranks and therefore 
# measure differences in location.  The K-S examines a 
# single maximum difference between two distributions.
# If a statistical difference is found between the 
# distributions of X and Y, the test provides no insight as to 
# what caused the difference. The difference could be due 
# to differences in location (mean), variation (standard deviation), 
# presence of outliers, type of skewness, type of kurtosis, 
# number of modes, and so on. 


# Kolmogorov-Smirnov Test
# - Procedure -
# Note that the hypotheses for K-S are NOT rooted in a 
# mean or median (measures of central tendency).

# The null and alternative hypotheses  for the K-S test 
# relate to the equality of the two distribution functions 
# [usually noted as F(X) or F(Y)].

# Thus, the typical two-tailed hypothesis becomes:
# Ho: F(X) = F(Y)
# Ha: F(X) ≠ F(Y)


# Kolmogorov-Smirnov Test
# - Procedure -
# 1. Find X_{min} and X_{max} for 2 samples and lay out a      
#    column of class categories.
# 2. List the cumulative frequencies of the two samples in 
#    respective columns.
# 3. Determine relative expected frequencies by dividing 
#    by sample sizes.
# 4. Determine the absolute differences (d) between 
#    relative expected frequencies.
# 5. Identify largest d, becomes D_{max}
# 6. Multiply D_{max} by n1n2  (calc test value).
# 7. Compare D_{max}n1n2 with critical value in table.


# Kolmogorov-Smirnov Test
# - Example: Juniper Data -
#  Y   Male   Female   M/n1     F/n2     d
# 71   1      0        0.166    0.000    0.166
# 72   2      0        0.333    0.000    0.333
# 73   2      1        0.333    0.166    0.167
# 74   3      1        0.500    0.166    0.334
# 75   4      1        0.666    0.166    0.500
# 76   4      1        0.666    0.166    0.500
# 77   5      1        0.833    0.166    0.667
# 78   6      1        1.000    0.166    0.834
# 79   6      2        1.000    0.333    0.667
# 80   6      3        1.000    0.500    0.500
# 81   6      3        1.000    0.500    0.500
# 82   6      4        1.000    0.666    0.334
# 83   6      5        1.000    0.833    0.167
# 84   6      6        1.000    1.000    0.000


# Kolmogorov-Smirnov Cumulative Expected Frequencies Distribution 
# Plot Note what D_{max} is evaluating.

# Kolmogorov-Smirnov  Test
# - Example -
# In this example, the largest difference is D_{max} = 0.834
# D_{calc} = D_{max}(n1)(n2) = 0.834 (6) (6) = 30.02
# D_{table} = 30 at n1 = 6, n2 = 6, P = 0.05 
# D_{calc} > D_{table} therefore, reject Ho  (barely)

# NB: decision was closer than MWU Test)


# Kolmogorov-Smirnov Test
# - Using R -
ks.test(male, female)

# NB: this P-value needs to be multiplied by 2 for a 2-tail test.
# Thus, P = 0.05194 ([exact] same as hand-worked example).


# Comparison
# Note that we used a constant data set for a reason 
# (only one of these three tests was the “appropriate” test to use).
# The consequence of using the incorrect test is an incorrect
# P-value,which is connected to power of test, and ultimately your 
# conclusion. The consequences here were minimal but could be profound.

# Kolmogorov-Smirnov:  P = 0.05194
# Mann-Whitney U-test: P = 0.02597
# T-test: P = 0.01645

