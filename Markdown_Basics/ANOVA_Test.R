
# 일원분산분석(one-way ANOVA) : aov()
# http://rfriend.tistory.com/131

# 2개의 모집단에 대한 평균을 비교, 분석하는 통계적 기법으로 
# t-Test를 활용하였다면, 비교하고자 하는 집단이 3개 이상일 경우에는 
# 분산분석 (ANOVA : Analysis Of Variance)를 이용합니다. 

# 설명변수는 범주형 자료(categorical data)이어야 하며, 
# 종속변수는 연속형 자료(continuous data) 일 때 3개 이상 집단 간 
# 평균 비교분석에 분산분석(ANOVA) 을 사용하게 됩니다.

# 분산분석(ANOVA)은 기본적으로 분산의 개념을 이용하여 분석하는 방법으로서, 
# 분산을 계산할 때처럼 편차의 각각의 제곱합을 해당 자유도로 나누어서 
# 얻게 되는 값을 이용하여 수준평균들간의 차이가 존재하는 지를 판단하게 됩니다.  

# 분산분석 (ANOVA)은 영국의 통계학자 피셔(R.A. Fisher)가 
# 고안한 분석 기법으로서, 최초에는 농사실험과 관련된 연구에서 
# 사용되었으나 요즘에는 사회과학, 공학, 의학 등 폭넓게 적용되고 있습니다. 

# 3개 이상의 집단에 대한 비교 시 모집단이 정규분포를 따르지 않을 경우에는 
# 비모수 추정과 검정 기법을 이용해야 하며, 
# 이에 대해서는 추후에 별도로 다루도록 하겠습니다.

# 분산분석은 측정하고자 하는 값에 영향을 미치는 요인(Factor)의 수에 
# 따라서 구분하는데요, 가령, 작업자(Worker)에 따라서 
# 생산량(Volume of Production)의 차이가 있는가를 비교 분석한다고 했을 때 
# '작업자(Worker)'가 바로 요인(Factor)이 되겠습니다. 
# 그리고 작업자 3명 '홍길동', '김철희', '최영희'를 요인 수준 (Factor Level) 
# 혹은 처리 (treatment) 라고 합니다.

# 측정값에 영향을 미치는 요인(Factor)이 1 개인 실험은 '일원분산분석' 
# (one-way ANOVA) 라고 하며, 측정값에 영향을 미치는 요인(Factor)이 
# 2 개인 실험은 '이원분산분석' (two-way ANOVA) 라고 부릅니다.


# [ ANOVA Model and Description in R ]

# n-way ANOVA     Model        Description

# one-way ANOVA   y ~ x1       y is explained by x1 only

# two-way ANOVA   y ~ x1 + x2  y is explained by x1 and x2

# two-way ANOVA   y ~ x1 * x2  y is explained by x1, x2 and the interaction between them 

# three-way ANOVA y ~ x1 + x2 + x3   y is explained by x1, x2 and x3


# 이번 포스팅에서는 요인(Factor)이 1개인 일원분산분석(one-way ANOVA)의 이론과 
# R의 aov() 함수의 사용법에 대해서 설명해보겠습니다.

# 요인(Factor)이 1개이면서 r개의 요인 수준(Factor Level)을 가지고 있고, 
# 각 수준에서 박복 측정수가 n개인 일원분산분석(one-way ANOVA)는 
# 다음과 같은 형태의 자료 형태와 모형을 가지게 됩니다. 


# 분산분석(ANOVA)은 측정된 자료값들의 전체 변동을 비교하고자 
# 하는 요인 수준(Factor Level) 간의 차이에 의해서 발생하는 변동과 
# 그 밖의 요인에 의해서 발생하는 변동으로 나누어서 자료를 분석하는 것이 
# 기본 원리가 되겠습니다. 

# 위에서 제시한 식의 양변을 제곱하여 전체 측정값들에 대해서 모두 더하여 식을 정리하면 아래와 같이 됩니다. (자세한 과정은 생략함) 

# 위의 총제곱합(SST)와 처리제곱합(SSTR), 오차제곱합(SSE) 
# 간의 기본원리를 이용하여 분산분석에서 사용하는 통계량들을 
# 표로 일목요연하게 정리한 것이 아래의 일원분산분석표(one-way ANOVA table) 가 
# 되겠습니다.

# 분산분석표의 제일 오른쪽에 있는 F0 통계량을 이용해서 
# 전체 수준들간의 평균이 같은지 아니면 다른지를 검정합니다.  
# 기본 개념을 설명하자면, F0 통계량은 처리평균제곱 (MSTR)의 크기에 
# 영향을 받으므로 처리평균제곱 (MSTR)이 커지면 오차평균제곱(MSE)은 
# 작아지게 되며, 따라서 F0 통계량은 분자가 커지고 분모가 작아지므로 
# 당연히 커지게 됩니다. 즉, F0 통계량 값이 크다는 것은 
# 수준 평균들간에 평균의 차이가 크다는 의미가 되겠습니다.

# 그러면, 요인효과에 대한 검정을 위해서 분산분석에서는 아래와 같은 
# 귀무가설과 대립가설을 사용하며, 검정통계량으로는 F 를 사용하여 
# 기각역 또는 P-value 에 의해서 검정을 하게 됩니다.  

# 이론 설명이 무척 길어졌는데요, 이제 드디어, 
# 아래 문제를 R의 aov() 함수를 사용해서 풀어보도록 하겠습니다.

# 문제 ) 정유사에서 온도(Factor, one-way)에 따라서 휘발유 
# 생산량에 변화가 있는지 (즉, 영향이 있는지) 알아보기 위하여 
# 온도를 3가지 조건(3 Factor Levels)으로 실험설계를 하여 
# 10번에 걸쳐서 휘발유 생산량을 측정하였습니다. 
# 관찰값이 아래와 같을 때 조사되었을 때 온도의 조건에 따라서 
#  휘발유 생산량에 차이가 있는지 유의수준 α = 10% 로 검정하시오. 

# R에 (1) 위의 관측값들을 벡터로 입력하고,
# (2) boxplot() 함수와 summary()를 이용하여 탐색적 분석을 해본 후에,
# (3) aov() 함수를 이용하여 one-way ANOVA 분석을 하였으며 
# : aov(y ~ group, data = dataset)
# (4) 기본 가정 중에 오차의 등분산성을 검정하기 위해 Bartlett 검정
# : bartlett.test(y ~ group, data = dataset)
# 을 실시하였습니다.
# 이때 조심해야 할 것이 있는데요, dataset을 데이터프레임으로 해서 
# 그룹(group, factor level)에 해당하는 변수는 반드시 
# 요인(Factor)형이어야 하므로, 만약 요인(Factor)형이 아니라면 
# 먼저 transfrom(factor()) 함수를 이용해서 변환을 시켜주고 
# ANOVA 분석을 수행해야 합니다. 


##----------------------------------------------------------
## One-way ANOVA : aov(), oneway.test
##----------------------------------------------------------

##--- Are there any daily outcome differences among temperature conditions?
# group 1 : temperature condition 1 
# group 2 : temperature condition 2
# group 3 : temperature condition 3
 
# daily outcome by tmep condition (group 1/2/3)
y1 <- c(50.5, 52.1, 51.9, 52.4, 50.6, 51.4, 51.2, 52.2, 51.5, 50.8)
y2 <- c(47.5, 47.7, 46.6, 47.1, 47.2, 47.8, 45.2, 47.4, 45.0, 47.9)
y3 <- c(46.0, 47.1, 45.6, 47.1, 47.2, 46.4, 45.9, 47.1, 44.9, 46.2)

y <- c(y1, y2, y3)
y
n <- rep(10, 3)
n
group <- rep(1:3, n)
group

# combining into data.frame
group_df <- data.frame(y, group)
group_df

sapply(group_df, class)

# transform from 'integer' to 'factor'
group_df <- transform(group_df, group = factor(group))
sapply(group_df, class)

# boxplot
attach(group_df)
boxplot(y ~ group,main = "Boxplot of Daily Outcome by Temperature condition 1/2/3"
        ,xlab = "Factor Levels : Temperature condition 1/2/3"
        ,ylab = "Daily Outcome")


# descriptive statistics by group
tapply(y, group, summary)

detach(group_df)

# one-wayANOVA
aov(y ~ group, data = group_df)
summary(aov(y ~ group, data = group_df))

# Bartlett test to test the null hypothesis of equal group variances
bartlett.test(y ~ group, data = group_df)


# 일원분산분석(one-way ANOVA) 결과 검정통계량 F-value가 81으로 나왔으며, 
# P-value 값은 '1.2e-13'으로서 매우 작게 나와 유의수준 10% 에서 
# 귀무가설을 기각하고 대립가설을 채택하게 되어 "온도 조건 1/2/3에 
# 따라서 휘발유 생산량에 차이가 있다"라고 판단할 수 있겠습니다.

# 오차의 등분산성 가정에 대해 Bartlett 검정 결과 P-value가 0.4368로서 
# 유의수준 10%에서 귀무가설을 채택하여 "오차의 등분산성 가정을 만족한다"고 
# 할 수 있겠습니다.


# R (2) 다중비교 - Tukey의 HSD (honestly significant difference) 검정 : 
# TukeyHSD()

# 지난번 포스팅에서 일원분산분석(one-way ANOVA) 이론과 R 의 aov() 함수에 
# 대해서 알아보았습니다.  그러면 이번 포스팅에서는 일원분산분석 후에 
# 이어서 하게 되는 다중비교(Multiple Comparison)에 대해서 알아보겠습니다.

# 수준의 수가 2일 때 일원분산분석에서 귀무가설 H0가 기각될 경우에는 
# 두 수준 평균간의 차이에 대해서는 t-Test를 적용하면 됩니다.  
# 하지만 수준의 수(number of Levles, groups)가 3개 이상인 경우에는 
# t-Test를 사용하면 안됩니다.  왜냐하면 비교해야할 수준수가 커질수록 
# 두 수준간의 짝들 간의 비교(pair-wise comaprison)를 하게 되면 
# 제 1종 오류(참을 거짓으로 판정할 오류)가 애초 정한 
# 유의수준 α(significance level α)가 아니라 이보다 훨씬 커지게 되기 때문입니다.

# t-Test를 사용할 경우 무슨 일이 벌어지는지 예를 들어서 설명해보겠습니다.  
# 만약 수준의 수(number of Levels, groups)가 5개일 경우 
# 각 각 쌍을 지어서 비교를 한다고 했을 때 수준평균짝들의 
# 총 갯수는 10개(5 Combination 2 = 4 + 3 + 2 + 1 = 10) 
# (R의 combination 함수를 사용하면, choose(5,2) 하면 10이 나옴) 
# 이므로 t-Test를 10번 적용해야 합니다.  
# 만약 유의수준 α (significance level α) = 0.05라고 한다면, 
# 10번의 t-Test를 각각 독립적으로 하고 나면 
# 제1종 오류가 생길 확률은 1 - (0.95)^10 = 0.4012631 가 됩니다.  
# 이는 처음의 유의수준 α 0.05보다 약 8배 크며, 
# 1종 오류가 생길 확률도 약 8배 정도 크다는 소리입니다. 

# 이러한 오류를 피하기 위해서는 유의수준을 설정된 크기로 유지하면서 
# 모든 두 수준짝들의 평균을 동시에 비교할 수 있도록 고안된 검정방법인 
# 다중비교(multiple comparison)으로 
# Tukey's HSD(honestly significant difference) test와 
# Duncan's LSR(least significant range) test 가 있는데요, 
# 이번 포스팅에서는 Tukey's HSD test 에 대해서 소개하겠습니다.

# Tukey's HSD(honestly significant difference) test는 
# studentized range distribution을 이용하여 모든 가능한 
# 두 수준작들의 평균간의 차이가 있는지를 
# 검정(pairwise post-hoc testing using Tukey HSD test)하는 방법입니다.  
# Tukey's HSD test 의 통계량 및 검정 방법은 아래와 같습니다. 

# F-검정 통계량을 사용해서 한개의 요인(Factor) 내 
# 세 개 이상의 수준(Levels)의 집단 간에 평균의 차이가 있는지 
# 없는지를 검정하였는데, 만약 분석 결과가 "인자 수준들의 평균 μi 는 
# 모두 같다"는 귀무가설 H0를 기각하고 "인자 수준들의 평균 μi 는 
# 모두 같지 않다" 대립가설 H1을 채택할 경우가 생겼다고 합시다. 
# 이럴 경우 수준(Levels)의 집단 간에 어디에서 차이가 생겼는지 알고 
# 싶을 때 사용하는 분석법이 다중비교(multiple comparison)이 되겠습니다.  


# 그러면, (1) R의 aov() 함수를 사용해서 유의수준 0.05 하에 
# 일원분산분석(one-way ANOVA)를 먼저 해보고 
# (<= 이것은 지난번 포스팅과 동일합니다), 
# (2) Tukey's HSD test를 통해서 다중비교(multiple comparison)을 하여 
# 쌍을 이룬 집단간의 평균 비교 중에서 어디서 차이가 발생한건지 알아보도록 
# 하겠습니다. 

##----------------------------------------------------------
## One-way ANOVA : aov(), oneway.test
##----------------------------------------------------------
##--- Are there any daily outcome differences among temperature conditions?
# group 1 : temperature condition 1 
# group 2 : temperature condition 2
# group 3 : temperature condition 3
 
# daily outcome by tmep condition (group 1/2/3)
y1 <- c(50.5, 52.1, 51.9, 52.4, 50.6, 51.4, 51.2, 52.2, 51.5, 50.8)
y2 <- c(47.5, 47.7, 46.6, 47.1, 47.2, 47.8, 45.2, 47.4, 45.0, 47.9)
y3 <- c(46.0, 47.1, 45.6, 47.1, 47.2, 46.4, 45.9, 47.1, 44.9, 46.2)
 
y <- c(y1, y2, y3)
y
n <- rep(10, 3)
n
group <- rep(1:3, n) 
group

# combining into data.frame
group_df <- data.frame(y, group)
group_df
sapply(group_df, class)

# transform from 'integer' to 'factor'
group_df <- transform(group_df, group = factor(group))
sapply(group_df, class)

# boxplot
attach(group_df)

boxplot(y ~ group, 
        main = "Boxplot of Daily Outcome by Temperature condition 1/2/3", 
        xlab = "Factor Levels : Temperature condition 1/2/3", 
        ylab = "Daily Outcome")




# descriptive statistics by group
tapply(y, group, summary)
detach(group_df)

# one-wayANOVA
aov(y ~ group, data = group_df)

summary(aov(y ~ group))

# Bartlett test to test the null hypothesis of equal group variances
bartlett.test(y ~ group, data = group_df)


##------------------------------------------------------------------
## multiple comparison 
## - Tukey's HSD(honestly significant difference) test : TukeyHSD()
##------------------------------------------------------------------
  
group_aov <- aov(y ~ group, data = group_df)
TukeyHSD(group_aov)


# 위의 일원분산분석(one-way ANOVA) 결과를 보면 P-value가 9.34e-10 으로서 
# 대립가설 H1을 채택하게 되어 평균이 서로 다르다고 판단할 수 있으므로, 
# 자연스럽게 그 다음 질문은 "다르다고 하는데 그러면 쌍을 이룬 평균 비교 
# 중에서 어디에서 차이가 난다는 말인가?"가 될 것입니다.

# 이 질문에 답하기 위해 위의 Tukey's HSD test 결과를 보면, 
# "multiple comparisons of means, 95% family-wise confidence level'이라는 
# 설명이 나오는데요, 2개씩 쌍을 이룬 수준간 평균의 다중 비교를 95% 
# 신뢰수준으로 상한(upr)과 하한(lwr)의 신뢰계수 구간을 구했고, 
# P-value도 구해주었습니다.

# 위의 결과를 보면 'group 2'와 'group 1'은 평균 차이가 -4.52 이고 
# adj. P-value가 0.0000000이고, 'group 3'과 'group 1'의 평균 차이는 
# -5.11이고 adj. P-value가 0.0000000 으로서, 
# 유의수준 0.05보다 훨씬 작으므로 이들 group (수준, factor levels) 
# 간에는 평균의 차이가 있다고 유의수준 0.05 기준 하에 판단할 수 있겠습니다.  
# 반면에, 'group 3'과 'group 2'는 평균 차이가 -0.59이고 adjusted P-value 
# 가 0.2813795 로서 유의수준 0.05보다 크므로 
# 귀무가설 H0 '두 집단 간 평균차이는 없다'를 채택하게 됩니다.  
# 이는 저 위에 있는 Boxplot을 보면 좀더 쉽게 이해가 될 것 같습니다.

# 위 분석 결과를 보기에 좋게 표로 나타내면 아래와 같습니다. 

# Tukey's HSD test의 이론에 대해서는 알쏭달쏭한 말들을 잔뜩 써놨는데요, 
# R 함수는 TukeyHSD() 딱 한줄이어서 미친 듯이 간단합니다. ^^'  
# 다음번 포스팅에서는 Duncan's LSR(least significant range) test에 
# 대해서 알아보도록 하겠습니다.


