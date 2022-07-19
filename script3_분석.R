getwd()
setwd('C:/Users/dnflc/Desktop/데이터_분석을_위한_R_프로그래밍')


# 상관성 분석
cars
out <- boxplot(cars$dist)
names(out)
out$stats

out.cars <- cars[!(cars$dist>93),] # 이상치 제거(cars안의 dist이 93 이하인 데이터 선택)

library(psych)
corr.test(out.cars) # 영가설 기각, p-value=0에 가까운 수인 것
corr.test(out.cars$speed, out.cars$dist)


# 다중 회귀 linear model
data <- read.csv('제품만족도.csv')
str(data)
head(data)

result2 <- lm(만족감 ~ 외관+편의성+유용성, data) # 참고)기울기로 비교하면 안된다!
summary(result2) # R^2=0.37(외관, 편의성, 유용성이 제품만족도를 37% 설명한다)

install.packages('lm.beta')
library(lm.beta)
lm.beta(result2) # 표준화된 계수 산출, '외관'이 제일 중요함을 알 수 있음

capture.output(summary(result2), file='다중회귀.txt') # write.table()로는 안됨, 단순히 메모장으로 출력함


# 
