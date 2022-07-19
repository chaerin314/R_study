getwd()
setwd('C:/Users/dnflc/Desktop/데이터_분석을_위한_R_프로그래밍')

install.packages('psych') # 패키지를 설치하는 함수
library(psych)

# 데이터 입력하기
scores <- c(63,87,92,77,100)
mode(scores)

scores2 <- data.frame()
scores2 <-edit(scores2) 
View(scores2)

points <- data.frame(label=c('low','mid','high'),
                     lbound=c(0, 0.67, 1.64),
                     ubound=c(0.674, 1.64, 2.33))
View(points)
print(points)

# 범주형 변수 기술통계
x <- c('yes', 'no', 'no', 'yes', 'no')
table(x)
factor(x)
x <- c(3,2,1,1,3,4,3,3,1,3,2)
barplot(table(x)/length(x))

x.count <- table(x)
names(x.count) <- c('기호1','기호2','기호3','기호4')
pie(x.count)
pie(x.count, col=c('purple','green2','cyan','white'))

# 수치형 자료, 연속형 자료
mean(scores)
sd(scores)
range(scores)
IQR(scores)
summary(scores)
var(scores)

library(psych)
write.csv(describe(scores, IQR=T), file='기술통계.csv')

cars
mean(cars)
apply(cars, 2, mean) #각각 적용한다(이 데이터를, 열방향으로, 이 함수로)
apply(cars, 2, sd)
describe(cars)

# 히스토그램
hist(speed)
hist(cars$speed)
hist(cars$dist)
hist(cars$speed, prob=T)
lines(density(cars$speed))

# 상자 그림
boxplot(cars$speed, main='속도')
boxplot(cars$dist, main='거리')

png('상자도표.png')
boxplot(cars$speed, main='속도')
dev.off()

# 산점도
plot(cars$speed, cars$dist)
plot(cars)

# 회귀선
model <- lm(dist~speed, cars) #linear model(y~x, 이 데이터에서)
abline(model) # 선 추가

# 선도표
plot(cars$speed, cars$dist, type='l')

# 함수를 모를 때
?plot # 내장함수 아니면 ??로
