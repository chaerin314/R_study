getwd()
setwd('C:/Users/dnflc/Desktop/데이터_분석을_위한_R_프로그래밍')

install.packages('dplyr')
install.packages('ggplot2')
install.packages('GGally')

# 데이터 확인
str(mtcars)
head(mtcars)

library(dplyr)
library(ggplot2)

# mpg의 히스토그램 작성
mtcars %>%
  ggplot(aes(x=mpg))+
  geom_histogram(binwidth=2, color="black", fill="gray") +
  geom_vline(aes(xintercept=mean(mpg)), linetype="dashed") +   # 세로선
  labs(title="Histogram of mpg", x="mpg (Miles/(US) gallon", y="frequency")

# 기어(자동/수동) 방식에 따른 연비 평균 막대그래프
cars <- mtcars
cars$am <- ifelse(cars$am==0, '자동', '수동')   # ifelse(조건,T,F)
cars %>%
  group_by(am) %>%   # am 변수에 따라 데이터 나눔
  summarise(연비평균=mean(mpg)) %>%   # mpg 평균 계산
  ggplot(aes(x=am, y=연비평균)) +   # x, y축만 생김
  geom_col(fill=c(3,4)) +   # 막대그래프 작성(fill은 색상 번호)
  labs(title="기어종류에 따른 연비", x="기어종류", y="연비평균")   # labels

# 기어(자동/수동), 연비, 배기량 간 관계
library(GGally)
ggpairs(cars, columns=c('mpg', 'disp', 'am'), title='correlation matrix')   # 두개씩 묶어서 3가지의 관련성을 찾음