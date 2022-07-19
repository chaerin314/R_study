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


# 제품만족감에 영향을 미치는 요인들: 다중 회귀 linear model
data <- read.csv('제품만족도.csv')
str(data)
head(data)

result2 <- lm(만족감 ~ 외관+편의성+유용성, data) # 참고)기울기로 비교하면 안된다!
summary(result2) # R^2=0.37(외관, 편의성, 유용성이 제품만족도를 37% 설명한다)

install.packages('lm.beta')
library(lm.beta)
lm.beta(result2) # 표준화된 계수 산출, '외관'이 제일 중요함을 알 수 있음

capture.output(summary(result2), file='다중회귀.txt') # write.table()로는 안됨, 단순히 메모장으로 출력함


# 암기방법에 따른 기억력 차이: ANOVA
data2 <- read.csv('기억.csv')
str(data2)
head(data2)

data2$group <- factor(data2$group)

boxplot(data2$recall~data2$group)
describeBy(data2$recall, data2$group) # 집단에 따라 기술통계

install.packages('car')
library(car)
leveneTest(recall~group, data2) # 유의확률>유의수준=>영가설 수용=>등분산 가정 충족된 data임

oneway.test(recall~group, data2, var.equal=T) # 일원분산분석, 영가설 기각
pairwise.t.test(data2$recall, data2$group, p.adj='bonferroni') # 엄격한 기준(3번비교=>0.05/3으로), imagery > count = rhyming(imagery가 가장 효과)


# 성별에 따른 신장의 차이: t-test
data3 <- read.table('신장.txt', header=T)
str(data3)

data3$gender <- factor(data3$gender)

leveneTest(height~gender, data3)
t.test(height~gender, data3, var.equal=T) # t검정, 영가설 기각(신장 평균이 다름)



########## 군집분석 ##########
# 분석 데이터 확인
str(iris)
head(iris)

data4 <- iris
names(data4) <- c('꽃받침길이', '꽃받침넓이', '꽃잎길이', '꽃잎넓이', '종') # 변수 이름 변경
str(data4)

# 특성에 따른 종 분류
# 1) 계층적 군집분석(hc)
# 거리 계산(유클리디안)
dist <- dist(data4[, 1:4])  # '종'변수 제외
hc <- hclust(dist)
hc
# 덴드로그램 그리기
plot(hc, hang=-1)  # hang: 레이블 정렬 (0, -1)
rect.hclust(hc, k=3, border='blue')  # 군집 수 3개로 지정
# 분류결과
cluster <- cutree(hc, k=3) # 군집 1/2/3으로 분류
data4$c <- cluster # 분류한 군집 번호를 새로운 열에 추가
table(data4$c) # 각 군집별 개수
table(data4$종)
table(data4$c, data4$종) # 분류결과와 종의 일치도: 분류가 제대로 안됨(특성이 일부 겹침)

# 2) 비 계층적 군집분석: k-means 군집분석
# 군집의 수를 미리 지정한 상태에서 분석
# 계층적 군집분석보다 연산이 빠르며 결과도 안정적
# 초기에 랜덤으로 중심을 잡기 때문에 분석할 때 마다 분석결과 다름
# 군집의 수 결정
install.packages('NbClust')
library(NbClust)
cn <- NbClust(data4[,1:4], min.nc=2,  max.nc=5, method="kmeans", index="all") # 데이터를 지정해준 범위 내에서 각각 계산해서 plot(2-3개에서 크게 감소)

# K-means 군집분석(둘다 확인)
km2 <- kmeans(data4[,1:4], 2)    # 2개로 지정
km3 <- kmeans(data4[,1:4], 3)    # 3개로 지정

# 분류결과
table(km2$cluster)
table(km2$cluster, data4$종) # 분류결과와 종의 일치도
plot(data4[, c('꽃받침길이', '꽃받침넓이')], col= km2$cluster)
plot(data4[, c('꽃잎길이', '꽃잎넓이')], col= km2$cluster) # 더 확실히 구분됨

table(km3$cluster)
table(km3$cluster, data4$종) # 분류결과와 종의 일치도
plot(data4[, c('꽃받침길이', '꽃받침넓이')], col= km3$cluster)
plot(data4[, c('꽃잎길이', '꽃잎넓이')], col= km3$cluster) # 더 확실히 구분됨

data4$kmc <- km3$cluster
str(data4)
# 비계층적 군집분석 결과가 더 안정적임



########## 워드클라우드 ##########
install.packages('wordcloud2')
install.packages(c('hash', 'tau', 'Sejong', 'RSQLite', 'devtools',
                   'bit', 'rex', 'lazyeval', 'htmlwidgets', 
                   'crosstalk', 'promises', 'later', 'sessioninfo', 
                   'xopen', 'bit64', 'blob', 'DBI', 'memoise', 
                   'plogr', 'covr', 'DT', 'rcmdcheck', 
                   'rversions'), type='binary')
install.packages('rJava')
rJava::.jinit() # error가 없는 경우 KoNLP 설치로 이동
# error인 경우 'jdk-18_windows-x64_bin'설치

install.packages('multilinguer') # jdk 설치 패키지
library(multilinguer)
install_jdk()

# KoNLP 설치
install.packages('remotes')      
remotes::install_github('haven-jeon/KoNLP', upgrade='never', INSTALL_opts=c('--no-multiarch'), force=T)
# 설치 오류 시 scala-library-2.11.8.jar 파일을 java폴더에
# 복사하여 붙여넣은 후 프로그램 재실행하여 library 실행
library(KoNLP)
extractNoun('테스트입니다 안녕하세요')
# 테스트문장에서 단어가 추출되는지 콘솔창 확인
# 단어가 추출되어야 KoNLP가 제대로 설치되어 있는 것

# 사전 추가
useSejongDic()

# 데이터 불러오기
data5 <- readLines('2022 신년사.txt')
data5

# 각 문장에서 단어 추출
ns <- sapply(data5, extractNoun, USE.NAMES=F) 
ns.unlist <- unlist(ns)

# 텍스트 전처리
ns.unlist <- gsub('[~!@#$%&*()_+=?<>]','', ns.unlist)     # 특수문자 삭제
ns.unlist <- gsub('[ㄱ-ㅎ]','', ns.unlist)                # ㅋㅋ, ㅎㅎ 삭제
ns.unlist <- gsub('(ㅜ|ㅠ)','', ns.unlist)                # ㅜㅜ, ㅠㅠ 삭제
ns.unlist <- gsub("\\d+","", ns.unlist)                   # 숫자 삭제
ns.unlist <- Filter(function(x){nchar(x)>=2}, ns.unlist)  # 2글자 이상

sort(table(ns.unlist), decreasing=T)

ns.unlist <- gsub("회장", "", ns.unlist) # 불용어 삭제
ns.unlist <- gsub("한다”고", "", ns.unlist)     
ns.unlist <- gsub("“올해는", "", ns.unlist)

ns.unlist <- gsub("‘고객", "고객", ns.unlist) # 단어 변환

# 단어빈도 TOP50
top50 <- head(sort(table(ns.unlist), decreasing=T), 50)
top50

# 워드클라우드
library(wordcloud2)
wc <- wordcloud2(top50,
                 size= 1,                           #폰트사이즈
                 rotateRatio= 0,                    #회전단어비율
                 color= 'random-dark',              #단어색상
                 shape= 'circle',                   #모양
                 fontFamily= "LG Smart UI Regular") #폰트명
wc

# 워드클라우드 저장
library(htmlwidgets)
saveWidget(wc, "워드클라우드.html", selfcontained=F)
