# ** quantile 함수 -> 데이터의 분위수를 표시
# ** IQR(Inter quantile range) --> 3분위수와 1분위수의 차이를 표시
# ** sampling package
# 층화 샘플링은 크게 두 가지 방법 이용 가능
  # 1. sampling package 설치 -> strata(나눌 기준 층 변수, size, method, data)
  # 2. doBy package     설치 -> sampleBy(층으로 나눌 기준 변수, frac, data)

# ** 계통 추출법(systematic sampling)
# 무작위로 집단을 뽑아 k번째 위치에 해당하는 표본 추출. 주기성이 있을 때는 사용 하면 안된다.
# 방법. sampleBy 에서 Systemetic = TRUE로 주면 됨

# ** 분할표: 집단을 어떤 특성(ex)성별, 나이)에 따라 분할 할 때 만들어지는 자료 정리표
# 이에 대한 독립성 검정, 동질성 가정은 카이제곱 승으로 계산한다.

# ** table 과 xtabs 의 차이
# table은 빈도수를 나타내고 xtabs은 num 안에 있는 value 값을 나타냄
# 각 행과 열에 대한 바깥부분의 비율과 집계 값을 계산 할 수 있음.
# margin.table(data, 1 = 행 , 2 = 열), prop.table(data, 1 = 행, 2 = 열), 리턴 값은 리스트 형식으로 반환

# ** 독립성 검정(indepedence Test)
# 카이 통계량이 Ho 를 기각 시키지 못하면 독립성 존재, Ho 를 기각 시키면 독립성 x
# chi.square Test 는 xtabs 가 존재 해야 사용 가능

# ** 피셔의 정확 검정(Fisher’s Exact Test)
# 독립성 검정에서 표본의 개수가 적을 때 카이 검정이 불가능 하므로, 피셔의 정확 검정을 통해 독립성 검정 실시

#


# 예제1. 정규분포를 띄는 고정된 난수 100개를 연속 함수 형태로 만들어 보기(단, 소수점 2째자리까지 표현)

# 예제2. 평균이 0, 표준편차 5이고 정규분포를 띄는 표본 100개를 뽑고 각 1분위, 2분위, 3분위에 해당되는 값을 출력하고,
# 3분위 수와 1분위 수의 차이를 구하시오

# 예제3. iris의 Data를 셔플링 한 후(150개), 전체의 70%는 train에, 30% test에 담아 보시오
  # 1. 데이터를 강제적으로 뒤섞은 경우, ind1 에 150의 인덱스를 임의로 섞은 값을 저장 후, iris[ind1]을 A1 에 다시 저장하고
  # 그 A1을 0.7 , 0.3 으로 나눔
  # 2. row의 수를 n1에 저장 후,  처음에 인덱스 자체를 뽑을 때 전체의 0.7 개만 뽑아 나눠서 적용하는 경우
  # 3. 가중치를 주는 방식

# 예제4. iris 데이터에서 Species 를 기준으로 층화 샘플링을 진행하시오. 각각 3개씩만 뽑으시오

# 예제5. iris 데이터에서 층화 샘플링을 진행 하시오. (각각 랜덤으로 75개씩 2개의 층과 종에 대한 층 : 총 6개의 층을 만들어 진행, 2개씩 표본을 추출)

# 예제6. sampleby 함수를 통해 30% 씩 추출

# 예제7. for 문을 통해 iris 데이터를 계통 추출 하시오. (k = 10)

# 예제8. c("1", "2", "2", "1"), c("A", "B", "A", "B"), num=c(3, 5, 8, 7)인 분할표를 만들어서, table, xtabs 함수를 이용해서 확인하시오

# 예제9. 성별과 운동량에 따른 심박수의 차이를 볼 때, 독립성 검정을 실시하시오

# 예제10. fisher의 정확 검정을 통해 오른손 잡이와 박수 칠때 올라오는 손의 위치 간의 관계가 있는지 독립성 검정을 실시하시오

# 예제11.

fisher.test(xtabs(~W.Hnd + Clap, data = survey))
# 결과 - 오른손 잡이와 박수 칠 때 올라오는 손의 관계가 유의미 하다는 평가 - (p-value 값이 0.05 보다 작음)

# 예제 1번 답
set.seed(100)
options(digit = 3)
x1 <- rnorm(100, mean = 0, sd= 5 )
x2 <- density(x1)
plot(x2)

# 예제 2번 답
x1 <- rnorm(100, mean = 0, sd = 5)
quantile(x1, c(0.25, 0.5, 0.75))
IQR(x1)

# 예제 3번 답
# 1.
ind1 = sample(1:150, 150)
A1 = iris[ind1,]
A1Length = nrow(A1)
train = A1[1:(A1Length*0.7),]
test = A1[(A1Length*0.7):A1Length,]

# 2.
n1 = nrow(iris)
ind2 = sample(n1, n1*0.7, replace = F )
train = iris[ind2, ]
test = iris[-ind2, ]

# 3.
ind2 = sample(2, n1, replace = T, pro=c(0.7, 0.3))
train2 = A1[ind2 == 1, ]
test2 = A1[ind2 == 2,  ]

# 4.
library(sampling)
stra1 = strata(c("Species"),size=c(3,3,3), method = 'srswor' ,data=iris)

# 5.
A2 = iris
A2$Species2 = rep(1:2, 75)
stra2 = strata(c("Species", "Species2"), size=c(2,2,2,2,2,2), method='srswor', data=A2)
stra2

# 6.
library(doBy)
sampleBy(~Species+Species2, frac = 0.3, data=A1)

# 7.
A2 = data.frame()
for(i in seq(sample(1:10, 1), 150, 10)){
  midA2 = iris[i,]
  A2 = rbind(A2, midA2)
}

# 8.
fra1 = data.frame( x=c("1", "2", "2", "1"), y=c("A", "B", "A", "B"), num=c(3, 5, 8, 7))
table(fra1)
xtabs(num~x+y, data=fra1)

# 9.
library(MASS)
View(survey)
xtable1 = xtabs(~Exer+Sex, data=survey)
chisq.test(xtable1)
# 결과 -> p-value 값이 0.05 보다 크므로, 성별과 운동 간의 독립성이 존재

# 10.
W.Hnd + Clap, data = survey
