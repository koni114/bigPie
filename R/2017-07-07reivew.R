# 주옥같은 교수님의 말들
# - 공공 데이터는 분석을 위한 목적으로 데이터가 수집된 것이 아님

# ** 분산분석(ANOVA)

# 분산분석(ANOVA)은 비교할 집단의 수가 2개 이상일 때 사용
# 분산분석은 한 변수에 대해서 그룹(factor, 요인)으로 나눌 수 있어야 함.
# 집단간 차이를 분산을 통해서 검정. 집단 간 변동 / 집단 내 변동 ==> F분포를 통해서 확인
# 집단 간 변동 / 집단 = F 값이므로 이 값이 커지면 집단 간 차이가 있다고 판단! (F 통계량)
# 종류: 일원배치 분산분석(one-way ANOVA), 이원배치 분산분석(two-way ANOVA), 다원변량분산분석(MANOVA)
# 이원배치 분산분석 같은 경우 교호작용을 고려 해 주어야 한다.
# 여러가지 함수가 있지만, 사후 검정이 가능한 aov() 함수를 추천

# ** 등분산검정(leveneTest)
# ANOVA Test 는 분산을 통해 유의 검정을 하는 것인데, 이때 등분산인지 검정이 필요
# 이를 car package에 leveneTest를 통해 검정

# ** 교호작용(interaction)
# 요인 A의 수준에 따라 요인 B의 효과가 달라지는 것

# ** melt() 함수
# id -> 내가 고정하고자 하는 변수
# variable 과 value로 변수 이름이 나타나는 것 같음

# ** 데이터 타입
# 1. 스칼라(scalor) - 변수 하나를 의미
# 2. 백터(vector) - c() 형태로 말 그대로 벡터 - 동일한 타입이 들어가야 함. 만약 다른 데이터 타입의 값이 들어오면, 하나로 통일 됨(coercion)
# 3. 매트릭스(matrix) - 말 그대로 매트릭스 구조로, 동일한 데이터 구조의 행과 열로 만들어진 구조
# 4. 데이터프레임(data.frame) - 행과 열로 만들어진 구조이지만 다른 데이터 타입으로 만들어진 구조
# 5. 배열(array) - 매트릭스와 같이 데이터 타입이 같아야 하지만, 행, 열, 면으로 크기가 더 큰 구조
# 6. 리스트(list) - 여러 데이터 타입 및 객체를 여러 차원에 담을 수 있는 구조

# **** 데이터 타입 변환
# as 명령어를 통해 데이터 변환이 가능
# rownames() colnames() names() 함수를 통해 각 행과 열의 rowmame, colname을 가져올 수 있음

# ** grep 함수 --> 특정 문자가 포함되어 있는 data의 index값 추출

# ** 데이터 셋 사이트
# http://archive.ics.uci.edu/ml/index.php
# GPS data 등 다양한 데이터들이 올라오는 사이트. 아직 한국의 데이터 셋은 많지 않다. 따라서 외국 사이트 참조
# 데이터에 대한 경험이 굉장히 중요하므로, 데이터들을 많이 찾아서 보기!

# ** modeling
# 모형은 보통 인과관계를 이야기 하는데, 가장 default는 선형 회귀! -> lm
# 회귀 에서 가장 중요한 것은 Y = a + bx
# p-value 가 0.05보다 작다는 것은 가중치(위에서 b)가 있다는 것.
# 중요한 데이터들은 전송이 가능
# 원래 값과 match 율이 가장 가까운 알고리즘이 가장 좋은 알고리즘이라고 할 수 있음




# 1. 오전 오후에 대한 평균 황사 농도가 지역별로 차이가 있는지 검정하시오
# c1 = read.table('clipboard', header = T)
c1$q1 = apply(c1[, 2:7], 1, FUN = mean)
c1$q2 = apply(c1[, 8:13], 1, FUN = mean)
c1$q3 = apply(c1[, 16:21], 1, FUN = mean)
c1$q4 = apply(c1[, 22:27], 1, FUN = mean)
var.test(c1$q2, c1$q3) # p-value 값이 0.05 보다 크므로, 등분산
t.test( x=c1$q2, y=c1$q3, var.equal = T, paired = T) # p-value 값이 0.05 보다 작으므로 차이가 있음을 보임



# 2. MASS 데이터를 통해서 흡연량이 심박수에 영향을 미치는지 ANOVA 분석을 통해 확인하시오
library(MASS)
View(survey)
anova1 = aov( Pulse~Smoke, data=survey)
anova2 = aov( Pulse~Exer, data=survey)
anova3 = aov( Pulse~Smoke+Exer+Smoke*Exer, data=survey) # 흠연량과 운동량교호작용까지 고려
summary(anova1) # p-value 값이 0.05 보다 크므로 담배에 대한 심박수의 차이는 보이지 않음
summary(anova2) # p-value 값이 0.05 보다 작으므로 운동에 대한 심박수의 차이는 보임
summary(anova3) # 흡연량과 운동량에 대한 교호작용은 없음


# 3. 앞서 ANOVA 분석에 대한 사후검정을 실시하시오
t1 = TukeyHSD(anova2, "Exer") #구체적으로 적당히 운동하는 사람과 열심히 운동 하는 사람간의 심박수 차이가 있음을 알 수 있다

# 4. 위에 가져온 황사 데이터에서 시간대 별로 황사 농도가 차이가 있는지 확인하시오
c1
library(reshape2)
c1[, 1:25]
c2 = melt(c1[ ,1:25], id=1)
str(c2)
anova4 = aov(value~variable, data=c2) # melt 함수는 고정된 id 가 value
resultTest = TukeyHSD(anova4, 'variable') # 사후 검정 실시
table1 = as.matrix(resultTest$variable) # resultTest 는 dataType 이 list 이므로, matrix 형태로 변경
total= rownames(table1[which(table1[,4]<0.5),]) # 결론적으로 total에 NULL이라는 것은 각각 농도의 차이가 없다는 것을 보인다

# 5. vowel 데이터에서 소문자  class에 'o' 가 들어간 데이터를 모두 가져오시오
library(mlbench)
data(Vowel)
v1 = Vowel[ grep('i', Vowel$Class), ]

