## library path 오류시,
# -> libPaths("추가할 경로 설정")

## %in% : for문에서의 in과 같은 역할. 오른쪽 해당 값이 포함되어 있으면 TRUE, 아니면 FALSE
# 1. 조건 없는 변수 값 확인
x  %in% c(1,4,7,5) # 단순히 이러한 코드 형식으로도 TRUE, FALSE 반환

# 2. 조건 내에 변수 값 확인
for (  i in c(1,3,5) ){
  print(i %in% c(1,4,7,5)) # for문 안에서는 print를 사용해서 TRUE, FALSE를 확인
}

## seq(시작값, 끝값, 간격) sequence를 의미.
# for문을 돌릴 때 seq()를 이용해서 특정 간격으로 돌릴 수 있음
sum=0
for( i in 1:9) #  1부터 9까지 차례대로
for(i in seq(1, 100, 3)){
  sum = sum+i
  print(sum)
  }

## 데이터 객체 탐색을 위한 함수
# 1. str() -> 데이터 구조, 변수 갯수, 변수 명, 관찰지 갯수, 관찰지의 미리보기
# 2. head() -> 상위 몇개의 데이터만을 보여줌 -> 값을 입력하지 않으면 6개의 값을 출력
# 3. tail() -> 마지막 몇개의 데이터만을 보여줌 -> 값을 입력하지 않으면 6개의 값을 출력
# 4. dim() -> 데이터 객체의 차원을 의미 ex) dim=c(1, 50) => 1차원의 50개의 변수를 의미

s = array(dim=c(1,50)) # array로 선언을 하였지만 1차원이기 때문에 벡터 형태로 나타남.
str(s)                 # 벡터 형태임을 확인
for(i in 1:50){
  if(iris[49:50, 1] > 6.5){ # 이렇게 표기하면 error, -> if안에 boolean값이 vector이기 때문에 에러.
    s[1, i] = 1;
  }else{
    s[1, i] = 0;
  }
}

## ifelse(조건, true일때 조건, FALSE일때 조건)
s1 = ifelse(iris$Sepal.Length > 7, 1, 0) # 꽃받침의 길이가 7보다 크면 1, 아니면 0을 출력하는 예제


## 실습 1부터 150까지 10개씩 뽑은 표본의 평균(총 1000개)을 구하고, 이러한 평균들의 분포가 어떠한지 확인하는 예제.
m2 = c()
for(i in 1:1000){
  ind1= sample(1:150, 10, replace=T)
  m1 = mean(ind1)
  m2 = c(m2, m1)
  }
hist(m2)
mean(m2)

## 함수 선언 -> javascript와 유사. 변수에 함수를 선언할 수 있음.
#  is 함수 ( ex) is.character ) -> 데이터 타입을 확인.
#  as 함수 ( ex) as.factor    ) -> 데이터 타입을 변환.
#  set 으로 시작 -> 환경 설정 관련 함수
# 함수 위에 ctrl + 커서 클릭 하면, 함수가 구체적으로 어떻게 구성되어 있는지 확인 가능

#  add 함수에 a와b라는 변수를 받아 리턴 시키는 함수
#  1) a,b가 숫자라면 더하고, a,b가 문자라면 붙여서 리턴 시키는 함수 구현
add <- function(a, b){

  if( is.character(a) && is.character(b) ){

    add1 = paste(a,b, sep=' ') # sep는 단어와 단어 사이의 공백을 줄 수 있음
    return (add1)

  }else if( is.numeric(a) && is.numeric(b)){
    add1 = a+b
    return (add1)

  }else{
    print("데이터 타입이 잘못되었습니다.")
     }
}

add( 2, 1 ) # 숫자의 조합이므로 3이 출력
add( "허", "재훈") # 단어의 조합이므로 paste값이 출력

## 나누기에 대한 기본적인 연산자
#  /, %%, %/%
# 1. / -> 소수점 이하까지 다 나누어서 계산
# 2. %% -> 나머지만 보여줌
# 3. %/% -> 몫만 보여줌

5/3
5 %% 3
5 %/% 3


## 기본 sin그래프에 정규분포를 띠는 error값이 포함된 y값 만들기

pi                           # pi -> 수리적인 의미이 파이를 의미함
1:(2*pi)                     # 소수점까지 계산하여도 정수값의 백터로 표현
x = seq(0, 2*pi, 0.01)       # 0~3.141593 까지 0.01단위의 벡터 값
y = sin(x)+ rnorm(length(x)) # sin 그래프에 기본 error가 만들어지도록 생성한 y 그래프
                             # rnorm -> 평균이 0, 분산이1인 정규분포를 의미


## excel, 이미지 데이터 읽어오기
# 1. clipboard 로 간단한 데이터 가져오기
data1 = read.table('clipboard', header = T)
## 2. 직접 데이터 읽어오기

## 2.1 csv 읽어오기

data2 = read.csv('test.csv') # skip 같은 경우, 내가 처음 n줄을 스킵하겠다는 의미,
names(data2) = c("x1", "x2") # names() -> 변수의 이름을 지정하는 함수.

## 2.2. xlsx 데이터 읽어오기

library(xlsx)
data3 = read.xlsx('test.xlsx', 1)
names(data3) = c("x1", "x2")

# 2.3. 이미지 데이터 읽어오기 jpeg, png
# bitmap 함수를 이용해서 읽을 수 있음.
library(readbitmap)
png1 = read.bitmap('img7.png')
jpg1 = read.bitmap('img7.jpg')

dim(png1) # ** png -> 면수가 4
dim(jpg1) # ** jpg -> 면수가 3

jpg1_m = matrix(jpg1, nrow = 1, byrow = T ) # vectorize
png1_m = matrix(png1, nrow = 1, byrow = T )

jpg1_m[1:10]

dim(jpg1_m) # 차원, 변수개수 확인
dim(png1_m)

str(iris3)
iris3[,,1]
iris[1,1,]

# 데이터 파일명 scan 해주는 함수
list1 = list.dirs('.', full.names = T) # 폴더 . -> 현재 디렉토리에 존재하는 폴더 확인
for(i in 2:length(list1)){
  list2 = list.files( list1[i], full.names =T, include.dirs = T )
}

str(list2)