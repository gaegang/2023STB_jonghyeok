#.패키지 설치 및 구동
library(dplyr)
library(ggplot2)
library(foreign)

#.데이터 불러오기
mental <- read.spss("KIPA_DATA_2022.SAV")
class(mental)#.객체유형 확인: list
mental <- as.data.frame(mental)#.데이터프레임으로 변환
class(mental)#.객체유형 확인: data.frame

#.변수 추출 후 이름 변경
mental <-mental %>%
  select(q32_2,q1_4,q32_1,q34_1,q55,d17,d1,d2,ara)%>%
  rename(suicide=q32_2,
         satisfaction=q1_4,
         loneliness=q32_1,
         family_belief=q34_1,
         wealth=q55,
         health=d17,
         sex=d1,
         age=d2,
         area=ara)

#.변수 유형 및 빈도 확인
str(mental)

table(mental$suicide)
table(mental$health)
table(mental$wealth)
table(mental$satisfaction)

#.6개 변수간 관계분석을 위한 유형 변경
mental$suicide <-as.integer(mental$suicide)
mental$satisfaction <-as.integer(mental$satisfaction)
mental$loneliness <-as.integer(mental$loneliness)
mental$family_belief <-as.integer(mental$family_belief)
mental$wealth <-as.integer(mental$wealth)
mental$health <-as.integer(mental$health)

#.변수 빈도 확인
table(mental$suicide)
table(mental$health)
table(mental$wealth)
table(mental$satisfaction)

#.변수 정리
mental$satisfaction<-mental$satisfaction-1
mental$wealth<-mental$wealth-1

table(mental$wealth)
table(mental$satisfaction)

#.범주형변수를 문자형으로 변경:오류방지
mental$age<-as.character(mental$age)
mental$sex<-as.character(mental$sex)
mental$area<-as.character(mental$area)

table(mental$sex)
table(mental$age)
table(mental$area)

mental$age<-ifelse(mental$age=="19~29세","20대",mental$age)
mental$age<-ifelse(mental$age=="60세 이상","60대",mental$age)
table(mental$age)

#.결측치와 이상치 확인
summary(mental)

#세부과제1-1
#연령대별 성별(남/여) 분류후 삶의 만족도 평균 분석, 상위 4개 집단 분석
#.성별빈도분석
mental%>%
  group_by(sex)%>%
  summarise(n=n())%>% #sex변수의 범주별 빈도 계산
  mutate(total=sum(n), #sex변수의 빈도 총계
         pct=round(n/total*100,1)) #sex변수의 범주별 비율

#.연령대별빈도분석
mental%>%
  group_by(age)%>%
  summarise(n=n())%>% #age변수의 범주별 빈도 계산
  mutate(total=sum(n), #age변수의 빈도 총계
         pct=round(n/total*100,1)) #age변수의 범주별 비율

#연령대별,성별 만족도 평균 확인(상위4개)
mental%>%
  group_by(age,sex)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))%>% head(4)

#세부과제1-2
#지역,연령대,성별로 분류(3개변수교차) 삶의 만족도 평균 분석,(상위5)
mental%>%
  group_by(age,sex,area)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))%>% head(5)

#세부과제1-3
#.연령대별 삶의 만족도 차이
mental%>%
  group_by(age)%>%
  summarise(m=mean(satisfaction))%>%
  arrange(desc(m))

#세부과제1-4
#(독립표본 t검정)연령대별 만족도 가장높은 연령대와 낮은 연령대의
#통계적 차이가 있는지 검정하기
#가장높은 연령대와 가장낮은 연령대 묶음(satisfactionage)
satisfactionage <- mental%>%
  filter(age=='30대'| age=='60대')

#만족도와 연령대의 검정
t.test(data=satisfactionage, satisfaction~age)
