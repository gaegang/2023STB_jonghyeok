#ppt의 07.07. degree of congesti 대신 07. seoul_subway_crowded로 저장함.
#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#data 파일은 crowded로 바꿔서 저장함.
#CSV형식의 파일 불러와서 subway객체에 입력하고 구조 확인
str(crowded)

#변수의 이상치와 결측치 확인하고 처리
summary(crowded)

#결측치 개수 확인
is.na(crowded)
sum(is.na(crowded))
colSums(is.na(crowded))

#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
crowded1 <- crowded[!is.na(crowded$s0600),]
colSums(is.na(crowded1))

##23시 30분 출발기차의 결측치를 제거
crowded1 <- crowded1[!is.na(crowded1$s2330),]
colSums(is.na(crowded1))

#남은 결측치를 0으로 대체
crowded1[is.na(crowded1)] <- 0
colSums(is.na(crowded1))

#이상치 확인
ggplot(crowded1, aes(y=s0530))+ 
  geom_boxplot()

summary(crowded1$s0530)

#파생변수
#호선
table(crowded1$line)
mean(crowded1$line)


#2-1.지하철역의 하루 평균 혼잡도
crowded1$day_mean <-
  rowMeans(crowded1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])
crowded1%>%
  summarise(mean(crowded1$day_mean))

#2-2.지하철 호선별 하루평균 혼잡도
#2-3.지하철 호선별 출근시간(07:00~09:00)대의 평균혼잡도



#2-4.08시 지하철 혼잡도 범주화/범주별 빈도분석
crowded1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>% 
  group_by(s80_grade) %>% 
  summarise(n=n())%>% 
  mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  select(s80_grade,n,pct)%>% 
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
crowded1 %>% 
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>% 
  group_by(line, s80_grade) %>% 
  summarise(n=n())%>% 
  mutate(total=sum(n), pct=round(n/total*100,1))%>% 
  filter(s80_grade=="caution")%>% 
  select(line, s80_grade,n,pct)%>% 
  arrange(desc(pct))%>% 
  head(5)
