#작업에 필요한 패키지 설치(7주차 ppt 18p.데이터 전처리작업 시작)
install.packages("dplyr")
install.packages("ggplot2")

#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#csv형식의 파일을 불러와서 subway객체에 입력하고 구조 확인
str(subway)

#변수의 이상치와 결측치 확인하고 처리
summary(subway)
#세부과제1은 NA's(결측치)가 미표시됨.

#분석목적에 따른 파생변수 만들기(7주차 ppt 25p)
#파생변수1.정수형day변수
subway$day <- substr(subway$Date,7,8)
class(subway$day)
subway$day <- as.integer(subway$day)

#파생변수2.line변수
table(subway$Line)
subway$Line <- ifelse(subway$Line=="9호선2~3단계","9호선",subway$Line)

#파생변수3.station변수
table(subway$Station)

#파생변수4.총승하차승객수 total passenger
subway$total_passenger <- subway$on_board+subway$getting_off

#분석데이터 최종확인 (7주차 ppt 32p. 전처리작업 끝)
str(subway)

#데이터 분석(7주차 ppt 33p. 9가지 항목 분석)
#1.승차/하차승객수
subway%>% 
  summarise(on_m=mean(on_board), off_m=mean(getting_off))
#1답. 승차10966/ 하차10925


#2.승차승객수가 가장 많은 역의 노선은?
#2-1. soulution
max(subway$on_board)
subway%>% 
  filter(on_board==94732)%>% 
  select(Date, Line, Station, on_board)
#2답. 2호선, 잠실(송파구청), 승차승객94732


#3.역별 하루 평균 전체승객수 분석
passenger10 <- subway %>%
  group_by(Station)%>%
  summarise(m=mean(total_passenger))%>%
  arrange(desc(m))%>%
  head(10)

head(passenger10, 3)
#3답. 강남역이 148890명으로 가장많습니다.


#4.역별 일평균 전체승객수 상위 10개 역을 막대그래프로 작성
ggplot(data=passenger10, aes(x=reorder(Station, m), y=m))+
  geom_col()+
  coord_flip()
#4답. plot 창에 나오고, passenger10의 순서대로 나옵니다. 


#5. 일별 전체승객 분석
subway %>%
  group_by(Date) %>% 
  summarise(total=sum(total_passenger)) %>%
  arrange(desc(total)) %>%
  head(3)
#5답.  전체인구 대비 승객비율   15699271/51558034
#      수도권인구 대비 승객비율 15699271/23034430

#6. 특정 line 분석(1호선)
subway %>%
  filter(Line=="1호선") %>%
  filter(total_passenger==max(total_passenger)) %>%
  select(Date, Station, on_board, 
         getting_off, total_passenger)
#6답. 1216일에 서울역에서 승차59873명, 하차 58758명
#합쳐서 118631명이 사용하였습니다.


#7. 노선별 전체승객 비율비교
line_pct <- subway %>%
  group_by(Line) %>% 
  summarise(total=sum(total_passenger)) %>%
  mutate(all=sum(total), 
         pct=round(total/all*100,2))

line_pct %>%
  arrange(desc(pct)) %>%
  head(3)
#7답. 2호선을 가장많이 이용하였습니다


#8. 지하철 전체 승객 비율 막대그래프 그리기
line_pct10 <- line_pct %>%
  filter(Line%in%c("1호선","2호선","3호선","4호선","5호선","6호선","7호선","8호선","9호선","분당선" ))

ggplot(data = line_pct10, aes(x=reorder(Line,pct),y=pct))+
  geom_col()+
  coord_flip()+
  ggtitle("수도권 지하철 노선별 이용비율")+
  xlab("노선")+
  ylab("이용비율")
#8답. plot창 확인 

#9. 일별 전체승객 선그래프 그리기
line_graph <- subway %>%
  group_by(day) %>%
  summarise(s=sum(total_passenger))

ggplot(data = line_graph, aes(x=day, y=s, group=1))+
  geom_line()+
  ggtitle("수도권 지하철 일별 이용승객수")+
  xlab("일")+
  ylab("이용승객")
#9답. plot창 확인 