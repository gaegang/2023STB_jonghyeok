#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#데이터를 새객체 foodshop으로 불러오기
#비어있는 셀은 결측치 처리/파라미터 문자형으로 변환
foodshop <- read.csv("seoul_food_2023.csv", na="", stringsAsFactors = F)

#데이터 구조 확인
str(foodshop)
#분석변수 추출 및 변수이름 변경
foodshop <- foodshop %>%
  rename(open_date=인허가일자, status=상세영업상태명, 
         close_date=폐업일자, name=사업장명, type=업태구분명,
         address=소재지전체주소) %>%
  select("name","type","status","open_date","close_date", "address")
#추출된 데이터 구조 확인
str(foodshop)
#날짜데이터를 분석용 데이터로 변경
#1.YYYYMMDD형식으로 변경
foodshop$open_date <- gsub("-","",foodshop$open_date)
foodshop$close_date <- gsub("-","",foodshop$close_date)
#2.문자형 데이터를 정수형 데이터로 변환
foodshop$open_date <- as.integer(foodshop$open_date)
foodshop$close_date <- as.integer(foodshop$close_date)
#3.변경된 데이터구조 확인
str(foodshop)

#.파생변수 만들기
#1.status변수
table(foodshop$status)
#영업상태가 영업/폐업이 아닌 것을 제외
foodshop <- foodshop %>% 
  filter(status == '영업' | status == '폐업') %>%
  select(name,type,status,open_date,close_date,address)
#처리결과 확인
table(foodshop$status)

#2.type변수
table(foodshop$type)
#업종에 컬럼의 모든 값이 포함된 레코드 제외외
foodshop <- foodshop %>% 
  filter(address != '서울특별시 동대문구 용두동 235-18' | open_date != '20190708' | close_date != '20210902') %>%
  select(name,type,status,open_date,close_date,address)
#처리결과 확인
table(foodshop$type)

#3.open_date변수
range(foodshop$open_date, na.rm = T)
table(is.na(foodshop$open_date))#결측치 없음
foodshop$open_year<-substr(foodshop$open_date,1,4)#인허가년도 변수 생성

#4.close_date변수
range(foodshop$close_date, na.rm = T)
foodshop$close_year<-substr(foodshop$close_date,1,4)
#인허가년도 변수 생성

#5.address변수
foodshop$district<-substr(foodshop$address,7,9)
#구 정보를 분리하여 변수 생성
table(foodshop$district)#이상치 확인
foodshop$district <- ifelse(foodshop$district%in%c("도 제","시 망","시 수","시 영","시 원","시 일"),NA,foodshop$district)#이상치제거
table(foodshop$district)#이상치 확인

#최종 확인
str(foodshop)

#문자형데이터를 정수형으로 변경
foodshop$open_year <- as.integer(foodshop$open_year)
foodshop$close_year <- as.integer(foodshop$close_year)
str(foodshop)

#데이터분석
#1.가장 오래 영업 중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터 추출
  filter(open_date==min(open_date)) %>% #개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address)
#1답. 골목식당/ 한식/ 19110528/ 서울특별시 강남구 청담동 126-6번지 


#2.주요 업종별로 가장 오래 영업중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터 추출
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type) %>%#업종별 분류
  filter(open_date==min(open_date)) %>% #개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address)
#2답.
#경양식/     사카/        19651010/ 서울특별시 중구 초동 40-4번지
#일식/      교동 전선생/  19660806/ 서울특별시 중구 회현동1가 92-1번지 
#중국식/      태화관/     19651010/ 서울특별시 용산구 후암동 244-60번지
#호프,통닭/   풍년집/      19670127/ 서울특별시 종로구 명륜4가 46-1번지
#분식/ 동명삼계탕/         19670127/ 서울특별시 종로구 중학동 91-0번지
#기타/ 케이에프씨 중앙대점/ 19671010 /서울특별시 동작구 흑석동 223-1


#3.업종별 개업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(10)
#3번 답.
#1 한식               112367 264882  42.4
#2 분식                35185 264882  13.3
#3 기타                27847 264882  10.5
#4 경양식              26364 264882  10  
#5 호프/통닭           18580 264882   7  
#6 일식                11238 264882   4.2
#7 중국식               9043 264882   3.4
#8 정종/대포집/소주방   5906 264882   2.2
#9 까페                 4242 264882   1.6
#10 통닭(치킨)           4206 264882   1.6



#4.영업 중인 음식점의 업종별 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(status=="영업") %>% #영업만 추출
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(5)
#4번답. 종류     개수  전체   비율
#1    한식      52928 125565  42.2
#2    기타      20643 125565  16.4
#3    호프/통닭 10043 125565   8  
#4    경양식     9598 125565   7.6
#5    분식       8689 125565   6.9


#5.전체 음식점의 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(status) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) #범주별비율계산

#5번답. 종류   개수   전체   비율 
#1     영업   125565 264882  47.4
#2     폐업   139317 264882  52.6


#6.주요 업종별 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type,status) %>%#교차차 분류
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  filter(status=="영업") %>% #영업만 추출
  arrange(desc(n))
#6번답.종류      상태   개수  전체   비율
#1    기타      영업   20643 27847  74.1
#2    호프/통닭 영업   10043 18580  54.1
#3    경양식    영업    9598 26364  36.4
#4    분식      영업    8689 35185  24.7
#5    일식      영업    6601 11238  58.7
#6    중국식    영업    4882  9043  54  


#7.개업이 많았던 연도
foodshop %>%
  filter(!is.na(open_date)&!is.na(district))%>% #결측치제외
  group_by(open_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#7번답. 연도 점포개수
#1      2022 11652
#2      2019 10750
#3      2023 10533
#4      2021 10067
#5      1994  9247


#8.폐업이 많았던 연도
foodshop %>%
  filter(!is.na(close_date)&!is.na(district))%>% #결측치제외
  group_by(close_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#8번답. 연도  개수
#1       1999  7276
#2       2000  6479
#3       2002  6429
#4       1998  5870
#5       2001  5669


#9.연도별 개업 음식점수 그래프
#연도별 개업 음식점수
open_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(open_year) %>%
  summarise(open_n=n())
#open_trend 구조
str(open_trend)
#연도별 개업 음식점수 막대그래프
ggplot(data=open_trend,aes(x=open_year,y=open_n))+
  geom_col()+
  xlab("연도") + ylab("개업수")
#9답.plot 확인


#10.연도별 폐업 음식점수 그래프
#연도별 폐업 음식점수
close_trend <- foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제외
  group_by(close_year) %>%
  summarise(close_n=n())
#open_trend 구조
str(close_trend)
#연도별 개업 음식점수 막대그래프
ggplot(data=close_trend,aes(x=close_year,y=close_n))+
  geom_col()+
  xlab("연도") + ylab("폐업수")
#10번답. plot 확인인

#11.개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)#연도이름 변경
close_trend1<-rename(close_trend,year=close_year)#연도이름 변경

open_close_trend<-left_join(open_trend1,close_trend1,by="year")#통합

ggplot()+
  geom_line(data=open_close_trend, aes(year,open_n))+#개업그래프
  geom_line(data=open_close_trend, aes(year,close_n,color="red"))+#폐업그래프
  xlab("연도") + ylab("개수")
#11답.plot 확인인

#12.폐업음식점수가 개업음식점수보다 많았던 기간 확인
open_close_trend %>%
  filter(close_n>open_n)
#12답. 연도   개업    폐업
#1     2006   4739    5421
#2     2008   3995    4095

#13.영업중인 음식점수가 가장 많은 5개 구
district_business<-foodshop %>%
  filter(!is.na(open_date)&!is.na(district)&status=="영업") %>% #결측치제거
  group_by(district) %>%
  summarise(n=n())

district_business %>%
  arrange(desc(n)) %>%
  head(5)
#13답.  구역   개수수
#1      강남구   12554
#2      마포구    8477
#3      송파구    7401
#4      종로구    7226
#5      영등포    6951

#14,25개 구의 음식점수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n))+
  geom_col()+
  coord_flip()+#막대 90도회전
  xlab("영업구")+
  ylab("영업 음식점 수")
#14답. plot 확인인

#15.주요 업종별로 영업하는 음식점이 많은 구
foodshop %>%
  filter(!is.na(open_date)&!is.na(district)) %>% #결측치제거
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  filter(status=="영업") %>% #영업만 추출
  group_by(type,district) %>%
  summarise(n=n()) %>%
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  group_by(type) %>%
  filter(pct==max(pct))#type별 district비율이 가장 높은 데이터 추출
#15답. 종류     구역      개수   전체  비율율
#1    경양식    강남구    2240   9598  23.3
#2    기타      마포구    2749  20643  13.3
#3    분식      강남구     865   8689  10  
#4    일식      강남구     984   6601  14.9
#5    중국식    영등포     552   4882  11.3
#6    호프/통닭 마포구     643  10043   6.4