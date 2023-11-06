#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#데이터를 새객체 foodshop으로 불러오기
#비어있는 셀은 결측치 처리/파라미터 문자형으로 변환
foodshop <- read.csv("ggd_food_2023.csv", na="", stringsAsFactors = F)

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
table(is.na(foodshop$open_date))#결측치 64개
#na값 제외
foodshop <- foodshop %>%
  filter(open_date != '') %>%
  select(name,type,status,open_date,close_date,address)
foodshop$open_year<-substr(foodshop$open_date,1,4)#인허가년도 변수 생성

#4.close_date변수
range(foodshop$close_date, na.rm = T)
foodshop$close_year<-substr(foodshop$close_date,1,4)#인허가년도 변수 생성

#5.address변수
foodshop$district<-substr(foodshop$address,5,7)#구 정보를 분리하여 변수 생성
table(foodshop$district)#이상치 확인
foodshop$district <- ifelse(foodshop$district%in%c(" 밀양"," 영암","106","번지","시 강","시 계","시 관","시 금","시 남","시 노","시 마","시 마","시 미","시 서","시 용","시 은","시흥시","회","여주군"),NA,foodshop$district)#이상치제거
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
#1답. 이름  업종  개업일     주소
#   영동집  한식  19601130   경기도 오산시 원동 763-9


#2.주요 업종별로 가장 오래 영업중인 음식점
foodshop %>%
  filter(!is.na(open_date)&status=="영업") %>% #결측치제거, 영업데이터 추출
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type) %>%#업종별 분류
  filter(open_date==min(open_date)) %>% #개업일이 가장 빠른 데이터 추출
  select(name, type, open_date, address)
#2답. 이름     업종   개업일     주소
#1 지동관      중국식     19670214  경기도 의정부시 의정부동 196-14…
#2 비어팝경양식 경양식     19730411 경기도 의정부시 의정부동 193번지
#3 바다세상     일식       19781211 경기도 평택시 통복동 67-21번지  
#4 주막거리     호프/통닭  19671019 경기도 이천시 관고동 4-31번지   
#5 사사육(446)  기타       19720320 경기도 남양주시 조안면 능내리 … 
#6 일번지손칼국수전문점 분식19721129 경기도 성남시 중원구 금광동 1번…


#3.업종별 개업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(10)
#3답.    업종            개수   전체   비율율
#1   한식               194803 422924  46.1
#2   기타                49710 422924  11.8
#3   호프/통닭           47984 422924  11.3
#4   분식                27790 422924   6.6
#5   경양식              18413 422924   4.4
#6   식육(숯불구이)      16361 422924   3.9
#7   중국식              13813 422924   3.3
#8   통닭(치킨)          12681 422924   3  
#9   일식                11017 422924   2.6
#10  정종/대포집/소주방   6986 422924   1.7


#4.영업 중인 음식점의 업종별 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(status=="영업") %>% #영업만 추출
  group_by(type) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) %>% #범주별비율계산
  arrange(desc(n)) %>%
  head(5)
#4답.    업종         개수  전체    비율율
#1    한식           62688 143422  43.7
#2    기타           20768 143422  14.5
#3    호프/통닭      14276 143422  10  
#4    식육(숯불구이)  8527 143422   5.9
#5    분식            7173 143422   5  



#5.전체 음식점의 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  group_by(status) %>%
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1)) #범주별비율계산
#5답.상태     개수   전체    비율율
#1    영업   143422 422924  33.9
#2    폐업   279502 422924  66.1


#6.주요 업종별 영업과 폐업 비율
foodshop %>%
  filter(!is.na(open_date)&!is.na(type)&!is.na(district)) %>% #결측치제외
  filter(type%in%c("기타","경양식","분식","일식","중국식","호프/통닭"))%>%
  group_by(type,status) %>%#교차차 분류
  summarise(n=n()) %>% #범주빈도계산
  mutate(total=sum(n),pct=round(n/total*100,1))%>% #범주별비율계산
  filter(status=="영업") %>% #영업만 추출
  arrange(desc(n))
#6답.  업종     상태   개수   전체  비율율
#1    기타      영업   20768 49710  41.8
#2    호프/통닭 영업   14276 47984  29.8
#3    분식      영업    7173 27790  25.8
#4    경양식    영업    6214 18413  33.7
#5    중국식    영업    5781 13813  41.9
#6    일식      영업    4329 11017  39.3


#7.개업이 많았던 연도
foodshop %>%
  filter(!is.na(open_date)&!is.na(district))%>% #결측치제외
  group_by(open_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#7답.   연도   개점수 
#1      2001 17090
#2      2002 16488
#3      2021 16088
#4      2020 15680
#5      2003 15619


#8.폐업이 많았던 연도
foodshop %>%
  filter(!is.na(close_date)&!is.na(district))%>% #결측치제외
  group_by(close_year) %>%
  summarise(n=n()) %>% #범주빈도계산
  arrange(desc(n)) %>%
  head(5)
#8답.    연도  폐점수
#1       2005 14400
#2       2006 13551
#3       2022 13548
#4       2018 12796
#5       2019 12382


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
#9.plot 확인


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
#10.plot 확인

#11.개업과 폐업 음식점 통합 그래프
open_trend1<-rename(open_trend,year=open_year)#연도이름 변경
close_trend1<-rename(close_trend,year=close_year)#연도이름 변경

open_close_trend<-left_join(open_trend1,close_trend1,by="year")#통합

ggplot()+
  geom_line(data=open_close_trend, aes(year,open_n))+#개업그래프
  geom_line(data=open_close_trend, aes(year,close_n,color="red"))+#폐업그래프
  xlab("연도") + ylab("개수")
#11.plot 확인 

#12.폐업음식점수가 개업음식점수보다 많았던 기간 확인
open_close_trend %>%
  filter(close_n>open_n)
#12답. 기간  개업    폐업
#1    2005  12502   14400
#2    2006  12156   13551
#3    2008  11164   11326


#13.영업중인 음식점수가 가장 많은 5개 구
district_business<-foodshop %>%
  filter(!is.na(open_date)&!is.na(district)&status=="영업") %>% #결측치제거
  group_by(district) %>%
  summarise(n=n())

district_business %>%
  arrange(desc(n)) %>%
  head(5)
#13답. 지역     점포개수 
#1    수원시   12880
#2    화성시   10222
#3    고양시   10037
#4    성남시    9320
#5    용인시    8672


#14,25개 구의 음식점수 막대그래프
ggplot(data = district_business, aes(x=reorder(district,n),y=n))+
  geom_col()+
  coord_flip()+#막대 90도회전
  xlab("영업구")+
  ylab("영업 음식점 수")
#14.plot 확인 

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
#15. 업종     지역   점포개수 전체  비율
#1  경양식    성남시     735  6214  11.8
#2  기타      수원시    3129 20768  15.1
#3  분식      수원시     625  7173   8.7
#4  일식      고양시     462  4329  10.7
#5  중국식    수원시     450  5781   7.8
#6  호프/통닭 수원시    1368 14276   9.6
