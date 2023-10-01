#2step. gender인자 한개로 도수분포표 작성
table(X_Week4_2023_STB_survey_2_$Gender)

#3step. gender인자 한개로 상대도수분포표 작성
ECN <- table((X_Week4_2023_STB_survey_2_$Gender))
prop.table(ECN)

#4step. gender와grade 2개의 인자를 가지고 교차표 작성
table((X_Week4_2023_STB_survey_2_$Gender), (X_Week4_2023_STB_survey_2_$Grade))

#5step. nationality 1개 인자 가지고 막대그래프 작성
barplot(table(X_Week4_2023_STB_survey_2_$Nationality))

#6step. residential area 1개 인자로 (가로)막대그래프 작성
barplot(table(X_Week4_2023_STB_survey_2_$`residential area`), xlab = "인원", ylab = "지역", xlim=c(0,40), horiz=TRUE)

#7step. gender와grade 2개 인자가지고 막대그래프 작성
entry <- table(X_Week4_2023_STB_survey_2_$Gender, X_Week4_2023_STB_survey_2_$Grade)
barplot(entry, legend = TRUE)


#8step. grade 1개 인자가지고 파이차트 작성
pie(table(X_Week4_2023_STB_survey_2_$Grade))

#9step. age 인자 가지고 히스토그램 작성
hist(X_Week4_2023_STB_survey_2_$Age, main="경영통계분석2 4주차 실습", col=terrain.colors(12))


#10step. grade별 age를 비교하는 박스플롯 만들기
boxplot(X_home_2grade$Age, X_home_3grade$Age, X_home_4grade$Age, main=" 학년과 나이의 관계", col="yellow", names = c("2","3","4"))



#11step. grade를 x값, age를 y값으로 하는 산점도 만들기
plot(x=X_Week4_2023_STB_survey_2_$Grade, y=X_Week4_2023_STB_survey_2_$Age, xlab="학년", ylab="나이", main="학년과 나이의 관계")

