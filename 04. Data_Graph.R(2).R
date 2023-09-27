#히스토그램
hist(finedust$`3_ultrafine dust`, main="서울시 서대문구 2020년 1월초미세먼지 측정분포", col=terrain.colors(12))


#박스플롯 만들기
boxplot(finedust$`3_fine dust`, main="야식업의 2020년 1월미세먼지 발생현황", col="yellow")
boxplot(finedust$`3_fine dust`, finedust$`7_fine dust`, main="업종별 2020년 1월 미세먼지발생현황", col="yellow", names = c("야식업","중식"))

#산점도 연습
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의변화")
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의변화", pch=24, col="red", bg="yellow", cex=1.5)

#색깔,심볼 바꾸기
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의변화", pch=21, col="black", bg="green", cex=1.5)


#산점도 그래프 방식 바꿈
plot(x=finedust$`3_fine dust`, y=finedust$`3_ultrafine dust`, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의변화", type = "h")
