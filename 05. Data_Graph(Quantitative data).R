#히스토그램 작성1
hist(dust$pm25, main="서울시 서대문구 2022년 1월 초미세먼지 측정분포", col=terrain.colors(12))

#히스토그램 작성2:확률밀도 그래프
hist(dust$pm25, main="서울시 서대문구 2022년 1월 초미세먼지 측정분포", col=terrain.colors(12), freq = FALSE)

#히스토그램 작성3:확률밀도 선 추가
lines(density(dust$pm25), lwd=2)

#박스플롯 작성1
boxplot(dust$X3pm25, main="야식업의 2022년 1월 미세먼지 발생현황", col="yellow")

#박스플롯 작성2:병렬
boxplot(dust$X3pm25, dust$X7pm25, main="업종별 2022년 1월 미세먼지 발생현황", col="yellow", names = c("야식업","중식"))

#산점도1
plot(x=dust$pm10, y=dust$pm25, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의 변화")

#산점도2
plot(x=dust$pm10, y=dust$pm25, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의 변화", pch=24, col="red", bg="yellow", cex=1.5)

#산점도3
plot(x=dust$pm10, y=dust$pm25, xlab="미세먼지", ylab="초미세먼지", main="미세먼지와 초미세먼지의 변화", type = "h")
