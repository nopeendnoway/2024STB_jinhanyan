
salary=c(45,50,50,55,55,60,60,65,70,90) #연봉데이터
com2=combn(salary,2) #연봉데이터에서 2개씩 선택해서 데이터로 생성, com2에 저장
print(com2)

#표본의 크기2로 추출된 데이터의 평균을 구한 뒤 막대그래프 작성
com2t=t(com2) #데이터의 전치치
z=rowMeans(com2t) #전치된 데이터의 행평균 계산
barplot(table(z),xlab="average annual salary",ylab="frequency",main="distribution of average annual salary") #barplot(table(z),xlab="연봉편균",ylab="도수",main="연봉평균의 분포")

#표본평균의 기댓값 구하기
meanofz=mean(z) #z의 평균
print(meanofz)

#표본의 크기를 늘려 정규분포에 가까워지는지 확인하기
#sample size=2
data50=c(1:50)#데이터
com2=combn(data50,2)#데이터에서 2개씩 선택하여 데이터 생성
com2t=t(com2)#데이터의 전치
z2=rowMeans(com2t)#전치된 데이터의 행평균 계산
meanofz2=mean(z2)#기됏값을 계산하여 조장
print(meanofz2)
barplot(table(z2),xlab="Average2",ylab="frequency",main="requency distribution table of sample size 2") #fbarplot(table(z2),xlab="Average2",ylab="frequency",main="표본크기2의 도수분포포") 

#sample size=3
com3=combn(data50,3)#데이터에서 3개씩 선택하여 데이터 생성
com3t=t(com3)#데이터의 전치
z3=rowMeans(com3t) #전치된 데이터의 행평균 계산
meanofz3=mean(z3)#기댓값을 계산하여 저장
print(meanofz3)
barplot(table(z3),xlab="average3",ylab="frequency",main="requency distribution table of sample size 3")


com5=combn(data50,5)#데이터에서 5개씩 선택하여 데이터 생성
com5t=t(com5)#데이터의 전치
z5=rowMeans(com5t)#전치된 데이터의 행평균 계산
meanofz5=mean(z5)#기댓값을 계산하여 저장
print(meanofz5)
barplot(table(z5),xlab="average5",ylab="frequency",main="requency distribution table of sample size 5")

#표본추출방법: 단순무작위추출법
sampling=c(1:100)
print(sampling)
sample(x=sampling,size=10)

#표본추출방법: 충화추출법
install.packages("sampling")
library(sampling)
sam2=strata(data=iris,stratanames=c("Species"),size=c(3,3,3),method='srswor')#iris데이터 사용하여 표본추출, Species층을 기준으로
sample2=getdata(data=iris,m=sam2)#추출된데이터 저장
print(sample2)
