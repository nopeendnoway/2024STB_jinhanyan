#.패키지 설치 및 구동
library(dplyr)
library(ggplot2)
library(foreign)

#.데이터 불러오기
koweps24 <- read.spss("Koweps_h18_2023_beta1.sav")
koweps24 <- as.data.frame(koweps24)

#.변수 추출 후 이름 변경
welfare24 <-koweps24 %>%
  select(h1801_4,h1801_5,h1801_6,h18_reg5,h1808_114,h1808_122)

welfare24 <-welfare24 %>%
  rename(sex=h1801_4,
         birth=h1801_5,
         edu=h1801_6,
         reg=h18_reg5,
         p_salary=h1808_114,
         t_salary=h1808_122)

#.변수 유형 및 빈도 확인
str(welfare24)

#.결측치와 이상치 확인
summary(welfare24)

#상용직근로자의 최소 급여 0값 결측치 처리리
table(is.na(welfare24$p_salary))
welfare24$p_salary<-ifelse(welfare24$p_salary==0, NA, welfare24$p_salary)
table(is.na(welfare24$p_salary))

#일용직근로자의 최소 급여 0값 결측치 처리
table(is.na(welfare24$t_salary))
welfare24$t_salary<-ifelse(welfare24$t_salary==0, NA, welfare24$t_salary)
table(is.na(welfare24$t_salary))

#일용직 근로자의 최대 급여 결측치 처리
#table(is.na(welfare24$t_salary))
#welfare24$t_salary<-ifelse(welfare24$t_salary==64800, NA, welfare24$t_salary)
#table(is.na(welfare24$t_salary))

#변수 기초 정리
#sex
table(welfare24$sex)
welfare24$sex<-ifelse(welfare24$sex==1, "male","female")
table(welfare24$sex)

#age
welfare24$age<-2024-welfare24$birth+1
range(welfare24$age)

#edu
welfare24$edu_grade<-ifelse(welfare24$edu%in%c(1,2,3,4),"중학이하",
                        ifelse(welfare24$edu==5, "고교",
                               ifelse(welfare24$edu==6,"전문대","대학 이상")))
table(welfare24$edu_grade)

#상용직과 일용직의 평균 총급여 비교
mean(welfare24$p_salary,na.rm = T)
mean(welfare24$t_salary,na.rm = T)

#성별 평균 총급여 차이 검정
t.test(data=welfare24,p_salary~sex)

#성별 최대 총급여 상용직 근로자 찾기
welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(sex)%>%
  filter(p_salary==max(p_salary))%>%
  select(sex,age,edu,edu_grade,reg,p_salary)

#연령별 평균 총급여
age_salary1<-welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(age)%>%
  summarise(m=mean(p_salary))

age_salary1%>%
  arrange(desc(m))%>%
  head(3)

#연령별 평균 총급여 그래프 작성
ggplot(data=age_salary1,aes(x=age,y=m))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

#연령별 남녀 평균 총급여 그래프 작성
age_salary2<-welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(age,sex)%>%
  summarise(m=mean(p_salary))

ggplot(data=age_salary2,aes(x=age,y=m, col=sex))+
  geom_line()+
  xlab("연령")+
  ylab("총급여")

#교육수준별 상용직 평균 총급여 비교
edu_salary1<-welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(edu_grade)%>%
  summarise(m=mean(p_salary))

edu_salary1%>%
  arrange(desc(m))

ggplot(data=edu_salary1,aes(x=reorder(edu_grade,m),y=m))+
  geom_col()

#상용직 근로자의 교육수준과 성별 총급여 분석
edu_salary2<-welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(edu_grade,sex)%>%
  summarise(m=mean(p_salary))

edu_salary2%>%
  arrange(desc(m))

ggplot(data=edu_salary2,aes(x=edu_grade,y=m,fill=sex))+
  geom_col(position="dodge")+
  scale_x_discrete(limits=c("중학이하","고교","전문대","대학 이상"))

#권역별 평균 총급여 비교
region_salary<-welfare24%>%
  filter(!is.na(p_salary))%>%
  group_by(reg)%>%
  summarise(m=mean(p_salary))

region_salary%>%
  arrange(desc(m))

ggplot(data=region_salary,aes(x=reorder(reg,-m),y=m))+
  geom_col()
