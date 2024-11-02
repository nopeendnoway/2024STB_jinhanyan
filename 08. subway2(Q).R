#실습에 필요한 packages를 라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#결측치 갯수 확인
is.na(congestion)
sum(is.na(congestion))
colSums(is.na(congestion))
#결측치가 있는 행을 제거한 새로운 데이터 프레임 생성
#6시 출발기차의 결측치를 제거
congestion1 <- congestion[!is.na(congestion$s0600),]
colSums(is.na(congestion1))
#23시 30분 출발기차의 결측치를 제거
congestion1 <- congestion1[!is.na(congestion1$s2330),]
colSums(is.na(congestion1))
#남은 결측치를 0으로 대체
congestion1[is.na(congestion1)] <- 0
colSums(is.na(congestion1))

#이상치 확인
ggplot(congestion1, aes(y=s0530))+
  geom_boxplot()
summary(congestion1$s0530)

#파생변수만들기
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <- rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#데이터분석
#1.  수도권 지하철의 하루 평균 혼잡도？
mean_congestion <- mean(congestion1$day_mean)
mean_congestion

#2. 호선별 하루평균혼잡도？
line_mean_congestion <- congestion1 %>%
  group_by(line) %>%
  summarise(mean_congestion = mean(day_mean))
line_mean_congestion

#2. 호선별 출근시간(07:00~09:00)의 혼잡도 평균？
# 计算每个线路在07:00到09:00之间的平均拥挤度
# 假设congestion1数据集中有表示拥挤度的列，如s0700, s0730, s0800, s0830, s0900
# 并且这些列的值表示了对应时间点的拥挤度

morning_avg_congestion <- congestion1 %>%
  # 确保没有缺失值
  filter(complete.cases(s0700, s0730, s0800, s0830, s0900)) %>%
  # 按线路分组
  group_by(line) %>%
  # 计算每个时间点的平均拥挤度，并计算整个时间段的平均值
  summarise(
    avg_0700 = mean(s0700, na.rm = TRUE),
    avg_0730 = mean(s0730, na.rm = TRUE),
    avg_0800 = mean(s0800, na.rm = TRUE),
    avg_0830 = mean(s0830, na.rm = TRUE),
    avg_0900 = mean(s0900, na.rm = TRUE),
    avg_morning = mean(c(s0700, s0730, s0800, s0830, s0900), na.rm = TRUE)
  )

# 查看结果
morning_avg_congestion
#2-1. 호선별 출근시간(07:00~09:00)의 기술통계？
morning_congestion_stats <- congestion1 %>%
  select(line, s0700, s0730, s0800, s0830, s0900) %>%
  group_by(line) %>%
  summarise(
    mean_morning = mean(c_across(everything())),
    sd_morning = sd(c_across(everything())),
    min_morning = min(c_across(everything())),
    max_morning = max(c_across(everything())),
    median_morning = median(c_across(everything()))
  )
morning_congestion_stats

#2-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기？
ggplot(morning_congestion_stats, aes(x = line, y = mean_morning, fill = line)) +
  geom_bar(stat = "identity") +
  labs(title = "호선별 출근시간 평균혼잡도", x = "호선", y = "평균혼잡도")

#2-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역？
max_line <- max(morning_congestion_stats$mean_morning)
max_line_congestion <- congestion1 %>%
  filter(line == which(morning_congestion_stats$mean_morning == max_line)) %>%
  select(station, s0700, s0730, s0800, s0830, s0900) %>%
  group_by(station) %>%
  summarise(
    mean_station = mean(c_across(everything()))
  ) %>%
  arrange(desc(mean_station))
max_line_congestion


#3.08시 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  select(s80_grade,n,pct)%>%
  arrange(desc(n))

#3-1. 호선별로 08시 지하철 혼잡도 범주화
congestion1 %>%
  mutate(s80_grade=ifelse(s0800<=80, "good", ifelse(s0800<=130, "normal", ifelse(s0800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>%
  summarise(n=n())%>%
  mutate(total=sum(n), pct=round(n/total*100,1))%>%
  filter(s80_grade=="caution")%>%
  select(line, s80_grade,n,pct)%>%
  arrange(desc(pct))%>%
  head(5)

#4. 호선별 퇴근시간(18:00~20:00)의 혼잡도 평균？
evening_congestion_mean <- congestion1 %>%
  select(line, s1700, s1730, s1800, s1830, s1900) %>%
  group_by(line) %>%
  summarise(mean_evening = mean(c_across(everything())))
evening_congestion_mean
#4-1. 호선별 퇴근시간(18:00~20:00)의 기술통계？
evening_congestion_stats <- congestion1 %>%
  select(line, s1700, s1730, s1800, s1830, s1900) %>%
  group_by(line) %>%
  summarise(
    mean_evening = mean(c_across(everything())),
    sd_evening = sd(c_across(everything())),
    min_evening = min(c_across(everything())),
    max_evening = max(c_across(everything())),
    median_evening = median(c_across(everything()))
  )
evening_congestion_stats
#4-2. 평균혼잡도가 가장 높은 시간대를 막대그래프로 그리기？
ggplot(evening_congestion_stats, aes(x = line, y = mean_evening, fill = line)) +
  geom_bar(stat = "identity") +
  labs(title = "Average amount of congestion during rush hour by line", x = "line", y = "Average Confusion")

#4-3. 평균혼잡도가 가장 높은 호선에서 기여도가 높은 역？
max_line_evening <- max(evening_congestion_stats$mean_evening)
max_line_congestion_evening <- congestion1 %>%
  filter(line == which(evening_congestion_stats$mean_evening == max_line_evening)) %>%
  select(station, s1700, s1730, s1800, s1830, s1900) %>%
  group_by(station) %>%
  summarise(
    mean_station = mean(c_across(everything()))
  ) %>%
  arrange(desc(mean_station))
max_line_congestion_evening
