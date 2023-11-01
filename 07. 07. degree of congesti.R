#실습에 필요한 packages를 설치
install.packages("dplyr")
install.packages("ggplot2")

#실습에 필요한 packages를라이브러리에 등록
library(dplyr)
library(ggplot2)

#CSV형식의 파일 불러와서 congestion객체에 입력하고 구조 확인
str(congestion)

#변수의 이상치와 결측치 확인하고 처리
summary(congestion)
#NA’s(결측치)가 미표시됨

#결측치 개수 확인
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
ggplot(congestion1, aes(y=s0530))+geom_boxplot()

summary(congestion1$s0530)

#분석목적에 따른 파생변수 만들기
#파생변수1.정수형 day_mean변수
#1.지하철역의 하루 평균 혼잡도
congestion1$day_mean <-
  rowMeans(congestion1[,c('s0530','s0600','s0630','s0700','s0730','s0800','s0830','s0900','s0930','s1000','s1030','s1100','s1130','s1200','s1230','s1300','s1330','s1400','s1430','s1500','s1530','s1600','s1630','s1700','s1730','s1800','s1830','s1900','s1930','s2000','s2030','s2100','s2130','s2200','s2230','s2300','s2330')])

#수도권 지하철의 하루 평균 혼잡도
summary(congestion1$day_mean)
mean(congestion1$day_mean)


#2 지하철 호선별 하루 평균 혼잡도 
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(day_mean))%>%
  arrange(desc(m))%>%
  head(10)
head(passenger10,10)

ggplot(data=passenger10, aes(x=reorder(line, m), y=m))+
  geom_col()+
  coord_flip()

#지하철 호선별 출근시간별7시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s0700))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#지하철 호선별 출근시간별7시30분
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s0730))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#지하철 호선별 출근시간별8시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s0800))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

ggplot(data=passenger10, aes(x=reorder(line, m), y=m))+
  geom_col()+
  coord_flip()

#지하철 호선별 출근시간별8시30분
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s0830))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#지하철 호선별 출근시간별9시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s0900))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#기술통계분석
summary(congestion1)

# 지하철 호선별, 시간대(7-9시)별 평균 혼잡도 계산
average_congestion_by_hour <- colMeans(congestion1[,c('s0700','s0730','s0800','s0830','s0900')])

# 가장 높은 평균 혼잡도를 가진 시간대 찾기
max_congestion_hour <- which.max(average_congestion_by_hour)

# 해당 시간대 이름 찾기
hour_names <- names(average_congestion_by_hour)
max_congestion_hour_name <- hour_names[max_congestion_hour]

max_congestion_hour_name


# 출근 시간대의 평균 혼잡도 계산
congestion1$day_work <- rowMeans(congestion1[,c('s0700','s0730','s0800','s0830','s0900')])

# 상위 4개 호선 선택
top_lines <- congestion1 %>%
  group_by(line) %>%
  summarise(avg = mean(day_work)) %>%
  arrange(desc(avg)) %>%
  head(4)

# 선택된 상위 4개 호선의 역별 기여도 계산
# 라인이 7이거나 2이거나 4이거나 8인 역의 역별 기여도
station_contributions_호선 <- congestion1 %>%
  filter(line == "7" | line == "2" | line == "4" | line == "8") %>%
  group_by(line, station) %>%
  summarise(total_contrib = sum(day_work)) %>%
  arrange(desc(total_contrib))
station_contributions_호선


#출발시간 8시의 지하철 혼잡도 범주화/범주별 빈도분석
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

# 지하철 호선별, 시간대(18-20시)별 평균 혼잡도 계산
average_congestion_by_hour_night <- colMeans(congestion1[,c('s1800','s1830','s1900','s1930','s2000')])

# 가장 높은 평균 혼잡도를 가진 시간대 찾기
max_congestion_hour_night <- which.max(average_congestion_by_hour_night)

# 해당 시간대 이름 찾기
hour_names_night <- names(average_congestion_by_hour_night)
max_congestion_hour_name_night <- hour_names_night[max_congestion_hour_night]

max_congestion_hour_name_night

#지하철 호선별 퇴근시간18시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s1800))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

ggplot(data=passenger10, aes(x=reorder(line, m), y=m))+
  geom_col()+
  coord_flip()

#지하철 호선별 퇴근시간18시30분
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s1830))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)


#지하철 호선별 퇴근시간19시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s1900))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#지하철 호선별 퇴근시간19시30분
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s1930))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)

#지하철 호선별 퇴근시간20시
passenger10 <-congestion1 %>%
  group_by(line)%>%
  summarise(m=mean(s2000))%>%
  arrange(desc(m))%>%
  head(8)
head(passenger10, 8)


# 퇴근 시간대의 평균 혼잡도 계산
congestion1$day_night <- rowMeans(congestion1[,c('s1800','s1830','s1900','s1930','s2000')])

# 상위 4개 호선 선택
top_lines <- congestion1 %>%
  group_by(line) %>%
  summarise(avg = mean(day_night)) %>%
  arrange(desc(avg)) %>%
  head(4)
top_lines

# 선택된 상위 4개 호선의 역별 기여도 계산
# 라인이 2이거나 7이거나 4이거나 3인 역의 역별 기여도
station_contributions_night_호선 <- congestion1 %>%
  filter(line == "2" | line == "7" | line == "4" | line == "3") %>%
  group_by(line, station) %>%
  summarise(total_contrib = sum(day_night)) %>%
  arrange(desc(total_contrib))
station_contributions_night_호선


#출발시간 18시의 지하철 혼잡도 범주화/범주별 빈도분석
congestion1 %>%  
  mutate(s80_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(s80_grade) %>% 
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  select(s80_grade,n,pct)%>%  
  arrange(desc(n))

#3-1. 호선별로 18시 지하철 혼잡도 범주화
congestion1 %>%  
  mutate(s80_grade=ifelse(s1800<=80, "good", ifelse(s1800<=130, "normal", ifelse(s1800<=150, "caution", "bad"))))%>%
  group_by(line, s80_grade) %>% 
  summarise(n=n())%>%  
  mutate(total=sum(n), pct=round(n/total*100,1))%>%  
  filter(s80_grade=="bad")%>%  
  select(line, s80_grade,n,pct)%>%  
  arrange(desc(pct))%>%  
  head(5)
