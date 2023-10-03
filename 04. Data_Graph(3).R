# 1. X2023_STB_survey의 Gender 1개의 인자를 가지고 도수분포표를 작성하세요.
table(X2023_STB_survey $Gender)

# 2. X2023_STB_survey의 Gender 1개의 인자를 가지고 상대도수분포표를 작성하세요.
ECN <-table(X2023_STB_survey $Gender)
prop.table(ECN)

# 3. X2023_STB_survey의 Gender와 Grade 2개의 인자를 가지고 교차표를 작성하세요.
table(X2023_STB_survey $Gender, X2023_STB_survey $Grade)

# 4. X2023_STB_survey의 Nationality 1개의 인자를 가지고 막대그래프를 작성하세요.
barplot(table(X2023_STB_survey $Nationality),col=blues9)

# 5. X2023_STB_survey의 residential area 1개의 인자를 가지고 (가로)막대그래프를 작성하세요.
barplot(table(X2023_STB_survey $`residential area`),horiz=TRUE,col=terrain.colors(12))

# 6. X2023_STB_survey의 Gender와 Grade 2개의 인자를 가지고 막대그래프를 작성하세요.
barplot(table(X2023_STB_survey $Gender, X2023_STB_survey $Grade), legend=TRUE,col=blues9)

# 7. X2023_STB_survey의 Grade 1개의 인자를 가지고 파이차트를 작성하세요.
pie(table(X2023_STB_survey $Grade),col=blues9)

# 8. X2023_STB_survey의 Age 인자를 가지고 히스토그램을 작성하세요.
hist(X2023_STB_survey $Age, main="AGE",col=terrain.colors(8))

# 9. X2023_STB_survey의 Grade별 Age를 비교하는 박스 플롯을 만들어 보세요.그리고 Grade별 Age에 대한 기술통계분석을 실시하여 각 박스 플롯을 비교 설명하세요.
boxplot (Age ~ Grade, X2023_STB_survey, ylim=c(18,28), ylab="AGE", xlab="GRADE" ,main="",col=blues9)
# 이로도 똑같은 결과 나온다.
boxplot(X2023_STB_survey$Age~X2023_STB_survey$Grade)

#기술통계
tapply(X2023_STB_survey$Age, X2023_STB_survey$Grade, summary)

# 10. X2023_STB_survey의 Grade를 X값으로 Age를 Y값으로 하는 산점도를 만들어 보세요.
plot(x=X2023_STB_survey$Grade, y=X2023_STB_survey$Age, xlab="Grade", ylab="Age", main="Grade and Age", pch=23, col="purple",bg="yellow", cex=1.0)



