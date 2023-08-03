# ---- 단순회귀모형 연습 ----

# 자료 읽기 
getwd()
m.data <- read.table("machine.txt", header = T)
head(m.data)

# 산점도 그리기
plot(m.data$age, m.data$cost, pch=19)

# 회귀모형 적합하기
machine.lm <- lm(cost ~ age, data = m.data)
summary(machine.lm)

# 분산분석표 구하기
anova(machine.lm)

# 잔차 및 추정값 보기
names(machine.lm)
cbind(m.data, machine.lm$residuals, machine.lm$fitted.values)

# 잔차그림 그리기
plot(m.data$age, machine.lm$residuals, pch=19)
abline(h=0, lty=2)
identify(m.data$age, machine.lm$residuals, tolerance = 2)

plot(machine.lm$fitted.values, machine.lm$residuals, pch=19)
abline(h=0, lty=2)

# 추정값의 신뢰대 그리기
p.frame <- data.frame(age = c(1:15))
pc <- predict(machine.lm, interval = "confidence", newdata = p.frame)
head(pc)
pred.age <- p.frame$age
plot(m.data$age, m.data$cost, ylim = range(m.data$cost, pc))
matlines(pred.age, pc, lty = c(1,2,2), col = "RED")
identify(m.data$age, m.data$cost, tolerance = 1)

# ---- 중회귀모형 연습 ----
# 자료파일 읽기
getwd()
install.packages("readxl")
library(readxl)
water.data <- read_xlsx("water.xlsx")
head(water.data)

# 기술통계량 및 상관계수 보기
summary(water.data)
cor(water.data)

# 회귀모형 적합하기
water.lm <- lm(Y ~ X1+X2+X3, data = water.data)
summary(water.lm)

library(car)
avPlots(water.lm)

water.lm1 <- lm(Y ~ X1, data = water.data)
summary(water.lm1)

# 분산분석표
anova(water.lm2)
anova(water.lm1)

# 잔차산점도 그리기
par(mfrow=c(2,2))
plot(water.data$X1, water.lm$residuals)
plot(water.data$X2, water.lm$residuals)
plot(water.data$X3, water.lm$residuals)
identify(water.data$X1, water.lm$residuals, tolerance = 1)
identify(water.data$X2, water.lm$residuals, tolerance = 1)
identify(water.data$X2, water.lm$residuals, tolerance = 1)


par(mfrow=c(1,2))
plot(water.lm$fitted.values, water.lm$residuals)
abline(h=0, lty=2)
identify(water.lm$fitted.values, water.lm$residuals, tolerance = 1)
plot(water.lm1$fitted.values, water.lm1$residuals)
abline(h=0, lty=2)
identify(water.lm1$fitted.values, water.lm1$residuals, tolerance = 1)

# ---- help ----
help("predict")
help("matlines")
help("identify")
rm(list = ls())