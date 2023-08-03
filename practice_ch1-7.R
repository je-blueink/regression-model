#---- 1장 단순회귀모형 ----
X <- c(0, 0, 3, 3, 6, 6, 9, 9, 12, 12)
Y <- c(8.5, 8.4, 7.9, 8.1, 7.8, 7.6, 7.3, 7.0, 6.8, 6.7)
freshness <- cbind(Y, X)
freshness.df <- data.frame(freshness)

# 단순회귀
freshness.lm <- lm(Y~X, data = freshness.df)
summary(freshness.lm)

# 가중회귀
w <- 1/X
w.lm <- lm(Y~X, weights = w)
summary(w.lm)

# "0"을 없애고 계산
x <- X+1
w <- 1/x
w.lm <- lm(Y~x, weights = w)
summary(w.lm)

q.val <- qt(0.975,8)
b1.under <- -0.1400 - q.val*0.0083
b1.upper <- -0.1400 + q.val*0.0083
b1 <- list(b1.under, b1.upper)
b1

#---- 2장 중회귀모형 ----
# 데이터 입력
water <- read.table("C:/Users/bluei/Rstudio_data/water.txt", sep=",", header=T)
head(water)

# 변수 설정
X <- water[, c(2:4)]
X <- cbind(1, X) # 절편 추가
Y <- water[, 1]

# 행렬 만들기
X <- as.matrix(X)
Y <- as.matrix(Y)
X
Y

# beta 행렬 구하기
XtX <- t(X) %*% X
XtXi <- solve(XtX)
XtY <- t(X) %*% Y
beta <- XtXi %*% XtY
beta <- round(beta, 3)
beta

# 회귀모형 적합
water.lm <- lm(Y~X1+X2+X3, data = water)
summary(water.lm)

# 분산분석표 작성
anova(water.lm)

# 유의하지 않은 변수 제거
fit.con <- lm(Y~X1+X2+X3, data = water)
fit.for <- step(fit.con, direction = "forward") #전진소거법
summary(fit.for)
fit.back <- step(fit.con, direction = "backward") #후진소거법
summary(fit.back)
fit.both <- step(fit.con, direction = "both") # 단계선택법
summary(fit.both)

# 회귀진단
par(mfrow=c(2,2))
plot(water.lm)
par(mfrow=c(2,2))
plot(fit.both)

# 물소비량 추정
x <- c(20, 27, 60)
Yhat <- 2.409 + 0.070*x[1] - 0.025*x[2] + 0.006*x[3]
Yhat
y_hat <- 1.956 + 0.079*x[1]
y_hat

y.data <- data.frame(X1=20, X2=27, X3=60)
y.hat <- predict(water.lm, int="c", newdata = y.data)
y.hat

#---- 3장 변수선택 ----
# 패키지 준비
install.packages("leaps")
library(leaps)

# 데이터 읽기
fuel_data <- read.csv("C:/Users/bluei/Rstudio_data/p118.csv", header = T)
head(fuel_data, 3)

# forward selection
start.lm <- lm(Y~1, data=fuel_data)
full.lm <- lm(Y~., data=fuel_data)
step(start.lm, scope = list(lower=start.lm, upper=full.lm), direction = "forward")

# backward selection
step(full.lm, data=fuel_data, direction = "backward")

# stepwise selection
step(start.lm, scope = list(upper=full.lm), data=fuel_data, direction = "both")

# all subset regression
all.lm <- regsubsets(Y~., data=fuel_data)
summary(all.lm)

rs <- summary(all.lm)
names(rs)
plot(all.lm, scale = "adjr2")
data.frame(rs$rsq, rs$adjr2, rs$cp)

model6.lm <- lm(Y~X4+X5+X6+X8+X9+X10, data = fuel_data)
summary(model6.lm)

model5.lm <- lm(Y~X4+X7+X8+X9+X10, data = fuel_data)
summary(model5.lm)

model4.lm <- lm(Y~X4+X8+X9+X10, data = fuel_data)
summary(model4.lm)

model7.lm <- lm(Y~X1+X4+X5+X6+X8+X9+X10, data = fuel_data)
summary(model7.lm)

model3.lm <- lm(Y~X5+X6+X8, data = fuel_data)
summary(model3.lm)

#---- 4장 모형개발 ----
# 데이터 준비
salary.data <- read.csv("p138.csv", header = T)
head(salary.data)

# 숫자변수를 인자변수로 변경
salary.data$sex <- factor(salary.data$sex, levels = c(0,1), labels = c("male", "female"))
head(salary.data)

# 월급액과 근속연수에 대한 산점도
plot(salary.data$SL, salary.data$YR, type = "n")
points(salary.data$SL[salary.data$sex=="male"], 
       salary.data$YR[salary.data$sex=="male"], 
       pch=17, col="BLUE")
points(salary.data$SL[salary.data$sex=="female"], 
       salary.data$YR[salary.data$sex=="female"], 
       pch=19, col="RED")
legend("bottomright", legend = levels(salary.data$sex), 
       pch = c(17, 19), col = c("BLUE", "RED"))

# 월급액에 대하여 근속연수와 성별을 설명변수로 하는 회귀모형 적합
salary.lm <- lm(SL~YR+sex, data = salary.data)
summary(salary.lm)

#---- 5장 자료진단 ----
# 데이터 입력
hdata <- read.csv("p162.csv")
head(hdata, 3)

# 회귀모형 적합
hdata.lm <- lm(Y~., data = hdata)
summary(hdata.lm)

# 분산분석표 작성
anova(hdata.lm)

# 잔차분석
hdata.res <- ls.diag(hdata.lm)
help("ls.diag")
names(hdata.res)
res.result <- cbind(hdata.res$std.res, hdata.res$stud.res, hdata.res$hat)
colnames(res.result) <- c("표준화잔차", "스튜던트화잔차", "H_ii")
res.result <- round(res.result, 3)
print(res.result)

# 스튜던트화 잔차 확인
rstudent(hdata.lm)

# 스튜던트화 잔차와 예측값에 대한 산점도
plot(hdata.lm$fitted.values, hdata.res$stud.res, pch=19)
identify(hdata.lm$fitted.values, hdata.res$stud.res)

# 특이점 검정
install.packages("car")
library(car)
outlierTest(hdata.lm)

# Bonferroni 유의수준 0.01에서 기각치
qt(0.01/2*30, 25)

# Bonferroni 유의수준 0.05에서 기각치
qt(0.05/2*30, 25)

#---- 6장 모형진단 ----

getwd()

# 데이터 준비
church <- read.csv("p187.csv")
head(church)
colnames(church) <- c("no", "X", "Y")
head(church)

# 회귀모형 적합
church.lm <- lm(Y ~ X, data = church)
summary(church.lm)

# 산점도와 잔차산점도
plot(church$X, church$Y, pch=15)
abline(church.lm)
title("X-Y 산점도")
plot(church.lm$fitted.values, church.lm$residuals, pch=19) # 오차의 등분산 가정 진단
title("Y추정값-잔차 산점도") 

# 스코어 검정 - 오차의 등분산 가정 진단
library(car)
ncvTest(church.lm)

# 오차의 정규성 가정 진단
qqPlot(church.lm) #정규확률그림

install.packages("mvnormtest")
library(mvnormtest)
church.rstd <- rstudent(church.lm) #스튜던트화 잔차 구하기
shapiro.test(church.rstd)

# Box-Cox 변환
library(MASS)
church.bc <- data.frame(boxcox(Y~X, data=church, lambda = seq(-2, 2, 0.1)))
head(church.bc)
(lambda1 <- church.bc[which.max(church.bc[,2]),])
(lambda1 <- lambda1$x)

# 다른 함수로 box-cox 변환 시도
install.packages("forecast")
library(forecast)
(lambda2 <- BoxCox.lambda(church$Y, method = "loglik"))

# lambda 적용
new.church1 <- BoxCox(church, lambda1)
church.lm1 <- lm(Y~X, data = new.church1)
summary(church.lm1)
ncvTest(church.lm1)

new.church2 <- BoxCox(church, lambda2)
church.lm2 <- lm(Y~X, data = new.church2)
summary(church.lm2)
ncvTest(church.lm2)

head(new.y1)
new.church <- data.frame()
plot(new.church$X, new.church$Y, pch=15)
abline(church.lm2)
title("변환한 X-Y 산점도")
plot(church.lm2$fitted.values, church.lm2$residuals, pch=19) # 오차의 등분산 가정 진단
title("변환한 Y추정값-잔차 산점도") 
qqPlot(church.lm2)

#---- 7장 일반화선형모형 ----
getwd()

# 데이터 준비
glider = read.csv("sugar_glider_binomial.csv")
head(glider)

# 모형적합
model1 = glm(occurr ~ con_metric, 
             family = binomial(link = logit), data = glider)
summary(model1)

# 유의성 검정
model0 = glm(occurr ~ 1, family = binomial(link = logit), data = glider)
anova(model0, model1, test = "Chisq")

# 모형의 적합도 평가
1-pchisq(68.889, 48)

#---- 마무리 ----
help("BoxCox")
help("BoxCox.lambda")
help("boxcox")
rm(list = ls())