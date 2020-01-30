library(dplyr)
EPI_data = read.csv('2010EPI_data.csv', skip=1)
#View(EPI_data)
attach(EPI_data)

plot(ecdf(EPI), do.points=FALSE, verticals = TRUE)
plot(ecdf(EPI), do.points = TRUE, verticals = TRUE) #points are visible on plot
par(pty="s")
qqnorm(EPI)
qqline(EPI)
x = seq(30, 95, 1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot")
qqline(x)

plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals = TRUE)
plot(ecdf(ECOSYSTEM), do.points = TRUE, verticals = TRUE) #points are visible on plot
par(pty="s")
qqnorm(ECOSYSTEM)
qqline(ECOSYSTEM)
x2 = seq(30, 95, 2)
qqplot(qt(ppoints(250),df=5),x2,xlab="Q-Q plot")
qqline(x2)

plot(ecdf(DALY), do.points=FALSE, verticals = TRUE)
plot(ecdf(DALY), do.points = TRUE, verticals = TRUE) #points are visible on plot
par(pty="s")
qqnorm(DALY)
qqline(DALY)
x3 = seq(30, 96, 2)
qqplot(qt(ppoints(250),df=5),x3,xlab="Q-Q plot")
qqline(x3)

multivariate = read.csv("multivariate.csv")
attach(multivariate)
mm = lm(Homeowners ~ Immigrant)
mm
summary(mm)$coef
plot(Homeowners ~ Immigrant)
abline(mm)
abline(mm, col=2, lwd=3)
newImmigrantData = data.frame(Immigrant=c(0,20))
mm %>% predict(newImmigrantData)
abline(mm)
abline(mm, col=3, lwd=3)
attributes(mm)
mm$coefficients

head(multivariate)

plot(Homeowners ~ Population)
mm2 = lm(Homeowners ~ Population)
summary(mm2)$coef
abline(mm2)

abline(mm2, col=3, lwd=3)
