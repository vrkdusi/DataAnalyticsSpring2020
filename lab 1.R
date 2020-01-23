help("data.frame")
EPI_data = read.csv('2010EPI_data.csv', skip=1)
View(EPI_data)
attach(EPI_data)
fix(EPI_data) # launches a simple data editor
# Ecosystem distributions
summary(ECOSYSTEM)
fivenum(ECOSYSTEM,na.rm=TRUE)
stem(ECOSYSTEM)
hist(ECOSYSTEM)
rug(ECOSYSTEM) # Adds a rug representation (1-d plot) of the data to the plot.
plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(ECOSYSTEM, main = 'Normal QQ Plot of ECOSYSTEM');qqline(ECOSYSTEM) #quantile-quantile plot

# AIR_H distributions
summary(AIR_H)
fivenum(AIR_H,na.rm=TRUE)
stem(AIR_H)
hist(AIR_H)
rug(AIR_H) # Adds a rug representation (1-d plot) of the data to the plot.
plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(AIR_H, main= 'Normal QQ Plot of AIR_H');qqline(AIR_H) #quantile-quantile plot
boxplot(ECOSYSTEM, AIR_H)

# Exercise 2
EPILand = EPI[!Landlock]
ELand = EPILand[!is.na(EPILand)]
hist(ELand)
hist(ELand, seq(30, 95, 1), prob=TRUE)
lines(density(ELand, na.rm=TRUE, bw='SJ'))
rug(EPI)
qqnorm(ELand, main = 'Normal QQ Plot of ELand'); qqline(ELand)

NoSurfWater = EPI[!No_surface_water]
NoSurfWater = NoSurfWater[!is.na(NoSurfWater)]
hist(NoSurfWater)
hist(NoSurfWater, seq(30,95,1), prob=TRUE)
lines(density(NoSurfWater, na.rm=TRUE, bw=2))
rug(EPI)
qqnorm(NoSurfWater, main = 'Normal QQ Plot of No Surface Water'); qqline(NoSurfWater)

desert = EPI[!Desert]
desert = desert[!is.na(desert)]
hist(desert)
hist(desert, seq(30,95,1), prob=TRUE)
lines(density(desert, na.rm=TRUE, bw=3))
rug(EPI)
qqnorm(desert, main = 'Normal QQ Plot of Desert'); qqline(desert)

HighPopDensity = EPI[!High_Population_Density]
HighPopDensity = HighPopDensity[!is.na(HighPopDensity)]
hist(HighPopDensity)
hist(HighPopDensity, seq(30,95,1), prob=TRUE)
lines(density(HighPopDensity, na.rm=TRUE, bw=1))
rug(EPI)
qqnorm(HighPopDensity, main = 'Normal QQ Plot of High Population Density'); qqline(HighPopDensity)


# GPW3_GRUMP
GPW3 = read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')
View(GPW3)
attach(GPW3)

# Percent Total Land Area in Urban Extents (sq km) Distributions
summary(Pct.Total.Land.Area.in.Urban.Extents..sq.km.)
pct=Pct.Total.Land.Area.in.Urban.Extents..sq.km.
fivenum(pct, na.rm=TRUE)
pct = pct[!is.na(pct)]
hist(pct)
plot(ecdf(pct), do.points=FALSE, verticals=TRUE) #cumulative density function
par(pty='s')
qqnorm(pct, main = 'Normal QQ Plot of Pct Total Land Area in Urban Extents (sq km)'); qqline(pct)

