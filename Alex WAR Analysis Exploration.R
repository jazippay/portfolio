# Exploratory Analysis

library(Lahman)
library(dplyr)
library("PerformanceAnalytics")
library(ggplot2)

# Create correlation matrices of relevant data
Batting_quant <- Batting10[, c(2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22)]
chart.Correlation(Batting_quant, histogram=FALSE, method="pearson")

chart.Correlation(Batting, histogram = FALSE, method="pearson")

Pitching_quant <- Pitching10[, c(2,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)]
chart.Correlation(Pitching_quant, histogram=FALSE, method="pearson")

cor.test(warBatA$WAR, warBatA$salary, method="pearson", use = "complete.obs")
cor.test(warBatB$WAR, warBatB$salary, method="pearson", use = "complete.obs")

warBatA_quant <- warBatA[, c(2,5,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,47,48,49)]
chart.Correlation(warBat_quant, histogram=FALSE, method="pearson")

# Pair down data
## Subset "Batting" to only include data from 2007-2016
Batting10 <- Batting[which(Batting$yearID >= 2007 & Batting$yearID <= 2016), ]

## Subset "Pitching" to only include data from 2007-2016
Pitching10 <- Pitching[which(Pitching$yearID >= 2007 & Pitching$yearID <= 2016), ]

# Split data between pre-advanced metrics and post-advanced metrics samples
## Subset "warBat" to only include data from after 1995-1999
warBatA <- warBatNA[which(warBatNA$year_ID >= 1995 & warBatNA$year_ID <= 1999), ]

## Subset "warBat" to only include data from 2015-2019
warBatB <- warBatNA[which(warBatNA$year_ID >= 2015 & warBatNA$year_ID <= 2019), ]

## Subset "warPitch" to only include data from after 1995-1999
warPitchA <- warPitchNA[which(warPitch$year_ID >= 1995 & warPitch$year_ID <= 1999), ]

## Subset "warPitch" to only include data from 2015-2019
warPitchB <- warPitchNA[which(warPitch$year_ID >= 2015 & warPitch$year_ID <= 2019), ]

View(warPitchA)
View(warPitchB)
View(warBatA)
View(warBatB)

warPitchA <- na.omit(warPitchA)
warPitchB <- na.omit(warPitchB)
warBatA <- na.omit(warBatA)
warBatB <- na.omit(warBatB)

warBatA$age <- as.numeric(warBatA$age)
warBatB$age <- as.numeric(warBatB$age)

# Linear regression for salary/WAR for both samples
lin_reg1 <- lm(salary ~ WAR, warBatA)
print(lin_reg1)
summary(lin_reg1)

lin_reg2 <- lm(salary ~ WAR, warBatB)
print(lin_reg2)
summary(lin_reg2)

lin_reg3 <- lm(salary ~ WAR, warPitchA)
print(lin_reg3)
summary(lin_reg3)

lin_reg4 <- lm(salary ~ WAR, warPitchB)
print(lin_reg4)
summary(lin_reg4)

ggplot(warBatA, aes(x = factor(year_ID), y = WAR)) + geom_boxplot()
ggplot(warBatB, aes(x = factor(year_ID), y = WAR)) + geom_boxplot()
ggplot(warBatA, aes(x = factor(year_ID), y = WAR)) + geom_boxplot() + scale_y_log10()
ggplot(warBatB, aes(x = factor(year_ID), y = WAR)) + geom_boxplot() + scale_y_log10()

ggplot(warPitchA, aes(x = factor(year_ID), y = WAR)) + geom_boxplot()
ggplot(warPitchB, aes(x = factor(year_ID), y = WAR)) + geom_boxplot()
ggplot(warPitchA, aes(x = factor(year_ID), y = WAR)) + geom_boxplot() + scale_y_log10()
ggplot(warPitchB, aes(x = factor(year_ID), y = WAR)) + geom_boxplot() + scale_y_log10()

# Merge advanced metrics data frame with traditional stats data frame for analysis
warBatNA1 <- warBatNA[which(warBatNA$year_ID > 1984 & warBatNA$year_ID < 2017), ]
names(warBatNA1)[names(warBatNA1) == "player_ID"] <- "playerID"
names(warBatNA1)[names(warBatNA1) == "year_ID"] <- "yearID"

battingAdv <- Batting1 %>% right_join(warBatNA1, by=c("playerID","yearID"))
battingAdvAgg <- aggregate(battingAdv$age, by=list(Category=battingAdv$WAR, battingAdv$salary), FUN=sum)

colnames(battingAdv)
keepsAdv <- c("yearID", "age", "G.x", "AB", "R", "H", "X2B", "X3B", "RBI", "SB", "CS", "SO", "IBB", "SH", "SF", "GIDP", "WAR", "salary")
battingAdv1 <- battingAdv[keepsAdv]

cor.test(battingAdv1$RBI, battingAdv1$salary, method="pearson", use = "complete.obs")
chart.Correlation(battingAdv1, histogram=FALSE, method="pearson")