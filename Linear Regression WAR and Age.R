#subset data by age
#is there a higher WAR relationship in younger players than older players
#linear regression on WAR and age

#Create dataframes based on range of years; 95-2019,95-99 and 2015-2019
warBattingAll <- warBatNA[which(warBatNA$year_ID > 1994 & warBatNA$year_ID < 2020), ]
#Subset data by groups of age values
warBatYoung <- warBattingAll[which(warBattingAll$age > 17 & warBattingAll$age < 30), ]
warBatOld <- warBattingAll[which(warBattingAll$age >= 30 & warBattingAll$age <= 50), ]

WarBattingAge <- na.omit(warBattingAll$WAR)
WarBattingAge1 <- na.omit(WarBattingAge$age)
warBatting <- na.omit(warBattingAll)

#linear model formula
lin_reg <- lm(age ~ WAR, warBatYoung)
summary(lin_reg)
#adjusted r-sqaured = .002
lin_reg <- lm(age ~ WAR, warBatOld)
summary(lin_reg)
#adjusted r-squared = .006

#from a linear regression stand point there is really no relationship between WAR and age in either subset
#looking at residuals we do see that Max WAR is much higher among the older group, 
#while median and 3quarters WAR is higher in the younger group
#average age of a baseball player in this dataset is 28.3
#average WAR for all is .47
#how do we explain the variance in each subset?

#descriptive stats on dataset 
summary(warBatOld$age)
summary(warBatYoung$age)
library(psych)
describe(warBattingAll)
describe(warBatOld)
#mean WAR .48, mean age 32.9
describe(warBatYoung)
#mean WAR .46, mean age 25.8

scatter.smooth(x=warBatYoung$age, y=warBatYoung$WAR, main="Age of Younger Players by WAR")
#Does the age of a player affect their WAR?
library(ggplot2)
quadPlot <- ggplot(warBatting, aes(x = age, y= WAR)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot
quadPlot2 <- ggplot(warBatting, aes(x = Salary, y= WAR)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot2
quadPlot3 <- ggplot(warBatting, aes(x = age, y= Salary)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot3
SalariesNA <- na.omit(Salaries)
quadPlot4 <- ggplot(teamAvg, aes(x = teamID, y= salary)) + geom_point() + stat_smooth(method = "lm", formula = y ~x + I(x^2), size =1)
quadPlot4



# age, year_ID, team_ID, PA, G, Inn, runs_bat, runs_br, runs_dp, runs_field, runs_infield, runs_outfield, runs_catcher, runs_good_plays, runs_defense, runs_position, 

