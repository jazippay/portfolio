#Linear Regression Analysis WAR and PA, WAR and IPouts
#PA = Plate Attempts, WAR = Wins Above Average, IPouts
#Create a Dataframe of WAR and PA by combining warBatNA and batting
#Create a Dataframe of WAR and IPouts from warPitchNA

#Dataframe merging WAR and AB variables, this is for AB changed to PA      
#keepA <- c('yearID', 'AB', 'playerID')
#keepB <- c('WAR', 'team_ID', 'playerID')
#BattingA <- Batting[keepA]
#warB <- warBatNA[keepB]

#warBatting <- merge(BattingA,warB,by=c("playerID"))
#View(warBatting)

#Create 3 Different dataframes based on range of years; 95-2019,95-99 and 2015-2019
warBattingAll <- warBatNA[which(warBatNA$year_ID > 1994 & warBatNA$year_ID < 2020), ]
warBatting90 <- warBatNA[which(warBatNA$year_ID > 1994 & warBatNA$year_ID < 2000), ]
warBatting2000 <- warBatNA[which(warBatNA$year_ID > 2014 & warBatNA$year_ID < 2020), ]

#Remove NA from all datasets
#warBattingAll <- na.omit(warBattingAll)
#Conver WAR to numeric
#warBatingAll$WAR <- as.numeric(warBatingAll$WAR)

#linear model formula
lin_reg <- lm(PA ~ WAR, warBattingAll)
summary(lin_reg)
#Adjusted R-sqaured .52
#significant p value
lin_reg <- lm(PA ~ WAR, warBatting90)
summary(lin_reg)
#Adjusted R-sqaured .51
#significant p value
lin_reg <- lm(PA ~ WAR, warBatting2000)
summary(lin_reg)
#Adjusted R-sqaured .53
#significant p value
#therefore Plate Attempts tells about 50% of the factors that go into WAR, pretty high!

#Create a Dataframe of WAR and IPouts from warPitchNA
#Create 3 Different dataframes based on range of years; 95-2019,95-99 and 2015-2019
warPitchAll <- warPitchNA[which(warPitchNA$year_ID > 1994 & warPitchNA$year_ID < 2020), ]
warPitch90 <- warPitchNA[which(warPitchNA$year_ID > 1994 & warPitchNA$year_ID < 2000), ]
warPitch2000 <- warPitchNA[which(warPitchNA$year_ID > 2014 & warPitchNA$year_ID < 2020), ]

#Linear Regression on WAR IPouts
lin_reg <- lm(IPouts ~ WAR, warPitchAll)
summary(lin_reg)
#Adjusted R-sqaured .42
#significant p value
lin_reg <- lm(IPouts ~ WAR, warPitch90)
summary(lin_reg)
#Adjusted R-sqaured .46
#significant p value
lin_reg <- lm(IPouts ~ WAR, warPitch2000)
summary(lin_reg)
#Adjusted R-sqaured .39
#significant p value
#therefore, IPouts determine atleast 40% of the factors that go into WAR, not too shabby

#final conclusion there must be some other factors that determine WAR in both pitching and batting. 
#But IPouts explain the variability of WAR in pitching more so than AB explains WAR in batting
#When using Plate Attempts we find 50% of WAR explained in WAR batting stats. 

#Plot of linear models
library(ggplot2)
d <- ggplot(warPitch90, aes(x = WAR, y = IPouts))
d + geom_point() + geom_smooth(method=lm, se=FALSE)
#the linear relationship can be seen much more easily with IPouts to WAR then AB to WAR
ggplot(warBatting2000, aes(WAR, AB)) +
  geom_point() +
  geom_smooth(method = "lm")

d <- ggplot(warBatting2000, aes(WAR, PA))
d + geom_point() + geom_smooth(method = "lm", se=FALSE)

scatter.smooth(x=warPitch90$IPouts, y=warPitch90$WAR, main="IPouts by WAR")
#Residuals are really high, 
