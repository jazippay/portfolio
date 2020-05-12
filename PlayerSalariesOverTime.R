#Player Salaries Over Time

keep11 <- c("name_common", "playerID", "G", "age", "runs_above_avg", "year_ID", "team_ID", "Salary")
playerSalaries <- war_daily_bat[keep11]
View(playerSalaries)

#Remove NA from df
playerSalaries <- na.omit(playerSalaries)
#Convert Salary to Numeric
playerSalaries$Salary <- as.numeric(playerSalaries$Salary)

#Mean Salaries By Player 1995-1999 and 2015-2019, creating data frames to work with
#playerAvgSalaries9599 = a subset of years from baseballSalariesM, teams, salary, year, 1995-1999
playerSalaries9599 <- playerSalaries[which(playerSalaries$year_ID > 1994 & playerSalaries$year_ID < 2001), ]
#playerAvgSalaries1519 = a subset of years from baseballSalariesM,teams, salary, year, 2015-2019
playerSalaries1519 <- playerSalaries[which(playerSalaries$year_ID > 2010 & playerSalaries$year_ID < 2020), ]

View(playerSalaries9599)
View(playerSalaries1519)

#Repeated Measurses ANOVA on Modern and Pre Analytics Data frames
#Assumptions for ANOVA
leveneTest(Salary ~ age, data=playerSalaries9599)
leveneTest(Salary ~ age, data=playerSalaries1519)

#Repeated measures ANOVA years 2000-2016, salary changes per year
PreplayerAnova <- aov(Salary~age+Error(year_ID), playerSalaries9599)
summary(PreplayerAnova)

ModernplayerAnova <- aov(Salary~age+Error(year_ID), playerAvgSalaries1519)
summary(ModernplayerAnova)

write.csv(playerSalaries, '/users/anthonyzippay/Desktop/playerSalaries.csv')
write.xlsx(teamAvgSalaries1519, "/users/anthonyzippay/documents/teamAvgSalaries1519.xlsx")