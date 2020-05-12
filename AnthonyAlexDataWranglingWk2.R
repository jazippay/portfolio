# Final Project: Baseball Data Analysis

library(Lahman)
library(dplyr)

# Subset "Teams" to only include teams from 1985-2016 (the years for which we have salary information)
Teams0 <- Teams[1918:2835, ]

# Subset "Batting" to only include data from 1985-2016 (the years for which we have salary information)
Batting0 <- Batting[which(Batting$yearID > 1984 & Batting$yearID < 2017), ]

# Subset "Pitching" to only include data from 1985-2016 (the years for which we have salary information)
Pitching0 <- Pitching[which(Pitching$yearID > 1984 & Pitching$yearID < 2017), ]

# Subset "Fielding" to only include data from 1985-2016 (the years for which we have salary information)
Fielding0 <- Fielding[which(Fielding$yearID > 1984 & Fielding$yearID < 2017), ]

# Rename player ID column in "war_daily_bat" and "war_daily_pitch" to be consistent
names(war_daily_bat)[names(war_daily_bat) == "player_ID"] <- "playerID"
names(war_daily_pitch)[names(war_daily_pitch) == "player_ID"] <- "playerID"

names(war_daily_bat)[names(war_daily_bat) == "salary"] <- "Salary"
names(war_daily_pitch)[names(war_daily_pitch) == "salary"] <- "Salary"

# Eliminate unneeded columns from Batting table, namely "stint"
colnames(Batting0)
keeps1 <- c("playerID", "G", "AB", "R", "H", "X2B", "X3B", "RBI", "SB", "CS", "SO", "IBB", "SH", "SF", "GIDP")
Batting1 <- Batting0[keeps1]

# Eliminate unneeded columns from "Pitching" table, namely "stint"
colnames(Pitching0)
keeps2 <- c("playerID", "W", "L", "G", "GS", "CG", "SHO", "SV", "IPouts", "H", "ER", "HR", "BB", "SO", "BAOpp", "ERA")
Pitching1 <- Pitching[keeps2]

# Eliminate unneeded columns from "People" table
colnames(People)
keeps3 <- c("playerID", "nameFirst", "nameLast")
People1 <- People[keeps3]

# Eliminate unneeded columns from "Salaries" table
colnames(Salaries)
keeps4 <- c("playerID", "yearID", "teamID", "salary")
Salaries1 <- Salaries[keeps4]

# Eliminate unneeded columns from "Teams" table
colnames(Teams)
keeps5 <- c("teamID", "yearID", "W", "L", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "HBP", "SF", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "name", "attendance")
Teams1 <- Teams0[keeps5]

# Eliminate unneeded columns from "war_daily_bat" table
colnames(war_daily_bat)
keeps6 <- c("playerID", "Salary", "WAR", "WAA", "OPS_plus")
warB <- war_daily_bat[keeps6]

# Eliminate unneeded columns from "war_daily_pitch" table
colnames(war_daily_pitch)
keeps8 <- c("playerID", "Salary", "WAR", "WAA", "ERA_plus")
warP <- war_daily_pitch[keeps8]

# Recode "teamID" so singular franchises are consistent
Teams1$teamID[Teams1$teamID == 'ANA'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'CAL'] <- 'LAA'
Teams1$teamID[Teams1$teamID == 'FLO'] <- 'MIA'
Teams1$teamID[Teams1$teamID == 'ML4'] <- 'MIL'
Teams1$teamID[Teams1$teamID == 'MON'] <- 'WAS'

Salaries1$teamID[Salaries1$teamID == 'ANA'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'CAL'] <- 'LAA'
Salaries1$teamID[Salaries1$teamID == 'FLO'] <- 'MIA'
Salaries1$teamID[Salaries1$teamID == 'ML4'] <- 'MIL'
Salaries1$teamID[Salaries1$teamID == 'MON'] <- 'WAS'

# Merge "People" data frame and "Salaries" data frame by "playerID"
baseballSalaries <- merge(People1, Salaries1, by=c("playerID"), all=TRUE)
View(baseballSalaries)

# Merge "Batting" data frame and "baseballSalaries" data frame by "playerID"
baseball1 <- merge(Batting1, baseballSalaries, by=c("playerID"), all=TRUE)

View(baseball1)

# Merge in "warB", which contains advanced batting metrics
batAdv <- merge(baseball1, warB, by=c("playerID"), all=TRUE)
View(batAdv)

# Merge "Pitching" data frame and "baseball0" data frame by "playerID"
baseball2 <- merge(baseballSalaries, Pitching1, by=c("playerID"), all=TRUE)

# Merge in "warP", which contains advanced pitching metrics
pitchAdv <- merge(baseball2, warP, by=c("playerID"), all=TRUE)

# Simple Player Salaries dataset (only "playerID", "yearID", "salary")
keepsSmall <- c("playerID", "yearID", "salary")
PlayersSimple <- baseballSalaries[keepsSmall]

# Merge "Teams" data frame and "Salaries" data frame by "teamID"
teamSalaries <- merge(Teams1, Salaries1, by=c("teamID"), all=TRUE)

# Aggregate "teamSalaries" by both team and season
teamSalariesSTY <- aggregate(teamSalaries$salary, by=list(Category=teamSalaries$teamID, teamSalaries$yearID.x), FUN=sum)

## For future use in Python
# Remove NA from any dataset, based on final datasets that have been merged together baseball0, baseball1, baseball2 and teamSalaries
baseball0NA <- na.omit(baseball0)
baseball1NA <- na.omit(baseball1)
baseball2NA <- na.omit(basbeall2)
teamSalariesNA <- na.omit(teamSalaries)