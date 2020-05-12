#Upward Bound Data Exploration and Analysis
#Reserach Questions:
#does participation in our Upward Bound activities improve academic performance? how can I know who partcipated most? 
#What factors mostly contribute to the improvement of academic performance?
# Other questions about data, student control number and control number what is that?
# datasets are lacking data on students that have 0 contacts?
# is there a significant difference between GPA at entry and end of year?
#Data analysis that we may run to answer these questions - testing for assumptions, 
#Possible data wrangling, recode first generationa and low income, convert contact date data type, remove NA

library("rcompanion")
library("car")
library("fastR")
library(dbplyr)
library(ggplot2)

summary(UB_Contact_Data_for_Research)
summary(UB_Student_Data_for_Research)
describe(UB_Contact_Data_for_Research)
#Convert field to numeric and drop unwanted columns


#Correlation Matrix
UBcontact.cor = cor(UB_Contact_Data_for_Research)
UBstudent.cor = cor(UB_Student_Data_for_Research)

#regression analysis of GPA
names(UB_Student_Data_for_Research)
keeps <- c("Entry GPA", "H.S. GPA at end of the year")
gpaData <- UB_Student_Data_for_Research[keeps]
View(gpaData)
gpaData1 <- na.omit(gpaData)
#is there a linear relationship between GPAs at entry and exit? entry is x
scatterplot("Entry GPA", "H.S. GPA at end of the year", gpaData1="Scatterplot Example",
     xlab="GPA at Entry", ylab="GPA After Year", pch=19)

d <- ggplot(gpaData1, aes(x = "H.S. GPA at end of the year", y = "Entry GPA"))
d + geom_point() + geom_smooth(method=lm, se=FALSE)


#Convert contact method and type to factor from character
contactDF$`Contact Method` <- as.factor(contactDF$`Contact Method`)


#linear model
linreg_Contact <- lm(Hours ~ `Contact Method`, contactDF)
summary(linreg_Contact)
#method of 'Group' is significant p value meaning that there are more significance of relationship of hours in that method

linreg_Contact2 <- lm(Hours ~ `Contact Type`, contactDF)
summary(linreg_Contact2)
#significance of hours for contact types involving field trips

#need to conver to numeric to run correlation matrix, could subset data to include numeric columns of interest, Hours, Entry GPA, HS GPA at end of year
#recode contact type and high risk of academic failure
cor(contactDF, method = "pearson", use = "complete.obs")
res <- cor(my_data)
round(res, 2)

#Getting Back to Question of Performance Metrics, Wrangling Data to work with, Need to combine datasets and analyze change of GPA fro entry to end of year and contact type


#Vectors for DF
keepsB <- c('Contact Method', 'Hours', 'Contact Type', 'control.number')
contactDF <- UB_Contact_Data_for_Research[keepsB]
keepsC <- c('Hours', 'Student Control Number')
KeepsD <- c('Entry GPA', 'H.S. GPA at end of the year', 'Contact Type', 'Hours', 'High School Graduation Status', 'APR::FirstEnrollDT', 'APR::RigorousStudy')

#rename columns
names(UB_Student_Data_for_Research)[names(UB_Student_Data_for_Research) == 'Control Number'] <- 'control.number'
names(UB_Contact_Data_for_Research)[names(UB_Contact_Data_for_Research) == 'Student Control Number'] <- 'control.number'
#fix problem with control number column showing scientific notation
UB_Student_Data_for_Research$control.number <- as.integer(UB_Student_Data_for_Research$control.number)
UB_Contact_Data_for_Research$control.number <- as.integer(UB_Contact_Data_for_Research$control.number)

#Trying to Merge contact data with GPA data
#first merge two main datasets by control.number
#StuContact_df1 looks good!
StuContact_df1 <- merge(UB_Student_Data_for_Research, UB_Contact_Data_for_Research,by="control.number")
gpaContact_df1 <- StuContact_df1[KeepsD]

gpaContact_df1$`Contact Type` <- as.factor(gpaContact_df1$`Contact Type`)

library(dplyr)

#VALUES OF EACH COLUMN
unique(gpaContact_df1$`APR::FirstEnrollDT`)
unique(gpaContact_df1$`Contact Type`)
unique(gpaContact_df1$`High School Graduation Status`)


#recode APR:FirstEnrollDT by condition
gpaContact_df2 <- gpaContact_df1 %>%
  mutate(`APR::FirstEnrollDT` = recode(`APR::FirstEnrollDT`, "99/99/999" = 0, "88/88/8888" = 0, "09/21/2019" = 1,  "08/27/2018" = 1, "08/26/2019" = 1, "01/19/2019" = 1, "09/01/2019" = 1, "08/30/2019" = 1, "08/19/2019" = 1, "08/14/2018" = 1, "09/09/2019" = 1, "09/19/2019" = 1, "09/16/2019" = 1, "07/26/2019" = 1, "08/09/2019" = 1, "09/10/2019" = 1, "10/01/2019" = 1, "09/29/2019" = 1, "06/04/2019" = 1))
gpaContact_df2         

#recode High School Graduation Status
gpaContact_df2$HighSchoolGraduationStatusR[gpaContact_df2$`High School Graduation Status`=="Received high school diploma"] <- 1
gpaContact_df2$HighSchoolGraduationStatusR[gpaContact_df2$`High School Graduation Status`=="Currently enrolled in high school"] <- 0

#recode contact type
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Knowledge"] <- 1
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Level Course"] <- 2
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="SAT/ACT Info"] <- 3
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Student Development"] <- 4
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Academic Prep"] <- 5
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Financial Literacy"] <- 6
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Visit"] <- 7
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Application"] <- 8
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Pre Field Trip"] <- 9
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="FAFSA Info"] <- 10
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Senior Exit"] <- 11
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Essay / Personal Statement"] <- 12
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Course Selection / A-G"] <- 13
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Tutoring Services / Referral"] <- 14
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Career Exploration"] <- 15
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Senior Planning"] <- 16
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Educational Field Trip"] <- 17
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Upward Bound Course"] <- 18
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Cultural Activities"] <- 19
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="IAP Development"] <- 20
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Community Service"] <- 21
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="FaFSA Info"] <- 22
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Academic Prep\r"] <- 23
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="STEM"] <- 24
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="English Workshop"] <- 25
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Foreign Language Workshop"] <- 26
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="STU1"] <- 27
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="College Course STU1"] <- 28
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Math / Science"] <- 29
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="English"] <- 30
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Summer Program (Foreign Language)"] <- 31
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="academic Prep"] <- 5
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="Workshop"] <- 33
gpaContact_df2$ContactTypeR[gpaContact_df2$`Contact Type`=="college knowledge"] <- 1



#DROP NA AND DROP UNWANTED COLUMNS AND RUN ANALYSIS
gpaContact_df3 <- na.omit(gpaContact_df2)
keepsN <- c('Entry GPA', 'H.S. GPA at end of the year', 'ContactTypeR', 'Hours', 'HighSchoolGraduationStatusR', 'APR::FirstEnrollDT', 'APR::RigorousStudy')
gpaContact_df4 <- gpaContact_df3[keepsN]
#rename columns
names(gpaContact_df4)[names(gpaContact_df4) == 'APR::FirstEnrollDT'] <- 'PostSecondaryEnroll'
names(gpaContact_df4)[names(gpaContact_df4) == 'APR::RigorousStudy'] <- 'RigorousStudy'
names(gpaContact_df3)[names(gpaContact_df3) == 'Contact Type'] <- 'ContactType'
names(gpaContact_df3)[names(gpaContact_df3) == 'H.S. GPA at end of the year'] <- 'EndofYearGPA'
gpaContact_df2$`APR::RigorousStudy` <- as.numeric(gpaContact_df2$`APR::RigorousStudy`)

summary(gpaContact_df2)

#Correlation Matrix
library(corrplot)
gpaContact_df5 = cor(gpaContact_df4)

  