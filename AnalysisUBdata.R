
#Analysis, post-secondary enrollment is our dependent variable and contact type is our independent variable
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")


#this didnt go over well
model = lm('Contact Type' ~ PostSecondaryEnroll + HighSchoolGraduationStatusR + PostSecondaryEnroll:HighSchoolGraduationStatusR, data = gpaContact_df3)
Anova(model,
      type = "II")

#something is wrong here
scatter.smooth(x=gpaContact_df4$ContactTypeR, y=gpaContact_df4$PostSecondaryEnroll, main="College Enrollment By Contacts")

#this doenst look right
contactANOVA <- aov(gpaContact_df4$PostSecondaryEnroll ~ gpaContact_df4$ContactTypeR)
contactANOVA


UBanova1 <- aov(PostSecondaryEnroll~ContactType+Error(ContactTypeR), gpaContact_df3)
summary(UBanova1)
#contact type is significant

#this requires a logistical regression model
scatter.smooth(x=gpaContact_df3$PostSecondaryEnroll, y=gpaContact_df3$ContactTypeR, main="Post Secondary Enrollment by Contact Type")
#is this showing increase, almost logistical regression, but with a decrease at 100%. Why is this showing in decimal?
scatter.smooth(x=gpaContact_df4$PostSecondaryEnroll, y=gpaContact_df4$ContactTypeR, main="Post Secondary Enrollment by Contact Type")
scatter.smooth(x=gpaContact_df3$ContactTypeR, y=gpaContact_df3$EndofYearGPA, main="GPA Caused By Contact Type")


#linear model of contact type and post secondary enrollment
lmMod <- lm(ContactTypeR~PostSecondaryEnroll, data=gpaContact_df4)
par(mfrow=c(2,2))
plot(lmMod)
lmtest::bptest(lmMod)
car::ncvTest(lmMod)
#we met our assumptions we do not have heterodascity
summary(lmMod)
#p-value is significant, but adjusted R squared is very small .001, therefore this model needs to be adjusted at current it showing a very weak linear relationship
#do we adjust the model type, or do we group the values in the variable to reduce the variance?

#stepwise regression to determine which variables are worth looking into deeper for our model
FitAll = lm(PostSecondaryEnroll ~ ., data = gpaContact_df4)
summary(FitAll)

step(FitAll, direction = 'backward')

fitsome = lm(PostSecondaryEnroll ~ ContactTypeR + RigorousStudy, data = gpaContact_df4)
summary(fitsome)

#LOGIT REGRESSION MODEL
mylogit <- glm(PostSecondaryEnroll ~ ContactTypeR, data=gpaContact_df3, family="binomial")
probabilities <- predict(mylogit, type = "response")
gpaContact_df3$Predicted <- ifelse(probabilities > .5, "pos", "neg")
gpaContact_df3$PredictedR <- NA
gpaContact_df3$PredictedR[gpaContact_df3$Predicted=='pos'] <- 1
gpaContact_df3$PredictedR[gpaContact_df3$Predicted=='neg'] <- 0

gpaContact_df3$PredictedR <- as.factor(gpaContact_df3$PredictedR)
gpaContact_df3$ContactTypeR <- as.factor(gpaContact_df3$ContactTypeR)

conf_mat <- caret::confusionMatrix(gpaContact_df3$PredictedR, gpaContact_df3$ContactTypeR)
conf_mat
#Hours and GPA
mylogit <- glm(PostSecondaryEnroll ~ Hours, data=gpaContact_df3, family="binomial")
probabilities <- predict(mylogit, type = "response")
gpaContact_df3$Predictedc <- ifelse(probabilities > .5, "pos", "neg")
gpaContact_df3$PredictedRH <- NA
gpaContact_df3$PredictedRH[gpaContact_df3$Predictedc=='pos'] <- 1
gpaContact_df3$PredictedRH[gpaContact_df3$Predictedc=='neg'] <- 0

gpaContact_df3$PredictedRH <- as.factor(gpaContact_df3$PredictedRH)
gpaContact_df3$Hours <- as.factor(gpaContact_df3$Hours)

conf_mat <- caret::confusionMatrix(gpaContact_df3$PredictedR, gpaContact_df3$ContactTypeR)
conf_mat

