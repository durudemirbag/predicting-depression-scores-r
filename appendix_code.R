group3<-read.csv("Group3.csv",header = T)
names(group3)


modeldata<-subset(group3,TRAINTEST==1)
testdata<-subset(group3,TRAINTEST==0)

library(ggplot2)
#histogram shows the distribution of depression scores of the observation, 
#but the graph didn't indicate there is normal distribution.  
ggplot(modeldata)+theme_minimal()+geom_histogram(aes(x=DEPSCORE),fill="royalBlue",color="grey60")

# Scatter plot of BMI against Depression score
ggplot(group3, aes(x = BMI, y = DEPSCORE)) +
  geom_point(color = "black", alpha = 0.6) + 
  labs(
    title = "Relationship between BMI and Depression Score",
    x = "Body Mass Index (BMI)",
    y = "Depression Score (PHQ-9)"
  ) + 
  theme_gray()

#There is a box plot between sex factor and shows that sex factor may impact the DEPSCORE.
ggplot(modeldata, aes(x = factor(SEX), y = DEPSCORE)) + 
  +   geom_boxplot(fill = c("lightblue", "lightgreen")) +
  +   labs(title = "PHQ-9 Depression Scores by Sex", x = "Sex", y = "Depression Score")


#box plot between Residence factor and shows the residence factor may impact the DEPSCORE.
ggplot(modeldata, aes(x = factor(RESIDENCE), y = DEPSCORE)) + 
  +   geom_boxplot(fill = c("lightblue", "lightgreen")) +
  +   labs(title = "PHQ-9 Depression Scores by Residence", x = "Residence", y = "Depression Score")

library(GGally)
#ggpairs indicate that the same variable is affected by each other.
#eg,grade and age,grade and id,residence and school type,oslo3 and health state,bmi and oslo3, 
#and residence,health, oslo3,bmi have effect on depscore. 
ggpairs(modeldata)


#build a linear model with all the variables which shows SCHOOL TYPE, Age and Grade have high P-value, there is no significant evidence to reject that they have no effect on DEPSCORE.
model1<-lm(DEPSCORE~AGE+SEX+GRADE+SCHOOLTYPE+RESIDENCE+HEALTH+OSLO3+BMI,data = modeldata)
summary(model1)

round(coef(summary(model1)),3)

#running a double check to make sure about our result.
#Both adjusted R2 and CP plots indicate we should keep 6 variables. BIC indicates about 4 variables.
library(leaps)
check_model<-regsubsets(DEPSCORE~AGE+SEX+GRADE+SCHOOLTYPE+RESIDENCE+HEALTH+OSLO3+BMI,data = modeldata)

#Visualizing Model Selection Metrics
library(patchwork)
results_metrics<-summary(check_model)
library(patchwork)
results_metrics<-summary(check_model)
data_plot<-data.frame(adjr2=results_metrics$adjr2,bic=results_metrics$bic,cp=results_metrics$cp)
g<-ggplot(data_plot)+theme_minimal()+aes(x=1:8)+labs(x="Number of variables")+scale_x_continuous(breaks = 1:8)
g1<-g+geom_line(aes(y=adjr2))+labs(y="Adjusted R2")
g2<-g+geom_line(aes(y=bic))+labs(y="BIC")
g3<-g+geom_line(aes(y=cp))+labs(y="cp")
g1+g2+g3


#build a linear regression model for 4 variables, and they all have very small p-value which small that 0.05, pleased to keep them. 
best_model4<-lm(DEPSCORE~GRADE+HEALTH+OSLO3+BMI,data = modeldata)
round(coef(summary(best_model4)),3)

#build a linear regression model for 6 variable, and they all have very small p-value which small that 0.05, pleased to keep them. 
best_model6<-lm(DEPSCORE~SEX+GRADE+RESIDENCE+HEALTH+OSLO3+BMI,data = modeldata)
round(coef(summary(best_model6)),3)

#Running backward regression and giving 6 variable linear regression model same as the best_model6 we built before. 
model_backward<-step(model1,direction = "backward")

summary(model_backward)

summary(best_model4)

par(mfrow=c(2,2))
plot(best_model4)

plot(best_model6)


#decide to continue with best_model6 as it has a higher R2. 
#but the QQ plot of best_model6 didn't not shows Normal distribution , so tried different ways to improve it.
best_model61<-lm(DEPSCORE~SEX+GRADE+RESIDENCE+HEALTH+OSLO3+I(BMI^2)+BMI,data = modeldata)
summary(best_model61)

plot(best_model61)


best_model62<-lm(DEPSCORE~SEX*BMI+GRADE*BMI+RESIDENCE*BMI+HEALTH*BMI+OSLO3*BMI+OSLO3*BMI+BMI,data = modeldata)
summary(best_model62)

#Drop variable according to result.
best_model62<-lm(DEPSCORE~SEX+GRADE+RESIDENCE+HEALTH+OSLO3+OSLO3*BMI+BMI,data = modeldata)
summary(best_model62)


plot(best_model62)

#test model with dommy variable ”residence”.
best_model63<-lm(DEPSCORE~RESIDENCE*GRADE+SEX*RESIDENCE+RESIDENCE*HEALTH+RESIDENCE*OSLO3+RESIDENCE*BMI+RESIDENCE,data=modeldata)
summary(best_model63)

plot(best_model63)

#test model with dummy variable “Sex”.
best_model64<-lm(DEPSCORE~SEX*GRADE+SEX*RESIDENCE+SEX*HEALTH+SEX*OSLO3+SEX*BMI+SEX,data=modeldata)
summary(best_model64)

plot(best_model64)

#test model with log transformation.
best_model65<-lm(log(DEPSCORE+1)~SEX+log(GRADE)+RESIDENCE+log(HEALTH)+log(OSLO3)+log(BMI),data = modeldata)
summary(best_model65)

plot(best_model65)

# test model with a single log term.
best_model66<-lm(DEPSCORE~SEX+GRADE+RESIDENCE+HEALTH+OSLO3+log(BMI),data = modeldata)
summary(best_model66)

plot(best_model66)

#best_model161 gives the smallest RMSE 0.3074088, so continue with model161.
predictions <- predict(best_model6, newdata = modeldata)
rmse <- sqrt(mean((predictions - testdata$DEPSCORE)^2))
rmse

predictions <- predict(best_model61, newdata = modeldata)
rmse <- sqrt(mean((predictions - testdata$DEPSCORE)^2))
rmse


predictions <- predict(best_model62, newdata = modeldata)
rmse <- sqrt(mean((predictions - testdata$DEPSCORE)^2))
rmse


library(dplyr)
library(caret)
library(rpart)

#Using best_model61 to predict PHQ-9 Depression Scores based on the test data,and add it to the test data table.
testdata$fitted_values <-predict(best_model61,newdata = testdata)

#get rmse for model61 based on test data.
predictions <- predict(best_model61, newdata = testdata)
rmse <- sqrt(mean((predictions - testdata$DEPSCORE)^2))
rmse


#Add columns of residuals of predicted value and real data to test data.
testdata<-testdata%>%mutate(residuals=DEPSCORE-fitted_values)
 
#summary the data and get correlation of it.
testdata%>%summarise(cor=cor(DEPSCORE,fitted_values))%>%
  +   mutate(R2=cor^2)

#plot the scatter plot of observed PHQ-9 Depression Scores against Predict PHQ-9 Depression Scores
ggplot(testdata)+geom_point(aes(x=fitted_values,y=DEPSCORE))+labs(x="Predict PHQ-9 Depression Scores ",y ="Observed PHQ-9 Depression Scores",title = "scatter plot of observed PHQ-9 Depression Scores against Predict PHQ-9 Depression Scores")

ggplot(testdata)+aes(x=GRADE,y=fitted_values,color=SEX)+geom_point()+facet_wrap(~RESIDENCE)+theme_bw()






