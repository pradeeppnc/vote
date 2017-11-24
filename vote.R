#Load the dataset
gerber  = read.csv(choose.files())
str(gerber)
prop.table(gerber$voting)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

#Logistic model
library(rpart)
library(rpart.plot)
Model1 = glm(voting ~ civicduty + hawthorne + self + neighbors, data = gerber, family = binomial())
coef(Model1)
summary(Model1)

#Prediction
Prediction = predict(Model1, type = "response")
Prediction

#Accuracy with threshold as .3
table(gerber$voting, Prediction > .3)
(134513+51966)/(134513+51966+100875+56730)

#Accuracy with threshold as .5
table(gerber$voting, Prediction > .5)
(235388)/(235388+108696)

#Accuracy of Baseling model
table(gerber$voting)
(108696)/(108696+235388)

#AUC using ROCR package
library(ROCR)
pred = prediction(Prediction, gerber$voting)
pred
as.numeric(performance(pred, "auc")@y.values)

#Cart model1
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)

#Cart model2
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#Cartmodel3 include sex as independant variable
CARTmodel3 = rpart(voting ~ civicduty + sex + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel3)

#Cartmodel4 using control variable as independant
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
(.34 - .296638)

#Cartmodel5 using control and sex variable as independant
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

#Logistic model using sex and control variable alone
Model2 = glm(voting ~ sex + control, data = gerber, family = binomial())
summary(Model2)

#Create a new data frame 
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(Model2, newdata=Possibilities, type="response")
#absolute difference btw woman control in cart & log model
(.290806 - .290456)

#Create logistic model with sex and control combination variable
Model3 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(Model3)

#Prediction
predict(Model3, newdata = Possibilities, type = "response")
