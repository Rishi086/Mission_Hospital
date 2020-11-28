install.packages("corrplot")
library(ggplot2)
library(corrplot)
library(caret)
library(caTools)
library(psych)
Marketing <- read.csv("D:/R programs/PPA/Marketing.csv")

str(Marketing)

p <- ggplot(Marketing, aes(x = Salary, y = AmountSpent, colour=Gender))
p+geom_point()
p <- ggplot(Marketing, aes(x = AmountSpent, fill=Gender))
p+geom_histogram()

p+geom_histogram(position="dodge")
p+geom_histogram()+facet_grid(Gender~.)

#Boxplot
p <- ggplot(Marketing, aes(x = Gender, y = AmountSpent, fill=Gender))
p+geom_boxplot()


insurance<- read.csv("D:/R programs/PPA/insurance_LR1.csv", stringsAsFactors = TRUE)


summary(insurance)

insurance$bmi[is.na(insurance$bmi)] <- mean(insurance$bmi, na.rm =TRUE)

insurance$bmi[is.na(insurance$bmi)] <- 0

insurance <- na.omit(insurance)

cr <- cor(insurance[c("age","bmi","charges")])
cr
corrplot(cr,type="full")
corrplot(cr,method="number")
corrplot.mixed(cr)


#Dummy Variable
insurance$smoker_yes <- ifelse(insurance$smoker == "yes",1,0)
insurance$smoker_no <- ifelse(insurance$smoker == "no",1,0)

insurance$region_se <- ifelse(insurance$region == "southeast",1,0)
insurance$region_ne <- ifelse(insurance$region == "northeast",1,0)
insurance$region_nw <- ifelse(insurance$region == "northwest",1,0)
insurance$region_sw <- ifelse(insurance$region == "southwest",1,0)



#divide the data set into training and testing

split <- sample.split(insurance$charges, SplitRatio = 0.7)
training_data <- subset(insurance, split == "TRUE")
testing_data <- subset(insurance, split == "FALSE")

#pair.panel

pairs.panels(training_data[c("age","bmi","charges")])


#Linear Regression Model

model1 <- lm(charges~age, data = training_data)
summary(model1)

model2 <- lm(charges~age+bmi, data = training_data)
summary(model2)

model3 <- lm(charges~age+bmi+smoker_yes, data = training_data)
summary(model3)

plot(model3$fitted.values,model3$residuals)

#Prediction
prediction<- predict(model3,testing_data)
head(prediction)
head(testing_data$charges)
plot(prediction,type="l",col="green")
lines(testing_data$charges,col="red")
