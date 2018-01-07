#TITANIC EXPLORATORY ANALYSIS

#Installing the packages
library(dplyr) # data frame manipulation
library(ggplot2) # for visualization
library(gridExtra) # for plotting multiple plots
library(mice) #multivariate imputation by chained equations
library(randomForest) #random Forest model

# Loading the data

titanic_train <- read.csv('data/train.csv')
str(titanic_train)

#Setting the colors for the plots
group_colors <- c("0" = "tomato", "1" = "limegreen", "male" = "skyblue", "female" = "pink")

titanic_train$Survived <- factor(titanic_train$Survived)
ggplot (titanic_train, aes(x = Survived)) + 
  geom_bar(fill = c("0" = "tomato", "1" = "limegreen")) +
  labs(title = "Survival on the Titanic", x = "Survival", y = "Number of Passengers")
#We see that, more died than survived

#Let's visualize survival by the gender
ggplot(titanic_train, aes(x = Sex)) + 
  geom_bar(aes(fill = Survived), position = "fill") +
  scale_fill_manual(values = group_colors) +
  labs(title = "Survival by Sex", x = "Sex", y = "Proportion of Passengers")

#Distriution of the Age by Sex
ggplot(titanic_train, aes(x = Age)) + 
  geom_histogram(aes(fill = Sex), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Sex", x = "Age", y = "Number of Passengers")
#Distribution of Age by Survival
ggplot(titanic_train, aes(x = Age)) + 
  geom_histogram(aes(fill = Survived), binwidth = 2) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Age by Survival", x = "Age", y = "Number of Passengers")

#Distribution of Passenger Class by Survival
ggplot (titanic_train, aes(x = Pclass)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of Passenger Class by Survival", x = "Passenger Class", y = "Number of Passengers")

#Distribution of the Fare by Survival
ggplot (titanic_train, aes(x = Fare)) + 
  geom_histogram(aes(fill = Survived), binwidth = 10) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of the Fare by Survival", x = "Fare Paid", y = "Number of Passengers")

#Distribution of the Origin by Survival
ggplot (titanic_train, aes(x = Embarked)) + 
  geom_bar(aes(fill = Survived)) +
  scale_fill_manual(values = group_colors) +
  labs(title = "Distribution of the Origin by Survival", x = "Origin (port of embarkment)", y = "Number of Passengers")

#FEATURE ENGINEERING
titanic_test <- read.csv('data/test.csv')
combined <- bind_rows(titanic_train, titanic_test)

str(combined)

#Missing Data

sapply(combined, function(x) sum(is.na(x)))

#Factorization of the variables
combined$Survived <- factor(combined$Survived)
combined$Pclass <- factor(combined$Pclass)
combined$Sex <- factor(combined$Sex)
combined$Embarked <- factor(combined$Embarked)

str(combined)

#We will use the mice library to create a dataset with the imputed missing variables
set.seed(123)

imputes <- mice(combined[c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")], method = 'rf')
imputes_output <- complete(imputes)

# We will visualize the changes of the imputed data
#For Age
impute_age <- ggplot(imputes_output, aes(x = Age)) + 
  geom_histogram(binwidth = 2, fill = "thistle") +
  labs(x = "Imputed Age")
age <- ggplot(titanic_train, aes(x = Age)) + geom_histogram(binwidth = 2)
grid.arrange(age, impute_age, ncol = 2)
#for Fare
impute_fare <- ggplot(imputes_output, aes(x = Fare)) + 
  geom_histogram(binwidth = 10, fill = "thistle") +
  labs(x = "Imputed Fare Paid")
fare <- ggplot(titanic_train, aes(x = Fare)) + geom_histogram(binwidth = 10) + labs(x = "Fare Paid")
grid.arrange(fare, impute_fare, ncol = 2)
# for Embarked
impute_embarked <- ggplot(imputes_output, aes(x = Embarked)) + 
  geom_bar(fill = "thistle") +
  labs(x = "Imputed Origin")
embarked <- ggplot(titanic_train, aes(x = Embarked)) + geom_bar() + labs(x = "Origin")
grid.arrange(embarked, impute_embarked, ncol = 2)

#Let's update the data with the new changes
combined$Age <- imputes_output$Age
combined$Fare <- imputes_output$Fare
combined$Embarked <- imputes_output$Embarked

sapply(combined, function(x) sum(is.na(x)))

#Title extraction
combined$Title <- factor(gsub('(.*, )|(\\..*)', '', combined$Name))
table(combined$Title)

#Famsize variable
combined$FamSize <- combined$SibSp + combined$Parch +1
ggplot(combined, aes(x = FamSize)) + 
  geom_bar() +
  labs(x = "Family Size", y = "Number of Passengers", title = "Family Size of Passengers")

#Variable Child

combined$child <- NA
combined$child[combined$Age <=16] <- TRUE
combined$child[combined$Age >16] <-FALSE

str(combined)

#Spliting the train and the test
titanic_train <- combined[1:891,]
titanic_test <- combined[892:1309,]

#THE PREDICTION

rf_titanic <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch 
                           + Fare + Embarked + Title + FamSize + child, 
                           data = titanic_train, ntree = 1000)
plot(rf_titanic)

#Analyze the Variable importance
var_imp <- importance(rf_titanic)
var_imp_df <- data.frame(Var=row.names(var_imp), var_imp)
var_imp_df %>% arrange(desc(MeanDecreaseGini))

#MAking the prediction
predicted <- predict(rf_titanic, newdata=titanic_test)
solution <- data.frame(PassengerID = titanic_test$PassengerId, Survived=predicted)
write.csv(solution, 'soution_in_R.csv', row.names=FALSE)



