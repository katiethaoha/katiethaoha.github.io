install.packages("DataExplorer")
install.packages("countrycode")
install.packages("corrplot")
install.packages("tidyr")
install.packages("gbm")
install.packages("dummies")
install.packages("forecast")
install.packages("randomForest")

#allPackages <- c("DataExplorer","countrycode","corrplot","tidyr","gbm","forecast","randomForest")
#install.packages(allPackages)


library(DataExplorer)
library(countrycode)
library(corrplot)
library(dplyr)
library(tidyr)
library(gbm)
library(dummies)
library(gbm)
library(forecast)
library(randomForest)

#path setting
path <- getwd()
setwd(path)

rawDataSet <- read.csv("SuicideDataset.csv")

#Initial EDA
dim(rawDataSet)
str(rawDataSet)

introduce(rawDataSet)
str(rawDataSet)

plot_intro(rawDataSet)
plot_missing(rawDataSet)

#dropping HDI.for.year beacause it has almot 70% missing values
rawDataSet$HDI.for.year <- NULL
#dropping country.year beacause it has redundant data which comprises of country and Year
rawDataSet$country.year <- NULL

#renaming i.country to country
colnames(rawDataSet)[colnames(rawDataSet) == 'ï..country'] <- 'country'
colnames(rawDataSet)[colnames(rawDataSet) == 'gdp_for_year....'] <- 'gdp_for_year'
colnames(rawDataSet)[colnames(rawDataSet) == 'gdp_per_capita....'] <- 'gdp_per_capita'

unique(rawDataSet$age)
x<-unique(rawDataSet$generation)
View(x)

rawDataSet = rawDataSet %>% separate(age,c("age","yearss"),sep = " ")
rawDataSet$age <- factor(rawDataSet$age,
                         ordered = T,
                         levels = c('5-14','15-24','25-34','35-54','55-74','75+'))
rawDataSet$yearss <- NULL # Copy the dataset

# separate age range column to age_min and age_max
rawDataSet = rawDataSet %>% separate(age,c("age_min","age_max"),sep = "([\\-])")
rawDataSet$age_min[suicide$age_min=='75+'] <- '75'
rawDataSet$age_max[is.na(suicide$age_max)] <- '100'
rawDataSet$HDI.for.year <- NULL

rawDataSet$generation <- factor(rawDataSet$generation, 
                                ordered = T, 
                                levels = c("G.I. Generation", 
                                           "Silent",
                                           "Boomers", 
                                           "Generation X", 
                                           "Millenials", 
                                           "Generation Z"))

rawDataSet$continent <- countrycode(sourcevar = rawDataSet[, "country"],
                                    origin = "country.name",
                                    destination = "continent")

head(rawDataSet)
suicide <- rawDataSet

# convert to numeric and factorize the variables
suicide$gdp_for_year <- as.numeric(suicide$gdp_for_year)
#suicide$country <- NULL
suicide$continent <- factor(suicide$continent, ordered = T)
levels(suicide$continent)

suicide$age_min <- as.numeric(suicide$age_min)
suicide$age_max <- as.numeric(suicide$age_max)

#suicide[!complete.cases(suicide),] #checking for NA

# Get dummies sex variable
suicide <- dummy.data.frame(suicide, names = c("sex","continent","generation") , sep = ".")

# Convert continent and genration variables to numerical variables
suicide <- suicide %>% mutate_if(is.factor, as.numeric)

str(suicide)

#Correlation MAtrix
correaltionMatrix <-suicide %>%
  select_if(is.numeric) %>%
  cor(.)

corrplot(correaltionMatrix, method = "circle")

# Drop the suicide no column
#suicide<- suicide[-5]
head(suicide)
View(suicide)

suicide$country.year <- NULL

# Create training and testing set
set.seed(88)
suicide <- suicide[sample(nrow(suicide)), ]
split <- round(nrow(suicide)*0.8)
train <- suicide[1:split, ]
test <- suicide[(split+1):nrow(suicide), ]
str(suicide)

gbm <- gbm(formula=suicides.100k.pop~ country +sex.male+age_min+age_max+suicides_no+year+sex.female,
          distribution = "gaussian",
          data=train,
          n.trees = 10000,
          cv.folds = 5,
          interaction.depth = 3,
          shrinkage = 0.1)

summary(gbm)

# Find the optimized number of trees by cv method
ntree_opt_cv <- gbm.perf(gbm, method="cv")
ntree_opt_cv

sqrt(min(gbm$cv.error))

# Run the predicting model on test set
predicts <- predict(object = gbm,
                    newdata = test,
                    n.trees = ntree_opt_cv,
                    type = "response")

View(predicts)

# Accessing the accuracy of result
accuracy(predicts,test$suicides.100k.pop)
caret::RMSE(predicts, test$suicides.100k.pop)

residuals = test$suicides.100k.pop - predicts
# Calculate total sum of squares
tss =  sum((test$suicides.100k.pop - mean(test$suicides.100k.pop))^2 )
# Calculate residual sum of squares
rss =  sum(residuals^2)
# Calculate R-squared
rsq  =  1 - (rss/tss)
rsq

#Random Forest Prediction
#table(is.na(train)) #checking for NA in train set
randomForestPrediction <- randomForest(suicides.100k.pop~ country +sex.male+age_min+age_max+suicides_no+year+sex.female,
                                       data=train,
                                       ntree = 500, 
                                       mtry = 6, 
                                       importance = TRUE)
predictsRandomForest <- predict(object = randomForestPrediction,newdata = test)
accuracy(predictsRandomForest,test$suicides.100k.pop)

residualsRM = test$suicides.100k.pop - predictsRandomForest
# Calculate total sum of squares
tssRM =  sum((test$suicides.100k.pop - mean(test$suicides.100k.pop))^2 )
# Calculate residual sum of squares
rssRM =  sum(residualsRM^2)
# Calculate R-squared
rsqRM  =  1 - (rssRM/tssRM)
rsqRM

AcuuracyTable <- matrix(c(rsq,rsqRM),ncol=2,byrow=TRUE)
colnames(AcuuracyTable) <- c("Gradient Boosting","Random Forest")
rownames(AcuuracyTable) <- c("RMSE")

AcuuracyTable <- as.table(AcuuracyTable)
AcuuracyTable


