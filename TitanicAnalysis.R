####
##
##   name: TitanicAnalysis.R
## author: Dennis F Langley
##   date: 2018-03-09
##   what: Analysis of Titanic Data Set
##         for Kaggle Competition
##
####

# preamble

rm(list = ls())     # clears global environment
dev.off()           # clears plot window
options(scipen=999) # turns off scientific notation
setwd("~/Documents/Work/Titanic Data") # sets working directory
library(tidyverse)  # loads several packages in the tidyverse
library(plyr)       # misc. functions
library(gdata)      # for keep() function
library(titanic)    # loads the titanic data sets
library(Amelia)     # for checking data and imputing missing values
library(ggplot2)
library(ggthemes)
library(plotly)
library(mice)
library(scales)
library(data.table)
library(reshape2)
cat("\014")

# ---
  
# data import

d.train <- titanic_train
d.test  <- titanic_test
d.full  <- data.table(bind_rows(d.train, d.test))

# ---

# first-look

# look at missingness

ggplot_missing <- function(x){
  
  x %>% 
    is.na %>%
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}

ggplot_missing(d.full)
 # mostly age

# take apart ticket strings
temp<-sapply(d.full$Ticket, function(x) strsplit(x, " ")[[1]][1])
num<-substr(temp, 1, 1) %in% as.character(seq(1,9))
table(num)
table(num, d.full$Survived)
d.full$TicketType <- ifelse(num == T, "Num", NA)
d.full$TicketType[num == F] <- substr(temp, 1, 1)[num == F]
d.full$TicketType<-factor(d.full$TicketType)
rm(num, temp)

d.full$TicketNumber <- sapply(d.full$Ticket, 
                              function(x) tail(strsplit(x, " ")[[1]], 1))


# Titles

# use real expressions to pick out titles and names
d.full$Title <- gsub('(.*, )|(\\..*)', '', d.full$Name)
table(d.full$Sex, d.full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')
d.full$Title[d.full$Title == 'Mlle']        <- 'Miss' 
d.full$Title[d.full$Title == 'Ms']          <- 'Miss'
d.full$Title[d.full$Title == 'Mme']         <- 'Mrs' 
d.full$Title[d.full$Title %in% rare_title]  <- 'Rare Title'
table(d.full$Sex, d.full$Title)
d.full$Surname <- sapply(d.full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


# Family Size

d.full$Fsize <- d.full$SibSp + d.full$Parch + 1

d.full$FsizeD[d.full$Fsize == 1] <- 'singleton'
d.full$FsizeD[d.full$Fsize < 5 & d.full$Fsize > 1] <- 'small'
d.full$FsizeD[d.full$Fsize > 4] <- 'large'

# Create a family variable 
d.full$FamilyID <- paste(d.full$Surname, d.full$Fsize, sep='_')

# Use ggplot2 to visualize the relationship between family size & survival
ggplot(d.full[1:891,], aes(x = Fsize, fill = factor(Survived))) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

d.full$Single <- ifelse(d.full$Fsize==1, 1, 0)
ggplot(d.full[1:891,], aes(x = Single, fill = as.factor(Survived))) + 
  geom_bar(position = "fill")


d.full$Cabin[d.full$Cabin==""] <- "Missing"

d.full$Deck<-sapply(d.full$Cabin, function(x) strsplit(x, NULL)[[1]][1])
d.full$Deck[d.full$Deck=="M"] <- "Missing"
d.full$Deck <- as.factor(d.full$Deck)

# fill in missing values
# fare variable
d.full[is.na(d.full$Fare)==TRUE,]
d.full$Fare[1044] <- median(d.full$Fare[d.full$Embarked=="S" & d.full$Pclass==3], na.rm=T)

# embarked variable
d.full$Embarked[d.full$Embarked==""] <- "C"

# Replace Age using multiple imputation/mice
str(d.full)

factor_vars<-c('Sex', 'PassengerId', 'Pclass', 'Embarked', 'Surname', 'FsizeD', 'Title')
omit_vars<-c('PassengerId','Name','Ticket','Cabin','FamilyID','Surname','Survived', 'TicketNumber', 'Single')

# change these vars to factors
d.full<-as.data.frame(d.full)
d.full[factor_vars]<- lapply(d.full[factor_vars], function(x) as.factor(x))
d.full<-as.data.table(d.full)

# Set a random seed
set.seed(1999)
# Perform imputation using mice, excluding the variables to omit
mice_mod<-mice(select(d.full, -one_of(omit_vars)), method="rf")
mice_output <- complete(mice_mod)


# compare distribution of age before and after imputation
age_orig<-ggplot(d.full, aes(x=Age)) +
  geom_histogram(color="black", fill=rgb(0.8, 0.2, 0.2, 0.9)) +
  theme_few()+
  ggtitle("Age: Original Data")

age_mice<-ggplot(mice_output, aes(x=Age)) +
  geom_histogram(color="black", fill=rgb(0.6, 0.2, 0.2, 0.5)) +
  theme_few()+
  ggtitle("Age: MICE")

library(gridExtra)
library(grid)
library(lattice)
grid.arrange(age_orig, age_mice, ncol=2)

# replace
d.full$Age<-mice_output$Age

# survival based on Age
ggplot(d.full[-which(is.na(d.full$Survived)),], aes(Age, fill = factor(Survived))) + 
  geom_histogram(bins=30) + 
  facet_grid(.~Sex) + 
  theme_few() +
  guides(fill=guide_legend(title="Survived"))

# add child
d.full$Child[d.full$Age>=18] <- 'Adult'
d.full$Child[d.full$Age<18]  <- 'Child'

# add mother
d.full$Mother<-'Not Mother'
d.full$Mother[d.full$Parch>0 & d.full$Sex=='female' & d.full$Age>18 & d.full$Title!='Miss']<-'Mother'       

table(d.full$Mother, d.full$Survived)

# set as factors
d.full$Child  <- factor(d.full$Child)
d.full$Mother <- factor(d.full$Mother)

md.pattern(select(d.full, -one_of(omit_vars)))

ggplot_missing(d.full)


# machine learning modeling

omit <- c("PassengerId", "Name", "Ticket", "Cabin", "TicketNumber", 
          "Surname", "Fsize", "FamilyID", "Single")

### Predicting

library(caret)

# split back to test and train
train<-d.full[which(!is.na(d.full$Survived)),]
test<-d.full[which(is.na(d.full$Survived)),]

# split training set into train and validation set
set.seed(1999)
split1 <- createDataPartition(train$Survived, p = .75)[[1]]
trainDat <- train[split1,]
validDat <- train[-split1,]

# view splits
table(trainDat$Survived)
table(validDat$Survived)


# set up for tuning
ctrl<-trainControl(method="cv",
                   n=10,
                   classProbs=T,
                   #  summaryFunction=fiveStats,
                   verboseIter=T,
                   allowParallel=T,
                   savePredictions="final")


# mod DV
# glm needs the outcome to be a factor
trainDat$Survived[which(trainDat$Survived==0)]<- 'no'
trainDat$Survived[which(trainDat$Survived==1)]<- 'yes'

validDat$Survived[which(validDat$Survived==0)]<- 'no'
validDat$Survived[which(validDat$Survived==1)]<- 'yes'


# features at this point
head(d.full)

library(e1071)

# Logit
set.seed(1999)
train_glm<-suppressWarnings(train(Survived~.,
                                  data=select(trainDat, -one_of(omit)),
                                  trControl=ctrl,
                                  method="glm",
                                  metric="Accuracy"))

# Boosted Logit
set.seed(1999)
train_LogitBoost<-suppressWarnings(train(Survived~.,
                                         data=select(trainDat, -one_of(omit)),
                                         trControl=ctrl,
                                         tuneLength=10,
                                         method="LogitBoost",
                                         metric="Accuracy",
                                         preProcess=c("center", "scale")))

# Elastic Net
set.seed(1999)
train_enet<-suppressWarnings(train(Survived~.,
                                   data=select(trainDat, -one_of(omit)),
                                   trControl=ctrl,
                                   tuneLength=10,
                                   method="glmnet",
                                   metric="Accuracy",
                                   preProcess=c("center", "scale")))


# Decision Tree
set.seed(1999)
train_cart<-suppressWarnings(train(Survived~.,
                                   data=select(trainDat, -one_of(omit)),
                                   trControl=ctrl,
                                   tuneLength=10,
                                   method="rpart",
                                   metric="Accuracy"))


# Ranger
set.seed(1999)
train_ranger<-suppressWarnings(train(Survived~.,
                                     data=select(trainDat, -one_of(omit)),
                                     trControl=ctrl,
                                     tuneLength=10,
                                     method="ranger",
                                     metric="Accuracy"))


# Cforest
set.seed(1999)
train_cforest<-suppressWarnings(train(Survived~.,
                                      data=select(trainDat, -one_of(omit)),
                                      trControl=ctrl,
                                      tuneLength=10,
                                      method="cforest",
                                      metric="Accuracy"))


# Boosted trees
# Cforest
set.seed(1999)
train_gbm<-suppressWarnings(train(Survived~.,
                                  data=select(trainDat, -one_of(omit)),
                                  trControl=ctrl,
                                  tuneLength=10,
                                  method="gbm",
                                  metric="Accuracy"))

train_models<-lapply(ls(pattern="train_"), get)
