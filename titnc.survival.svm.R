library(dplyr)
library(caret)

dat <- read.table("/Users/mac/Desktop/polyphenol extraction/Journal articles/train.csv", header=T, sep=",")

####### Feature engineering 
#### creating variables for deck ####
dat$Deck <- gsub("[0-9]+", "", dat$Cabin)
dat$Deck <- sapply(dat$Deck, function(x) substr(x, 1, 1) )

#### creating variables for ticket #####
dat$Ticket.Prf <- gsub("/", "", gsub("\\.", "", gsub("^[0-9]*","", gsub( " .*", "", dat$Ticket)))) #### ticket prefix was extracted

table(dat$Ticket.Prf) #### diagnostics to know the frequency of each class in the ticket predictor.  

dat$Ticket.Prf[dat$Ticket.Prf==""] <- 'NoPref' #### tickets with no prefixes where changed to NoPref
dat$Ticket.Prf[dat$Ticket.Prf=="SCPARIS"] <- 'SCParis' 
dat$Ticket.Prf <- as.factor(dat$Ticket.Prf)

dat$Ticket.no <- gsub(".* ","", dat$Ticket) #### obtaining ticket no. 

#### Extracting honorifics of a person ####
table(dat$Sex, dat$hon)
dat$hon <- gsub("\\..*","",gsub(".*\\, ","", dat$Name))

rare.title <- c('Dr', 'Jonkheer', 'Lady', 'Major', 'Rev', 'Sir', 'the Countess', 'Capt', 'Col', 'Don')
#### Titles which are not usually used are lumped into "rare.title"

dat$hon[dat$hon=='Ms']<- 'Miss'
dat$hon[dat$hon=='Mme']<- 'Mrs'
dat$hon[dat$hon=="Mlle"] <- 'Miss'
dat$hon[dat$hon %in% rare.title] <- 'rare title'


dat <- dat %>% mutate(family.size =SibSp+Parch) 
## family size is included which is the linear combination of siblings and number of parents or children

dat$hon <- as.factor(dat$hon)
dat$Deck <-as.factor(dat$Deck)
dat$Pclass <- as.factor(dat$Pclass)
dat$Ticket.no <- as.numeric(dat$Ticket.no)

### missing values for embarked and Age
which(dat$Embarked=="")
which(is.na(dat$Age))
which(dat$Deck=="")


### converting missing values to NAs ready for missing values imputation using KNN
dat$Embarked[dat$Embarked==""] <- NA
dat$Deck[dat$Deck==""] <- NA

require(DMwR)

#### creating child, mother  and adult variables

dat$Cohort[dat$Age < 18] <- 'Child'
dat$Cohort[dat$Age > 18] <- 'Adult'
dat$Cohort[dat$hon =='Mrs' & dat$Parch > 0] <- 'Mother'
dat$Cohort <- as.factor(dat$Cohort)


dat.fnl <- dat %>% select(-c(PassengerId, Name, Cabin, Survived, Ticket)) #### finalizing data ready for imputation

#### Imputation of missing values
dat.fnl <- knnImputation(dat.fnl ,meth="median")  


dum.var <- dummyVars(~., dat.fnl)
dat.fnl <- data.frame(Survived=dat$Survived, predict(dum.var, dat.fnl))
dat.fnl <- dat.fnl %>% select(-c(Deck., Embarked., Deck.T))


#### model building and tuning using SVM 
set.seed(476)
ctrl <- trainControl(summaryFunction = twoClassSummary,
                     classProbs = TRUE, method="cv", number= 10, savePredictions = T)

titanic.train <- train(dat.fnl[,2:ncol(dat.fnl)], y=make.names(dat.fnl$Survived),
                       preProcess =c("center", "scale"),
                       method="svmRadial", trControl=ctrl, metric="ROC")

confusionMatrix(data = titanic.train$pred$pred,
                reference = titanic.train$pred$obs)

cols.dat.fnl <- colnames(dat.fnl)

