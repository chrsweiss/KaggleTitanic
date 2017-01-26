require(Amelia)
require(corrgram)
require(dplyr)
require(caret)
#require(plyr)

getTitle <- function(data){
    my.match <- regexpr(pattern = '\\s\\w+\\.', text = data, ignore.case = TRUE)
    st <- my.match + 1
    end  <- my.match + attr(my.match, "match.length") - 2
    substr(x = data, start = st, stop = end)
}

data.path <- "https://raw.githubusercontent.com/chrsweiss/KaggleTitanic/master/"
test.file <- "test.csv"
train.file <- "train.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv

train.data <- read.csv(url(paste0(data.path, train.file)),
                      header = TRUE,
                      colClasses = train.column.types,
                      na.strings = c("NA",""))

test.data <- read.csv(url(paste0(data.path, test.file)),
                      header = TRUE,
                      colClasses = test.column.types,
                      na.strings = c("NA",""))

missmap(train.data, main="Titanic Training Data - Missing Map",
        col = c("yellow","black"), legend = FALSE)


barplot(table(train.data$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(train.data$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(train.data$Sex), main="Sex (gender)", col="darkviolet")
hist(train.data$Age, main="Age", xlab = NULL, col="brown")
barplot(table(train.data$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(train.data$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(train.data$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(train.data$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

mosaicplot(train.data$Pclass ~ train.data$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

boxplot(train.data$Age ~ train.data$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

mosaicplot(train.data$Embarked ~ train.data$Survived, 
           main="Passenger Fate by Port of Embarkation",
           shade=FALSE, color=TRUE, xlab="Embarked", ylab="Survived")

corrgram.data <- train.data
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

train.data$Title <- getTitle(train.data$Name)

with(train.data[order(train.data$Age),], boxplot(Age~Title))



average.age <- 
    train.data %>% 
    filter(!is.na(Age)) %>%
    group_by(Title) %>% 
    summarise(EstAge=median(Age))

train.data <- merge(train.data, average.age, by='Title')

train.data[is.na(train.data$Age),which(colnames(train.data)=='Age')] <-
    train.data[is.na(train.data$Age),which(colnames(train.data)=='EstAge')]

inTrain = createDataPartition(train.data$Survived, p = 3/4)[[1]]

mytrain = train.data[ inTrain,]

mytest = train.data[-inTrain,]


modrf <- train(Survived~Age+Sex, data=mytrain, method="rf")
modgbm <- train(Survived~Age+Sex, data=mytrain, method="gbm")
modlda <- train(Survived~Age+Sex, data=mytrain, method="lda")

predrf <- predict(modrf, mytrain)
predgbm <- predict(modgbm, mytrain)
predlda <- predict(modlda, mytrain)

datacomb <- data.frame(predrf, predgbm, predlda, Survived=mytrain$Survived)

modcomb <- train(Survived~Age+Sex, data=mytrain, method="rf")

predcomb <- predict(modcomb, datacomb)

c_rf <- confusionMatrix(predrf, mytest$Survived)
c_gbm <- confusionMatrix(predgbm, testing$diagnosis)
c_lda <- confusionMatrix(predlda, testing$diagnosis)
c_comb <- confusionMatrix(predcomb, testing$diagnosis)

c_rf$overall[1]
c_gbm$overall[1]
c_lda$overall[1]
c_comb$overall[1]

