require(Amelia)
require(dplyr)
require(caret)

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



train.data$Title <- getTitle(train.data$Name)

average.age <- 
    train.data %>% 
    filter(!is.na(Age)) %>%
    group_by(Title) %>% 
    summarise(EstAge=median(Age))

train.data <- merge(train.data, average.age, by='Title')

train.data[is.na(train.data$Age),which(colnames(train.data)=='Age')] <-
    train.data[is.na(train.data$Age),which(colnames(train.data)=='EstAge')]



train.data$FareLog <- log(train.data$Fare + 1)
modrf <- train(Survived~Age+Sex+FareLog, data=train.data, method="rf")

test.data$Title <- getTitle(test.data$Name)

average.age <- 
    test.data %>% 
    filter(!is.na(Age)) %>%
    group_by(Title) %>% 
    summarise(EstAge=median(Age))

test.data <- merge(test.data, average.age, by='Title')

test.data[is.na(test.data$Age),which(colnames(test.data)=='Age')] <-
    test.data[is.na(test.data$Age),which(colnames(test.data)=='EstAge')]

test.data$FareLog <- log(test.data$Fare + 1)
test.prediction <- predict(modrf, test.data)

output <- data.frame(PassengerId=test.data$PassengerId, Survived=test.prediction)


