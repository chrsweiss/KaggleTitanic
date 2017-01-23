require(Amelia)
require(corrgram)

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