require(stringr)
require(plyr)
require(ggplot2)
require(mboost)
require(randomForest)

## Load Data

sampleSub <- read.csv(
   file="sample_submission_file.csv",
   stringsAsFactors=F)

head(sampleSub)
nrow(sampleSub)

train <- read.delim(file="train.tsv",
                    stringsAsFactors=FALSE,
                    fill=FALSE)

test <- read.delim(file="test.tsv",
                   stringsAsFactors=FALSE,
                   fill=FALSE)

nrow(test)

names(train)



## Normalize grades

train[which(is.na(train$grade)), ]

norm.gradeMax <- vector()

for(i in 1:5)
{
   norm.gradeMax[[i]] <- max(train$grade[which(train$set==i)])
}

norm.factor <- max(norm.gradeMax) / norm.gradeMax

train$factor <- norm.factor[train$set]
test$factor <- norm.factor[test$set]

train$normalizedGrade <- train$grade * train$factor

ggplot(train, aes(x=normalizedGrade)) + geom_histogram()
ggplot(train, aes(x=wc)) + geom_histogram()



## Add Features


# Word Count



str_count(testRegex, "[ ]")

train$wc <- str_count(train$essay, "[ ]")
test$wc <- str_count(test$essay, "[ ]")



# Essay length

train$essayLength <- nchar(train$essay)


ggplot(train, aes(x=normalizedGrade, y=essayLength)) +
   geom_point(shape=1) +
   geom_smooth(method=lm) +
   ggtitle("Goodness of fit for linear model") +
   xlab("grade normalized on 12 point scale") +
   ylab("letter count")


## Train and Validate models


# Blackboost

model <- blackboost(normalizedGrade ~ wc, train)
summary(model)

ggplot(train, aes(x=normalizedGrade, y=wc)) +
   geom_point(shape=1) +
   geom_smooth(method=lm) +
   ggtitle("Goodness of fit for linear model") +
   xlab("grade normalized on 12 point scale") +
   ylab("word count")


plot(normalizedGrade ~ wc, data=train)
lines(train$wc, predict(model), col = "red")

plot(resid(model))

names(model)

head(predict(model))

head(predict(predict(model), newData=train$wc))
head(train$normalizedGrade)

head(predict(model, newdata=test))

test$normalizedGrade <- round(predict(model, newdata=test), digits = 0)

test$grade <- test$normalizedGrade / test$factor


train$predictedGrade <- floor(predict(model))

table(train$normalizedGrade == train$predictedGrade)


# Random forest

model.rf <- randomForest(normalizedGrade ~ wc, data=train)

nrow(predict(model.rf, newdata=test))

model.rf[["type"]]



## Write out results

write.table(submission1, file="submission1.csv", , sep=",", row.names=FALSE, na="NA", eol="\n", quote=FALSE)

submission1 <- data.frame(test$id, test$set, sampleSub$weight, test$grade)
names(submission1) <- names(sampleSub)
