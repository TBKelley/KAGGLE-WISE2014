## Test different Probability Threshold other than >0.5 to determine TRUE

library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
source("score.R")

label <- 4
trainPercent <- 0.3
cost = 1 # Regularisation penalty
tolerance = 0.001

dfF1 <- data.frame(Threshold=numeric(0), F1=numeric(0))

system.time(X.train.all <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 172 Sec
system.time(y.train.all.labels.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3] #   0.5 MB RAM, 185 Sec
system.time(X.test <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # 108.7 MB RAM,  56 Sec  [64858:99780,]

label.pattern <- paste(",", as.character(label), ",", sep="")
y.train.all.labels.string <- paste(",", as.character(y.train.all.labels.factor), ",", sep="")
y.train.all.int <- as.integer(grepl(label.pattern, y.train.all.labels.string)) # 1=TRUE row in label
set.seed(42)
indexTrain <- createDataPartition(y.train.all.int, p=trainPercent, list=TRUE)$Resample1 # Split based on y    

y.train.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[ indexTrain]))) # Sum of rows with label
y.cross.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[-indexTrain]))) # Sum of rows with label

y.train.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[ indexTrain])) # TRUE row in label
y.cross.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[-indexTrain])) # TRUE row in label

X.train <- X.train.all[ indexTrain, ]
X.cross <- X.train.all[-indexTrain, ]

set.seed(42)
y.train.bool.table <- table(y.train.bool.factor)
TRUE_count <- y.train.bool.table["TRUE"]
FALSE_count <- y.train.bool.table["FALSE"]
weights <- FALSE_count / y.train.bool.table  # Give higher weight to TRUE rows
weights["FALSE"] <- 1 # Should already be 1
weights["TRUE"] <- as.integer(FALSE_count/TRUE_count)
weights["TRUE"] <- ifelse(weights["TRUE"] * TRUE_count < 2000, weights["TRUE"], as.integer(2000/TRUE_count))
weights["TRUE"] <- ifelse(weights["TRUE"] < 1, 1, weights["TRUE"])
#weights["TRUE"] <- weights["TRUE"] * 0 + 1 # Set Weights to 1 ie no weight

# Build Models
svm.linear.model.elapse.seconds <- system.time(svm.linear.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="linear", cost=cost, tolerance=tolerance, class.weights=weights, probability=TRUE, seed=42))[3]

svm.linear.y.train.predict <- predict(svm.linear.model, X.train, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.linear.y.train.prob <- attr(svm.linear.y.train.predict, "probabilities")[,"TRUE"]
score.svm.linear.train.label.table <- table(Predicted=svm.linear.y.train.predict, Reference=y.train.bool.factor, dnn=c("", "SVM Linear train"))
score.svm.linear.train.label.table

# Cross Validation
svm.linear.y.cross.predict <- predict(svm.linear.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
svm.linear.y.cross.prob <- attr(svm.linear.y.cross.predict, "probabilities")[,"TRUE"]

svm.linear.y.cross.predict <- predict(svm.linear.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
score.svm.linear.cross.label.table <- table(Predicted=svm.linear.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear Cross"))
cat(sprintf("linear cross F1=%6.4f", score(score.svm.linear.cross.label.table)))
score.svm.linear.cross.label.table


dfF1 <- data.frame(Threshold=numeric(0), F1=numeric(0))

svm.linear_50.y.cross.predict <- (svm.linear.y.cross.prob >= 0.50)
score.svm.linear_50.cross.label.table <- table(Predicted=svm.linear_50.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_50 Cross"))
F1 <- score(score.svm.linear_50.cross.label.table); cat(sprintf("linear_50.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.50, F1)
score.svm.linear_50.cross.label.table

svm.linear_40.y.cross.predict <- (svm.linear.y.cross.prob >= 0.40)
score.svm.linear_40.cross.label.table <- table(Predicted=svm.linear_40.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_40 Cross"))
F1 <- score(score.svm.linear_40.cross.label.table); cat(sprintf("linear_40.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.40, F1)
score.svm.linear_40.cross.label.table

svm.linear_30.y.cross.predict <- (svm.linear.y.cross.prob >= 0.30)
score.svm.linear_30.cross.label.table <- table(Predicted=svm.linear_30.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_30 Cross"))
F1 <- score(score.svm.linear_30.cross.label.table); cat(sprintf("linear_30.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.30, F1)
score.svm.linear_30.cross.label.table

svm.linear_20.y.cross.predict <- (svm.linear.y.cross.prob >= 0.20)
score.svm.linear_20.cross.label.table <- table(Predicted=svm.linear_20.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_20 Cross"))
F1 <- score(score.svm.linear_20.cross.label.table); cat(sprintf("linear_20.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.20, F1)
score.svm.linear_20.cross.label.table

svm.linear_10.y.cross.predict <- (svm.linear.y.cross.prob >= 0.10)
score.svm.linear_10.cross.label.table <- table(Predicted=svm.linear_10.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_10 Cross"))
F1 <- score(score.svm.linear_10.cross.label.table); cat(sprintf("linear_10.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.10, F1)
score.svm.linear_10.cross.label.table

svm.linear_05.y.cross.predict <- (svm.linear.y.cross.prob >= 0.05)
score.svm.linear_05.cross.label.table <- table(Predicted=svm.linear_05.y.cross.predict, Reference=y.cross.bool.factor, dnn=c("", "SVM Linear_05 Cross"))
F1 <- score(score.svm.linear_05.cross.label.table); cat(sprintf("linear_05.cross F1=%6.4f", F1))
dfF1[nrow(dfF1)+1, ] <- c(0.05, F1)
score.svm.linear_05.cross.label.table

plot(dfF1$Threshold, dfF1$F1, type="l")
dfF1
