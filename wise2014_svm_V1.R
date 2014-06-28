library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
source("writeAnswer.R")
source("score.R")

cpuCore <- 4 # Number of CPU Codes
batchSize <- 1000 # Number of training rows
answerUNC <- "Answer/submission_%5.3f.csv"

system.time(X.train.all <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 172 Sec
system.time(y.train.all.labels.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3] #   0.5 MB RAM, 185 Sec
system.time(X.test <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # 108.7 MB RAM,  56 Sec  [64858:99780]

trainBatchUNC <- "Data/wise2014-train_Batch.libsvm.txt"
crossBatchUNC <- "Data/wise2014-cross_Batch.libsvm.txt"
if (batchSize > 0) {
    if (file.exists(crossBatchUNC)) {
        X.cross <- read.matrix.csr(crossBatchUNC)$x
        y.cross.labels.factor <- read.matrix.csr(crossBatchUNC)$y
    } else {
        X.cross <- X.train.all[(batchSize+1):(2*batchSize), ]
        y.cross.labels.factor <- y.train.all.labels.factor[(batchSize+1):(2*batchSize)]
        write.matrix.csr(x=X.cross, y=y.cross.labels.factor, file=crossBatchUNC)        
    }

    if (file.exists(trainBatchUNC)) {
        X.train <- read.matrix.csr(trainBatchUNC)$x
        y.train.labels.factor <- read.matrix.csr(trainBatchUNC)$y
    } else {
        X.train <- X.train.all[1:batchSize, ]
        y.train.labels.factor <- y.train.all.labels.factor[1:batchSize]
        write.matrix.csr(x=X.train, y=y.train.labels.factor, file=trainBatchUNC)        
    }
}

y.train.labels.string <- paste(",", as.character(y.train.labels.factor), ",", sep="")
y.cross.labels.string <- paste(",", as.character(y.cross.labels.factor), ",", sep="")

modle.name <- "svm"
score.cross.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
answers <- rep("", dim(X.test)[1])

# library(doParallel)
# cluster <- makeCluster(cpuCore)
# registerDoParallel(cluster)
# getDoParWorkers() # Verify how many cores will be used.

for (label in 1:203) {
    label.name <- paste("", as.character(label)) # Example: " 32"    
    label.pattern <- paste(",", as.character(label), ",", sep="")
    y.train.int <- as.integer(grepl(label.pattern, y.train.labels.string)) # 1=TRUE row in label
    y.cross.int <- as.integer(grepl(label.pattern, y.cross.labels.string)) # 1=TRUE row in label
    y.train.bool.factor <- as.factor(grepl(label.pattern, y.train.labels.string)) # TRUE row in label
    y.cross.bool.factor <- as.factor(grepl(label.pattern, y.cross.labels.string)) # TRUE row in label
    
    modelUNC <- sprintf("Model/%s_model_%03d.rds", modle.name, label)
    cat(sprintf("%s label=%03d train: TRUE=%d/%d\n", modle.name, label, sum(y.train.int), length(y.train.int)))

    
    if (sum(y.train.int) == 0) { # All training = FALSE
        score.cross.label.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
        score.cross.label.table[1,1] <- length(y.train.int) # TN
        y.test.predict <- as.factor(rep("FLASE", dim(X.train)[1]))
    } else {
        if (file.exists(modelUNC)) {
            smv.model <- readRDS(modelUNC)
        } else {
            system.time(smv.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="linear", probability=TRUE, seed=42))[3]
            saveRDS(smv.model, file=modelUNC) # Checkpoint: Cache model
        }
        y.cross.predict <- predict(smv.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
        #attr(y.predict, "decision.values")[1:4,] # -0.9568987 -0.9338800 -0.9719220  if( >=0, TRUE, FALSE)
        #attr(y.predict, "probabilities")[1:4,]  # matrix(c("TRUE", "FALSE"), c(Prob_TRUE, prob_FALSE))
        
        y.test.predict <- predict(smv.model, X.test)
        score.cross.label.table <- table(Predicted=y.cross.predict, Reference=y.cross.bool.factor) # TP=[2,2], TN=[1,1], FP=[2,1], FN=[1,2]
    }
    answers <- paste(answers, ifelse(y.test.predict == "TRUE", label.name, ""), sep="")
    
    # Calculate mean F1 Score estimate
    if (dim(score.cross.label.table)[2] == 1) { # No Reference=TRUE column
        score.cross.label.table <- cbind(score.cross.label.table, "TRUE"=c(0, 0)) # Fill TRUE column with 0
    }
    score.cross.table <- score.cross.table + score.cross.label.table
    
    cat(sprintf("%s label=%03d F1=%6.4f\n", modle.name, label, score(score.cross.label.table)))
}

# stopCluster(cluster)
F1.mean <- score(score.cross.table)
sprintf("%s F1.mean=%6.4f", modle.name, F1.mean)
 
confusionMatrix(score.cross.table, positive='TRUE') # Metrics TP=$table[2,2], TN=$table[1,1], FP=$table[2,1], FN=$table[1,2]

sprintf("Write: %s ", answerUNC)
writeAnswer(answers, sprintf(answerUNC, F1.mean))
sprintf("FINISHED")

