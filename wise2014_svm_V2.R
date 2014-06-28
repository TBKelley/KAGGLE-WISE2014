library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
source("writeAnswer.R")
source("score.R")

run.name <- "svm"
cpuCore <- 1 # Number of CPU Codes
trainPercent <- 0.3 # Percent of wise2014-train.libsvm.txt used for training, the rest will be for CV
answerUNC <- "Answer/submission_%5.3f.csv"

cat(sprintf("%s %s: cpuCore=%d trainPercent=%.2f\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name, cpuCore, trainPercent))

system.time(X.train.all <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 172 Sec
system.time(y.train.all.labels.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3] #   0.5 MB RAM, 185 Sec
system.time(X.test <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # 108.7 MB RAM,  56 Sec  [64858:99780,]

y.train.all.labels.string <- paste(",", as.character(y.train.all.labels.factor), ",", sep="")

score.cross.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
answers <- rep("", dim(X.test)[1])

# library(doParallel)
# cluster <- makeCluster(cpuCore)
# registerDoParallel(cluster)
# getDoParWorkers() # Verify how many cores will be used.

for (label in 1:203) {
    label.name <- paste("", as.character(label)) # Example: " 32"    
    label.pattern <- paste(",", as.character(label), ",", sep="")

    y.train.all.int <- as.integer(grepl(label.pattern, y.train.all.labels.string)) # 1=TRUE row in label
    
    if (sum(y.train.all.int) > 0) {
        set.seed(42)
        indexTrain <- createDataPartition(y.train.all.int, p=trainPercent, list=TRUE)$Resample1 # Split based on y    
    } else {
        indexTrain <- c(1)
    }
    rm(y.train.all.int) # Clean-up
    
    y.train.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[ indexTrain]))) # Sum of rows with label
    y.cross.int.sum <- sum(as.integer(grepl(label.pattern, y.train.all.labels.string[-indexTrain]))) # Sum of rows with label
                                                  
    y.train.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[ indexTrain])) # TRUE row in label
    y.cross.bool.factor <- as.factor(grepl(label.pattern, y.train.all.labels.string[-indexTrain])) # TRUE row in label
    
    X.train <- X.train.all[ indexTrain, ]
    X.cross <- X.train.all[-indexTrain, ]
    
    cat(sprintf("%s label=%03d cross: TRUE=%d/%d train: TRUE=%d/%d\n", modle.name, label, y.cross.int.sum, length(y.cross.bool.factor), y.train.int.sum, length(y.train.bool.factor)))
  
    y.test.predict.sum <- 0
    if (y.train.int.sum == 0) { # All training = FALSE
        score.cross.label.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
        score.cross.label.table[1,1] <- length(y.train.bool.factor) # TN
        y.test.predict <- as.factor(rep("FLASE", dim(X.train)[1]))
    } else {
        modelUNC <- sprintf("Model/%s_model_%03d.rds", modle.name, label)
        if (file.exists(modelUNC)) {
            smv.model <- readRDS(modelUNC)
            cat(sprintf("Load model: %s", modelUNC))
        } else {
            y.train.bool.table <- table(y.train.bool.factor)
            TRUE_count <- y.train.bool.table["TRUE"]
            FALSE_count <- y.train.bool.table["FALSE"]
            weights <- FALSE_count / y.train.bool.table  # Give higher weight to TRUE rows
            weights["FALSE"] <- 1
            weights["TRUE"] <- as.integer(FALSE_count/TRUE_count)
            weights["TRUE"] <- ifelse(weights["TRUE"] * TRUE_count < 2000, weights["TRUE"], as.integer(2000/TRUE_count))
            weights["TRUE"] <- ifelse(weights["TRUE"] < 1, 1, weights["TRUE"])
            
            system.time(smv.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="linear", class.weights=weights, probability=TRUE, seed=42))[3]
            #saveRDS(smv.model, file=modelUNC) # Checkpoint: Cache model
        }
        y.cross.predict <- predict(smv.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
        #attr(y.predict, "decision.values")[1:4,] # -0.9568987 -0.9338800 -0.9719220  if( >=0, TRUE, FALSE)
        #attr(y.predict, "probabilities")[1:4,]  # matrix(c("TRUE", "FALSE"), c(Prob_TRUE, prob_FALSE))
        
        y.test.predict <- predict(smv.model, X.test, decision.values=TRUE, probability=TRUE)
        y.test.predict.sum <- sum(y.test.predict == "TRUE")
        score.cross.label.table <- table(Predicted=y.cross.predict, Reference=y.cross.bool.factor) # TP=[2,2], TN=[1,1], FP=[2,1], FN=[1,2]
    }
    answers <- paste(answers, ifelse(y.test.predict == "TRUE", label.name, ""), sep="")
    
    # Calculate mean F1 Score estimate
    if (dim(score.cross.label.table)[2] == 1) { # No Reference=TRUE column
        score.cross.label.table <- cbind(score.cross.label.table, "TRUE"=c(0, 0)) # Fill TRUE column with 0
    }
    score.cross.table <- score.cross.table + score.cross.label.table
    
    F1.running <- score(score.cross.table)
    cross.F1 <- score(score.cross.label.table)
    cross.TP <- score.cross.label.table[2,2]
    cat(sprintf("%s label=%03d cross: F1=%6.4f(%d/%d)    test : TRUE=%d/%d running: F1=%6.4f\n", modle.name, label, cross.F1, cross.TP, y.cross.int.sum, y.test.predict.sum, length(y.test.predict), F1.running))
    cat("")
}

# stopCluster(cluster)
F1.mean <- score(score.cross.table)
sprintf("%s F1.mean=%6.4f", modle.name, F1.mean)
 
confusionMatrix(score.cross.table, positive='TRUE') # Metrics TP=$table[2,2], TN=$table[1,1], FP=$table[2,1], FN=$table[1,2]

sprintf("Write: %s ", sprintf(answerUNC, F1.mean))
writeAnswer(answers, sprintf(answerUNC, F1.mean))
sprintf("FINISHED")

