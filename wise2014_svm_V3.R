library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
library(doParallel)
library(foreach)
source("writeAnswer.R")
source("score.R")
source("processLabelBySVM_linear.R")

run.name <- "svm_linear"
cpuCore <- 3 # Number of CPU Codes
trainPercent <- 0.3 # Percent of wise2014-train.libsvm.txt used for training, the rest will be for CV
label.block.size <- 16 # Number of training fits to run in parallel without feed back.
logUNC <- sprintf("wise2014_svm_V3_%s.log", format(Sys.time(), "%Y%m%d_%H%M%S"))
sink(logUNC)
cat(sprintf("%s %s: cpuCore=%d trainPercent=%.2f label.block.size=%d\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name, cpuCore, trainPercent, label.block.size))
sink()        

answerUNC <- "Answer/submission_%5.3f.csv"

system.time(X.train.all <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 172 Sec
system.time(y.train.all.labels.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3] #   0.5 MB RAM, 185 Sec
system.time(X.test <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # 108.7 MB RAM,  56 Sec  [64858:99780,]

y.train.all.labels.string <- paste(",", as.character(y.train.all.labels.factor), ",", sep="")

# initialise accumulation structures
score.cross.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
answers <- rep("", dim(X.test)[1])

library(doParallel)
cluster <- makeCluster(cpuCore)
registerDoParallel(cluster)
getDoParWorkers() # Verify how many cores will be used.

label.seq <- seq(1, 203, by=label.block.size)
for (label.block.start in label.seq) {
    label.block.end <- label.block.start+label.block.size-1
    cat(sprintf("%s labels=%03d:%03d\n", run.name, label.block.start, label.block.end))
    
    # Process blocks of labels in Parallel
    # Returns: label, model.name, model.elapse.seconds, y.test.predict, score.cross.label.table, label.elapse.seconds
    packages <- c("SparseM", "e1071", "rpart", "caret")
    label.subSeq <- seq(label.block.start, label.block.end)

    label.subResults <- foreach (label=label.subSeq, .combine=rbind, .packages=packages) %dopar% {
        set.seed(42)
        label.elapse.seconds <- system.time(results <- processLabelBySVM_linear(logUNC, label, trainPercent, y.train.all.labels.string, X.train.all, X.test))[3]
        list(results, label.elapse.seconds)
    }
    
    # Process label.subResults
    for (index in 1:label.block.size) {
        label.name <- paste("", as.character(label)) # Example: " 32"    
        
        label <- label.subResults[index][[1]][[1]]
        model.name <- label.subResults[index][[1]][[2]]
        model.elapse.seconds <- label.subResults[index][[1]][[3]]
        y.test.predict <- label.subResults[index][[1]][[4]]
        score.cross.label.table <- label.subResults[index][[1]][[5]]
        label.elapse.seconds <- label.subResults[index, 2][[1]]

        answers <- paste(answers, ifelse(y.test.predict == "TRUE", label.name, ""), sep="")

        # Calculate mean F1 Score estimate
        if (dim(score.cross.label.table)[2] == 1) { # No Reference=TRUE column
            score.cross.label.table <- cbind(score.cross.label.table, "TRUE"=c(0, 0)) # Fill TRUE column with 0
        }
        score.cross.table <- score.cross.table + score.cross.label.table
        
        F1.running <- score(score.cross.table)
        cross.F1 <- score(score.cross.label.table)
        cross.TP <- score.cross.label.table[2,2]
        
        y.test.predict.sum <- sum(y.test.predict == "TRUE")
        y.cross.int.sum <- sum(score.cross.label.table)
        cat(sprintf("%s label=%03d cross: F1=%6.4f(%d/%d) test : TRUE=%d/%d running: F1=%6.4f\n", run.name, label, cross.F1, cross.TP, y.cross.int.sum, y.test.predict.sum, length(y.test.predict), F1.running))
        cat("\n")
        
        sink(logUNC, append=TRUE)
        cat(sprintf("%s %s: label=%03d cross: F1=%6.4f(%d/%d) test : TRUE=%d/%d running: F1=%6.4f\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name, label, cross.F1, cross.TP, y.cross.int.sum, y.test.predict.sum, length(y.test.predict), F1.running))
        sink()        
    }     
}

stopCluster(cluster)

F1.mean <- score(score.cross.table)
sprintf("%s F1.mean=%6.4f", run.name, F1.mean)
confusionMatrix(score.cross.table, positive='TRUE') # Metrics TP=$table[2,2], TN=$table[1,1], FP=$table[2,1], FN=$table[1,2]
sprintf("Write: %s ", sprintf(answerUNC, F1.mean))
writeAnswer(answers, sprintf(answerUNC, F1.mean))

sink(logUNC, append=TRUE)
cat(sprintf("%s %s: F1.mean=%6.4f\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name, F1.mean))
print(confusionMatrix(score.cross.table, positive='TRUE'))
cat(sprintf("%s %s:Write: %s \n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name, sprintf(answerUNC, F1.mean)))
sink()   



sprintf("FINISHED")
sink(logUNC, append=TRUE)
cat(sprintf("%s %s: FINISHED\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), run.name))
sink()  

