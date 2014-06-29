library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(caret)
source("score.R")

processLabelBySVM_radial <- function(logUNC, label, trainPercent, y.train.all.labels.string, X.train.all, X.test) {
    model.name <- "smv_radial"
    model.elapse.seconds <- 0
    pid <- Sys.getpid()
    
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
    
    
    cost = 1 # Regularisation penalty
    gamma = 1/dim(X.train)[1]
    tolerance = 0.001
        
    sink(logUNC, append=TRUE)
    cat(sprintf("%s %s(%d): label=%d cost=%.1f gamma=%.5f tolerance=%.3f - Start\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), model.name, pid, label, cost, gamma, tolerance))
    sink()
    
    weights <- "No weights"
    if (y.train.int.sum == 0) { # All training = FALSE
        score.cross.label.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE"), dnn=c("Predicted", "Reference")) * 0
        score.cross.label.table[1,1] <- length(y.train.bool.factor) # TN
        y.test.predict <- as.factor(rep("FLASE", dim(X.train)[1]))
    } else {
        set.seed(42)
        y.train.bool.table <- table(y.train.bool.factor)
        TRUE_count <- y.train.bool.table["TRUE"]
        FALSE_count <- y.train.bool.table["FALSE"]
        weights <- FALSE_count / y.train.bool.table  # Give higher weight to TRUE rows
        weights["FALSE"] <- 1 # Should already be 1
        weights["TRUE"] <- as.integer(FALSE_count/TRUE_count)
        weights["TRUE"] <- ifelse(weights["TRUE"] * TRUE_count < 2000, weights["TRUE"], as.integer(2000/TRUE_count))
        weights["TRUE"] <- ifelse(weights["TRUE"] < 1, 1, weights["TRUE"])
        
        model.elapse.seconds <- system.time(smv.model <- svm(x=X.train, y=y.train.bool.factor, scale=FALSE, kernel="radial", cost=cost, tolerance=tolerance, class.weights=weights, probability=TRUE, seed=42))[3]

        
        y.cross.predict <- predict(smv.model, X.cross, decision.values=TRUE, probability=TRUE) # FALSE TRUE,...
        
        y.test.predict <- predict(smv.model, X.test, decision.values=TRUE, probability=TRUE)
        score.cross.label.table <- table(Predicted=y.cross.predict, Reference=y.cross.bool.factor) # TP=[2,2], TN=[1,1], FP=[2,1], FN=[1,2]
    }
    cross.F1 <- score(score.cross.label.table)
    sink(logUNC, append=TRUE)
    cat(sprintf("%s %s(%d): label=%d weights&score.cross.label.table cross.F1=%6.4f elapse=%.2f Sec.\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), model.name, pid, label, cross.F1, model.elapse.seconds))
    print(weights)
    print(score.cross.label.table)
    sink()    
    
    # return
    #c(label, model.name, model.fit, model.elapse.seconds, y.test.predict, score.cross.label.table)
    list(label, model.name, model.elapse.seconds, y.test.predict, score.cross.label.table)
}

