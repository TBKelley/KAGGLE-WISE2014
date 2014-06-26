score <- function(score.table) {
    if ("FALSE" %in% rownames(score.table) && "FALSE" %in% colnames(score.table)) {
        TN <- score.table["FALSE","FALSE"]
    } else {
        TN <- 0
    }
    
    if ("FALSE" %in% rownames(score.table) && "TRUE" %in% colnames(score.table)) {
        FN <- score.table["FALSE","TRUE"]
    } else {
        FN <- 0
    }
    
    if ("TRUE" %in% rownames(score.table) && "FALSE" %in% colnames(score.table)) {
        FP <- score.table["TRUE","FALSE"]
    } else {
        FP <- 0
    }
    
    if ("TRUE" %in% rownames(score.table) && "TRUE" %in% colnames(score.table)) {
        TP <- score.table["TRUE","TRUE"]
    } else {
        TP <- 0
    }
    
    predicted <- ifelse((TP + FP) == 0, 1, TP /(TP + FP))
    recall <- ifelse((TP + FN) == 0, 1, TP/(TP + FN))
    
    F1 <- 2 * predicted * recall /(predicted + recall)
    
    F1
}
