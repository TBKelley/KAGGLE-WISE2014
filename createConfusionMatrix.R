# createConfusionMatrix.R
#
# Create a 4x4 Confusion Matrix. Will ensure matrix shape and row/colun names.
#
#                  Reference - Title
#                  FALSE   TRUE
# Predicted FALSE  TN[1,1] FN[1,2]
#           TRUE   FP[2,1] TP[2,2]
#
# Predicted : y.cross.predict
# Reference : y.cross.bool.factor
# Title     : "SVM Cross"
#
# Unit Test
# Predicted<-c(FALSE, FALSE); Reference<-c(FALSE, FALSE); Title<-"Test1"
# Predicted<-c(FALSE, FALSE); Reference<-c(FALSE, TRUE); Title<-"Test2"
# Predicted<-c(FALSE, FALSE); Reference<-c(TRUE, FALSE); Title<-"Test3"
# Predicted<-c(FALSE, FALSE); Reference<-c(TRUE, TRUE); Title<-"Test4"

# Predicted<-c(FALSE, TRUE); Reference<-c(FALSE, FALSE); Title<-"Test5"
# Predicted<-c(FALSE, TRUE); Reference<-c(FALSE, TRUE); Title<-"Test6"
# Predicted<-c(FALSE, TRUE); Reference<-c(TRUE, FALSE); Title<-"Test7"
# Predicted<-c(FALSE, TRUE); Reference<-c(TRUE, TRUE); Title<-"Test8"

# Predicted<-c(TRUE, FALSE); Reference<-c(FALSE, FALSE); Title<-"Test9"
# Predicted<-c(TRUE, FALSE); Reference<-c(FALSE, TRUE); Title<-"Test10"
# Predicted<-c(TRUE, FALSE); Reference<-c(TRUE, FALSE); Title<-"Test11"
# Predicted<-c(TRUE, FALSE); Reference<-c(TRUE, TRUE); Title<-"Test12"

# Predicted<-c(TRUE, TRUE); Reference<-c(FALSE, FALSE); Title<-"Test13"
# Predicted<-c(TRUE, TRUE); Reference<-c(FALSE, TRUE); Title<-"Test14"
# Predicted<-c(TRUE, TRUE); Reference<-c(TRUE, FALSE); Title<-"Test15"
# Predicted<-c(TRUE, TRUE); Reference<-c(TRUE, TRUE); Title<-"Test16"
# createConfusionMatrix(Predicted=Predicted, Reference=Reference, Title=Title)

createConfusionMatrix <- function(Predicted, Reference, Title){
    column.Title <- sprintf("Reference - %s", Title)
    temp.table <- table(Predicted=Predicted, Reference=Reference)

    score.label.table <- table(c("FALSE","TRUE"), c("FALSE","TRUE")) * 0
    if ("FALSE" %in% rownames(temp.table) & "FALSE" %in% colnames(temp.table)) {
        score.label.table["FALSE", "FALSE"] <- temp.table["FALSE", "FALSE"]
    }
    if ("FALSE" %in% rownames(temp.table) & "TRUE" %in% colnames(temp.table)) {
        score.label.table["FALSE", "TRUE"] <- temp.table["FALSE", "TRUE"]
    }
    if ("TRUE" %in% rownames(temp.table) & "FALSE" %in% colnames(temp.table)) {
        score.label.table["TRUE", "FALSE"] <- temp.table["TRUE", "FALSE"]
    }
    if ("TRUE" %in% rownames(temp.table) & "TRUE" %in% colnames(temp.table)) {
        score.label.table["TRUE", "TRUE"] <- temp.table["TRUE", "TRUE"]
    }
    names(attributes(score.label.table)$dimnames) <- c("Predicted", column.Title)
    
    score.label.table
}
    
