writeAnswer <- function(v, outputFileUNC) {
    outputFile <- file(outputFileUNC)
    v.trim <- sub("^\\s+", "", v)
    writeLines(c("ArticleId,Labels", sprintf("%d,%s", (1:length(v.trim))+64857, v.trim)), outputFile) # 64858= First test id
    close(outputFile)
}

