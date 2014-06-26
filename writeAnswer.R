writeAnswer <- function(v, outputFileUNC) {
    outputFile <- file(outputFileUNC)
    v.trim <- sub("^\\s+", "", v)
    writeLines(sprintf("%d,%s", (1:length(v.trim))+64858, v.trim), outputFile) # 64858= First test id
    close(outputFile)
}

