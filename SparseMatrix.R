library(e1071)
library(rpart)
library(SparseM) # matrix.csr

system.time(X.matrix.csr <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$x)[3]  # 197.2 MB RAM, 291.89Sec 5 Min
system.time(Y.factor <- read.matrix.csr("Data/wise2014-train.libsvm.txt")$y)[3]  # 0.5 MB RAM, 166.75 Sec 3 Min

# WARNING: Y.factor =  "1","1,11,198,39",..
# so multiple lables need to be split.

system.time(write.matrix.csr(X.matrix.csr, y=Y.factor, file="Data/TestXY.txt"))[3]

m.full <- matrix(data=0, nrow=64857, ncol=301561) # 145.7 GB
m.1000 <- matrix(data=0, nrow=1000, ncol=301561) # 2.25 GB
round(object.size(m.1000)/(1024*1024*1024), 2) # 2.25 GB

df.1000 <- data.frame(m.1000) # 2.28 GB
round(object.size(df.1000)/(1024*1024*1024), 2) # 2.28 GB