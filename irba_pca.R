# irba_pca.R
#
# The following code will create a reduced dense matrix using PCA techniques from
# the origin train and test files wise2014-train.libsvm.txt and wise2014-test.libsvm.txt
#
# Ref: http://illposed.net/irlb.html was used for the inspiration of this code.
# WARNING: There was a small bug in the matmul() function discribed in the artical.
#
# This process avoid large memory images by using the "matmul" paramater of the "irab"
# SVD package.
#
# Outputs:
# X.train.matrix  : A dense training feature matrix with dim=[64857 features.required]
# y.train.string  : Labels vector as strings
# X.test.matrix   : A dense test feature matrix with dim=[34923 features.required]
# irlba.svd       : SVD. You need to use irlba.svd$v to convert new X data into the reduced features set.
library(e1071)
library(rpart)
library(SparseM) # matrix.csr
library(Matrix)
library(irlba)

features.required <- 200 # SVD Elapse 5946 Seconds (1.5 Hrs)
features.required <- 100 # SVD Elapse  898 Seconds ( 15 Min)
features.required <-  10 # SVD Elapse   13 Seconds
features.required <-   5 # SVD Elapse   10 Seconds

dataUNC <- sprintf("Data/WiseData_%03d.RData", features.required)

# Read original test and train sparse matrix in e1071 matrix.csr format.
## Training data
system.time(data.train.matrix.csr <- read.matrix.csr("Data/wise2014-train.libsvm.txt"))[3]  # 172 Sec
X.train.matrix.csr <- data.train.matrix.csr$x # dim=[64857 125745] 197.2 MB RAM
y.train.factor <- data.train.matrix.csr$y     # length=64857 0.5 MB RAM
y.train.string <- paste(",", as.character(y.train.factor), ",", sep="")
rm(data.train.matrix.csr)
rm(y.train.factor)

## Test data
system.time(X.test.matrix.csr <- read.matrix.csr("Data/wise2014-test.libsvm.txt")$x)[3]    # dim=[34923 125745] 108.7 MB RAM,  56 Sec  


# Convert from matrix.csr format to sparseMatrix dgCMatrix format
# NOTE: dgCMatrix format works best with the irlba SVD
X.train.sparesMatrix <- as(X.train.matrix.csr, "sparseMatrix") # dim=[64857 125745]
rm(X.train.matrix.csr)
X.test.sparesMatrix <- as(X.test.matrix.csr, "sparseMatrix") # dim=[34923 125745]
rm(X.test.matrix.csr)

# Build a SVD which is the most efficent way to do PCA on sparse matrix
## Define a matmul() function to avoid large memory images in irlba SVD.
## matmul() will mean scale the input matrix A using minimal memory from within irlba
## A = X.train.sparesMatrix
matmul <- function(A)
{
    v <- colMeans(A) # Cache column mean
    function(A,x,transpose=FALSE)
    {
        if(transpose)
            return( as.matrix(t(crossprod(x,A)) - sum(x) * v))
        
        as.matrix(A %*% x - cbind(rep(crossprod(v,x)[1],dim(A)[1]))) # Found a bug in artical length(v) should dim(A)[1]
    }
}

# Calculate SVD 
# Only require irlba.svd$d and irlba.svd$v
system.time(irlba.svd <- irlba(X.train.sparesMatrix, nv=features.required, matmul=matmul(X.train.sparesMatrix)))[3]
#plot(irlba.svd$d) # To see how much of the variation is captured in the reduced features.

# Do PCA to get a reduced dense feature matrix
X.train.matrix <- as.matrix(X.train.sparesMatrix %*% irlba.svd$v)
X.test.matrix <- as.matrix(X.test.sparesMatrix %*% irlba.svd$v)

# Memory cleanup
rm(X.train.sparesMatrix)
rm(X.test.sparesMatrix)

# You can now use the dense matrix with any R machine learning packages.
dim(X.train.matrix)    # dim=[64857 features.required]
length(y.train.string) # len= 64857
dim(X.test.matrix)     # dim=[34923 features.required]

# Save reduced data to file for quick loading in future
save(y.train.string, X.train.matrix, X.test.matrix, file=dataUNC)
rm(y.train.string); rm(X.train.matrix); rm(X.test.matrix)

# Restart reduced data from file
load(dataUNC) # y.train.string, X.train.matrix, X.test.matrix
dim(X.train.matrix)    # dim=[64857 features.required]
length(y.train.string) # len= 64857
dim(X.test.matrix)     # dim=[34923 features.required]

