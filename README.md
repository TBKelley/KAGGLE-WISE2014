KAGGLE-WISE2014
===============

Kaggle Greek Media Monitoring Multilabel Classification (WISE 2014)

## Installation
install.packages("SparseM")
install.packages("caret")
install.packages("e1071")
install.packages("doParallel")
install.packages("foreach")

Download Training and Test data
Folder structure

KAGGLE-WISE2014
	writeAnswer.R    - Create a submission file
	score.r          - Calculate F1 score
	processLabelBySVM_linear.R - doPar pay load
	wise2014_svm_V01.R
	wise2014_svm_V02.R
	wise2014_svm_V03.R	
KAGGLE-WISE2014\Data
	wise2014-test.libsvm.txt
	wise2014-train.libsvm.txt
KAGGLE-WISE2014\Answer
	sampleSubmission.csv
KAGGLE-WISE2014\Model

## How to run
Set any parameters at the top of the program. 

Main program is
wise2014_svm_02.R  - SVM Linear
wise2014_svm_03.R  - SVM Linear Parallel



