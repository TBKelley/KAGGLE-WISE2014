KAGGLE-WISE2014
===============

Kaggle Greek Media Monitoring Multilabel Classification (WISE 2014)

## Installation
install.packages("SparseM")<br/>
install.packages("caret")<br/>
install.packages("e1071")<br/>
install.packages("doParallel")<br/>
install.packages("foreach")<br/>
<br/>
Download Training and Test data<br/>
<br/>
Folder structure<br/>
KAGGLE-WISE2014<br/>
	writeAnswer.R    - Create a submission file<br/>
	score.r          - Calculate F1 score<br/>
	processLabelBySVM_linear.R - doPar pay load<br/>
	wise2014_svm_V01.R<br/>
	wise2014_svm_V02.R<br/>
	wise2014_svm_V03.R<br/>
KAGGLE-WISE2014\Data<br/>
	wise2014-test.libsvm.txt<br/>
	wise2014-train.libsvm.txt<br/>
KAGGLE-WISE2014\Answer<br/>
	sampleSubmission.csv<br/>
KAGGLE-WISE2014\Model<br/>

## How to run
Set any parameters at the top of the program.<br/> 
<br/>
Main program is<br/>
wise2014_svm_02.R  - SVM Linear<br/>
wise2014_svm_03.R  - SVM Linear Parallel<br/>





