library(readxl)
library(writexl)
library(glmnet)
library(dplyr)
library(corrplot)
library(funModeling)
library(pheatmap)
library(broom)
library(pROC)
library(magrittr)
library(car)
library(stringr)
library(caret)
library(ROCR)
setwd("D:/R_code for 2023/New project/R_aml/")
#-----------(input the data)----------------------------------------------------
#173
aml_173<-as.data.frame(read.csv("ID_173.csv"))

#80
aml_80<-as.data.frame(read.csv("AML_nk_80_ID.csv"))
aml_80_01<-aml_80[,c(2:14,18:21)]


#-----------(logistic regression & model evaluation)-----------------------------------------
aml_173_01<-aml_173[,c(2:16)]

#(1)Confusion matrix and 

  

#(2)ROC CURVES
#(2-1) split dataset
set.seed(500)
random_sa = sample(nrow(aml_173_01), 80)
aml_trn = aml_173_01[random_sa, ]
aml_tst = aml_173_01[-random_sa, ]

glm_trn<-glm(OS_STATUS_01~FAB_01+
                    AGE_01+               
                    SEX_01+
                    WBC_01+
                    BM_BLAST_PERCENTAGE_01
                    ,family = "binomial", data = aml_trn)

exp(coef(glm_trn))
test_prob = predict(glm_trn, newdata = aml_tst, type = "response")
test_roc = roc(aml_tst$OS_STATUS_01 ~ test_prob, plot = TRUE, print.auc = TRUE)


ROCRpred = prediction(test_prob, aml_tst$OS_STATUS_01)



# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# Plot ROC curve
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,0.06,by=0.02), text.adj=c(-0.2,1.7))


