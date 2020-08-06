if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not

pacman::p_load("readxl", "openxlsx", "caret", "partykit", "ROCR", "lift", "glmnet", "randomForest", "xgboost",
               "rpart", "MASS","e1071", "tensorflow", "keras", "plyr", "dplyr", "ggplot2") #Check, and if needed install the necessary packages

creditdata <- read_excel(file.choose())
creditapp <- read_excel(file.choose())
creditapp$default_0 <- NA
credit_all <- rbind(creditdata, creditapp)

credit_all$EDUCATION <- ifelse(credit_all$EDUCATION == 0, 4, credit_all$EDUCATION)
credit_all$MARRIAGE <- ifelse(credit_all$MARRIAGE == 0, 3, credit_all$MARRIAGE)
credit_all$AGE <- as.numeric(credit_all$AGE)
credit_all <- mutate(credit_all, AGE_BIN= case_when(AGE >= 60  & AGE <= 90 ~ '5',
                                                    AGE >= 50  & AGE <= 59 ~ '4',
                                                    AGE >= 40  & AGE <= 49 ~ '3',
                                                    AGE >= 30  & AGE <= 39 ~ '2',
                                                    AGE >= 20  & AGE <= 29 ~ '1'))

# remaining credit balance after payment
credit_all$END_BAL1 <- credit_all$BILL_AMT1 - credit_all$PAY_AMT1
credit_all$END_BAL2 <- credit_all$BILL_AMT2 - credit_all$PAY_AMT2
credit_all$END_BAL3 <- credit_all$BILL_AMT3 - credit_all$PAY_AMT3
credit_all$END_BAL4 <- credit_all$BILL_AMT4 - credit_all$PAY_AMT4
credit_all$END_BAL5 <- credit_all$BILL_AMT5 - credit_all$PAY_AMT5
credit_all$END_BAL6 <- credit_all$BILL_AMT6 - credit_all$PAY_AMT6

# credit utilization (monthly statement balance / total limit)
credit_all$UTILI_1 <- credit_all$BILL_AMT1 / credit_all$LIMIT_BAL
credit_all$UTILI_2 <- credit_all$BILL_AMT2 / credit_all$LIMIT_BAL
credit_all$UTILI_3 <- credit_all$BILL_AMT3 / credit_all$LIMIT_BAL
credit_all$UTILI_4 <- credit_all$BILL_AMT4 / credit_all$LIMIT_BAL
credit_all$UTILI_5 <- credit_all$BILL_AMT5 / credit_all$LIMIT_BAL
credit_all$UTILI_6 <- credit_all$BILL_AMT6 / credit_all$LIMIT_BAL
billamt <- c("BILL_AMT1", "BILL_AMT2", "BILL_AMT3", "BILL_AMT4", "BILL_AMT5", "BILL_AMT6")
utilization <- c("UTILI_1", "UTILI_2", "UTILI_3", "UTILI_4", "UTILI_5", "UTILI_6")
for (i in 1:6) {
  idx <- which(credit_all[, billamt[i]] < 0)
  credit_all[idx, utilization[i]] = 0
}

# average utilization
credit_all$AVGUTILI_16 <- (credit_all$UTILI_1+credit_all$UTILI_2+credit_all$UTILI_3+credit_all$UTILI_4+credit_all$UTILI_5+credit_all$UTILI_6)/6
credit_all$AVGUTILI_15 <- (credit_all$UTILI_1+credit_all$UTILI_2+credit_all$UTILI_3+credit_all$UTILI_4+credit_all$UTILI_5)/5
credit_all$AVGUTILI_14 <- (credit_all$UTILI_1+credit_all$UTILI_2+credit_all$UTILI_3+credit_all$UTILI_4)/4
credit_all$AVGUTILI_13 <- (credit_all$UTILI_1+credit_all$UTILI_2+credit_all$UTILI_3)/3
credit_all$AVGUTILI_46 <- (credit_all$UTILI_4+credit_all$UTILI_5+credit_all$UTILI_6)/3
#credit_all$AVGUTILI_13_16 <- credit_all$AVGUTILI_13 / credit_all$AVGUTILI_16
#credit_all$AVGUTILI_46_16 <- credit_all$AVGUTILI_46 / credit_all$AVGUTILI_16
credit_all$AVGUTILI_13_46 <- credit_all$AVGUTILI_13 - credit_all$AVGUTILI_46

# credit balance percentage (ending balance / total limit)
credit_all$BAL_PERC1 <- credit_all$END_BAL1 / credit_all$LIMIT_BAL
credit_all$BAL_PERC2 <- credit_all$END_BAL2 / credit_all$LIMIT_BAL
credit_all$BAL_PERC3 <- credit_all$END_BAL3 / credit_all$LIMIT_BAL
credit_all$BAL_PERC4 <- credit_all$END_BAL4 / credit_all$LIMIT_BAL
credit_all$BAL_PERC5 <- credit_all$END_BAL5 / credit_all$LIMIT_BAL
credit_all$BAL_PERC6 <- credit_all$END_BAL6 / credit_all$LIMIT_BAL
endbal <- c("END_BAL1", "END_BAL2", "END_BAL3", "END_BAL4", "END_BAL5", "END_BAL6")
endutilization <- c("BAL_PERC1", "BAL_PERC2", "BAL_PERC3", "BAL_PERC4", "BAL_PERC5", "BAL_PERC6")
for (i in 1:6) {
  idx <- which(credit_all[, endbal[i]] < 0)
  credit_all[idx, endutilization[i]] = 0
}

# Spent / Limit
credit_all$SPENT_PERC1 <- (credit_all$BILL_AMT1 - credit_all$BILL_AMT2 + credit_all$PAY_AMT2) / credit_all$LIMIT_BAL #Spending in last month
credit_all$SPENT_PERC2 <- (credit_all$BILL_AMT2 - credit_all$BILL_AMT3 + credit_all$PAY_AMT3) / credit_all$LIMIT_BAL #Spending in 2nd last month
credit_all$SPENT_PERC3 <- (credit_all$BILL_AMT3 - credit_all$BILL_AMT4 + credit_all$PAY_AMT4) / credit_all$LIMIT_BAL #Spending in 3rd last month
credit_all$SPENT_PERC4 <- (credit_all$BILL_AMT4 - credit_all$BILL_AMT5 + credit_all$PAY_AMT5) / credit_all$LIMIT_BAL #Spending in 4th last month
credit_all$SPENT_PERC5 <- (credit_all$BILL_AMT5 - credit_all$BILL_AMT6 + credit_all$PAY_AMT6) / credit_all$LIMIT_BAL #Spending in 5th last month
spentamt <- c("SPENT_PERC1", "SPENT_PERC2", "SPENT_PERC3", "SPENT_PERC4", "SPENT_PERC5")
for (i in 1:5) {
  idx <- which(credit_all[, spentamt[i]] < 0)
  credit_all[idx, spentamt[i]] = 0
}

# Pay / Bill
credit_all$payratio1 <- credit_all$PAY_AMT1 / credit_all$BILL_AMT1
credit_all$payratio2 <- credit_all$PAY_AMT2 / credit_all$BILL_AMT2
credit_all$payratio3 <- credit_all$PAY_AMT3 / credit_all$BILL_AMT3
credit_all$payratio4 <- credit_all$PAY_AMT4 / credit_all$BILL_AMT4
credit_all$payratio5 <- credit_all$PAY_AMT5 / credit_all$BILL_AMT5
credit_all$payratio6 <- credit_all$PAY_AMT6 / credit_all$BILL_AMT6
payratios <- c("payratio1", "payratio2", "payratio3", "payratio4", "payratio5", "payratio6")
for (i in 1:6) {
  idx <- which(credit_all[,billamt[i]] <= 0)
  credit_all[idx, payratios[i]] = 1 
}

# Sex & Marriage
credit_all <- mutate(credit_all, SEX_MAR = case_when(SEX == 1 & MARRIAGE == 1 ~ "1", #Married Men
                                                     SEX == 1 & MARRIAGE == 2 ~ "2", #Single Men
                                                     SEX == 1 & MARRIAGE == 3 ~ "3", #Other Marital Status Men
                                                     SEX == 2 & MARRIAGE == 1 ~ "4", #Married Women
                                                     SEX == 2 & MARRIAGE == 2 ~ "5", #Single Women
                                                     SEX == 2 & MARRIAGE == 3 ~ "6", #Other Marital Status Women
))
credit_all$SEX_MAR <- as.factor(credit_all$SEX_MAR)
summary(credit_all$SEX_MAR)

# Sex & Age
credit_all <- mutate(credit_all, SEX_AGE = case_when(SEX==1  & AGE_BIN == 1  ~ '1', #man in 20s
                                                     SEX==1  & AGE_BIN == 2  ~ '2', #man in 30s
                                                     SEX==1  & AGE_BIN == 3  ~ '3', #man in 40s
                                                     SEX==1  & AGE_BIN == 4  ~ '4', #man in 50s
                                                     SEX==1  & AGE_BIN == 5  ~ '5', #man with age>= 60
                                                     SEX==2  & AGE_BIN == 1  ~ '6', #woman in 20s
                                                     SEX==2  & AGE_BIN == 2  ~ '7', #woman in 30s
                                                     SEX==2  & AGE_BIN == 3  ~ '8', #woman in 40s
                                                     SEX==2  & AGE_BIN == 4  ~ '9', #woman in 50s
                                                     SEX==2  & AGE_BIN == 5  ~ '10' #woman with age>= 60
))
credit_all$SEX_AGE <- as.factor(credit_all$SEX_AGE)

# Sex & Education
credit_all <- mutate(credit_all, SEX_ED = case_when(SEX==1  & EDUCATION == 1  ~ '1',  #graduate man
                                                    SEX==1  & EDUCATION == 2  ~ '2',  #undergraduate man
                                                    SEX==1  & EDUCATION == 3  ~ '3',  #man + high school
                                                    SEX==1  & EDUCATION == 4  ~ '4',  #man +other
                                                    SEX==1  & EDUCATION == 5  ~ '5',  #man +unknown 5
                                                    SEX==1  & EDUCATION == 6  ~ '6',  #man +unknown 6
                                                    SEX==2  & EDUCATION == 1  ~ '7',  #graduate woman
                                                    SEX==2  & EDUCATION == 2  ~ '8',  #undergraduate woman
                                                    SEX==2  & EDUCATION == 3  ~ '9',  #woman + high school
                                                    SEX==2  & EDUCATION == 4  ~ '10', #woman +other
                                                    SEX==2  & EDUCATION == 5  ~ '11', #woman +unknown 5
                                                    SEX==2  & EDUCATION == 6  ~ '12'  #woman +unknown 6
))
credit_all$SEX_ED <- as.factor(credit_all$SEX_ED)

# Age & Education
credit_all <- mutate(credit_all, AGE_ED = case_when(AGE_BIN==1  & EDUCATION == 1  ~ '1',  #
                                                    AGE_BIN==1  & EDUCATION == 2  ~ '2',  #
                                                    AGE_BIN==1  & EDUCATION == 3  ~ '3',  #
                                                    AGE_BIN==1  & EDUCATION == 4  ~ '4',  #
                                                    AGE_BIN==1  & EDUCATION == 5  ~ '5',  #
                                                    AGE_BIN==1  & EDUCATION == 6  ~ '6',  #
                                                    AGE_BIN==2  & EDUCATION == 1  ~ '7',  #
                                                    AGE_BIN==2  & EDUCATION == 2  ~ '8',  #
                                                    AGE_BIN==2  & EDUCATION == 3  ~ '9',  #
                                                    AGE_BIN==2  & EDUCATION == 4  ~ '10', #
                                                    AGE_BIN==2  & EDUCATION == 5  ~ '11', #
                                                    AGE_BIN==2  & EDUCATION == 6  ~ '12', #
                                                    AGE_BIN==3  & EDUCATION == 1  ~ '13', #
                                                    AGE_BIN==3  & EDUCATION == 2  ~ '14', #
                                                    AGE_BIN==3  & EDUCATION == 3  ~ '15', #
                                                    AGE_BIN==3  & EDUCATION == 4  ~ '16', #
                                                    AGE_BIN==3  & EDUCATION == 5  ~ '17', #
                                                    AGE_BIN==3  & EDUCATION == 6  ~ '18', #
                                                    AGE_BIN==4  & EDUCATION == 1  ~ '19', #
                                                    AGE_BIN==4  & EDUCATION == 2  ~ '20', #
                                                    AGE_BIN==4  & EDUCATION == 3  ~ '21', #
                                                    AGE_BIN==4  & EDUCATION == 4  ~ '22', #
                                                    AGE_BIN==4  & EDUCATION == 5  ~ '23', #
                                                    AGE_BIN==4  & EDUCATION == 6  ~ '24', #
                                                    AGE_BIN==5  & EDUCATION == 1  ~ '25', #
                                                    AGE_BIN==5  & EDUCATION == 2  ~ '26', #
                                                    AGE_BIN==5  & EDUCATION == 3  ~ '27', #
                                                    AGE_BIN==5  & EDUCATION == 4  ~ '28', #
                                                    AGE_BIN==5  & EDUCATION == 5  ~ '29', #
                                                    AGE_BIN==5  & EDUCATION == 6  ~ '30'  #
))
credit_all$AGE_ED <- as.factor(credit_all$AGE_ED)

credit_all$delay_1 <- ifelse((credit_all$PAY_1) > 2, 1, 0)
credit_all$delay_2 <- ifelse((credit_all$PAY_2) > 2, 1, 0)
credit_all$delay_3 <- ifelse((credit_all$PAY_3) > 2, 1, 0)
credit_all$delay_4 <- ifelse((credit_all$PAY_4) > 2, 1, 0)
credit_all$delay_5 <- ifelse((credit_all$PAY_5) > 2, 1, 0)
credit_all$delay_6 <- ifelse((credit_all$PAY_6) > 2, 1, 0)

credit_all$delay_ALL <- as.factor(ifelse(((credit_all$delay_1==1) & (credit_all$delay_2==1) & (credit_all$delay_3==1) & 
                                            (credit_all$delay_4==1) & (credit_all$delay_5==1) & (credit_all$delay_1)), 1, 0))
#table(credit_all$delay_ALL)

credit_all$Overlimit1 <- as.factor(ifelse((credit_all$BILL_AMT1 > credit_all$LIMIT_BAL), 1, 0))
credit_all$Overlimit2 <- as.factor(ifelse((credit_all$BILL_AMT2 > credit_all$LIMIT_BAL), 1, 0))
credit_all$Overlimit3 <- as.factor(ifelse((credit_all$BILL_AMT3 > credit_all$LIMIT_BAL), 1, 0))
credit_all$Overlimit4 <- as.factor(ifelse((credit_all$BILL_AMT4 > credit_all$LIMIT_BAL), 1, 0))
credit_all$Overlimit5 <- as.factor(ifelse((credit_all$BILL_AMT5 > credit_all$LIMIT_BAL), 1, 0))
credit_all$Overlimit6 <- as.factor(ifelse((credit_all$BILL_AMT6 > credit_all$LIMIT_BAL), 1, 0))

credit_all$Overlimit_FLAG <- as.factor(ifelse(((credit_all$Overlimit1==1) & (credit_all$Overlimit2==1) & (credit_all$Overlimit3==1) &
                                                 (credit_all$Overlimit4==1) & (credit_all$Overlimit5==1) & (credit_all$Overlimit6==1)), 1, 0))
#table(credit_all$Overlimit_FLAG)

credit_all$SEX <-as.factor(credit_all$SEX)
credit_all$EDUCATION <- as.factor(credit_all$EDUCATION)
credit_all$MARRIAGE <- as.factor(credit_all$MARRIAGE)
credit_all$AGE_BIN <- as.factor(credit_all$AGE_BIN)
credit_all$PAY_1 <- as.factor(credit_all$PAY_1)
credit_all$PAY_2 <- as.factor(credit_all$PAY_2)
credit_all$PAY_3 <- as.factor(credit_all$PAY_3)
credit_all$PAY_4 <- as.factor(credit_all$PAY_4)
credit_all$PAY_5 <- as.factor(credit_all$PAY_5)
credit_all$PAY_6 <- as.factor(credit_all$PAY_6)
credit_all$default_0 <- as.factor(credit_all$default_0)
credit_all$delay_1 <- as.factor(credit_all$delay_1)
credit_all$delay_2 <- as.factor(credit_all$delay_2)
credit_all$delay_3 <- as.factor(credit_all$delay_3)
credit_all$delay_4 <- as.factor(credit_all$delay_4)
credit_all$delay_5 <- as.factor(credit_all$delay_5)
credit_all$delay_6 <- as.factor(credit_all$delay_6)

# split back to data & app
credit_data <- credit_all[1:24000,]
credit_app <- credit_all[-c(1:24000),]

#credit_all_1 <- credit_all[,-c(66:79)]
#credit_data <- credit_all_1[1:24000,]
#credit_app <- credit_all_1[-c(1:24000),]

credit_data$ID <- as.numeric(credit_data$ID)
credit_app$ID <- as.factor(credit_app$ID)
app_id <- credit_app$ID #save original app data ID separately
credit_app$ID <- as.numeric(c(24001:25000))

set.seed(77850)
inTrain <- createDataPartition(y = credit_data$ID,
                               p = 19199/24000, list = FALSE)
training <- credit_data[inTrain,]
testing <- credit_data[-inTrain,]

#Logistic Modeling
model_logistic <- glm(default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE_BIN + SEX_MAR + SEX_AGE + SEX_ED + AGE_ED +
                        PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                        BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                        PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +
                        END_BAL1 + END_BAL2 + END_BAL3 + END_BAL4 + END_BAL5 + END_BAL6 +
                        UTILI_1 + UTILI_2 + UTILI_3 + UTILI_4 + UTILI_5 + UTILI_6 +
                        BAL_PERC1 + BAL_PERC2 + BAL_PERC3 + BAL_PERC4 + BAL_PERC5 + BAL_PERC6 +
                        SPENT_PERC1 + SPENT_PERC2 + SPENT_PERC3 + SPENT_PERC4 + SPENT_PERC5 +
                        AVGUTILI_16 + AVGUTILI_15 + AVGUTILI_14 + AVGUTILI_13 + AVGUTILI_46 + AVGUTILI_13_46 +
                        payratio1 + payratio2 + payratio3 + payratio4 + payratio5 + payratio6 +
                        delay_1 + delay_2 + delay_3 + delay_4 + delay_5 + delay_6 + delay_ALL +
                        Overlimit1 + Overlimit2 + Overlimit3 + Overlimit4 + Overlimit5 + Overlimit6 + Overlimit_FLAG,
                      data = training, family="binomial"(link="logit"))

model_logistic_0 <- glm(default_0 ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE_BIN + SEX_MAR + SEX_AGE + SEX_ED + AGE_ED +
                          PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                          BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                          PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +
                          END_BAL1 + END_BAL2 + END_BAL3 + END_BAL4 + END_BAL5 + END_BAL6 +
                          UTILI_1 + UTILI_2 + UTILI_3 + UTILI_4 + UTILI_5 + UTILI_6 +
                          BAL_PERC1 + BAL_PERC2 + BAL_PERC3 + BAL_PERC4 + BAL_PERC5 + BAL_PERC6 +
                          SPENT_PERC1 + SPENT_PERC2 + SPENT_PERC3 + SPENT_PERC4 + SPENT_PERC5 +
                          AVGUTILI_16 + AVGUTILI_15 + AVGUTILI_14 + AVGUTILI_13 + AVGUTILI_46 + AVGUTILI_13_46 +
                          payratio1 + payratio2 + payratio3 + payratio4 + payratio5 + payratio6,
                        data = training, family="binomial"(link="logit"))

#summary(model_logistic)

#model_logistic_stepwiseAIC_0 <- stepAIC(model_logistic_0,direction = c("both"),trace = 1) #AIC stepwise
model_logistic_stepwiseAIC <- stepAIC(model_logistic,direction = c("both"),trace = 1) #AIC stepwise
#summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC_0) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC, newdata=testing, type="response") #Predict probabilities
logistic_classification<-rep("1",4800)
logistic_classification[logistic_probabilities < nrow(filter(credit_data, default_0 == "1"))/nrow(credit_data)]="0" 
logistic_classification<-as.factor(logistic_classification)

###Confusion matrix  
logistic_confusionmatrix <- confusionMatrix(logistic_classification,testing$default_0, positive = "1") #Display confusion matrix
logistic_sensitivity <- as.data.frame(logistic_confusionmatrix[4])[1,1]
logistic_specificity <- as.data.frame(logistic_confusionmatrix[4])[2,1]

####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
logistic_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### CTREE 
ctree_tree<-ctree(default_0~.,data=training) #Run ctree on training data
plot(ctree_tree, gp = gpar(fontsize = 8)) #Plotting the tree (adjust fontsize if needed)

ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") #Predict probabilities
ctree_classification<-rep("1",4800)
ctree_classification[ctree_probabilities[,2] < nrow(filter(credit_data, default_0 == "1"))/nrow(credit_data)]="0" #Predict classification using 0.6073 threshold. Why 0.6073 - that's the average probability of being retained in the data. An alternative code: logistic_classification <- as.integer(logistic_probabilities > mean(testing$default_0 == "1"))
ctree_classification<-as.factor(ctree_classification)

###Confusion matrix  
ctree_confusionmatrix <- confusionMatrix(ctree_classification,testing$default_0, positive = "1")
ctree_sensitivity <- as.data.frame(ctree_confusionmatrix[4])[1,1]
ctree_specificity <- as.data.frame(ctree_confusionmatrix[4])[2,1]

####ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") #Predict probabilities
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) #Calculate errors
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") #Create ROC curve data
plot(ctree_ROC_testing) #Plot ROC curve

####AUC (area under curve)
auc.tmp <- performance(ctree_pred_testing,"auc") #Create AUC data
ctree_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
ctree_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value

#### Lift chart
plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

###
### RPART
CART_cp = rpart.control(cp = 0.0015) #set cp to a small number to "grow" a large tree

rpart_tree<-rpart(default_0~.,data=training, method="class", control=CART_cp) #"Grow" a tree on training data

prunned_rpart_tree<-prune(rpart_tree, cp=0.0017) #Prun the tree. Play with cp to see how the resultant tree changes
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) #Plotting the tree (adjust fontsize if needed)

# Understand the relationship between the cross-validated error, size of the tree and cp.
plotcp(rpart_tree) # Use printcp(rpart_tree) to print the values. As a rule of thumb pick up the largest cp which does not give a substantial drop in error

rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") #Predict classification (for confusion matrix)
rpart_confusionmatrix <- confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") #Display confusion matrix
rpart_sensitivity <- as.data.frame(rpart_confusionmatrix[4])[1,1]
rpart_specificity <- as.data.frame(rpart_confusionmatrix[4])[2,1]

rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") #Predict probabilities
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) #Calculate errors
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") #Create ROC curve data
plot(rpart_ROC_testing) #Plot ROC curve

auc.tmp <- performance(rpart_pred_testing,"auc") #Create AUC data
rpart_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
rpart_auc_testing #Display AUC value

plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

### Random Forest
###
model_forest <- randomForest(default_0~ ., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 500,           # hyperparameter: number of trees in the forest
                             mtry = 10,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.5, 0.5)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
)
plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot
varImpPlot(model_forest) # plots variable importances; use importance(model_forest) to print the values

###Finding predicitons: probabilities and classification
forest_probabilities<-predict(model_forest, newdata=testing, type="prob") #Predict probabilities -- an array with 2 columns: for not retained (class 0) and for retained (class 1)
forest_classification<-rep("1",4800)
forest_classification[forest_probabilities[,2]<0.5]="0" #Predict classification using 0.5 threshold. Why 0.5 and not 0.6073? Use the same as in cutoff above
forest_classification<-as.factor(forest_classification)

forest_confusionmatrix <- confusionMatrix(forest_classification,testing$default_0, positive="1") #Display confusion matrix. Note, confusion matrix actually displays a better accuracy with threshold of 50%
forest_sensitivity <- as.data.frame(forest_confusionmatrix[4])[1,1]
forest_specificity <- as.data.frame(forest_confusionmatrix[4])[2,1]

####ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) #Calculate errors
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(forest_ROC) #Plot ROC curve
####AUC (area under curve)
AUC.tmp <- performance(forest_ROC_prediction,"auc") #Create AUC data
forest_AUC <- as.numeric(AUC.tmp@y.values) #Calculate AUC
forest_AUC #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
#### Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_forest <- performance(forest_ROC_prediction,"lift","rpp")
plot(Lift_forest)

### Gradient Boosting Machines (XGboost)
# As many other methods, xgboost requires matrices as inputs
credit_data_matrix <- model.matrix(default_0~ ., data = credit_data)[,-1]

x_train <- credit_data_matrix[ inTrain,]
x_test <- credit_data_matrix[ -inTrain,]

y_train <- training$default_0
y_test <- testing$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction <- predict(model_XGboost,newdata=x_test, type="response") #Predict classification (for confusion matrix)

xgboost_confusionmatrix <- confusionMatrix(as.factor(ifelse(XGboost_prediction > nrow(filter(credit_all, default_0 == "1"))/nrow(credit_all), 1, 0)),y_test,positive="1") #Display confusion matrix
xgboost_sensitivity <- as.data.frame(xgboost_confusionmatrix[4])[1,1]
xgboost_specificity <- as.data.frame(xgboost_confusionmatrix[4])[2,1]
####ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) #Calculate errors
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(XGboost_ROC_testing) #Plot ROC curve
####AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") #Create AUC data
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
XGboost_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_XGboost <- performance(XGboost_ROC_prediction,"lift","rpp")
plot(Lift_XGboost)

### Support Vector Machines
###
model_svm <- svm(default_0 ~., data=training, probability=TRUE)
summary(model_svm)

svm_probabilities<-attr(predict(model_svm,newdata=testing, probability=TRUE), "prob")
svm_prediction<-svm_probabilities[,1]

svm_classification<-rep("1",4800)
svm_classification[svm_prediction < nrow(filter(credit_all, default_0 == "1"))/nrow(credit_all)]="0" 
svm_classification<-as.factor(svm_classification)
svm_confusionmatrix <- confusionMatrix(svm_classification,testing$default_0,positive = "1")
svm_sensitivity <- as.data.frame(svm_confusionmatrix[4])[1,1]
svm_specificity <- as.data.frame(svm_confusionmatrix[4])[2,1]
####ROC Curve
svm_ROC_prediction <- prediction(svm_prediction, testing$default_0) #Calculate errors
svm_ROC_testing <- performance(svm_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(svm_ROC_testing) #Plot ROC curve
####AUC
auc.tmp <- performance(svm_ROC_prediction,"auc") #Create AUC data
svm_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
svm_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
#### Lift chart
plotLift(svm_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_svm <- performance(svm_ROC_prediction,"lift","rpp")
plot(Lift_svm)


### Deep Learning with TensorFlow
###
# Preprocessing data for inputting into Keras
# Tensors are matrices... hence the input data has to be in a form of a matrix
credit_data_matrix <- model.matrix(default_0~ ., data = credit_data)[,-1]
credit_data_matrix <- scale(credit_data_matrix) # scaling for X

x_train <- credit_data_matrix[ inTrain,]
x_test <- credit_data_matrix[ -inTrain,]

y_train <-training$default_0
y_test <-testing$default_0

x_train <- array_reshape(x_train, c(nrow(x_train), as.numeric(dim(x_train)[2]))) #Keras interprets data using row-major semantics (as opposed to R's default column-major semantics). Hence need to "reshape" the matrices 
x_test <- array_reshape(x_test, c(nrow(x_test), as.numeric(dim(x_train)[2])))

y_train <- to_categorical(y_train, 2) # converting to categorical for Y
y_test <- to_categorical(y_test, 2) # converting to categorical for Y

#
# Defining the neural network model architecture: layers, units, activations. 
#
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.3) %>% 
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = 'softmax')

#
# Compiling the model 
#
model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

# Training / "fitting" the model
history <- model %>% fit(
  x_train, y_train, # on what data to train
  epochs = 30, # how many repetitions to have
  batch_size = 128, # how many datapoints are fed to the network at a time 
  validation_split = 0.2  # percentage of training data to keep for cross-validation 
)

summary(model)
plot(history)

# model %>% evaluate(x_test, y_test) # apply the model to testing data
TF_NN_probabilities <- model %>% predict(x_test)  # predict probabilities
TF_NN_prediction <- TF_NN_probabilities[,2]

TF_NN_classification<-rep("1",4800)
TF_NN_classification[TF_NN_prediction < nrow(filter(credit_data, default_0 == "1"))/nrow(credit_data)]="0" 
TF_NN_classification<-as.factor(TF_NN_classification)
###Confusion Matrix
TF_confusionmatrix <- confusionMatrix(TF_NN_classification,testing$default_0, positive = "1")
TF_sensitivity <- as.data.frame(TF_confusionmatrix[4])[1,1]
TF_specificity <- as.data.frame(TF_confusionmatrix[4])[2,1]
####ROC Curve
TF_NN_ROC_prediction <- prediction(TF_NN_prediction, testing$default_0) #Calculate errors
TF_NN_ROC_testing <- performance(TF_NN_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(TF_NN_ROC_testing) #Plot ROC curve
####AUC
auc.tmp <- performance(TF_NN_ROC_prediction,"auc") #Create AUC data
TF_NN_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
TF_NN_auc_testing #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
#### Lift chart
plotLift(TF_NN_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
### An alternative way is to plot a Lift curve not by buckets, but on all data points
Lift_TF_NN <- performance(TF_NN_ROC_prediction,"lift","rpp")
plot(Lift_TF_NN)

###
# Profit Function
profit_func <- function(confusionmatrix) {
  confusion_matrix <- data.frame("0" = c(data.frame(confusionmatrix[2])[,3][1], data.frame(confusionmatrix[2])[,3][2]),
                                 "1" = c(data.frame(confusionmatrix[2])[,3][3], data.frame(confusionmatrix[2])[,3][4]))
  rownames(confusion_matrix) <- c("0", "1")
  
  pred_0 <- sum(confusion_matrix[1,])
  pred_1 <- sum(confusion_matrix[2.])
  actual_0 <- sum(confusion_matrix[,1])
  actual_0 <- sum(confusion_matrix[,2])
  total <- sum(confusion_matrix)
  
  correct_pred0 <- confusion_matrix[1,1] / pred_0
  incorrect_pred0 <- 1 - correct_pred0
  x_0 <- 1000 * pred_0 / total
  
  revenue <- 1500 * x_0 * correct_pred0
  loss <- 5000 * x_0 * incorrect_pred0
  profit <- revenue - loss
  
  return(profit)
}

output_79 <- data.frame("model" = c("Logistic", "CTree", "RPart", "RandomForest", "GBM", "SVM", "TensorFlow"),
                        "Profit" = c(profit_func(logistic_confusionmatrix), profit_func(ctree_confusionmatrix), profit_func(rpart_confusionmatrix),
                                     profit_func(forest_confusionmatrix), profit_func(xgboost_confusionmatrix), profit_func(svm_confusionmatrix),
                                     profit_func(TF_confusionmatrix)),
                        "AUC" = c(logistic_auc_testing, ctree_auc_testing, rpart_auc_testing, forest_AUC, XGboost_auc_testing, svm_auc_testing, TF_NN_auc_testing),
                        "Sensitivity" = c(logistic_sensitivity, ctree_sensitivity, rpart_sensitivity, forest_sensitivity, xgboost_sensitivity, svm_sensitivity, TF_sensitivity),
                        "Specificity" = c(logistic_specificity, ctree_specificity, rpart_specificity, forest_specificity, xgboost_specificity, svm_specificity, TF_specificity))

output_65 <- data.frame("model" = c("Logistic", "CTree", "RPart", "RandomForest", "GBM", "TensorFlow"),
                        "Profit" = c(profit_func(logistic_confusionmatrix), profit_func(ctree_confusionmatrix), profit_func(rpart_confusionmatrix),
                                     profit_func(forest_confusionmatrix), profit_func(xgboost_confusionmatrix), profit_func(TF_confusionmatrix)),
                        "AUC" = c(logistic_auc_testing, ctree_auc_testing, rpart_auc_testing, forest_AUC, XGboost_auc_testing, TF_NN_auc_testing),
                        "Sensitivity" = c(logistic_sensitivity, ctree_sensitivity, rpart_sensitivity, forest_sensitivity, xgboost_sensitivity, TF_sensitivity),
                        "Specificity" = c(logistic_specificity, ctree_specificity, rpart_specificity, forest_specificity, xgboost_specificity, TF_specificity))
output_79
output_65

###
# Final Prediction using Deep Learning with TensorFlow
###
credit_all$ID <- as.numeric(c(1:25000))
training <- credit_all[c(1:24000),]
testing <- credit_all[-c(1:24000),]

###Finding predicitons: probabilities and classification
logistic_probabilities<-predict(model_logistic_stepwiseAIC, newdata=testing, type="response") #Predict probabilities
logistic_classification<-rep("1",1000)
logistic_classification[logistic_probabilities < nrow(filter(training, default_0 == "1"))/nrow(training)]="0" 
#logistic_classification<-as.factor(logistic_classification)
logistic_classification<-as.numeric(logistic_classification)
swapped <- ifelse(logistic_classification == 0, 1, 0)

output_df <- data.frame("ID" = app_id,
                        "Decision" = swapped)
write.xlsx(output_df, "MMA867 Assignment 3 Credit Prediction Output.xlsx")

# Question 2
#
prob_df <- data.frame("ID" = app_id,
                      "Probability" = logistic_probabilities,
                      "Decision" = swapped)
prob_df <- prob_df
head(prob_df)
prob_df[order(prob_df$Probability),][c(1:3),] #Top 3 for repay loan
prob_df[order(-prob_df$Probability),][c(1:3),] #Bottom 3 for repay loan
#
confmatr_num <- data.frame(logistic_confusionmatrix[2])[,3]
positive_accuracy <- confmatr_num[1] / (confmatr_num[1] + confmatr_num[3])
negative_accuracy <- confmatr_num[2] / (confmatr_num[2] + confmatr_num[4])
# We have high positive prediction accuracy, thus we are fairly confident that A will repay
# The negative prediction accuracy is only 50%, thus we might need to do extra due diligence
#
rob_threshold <- nrow(filter(credit_data, default_0 == "1"))/nrow(credit_data)
threshold_list <- c(prob_threshold - 0.1, prob_threshold - 0.075, prob_threshold - 0.05, prob_threshold - 0.025, prob_threshold, prob_threshold + 0.025, prob_threshold + 0.05, prob_threshold + 0.075, prob_threshold + 0.1)

credit_approval_summary <- data.frame("Type" = c("Correct Approval #", "Sensitivity", "Approval #", "Specificity"))
roc_summary <- c() #to store ROC data

for (i in 1:length(threshold_list)) {
  logistic_probabilities_2<-predict(model_logistic_stepwiseAIC, newdata=testing, type="response") #Predict probabilities
  logistic_classification_2<-rep("1",4800)
  logistic_classification_2[logistic_probabilities_2 < threshold_list[i]]="0"
  logistic_classification_2<-as.factor(logistic_classification_2)
  logistic_confusionmatrix_2 <- confusionMatrix(logistic_classification_2,testing$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_2 <- prediction(logistic_probabilities_2, testing$default_0)
  logistic_ROC_2 <- performance(logistic_ROC_prediction_2,"tpr","fpr") #Create ROC curve data
  #
  num_correct_yes <- data.frame(logistic_confusionmatrix_2[2])[,3][1]
  confmatr_sensitivity <- data.frame(logistic_confusionmatrix_2[4])[1,1]
  num_yes <- data.frame(logistic_confusionmatrix_2[2])[,3][1] + data.frame(logistic_confusionmatrix_2[2])[,3][3]
  confmatr_specificity <- data.frame(logistic_confusionmatrix_2[4])[2,1]
  
  credit_approval_rate <- data.frame("clname" = c(num_correct_yes, confmatr_sensitivity, num_yes, confmatr_specificity))
  names(credit_approval_rate) <- paste("@", round(threshold_list[i], 3))
  credit_approval_summary <- cbind(credit_approval_summary, credit_approval_rate)
  #
  roc_summary <- c(roc_summary, logistic_ROC_2)
}
#
credit_approval_summary
#
plot(roc_summary[[1]], colorize = TRUE)
plot(roc_summary[[2]], add = TRUE, colorize = FALSE)
plot(roc_summary[[3]], add = TRUE, colorize = TRUE)
plot(roc_summary[[4]], add = TRUE, colorize = FALSE)
plot(roc_summary[[5]], add = TRUE, colorize = TRUE)
plot(roc_summary[[6]], add = TRUE, colorize = FALSE)
plot(roc_summary[[7]], add = TRUE, colorize = TRUE)
plot(roc_summary[[8]], add = TRUE, colorize = FALSE)
plot(roc_summary[[9]], add = TRUE, colorize = TRUE)

# Question 3
training <- credit_data[inTrain,]
testing <- credit_data[-inTrain,]

testing_men <- testing[which(testing$SEX == "1"),] # for Q3
testing_women <- testing[which(testing$SEX == "2"),] # for Q3

#Logistic Modeling without Sex variables
model_logistic_1 <- glm(default_0 ~ LIMIT_BAL + EDUCATION + MARRIAGE + AGE_BIN + AGE_ED +
                          PAY_1 + PAY_2 + PAY_3 + PAY_4 + PAY_5 + PAY_6 +
                          BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 +
                          PAY_AMT1 + PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6 +
                          END_BAL1 + END_BAL2 + END_BAL3 + END_BAL4 + END_BAL5 + END_BAL6 +
                          UTILI_1 + UTILI_2 + UTILI_3 + UTILI_4 + UTILI_5 + UTILI_6 +
                          BAL_PERC1 + BAL_PERC2 + BAL_PERC3 + BAL_PERC4 + BAL_PERC5 + BAL_PERC6 +
                          SPENT_PERC1 + SPENT_PERC2 + SPENT_PERC3 + SPENT_PERC4 + SPENT_PERC5 +
                          AVGUTILI_16 + AVGUTILI_15 + AVGUTILI_14 + AVGUTILI_13 + AVGUTILI_46 + AVGUTILI_13_46 +
                          payratio1 + payratio2 + payratio3 + payratio4 + payratio5 + payratio6 +
                          delay_1 + delay_2 + delay_3 + delay_4 + delay_5 + delay_6 + delay_ALL +
                          Overlimit1 + Overlimit2 + Overlimit3 + Overlimit4 + Overlimit5 + Overlimit6 + Overlimit_FLAG,
                        data = training, family="binomial"(link="logit"))
#summary(model_logistic_1)

# stepAIC without "Sex" variable
model_logistic_stepwiseAIC_1 <- stepAIC(model_logistic_1, direction = c("both"), trace = 1)
# stepAIC with "Sex" variable
model_logistic_stepwiseAIC

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC_1) #Error plots: similar nature to lm plots
plot(model_logistic_stepwiseAIC)
par(mfrow=c(1,1))

###
# Approval Rate CUrve by different threshold
###
prob_threshold <- nrow(filter(credit_data, default_0 == "1"))/nrow(credit_data)
threshold_list <- c(prob_threshold - 0.1, prob_threshold - 0.075, prob_threshold - 0.05, prob_threshold - 0.025, prob_threshold, prob_threshold + 0.025, prob_threshold + 0.05, prob_threshold + 0.075, prob_threshold + 0.1)

credit_approval_nosex <- data.frame("Type" = c("Both_nosex", "Men_nosex", "Women_nosex"))
credit_approval_sex <- data.frame("Type" = c("Both_sex", "Men_sex", "Women_sex"))

for (i in 1:length(threshold_list)) {
  # logistic model without "sex" variables
  logistic_probabilities_1<-predict(model_logistic_stepwiseAIC_1, newdata=testing, type="response") #Predict probabilities
  logistic_classification_1<-rep("1",4800)
  logistic_classification_1[logistic_probabilities_1 < threshold_list[i]]="0"
  logistic_classification_1<-as.factor(logistic_classification_1)
  logistic_confusionmatrix_1 <- confusionMatrix(logistic_classification_1, testing$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_1 <- prediction(logistic_probabilities_1, testing$default_0)
  logistic_ROC_1 <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_1) ##plot ROC curve
  auc.tmp_1 <- performance(logistic_ROC_prediction,"auc") #Create AUC data
  logistic_auc_1 <- as.numeric(auc.tmp@y.values) #Calculate AUC
  c(logistic_auc_1,logistic_auc_men_1,logistic_auc_women_1) #Display AUC value: 90+% - excellent, 80-90% - very good, 70-80% - good, 60-70% - so so, below 60% - not much value
  #plotLift(logistic_probabilities_1, testing$default_0, cumulative = TRUE, n.buckets = 10) # #plot Lift chart
  logistic_profit_1 <- profit_func(logistic_confusionmatrix_1)
  credit_decision_1 <- data.frame(logistic_confusionmatrix_1[2])[,3]
  credit_granted_percentage_1 <- (credit_decision_1[1] + credit_decision_1[3]) / sum(credit_decision_1)
  #
  logistic_probabilities_men_1 <- predict(model_logistic_stepwiseAIC_1, newdata=testing_men, type="response")
  logistic_classification_men_1<-rep("1",1935)
  logistic_classification_men_1[logistic_probabilities_men_1 < threshold_list[i]]="0"
  logistic_classification_men_1<-as.factor(logistic_classification_men_1)
  logistic_confusionmatrix_men_1 <- confusionMatrix(logistic_classification_men_1, testing_men$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_men_1 <- prediction(logistic_probabilities_men_1, testing_men$default_0)
  logistic_ROC_men_1 <- performance(logistic_ROC_prediction_men,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_men_1) ##plot ROC curve
  auc.tmp_men_1 <- performance(logistic_ROC_prediction_men,"auc") #Create AUC data
  logistic_auc_men_1 <- as.numeric(auc.tmp_men@y.values) #Calculate AUC
  #plotLift(logistic_probabilities_men_1, testing_men$default_0, cumulative = TRUE, n.buckets = 10) # #plot Lift chart
  logistic_profit_men_1 <- profit_func(logistic_confusionmatrix_men_1)
  credit_decision_men_1 <- data.frame(logistic_confusionmatrix_men_1[2])[,3]
  credit_granted_percentage_men_1 <- (credit_decision_men_1[1] + credit_decision_men_1[3]) / sum(credit_decision_men_1)
  #
  logistic_probabilities_women_1 <- predict(model_logistic_stepwiseAIC_1, newdata=testing_women, type="response")
  logistic_classification_women_1<-rep("1",4800-1935)
  logistic_classification_women_1[logistic_probabilities_women_1 < threshold_list[i]]="0"
  logistic_classification_women_1<-as.factor(logistic_classification_women_1)
  logistic_confusionmatrix_women_1 <- confusionMatrix(logistic_classification_women_1, testing_women$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_women_1 <- prediction(logistic_probabilities_women_1, testing_women$default_0)
  logistic_ROC_women_1 <- performance(logistic_ROC_prediction_women,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_women_1) ##plot ROC curve
  auc.tmp_women_1 <- performance(logistic_ROC_prediction_women,"auc") #Create AUC data
  logistic_auc_women_1 <- as.numeric(auc.tmp_women@y.values) #Calculate AUC
  #plotLift(logistic_probabilities_women_1, testing_women$default_0, cumulative = TRUE, n.buckets = 10) # #plot Lift chart
  logistic_profit_women_1 <- profit_func(logistic_confusionmatrix_women_1)
  credit_decision_women_1 <- data.frame(logistic_confusionmatrix_women_1[2])[,3]
  credit_granted_percentage_women_1 <- (credit_decision_women_1[1] + credit_decision_women_1[3]) / sum(credit_decision_women_1)
  #
  credit_appr_nosex <- data.frame("Credit Approval %" = c(credit_granted_percentage_1, credit_granted_percentage_men_1, credit_granted_percentage_women_1))
  names(credit_appr_nosex) <- paste("Appr % @", round(threshold_list[i], 3))
  credit_approval_nosex <- cbind(credit_approval_nosex, credit_appr_nosex)
  #credit_approval_nosex
  
  logistic_probabilities_v<-predict(model_logistic_stepwiseAIC, newdata=testing, type="response") #Predict probabilities
  logistic_classification_v<-rep("1",4800)
  logistic_classification_v[logistic_probabilities_v < threshold_list[i]]="0"
  logistic_classification_v<-as.factor(logistic_classification_v)
  logistic_confusionmatrix_v <- confusionMatrix(logistic_classification_v,testing$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_v <- prediction(logistic_probabilities_v, testing$default_0)
  logistic_ROC_v <- performance(logistic_ROC_prediction_v,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_v) #Plot ROC curve
  auc.tmp_v <- performance(logistic_ROC_prediction_v,"auc") #Create AUC data
  logistic_auc_v <- as.numeric(auc.tmp_v@y.values) #Calculate AUC
  #plotLift(logistic_probabilities_v, testing$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  logistic_profit_v <- profit_func(logistic_confusionmatrix_v)
  credit_decision_v <- data.frame(logistic_confusionmatrix_v[2])[,3]
  credit_granted_percentage_v <- (credit_decision_v[1] + credit_decision_v[3]) / sum(credit_decision_v)
  #
  logistic_probabilities_men_v <- predict(model_logistic_stepwiseAIC, newdata=testing_men, type="response")
  logistic_classification_men_v<-rep("1",1935)
  logistic_classification_men_v[logistic_probabilities_men_v < threshold_list[i]]="0"
  logistic_classification_men_v<-as.factor(logistic_classification_men_v)
  logistic_confusionmatrix_men_v <- confusionMatrix(logistic_classification_men_v,testing_men$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_men_v <- prediction(logistic_probabilities_men_v, testing_men$default_0)
  logistic_ROC_men_v <- performance(logistic_ROC_prediction_men_v,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_men_v) #Plot ROC curve
  auc.tmp_men_v <- performance(logistic_ROC_prediction_men_v,"auc") #Create AUC data
  logistic_auc_men_v <- as.numeric(auc.tmp_men_v@y.values) #Calculate AUC
  #plotLift(logistic_probabilities_men_v, testing_men$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  logistic_profit_men_v <- profit_func(logistic_confusionmatrix_men_v)
  credit_decision_men_v <- data.frame(logistic_confusionmatrix_men_v[2])[,3]
  credit_granted_percentage_men_v <- (credit_decision_men_v[1] + credit_decision_men_v[3]) / sum(credit_decision_men_v)
  #
  logistic_probabilities_women_v <- predict(model_logistic_stepwiseAIC, newdata=testing_women, type="response")
  logistic_classification_women_v<-rep("1",4800-1935)
  logistic_classification_women_v[logistic_probabilities_women_v < threshold_list[i]]="0"
  logistic_classification_women_v<-as.factor(logistic_classification_women_v)
  logistic_confusionmatrix_women_v <- confusionMatrix(logistic_classification_women_v,testing_women$default_0, positive = "1") #Display confusion matrix
  logistic_ROC_prediction_women_v <- prediction(logistic_probabilities_women_v, testing_women$default_0)
  logistic_ROC_women_v <- performance(logistic_ROC_prediction_women_v,"tpr","fpr") #Create ROC curve data
  #plot(logistic_ROC_women_v) #Plot ROC curve
  auc.tmp_women_v <- performance(logistic_ROC_prediction_women_v,"auc") #Create AUC data
  logistic_auc_women_v <- as.numeric(auc.tmp_women_v@y.values) #Calculate AUC
  #plotLift(logistic_probabilities_women_v, testing_women$default_0, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
  logistic_profit_women_v <- profit_func(logistic_confusionmatrix_women_v)
  credit_decision_women_v <- data.frame(logistic_confusionmatrix_women_v[2])[,3]
  credit_granted_percentage_women_v <- (credit_decision_women_v[1] + credit_decision_women_v[3]) / sum(credit_decision_women_v)
  #
  credit_appr_sex <- data.frame("Credit Approval %" = c(credit_granted_percentage_v, credit_granted_percentage_men_v, credit_granted_percentage_women_v))
  names(credit_appr_sex) <- paste("Appr % @", round(threshold_list[i], 3))
  credit_approval_sex <- cbind(credit_approval_sex, credit_appr_sex)
  #credit_approval_sex
}

library(ggplot2)
plot(threshold_list, c(as.numeric(credit_approval_nosex[1,-1])), type="o", col="blue", pch="o", lty=1, ylim=c(0.2,0.9), ylab="Approval Rate w.o. Sex")
points(threshold_list, c(as.numeric(credit_approval_nosex[2,-1])), type="o", col="red", pch="*", lty=2, ylim=c(0.2,0.9))
points(threshold_list, c(as.numeric(credit_approval_nosex[3,-1])), type="o", col="black", pch="+", lty=3, ylim=c(0.2,0.9))
legend(0.29, 0.35, legend=c("Both", "Men", "Women"), col=c("blue", "red", "black"), pch = c("o", "*", "+"), lty = c(1,2,3), ncol = 1)

plot(threshold_list, c(as.numeric(credit_approval_sex[1,-1])), type="o", col="blue", pch="o", lty=1, ylim=c(0.2,0.9), ylab="Approval Rate w. Sex")
points(threshold_list, c(as.numeric(credit_approval_sex[2,-1])), type="o", col="red", pch="*", lty=2, ylim=c(0.2,0.9))
points(threshold_list, c(as.numeric(credit_approval_sex[3,-1])), type="o", col="black", pch="+", lty=3, ylim=c(0.2,0.9))
legend(0.29, 0.35, legend=c("Both", "Men", "Women"), col=c("blue", "red", "black"), pch = c("o", "*", "+"), lty = c(1,2,3), ncol = 1)

credit_approval_sex
credit_approval_nosex

write.xlsx(rbind(credit_approval_sex, credit_approval_nosex), "approval rate by thresholds.xlsx")

