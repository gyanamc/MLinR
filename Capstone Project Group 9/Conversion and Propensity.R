library(mlbench)
library(caret)
library(randomForest)
library(dplyr)
library(VIM)
library(corrplot)
library(caTools)
library(ROCR)
library(Metrics)
library(mice)
library(ROSE)
library(Boruta)
library(caTools)
library(e1071)
library(kernlab)
library(mlr)
library(UBL)
library(magrittr)

# Data loading
data = read.csv('GA Full Data Final.csv')
#str(dataset)
sum(is.na(data))
# No missing values
#data %>% count(Browser)

# Remove Zero and Near Zero-Variance Predictors
nzv <- nearZeroVar(data)
datanzv <- data[, -nzv]

# Removing client ID
datanzv<-datanzv[,-c(1)]
str(datanzv)

datanzv[,c(1:5)] = lapply(datanzv[,c(1:5)], function(x) as.factor(as.character(x)))
datanzv[,c(6:16)] = lapply(datanzv[,c(6:16)], function(x) as.numeric(as.integer(x)))
str(datanzv)

# Remove highly correlated  Predictors
# Identifying numeric variables
numericData <- datanzv[sapply(datanzv, is.numeric)]
# Calculate correlation matrix
descrCor <- cor(numericData)
print(descrCor)
summary(descrCor[upper.tri(descrCor)])
# Check Correlation Plot
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.4, tl.col = rgb(0, 0, 0))
# find attributes that are highly corrected
highlyCorrelated <- findCorrelation(descrCor, cutoff=0.8)
print(highlyCorrelated)
highlyCorCol <- colnames(numericData)[highlyCorrelated]
highlyCorCol
datanzv <- datanzv[, -which(colnames(datanzv) %in% highlyCorCol)]

#Binding output variable
Revenue = data$Revenue
datanzv = cbind(datanzv, Revenue)
datanzv$Revenue<-as.factor(datanzv$Revenue)
str(datanzv)

#datanzv[,c(1:4)] = lapply(datanzv[,c(1:4)], function(x) as.factor(as.character(x)))
#datanzv[,c(5:13)] = lapply(datanzv[,c(5:13)], function(x) as.numeric(as.character(x)))
#str(datanzv)

# Feature selection using Boruta
set.seed(123)
boruta = Boruta(Revenue~., data = datanzv, doTrace = 2)
print(boruta)
# confirmed 12 attributes
# rejected 1 attribute: Device.Category;
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

#Tentative Fix 
#bor = TentativeRoughFix(boruta) - no Tentative attributes
#print(bor)
attStats(boruta)
getNonRejectedFormula(boruta)
'Revenue ~ Source + Medium + Browser + User.Type + Goal.Conversion.Rate + 
    Avg....Conversion.Probability + X..New.Sessions + Organic.Searches + 
    Bounce.Rate + Pages...Session + Unique.Views.1.Blogs + Unique.Views.3.Primary.Pages'

# Removing variables rejected by Boruta - Device.Category
datanzv = datanzv[,-c(4)]
str(datanzv)
datann = datanzv 
data1 = datann # backup
#datanzv = datann

#datanzv[,c(1:5)] = lapply(datanzv[,c(1:5)], function(x) as.factor(as.character(x)))
#datanzv[,c(6:14)] = lapply(datanzv[,c(6:14)], function(x) as.numeric(as.integer(x)))
#str(datanzv)

# Removing 'Avg....Conversion.Probability' given by Google Analytics
datanzv = datanzv[,-c(6)]
str(datanzv)
#Scaling the data
datanzv[, 5:11] = scale(datanzv[, 5:11])

# Splitting into training and test set
set.seed(123)
split = sample.split(datanzv$Revenue, SplitRatio=0.8)
train = subset(datanzv, split==TRUE)
test = subset(datanzv, split==FALSE)
print(table(train$Revenue))

# ROSE for imbalanced data
train_over <- ovun.sample(Revenue ~ ., data = train, method = "over", N=320780)$data
train_under <- ovun.sample(Revenue ~ ., data = train, method = "under", N = 3794)$data
train_both <- ovun.sample(Revenue ~ ., data = train, method = "both")$data
# SMOTE for imbalanced data
train_sm <- SmoteClassif(Revenue ~., train, C.perc = "balance", dist = "HEOM")
table(train_sm$Revenue)

#---------------------------Building Random Forest Model----------------------------------#
set.seed(123)
memory.limit(56000)
classifier_rfo = randomForest(Revenue~., data = train_over)
print(classifier_rfo)

set.seed(123)
classifier_rfu = randomForest(Revenue~., data = train_under)
print(classifier_rfu)

set.seed(123)
memory.limit(56000)
classifier_rfb = randomForest(Revenue~., data = train_both)
print(classifier_rfb)

set.seed(123)
memory.limit(56000)
classifier_rfsm = randomForest(Revenue~., data = train_sm)
print(classifier_rfsm)

pred_Revenue_rfo = predict(classifier_rfo, newdata = test[-12], type = 'class')
pred_Revenue_rfu = predict(classifier_rfu, newdata = test[-12], type = 'class')
pred_Revenue_rfb = predict(classifier_rfb, newdata = test[-12], type = 'class')
pred_Revenue_rfsm = predict(classifier_rfsm, newdata = test[-12], type = 'class')

confusionMatrix(pred_Revenue_rfo, test[, 12])
# Accuracy :  0.8711
confusionMatrix(pred_Revenue_rfu, test[, 12])
# Accuracy : 0.8497  
confusionMatrix(pred_Revenue_rfb, test[, 12])
# Accuracy : 0.8731
confusionMatrix(pred_Revenue_rfsm, test[, 12])
# Accuracy : 0.9154 

roc.curve(test$Revenue, pred_Revenue_rfo)
# Area under the curve (AUC): 0.851

roc.curve(test$Revenue, pred_Revenue_rfu)
# Area under the curve (AUC): 0.872

roc.curve(test$Revenue, pred_Revenue_rfb)
# Area under the curve (AUC):  0.863

roc.curve(test$Revenue, pred_Revenue_rfsm)
# Area under the curve (AUC): 0.853

#---------------------------Building SVM Model----------------------------------#
set.seed(123)
classifier_svmo = svm(formula = Revenue ~ .,data = train_over, 
                      type = 'C-classification', kernel = 'radial',probability= TRUE)
summary(classifier_svmo)
# No Output

set.seed(123)
classifier_svmu = svm(formula = Revenue ~ .,data = train_under, 
                      type = 'C-classification', kernel = 'radial',probability= TRUE)
summary(classifier_svmu)

set.seed(123)
memory.limit(56000)
classifier_svmb = svm(formula = Revenue ~ .,data = train_both, 
                      type = 'C-classification', kernel = 'radial',probability= TRUE)
summary(classifier_svmb)
# no output

set.seed(123)
memory.limit(56000)
classifier_svmsm = svm(formula = Revenue ~ .,data = train_sm, 
                      type = 'C-classification', kernel = 'radial',probability= TRUE)
summary(classifier_svmsm)
# No Output

#pred_Revenue_svmo = predict(classifier_svmo, type = 'response', newdata=test[-12])
pred_Revenue_svmu = predict(classifier_svmu, type = 'response', newdata=test[-12])
#pred_Revenue_svmb = predict(classifier_svmb, type = 'response', newdata=test[-12])
#pred_Revenue_svmsm = predict(classifier_svmsm, type = 'response', newdata=test[-12])


confusionMatrix(pred_Revenue_svmo, test[, 12])
# Accuracy : no output
confusionMatrix(pred_Revenue_svmu, test[, 12])
# Accuracy : 0.7622   
confusionMatrix(pred_Revenue_svmb, test[, 12])
# Accuracy : no output
confusionMatrix(pred_Revenue_svmsm, test[, 12])
# Accuracy : no output


roc.curve(test$Revenue, pred_Revenue_svmo)
# Area under the curve (AUC): no output

roc.curve(test$Revenue, pred_Revenue_svmu)
# Area under the curve (AUC): 0.839

roc.curve(test$Revenue, pred_Revenue_svmb)
# Area under the curve (AUC): no output

roc.curve(test$Revenue, pred_Revenue_svmsm)
# Area under the curve (AUC): no output

#---------------------------Building Logistic Regression Model----------------------------------#
set.seed(123)
classifier_lo = glm(formula = Revenue ~ ., family = binomial, data = train_over)
summary(classifier_lo)

set.seed(123)
classifier_lu = glm(formula = Revenue ~ ., family = binomial, data = train_under)
summary(classifier_lu)

set.seed(123)
classifier_lb = glm(formula = Revenue ~ ., family = binomial, data = train_both)
summary(classifier_lb)

set.seed(123)
classifier_lsm = glm(formula = Revenue ~ ., family = binomial, data = train_sm)
summary(classifier_lsm)

prob_Revenue_lo = predict(classifier_lo, type = "response", newdata = test[-12])
pred_Revenue_lo = ifelse(prob_Revenue_lo>0.5,1,0)
pred_Revenue_lo = factor(pred_Revenue_lo)

prob_Revenue_lu = predict(classifier_lu, type = "response", newdata = test[-12])
pred_Revenue_lu = ifelse(prob_Revenue_lu>0.5,1,0)
pred_Revenue_lu = factor(pred_Revenue_lu)

prob_Revenue_lb = predict(classifier_lb, type = "response", newdata = test[-12])
pred_Revenue_lb = ifelse(prob_Revenue_lb>0.5,1,0)
pred_Revenue_lb = factor(pred_Revenue_lb)

prob_Revenue_lsm = predict(classifier_lsm, type = "response", newdata = test[-12])
pred_Revenue_lsm = ifelse(prob_Revenue_lsm>0.5,1,0)
pred_Revenue_lsm = factor(pred_Revenue_lsm)

confusionMatrix(pred_Revenue_lo, test[,12])
# Accuracy : 0.8137  
confusionMatrix(pred_Revenue_lu, test[,12])
# Accuracy : 0.8147 
confusionMatrix(pred_Revenue_lb, test[,12])
# Accuracy : 0.8129 
confusionMatrix(pred_Revenue_lsm, test[,12])
# Accuracy : 0.8165  

roc.curve(test$Revenue, pred_Revenue_lo)
# Area under the curve (AUC): 0.842

roc.curve(test$Revenue, pred_Revenue_lu)
# Area under the curve (AUC): 0.837

roc.curve(test$Revenue, pred_Revenue_lb)
# Area under the curve (AUC): 0.841

roc.curve(test$Revenue, pred_Revenue_lsm)
# Area under the curve (AUC): 0.842

#------------------------------------------------------------------------#
# Tuning Random Forest Model 
set.seed(123)
# using under sampled data
data_rft <- sample_n(train_under,3794, replace = T)%>% as_tibble() 

# using SMOTE data
#data_rft <- sample_n(train_sm,162288, replace = T)%>% as_tibble() 

defTaskrf <- makeClassifTask(data = data_rft, target = "Revenue")
rf <- makeLearner("classif.randomForest", predict.type = "prob")

# define hyperparameter search space ------------------
getParamSet("classif.randomForest")
paramSpacerf <- makeParamSet( makeIntegerParam("mtry", lower = 2, upper = 12),
                              makeIntegerParam("ntree", lower = 150, upper = 2000))

# Define search procedure ----------------------
randSearchrf <- makeTuneControlIrace(maxExperiments = 100)

# Define nested cross-validation ----------------------
innerrf <- makeResampleDesc("Holdout",split = 0.8)
outerrf <- makeResampleDesc("CV",iters = 3)

# Define wrapper for learner and tuning ---------------
rfWrapper <- makeTuneWrapper(learner = rf, resampling = innerrf, 
                             par.set = paramSpacerf, control = randSearchrf)

# Run Cross Validation --------------------------------
memory.limit(56000)
cvWtihTuningrf <- resample(learner = rfWrapper, task = defTaskrf, 
                           resampling = outerrf, models = TRUE)

cvWtihTuningrf$measures.test

# Train model using all data --------------------------
memory.limit(56000)
tunedParsrf <-tuneParams(learner = rf,
                         task = defTaskrf,
                         resampling = innerrf,
                         par.set = paramSpacerf,
                         control = randSearchrf)
tunedParsrf
# Under Sampled
# Op. pars: mtry=6; ntree=619 mmce.test.mean=0.1014493 Random
# Op. pars: mtry=6; ntree=1554 mmce.test.mean=0.1051091 Irace

# Tuned Random Forest Model with under sampled data
set.seed(123)
classifier_rfut = randomForest(Revenue~., data = train_under, mtry = 6,ntree = 1554)
print(classifier_rfut)

pred_Revenue_rfut = predict(classifier_rfut, newdata = test[-12], type = 'class')

confusionMatrix(pred_Revenue_rfut, test[, 12])
# Accuracy : 0.8639 Random / 0.861  Irace

roc.curve(test$Revenue, pred_Revenue_rfut)
# Area under the curve (AUC): 0.873 Random / 0.872 Irace

#variable importance - Tuned model with under sample data
varImpPlot(classifier_rfut,
           sort = T,
           main = "Variable Importance RF")
importance(classifier_rfut)
varUsed(classifier_rfut)

#variable importance - Non Tuned model with under sample data
varImpPlot(classifier_rfu,
           sort = T,
           main = "Variable Importance RF")
importance(classifier_rfu)
varUsed(classifier_rfu)

#---------------------------Building XGBoost Model----------------------------------#
library(xgboost)
library(Matrix)

# Under Sampled data
trainm = model.matrix(Revenue~.-Revenue, data = train_under)
head(trainm)
train_label = train_under[,"Revenue"]
train_label = as.numeric(train_label)-1
train_matrix = xgb.DMatrix(data = as.matrix(trainm), label=train_label)

testm = model.matrix(Revenue~.-Revenue, data = test)
head(testm)
test_label = test[,"Revenue"]
test_label = as.numeric(test_label)-1
test_matrix = xgb.DMatrix(data = as.matrix(testm), label=test_label)

#Parameters
nc  = length(unique(train_label))
xgb_params = list("objective" = "binary:hinge", "eval_metric" = "auc")

watchlist = list(train = train_matrix, test = test_matrix)

# XGB Model
set.seed(123)
bst_model = xgb.train(params = xgb_params, data = train_matrix, nrounds = 410, watchlist = watchlist,
                      eta = 0.008, mx.depth =7, gamma = 0.5,
                      subsample = 1, colsample_bytree = 1, missing = NA, set.seed =123)
summary(bst_model)

# Training and test error plot
e = data.frame(bst_model$evaluation_log)
plot(e$iter, e$train_auc, col = 'blue')
lines(e$iter, e$test_auc, col = 'red')
max(e$test_auc)
# AUC = 0.867926 i.e. 0.868 for 500 nrounds without hyperparameters
# AUC = 0.866399 i.e. 0.866 for 500 nrounds with hyperparameters
e[e$test_auc ==0.866399,] 
# AUC = 0.867926 for nround 10, 500 nrounds considered initially without hyperparameters
# AUC = 0.866399 for nround 410, 500 nrounds considered initially with hyperparameters

# Prediction and Confusion matrix
p = predict(bst_model, newdata = test_matrix)
p = as.numeric(p>0.5)
confusionMatrix(factor(p), factor(test$Revenue))
# Accuracy : 0.8317  for bst_model with 10 nrounds without hyperparameters
# Accuracy : 0.806  for bst_model with 410 nrounds with hyperparameters

roc.curve(test$Revenue, p)
#Area under the curve (AUC): 0.868 for bst_model with 10 nrounds without hyperparameters
#Area under the curve (AUC): 0.866 for bst_model with 410 nrounds with hyperparameters

# Variable Importance
xgb.importance(model = bst_model)


# SMOTE data
trainmsm = model.matrix(Revenue~.-Revenue, data = train_sm)
head(trainmsm)
trainsm_label = train_sm[,"Revenue"]
trainsm_label = as.numeric(trainsm_label)-1
trainsm_matrix = xgb.DMatrix(data = as.matrix(trainmsm), label=trainsm_label)

testmsm = model.matrix(Revenue~.-Revenue, data = test)
head(testmsm)
testsm_label = test[,"Revenue"]
testsm_label = as.numeric(testsm_label)-1
testsm_matrix = xgb.DMatrix(data = as.matrix(testmsm), label=testsm_label)

#Parameters
nc  = length(unique(train_label))
xgb_params_sm = list("objective" = "binary:hinge", "eval_metric" = "auc")

watchlistsm = list(train = trainsm_matrix, test = testsm_matrix)

# XGB Model
set.seed(123)
bst_model_sm = xgb.train(params = xgb_params_sm, data = trainsm_matrix, nrounds = 337, watchlist = watchlistsm,
eta = 0.008, mx.depth =7, gamma = 0.5,
subsample = 1, colsample_bytree = 1, missing = NA, set.seed =123)
summary(bst_model_sm)

# Training and test error plot
esm = data.frame(bst_model_sm$evaluation_log)
plot(esm$iter, esm$train_auc, col = 'blue')
lines(esm$iter, esm$test_auc, col = 'red')
max(esm$test_auc)
# AUC = 0.875962 i.e. 0.876 for 500 nrounds without hyperparameters
# AUC = 0.876051 i.e. 0.894 for 500 nrounds without hyperparameters
esm[esm$test_auc == 0.876051,] 
# AUC = 0.875962 for nround 11, 500 nrounds considered initially without hyperparameters
# AUC = 0.876051 for nround 337, 500 nrounds considered initially with hyperparameters

# Prediction and Confusion matrix
psm = predict(bst_model_sm, newdata = testsm_matrix)
psm = as.numeric(psm>0.5)
confusionMatrix(factor(psm), factor(test$Revenue))
# Accuracy : 0.8599 for bst_model_sm with 11 nrounds without hyperparameters
# Accuracy : 0.8457  for bst_model_sm with 337 nrounds with hyperparameters
roc.curve(test$Revenue, psm)
#Area under the curve (AUC): 0.876 for bst_model_sm with 11 nrounds without hyperparameters
#Area under the curve (AUC): 0.876 for bst_model_sm with 337 nrounds with hyperparameters

# Variable Importance
xgb.importance(model = bst_model_sm)


#---------------------------------Propensity Score-------------------------------#
dim(datanzv)
propdata = datanzv
#install.packages("MatchIt")
library(MatchIt) 
greedyMatching <- matchit( Revenue ~.,
                           distance = data$logitPscores,
                           data = propdata,
                           method = "exact",
                           ratio = 1,
                           replace = T,
                           caliper = 0.25
)

(balance.greedyMatching <- summary(greedyMatching, standardize = T))

summary(abs(balance.greedyMatching$sum.matched))
table(abs(balance.greedyMatching$sum.matched))

data.greedyMathing <- match.data(greedyMatching)

write.csv(data.greedyMathing,"fulldatapropensityscore2.csv")
