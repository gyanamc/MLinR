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
data = read.csv('GA Converted 3.csv')
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

datanzv[,c(1:4)] = lapply(datanzv[,c(1:4)], function(x) as.factor(as.character(x)))
datanzv[,c(5:21)] = lapply(datanzv[,c(5:21)], function(x) as.numeric(as.integer(x)))
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
str(datanzv)


#Binding Avg..Order.Value variable
Avg..Order.Value = data$Avg..Order.Value
datanzv = cbind(datanzv, Avg..Order.Value)
datanzv$Avg..Order.Value<-as.numeric(datanzv$Avg..Order.Value)
str(datanzv)

# Removing Transactions
datanzv<-datanzv[,-c(10)]
str(datanzv)

# Feature selection using Boruta
set.seed(123)
boruta = Boruta(Revenue~., data = datanzv, doTrace = 2)
print(boruta)
# 1 attribute confirmed unimportant: Organic.Searches
# 3 tentative attributes left: Unique.Views.1.Blogs, User.Type, X..New.Sessions
plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

# Tentative Fix 
bor = TentativeRoughFix(boruta)
print(bor)
# 1 attributes confirmed unimportant: Organic.Searches
attStats(boruta)
getNonRejectedFormula(boruta)
'Revenue ~ Source + Medium + Browser + User.Type + Goal.Completions + 
    Goal.Conversion.Rate + Avg....Conversion.Probability + X..New.Sessions + 
    Pages...Session + Avg..Session.Duration + Ecommerce.Conversion.Rate + 
    Unique.Views.1.Blogs + Unique.Views.2.Product.Pages + Avg..Order.Value'

# Remove unimportant variables - Organic Search
datanzv<-datanzv[,-c(9)]
str(datanzv)

datann = datanzv #neural network
data1 = datann 

# Splitting into training and test set
set.seed(222)
split = sample.split(datanzv, SplitRatio=0.8)
train = subset(datanzv, split==TRUE)
test = subset(datanzv, split==FALSE)

#-----------------Build Linear Regression Model-----------------#
set.seed(123)
regressor_lr = lm(formula = Revenue ~ .,data = train)
summary(regressor_lr)

levels(test$Browser) = levels(train$Browser)
Rev_predict_lr = predict(regressor_lr, newdata=test)
'Error in model.frame.default(Terms, newdata, na.action = na.action, xlev = object$xlevels) : 
factor Browser has new levels Others'
rmse_lr = rmse(test$Revenue, Rev_predict_lr)
rmse_lr
# 188906.8

#-----------------Build Random Forest Model-----------------#
set.seed(123)
regressor_rf = randomForest(Revenue ~., data = train)
print(regressor_rf)
# ntree = 500, mtry = 4

Rev_predict_rf = predict(regressor_rf, newdata=test)

rmse_rf = rmse(test$Revenue, Rev_predict_rf)
rmse_rf
# rmse = 131298.8


#-----------------Build Support Vector Machine Model-----------------#
set.seed(123)
regressor_svm = svm(formula = scale(Revenue) ~ Source + Medium + Browser + User.Type + scale(Goal.Completions) + 
                      scale(Goal.Conversion.Rate) + scale(Avg....Conversion.Probability) + scale(X..New.Sessions) + 
                      scale(Pages...Session) + scale(Avg..Session.Duration) + 
                      scale(Ecommerce.Conversion.Rate) + scale(Unique.Views.1.Blogs) + scale(Unique.Views.2.Product.Pages) + 
                      scale(Avg..Order.Value), data = train)

summary(regressor_svm)

Rev_predict_svm = predict(regressor_svm, newdata=test) * sd(test$Revenue) + mean(test$Revenue)

rmse_svm = rmse(test$Revenue, Rev_predict_svm)
rmse_svm
# rmse = 169991.8


#-----------------Build Neural Network Model-----------------#
train_nn = train
test_nn = test

str(train_nn)
train_nn[,c(1:4)] = lapply(train_nn[,c(1:4)], function(x) as.numeric(as.factor(x)))
str(test_nn)
test_nn[,c(1:4)] = lapply(test_nn[,c(1:4)], function(x) as.numeric(as.factor(x)))

train_nn[, 1:16] = scale(train_nn[, 1:16])
test_nn[, 1:16] = scale(test_nn[, 1:16])

library(h2o)
h2o.init(nthreads = -1)
regressor_nn = h2o.deeplearning(y =  "Revenue",
                                training_frame = as.h2o(train_nn),
                                activation = "Rectifier",
                                hidden = c(8), epochs = 100,
                                train_samples_per_iteration = -2,
                                seed = 123,
                                reproducible = TRUE)
print(regressor_nn)
plot(regressor_nn)

Rev_predict_nn = h2o.predict(regressor_nn, newdata=as.h2o(test_nn))
Rev_predict_nn = as.vector(Rev_predict_nn)
#MEDV_predict_nn = as.factor(MEDV_predict_nn)
h2o.shutdown()
Y

rmse_nn = rmse(test_nn$Revenue, Rev_predict_nn)
rmse_nn
# 32bit Java error

#-----------------------------------------------------------#
# Tuning the Random Forest model by varying values of hyperparameters
set.seed(123)
data_rft <- sample_n(train,1811)%>% as_tibble() 

# define task and learner ----------------------
defTaskrf <- makeRegrTask(data = data_rft, target = "Revenue")
rf <- makeLearner("regr.randomForest", predict.type = "response")

# define hyperparameter search space ----------------------
getParamSet("regr.randomForest")
paramSpacerf <- makeParamSet( makeIntegerParam("mtry", lower = 1, upper = 14),
                              makeIntegerParam("ntree", lower = 250, upper = 2000),
                              makeIntegerParam("nodesize", lower = 4, upper = 40))

# Define search procedure ----------------------
randSearchrf <- makeTuneControlIrace(maxExperiments = 100)

# Define nested cross-validation ----------------------
innerrf <- makeResampleDesc("Holdout",split = 0.8)
outerrf <- makeResampleDesc("CV",iters = 3)

# Define wrapper for learner and tuning ---------------
rfWrapper <- makeTuneWrapper(learner = rf, resampling = innerrf, 
                             par.set = paramSpacerf, control = randSearchrf)

# Run Cross Validation --------------------------------
set.seed(123)
cvWtihTuningrf <- resample(learner = rfWrapper, task = defTaskrf, 
                           resampling = outerrf, models = T)
cvWtihTuningrf$measures.test

# Train model using all data --------------------------
set.seed(123)
tunedParsrf <-tuneParams(learner = rf,
                         task = defTaskrf,
                         resampling = innerrf,
                         par.set = paramSpacerf,
                         control = randSearchrf)
tunedParsrf
# Op. pars: mtry=13; ntree=1078; nodesize=17 mse.test.mean=14877343312.8223019

# Tuned model with hyperparameters and RMSE calculation
set.seed(123)
regressor_rft = randomForest(Revenue ~., data = train, mtry = 13, 
                              ntree = 1078, nodesize = 17)
print(regressor_rft)

Rev_predict_rft = predict(regressor_rft, newdata=test)

rmse_rft = rmse(test$Revenue, Rev_predict_rft)
rmse_rft
# rmse 128416.4

#variable importance
#varImpPlot(regressor_rft)
varImpPlot(regressor_rft,
           sort = T,
           main = "Variable Importance")
importance(regressor_rft)
varUsed(regressor_rft)

# Tuning the SVR model by varying values of hyperparameters
set.seed(123)
data_svmt <- sample_n(train,1811)%>% as_tibble() 

# define task and learner ----------------------
defTasksvm <- makeRegrTask(data = data_svmt, target = "Revenue")
svm <- makeLearner("regr.svm", predict.type = "response")

# define hyperparameter search space ----------------------
getParamSet("regr.svm")
kernels <- c("polynomial","radial","sigmoid")
paramSpacesvm <- makeParamSet(makeDiscreteParam("kernel",values = kernels), 
                              makeIntegerParam("degree", lower = 1, upper = 4),
                              makeNumericParam("cost", lower = 1, upper = 10), 
                              makeNumericParam("epsilon", lower = 0, upper = 1))

# Define search procedure ----------------------
randSearchsvm <- makeTuneControlIrace(maxExperiments = 180)

# Define nested cross-validation ----------------------
innersvm <- makeResampleDesc("Holdout",split = 0.8)
outersvm <- makeResampleDesc("CV",iters = 3)

# Define wrapper for learner and tuning ---------------
svmWrapper <- makeTuneWrapper(learner = svm, resampling = innersvm, 
                              par.set = paramSpacesvm, control = randSearchsvm)

# Run Cross Validation --------------------------------
set.seed(123)
cvWtihTuningsvm <- resample(learner = svmWrapper, task = defTasksvm, 
                            resampling = outersvm, models = TRUE)

cvWtihTuningsvm$measures.test

# Train model using all data --------------------------
set.seed(123)
tunedParssvm <-tuneParams(learner = svm,
                          task = defTasksvm,
                          resampling = innersvm,
                          par.set = paramSpacesvm,
                          control = randSearchsvm)
tunedParssvm
# Op. pars: kernel=polynomial; degree=1; cost=5.14; epsilon=0.0876 mse.test.mean=19445574003.3577461

# Tuned SVM Model
set.seed(123)
regressor_svmt = svm(formula = scale(Revenue) ~ Source + Medium + Browser + User.Type + scale(Goal.Completions) + 
                      scale(Goal.Conversion.Rate) + scale(Avg....Conversion.Probability) + scale(X..New.Sessions) + 
                      scale(Pages...Session) + scale(Avg..Session.Duration) + 
                      scale(Ecommerce.Conversion.Rate) + scale(Unique.Views.1.Blogs) + scale(Unique.Views.2.Product.Pages) + 
                      scale(Avg..Order.Value), data = train, 
                    kernel= "polynomial", degree=1, cost=5.14, epsilon=0.0876)

summary(regressor_svmt)

Rev_predict_svmt = predict(regressor_svmt, newdata=test) * sd(test$Revenue) + mean(test$Revenue)

rmse_svmt = rmse(test$Revenue, Rev_predict_svmt)
rmse_svmt
# rmse = 188268.8

# SVM Variable Importance
library(rminer)
svmmodel=fit(Revenue~.,train,model="svm")
svm.imp = Importance(svmmodel, data = train)
plot(svm.imp$imp)
str(train)
# Avg..Order.Value, Goal.Completions, Avg....Conversion.Probability, Pages...Session, Unique.Views.1.Blogs  are top variables
#-------------------Summary-------------------#
# Without TR RMSE
# Linear Reg. - 188906.8
# RF - 131298.8
# RF Tuned - 128416.4
# SVM - 169991.8
# SVM Tuned - 188268.8