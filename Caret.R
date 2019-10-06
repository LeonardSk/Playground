install.packages(c('caret', 'skimr', 'RANN', 'randomForest', 'fastAdaboost', 'gbm', 'xgboost', 'caretEnsemble', 'C50', 'earth', 'ellipse'))
library(caret)
library(caTools)
library(dplyr)
library(skimr)
library(caretEnsemble)

orange <- read.csv('https://raw.githubusercontent.com/selva86/datasets/master/orange_juice_withmissing.csv')
summary(orange)
str(orange)

set.seed(1306)
### Train Test set
train.flag <- createDataPartition(orange$Purchase, p=0.8, list = F) # Return a matrix
train <- orange[train.flag,]
test <- orange[-train.flag,]

Y <- train %>% select(Purchase)
X <- train %>% select(-Purchase)

# Detailed Summary
skim_to_wide(train) %>% select(-n,-n_unique,-top_counts,-ordered)

### Impute NAs with KNN
Impute.Model <- preProcess(train, method = "knnImpute")
train.1 <- predict(Impute.Model, train)
# Check new data
anyNA(train.1)

# One hot encoding
# Create dummy variables to convert categorical variables to binary encoding
dummies <- dummyVars(Purchase~., data = train.1)
train.1.OHE <- predict(dummies, train.1)
# Store7 is One hot encoded

train.2 <- as.data.frame(train.1.OHE)

### Feature Pre-processing / Engineering
Range.Model <- preProcess(train.2, method = "range")
train.3 <- predict(Range.Model, train.2)
# Range converts all the numeric vars to be between 0 and 1

train.3$Purchase <- as.factor(Y$Purchase)

### Feature Importance plot
featurePlot(x = train.3%>%select(-Purchase),
            y = train.3$Purchase,
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

featurePlot(x = train.3%>%select(-Purchase),
            y = train.3$Purchase,
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

### Recursive feature Elimination

options(warn = -1) # Base option to remove R warnings

ctrl <- rfeControl(functions = rfFuncs, # fit Random forest models
                   method = "repeatedcv", # Repeated K-Fold cross val 
                   number = 15, # K parm for cross val
                   repeats = 5 # K-fold cross val repeats
)
subsets <- c(1:5, 10, 15, 18) #Max is the ncol of train.3%>%select(-Purchase), the predictors

profile1 <- rfe(train.3%>%select(-Purchase), train.3$Purchase,
                sizes = subsets ,
                rfeControl = ctrl)
# This tells us that the optimal model with 2 Predictors (LocalCH and PriceDiff) provides
# the highest degree of accuracy

### Model building

# All available Caret models
names(getModelInfo())
modelLookup("earth") # MARS model

set.seed(1306)

model.mars <- train(Purchase~., data=train.3, method="earth")
# Shows us the accuracy and Kappa (SD) for each hyperparm (nprune in this case)

# Plot of accuracy with different parms
plot(model.mars)
# PLot of Importance
plot(varImp(model.mars))

### Test data preparation and predictions
# Preprocess the test set
test.Y <- test$Purchase
test.1 <- predict(Impute.Model, test)
test.2 <- predict(dummies, test.1)
test.3 <- predict(Range.Model, test.2)

# Model prediction 
preds <- predict(model.mars, test.3)

### Confusion Matrix
conf <- table(preds, test.Y)
Accuracy <- (120+68) / (120+15+10+68)

confusionMatrix(reference = test.Y, data = preds, mode = "everything", positive = "MM")

### Hyperparameter tuning
# Lets define more parms for a second model
fit.Ctrl <- trainControl(
  method = 'cv',
  number = 10, # K-fold
  savePredictions = 'final',
  classProbs = T, # Predict probabilities, otherwise subject to class output with 0.5 cutoff
  summaryFunction = twoClassSummary
)

model.mars.2 <- train(Purchase~., data = train.3, method='earth',
                      tuneLength = 10, # automated tuning, just specify the number of times parms are tuned
                      metric = 'ROC', # default for class predictions is accuracy
                      trControl = fit.Ctrl)

preds.2 <- predict(model.mars.2, test.3)
confusionMatrix(reference = test.Y, data = preds.2, mode = "everything", positive = "MM")

# Grid search, instead of auto tune length
Grid1 <- expand.grid(nprune = seq(2,10,2),
                     degree = 1:3)

model.mars.3 <- train(Purchase~., data = train.3, method='earth',
                      tuneGrid = Grid1, # Specified tuning
                      metric = 'ROC', # default for class predictions is accuracy
                      trControl = fit.Ctrl)
preds.3 <- predict(model.mars.3, test.3)
confusionMatrix(test.Y, preds.3, mode = 'everything', positive = 'MM')

# The great thing about the train() in caret compared to other ML base models, is that it automatically
# picks the best parms (based on the tuning specified) and builds the model on that
# Hence tuning and K-fold val is all done in one stop!

### Multiple Model Evaluation
# AdaBoost
model.ada <- train(Purchase~., data = train.3, method = 'adaboost',
                   tuneLength = 3,
                   trControl = fit.Ctrl)
# Random Forest
model.RF <- train(Purchase~., data = train.3, method = 'rf',
                  tuneLength = 5,
                  trControl = fit.Ctrl)

# XGB Dart
model.XGBD <- train(Purchase~., data = train.3, method = 'xgbDART',
                    tuneLength = 5,
                    trControl = fit.Ctrl)

# SVM
model.SVM <- train(Purchase~., data = train.3, method = 'svmRadial',
                    tuneLength = 15,
                    trControl = fit.Ctrl)

# Compare model performances using resample()
models.compare <- resamples(list(ADA=model.ada, RF = model.RF, XGBD = model.XGBD,SVM = model.SVM))

### Ensemble modelling (multiple models at once with Caret)
train.CTRL <- trainControl(method = 'repeatedcv',
                           number = 10,
                           repeats = 3,
                           savePredictions = 'final',
                           classProbs = T)

algo.list <- c("rf","adaboost","xgbDART","svmRadial")

models <- caretList(Purchase~., train.3, trControl = train.CTRL,
                    methodList = algo.list)

# Plot the resample results
bwplot(resamples(models))

### Stacking models for final preds
stack.glm <- caretStack(models, method = 'glm',
                        metric = Accuracy,
                        trControl = train.CTRL)

preds.stacked <- predict(stack.glm, test.3)
confusionMatrix(test.Y,preds.stacked,mode = "everything", positive = "MM")
