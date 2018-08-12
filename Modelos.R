



#################################################################################################
##########                                   MODELOS                                     ########
#################################################################################################

library(parallel) #Support for Parallel computation
library(doParallel) #Provides a parallel backend
cores <- detectCores()-1
cluster <- makeCluster(cores)
registerDoParallel(cluster)


# In this case, we are goint to use 5 repeats of 10-fold cross validation
ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10)

test_results <- NULL 

secuencia<-seq(1,8,1)
resultados.RMSE<-matrix(ncol=7,nrow=8,NA)
resultados.RSq <-matrix(ncol=7,nrow=8,NA)


for(i in secuencia){
  
  # dividiendo el dataset en training sets y testing sets  
  library(caret)
  in_train <- createDataPartition(log(homeSalesDataModeling$price), p = 0.8, list = FALSE)  # 80% for training
  training <- homeSalesDataModeling[ in_train,]
  testing <- homeSalesDataModeling[-in_train,]
  
  test_results <- data.frame(price = log(testing$price)) #measuring logarithm's prices in testing set 
  
}




#################################################### de la clase de regresion avanzada

cars.fixed <- na.omit(cars.analysis.2)
any(is.na(cars.fixed))
dim(cars.fixed)
names(cars.fixed)

cars.formula.log <- as.formula(paste("log(price) ~ ", paste( as.factor(names(cars.fixed[c(5:ncol(cars.fixed))])), collapse="+"))) #excluyendo marca, serie y modelo.

library(caret)


# Split data into training and testing sets using the caret package
train_index <- createDataPartition(cars.fixed$price, p = .75, list = FALSE)
trainset <- cars.fixed[train_index, ]
testset <- cars.fixed[-train_index, ]

test_results <- data.frame(price = log(testset$price))

lm_model <- train(cars.formula.log,
                  data = trainset,
                  method = "lm",
                  preProcess = c("center", "scale")
)

test_results$lm_pred <- predict(lm_model, testset)
postResample(pred = test_results$lm_pred,  obs = test_results$price)

df_lm <- data.frame(predicted = test_results$lm_pred, observed = test_results$price)

ggplot(df_lm, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Linear Regression Observed VS Predicted")



# Stepwise regression
step_model <- train(cars.formula.log,
                    data = trainset,
                    method = "leapSeq",
                    preProcess = c("center", "scale"),
                    tuneGrid = expand.grid(nvmax = 3:40),
                    trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10)
)

test_results$step_pred <- predict(step_model, testset)
postResample(pred = test_results$step_pred,  obs = test_results$price)
update(plot(test_results$step_pred), main = "Stepwise Regression Performance")

df_step <- data.frame(predicted = test_results$step_pred, observed = test_results$price)

ggplot(df_step, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Linear Regression Observed VS Predicted")


#robust regression
rlm_tune <- train(cars.formula.log, 
                  data = trainset, 
                  method = "rlm", 
                  preProc=c('scale', 'center'),
                  trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$rlm <- predict(rlm_tune, testing)
postResample(pred = test_results$rlm,  obs = test_results$price)
df_rlm <- data.frame(predicted = test_results$rlm, observed = test_results$price)

ggplot(df_rlm, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Linear Regression Observed VS Predicted") 



# ridge regression
ridge_grid <- expand.grid(lambda = seq(0, .001, length = 20))
ridge_model <- train(cars.formula.log,
                     data = trainset,
                     method = "ridge",
                     #preProcess = c("center", "scale"),
                     #tuneGrid = ridge_grid,
                     trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$ridge_pred <- predict(ridge_model, testset)
postResample(pred = test_results$ridge_pred,  obs = testset$price)
update(plot(ridge_model), main = "Ridge Regression Performance")

df_ridge <- data.frame(predicted = test_results$ridge_pred, observed = test_results$price)

ggplot(df_ridge, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Ridge Regression Observed VS Predicted")





# lasso regression
lasso_grid <- expand.grid(fraction = seq(.05, 1, length = 20))
lasso_model <- train(cars.formula.log,
                     data = trainset,
                     method = "lasso",
                     #preProcess = c("center", "scale"),
                     #tuneGrid = lasso_grid,
                     trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$lasso_pred <- predict(lasso_model, testset)
postResample(pred = test_results$lasso_pred,  obs = testset$price)
update(plot(lasso_model), main = "Lasso Regression Performance")

lasso_imp <- varImp(lasso_model, scale = F) # rank the importance of the predictors
plot(lasso_imp, scales = list(y = list(cex = .95)))

df_lasso <- data.frame(predicted = test_results$lasso_pred, observed = test_results$price)

ggplot(df_lasso, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Lasso Regression Observed VS Predicted")






# Elastic net regression
enet_grid <- expand.grid(lambda = seq(0, .001, length = 10), fraction = seq(.05, 1, length = 10))
enet_model <- train(cars.formula.log,
                    data = trainset,
                    method = "enet",
                    #preProcess = c("center", "scale"),
                    #tuneGrid = enet_grid,
                    trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$enet_pred <- predict(enet_model, testset)
postResample(pred = test_results$enet_pred,  obs = testset$Balance)
update(plot(enet_model), main = "Elastic Net Performance")

df_enet <- data.frame(predicted = test_results$enet_pred, observed = test_results$price)

ggplot(df_enet, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Enet Regression Observed VS Predicted")







#PCR
pcr_model <- train(cars.formula.log,
                   data = trainset,
                   method = "pcr",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:15),
                   trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$pcr_pred <- predict(pcr_model, testset)
postResample(pred = test_results$pcr_pred,  obs = test_results$price)

df_pcr <- data.frame(predicted = test_results$pcr_pred, observed = test_results$price)

ggplot(df_pcr, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("PCR Regression Observed VS Predicted")



#PLS
pls_model <- train(cars.formula.log,
                   data = trainset,
                   method = "pls",
                   preProcess = c("center", "scale"),
                   tuneGrid = expand.grid(ncomp = 1:15),
                   trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$pls_pred <- predict(pls_model, testset)
postResample(pred = test_results$pls_pred,  obs = test_results$price)

df_pls <- data.frame(predicted = test_results$pls_pred, observed = test_results$price)

ggplot(df_pls, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("PLS Regression Observed VS Predicted")


#knn
knn_model <- train(cars.formula.log, 
                   data = trainset,
                   method = "knn",   
                   preProc=c('scale','center'),
                   trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$knn_pred <- predict(knn_model, testset)
postResample(pred = test_results$knn_pred,  obs = test_results$price)

df_knn <- data.frame(predicted = test_results$knn_pred, observed = test_results$price)

ggplot(df_knn, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("KNN Regression Observed VS Predicted")


#glmnet
glmnet_model <- train(cars.formula.log, 
                      data = trainset,
                      method='glmnet',
                      preProc=c('scale','center'),
                      trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

plot(glmnet_model)

test_results$glmnet_pred <- predict(glmnet_model, testset)
postResample(pred = test_results$glmnet_pred,  obs = test_results$price)

df_glmnet <- data.frame(predicted = test_results$glmnet_pred, observed = test_results$price)

ggplot(df_glmnet, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("GLMnet Regression Observed VS Predicted")



#random forest 
rf_model <- train(cars.formula.log, 
                  data = trainset,
                  method = "rf",
                  preProc=c('scale','center'),
                  trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10),
                  ntree = 10,
                  tuneGrid = expand.grid(mtry = c(10,15,20)), 
                  verbose = FALSE)

test_results$rf_pred <- predict(rf_model, testset)
postResample(pred = test_results$rf_pred,  obs = test_results$price)

df_rf <- data.frame(predicted = test_results$rf_pred, observed = test_results$price)

ggplot(df_rf, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Rambom forest Regression Observed VS Predicted")


#SVM
svmR_model <- train(cars.formula.log, 
                    data = trainset,
                    method = "svmRadial",
                    preProc=c('scale','center'),
                    trControl = trainControl(method= "repeatedcv", repeats = 5, number = 10))

test_results$svmR_pred <- predict(svmR_model, testset)
postResample(pred = test_results$svmR_pred,  obs = test_results$price)

df_svmR <- data.frame(predicted = test_results$svmR_pred, observed = test_results$price)

ggplot(df_svmR, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("Rambom forest Regression Observed VS Predicted")



# Combination
test_results$comb = (test_results$lm_pred + test_results$enet_pred + test_results$rf_pred)/3
R2.out = c(R2.out, cor(test_results$comb,test_results$price)^2)














