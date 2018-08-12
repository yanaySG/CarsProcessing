
#################################################################################################
##########                            IMPORTANCIA DE VARIABLES                           ########
#################################################################################################


# instalar las librerías que serán utilizadas en la importancia de variables




#Variable_selection <- function() {

#PAQUETES NECESARIOS


  #############################################################
  # ---              Relación entre variables               ---  
  #############################################################

  #trabajamos con el nuevo dataset procesado (sin missing values)
  cars.fixed = car.imp.missForest #en este caso se ha escogido el data set imputado por random forest

  #se divide el dataset en variabes categóricas y contínuos para continuar el análisis
  df.num <- data.frame(matrix(ncol = 0, nrow = nrow(cars.fixed)))   #dataframe para variables numéricas
  df.cat <- data.frame(matrix(ncol = 0, nrow = nrow(cars.fixed))) #dataframe para variables categóricas


  for (variable in as.vector(names(cars.fixed)) ){
    if (class(cars.fixed[[variable]])=="numeric" | class(cars.fixed[[variable]])=="integer") { #variables numéricas
      df.num[[variable]] <- cars.fixed[[variable]]
    }
    if(class(cars.fixed[[variable]])=="factor" ) { #variables categóricas de más de 50 valores
      df.cat[[variable]] <- cars.fixed[[variable]]
    }
  }

  #probando los dataset creados
  str(df.num)
  str(df.cat)


  # Relación entre variables numéricas (tres formas)
  df.num.corMatrix <- cor(df.num, use = "pairwise.complete.obs") # calculate correlation matrix. solo se puede utilizar las variables numéricas

  library(stats)
  heatmap(x = df.num.corMatrix, col = colorRampPalette(c("blue", "white", "red"))(20), symm = TRUE)

  library(corrplot)
  corrplot(df.num.corMatrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

  library(ggcorrplot)
  ggcorrplot(df.num.corMatrix, hc.order = TRUE, type = "lower", lab = TRUE)


  #encontrar variables más correladas, probando con 0.9, 0.8 y 0,75 respectivamente, 
  library(caret)
  set.seed(1) # ensure the results are repeatable
  findCorrelation(df.num.corMatrix, cutoff=0.9, names = TRUE, exact = TRUE) # find attributes that are highly corrected 
  
  set.seed(1) 
  findCorrelation(df.num.corMatrix, cutoff=0.8, names = TRUE, exact = TRUE) # find attributes that are highly corrected 

  set.seed(1) 
  findCorrelation(df.num.corMatrix, cutoff=0.75, names = TRUE, exact = TRUE)   # find attributes that are highly corrected (ideally > 0.75)
  
  
  # Relations between response and categorical variables
  dim(df.cat)
  df.catANDprice <- data.frame( price=cars.fixed$price, df.cat  )
  par(mfrow=c(4,3))
  sapply(c(2:ncol(df.catANDprice)), function(j)boxplot(df.catANDprice[,1] ~ df.catANDprice[,j], main=colnames(df.catANDprice)[j], ylab = "Price",xlab="",col="blue"))
  sapply(c(2:ncol(df.catANDprice)), function(j)boxplot(log(df.catANDprice[,1]) ~ df.catANDprice[,j], main=colnames(df.catANDprice)[j], ylab = "Price",xlab="",col="blue")) #observando con el log de la variable objetivo

  #
  library(GGally)
  ggscatmat(cars.fixed)
  
  
  ##################################################################
  # ---              Importancia de las variables                ---  
  ##################################################################
  
  ########### Regresión lineal ##########
  
  #observamos la importancia de la variable según una regresión lineal 
  summary(model.lm <- lm(price ~ ., data = cars.fixed)) #no todas las vatiables son significativas, además se detectan problemas con algunas variables q están generando NAs en el modelo
                                                        #hasta que no estén todos los datos no se puede tomar una decisión en las variables significativas del modelo, ni qué hacer con las variables que provocan NAs en el modelo porque los nuevos datos pueden dar resultados diferentes
  summary(model.lm.log <- lm(log(price) ~ ., data=cars.fixed)) #se observa que muchas más variables son significativas en este caso
  
  #diagnosis
  par(mfrow=c(2,2))
  plot(model.lm, pch=23 ,bg='orange',cex=2, main ="model.1")
  plot(model.lm.log, pch=23 ,bg='orange',cex=2, main ="model.1")
  par(mfrow=c(1,1))
  
  
  ########### Regresión robusta ##########   #la regresión robusta es menos sencible a valores atípicos
  library(MASS)
  summary(model.robust <- rlm(price ~ ., data=cars.fixed)) #Error in rlm.default(x, y, weights, method = method, wt.method = wt.method,:'x' is singular: singular fits are not implemented in 'rlm'

  
  
  ########### GLM ########## 
  fit.gaussian <- glm(price  ~ ., data = cars.fixed, family = gaussian)
  summary(fit.gaussian)
  
  fit.gaussian.log <- glm(log(price)  ~ ., data = cars.fixed, family = gaussian)
  summary(fit.gaussian)
  
  
  ########### Stepwise Regression ########### http://www.sthda.com/english/articles/37-model-selection-essentials-in-r/154-stepwise-regression-essentials-in-r/
  library(tidyverse)

  library(MASS) 
  step.model.lm <- stepAIC(model.lm, direction = "both",  trace = FALSE)  # Stepwise linear regression model
  step.model.lm.log <- stepAIC(model.lm.log, direction = "both",  trace = FALSE)  # Stepwise linear regression model whit logarithm of the targuet variable 
  step.model.gaussian <- stepAIC(fit.gaussian, direction = "both",  trace = FALSE)  # Stepwise gaussian regression model. (glm)
  step.model.gaussian.log <- stepAIC(fit.gaussian.log, direction = "both",  trace = FALSE)  # Stepwise gaussian regression model whit logarithm of the targuet variable (glm)  

  #diagnosis for all models. 
  par(mfrow=c(2,2))
  plot(step.model.lm, pch=23 ,bg='orange',cex=2, main ="model.1")
  plot(step.model.lm.log, pch=23 ,bg='orange',cex=2, main ="model.1")
  plot(step.model.gaussian, pch=23 ,bg='orange',cex=2, main ="model.1")
  plot(step.model.gaussian.log, pch=23 ,bg='orange',cex=2, main ="model.1")
  par(mfrow=c(1,1))
  
  
  library(caret)
  set.seed(123)   # Set seed for reproducibility
  train.control <- trainControl(method = "cv", number = 10)  # Set up repeated k-fold cross-validation

  # Train the linear model model
  step.model.lm <- train(price ~., data = cars.fixed,
                      method = "leapSeq", 
                      tuneGrid = data.frame(nvmax = 1:80), 
                      trControl = train.control
                      )   
  step.model.lm$results
  
  best.tune.lm <- step.model.lm$bestTune
  betas.lm <- coef(step.model.lm$finalModel, as.numeric(best.tune.lm))
  print(betas.lm)

  # Train the linear model model with logarithm of the target variable
  step.model.lm.log <- train(log(price) ~., data = cars.fixed,
                         method = "leapSeq", 
                         tuneGrid = data.frame(nvmax = 1:80),
                         trControl = train.control
  )   # Train the linear model model
  step.model.lm.log$results
  
  best.tune.lm.log <- step.model.lm.log$bestTune
  betas.lm.log <- coef(step.model.lm.log$finalModel, as.numeric(best.tune.lm.log))
  print(betas.lm.log)
  
  
  
  
  ########### regularization models using glmnet library ##########  
  library(glmnet)
  library(dplyr)
  '%ni%'<-Negate('%in%')
  
  data(mtcars)
  
  x<-model.matrix(price~.,data=cars.fixed)
  x=x[,-1]
  
  model.glmnet = glmnet(x, y=cars.fixed$price)
  plot(model.glmnet, label = TRUE)
  #print(model.glmnet)

  # modelo LASSO 
  fit.lasso = cv.glmnet(x, y=cars.fixed$price, alpha=1) #α=1 is the lasso (default) 
  plot(fit.lasso)
  
  fit.lasso$lambda.min #lambda.min is the value of λ that gives minimum mean cross-validated error. 
  fit.lasso$lambda.1se #lambda.1se gives the most regularized model such that error is within one standard error of the minimum
  
  lasso.coef <-as.data.frame( as.matrix(coef(fit.lasso, s='lambda.1se', exact=TRUE)))
  
  names(lasso.coef) <- "values"
  lasso.coef$names <- rownames(lasso.coef)
  lasso.coef <- lasso.coef %>% select(names, everything())
  rownames(lasso.coef) <- NULL
  lasso.coef %>% filter(values !=0)
  
  
  # modelo RIDGE. este modelo no es un selector de variables porque a todos los predictores les asigna un beta aunque no sea un predictor significativo
  fit.ridge = cv.glmnet(x, y=cars.fixed$price, alpha=0) #α=0 is the ridge.
  plot(fit.ridge)
  
  ridge.coef <-as.data.frame( as.matrix(coef(fit.ridge, s='lambda.1se', exact=TRUE)))
  
  names(ridge.coef) <- "values"
  ridge.coef$names <- rownames(ridge.coef)
  ridge.coef <- ridge.coef %>% select(names, everything())
  rownames(ridge.coef) <- NULL
  ridge.coef %>% filter(values !=0)
  
  
  #LASSO and RIDGE mixed. proveen mas cantidad de variables significativas
  fit.mixed = cv.glmnet(x, y=cars.fixed$price, alpha=.5) #α=1 is the lasso (default) 
  plot(fit.mixed)
  
  fit.mixed.coef <-as.data.frame( as.matrix(coef(fit.mixed, s='lambda.1se', exact=TRUE)))
  
  names(fit.mixed.coef) <- "values"
  fit.mixed.coef$names <- rownames(fit.mixed.coef)
  fit.mixed.coef <- fit.mixed.coef %>% select(names, everything())
  rownames(fit.mixed.coef) <- NULL
  fit.mixed.coef %>% filter(values !=0)
  


  ########### Random Forest ##########
  # library(caret)
  # set.seed(1) # ensure results are repeatable
  # ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10) # In this case, we are goint to use 5 repeats of 10-fold cross validation
  # 
  # model.rf <- train(price~., 
  #                   data=cars.fixed, 
  #                   method="rf", 
  #                   trControl=ctrl, 
  #                   importance = TRUE) # train the model with random forest
  # 
  # importance.rf <- varImp(model.rf) # estimate variable importance
  # print(importance.rf) # summarize importance
  # plot(importance.rf) # plot importance
  
  
  
  ########### Partial Least Squares ########### 
  library(caret)
  set.seed(1) # ensure results are repeatable
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10) # In this case, we are goint to use 5 repeats of 10-fold cross validation
  
  model.pls <- train(price~., 
                     data=cars.fixed, 
                     method="pls", 
                     trControl=ctrl,
                     preProcess = c("center", "scale"),
                     importance = TRUE) # train the model with random forest
  
  importance.pls <- varImp(model.pls) # estimate variable importance
  print(importance.pls) # summarize importance
  plot(importance.pls) # plot importance
  

  
  ########### Neuronal Network ##########
  library(caret)
  set.seed(1) # ensure results are repeatable
  ctrl <- trainControl(method = "repeatedcv", repeats = 5, number = 10) # In this case, we are goint to use 5 repeats of 10-fold cross validation
  
  #Multilayer Perceptron Network by Stochastic Gradient Descent
  model.mlpSGD <- train(price~., 
                       data=cars.fixed,  
                       trControl=ctrl, 
                       method = "mlpSGD",
                       preProcess = c("center", "scale"),
                       importance = TRUE) #Multilayer Perceptron Network by Stochastic Gradient Descent
  
  importance.mlpML <- varImp(model.mlpML) # estimate variable importance
  print(importance.mlpML) # summarize importance
  plot(importance.mlpML) # plot importance
  
  
  #elasticnet
  model.enet <- train(price~., 
                       data=cars.fixed,  
                       trControl=ctrl, 
                       method = "enet",
                       #preProcess = c("center", "scale"),
                       importance = TRUE) 
  
  importance.enet <- varImp(model.enet) # estimate variable importance
  print(importance.enet) # summarize importance
  plot(importance.enet) # plot importance
  
  
  
  
  
  
  
  
  
  
  
  
  


  
  #Feature Selection
  set.seed(7)   # ensure the results are repeatable
  ctrl <- rfeControl(functions=rfFuncs, method="cv", number=10) # define the control using a random forest selection function
  results <- rfe(cars.fixed[,2:ncol(cars.fixed)], cars.fixed[,1], sizes=c(2:ncol(cars.fixed)), rfeControl=ctrl) # run the RFE algorithm
  print(results)   # summarize the results
  predictors(results)   # list the chosen features
  plot(results, type=c("g", "o"))   # plot the results
  
  
  
  


















# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)

# calculate correlation matrix
correlationMatrix <- cor(df.num)
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)





# ensure results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the dataset
data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(diabetes~., data=PimaIndiansDiabetes, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



#################################################################################################
##########              PREPARACIÓN DE LOS DATOS: SELECCIÓN DE VARIABLES                 ########
#################################################################################################

cars.new <- car.imp.missForest
n=nrow(cars.new)

# Separamos nuestra muestra en dos partes el training y el test, 600 para la train y el resto para test
set.seed(1) # fijamos semilla para mantener fijo el vector train
train=sample(n,2000) # vector para extraer la muestra de training
cars.train=cars.new[train,]
cars.test=cars.new[-train,]

train.formula <- as.formula(paste("price ~ ", paste( as.factor(names(cars.fixed[c(5:ncol(cars.train))])), collapse="+"))) #excluyendo marca, serie y modelo.

library(tree)

tree.train=tree(
  train.formula, cars.train #, method = "recursive.partition", split = "gini"
)


tree.train
summary(tree.train)
plot(tree.train)
text(tree.train,pretty=0)

tree.pred=predict(tree.train, cars.test)
# matriz de confusion
table(tree.pred, muestra.test$OK)
(31+157)/(31+52+67+157) # un 61,23% de error de test. Ahora se tiene una mejor clasificacion por la eliminacion de los clientes con menos de 40 movimientos y los que no tienen al menos dos meses de movimientos, y la
# no consideracion del ultimo mes si tiene pocos movimientos. HA MEJORADO MUCHISIMO, OJO! QUE ES EN GRAN MEDIDA ES PORQUE USO un monton VARIABLES Y EL ARBOL ACABA EN 26 HOJAS
# Tengo 600 observaciones, luego deberia usar como mucho tantas variables como se desee pero que solo acabemos con unas 15 hojas, no 26...
(table(tree.pred,muestra.test$OK)[1]+table(tree.pred,muestra.test$OK)[4])/nrow(muestra.test) #mio: índice de aciertos cálculo dinámico






########################
####    BOOSTING    ####
########################
cars.new <- car.imp.missForest

cars.formula.1 <- as.formula(paste("price ~ ", paste( as.factor(names(cars.fixed[c(5:ncol(cars.new))])), collapse="+"))) #excluyendo marca, serie y modelo.

library(gbm)

set.seed(1)

boost3.muestra.train = gbm( cars.formula, 
                            distribution = "bernoulli", 
                            data=muestra.train , 
                            n.trees=5000, 
                            interaction.depth=7, 
                            shrinkage = 0.01
                            )










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



rlm_tune <- train(cars.formula.log, 
                  data = training, 
                  method = "rlm", 
                  preProc=c('scale', 'center'),
                  trControl = ctrl)

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
                     preProcess = c("center", "scale"),
                     tuneGrid = ridge_grid,
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
                     preProcess = c("center", "scale"),
                     tuneGrid = lasso_grid,
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
                    preProcess = c("center", "scale"),
                    tuneGrid = enet_grid,
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

pls_imp <- varImp(test_results$pls_model, scale = FALSE) # rank the importance of the predictors
plot(pls_imp, scales = list(y = list(cex = .95)))

df_pls <- data.frame(predicted = test_results$df_pls, observed = test_results$price)

ggplot(df_pls, aes(x = observed, y = predicted)) +
  geom_point() + lims(x = c(10, 15), y = c(10, 15)) +
  geom_abline(intercept = 0, slope = 1, colour = "blue") +
  ggtitle("PLS Regression Observed VS Predicted")





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
rf_model <- train(lcars.formula.log, 
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





#  return(TRUE)
#}
