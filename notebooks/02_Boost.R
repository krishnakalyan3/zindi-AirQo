#installed.packages("ggthemes")
#install.packages('devtools')
#install.packages("caret")

library(vroom)
library(tidyverse)
library(catboost)
library(caret)

path <- "/Users/kkalyan/github/airQo/data/"
raw_train <- vroom(paste0(path, "train-proc.csv"), delim = ",", col_types=list(location=col_factor()))
raw_tests <- vroom(paste0(path, "test-proc.csv"), delim = ",", col_types=list(location=col_factor()))

X <- raw_train %>% select(-c("target", "ID"))
y <- raw_train$target

params <- list(iterations = 7000, 
               loss_function = 'RMSE', 
               eval_metric='RMSE',
               learning_rate=0.1,
               od_wait=100,
               use_best_model=TRUE)

total_folds <- 20
folds <- cut(seq(1,nrow(X)), breaks=total_folds, labels=FALSE)

rmse_vec <- c()
X_test <- raw_tests %>% select(-c( "ID", "target"))
dtest <- catboost.load_pool(data=X_test)
pred_mat <- matrix(0, nrow = 5035, ncol = total_folds)

for(i in 1:total_folds){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==1,arr.ind=TRUE)
  X_train_fold <- X[-testIndexes, ]
  y_train_fold <- y[-testIndexes]
  
  X_val_fold <- X[testIndexes, ]
  y_val_fold <- y[testIndexes]
  
  dtrain <- catboost.load_pool(data=X_train_fold, label=y_train_fold)
  dval <- catboost.load_pool(data=X_val_fold, label=y_val_fold)
  
  model <- catboost.train(dtrain, dval, params=params)
  eval_preds <- catboost.predict(model, dval)
  test_preds <- catboost.predict(model, dtest) 
  
  pred_mat[,i] <- test_preds
  rmse_cat <- postResample(eval_preds, y_val_fold)
  rmse_vec <- c(rmse_cat, rmse_vec)
}

# RMSE: 23.85
mat_rmse <- matrix(rmse_vec, nrow=total_folds)
rowMeans(mat_rmse)

sub <- vroom(paste0(path, "sample_sub.csv"), delim = ",")
sub$target <- rowMeans(pred_mat)

write_path = "/Users/kkalyan/github/airQo/submissions/"
write.csv(sub, paste0(write_path,"r-baseline-catboost-cv-7k-20cv.csv"), row.names = FALSE)

# https://patchwork.data-imaginist.com/
