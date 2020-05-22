#installed.packages("ggthemes")
#install.packages('devtools')
#install.packages("factoextra")

library(vroom)
library(tidyverse)
library(catboost)
library(caret)
library(factoextra)

path <- "/Users/kkalyan/github/airQo/data/"
raw_train <- vroom(paste0(path, "train-proc.csv"), delim = ",", col_types=list(location=col_factor()))
raw_tests <- vroom(paste0(path, "test-proc.csv"), delim = ",", col_types=list(location=col_factor()))

train <- raw_train %>% select(-c("location", "ID", "target", "median_precip", "min_precip"))
test_proc <- raw_tests %>% select(-c("location", "ID", "target", "median_precip", "min_precip"))

train[is.na(train)] <- 0

# Missing Values
# train_proc <- train[complete.cases(train), ]

air.pca <- prcomp(train, center = TRUE,scale. = TRUE)
fviz_eig(air.pca)

#fviz_pca_ind(air.pca, gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

test_proc2 <- scale(test_proc, center = air.pca$center, scale = air.pca$scale)

pca.train = predict(air.pca, train)[,1:8]
pca.test = predict(air.pca, test_proc2)[,1:8]

path = "/Users/kkalyan/github/airQo/data"
write.csv(pca.train, paste0(path,"pca-train.csv"), row.names = FALSE)
write.csv(pca.test, paste0(path,"pca-test.csv"), row.names = FALSE)
