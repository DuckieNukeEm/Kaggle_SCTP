# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(lightgbm)
library(caret)


# Reading in the files
df = read.csv('Data/train_outlier.csv', stringsAsFactors = FALSE)

test_df = read.csv('Data/test.csv', stringsAsFactors = FALSE)

train_vec = sample(nrow(df), size = nrow(df) * 0.9)

df_train = df[train_vec,-1] %>%
  mutate(target = factor(target)) 

df_test = df[-train_vec,-1] %>%
  mutate(target = factor(target))

# what are the propotion of 0 and 1
df %>%
  group_by(target) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(PerTot = Count/sum(Count))

# LEts take a loook at our split sets
df_test %>%
  group_by(target) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(PerTot = Count/sum(Count))

df_train %>%
  group_by(target) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  mutate(PerTot = Count/sum(Count))


LGB_CV_Predict <- function(lgb_cv, data, num_iteration = NULL, folds=NULL, type=c("cv", "test")) {
  require(foreach)
  if (is.null(num_iteration)) {
    num_iteration <- lgb_cv$best_iter
  }
  if (type=="cv"){
    print("create cross validation predictions")
    pred_mat <- foreach::foreach(i = seq_along(lgb_cv$boosters), .combine = "c", .packages=c("data.table","lightgbm")) %do% {
      lgb_tree <- lgb_cv$boosters[[i]][[1]]
      predict(lgb_tree, 
              data[folds[[i]],], 
              num_iteration = num_iteration, 
              rawscore = FALSE, predleaf = FALSE, header = FALSE, reshape = FALSE)
    }
    
    as.double(pred_mat)[order(unlist(folds))]
    
  } else if (type=="test"){
    print("create test set predictions")
    pred_mat <- foreach::foreach(i = seq_along(lgb_cv$boosters), .combine = "+", .packages=c("data.table","lightgbm")) %do% {
      lgb_tree <- lgb_cv$boosters[[i]][[1]]
      predict(lgb_tree, 
              data, 
              num_iteration = lgb_cv$best_iter, 
              rawscore = FALSE, predleaf = FALSE, header = FALSE, reshape = FALSE)
    }
    as.double(pred_mat)/length(lgb_cv$boosters)
  }
}

df2= df

df_kmeans = kmeans(scale(df[,borruta_var[1:100,1]]), nstart = 5, centers = 20)
df2$cluster = factor(df_kmeans$cluster)
df2 %>% 
  group_by(target, cluster) %>% 
  summarise(Counter = n()) %>% 
  ungroup() %>%
  spread(target, Counter, fill = 0) %>%
  mutate(per_1 = round(`1`/(`1` + `0`)*100,2),
         per_run= cummean(per_1)) %>%
  data.frame()

df_train2 = df2[train_vec,-1] %>%
  mutate(target = factor(target)) 

df_test2 = df2[-train_vec,-1] %>%
  mutate(target = factor(target))

cvFoldsList <- createFolds(df_train2 %>% 
                             mutate(target2 = ifelse(target == 1,1,0)) %>%
                             pull(target2), k=10)





dtrain <- lgb.Dataset(as.matrix(df_train2[,-1]),
                      label=df_train2 %>% 
                        mutate(target2 = ifelse(target == 1,1,0)) %>%
                        pull(target2), free_raw_data = FALSE)

params <- list(
            objective = "binary", 
               boost="gbdt", #
               metric="auc",#
               boost_from_average="false", #
               num_threads=14, #
               learning_rate = 0.01, #
               num_leaves = 13, #
               max_depth=-1,#
               tree_learner = "serial", #
               feature_fraction = 0.045, # 
               bagging_freq = 5,#
               bagging_fraction = 0.38, #
               min_data_in_leaf = 80, #80
               min_sum_hessian_in_leaf = 10.0,
               verbosity = 1)

tme <- Sys.time()

lgb1 <- lgb.cv(params,
               dtrain,
               nrounds=35000,
               folds=cvFoldsList,
               early_stopping_rounds = 3000,
               eval_freq=500,
               seed=1985)

Sys.time() - tme


test_preds <- LGB_CV_Predict(lgb1, data.matrix(df_train2[,-1]), type="test")

summary(test_preds)

pROC::roc(df_test$target, ifelse(test_preds > 0.105, 1, 0))

pROC::roc(df_test$target, ifelse(test_preds > 0.99, 1, 0))


table(df_test$target, ifelse(test_preds > 0.9995, 1, 0))

df$pred_target = test_preds
df_test$pred_target =test_preds



####
#
# Weaker secondar check (should be pretty crappy)
####

cvFoldsList <- createFolds(df %>% 
                             mutate(target2 = ifelse(target == 1,1,0)) %>%
                             pull(target2), k=10)

dtrain_sub <- lgb.Dataset(as.matrix(df[,borruta_var[101:200,1]]),
                      label=df %>% 
                        mutate(target2 = ifelse(target == 1,1,0)) %>%
                        pull(target2), free_raw_data = FALSE)

tme <- Sys.time()

lgb_sub <- lgb.cv(params,
               dtrain_sub,
               nrounds=35000,
               folds=cvFoldsList,
               early_stopping_rounds = 3000,
               eval_freq=500,
               seed=44000)

Sys.time() - tme


test_preds <- LGB_CV_Predict(lgb_sub, data.matrix(df_test[, borruta_var[100:201,1]]), type="test")

summary(test_preds)

pROC::roc(df_test$target, ifelse(test_preds > 0.925, 1, 0))

table( ifelse(test_preds > 0.925, 1, 0), df_test$target )

df$pred_target = test_preds


table()

