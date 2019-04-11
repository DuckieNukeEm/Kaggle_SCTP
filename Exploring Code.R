# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)
library(h2o)
library(caret)

# Reading in the files
df = read.csv('Data/train_outlier.csv')

test_df = read.csv('Data/test.csv')

train_vec = sample(nrow(df), size = nrow(df) * 0.8)

df_train = df[train_vec,-1] %>%
             mutate(target = factor(target)) %>%
             filter(outlier < 0.5)

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

#Testing with PCA
df_pca = prcomp(df[,3:ncol(df)], scale = T, center = T)

summary(df_pca)

#lets look at the varriance
library(ggfortify)

autoplot(df_pca, data = df, colour = 'target')

# Running a boruta model to get importance of the data
df_boruta = Boruta(target ~., 
                   
                   data = df[,-1] %>% 
                     mutate(target = factor(target)) %>%
                      group_by(target) %>%
                     sample_n(20000), getImp = getImpFerns)

borruta_var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))


#let's try LDA to see if we can split the diffrence using Linear Discrimant Analysis

df_lda = lda(target ~., data = df[,c('target',borruta_Var$rowname)] %>% 
                                mutate(target = factor(target)) %>%
                                group_by(target) %>%
                                sample_n(40000, replace = T) %>% 
                                ungroup() %>%
                                select_(quo(borruta_Var[1:40,]$rowname), 'target')
               )
df_lda_pred = (as.integer(predict(df_lda, newdata = df)[[1]]) - 1)
pROC::roc()
# What if we see how the ROC curve changes with the number of variables!

df_roc = data.frame(var = 1:nrow(borruta_var), roc = 0, RMSE = 0)

for (x in 1:nrow(df_roc)) {
  print(x)
  df_lda = lda(target ~., data = df_train %>% 
                 mutate(target = factor(target)) %>%
                 group_by(target) %>%
                 sample_n(13000) %>% 
                 ungroup() %>%
                 select_(quo(borruta_var[1:df_roc[x,1],]$rowname), 'target')
          )
               
      df_lda_pred = (as.integer(predict(df_lda, newdata = df)[[1]]) - 1)
               
      df_roc[x,2] = as.numeric(pROC::roc(df$target, df_lda_pred)$auc)
      df_roc[x,3] = sqrt(mean((df$target - df_lda_pred)^2))

}


ggplot(data = df_roc, aes(x = var, y = RMSE)) + geom_line()


# let's try using caret to train a logistics regrssion

# train the model on training set
df_caret_log <- train(target ~ .,
               data = df_train[,c('target',borruta_Var[1:150,]$rowname)],
               trControl = trainControl(method = "cv", number = 10, sampling = 'up'),
               method = "glm",
               family=binomial())
df_log_pred = predict(df_caret_log, newdata = df_test, type = 'prob')


df_caret_svm <- train(target ~ .,
                      data = df_train %>%
                        mutate(target = factor(target)) %>%
                        group_by(target) %>%
                        sample_n(13000, replace = ),
                      trControl = trainControl(method = "cv", number = 10),
                      method = "lssvmRadial",
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
df_log_pred = predict(df_caret_log, newdata = df_test, type = 'prob')







# print cv scores
summary(model)


df_log = glm(target ~.,
             data = df_train[,c('target',borruta_var[1:201,1])] %>% 
                      mutate(Outlier2 = ifelse(outlier >= 0.485,1,0)),
             family = binomial)

pROC::roc(df_test$target,
          ifelse(predict(df_log, newdata = df_test %>% 
                           mutate(Outlier2 = ifelse(outlier >= 0.485,1,0)), type = 'response')>0.10705,1,0) )


table(df_test$target,
          ifelse(predict(df_log, newdata = df_test %>% 
                           mutate(Outlier2 = ifelse(outlier >= 0.485,1,0)), type = 'response')>0.10705,1,0) )
# Hrm....cool let's try some automl
# Hrm....cool let's try some automl

h2o.init()

aml <- h2o.automl(x = borruta_var[1:50,'rowname'], 
                  y = 'target',
                  training_frame = as.h2o(df %>% mutate(target = factor(target))),
                  nfolds = 10,
                  balance_classes = TRUE,
                  max_after_balance_size = 2.0,
#                  max_runtime_secs = 14400,
                  stopping_metric = 'AUC',
                  seed = 1985)

h2o_leaderboard = aml@leaderboard
print(h2o_leaderboard, n = nrow(h2o_leaderboard))

aml_pred = predict(aml, newdata = as.h2o(df_test2))
pROC::roc(df_test2$target, as.integer(as.tibble(aml_pred) %>% pull(predict)) - 1)
pROC::roc(df_test2$target, ifelse(as.tibble(aml_pred) %>% pull(p1) > 0.115,1,0))



table(df_test$target, as.integer(as.tibble(aml_pred) %>% pull(predict)) - 1)

####
#
# Building a model to predict false positives
#
####


df_test_2 = df_test
df_test_2$pred_target =  as.integer(as.tibble(predict(aml, newdata = as.h2o(df_test))) %>% pull(predict)) - 1
df_test_2 = df_test_2 %>%
            mutate(target2 = ifelse(target == 0 & pred_target == 1, 1, 0)) 


df_train_2 = df_train
df_train_2$pred_target =  as.integer(as.tibble(predict(aml, newdata = as.h2o(df_train))) %>% pull(predict)) - 1
df_train_2 = df_train_2 %>%
              mutate(target2 = ifelse(target == 0 & pred_target == 1, 1, 0))



aml_2 <- h2o.automl(x = setdiff(names(df_train_2),c('target','pred_target','target2')), y = 'target2',
                  training_frame = as.h2o(df_train_2 %>% filter(pred_target == 1) %>%
                                            group_by(target2) %>%
                                            sample_n(12000)),
                  nfolds = 10,
                  exclude_algos = c('XGBoost'),
                  #                  max_runtime_secs = 14400,
                  stopping_metric = 'AUC',
                  max_after_balance_size = 1.0,
                  seed = 1985)

h2o_leaderboard_2 = aml_2@leaderboard
print(h2o_leaderboard_2, n = nrow(h2o_leaderboard_2))

aml_pred_2 = predict(aml_2, newdata = as.h2o(df_test_2 %>% filter(pred_target == 1)))
pROC::roc(df_test_2 %>% filter(pred_target ==1) %>% pull(target2), as.integer(as.tibble(aml_pred_2) %>% pull(predict)) - 1)




df_log = glm(target2 ~.,
             data = df_train_2 %>%
               filter(pred_target == 1) %>%
               select(-target, - pred_target) %>%
               group_by(target2) %>%
               sample_n(12000),
             family = binomial)




df_log_pred = ifelse(predict(df_log, newdata = df_test_2 %>% filter(pred_target == 1), type = 'response') > 0.69, 1,0)
pROC::roc(df_test_2 %>% filter(pred_target == 1) %>% pull(target2), df_log_pred)
table(df_test_2 %>% filter(pred_target == 1) %>% pull(target), df_log_pred)



df_tester = df_test_2 %>%
            mutate(
                   pred =0)
df_tester[df_tester$pred_target == 1,'pred'] = df_log_pred
df_tester %>%
  mutate( pred_target = ifelse(pred == 1, 0, pred_target)) %>%
  select(target, pred_target) %>%
  table()

pROC::roc(df_tester$target,
df_tester %>%
  mutate( pred_target = ifelse(pred == 1, 0, pred_target)) %>%
  pull( pred_target))


pROC::roc(df_tester$target,
          df_tester %>%
           pull( pred_target))

# trying lm by class balance vs class weights

