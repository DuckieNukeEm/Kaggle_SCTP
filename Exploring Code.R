# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)
library(h2o)
# Reading in the files
df = read.csv('Data/train.csv')

test_df = read_csv('Data/test.csv')

train_vec = sample(nrow(df), size = nrow(df) * 0.8)

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

#Testing with PCA
df_pca = prcomp(df[,3:ncol(df)], scale = T, center = T)

summary(df_pca)

#lets look at the varriance
library(ggfortify)

autoplot(df_pca, data = df, colour = 'target')

# Running a boruta model to get importance of the data
df_boruta = Boruta(target ~., data = df_train %>% 
                     mutate(target = factor(target)) %>%
                      group_by(target) %>%
                     sample_n(16000), getImp = getImpFerns)

getSelectedAttributes(df_boruta)

borruta_Var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))


#let's try LDA to see if we can split the diffrence using Linear Discrimant Analysis

df_lda = lda(target ~., data = df[,c('target',borruta_Var$rowname)] %>% 
                                mutate(target = factor(target)) %>%
                                group_by(target) %>%
                                sample_n(40000, replace = T) %>% 
                                ungroup() %>%
                                select_(quo(borruta_Var[1:40,]$rowname), 'target')
               )
df_lda_pred = (as.integer(predict(df_lda, newdata = df)[[1]]) - 1)

# What if we see how the ROC curve changes with the number of variables!

df_roc = data.frame(var = 1:140, roc = 0, RMSE = 0)

for (x in 1:nrow(df_roc)) {
  print(x)
  df_lda = lda(target ~., data = df[,c('target',borruta_Var$rowname)] %>% 
                 mutate(target = factor(target)) %>%
                 group_by(target) %>%
                 sample_n(40000, replace = T) %>% 
                 ungroup() %>%
                 select_(quo(borruta_Var[1:df_roc[x,1],]$rowname), 'target')
          )
               
      df_lda_pred = (as.integer(predict(df_lda, newdata = df)[[1]]) - 1)
               
      df_roc[x,2] = as.numeric(pROC::roc(df$target, df_lda_pred)$auc)
      df_roc[x,3] = sqrt(mean((df$target - df_lda_pred)^2))

}


ggplot(data = df_roc, aes(x = var, y = RMSE)) + geom_line()


# Hrm....cool let's try some automl

h2o.init()

aml <- h2o.automl(x = borruta_Var[1:50,]$rowname, y = 'target',
                  training_frame = as.h2o(df_train),
                  validation_frame = as.h2o(df_test),
                  max_models = 20,
                  balance_classes = T,
                  seed = 1)

h2o_leaderboard = aml@leaderboard
print(h2o_leaderboard, n = nrow(h2o_leaderboard))


### Meh, could be better, let's try squaring all the vars and see what happens

df_train_sqr = bind_cols(df_train,
                         as.tibble(df_train[,-1]*df_train[,-1]) %>%
                           setNames(paste0('sqr_', names(.)))
                          )

df_test_sqr = bind_cols(df_test,
                        as.tibble(df_test[,-1]*df_test[,-1]) %>%
                          setNames(paste0('sqr_', names(.)))
                        
                        )
aml_sqr <- h2o.automl(x = names(df_train_sqr[,borruta_Var2[1:40,]$rowname]),
                  y = 'target',
                  training_frame = as.h2o(df_train_sqr),
                  validation_frame = as.h2o(df_test_sqr),
                  max_models = 20,
                  balance_classes = T,
                  seed = 1)

h2o_leaderboard = aml_sqr@leaderboard
print(h2o_leaderboard, n = nrow(h2o_leaderboard))

aml_pred = predict(aml_sqr, newdata = as.h2o(df_test_sqr))
pROC::roc(df_test_sqr$target, as.integer(as.tibble(aml_pred) %>% pull(predict)) - 1)

df_boruta = Boruta(target ~., data = df_train_sqr %>% 
                     mutate(target = factor(target)) %>%
                     group_by(target) %>%
                     sample_n(16000), getImp = getImpFerns)

getSelectedAttributes(df_boruta)

borruta_Var2 = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))

