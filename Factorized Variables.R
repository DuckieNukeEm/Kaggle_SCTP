# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)
library(caret)
library(h2o)
# Reading in the files
df = read.csv('Data/train.csv')

test_df = read_csv('Data/test.csv')

train_vec = sample(nrow(df), size = nrow(df) * 0.8)

for( i in names(df)[3:ncol(df)]){
  print(i)
    xs = quantile(df[,i],  c(0,0.2,0.4,0.6,0.8,1))
    df[,paste0('split','_', i )] = cut(df[,i], breaks = xs, labels = c('sl','l','m','h','sh'))
      
  
}

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

# building factor columns

# Running a boruta model to get importance of the data
df_boruta = Boruta(target ~., data = df[complete.cases(df),2:ncol(df)] %>% 
                     mutate(target = factor(target)) %>%
                     group_by(target) %>%
                     sample_n(40000, T), getImp = getImpFerns)

borruta_var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))


df_caret_log <- train(target ~ .,
                      data = df_train[,c('target',borruta_var[1:20,]$rowname)],
                      trControl = trainControl(method = "cv", number = 10, sampling = 'up'),
                      method = "glm",
                      family=binomial())

df_log_pred = as.integer(predict(df_caret_log, newdata = df_test, type = 'raw')) - 1


pROC::auc(df_test$taarget, df_log_pred)






df_kmeans = kmeans(scale(df_train[,borruta_var[1:100,1]]), nstart = 5, centers = 20)
df_train2$cluster = factor(df_kmeans$cluster)
df_train2 %>% 
  group_by(target, cluster) %>% 
  summarise(Counter = n()) %>% 
  ungroup() %>%
  spread(target, Counter, fill = 0) %>%
  mutate(per_1 = round(`1`/(`1` + `0`)*100,2),
         per_run= cummean(per_1)) %>%
  data.frame()
