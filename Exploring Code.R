# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)
library(pROC)

# Reading in the files
df = read.csv('Data/train.csv')

df_test = read_csv('Data/test.csv')

# what are the propotion of 0 and 1
df %>%
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
df_boruta = Boruta(target ~., data = df[,-1] %>% 
                     mutate(target = factor(target)) %>%
                      group_by(target) %>%
                     sample_n(10000), getImp = getImpFerns)

getSelectedAttributes(df_boruta)

borruta_Var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp)) %>% slice(1:140) 


#let's try LDA to see if we can split the diffrence

df_lda = lda(target ~., data = df[,c('target',borruta_Var$rowname)] %>% 
                                mutate(target = factor(target)) %>%
                                group_by(target) %>%
                                sample_n(40000, replace = T) %>% 
                                ungroup() %>%
                                select_(quo(borruta_Var[1:40,]$rowname), 'target')
               )
df_lda_pred = (as.integer(predict(df_lda, newdata = df)[[1]]) - 1)

sqrt(mean((df$target - df_lda_pred)^2))

roc(df$target, df_lda_pred)

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
               
      df_roc[x,2] = as.numeric(roc(df$target, df_lda_pred)$auc)
      df_roc[x,3] = sqrt(mean((df$target - df_lda_pred)^2))

}


ggplot(data = df_roc, aes(x = var, y = RMSE)) + geom_line()

df_qda = qda(target ~., 
             data = df[,c('target',borruta_Var$rowname)] %>% 
               mutate(target = factor(target)) %>%
               group_by(target) %>%
               sample_n(20000) %>% 
               ungroup() %>%
               select_(quo(borruta_Var[1:180,]$rowname), 'target') 
)

df_qda_pred = (as.integer(predict(df_qda, newdata = df)[[1]]) - 1)

sqrt(mean((df$target - df_qda_pred)^2))



roc(df$target, df_qda_pred)


df_log = glm(target ~., 
             data = df[,c('target',borruta_Var$rowname)] %>% 
               mutate(target = factor(target)) %>%
               group_by(target) %>%
               sample_n(20000) %>% 
               ungroup() %>%
               select_(quo(borruta_Var[1:40,]$rowname), 'target'),
             family = 'binomial')


df_log_pred = ifelse(predict(df_log, newdata = df) > 0.5, 1, 0)

sqrt(mean((df$target - df_log_pred)^2))


roc(df$target, df_log_pred)