# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)

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

borruta_Var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp)) %>% slice(1:10) %>% pull()


df
#let's try LDA to see if we can split the diffrence

df_lda = lda(target ~., data = df[,c('target',borruta_Var$rowname)] %>% 
                                mutate(target = factor(target)) %>%
                                group_by(target) %>%
                                sample_n(10000))