# Loading packages
library(MASS) #for LDA
library(tidyverse)
library(Boruta)
library(h2o)
library(dbscan)

# Reading in the files
df = read.csv('Data/train.csv', stringsAsFactors = FALSE)

test_df = read.csv('Data/test.csv', stringsAsFactors = FALSE) %>%
          mutate(target = 0)

train_vec = sample(nrow(df), size = nrow(df) * 0.9)

####
#
#  creating isolation data frame
#
####

df2= bind_rows(df %>% 
                 mutate(
                        rn = row_number(),
                        type = ifelse(rn %in% train_vec,'Train','Test')),
               test_df %>% 
                 mutate(
                   rn = row_number(),
                   type = 'Final')) %>%
      mutate(
        v_81x139x6 = var_81 * var_139 * var_6,
        v_6xv157 = var_6 * var_157,
        v_6X_0 = var_6 * var_0,
        v_110x113 = var_110 * var_113,
        v_110x191 = var_110 * var_191,
        v_26x48 = var_26 * var_48,
        v_26x134 = var_26 * var_134,
        v_190x171 = var_190 * var_171,
        v_133x_71 = var_113 * var_71,
        v_133x110 = var_113 * var_110,
        v_133x196 = var_113 * var_196,
        
        v_81x6 = var_81 * var_6,
        v_81x12 = var_81 * var_12,
        v_81x26 = var_81 * var_26,
        v_81x139 = var_81 * var_139,
        v_81x146 = var_81 * var_146,
        v_12x139 = var_12 * var_139,
        v_12x26 = var_12 * var_26,
        v_12x6 = var_12 * var_6,
        v_12x146 = var_12 * var_146,
        v_139x26 = var_139 * var_26,
        v_139x6 = var_139 * var_6,
        v_139x146 = var_139 * var_146,
        v_26x6 = var_26*var_6,
        v_26x146 = var_26*var_146,
        v_6x146 = var_6*var_146
        
        
        )

#81, 12,139,26,6,146


h2o.init()

if(FALSE){
  
isofor = h2o.isolationForest(as.h2o(df2 %>% filter(type == 'Train') %>% select(-type, -ID_code, -target)),
                             model_id = 'IsolationForest',
                             ntrees = 1000, #1000
                             max_depth = 64, #16
                             sample_size = 512, #512
                             mtries = 200 #100
                #             col_sample_rate_change_per_level = 0.95
                #             col_sample_rate_per_tree = 0.75
                             )


h2o.saveModel(object = isofor, path = '~/git/Kaggle_SCTP/Models/isolation_forest.h2o', force =TRUE)
} else {
isofor = h2o.loadModel('~/git/Kaggle_SCTP/Models/isolation_forest.h2o/IsolationForest')
isopred = predict(isofor, newdata = as.h2o(df2 %>% select(-type, -ID_code, -target)))


df2$outlier = as.data.frame(isopred)[,'predict']

df2 = df2 %>%
  mutate(
    v_81xO = var_81 * outlier,
    v_12xO = var_12 * outlier,
    v_139xO = var_139 * outlier,
    v_26xO = var_26 * outlier,
    v_6xO = var_6 * outlier,
    v_146xO = var_146 * outlier
    
  )
}


if(FALSE){
  tt = pointdensity(as.matrix(scale(df2 %>% 
                                      filter(type == 'Final') %>% 
                                      #select_(borruta_var[2:15,1])
                                      select(setdiff(names(df),c('target','ID_code')))
  )),
  eps = 20, 
  type = 'density')
} else {
  #load(, file = 'Data/DBASC_Values.RData')
  df2$density = 0
  df2[df2$type == 'Train',]$density = tt_train
  df2[df2$type == 'Train',]$density = tt_test
  df2[df2$type == 'Final',]$density = tt_final
  
}


######
#
# creating a ZScore average thingy
#
######

if(FALSE){

xs_train = rep(0, nrow(df2 %>% filter(type == 'Train')) )
xs_test = rep(0, nrow(df2 %>% filter(type == 'Test')))
xs_final = rep(0, nrow(df2 %>% filter(type == 'Final')))
for( i in names(df)[3:ncol(df)]){
  print(i)
  xs_train = xs_train + as.vector(scale(df2[df2$type == 'Train',i]))
  xs_test = xs_test + as.vector(scale(df2[df2$type == 'Test',i]))
  xs_final = xs_final + as.vector(scale(df2[df2$type == 'Final',i]))
  
}

df2$z_score = 1
df2[df2$type == 'Train', ]$z_score = xs_train/200
df2[df2$type == 'Test', ]$z_score = xs_test/200
df2[df2$type == 'Final', ]$z_score = xs_final/200

}

####
#
# Finding the important variables
#
####
df2$random = runif(nrow(df2))
df_boruta = Boruta(target ~., 
                   
                   data = df2%>% 
                     filter(type == 'Train') %>%
                     select(-type, -ID_code) %>%
                     mutate(target = factor(target)) %>%
                     group_by(target) %>%
                     sample_n(8000), getImp = getImpFerns)
plot(df_boruta)
borruta_var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))


df_boruta = Boruta(target ~.,
                   data =df2[,c(names(df),'type','outlier')] %>%
                     filter(type == 'Train') %>%
                     select(-type, -ID_code) %>%
                     mutate(target = factor(target)) %>%
                     group_by(target) %>%
                     sample_n(8000), getImp = getImpFerns)
plot(df_boruta)
clean_borruta_var = attStats(df_boruta) %>% rownames_to_column() %>% arrange(desc(meanImp))



df2 %>% filter(type == 'Train') %>% ggplot(aes(x = factor(target), y = outlier)) + geom_boxplot()




if(FALSE){ #seeing if KNN could make a diffrence, doub it, but you never know
rows_for_train = rownames(df2[df2$type == 'Train',])

knn_rows = sample(rows_for_train, nrow(df2[df2$type == 'Train',])/2)
rows_for_train[!rows_for_train %in% knn_rows]

rows_for_train = rownames(df2[df2$type == 'Train',])


tt_p1 = class::knn(train = scale(df2[knn_rows,borruta_var[2:100,1]]),
                                 test = scale(df2[rows_for_train[!rows_for_train %in% knn_rows], borruta_var[2:100,1]]),
                                 cl = factor(df2[knn_rows,]$target),
                                 k = 1)
                
tt_p2 = class::knn(train = scale(df2[rows_for_train[!rows_for_train %in% knn_rows], borruta_var[1:100,1]]), 
                   test = scale(df2[knn_rows,borruta_var[1:100,1]]),
                   cl = factor(df2[rows_for_train[!rows_for_train %in% knn_rows],]$target),
                   k = 1)
}                   
####
#
#  Creating clusters of the data; may help the predictive power
#
####

if(FALSE){
df_kmeans = kmeans(scale(df2[df2$type == 'Train',borruta_var[1:100,1]]), nstart = 5, centers = 20)

wss <- parallel::mclapply(seq(1, 20, by = 2), 
              function(k){kmeans(scale(df2[df2$type == 'Train',borruta_var[1:10,1]] %>%
                                         sample_n(18000)), 
                                 k, 
                                 nstart=50,iter.max = 15 )$tot.withinss},
              mc.cores = 8)

plot(x = seq(1,10,by = 1), y = unlist(wss))



df_kmeans = kmeans(scale(df2[df2$type == 'Train',borruta_var[100:200,1]]), nstart = 50, iter.max = 15, centers = 20)

df2$cluster = 0
df2[df2$type == 'Train',]$cluster = df_kmeans$cluster


df_kmeans = kmeans(scale(df2[df2$type == 'Train',borruta_var[1:200,1]]), nstart = 50, iter.max = 15, centers = 20)
data.frame(
  target = df2 %>% filter(type == 'Train') %>% pull(target),
  cluster = df_kmeans$cluster) %>% 
  group_by(target, cluster) %>% 
  summarise(Counter = n()) %>% 
  ungroup() %>%
  spread(target, Counter, fill = 0) %>%
  mutate(per_1 = round(`1`/(`1` + `0`)*100,2),
         per_run= cummean(per_1)) %>%
  data.frame()


df2$cluster = 0 

df2[df2$type == 'Train',]$cluster = df_kmeans$cluster


tt = class::knn(train = scale(df2[df2$type == 'Train', borruta_var[1:200,1]]),
               test = scale(df2[df2$type == 'Test', borruta_var[1:200,1]]),
               cl = factor(df_kmeans$cluster),
               k = 1
               )

df2[df2$type == 'Test',]$cluster = as.integer(tt)

df2 %>%
  filter(type != 'Final') %>%
  group_by(type, target, cluster) %>% 
  summarise(Counter = n()) %>% 
  ungroup() %>%
  spread(target, Counter, fill = 0) %>%
  mutate(per_1 = round(`1`/(`1` + `0`)*100,2)) %>%
  group_by(type) %>%
  mutate( per_run= cummean(per_1)) %>%
  data.frame()


tt = class::knn(train = scale(df2[df2$type == 'Train', borruta_var[1:200,1]]),
                test = scale(df2[df2$type == 'Final', borruta_var[1:200,1]]),
                cl = factor(df_kmeans$cluster),
                k = 1)

df2[df2$type == 'Final',]$cluster = as.integer(tt)

}


#####
#
# training xgboost in h2o
#
####
h2o.init()

h2o_xgboost = h2o.xgboost(
#  x =  setdiff(names(df2), c('target','type','ID_code')),
  x = c(borruta_var[borruta_var$meanImp > 0, 'rowname'],'cluster'),
  y = 'target',
  model_id = 'xgboooooooost',
  training_frame = as.h2o(df2 %>%
                            filter(type == 'Train') %>%
                            mutate(target = factor(target),
                                 cluster = factor(cluster),
                                         wt = ifelse(target == 1, 1.3, 1))), #the target gets a slightly higher weight
  #weights_column = 'wt',
  nfolds = 10,
  stopping_metric = 'AUC',
  stopping_tolerance = 0.0001,
  learn_rate = 0.05, #0.07,
  max_runtime_secs = 3600,
  seed = 1985,
  distribution = 'bernoulli',
  ntrees = 2500, #5000
  max_depth = 16,  #16,
  min_rows = 12,
#  subsample = 0.25, #0.75,
  subsample = 0.75, #0.75,
  colsample_bytree = 0.75,
  #grow_policy = 'lossguide',
  gamma = 0.000000001,
  score_tree_interval = 500,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)

h2o.saveModel(object = h2o_xgboost, path = '~/git/Kaggle_SCTP/Models/xgboost2.h2o', force =TRUE)
h2o_xgboost = h2o.loadModel( '~/git/Kaggle_SCTP/Models/xgboost2.h2o/xgboooooooost')
#####
#
# checking auc AUC on the test data set 
#
#####

h2o_xgb_pred = predict(h2o_xgboost, 
                       newdata = as.h2o(df2 %>% 
                                          filter(type == 'Test') %>% 
                                          mutate(target = factor(target),
                                                 cluster = factor(cluster))))

pROC::roc(df2 %>% filter(type == 'Test') %>% pull(target) , as.tibble(h2o_xgb_pred) %>% pull(p1), plot = T )

tt = predict(h2o_xgboost, newdata = as.h2o(df2 %>% filter(type=='Final') %>% select(-ID_code, - type)))

df_out = df2 %>%
          filter(type == 'Final') %>%
          select(ID_code, target)

df_out$target = as.data.frame(tt)$p1

write_csv(df_out, '~/git/Kaggle_SCTP/Data/out1.csv')

####
#
# trying lightgbm version of it 
#
#####


h2o_lgboost = h2o.xgboost(
  x =  setdiff(names(df2), c('target','type','ID_code')),
  y = 'target',
  model_id = 'lightgbm',
  training_frame = as.h2o(df2 %>%
                            filter(type == 'Train') %>%
                            mutate(target = factor(target),
                                   cluster = factor(cluster),
                                   wt = ifelse(target == 1, 1.3, 1))), #the target gets a slightly higher weight
  #weights_column = 'wt',
  nfolds = 10,
  stopping_metric = 'AUC',
  stopping_tolerance = 0.000001,
  learn_rate = 0.07,
  max_runtime_secs = 3600,
  seed = 1985,
  distribution = 'bernoulli',
  ntrees = 5000,
  subsample = 0.75,
  colsample_bytree = 0.75,
 # gamma = 0.000000001,
  score_tree_interval = 500,
  tree_method="hist",
  grow_policy="lossguide",
  min_sum_hessian_in_leaf = 10.0,
  min_data_in_leaf = 40, #80
  max_leaves = 8,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)

h2o.saveModel(object = h2o_lgboost, path = '~/git/Kaggle_SCTP/Models/lightgbm.h2o', force =TRUE)

h2o_lgb_pred = predict(h2o_lgboost,  newdata = as.h2o(df2 %>% 
                                                                   filter(type == 'Train') %>% 
                                                                   mutate(target = factor(target),
                                                                          cluster = factor(cluster))))

pROC::roc(df2 %>% filter(type == 'Train') %>% pull(target) , as.tibble(h2o_lgb_pred) %>% pull(p1), plot = T )

tt = predict(h2o_lgboost,  newdata = as.h2o(df2 %>% 
                                              filter(type=='Final') %>% 
                                              select(-ID_code, - type) %>%
                                              mutate(cluster = factor(cluster))))

df_out = df2 %>%
  filter(type == 'Final') %>%
  select(ID_code, target)

df_out$target = as.data.frame(tt)$p1

write_csv(df_out, '~/git/Kaggle_SCTP/Data/out1.csv')

####
#
# naiveBayes
#
#####

h2o_niave = h2o.naiveBayes(
  x =  setdiff(names(df2), c('target','type','ID_code')),
#  x = c( borruta_var[borruta_var$meanImp > 0, 'rowname']),
  y = 'target',
  model_id = 'bayes',
  training_frame = as.h2o(df2 %>% 
                            filter(type == 'Train') %>% 
                            mutate(target = factor(target),
                                   cluster = factor(cluster)
                                   )) , #the target gets a slightly higher weight
  nfolds = 10,
  laplace = 0,
  seed = 1985,
  fold_assignment = "Modulo",
  keep_cross_validation_predictions = TRUE
)

h2o_niave_pred = predict(h2o_niave,   newdata = as.h2o(df2 %>% 
                                                         filter(type == 'Test') %>% 
                                                         mutate(target = factor(target),
                                                                cluster = factor(cluster))))

pROC::roc(df2 %>% filter(type == 'Test') %>% pull(target), as.tibble(h2o_niave_pred) %>% pull(p1))


####
#
# building a NN
#
#####

# Not yet


####
#
# building an ensamble
#
#####


h2o_ensb = h2o.stackedEnsemble(
        x =  setdiff(names(df2), c('target','type','ID_code')),
        y = 'target',
        model_id = 'joined2',
        training_frame = as.h2o(df2 %>% 
                                  filter(type == 'Train') %>% 
                                  mutate(target = factor(target),
                                         cluster = factor(cluster)
                                  )),
     #   base_models = list(h2o_niave, h2o_lgboost, h2o_xgboost)
     base_models = list(h2o_xgboost, h2o_lgboost)
)

h2o_ensb_pred = predict(h2o_ensb,   newdata = as.h2o(df2 %>% 
                                                       filter(type == 'Test') %>% 
                                                       mutate(target = factor(target),
                                                              cluster = factor(cluster))))


pROC::roc(df2 %>% filter(type == 'Test') %>% pull(target), as.tibble(h2o_ensb_pred) %>% pull(p1))



tt = predict(h2o_ensb, newdata = as.h2o(df2 %>% 
                                          filter(type=='Final') %>% 
                                          select(-ID_code, - type) %>%
                                          mutate(cluster = factor(cluster))))

df_out = df2 %>%
  filter(type == 'Final') %>%
  select(ID_code, target)

df_out$target = as.data.frame(tt)$p1

write_csv(df_out, '~/git/Kaggle_SCTP/Data/out1.csv')



