library(readxl)
library(caret)
library(mice)
library(Hmisc)
library(Iscores)
library(data.table)
library(mltools)
library(writexl)

data=read_excel('data_incomplete.xlsx')
data_coord=read_excel('data_coord.xlsx')
data_distance=read_excel('data_distance.xlsx')
data_district=read_excel('data_district.xlsx')

data=as.data.frame(data)
data_coord=as.data.frame(data_coord)
data_distance=as.data.frame(data_distance)
data_district=as.data.frame(data_district)
str(as.factor(data_district$district))

data[sapply(data,is.logical)]=lapply(data[sapply(data,is.logical)],as.factor)
data[sapply(data,is.character)]=lapply(data[sapply(data,is.character)],as.factor)

data$energy_certificate=factor(data$energy_certificate)
data$energy_certificate=relevel(data$energy_certificate,ref='Low')

data$house_type_id=factor(data$house_type_id)
data$house_type_id=relevel(data$house_type_id,ref='Apartment')

str(data)


## data splitting

set.seed(123)

sample=createDataPartition(data$buy_price,p=0.8,groups=10,list=FALSE)  # stratified data splitting based on y quantiles

ind_train=rep(FALSE,dim(data)[1])
ind_train[sample]=TRUE
ind_train

train=data[sample,]
test=data[-sample,]

train_coord=data_coord[sample,]
test_coord=data_coord[-sample,]

train_distance=data_distance[sample,]
test_distance=data_distance[-sample,]

train_district=as.data.frame(data_district[sample,])
test_district=as.data.frame(data_district[-sample,])

c(dim(train)[1],dim(test)[1])


# missing data

round(colSums(is.na(train))/nrow(train),3)*100
round(colSums(is.na(test))/nrow(test),3)*100

#soglia

#50%
train_50=subset(train,select=-c(sq_mt_useful,sq_mt_allotment,n_floors,built_year,is_orientation_north,
                                is_orientation_south,is_orientation_west,is_orientation_east))
imp_50=mice(train_50,m=1,method='pmm',visitSequence='monotone',seed=123)
train_50_complete=complete(imp_50,1)

#25%
train_25=subset(train_50,select=-c(energy_certificate,has_individual_heating))
imp_25=mice(train_25,m=1,method='pmm',visitSequence='monotone',seed=123)
train_25_complete=complete(imp_25,1)

#5%
train_5=subset(train_25,select=-c(floor,is_exterior,has_lift))
imp_5=mice(train_5,m=1,method='pmm',visitSequence='monotone',seed=123)
train_5_complete=complete(imp_5,1)


set.seed(123)
control=trainControl(method='cv',number=10)
tune_rf=expand.grid(mtry=10,splitrule='variance',min.node.size=10)

fit_50=train(buy_price~.,data=train_50_complete,method='lmStepAIC',direction='both',
             k=log(nrow(train_50_complete)),trControl=control)
fit_50

fit_rf_50=train(buy_price~.,data=train_50_complete,method='ranger',importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_50


fit_25=train(buy_price~.,data=train_25_complete,method='lmStepAIC',direction='both',
             k=log(nrow(train_25_complete)),trControl=control)
fit_25

fit_rf_25=train(buy_price~.,data=train_25_complete,method='ranger',importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_25

fit_5=train(buy_price~.,data=train_5_complete,method='lmStepAIC',direction='both',
            k=log(nrow(train_5_complete)),trControl=control)
fit_5

fit_rf_5=train(buy_price~.,data=train_5_complete,method='ranger',importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_5

summary(fit_50$finalModel)
varImp(fit_50)
varImp(fit_rf_50)

train=train_50
test=subset(test,select=-c(sq_mt_useful,sq_mt_allotment,n_floors,built_year,is_orientation_north,
                           is_orientation_south,is_orientation_west,is_orientation_east))


## missing at random (MAR)

### single imputation

train_mmm=train
train_mmm$sq_mt_built=as.numeric(impute(train_mmm$sq_mt_built,mean))
train_mmm$n_bathrooms=as.numeric(impute(train_mmm$n_bathrooms,mean))
train_mmm$floor=as.numeric(impute(train_mmm$floor,mean))

mode=function(x) {
  x=na.omit(x)
  uniq=unique(x)
  uniq[which.max(tabulate(match(x,uniq)))]
}

train_mmm$is_floor_under[is.na(train_mmm$is_floor_under)]=mode(train_mmm$is_floor_under)
train_mmm$has_lift[is.na(train_mmm$has_lift)]=mode(train_mmm$has_lift)
train_mmm$has_individual_heating[is.na(train_mmm$has_individual_heating)]=mode(train_mmm$has_individual_heating)
train_mmm$is_new_development[is.na(train_mmm$is_new_development)]=mode(train_mmm$is_new_development)
train_mmm$is_exterior[is.na(train_mmm$is_exterior)]=mode(train_mmm$is_exterior)
train_mmm$energy_certificate[is.na(train_mmm$energy_certificate)]=mode(train_mmm$energy_certificate)


imp_lm=mice(train,m=1,defaultMethod=c('norm.predict','logreg','polyreg','polr'),visitSequence='monotone',seed=123)
train_lm=complete(imp_lm,1)

imp_sr=mice(train,m=1,defaultMethod=c('norm.nob','logreg','polyreg','polr'),visitSequence='monotone',seed=123)
train_sr=complete(imp_sr,1)

imp_default=mice(train,visitSequence='monotone',m=1,seed=123)
train_default=complete(imp_default,1)

imp_pmm=mice(train,m=1,method='pmm',visitSequence='monotone',seed=123)
train_pmm=complete(imp_pmm,1)

imp_cart=mice(train,m=1,method='cart',visitSequence='monotone',seed=123)
train_cart=complete(imp_cart,1)

imp_rf=mice(train,m=1,method='rf',visitSequence='monotone',seed=123)
train_rf=complete(imp_rf,1)

tr_imp=list()
tr_imp[[1]]=train_mmm
tr_imp[[2]]=train_lm
tr_imp[[3]]=train_sr
tr_imp[[4]]=train_default
tr_imp[[5]]=train_pmm
tr_imp[[6]]=train_cart
tr_imp[[7]]=train_rf

imputations=list()

for(i in 1:7){
  imputations[[i]]=lapply(1,function(j){
    newdata=one_hot(as.data.table(tr_imp[i]))
    return(newdata)
  })
}

train_hot=one_hot(as.data.table(train))

#### I-Scores 1

set.seed(123)
methods=c('MMM','LM+LOG','SR+LOG','PMM+LOG','PMM','CART','RF')
IS=Iscores(imputations,methods,train_hot,num.proj=5)
#Iscores(imputations,methods,train_hot,num.proj=10)
#Iscores(imputations,methods,train_hot,num.proj=20)
#Iscores(imputations,methods,train_hot,num.proj=50)


## multiple imputation

imp_lm_m=mice(train,m=5,defaultMethod=c('norm.predict','logreg','polyreg','polr'),visitSequence='monotone',seed=123)
train_lm_m=list()
for(i in 1:5){
  train_lm_m[[i]]=complete(imp_lm_m,i)
}

imp_sr_m=mice(train,m=5,defaultMethod=c('norm.nob','logreg','polyreg','polr'),visitSequence='monotone',seed=123)
train_sr_m=list()
for(i in 1:5){
  train_sr_m[[i]]=complete(imp_sr_m,i)
}

imp_default_m=mice(train,m=5,visitSequence='monotone',seed=123)
train_default_m=list()
for(i in 1:5){
  train_default_m[[i]]=complete(imp_default_m,i)
}

imp_pmm_m=mice(train,m=5,method='pmm',visitSequence='monotone',seed=123)
train_pmm_m=list()
for(i in 1:5){
  train_pmm_m[[i]]=complete(imp_pmm_m,i)
}

imp_cart_m=mice(train,m=5,method='cart',visitSequence='monotone',seed=123)
train_cart_m=list()
for(i in 1:5){
  train_cart_m[[i]]=complete(imp_cart_m,i)
}

imp_rf_m=mice(train,m=5,method='rf',visitSequence='monotone',seed=123)
train_rf_m=list()
for(i in 1:5){
  train_rf_m[[i]]=complete(imp_rf_m,i)
}


imputations=list()

imputations[[1]]=lapply(1:5,function(i){
  newdata=one_hot(as.data.table(train_lm_m[[i]]))
  return(newdata)
})

imputations[[2]]=lapply(1:5,function(i){
  newdata=one_hot(as.data.table(train_sr_m[[i]]))
  return(newdata)
})

imputations[[3]]=lapply(1:5,function(i){
  newdata=one_hot(as.data.table(train_default_m[[i]]))
  return(newdata)
})

imputations[[4]]=lapply(1:5,function(i){
  newdata=one_hot(as.data.table(train_pmm_m[[i]]))
  return(newdata)
})

imputations[[5]]=lapply(1:5,function(i){
  newdata=one_hot(as.data.table(train_cart_m[[i]]))
  return(newdata)
})

imputations[[6]]=lapply(1:5, function(i) {
  newdata=one_hot(as.data.table(train_rf_m[[i]]))
  return(newdata)
})

#### I-Scores 2

set.seed(123)
methods=c('LM+LOG','SR+LOG','PMM+LOG','PMM','CART','RF')
Iscores(imputations,methods,train_hot,num.proj=5,m=5)
Iscores(imputations,methods,train_hot,num.proj=10,m=5)
Iscores(imputations,methods,train_hot,num.proj=20,m=5)
Iscores(imputations,methods,train_hot,num.proj=50,m=5)


## selection of the method

train=train_rf
summary(train)

set.seed(123)
pred_m=matrix(1,nrow=length(test),ncol=length(test))
diag(pred_m)=0
pred_m[1,]=0
pred_m[,1]=0
imp_rf_test=mice(test,m=1,method='rf',visitSequence='monotone',predictorMatrix=pred_m,seed=123)
test_rf=complete(imp_rf_test,1)
test=test_rf

data=subset(data,select=-c(sq_mt_useful,sq_mt_allotment,n_floors,built_year,is_orientation_north,
                           is_orientation_south,is_orientation_west,is_orientation_east))
data[sample,]=train
data[-sample,]=test
data=as.data.frame(cbind(data,ind_train))
summary(data)

#write_xlsx(data,'data_complete1.xlsx')
#write_xlsx(train,'train1.xlsx')
#write_xlsx(test,'test1.xlsx')
#write_xlsx(train_coord,'train_coord.xlsx')
#write_xlsx(test_coord,'test_coord.xlsx')
#write_xlsx(train_distance,'train_distance.xlsx')
#write_xlsx(test_distance,'test_distance.xlsx')
#write_xlsx(train_district,'train_district.xlsx')
#write_xlsx(test_district,'test_district.xlsx')