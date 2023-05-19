library(readxl)
library(car)
library(Metrics)
library(nortest)
library(doParallel)
library(ggpubr)
library(sf)
library(spdep)
library(caret)
library(spatialreg)

train=as.data.frame(read_excel('train.xlsx'))
test=as.data.frame(read_excel('test.xlsx'))
train_reg=as.data.frame(read_excel('train_reg.xlsx'))
test_reg=as.data.frame(read_excel('test_reg.xlsx'))
train_coord=as.data.frame(read_excel('train_coord.xlsx'))
test_coord=as.data.frame(read_excel('test_coord.xlsx'))

x_train=train_reg[,-1]
y_train=train_reg[,1]
x_test=test_reg[,-1]
y_test=test_reg[,1]


# linear model

fit_lm=train(buy_price~.,data=train,method='lmStepAIC',direction='both',
             k=log(nrow(train)),trControl=control,trace=FALSE)
fit_lm$results[which.min(fit_lm$results$RMSE),]
summary(fit_lm$finalModel)

#confint(fit_lm$finalModel)

set.seed(123)
fit_lm_complete=train(buy_price~.,data=train_reg,method='lmStepAIC',direction='both',
                      k=log(nrow(train_reg)),trControl=control,trace=FALSE)
#fit_lm_complete$results[which.min(fit_lm_complete$results$RMSE),]
#summary(fit_lm_complete$finalModel)
#confint(fit_lm_complete$finalModel)

par(mfrow=c(2,3))
plot(fit_lm_complete$finalModel,which=1:6) # diagnostiche

par(mfrow=c(1,1))
set.seed(123)
sub_res=sample(fit_lm_complete$finalModel$residuals,100)
hist(sub_res,main='Istogramma dei Residui',xlab='Residui',ylab='Frequenza',col='#F8766D')
shapiro.test(sub_res)
lillie.test(fit_lm_complete$finalModel$residuals)

data_num_log=train[,sapply(train,is.numeric)]
data_num_log=as.data.frame(cbind(data_num_log[,-1],log(train$buy_price)))
pairs(~.,data=data_num_log,main="Diagrammi Multipli") # scatterplots con log(y)


# log-linear model

set.seed(123)
fit_loglm=train(log(buy_price)~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2),
                data=train,method='lmStepAIC',direction='both',k=log(nrow(train)),
                trControl=control,trace=FALSE)
fit_loglm$results[which.min(fit_loglm$results$RMSE),]
summary(fit_loglm$finalModel)

set.seed(123)
fit_loglm_complete=train(log(buy_price)~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2),
                         data=train_reg,method='lmStepAIC',direction='both',k=log(nrow(train_reg)),
                         trControl=control,trace=FALSE)
fit_loglm_complete$results[which.min(fit_loglm_complete$results$RMSE),]
summary(fit_loglm_complete$finalModel)

cv_log_error=function(data,k,seed=123){ # funzione per cross-validation con log(y)
  set.seed(seed)
  yourdata=data[sample(nrow(data)),]
  fold=cut(seq(1,nrow(yourdata)),breaks=k,labels=FALSE)
  rmse_val=NULL
  mae_val=NULL
  r2_val=NULL
  for(i in 1:k){
    test_index=which(fold==i,arr.ind=TRUE)
    test_data=yourdata[test_index,]
    train_data=yourdata[-test_index,]
    null_m=lm(log(buy_price)~1,data=train_data)
    all_m=lm(log(buy_price)~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2),data=train_data)
    model=step(null_m,direction='both',scope=formula(all_m),k=log(nrow(train_data)),trace=FALSE)
    pred=predict(model,newdata=test_data[,-1])
    pred_exp=exp(pred)
    rmse_val[i]=rmse(test_data[,1],pred_exp)
    r2_val[i]=cor(test_data[,1],pred_exp)^2
    mae_val[i]=mae(test_data[,1],pred_exp)
  }
  rmse_mean=mean(rmse_val)
  r2_mean=mean(r2_val)
  mae_mean=mean(mae_val)
  cv_error=as.data.frame(cbind(rmse_mean,r2_mean,mae_mean))
  colnames(cv_error)=c('RMSE','Rsquared','MAE')
  return(cv_error)
}
cv_log_error(train,10)
cv_log_error(train_reg,10)

par(mfrow=c(2,3))
plot(fit_loglm_complete$finalModel,which=1:6)

par(mfrow=c(1,1))
set.seed(123)
sub_res_log=sample(fit_loglm_complete$finalModel$residuals,100)
hist(sub_res_log,main='Istogramma sui Residui con log(y)',xlab='Residui',ylab='Frequenza',col='#F8766D')
shapiro.test(sub_res_log)
lillie.test(fit_loglm_complete$finalModel$residuals)

vif(fit_lm$finalModel)
vif(fit_loglm$finalModel)
vif(fit_lm_complete$finalModel)
vif(fit_loglm_complete$finalModel)

c(AIC(fit_lm$finalModel),BIC(fit_lm$finalModel))
c(AIC(fit_lm_complete$finalModel),BIC(fit_lm_complete$finalModel))
c(AIC(fit_loglm$finalModel)+2*sum(log(train_reg$buy_price)),
  BIC(fit_loglm$finalModel)+2*sum(log(train_reg$buy_price)))
c(AIC(fit_loglm_complete$finalModel)+2*sum(log(train_reg$buy_price)),
  BIC(fit_loglm_complete$finalModel)+2*sum(log(train_reg$buy_price)))

## prevision error

### linear model

obs_lm=predict(fit_lm_complete,newdata=x_train) # train error
c(rmse(y_train,obs_lm),mae(y_train,obs_lm))

pred_lm=predict(fit_lm_complete,newdata=x_test) # test error
c(rmse(y_test,pred_lm),mae(y_test,pred_lm))

### log-linear model

obs_loglm=predict(fit_loglm_complete,newdata=x_train)
obs_loglm=exp(obs_loglm)
c(rmse(y_train,obs_loglm),mae(y_train,obs_loglm))

pred_loglm=predict(fit_loglm_complete,newdata=x_test)
pred_loglm=exp(pred_loglm)
c(rmse(y_test,pred_loglm),mae(y_test,pred_loglm))

# glm

set.seed(123)
fit_glm=train(buy_price~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2), ## Normale -> log
              data=train_reg,method='glmStepAIC',family=gaussian(link='log'),
              direction='both',k=log(nrow(train_reg)),trControl=control,trace=FALSE)
summary(fit_glm)
fit_glm

set.seed(123)
fit_glm_gamma=train(buy_price~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2), ## Gamma -> inverse
                    data=train_reg,method='glmStepAIC',family=Gamma(link='log'),
                    direction='both',k=log(nrow(train_reg)),trControl=control,trace=FALSE)
fit_glm_gamma
summary(fit_glm_gamma)

set.seed(123)
fit_glm_poisson=train(buy_price~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2), ## Poisson -> log
                      data=train_reg,method='glmStepAIC',family=poisson,
                      direction='both',k=log(nrow(train_reg)),trControl=control,trace=FALSE)
summary(fit_glm_poisson)
fit_glm_poisson

cv_log_gamma_error=function(data,k,seed=123){
  set.seed(seed)
  yourdata=data[sample(nrow(data)),]
  fold=cut(seq(1,nrow(yourdata)),breaks=k,labels=FALSE)
  rmse_val=NULL
  mae_val=NULL
  r2_val=NULL
  cc=trainControl('none',allowParallel=TRUE)
  for(i in 1:k){
    test_index=which(fold==i,arr.ind=TRUE)
    test_data=yourdata[test_index,]
    train_data=yourdata[-test_index,]
    model=train(log(buy_price)~.+I(sq_mt_built^2)+I(n_rooms^2)+I(n_bathrooms^2),
                data=train_data,method='glmStepAIC',family=Gamma,direction='both',
                k=log(nrow(train_data)),trControl=cc,trace=FALSE)
    pred=predict(model,newdata=test_data[,-1])
    pred=exp(pred)
    rmse_val[i]=rmse(test_data[,1],pred)
    mae_val[i]=mae(test_data[,1],pred)
    r2_val[i]=cor(test_data[,1],pred)^2
  }
  rmse_mean=mean(rmse_val)
  r2_mean=mean(r2_val)
  mae_mean=mean(mae_val)
  cv_error=as.data.frame(cbind(rmse_mean,r2_mean,mae_mean))
  colnames(cv_error)=c('RMSE','Rsquared','MAE')
  return(cv_error)
}
cv_log_gamma_error(train_reg,10)

par(mfrow=c(2,3))
plot(fit_glm$finalModel,which=1:6)
plot(fit_glm_gamma$finalModel,which=1:6)
plot(fit_glm_poisson$finalModel,which=1:6)

par(mfrow=c(1,1))
obs_glm=predict(fit_glm,newdata=x_train)
c(rmse(obs_glm,y_train),mae(obs_glm,y_train))

pred_glm=predict(fit_glm,newdata=x_test)
c(rmse(pred_glm,y_test),mae(pred_glm,y_test))

obs_glm_gamma=predict(fit_glm_gamma,newdata=x_train)
obs_glm_gamma=exp(obs_glm_gamma)
c(rmse(obs_glm_gamma,y_train),mae(obs_glm_gamma,y_train))

pred_glm_gamma=predict(fit_glm,newdata=x_test)
pred_glm=exp(pred_glm_gamma)
c(rmse(pred_glm_gamma,y_test),mae(pred_glm_gamma,y_test))

obs_glm_poisson=predict(fit_glm_poisson,newdata=x_train)
c(rmse(obs_glm_poisson,y_train),mae(obs_glm_poisson,y_train))

pred_glm_poisson=predict(fit_glm_poisson,newdata=x_test)
c(rmse(pred_glm_poisson,y_test),mae(pred_glm_poisson,y_test))


# shrinkage (Ridge, Lasso, ElasticNet)

## Ridge

set.seed(123)
tune_ridge=expand.grid(alpha=0,lambda=c(0,10^seq(log10(10000),log10(0.1),length.out=300)))
#tune_ridge=expand.grid(alpha=0,lambda=c(0,10^seq(log10(50000),log10(0.1),length.out=300)))
fit_ridge=train(buy_price~.,data=train_reg,method='glmnet',tuneGrid=tune_ridge,trControl=control)
plot_ridge=ggplot(fit_ridge) + geom_point(color='#F8766D') + geom_line(color='#F8766D') +
  ylim(205800,210500) + theme_minimal() + labs(x=expression(lambda),y='RMSE')
plot_ridge
#fit_ridge$bestTune
fit_ridge$results[which.min(fit_ridge$results$RMSE),]

obs_ridge=predict(fit_ridge,newdata=x_train)
c(rmse(y_train,obs_ridge),mae(y_train,obs_ridge))

pred_ridge=predict(fit_ridge,newdata=x_test)
c(rmse(y_test,pred_ridge),mae(y_test,pred_ridge))

## Lasso

set.seed(123)
tune_lasso=expand.grid(alpha=1,lambda=c(0,10^seq(log10(10000),log10(0.1),length.out=300)))
fit_lasso=train(buy_price~.,data=train_reg,method='glmnet',tuneGrid=tune_lasso,trControl=control)
plot_lasso=ggplot(fit_lasso) + geom_point(color='#F8766D') + geom_line(color='#F8766D') +
  ylim(205800,210500) + theme_minimal() + labs(x=expression(lambda),y='RMSE')
plot_lasso
fit_lasso$results[which.min(fit_lasso$results$RMSE),]
coef(fit_lasso$finalModel,fit_lasso$bestTune$lambda)

obs_lasso=predict(fit_lasso,newdata=x_train)
c(rmse(y_train,obs_lasso),mae(y_train,obs_lasso))

pred_lasso=predict(fit_lasso,newdata=x_test)
c(rmse(y_test,pred_lasso),mae(y_test,pred_lasso))

## ElasticNet
set.seed(123)
tune_elnet=expand.grid(alpha=seq(0,1,by=0.1),lambda=c(0,10^seq(log10(10000),log10(0.1),length.out=300)))
fit_elnet=train(buy_price~.,data=train_reg,method='glmnet',tuneGrid=tune_elnet,trControl=control)
plot_elnet=ggplot(fit_elnet) + ylim(205800,210500) + theme_minimal() + labs(x=expression(lambda),y='RMSE') +
  scale_color_discrete(name=expression(alpha)) + scale_shape_discrete(name=expression(alpha))
plot_elnet
fit_elnet$results[which.min(fit_elnet$results$RMSE),]
coef(fit_elnet$finalModel,fit_elnet$bestTune$lambda)

obs_elnet=predict(fit_elnet,newdata=x_train)
c(rmse(y_train,obs_elnet),mae(y_train,obs_elnet))

pred_elnet=predict(fit_elnet,x_test)
c(rmse(y_test,pred_elnet),mae(y_test,pred_elnet))

#reg_plot=ggarrange(ggarrange(plot_ridge,plot_lasso,ncol=2,labels=c('Ridge','Lasso')),
#                   plot_elnet,labels='ElasticNet',vjust=27,nrow=2)
#annotate_figure(reg_plot)


# non-parametric regression

## K-nearest neighbors (KNN)

set.seed(123)
fit_knn=train(buy_price~.,data=train_reg,preProcess=c('center','scale'),method='knn',
              tuneGrid=expand.grid(k=c(1,seq(2,50,4))),trControl=control)
ggplot(fit_knn) + theme_minimal() + labs(x='k',y='RMSE')
fit_knn$results[which.min(fit_knn$results$RMSE),]

obs_knn=predict(fit_knn,newdata=x_train)
c(rmse(y_train,obs_knn),mae(y_train,obs_knn))

pred_knn=predict(fit_knn,newdata=x_test)
c(rmse(y_test,pred_knn),mae(y_test,pred_knn))

## Multivariate Adaptive Regression Spline (MARS)

set.seed(123)
fit_mars=train(buy_price~.,data=train_reg,method='earth',
               tuneGrid=expand.grid(degree=1:3,nprune=seq(2,50,by=4)),trControl=control)
ggplot(fit_mars) + theme_minimal() + labs(x='k',y='RMSE') +
  scale_color_discrete(name='Grado') + scale_shape_discrete(name='Grado')
fit_mars$results[which.min(fit_mars$results$RMSE),]

obs_mars=predict(fit_mars,newdata=x_train)
c(rmse(y_train,obs_mars),mae(y_train,obs_mars))

pred_mars=predict(fit_mars,newdata=x_test)
c(rmse(y_test,pred_mars),mae(y_test,pred_mars))


# tree-based models

## decision tree

set.seed(123)
tune_tree=expand.grid(maxdepth=1:20)
fit_tree=train(buy_price~.,data=train_reg,method='rpart2',tuneGrid=tune_tree,trControl=control)
ggplot(fit_tree) + theme_minimal() + labs(x="Profondita' Massima",y='RMSE')
fit_tree$results[which.min(fit_tree$results$RMSE),]

obs_tree=predict(fit_tree,newdata=x_train)
c(rmse(y_train,obs_tree),mae(y_train,obs_tree))

pred_tree=predict(fit_tree,newdata=x_test)
c(rmse(y_test,pred_tree),mae(y_test,pred_tree))

## Random Forest

set.seed(123)
fit_rf_split=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                   num.trees=50,tuneLength=10,trControl=control)
ggplot(fit_rf_split) + theme_minimal() + labs(x='Variabili Selezionate',y='RMSE') +
  scale_color_discrete(name='Regola di Split',labels=c('Varianza','Extra Tree')) +
  scale_shape_discrete(name='Regola di Split',labels=c('Varianza','Extra Tree'))
fit_rf_split$results[which.min(fit_rf_split$results$RMSE),]

tune_rf=expand.grid(mtry=15,splitrule='variance',min.node.size=5)
fit_rf_n25=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                 num.trees=25,tuneGrid=tune_rf,trControl=control)
fit_rf_n50=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                 num.trees=50,tuneGrid=tune_rf,trControl=control)
fit_rf_n100=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                  num.trees=100,tuneGrid=tune_rf,trControl=control)
fit_rf_n150=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                  num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_n200=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                  num.trees=200,tuneGrid=tune_rf,trControl=control)
fit_rf_n500=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
                  num.trees=500,tuneGrid=tune_rf,trControl=control)

fit_rf_n25$results[which.min(fit_rf_n25$results$RMSE),]
fit_rf_n50$results[which.min(fit_rf_n50$results$RMSE),]
fit_rf_n100$results[which.min(fit_rf_n100$results$RMSE),]
fit_rf_n150$results[which.min(fit_rf_n150$results$RMSE),]
fit_rf_n200$results[which.min(fit_rf_n200$results$RMSE),]
fit_rf_n500$results[which.min(fit_rf_n500$results$RMSE),]

set.seed(123)
tune_rf=expand.grid(mtry=seq(5,40,5),splitrule='variance',min.node.size=seq(2,17,3))
fit_rf=train(buy_price~.,data=train_reg,method='ranger',importance='impurity',
             num.trees=150,tuneGrid=tune_rf,trControl=control)
ggplot(fit_rf) + theme_minimal() + labs(x='Variabili Selezionate',y='RMSE') +
  scale_color_discrete(name='Nodo Minimo') + scale_shape_discrete(name='Nodo Minimo')
fit_rf$results[which.min(fit_rf$results$RMSE),]

obs_rf=predict(fit_rf,newdata=x_train)
c(rmse(y_train,obs_rf),mae(y_train,obs_rf))

pred_rf=predict(fit_rf,newdata=x_test)
c(rmse(y_test,pred_rf),mae(y_test,pred_rf))

## XG-Boost
set.seed(123)
tune_xgb1=expand.grid(nrounds=seq(50,1000,50),max_depth=2:6,eta=c(0.025,0.05,0.1,0.3),gamma=0,
                      min_child_weight=1,subsample=1,colsample_bytree=1)
fit_xgb1=train(buy_price~.,data=train_reg,method='xgbTree',tuneGrid=tune_xgb1,trControl=control)
ggplot(fit_xgb1) + theme_bw() + labs(x='Iterazioni',y='RMSE') +
  scale_color_discrete(name="Profondita' Massima") + scale_shape_discrete(name="Profondita' Massima")
fit_xgb1$results[which.min(fit_xgb1$results$RMSE),]

set.seed(123)
tune_xgb2=expand.grid(nrounds=seq(50,1000,50),max_depth=4,eta=0.1,gamma=0,
                      min_child_weight=1:5,subsample=1,colsample_bytree=1)
fit_xgb2=train(buy_price~.,data=train_reg,method='xgbTree',tuneGrid=tune_xgb2,trControl=control)
ggplot(fit_xgb2) + theme_minimal() + labs(x='Iterazioni',y='RMSE') +
  scale_color_discrete(name='Peso Nodo Figlio') + scale_shape_discrete(name='Peso Nodo Figlio')
fit_xgb2$results[which.min(fit_xgb2$results$RMSE),]

set.seed(123)
tune_xgb3=expand.grid(nrounds=seq(50,1000,50),max_depth=4,eta=0.1,gamma=0,
                      min_child_weight=2,subsample=c(0.5,0.75,1),colsample_bytree=c(0.4,0.6,0.8,1))
fit_xgb3=train(buy_price~.,data=train_reg,method='xgbTree',tuneGrid=tune_xgb3,trControl=control)
ggplot(fit_xgb3) + theme_bw() + labs(x='Iterazioni',y='RMSE') +
  scale_color_discrete(name='Percentuale di Variabili') +
  scale_shape_discrete(name='Percentuale di Variabili')
fit_xgb3$results[which.min(fit_xgb3$results$RMSE),]

set.seed(123)
tune_xgb4=expand.grid(nrounds=seq(50,1000,50),max_depth=4,eta=0.1,gamma=c(0,1,10000),
                      min_child_weight=2,subsample=1,colsample_bytree=1)
fit_xgb4=train(buy_price~.,data=train_reg,method='xgbTree',tuneGrid=tune_xgb4,trControl=control)
fit_xgb4$results[which.min(fit_xgb4$results$RMSE),]
ggplot(fit_xgb4) + theme_minimal() + labs(x='Iterazioni',y='RMSE') +
  scale_color_discrete(name='Gamma') + scale_shape_discrete(name='Gamma')

set.seed(123)
tune_xgb5=expand.grid(nrounds=seq(500,10000,100),max_depth=4,eta=c(0.01,0.025,0.05,0.075,0.1),gamma=0,
                      min_child_weight=2,subsample=1,colsample_bytree=1)
fit_xgb5=train(buy_price~.,data=train_reg,method='xgbTree',tuneGrid=tune_xgb5,trControl=control)
ggplot(fit_xgb5) + theme_minimal() + labs(x='Iterazioni',y='RMSE') +
  scale_color_discrete(name='Learning Rate') + scale_shape_discrete(name='Learning Rate')
fit_xgb5$results[which.min(fit_xgb5$results$RMSE),]

obs_xgb=predict(fit_xgb5,newdata=x_train)
c(rmse(y_train,obs_xgb),mae(y_train,obs_xgb))

pred_xgb=predict(fit_xgb5,newdata=x_test)
c(rmse(y_test,pred_xgb),mae(y_test,pred_xgb))


# spatial model

k_neigh_75=knn2nb(knearneigh(train_coord,k=75,longlat=TRUE))
k_distance_75=nbdists(k_neigh_75,train_coord,longlat=TRUE)
inv_k_distance_75=lapply(k_distance_75, function(x) (1/(x+0.001)))

set.seed(123)
moran.mc(fit_lm$finalModel$residuals,k_weight_75,nsim=100,alternative='two.sided')
#lm.morantest(fit_lm$finalModel,k_weight_75,alternative='two.sided',resfun=residuals)

lm.LMtests(fit_lm$finalModel,k_weight_75,test=c('LMlag','LMerr'))
lm.LMtests(fit_lm$finalModel,k_weight_75,test=c('RLMlag','RLMerr'))

## Spatial Error Model (SEM)

fit_err=errorsarlm(buy_price~sq_mt_built+n_bathrooms+n_rooms+house_type_id+is_exterior+
                     is_new_development+has_ac+has_lift+has_garden+has_individual_heating,
                   data=train,listw=k_weight_75,method='MC',tol.solve=1e-30)
summary(fit_err)

### Diagnostics

y=fit_err$y
X=fit_err$X
fitted=X %*% fit_err$coefficients
residuals=y-fitted
H=X %*% solve(t(X) %*% X) %*% t(X)
variance=sum(residuals^2)/(nrow(X)-ncol(X))
stdres=sqrt(abs(residuals/sqrt(variance*(1-diag(H)))))
par(mfrow=c(1,3))
plot(fit_err$fitted.values,fit_err$residuals,xlab='Residuals',ylab='Fitted values',
     main='Residuals vs Fitted')
qqnorm(fit_err$residuals,ylab='Standardized residuals')
qqline(fit_err$residuals,col='red')
plot(fit_err$fitted.values,stdres,xlab='Fitted values',ylab='sqrt(Standardized residuals)',
     main='Scale - Location')

### prevision error

cv_error_sem=function(data,k,coord,v,seed=123){
  set.seed(seed)
  yourdata=data[sample(nrow(data)),]
  fold=cut(seq(1,nrow(yourdata)),breaks=k,labels=FALSE)
  rmse_val=NULL
  mae_val=NULL
  r2_val=NULL
  for(i in 1:k){
    test_index=which(fold==i,arr.ind=TRUE)
    test_data=yourdata[test_index,]
    train_data=yourdata[-test_index,]
    te_coord=coord[test_index,]
    tr_coord=coord[-test_index,]
    k_neigh_75=knn2nb(knearneigh(tr_coord,k=v,longlat=TRUE))
    k_distance_75=nbdists(k_neigh_75,tr_coord,longlat=TRUE)
    inv_k_distance_75=lapply(k_distance_75, function(x) (1/(x+0.001)))
    k_weight_75=nb2listw(k_neigh_75,glist=inv_k_distance_75,style='W')
    model=errorsarlm(buy_price~sq_mt_built+n_bathrooms+n_rooms+house_type_id+
                       is_exterior+is_new_development+has_ac+has_lift+has_garden+
                       has_individual_heating,data=train_data,
                     listw=k_weight_75,method="MC",tol.solve=1e-30)
    cc=as.data.frame(rbind(tr_coord,te_coord))
    k_neigh_75_test=knn2nb(knearneigh(cc,k=v,longlat=TRUE))
    k_distance_75_test=nbdists(k_neigh_75_test,cc,longlat=TRUE)
    inv_k_distance_75_t=lapply(k_distance_75_test, function(x) (1/(x+0.001)))
    k_weight_75_test=nb2listw(k_neigh_75_test,glist=inv_k_distance_75_t,style='W')
    pred=predict(model,newdata=test_data[,-1],listw=k_weight_75_test,pred.type='trend')
    rmse_val[i]=rmse(test_data[,1],pred)
    r2_val[i]=cor(test_data[,1],pred)^2
    mae_val[i]=mae(test_data[,1],pred)
  }
  rmse_mean=mean(rmse_val)
  r2_mean=mean(r2_val)
  mae_mean=mean(mae_val)
  cv_error=as.data.frame(cbind(rmse_mean,r2_mean,mae_mean))
  colnames(cv_error)=c('RMSE','Rsquared','MAE')
  return(cv_error)
}
cv_error_sem(train,10,train_coord,75) # errore di cross-validation

rmse(y_train,fit_err$fitted.values)
mae(y_train,fit_err$fitted.values)

cc=as.data.frame(rbind(train_coord,test_coord))

k_neigh_75_test=knn2nb(knearneigh(cc,k=75,longlat=TRUE))
k_distance_75_test=nbdists(k_neigh_75_test,cc,longlat=TRUE)
inv_k_distance_75_t=lapply(k_distance_75_test, function(x) (1/(x+0.001)))
k_weight_75_test=nb2listw(k_neigh_75_test,glist=inv_k_distance_75_t,style='W')

pred_err=predict(fit_err,newdata=x_test,listw=k_weight_75_test,pred.type='trend')

rmse(y_test,pred_err)
mae(y_test,pred_err)