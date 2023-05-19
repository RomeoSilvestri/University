library(readxl)
library(gclus)
library(mclust)
library(sf)
library(spdep)
library(ggplot2)
library(caret)
library(spatialreg)
library(doParallel)
library(writexl)

data=as.data.frame(read_excel('data_complete.xlsx'))
train=as.data.frame(read_excel('train.xlsx'))
test=as.data.frame(read_excel('test.xlsx'))

data_distance=as.data.frame(read_excel('data_distance.xlsx'))
train_distance=as.data.frame(read_excel('train_distance.xlsx'))
test_distance=as.data.frame(read_excel('test_distance.xlsx'))

train_district=as.data.frame(read_excel('train_district.xlsx'))
test_district=as.data.frame(read_excel('test_district.xlsx'))

data_coord=as.data.frame(read_excel('data_coord.xlsx'))
train_coord=as.data.frame(read_excel('train_coord.xlsx'))
test_coord=as.data.frame(read_excel('test_coord.xlsx'))


train[sapply(train,is.logical)]=lapply(train[sapply(train,is.logical)],as.factor)
train[sapply(train,is.character)]=lapply(train[sapply(train,is.character)],as.factor)
test[sapply(test,is.logical)]=lapply(test[sapply(test,is.logical)],as.factor)
test[sapply(test,is.character)]=lapply(test[sapply(test,is.character)],as.factor)

attach(train)


# univariate analysis

## response variable Y (buy_price)

summary(buy_price)
par(mfrow=c(1,3))
boxplot(buy_price,main='Boxplot',ylab='Prezzo di Vendita',col='lightblue')
hist(buy_price,nclass=15,main='Istogramma',xlab='Prezzo di Vendita',ylab='Densità',col='lightblue')
qqnorm(buy_price,main='Q-Q Plot Normale',xlab='Quantile Teorico',ylab='Quantile Osservato')
qqline(buy_price,col='red')

boxplot(log(buy_price),main='Boxplot',ylab='Prezzo di Vendita',col='#F8766D')
hist(log(buy_price),nclass=15,main='Istogramma',xlab='Prezzo di Vendita',ylab='Densità',col='#F8766D')
qqnorm(log(buy_price),main='Q-Q Plot Normale',xlab='Quantile Teorico',ylab='Quantile Osservato')
qqline(log(buy_price),col='red')

## numerical variables

par(mfrow=c(2,4))
hist(sq_mt_built,nclass=15,main='Superficie Costruita',
     xlab='Superficie',ylab='Densità',col='#F8766D')
plot(as.factor(floor),main='Piano',xlab='Piano',ylab='Frequenza',col='#7CAE00')
plot(as.factor(n_rooms),main='Numero di Stanze',xlab='Stanze',ylab='Frequenza',col='#00BFC4')
plot(as.factor(n_bathrooms),main='Numero di Bagni',xlab='Bagni',ylab='Frequenza',col='#C77CFF')
boxplot(sq_mt_built,ylab='Superficie',col='#F8766D')
boxplot(as.factor(floor),ylab='Piano',col='#7CAE00')
boxplot(as.factor(n_rooms),ylab='Stanze',col='#00BFC4')
boxplot(as.factor(n_bathrooms),ylab='Bagni',col='#C77CFF')

## categorical variables

par(mfrow=c(1,2))
plot(energy_certificate,xlab='Certificazione Energetica',ylab='Frequenza',col='#F8766D')
plot(house_type_id,xlab='Tipo di Casa',ylab='Frequenza',col='#00BFC4',horiz=TRUE)

par(mfrow=c(1,3))
plot(is_exterior,xlab='Esterno',ylab='Frequenza',col='#F8766D')
plot(is_renewal_needed,xlab='Necessaria Ristrutturazione',ylab='Frequenza',col='#00BA38')
plot(is_new_development,xlab='Nuova Costruzione',ylab='Frequenza',col=4)

## presence / absence

par(mfrow=c(3,3))
plot(has_ac,xlab='Aria Condizionata',ylab='Frequenza',col='#F8766D')
plot(has_fitted_wardrobes,xlab='Armadio a Muro',ylab='Frequenza',col='#D39200')
plot(has_lift,xlab='Ascensore',ylab='Frequenza',col='#93AA00')
plot(has_balcony,xlab='Balcone',ylab='Frequenza',col='#00BA38')
plot(has_garden,xlab='Giardino',ylab='Frequenza',col='#00C19F')
plot(has_parking,xlab='Parcheggio',ylab='Frequenza',col='#00B9E3')
plot(has_pool,xlab='Piscina',ylab='Frequenza',col='#619CFF')
plot(has_storage_room,xlab='Ripostiglio',ylab='Frequenza',col='#DB72FB')
plot(has_individual_heating,xlab='Riscaldamento Autonomo',ylab='Frequenza',col='#FF61C3')

## distance
par(mfrow=c(2,3))
hist(train_distance$d_supermarket,nclass=15,main='Supermercato',xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_hospital,nclass=15,main='Ospedale',xlab='Distanza',ylab='Frequenza',col='#00BA38')
hist(train_distance$d_pharmacy,nclass=15,main='Farmacia',xlab='Distanza',ylab='Frequenza',col='#619CFF')
boxplot(train_distance$d_supermarket,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_hospital,ylab='Distanza',col='#00BA38')
boxplot(train_distance$d_pharmacy,ylab='Distanza',col='#619CFF')
     
par(mfrow=c(2,2))
hist(train_distance$d_post,nclass=15,main='Ufficio Postale',xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_bank,nclass=15,main='Banca',xlab='Distanza',ylab='Frequenza',col='#00BFC4')
boxplot(train_distance$d_post,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_bank,ylab='Distanza',col='#00BFC4')

par(mfrow=c(2,3))
hist(train_distance$d_university,nclass=15,main='Università',
     xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_school,nclass=15,main="Scuola dell'Obbligo",
     xlab='Distanza',ylab='Frequenza',col='#00BA38')
hist(train_distance$d_kindergarten,nclass=15,main="Scuola dell'Infanzia",
     xlab='Distanza',ylab='Frequenza',col='#619CFF')
boxplot(train_distance$d_university,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_school,ylab='Distanza',col='#00BA38')
boxplot(train_distance$d_kindergarten,ylab='Distanza',col='#619CFF')

par(mfrow=c(2,3))
hist(train_distance$d_train,nclass=15,main='Stazione dei Treni',
     xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_bus,nclass=15,main='Stazione dei Bus',
     xlab='Distanza',ylab='Frequenza',col='#00BA38')
hist(train_distance$d_airport,nclass=15,main='Aeroporto',
     xlab='Distanza',ylab='Frequenza',col='#619CFF')
boxplot(train_distance$d_train,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_bus,ylab='Distanza',col='#00BA38')
boxplot(train_distance$d_airport,ylab='Distanza',col='#619CFF')

par(mfrow=c(2,3))
hist(train_distance$d_gym,nclass=15,main='Palestra',xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_park,nclass=15,main='Parco',xlab='Distanza',ylab='Frequenza',col='#00BA38')
hist(train_distance$d_stadium,nclass=15,main='Stadio',xlab='Distanza',ylab='Frequenza',col='#619CFF')
boxplot(train_distance$d_gym,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_park,ylab='Distanza',col='#00BA38')
boxplot(train_distance$d_stadium,ylab='Distanza',col='#619CFF')

par(mfrow=c(2,3))
hist(train_distance$d_disco,nclass=15,main='Discoteca',xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_cinema,nclass=15,main='Cinema',xlab='Distanza',ylab='Frequenza',col='#00BA38')
hist(train_distance$d_library,nclass=15,main='Biblioteca',xlab='Distanza',ylab='Frequenza',col='#619CFF')
boxplot(train_distance$d_disco,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_cinema,ylab='Distanza',col='#00BA38')
boxplot(train_distance$d_library,ylab='Distanza',col='#619CFF')

par(mfrow=c(2,2))
hist(train_distance$d_historic,nclass=15,main='Edificio Storico',
xlab='Distanza',ylab='Frequenza',col='#F8766D')
hist(train_distance$d_attraction,nclass=15,main='Attrazione Turistica',
xlab='Distanza',ylab='Frequenza',col='#00BFC4')
boxplot(train_distance$d_historic,ylab='Distanza',col='#F8766D')
boxplot(train_distance$d_attraction,ylab='Distanza',col='#00BFC4')


# bivariate analysis

## numeric-numeric (multiple scatterplots)

train_num=train[,sapply(train,is.numeric)]
corr=cor(train_num)
colors=dmat.color(abs(corr),breaks=c(0,0.3,0.7,1),col=c('#D2F4F2','#FDFFDA','#F4BBDD'))
colnames(train_num)=c('Prezzo di Vendita','Superficie Costruita','Numero di Bagni',
                      'Numero di Stanze',"Piano d'Ingresso")
cpairs(train_num,panel.colors=colors,gap=0.5,main='Diagrammi Multipli') # scatterplots
round(corr,3) # correlation

## Numeric-Factor

par(mfrow=c(1,2))
boxplot(buy_price~house_type_id,main='Prezzo e Tipo di Casa',xlab='Tipo di Casa',
        ylab='Prezzo di Vendita',names=c('Appartamento','Attico','Casa'),col=c(2,3,4))
summary(lm(buy_price~house_type_id))

boxplot(buy_price~energy_certificate,main='Prezzo e Certificazione Energetica',
        xlab='Certificazione Energetica',ylab='Prezzo di Vendita',names=c('Alta','Bassa'),col=c(2,3))
summary(lm(buy_price~energy_certificate))


# spatial exploration

## knearneigh
knear=knearneigh(train_coord,k=300,longlat=TRUE) # numero di vicini k fissato
x=train$buy_price
r=sapply(1:300,function(i){
  cor(x,x[knear$nn[,i]])
})

data.frame(k=1:300,r=r) %>% # correlazione al variare di k -> correlazione 0,3 per k=75
  ggplot(aes(x=k,y=r)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  xlab('k-esimo nearest neighbour') +
  ylab('Correlazione') +
  theme_light()

k_moran_I=c()  # Moran index
for (i in seq(5,100,5)){
  k_neigh=knn2nb(knearneigh(train_coord,k=i,longlat=TRUE))
  k_distance=nbdists(k_neigh,train_coord,longlat=TRUE)
  inv_k_distance=lapply(k_distance,function(x)(1/(x+1)))
  k_weight=nb2listw(k_neigh,glist=inv_k_distance,style='W')
  k_moran=moran.mc(train$buy_price,k_weight,nsim=100,alternative='two.sided')
  k_moran_I=c(k_moran_I,k_moran$statistic)
}
moran_I_k=data.frame(moran=k_moran_I,k=seq(5,100,5))

ggplot(moran_I_k,aes(x=k,y=moran)) +
  geom_point() +
  geom_line() +
  xlab('k') +
  ylab('Moran') +
  theme_light()


## dnearneigh

d_moran_I=c()  
for (d in seq(0.01,1,0.03)){
  d_neigh=dnearneigh(train_coord,0,d,longlat=TRUE)
  d_distance=nbdists(d_neigh,train_coord,longlat=TRUE)
  inv_d_distance=lapply(d_distance, function(x)(1/(x+1)))
  d_weight=nb2listw(d_neigh,glist=inv_d_distance,style='W',zero.policy=TRUE)
  d_moran=moran.mc(train$buy_price,d_weight,nsim=100,alternative='two.sided',zero.policy=TRUE)
  d_moran_I=c(d_moran_I,d_moran$statistic)
}
moran_I_d=data.frame(moran=d_moran_I,distance=seq(0.01,1,0.03))

ggplot(moran_I_d,aes(x=distance,y=moran)) +
  geom_point() +
  geom_line() +
  xlab('Distanza') +
  ylab('Moran') +
  theme_light()

k_neigh_75=knn2nb(knearneigh(train_coord,k=75,longlat=TRUE)) # k=75
k_distance_75=nbdists(k_neigh_75,train_coord,longlat=TRUE)
inv_k_distance_75=lapply(k_distance_75, function(x) (1/(x+0.001)))
k_weight_75=nb2listw(k_neigh_75,glist=inv_k_distance_75,style='W')

moran.plot(train$buy_price,k_weight_75,xlab='Prezzo di Vendita (Y)',
           ylab='Prezzo di Vendita con Lag Spaziale (WY)',main='Diagramma di Moran')
moran.mc(train$buy_price,k_weight_75,nsim=100,alternative='two.sided')
geary.mc(train$buy_price,k_weight_75,nsim=100,alternative='two.sided')

d_neigh_75=dnearneigh(train_coord,0,0.8,longlat=TRUE) # d=0,8 km -> mean neighbours=75
d_distance_75=nbdists(d_neigh_75,train_coord,longlat=TRUE)
inv_d_distance_75=lapply(d_distance_75, function(x) (1/(x+0.001)))
d_weight_75=nb2listw(d_neigh_75,glist=inv_d_distance_75,style='W',zero.policy=TRUE)
d_weight_75$neighbours
moran.mc(train$buy_price,d_weight_75,nsim=100,alternative='two.sided',zero.policy=TRUE)
geary.mc(train$buy_price,d_weight_75,nsim=100,alternative='two.sided',zero.policy=TRUE)


# spatial variables

## districts

train_c_district=as.data.frame(cbind(train,train_distance,train_district))
test_c_district=as.data.frame(cbind(test,test_distance,test_district))
colnames(train_c_district)[39]='district'
colnames(test_c_district)[39]='district'
train_c_district$district=as.factor(train_c_district$district)
test_c_district$district=as.factor(test_c_district$district)

## clusters

data_cluster=as.data.frame(cbind(data[,c(2,3,4,6)],data_coord))

set.seed(123)
fit_mm=mclustBIC(data_cluster,verbose=FALSE,G=1:25,initialization=list(hcRandomPairs(data_cluster)))
plot(fit_mm,xlab="Numero di cluster")
title(main="Selezione del Modello")
fit_mm

fit=Mclust(data_cluster,verbose=FALSE,G=15,modelNames='EEV',initialization=list(hcRandomPairs(data_cluster)))
summary(fit)

cl=fit$classification
cl_train=cl[data$ind_train]
cl_test=cl[data$ind_train==FALSE]

train_c_cluster=as.data.frame(cbind(train,train_distance,cl_train))
test_c_cluster=as.data.frame(cbind(test,test_distance,cl_test))
colnames(train_c_cluster)[39]='cluster'
colnames(test_c_cluster)[39]='cluster'
train_c_cluster$cluster=as.factor(train_c_cluster$cluster)
test_c_cluster$cluster=as.factor(test_c_cluster$cluster)

## lisa

data_lisa=as.data.frame(cbind(data$buy_price,data_coord))
colnames(data_lisa)[1]='buy_price'

x_lisa=data$buy_price
knear_lisa=knearneigh(data_coord,k=300,longlat=TRUE)

r_lisa=sapply(1:300,function(i){
  cor(x_lisa,x_lisa[knear_lisa$nn[,i]])
})

data.frame(k=1:300,r=r_lisa) %>%
  ggplot(aes(x=k,y=r_lisa)) +
  geom_line() +
  geom_smooth(se=FALSE) +
  xlab('k-esimo nearest neighbour') +
  ylab('Correlazione')


neigh_lisa=knn2nb(knearneigh(data_coord,100,longlat=TRUE))
k_distance_lisa=nbdists(neigh_lisa,data_coord,longlat=TRUE)
inv_k_distance_lisa=lapply(k_distance_lisa, function(x) (1/(x+1)))
k_weight_lisa=nb2listw(neigh_lisa,glist=inv_k_distance_lisa,style='W')

lisa=localmoran(data_lisa$buy_price,k_weight_lisa)
lisa=attributes(lisa)$quadr[,1]
summary(lisa)

lisa_data=as.data.frame(cbind(data,data_distance,lisa))
lisa_train=lisa[data$ind_train]
lisa_test=lisa[data$ind_train==FALSE]

train_c_lisa=as.data.frame(cbind(train,train_distance,lisa_train))
test_c_lisa=as.data.frame(cbind(test,test_distance,lisa_test))
colnames(train_c_lisa)[39]='lisa'
colnames(test_c_lisa)[39]='lisa'
train_c_lisa$lisa=factor(train_c_lisa$lisa)
train_c_lisa$lisa=relevel(train_c_lisa$lisa,ref='High-High')
test_c_lisa$lisa=as.factor(test_c_lisa$lisa)


### best spatial variables

cl=makePSOCKcluster(11) # calcolo parallelo con 11 cores
registerDoParallel(cl)

control=trainControl(method='cv',number=10,allowParallel=TRUE)
tune_rf=expand.grid(mtry=10,splitrule='variance',min.node.size=10)

set.seed(123)
fit_lm_district=train(buy_price~.,data=train_c_district,method='lmStepAIC',direction='both',
                      k=log(nrow(train_c_district)),trControl=control,trace=FALSE)
summary(fit_lm_district$finalModel)
fit_lm_district$results[which.min(fit_lm_district$results$RMSE),]
fit_rf_district=train(buy_price~.,data=train_c_district,method='ranger',
                      importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_district$results[which.min(fit_rf_district$results$RMSE),]

set.seed(123)
fit_lm_cluster=train(buy_price~.,data=train_c_cluster,method='lmStepAIC',direction='both',
                     k=log(nrow(train_c_cluster)),trControl=control,trace=FALSE)
summary(fit_lm_cluster$finalModel)
fit_lm_cluster$results[which.min(fit_lm_cluster$results$RMSE),]
fit_rf_cluster=train(buy_price~.,data=train_c_cluster,method='ranger',
                     importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_cluster$results[which.min(fit_rf_cluster$results$RMSE),]

set.seed(123)
fit_lm_lisa=train(buy_price~.,data=train_c_lisa,method='lmStepAIC',direction='both',
                  k=log(nrow(train_c_lisa)),trControl=control,trace=FALSE)
summary(fit_lm_lisa$finalModel)
fit_lm_lisa$results[which.min(fit_lm_lisa$results$RMSE),]
fit_rf_lisa=train(buy_price~.,data=train_c_lisa,method='ranger',
                  importance='impurity',num.trees=150,tuneGrid=tune_rf,trControl=control)
fit_rf_lisa$results[which.min(fit_rf_lisa$results$RMSE),]

#write_xlsx(lisa_data,'data_reg.xlsx')
#write_xlsx(train_c_lisa,'train_reg.xlsx')
#write_xlsx(test_c_lisa,'test_reg.xlsx')