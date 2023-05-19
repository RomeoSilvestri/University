library(readr)
library(stringr)
library(Hmisc)
library(tidygeocoder)
library(writexl)

data=read_csv('houses_Madrid.csv')
data=as.data.frame(data)
data=subset(data,!is.na(data$street_number))
View(data)

data=subset(data,select=c(buy_price,buy_price_by_area,rent_price,parking_price,sq_mt_built,
                          sq_mt_useful,sq_mt_allotment,built_year,n_bathrooms,n_floors,n_rooms,
                          energy_certificate,floor,house_type_id,title,subtitle,raw_address,
                          street_name,street_number,neighborhood_id,is_accessible,is_exterior,
                          is_exact_address_hidden,is_renewal_needed,is_new_development,
                          is_parking_included_in_price,is_floor_under,is_orientation_north,
                          is_orientation_south,is_orientation_east,is_orientation_west,
                          has_ac,has_fitted_wardrobes,has_lift,has_balcony,has_garden,
                          has_parking,has_pool,has_storage_room,has_individual_heating,
                          has_central_heating,has_terrace,has_green_zones))

data=unique(data)
data=data[-c(5511,5989,6180),]
rownames(data)=1:nrow(data)

summary(data)
dim(data)


# management of variables

# 1) gestione delle variabili di prezzo
# 2) trasformazione delle variabili categoriali
# 3) gestione delle variabili di posizione
# 4) eliminazione delle variabili superflue


##1

for(i in 1:nrow(data)){
  if(!is.na(data$is_parking_included_in_price[i])&data$is_parking_included_in_price[i]==FALSE){
    data$buy_price[i]=data$buy_price[i]+data$parking_price[i]
  }
}


##2

### Certificazione Energetica

energy_certificate_factor=factor(data$energy_certificate)
energy_certificate_factor=relevel(energy_certificate_factor,ref='en trámite')
summary(energy_certificate_factor)
boxplot(data$buy_price~energy_certificate_factor,main='Boxplot',xlab='Energy Certificate',ylab='Buy Price')
summary(lm(data$buy_price~energy_certificate_factor))
summary(lm(data$buy_price~energy_certificate_factor+data$sq_mt_built))

data$energy_certificate[data$energy_certificate=='A'|data$energy_certificate=='B'|
                          data$energy_certificate=='C']='High'
data$energy_certificate[data$energy_certificate=='D'|data$energy_certificate=='E'|
                          data$energy_certificate=='F'|data$energy_certificate=='G'|
                          data$energy_certificate=='inmueble exento'|data$energy_certificate=='en trámite']='Low'
data$energy_certificate[data$energy_certificate=='no indicado']=NA
summary(as.factor(data$energy_certificate))
boxplot(data$buy_price~as.factor(data$energy_certificate),col=c('#F8766D','#00BFC4'))
summary(lm(buy_price~as.factor(energy_certificate)+sq_mt_built,data=data))

### Piano

summary(as.factor(data$floor))
data$floor[data$floor=='Entreplanta'|data$floor=='Entreplanta exterior'|data$floor=='Entreplanta interior']='0.5'
data$floor[data$floor=='Bajo']='0'
data$floor[data$floor=='Semi-sótano'|data$floor=='Semi-sótano exterior'|data$floor=='Semi-sótano interior']='-0.5'
data$floor[data$floor=='Sótano'|data$floor=='Sótano exterior'|data$floor=='Sótano interior']='-1'
data$floor=as.numeric(data$floor)
summary(data$floor)

### Tipo di Casa

data_house_type=str_split_fixed(data$title,' ',2)[,1]
summary(as.factor(data_house_type))

for(i in 1:nrow(data)){
  if(is.na(data$house_type_id[i])){
    data$house_type_id[i]=data_house_type[i]
  }
}

house_type_factor=factor(data$house_type_id)
house_type_factor=relevel(house_type_factor,ref='HouseType 1: Pisos')
summary(house_type_factor)
boxplot(data$buy_price~house_type_factor)
summary(lm(data$buy_price~house_type_factor))
summary(lm(data$buy_price~house_type_factor+data$sq_mt_built))

data$house_type_id[data$house_type_id=='HouseType 1: Pisos'|data$house_type_id=='Estudio'|
                     data$house_type_id=='HouseType 4: Dúplex']='Apartment'
data$house_type_id[data$house_type_id=='HouseType 2: Casa o chalet'|data$house_type_id=='Casa'|
                     data$house_type_id=='Finca']='Independent'
data$house_type_id[data$house_type_id=='HouseType 5: Áticos']='Attic'
summary(as.factor(data$house_type_id))
boxplot(data$buy_price~as.factor(data$house_type_id),xlab='House Type',ylab='Buy Price',
        col=c('#F8766D','#00BA38','#619CFF'))
summary(lm(buy_price~as.factor(house_type_id)+sq_mt_built,data=data))

### Balcone

summary(as.factor(data$has_balcony))
summary(as.factor(data$has_terrace))
data$has_balcony[data$has_balcony==TRUE|data$has_terrace==TRUE]='TRUE'
summary(as.factor(data$has_balcony))

### Giardino

summary(as.factor(data$has_garden))
summary(as.factor(data$has_green_zones))
data$has_garden[data$has_garden==TRUE|data$has_green_zones==TRUE]='TRUE'
summary(as.factor(data$has_garden))

### Riscaldamento

summary(as.factor(data$has_central_heating))
summary(as.factor(data$has_individual_heating))


##3

data_ng=as.data.frame(str_split_fixed(data$neighborhood_id,': ',3))
data_neighborhood=str_split_fixed(data_ng[,1],' ',2)[,2]
data_district=str_split_fixed(data_ng[,2],'District ',2)[,2]
summary(as.factor(data_neighborhood))
summary(as.factor(data_district))
data_district=as.data.frame(district)

data_address=matrix(NA,nrow=nrow(data))
for(i in 1:nrow(data)){
  if(!is.na(data$raw_address[i])){
    data_address[i,1]=paste(data$raw_address[i],data$subtitle[i],sep=', ')
  }
}
data_address=as.data.frame(data_address)
colnames(data_address)='address'
head(data_address)


### 4

data=subset(data,select=-c(buy_price_by_area,rent_price,parking_price,is_parking_included_in_price,
                           is_floor_under,has_central_heating,title,subtitle,raw_address,street_name,
                           street_number,neighborhood_id,is_exact_address_hidden,has_terrace,has_green_zones))
dim(data)


#code errors

round(colSums(is.na(data))/nrow(data),3)*100
summary(data)

data$has_ac=impute(data$has_ac,FALSE)
data$has_fitted_wardrobes=impute(data$has_fitted_wardrobes,FALSE)
data$has_balcony=impute(data$has_balcony,FALSE)
data$has_garden=impute(data$has_garden,FALSE)
data$has_storage_room=impute(data$has_storage_room,FALSE)
data$has_pool=impute(data$has_pool,FALSE)

data=subset(data,select=-is_accessible)

round(colSums(is.na(data))/nrow(data),3)*100


#geocode

data_geo=geo(address=data_address$address,method='arcgis',lat=latitude,long=longitude) # 45 minutes 
data_geo=as.data.frame(data_geo)
head(data_geo)

data_coord=data_geo[,c(3,2)]
colnames(data_coord)=c('x','y')
summary(data_coord)


#write_xlsx(data,'data_incomplete.xlsx')
#write_xlsx(data_coord,'data_coord.xlsx')
#write_xlsx(data_district,'data_district.xlsx')