library(readxl)
library(osmdata)
library(ggplot2)
library(dvmisc)
library(viridis)
library(sf)
library(ggpubr)
library(geosphere)
library(writexl)

data_coord=read_excel('data_coord.xlsx')
data_coord=as.data.frame(data_coord)
data=read_excel('data_incomplete.xlsx')
data=as.data.frame(data)

# map of Madrid

## streets

street_major=getbb(place_name='Madrid') %>%
  opq(timeout=100) %>%
  add_osm_feature(key='highway',value=c('motorway','trunk','primary')) %>%
  osmdata_sf()
street_major

street_minor=getbb(place_name='Madrid') %>%
  opq(timeout=100) %>%
  add_osm_feature(key='highway',value=c('secondary','tertiary')) %>%
  osmdata_sf()
street_minor

street_map=ggplot()+
  geom_sf(data=street_major$osm_lines,inherit.aes=FALSE,color='black',size=0.2) +
  geom_sf(data=street_minor$osm_lines,inherit.aes=FALSE,color='black',size=0.1) +
  theme_void()
street_map

buy_price_q=quant_groups(data$buy_price, groups = 4)
buy_price_q=as.factor(buy_price_q)
levels(buy_price_q)=c('Low','Medium Low','Medium High','High')

y_limit=c(40.3,40.55)  # buy price map
street_map +
  geom_point(data=data_coord,aes(x=x, y=y,colour=buy_price_q),size=1.5) +
  scale_color_viridis(discrete=TRUE)+
  coord_sf(ylim=y_limit,expand=FALSE) +
  labs(color='Buy Price')


## first necessity / healthcare

supermarket_madrid=getbb(place_name='Madrid') %>%  # supermarket
  opq(timeout=100) %>%
  add_osm_feature(key='shop',value='supermarket') %>%
  osmdata_sf()
supermarket_madrid

coord_supermarket=supermarket_madrid['osm_points']
coord_supermarket=do.call(rbind,coord_supermarket) %>% dplyr::select('osm_id','geometry')
coord_supermarket=st_coordinates(coord_supermarket)


hospital_madrid=getbb(place_name='Madrid') %>%  # hospital
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='hospital') %>%
  osmdata_sf()
hospital_madrid

coord_hospital=hospital_madrid['osm_points']
coord_hospital=do.call(rbind,coord_hospital) %>% dplyr::select('osm_id','geometry')
coord_hospital=st_coordinates(coord_hospital)


pharmacy_madrid=getbb(place_name='Madrid') %>%  # pharmacy
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='pharmacy') %>%
  osmdata_sf()
pharmacy_madrid

coord_pharmacy=pharmacy_madrid['osm_points']
coord_pharmacy=do.call(rbind,coord_pharmacy) %>% dplyr::select('osm_id','geometry')
coord_pharmacy=st_coordinates(coord_pharmacy)


supermarket_map=street_map + 
  geom_sf(data=supermarket_madrid$osm_points,color=2,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

hospital_map=street_map + 
  geom_sf(data=hospital_madrid$osm_polygons,color=3,size=1,fill=3,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

pharmacy_map=street_map + 
  geom_sf(data=pharmacy_madrid$osm_points,color=4,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

health_maps=ggarrange(supermarket_map,hospital_map,pharmacy_map,
                      labels=c('Supermarket','Hospital','Pharmacy'),vjust=10,nrow=2)
annotate_figure(health_maps,top=text_grob('Healthcare',color='blue',size=20,vjust=4))


## finance

post_madrid=getbb(place_name='Madrid') %>%  # post office
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='post_office') %>%
  osmdata_sf()
post_madrid 

coord_post=post_madrid['osm_points']
coord_post=do.call(rbind,coord_post) %>% dplyr::select('osm_id','geometry')
coord_post=st_coordinates(coord_post)


bank_madrid=getbb(place_name='Madrid') %>%  # bank
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='bank') %>%
  osmdata_sf()
bank_madrid

coord_bank=bank_madrid['osm_points']
coord_bank=do.call(rbind,coord_bank) %>% dplyr::select('osm_id','geometry')
coord_bank=st_coordinates(coord_bank)


post_map=street_map + 
  geom_sf(data=post_madrid$osm_points,color=2,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

bank_map=street_map + 
  geom_sf(data=bank_madrid$osm_points,color=3,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

finance_map=ggarrange(post_map,bank_map,labels=c('Post Office','Bank'),vjust=3)
annotate_figure(finance_map,top=text_grob('Finance',color='blue',size=20,vjust=1))


## education

university_madrid=getbb(place_name='Madrid') %>%  # university
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='university') %>%
  osmdata_sf()
university_madrid

coord_university=university_madrid['osm_points']
coord_university=do.call(rbind,coord_university) %>% dplyr::select('osm_id','geometry')
coord_university=st_coordinates(coord_university)


school_madrid=getbb(place_name='Madrid') %>%  # primary, middle and secondary school
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='school') %>%
  osmdata_sf()
school_madrid

coord_school=school_madrid['osm_points']
coord_school=do.call(rbind,coord_school) %>% dplyr::select('osm_id','geometry')
coord_school=st_coordinates(coord_school)


kindergarten_madrid=getbb(place_name='Madrid') %>%  # kindergarten
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='kindergarten') %>%
  osmdata_sf()
kindergarten_madrid

coord_kindergarten=kindergarten_madrid['osm_points']
coord_kindergarten=do.call(rbind,coord_kindergarten) %>% dplyr::select('osm_id','geometry')
coord_kindergarten=st_coordinates(coord_kindergarten)


university_map=street_map + 
  geom_sf(data=university_madrid$osm_polygons,color=2,fill=2,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

school_map=street_map + 
  geom_sf(data=school_madrid$osm_points,color=3,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

kindergarten_map=street_map + 
  geom_sf(data=kindergarten_madrid$osm_points,color=4,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

education_map=ggarrange(university_map,school_map,kindergarten_map,
                        labels=c('University','School','Kindergarten'),vjust=10,nrow=1)
annotate_figure(education_map,top=text_grob('Finance',color='blue',size=20,vjust=4))


## transport

train_madrid=getbb(place_name='Madrid') %>%  # train station
  opq(timeout=100) %>%
  add_osm_feature(key='building',value='train_station') %>%
  osmdata_sf()
train_madrid 

coord_train=train_madrid['osm_points']
coord_train=do.call(rbind,coord_train) %>% dplyr::select('osm_id','geometry')
coord_train=st_coordinates(coord_train)


bus_madrid=getbb(place_name='Madrid') %>%  # bus station
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='bus_station') %>%
  osmdata_sf()
bus_madrid 

coord_bus=bus_madrid['osm_points']
coord_bus=do.call(rbind,coord_bus) %>% dplyr::select('osm_id','geometry')
coord_bus=st_coordinates(coord_bus)


airport_madrid=getbb(place_name='Madrid') %>%  # airport
  opq(timeout=100) %>%
  add_osm_feature(key='aeroway',value='aerodrome') %>%
  osmdata_sf()
airport_madrid 

coord_airport=airport_madrid['osm_points']
coord_airport=do.call(rbind,coord_airport) %>% dplyr::select('osm_id','geometry')
coord_airport=st_coordinates(coord_airport)


train_map=street_map + 
  geom_sf(data=train_madrid$osm_points,color='#F8766D',size=2,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

bus_map=street_map + 
  geom_sf(data=bus_madrid$osm_points,color='#00BA38',size=2,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

airport_map=street_map + 
  geom_sf(data=airport_madrid$osm_polygons,color='#619CFF',fill='#619CFF',inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

transport_map=ggarrange(ggarrange(train_map,bus_map,ncol=2,labels=c('Train Station','Bus Station')),
                        airport_map,labels='Airport',nrow=2,vjust=25,hjust=-11.5)
annotate_figure(transport_map,top=text_grob('Transport',color='blue',size=20))


## entertainment

gym_madrid=getbb(place_name='Madrid') %>%  # gym
  opq(timeout=100) %>%
  add_osm_feature(key='leisure',value='fitness_centre') %>%
  osmdata_sf()
gym_madrid #

coord_gym=gym_madrid['osm_points']
coord_gym=do.call(rbind,coord_gym) %>% dplyr::select('osm_id','geometry')
coord_gym=st_coordinates(coord_gym)


library_madrid=getbb(place_name='Madrid') %>%  # library
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='library') %>%
  osmdata_sf()
library_madrid 

coord_library=library_madrid['osm_points']
coord_library=do.call(rbind,coord_library) %>% dplyr::select('osm_id','geometry')
coord_library=st_coordinates(coord_library)


park_madrid=getbb(place_name='Madrid') %>%  # park
  opq(timeout=100) %>%
  add_osm_feature(key='leisure',value='park') %>%
  osmdata_sf()
park_madrid 

coord_park=park_madrid['osm_points']
coord_park=do.call(rbind,coord_park) %>% dplyr::select('osm_id','geometry')
coord_park=st_coordinates(coord_park)


disco_madrid=getbb(place_name='Madrid') %>%  # disco
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='nightclub') %>%
  osmdata_sf()
disco_madrid 

coord_disco=disco_madrid['osm_points']
coord_disco=do.call(rbind,coord_disco) %>% dplyr::select('osm_id','geometry')
coord_disco=st_coordinates(coord_disco)


cinema_madrid=getbb(place_name='Madrid') %>%  # cinema
  opq(timeout=100) %>%
  add_osm_feature(key='amenity',value='cinema') %>%
  osmdata_sf()
cinema_madrid 

coord_cinema=cinema_madrid['osm_points']
coord_cinema=do.call(rbind,coord_cinema) %>% dplyr::select('osm_id','geometry')
coord_cinema=st_coordinates(coord_cinema)


stadium_madrid=getbb(place_name='Madrid') %>%  # cinema
  opq(timeout=100) %>%
  add_osm_feature(key='building',value='stadium') %>%
  osmdata_sf()
stadium_madrid 

coord_stadium=stadium_madrid['osm_points']
coord_stadium=do.call(rbind,coord_stadium) %>% dplyr::select('osm_id','geometry')
coord_stadium=st_coordinates(coord_stadium)


gym_map=street_map + 
  geom_sf(data=gym_madrid$osm_points,color=2,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

park_map=street_map + 
  geom_sf(data=park_madrid$osm_polygons,color=3,fill=3,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

stadium_map=street_map + 
  geom_sf(data=stadium_madrid$osm_points,color=4,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

disco_map=street_map + 
  geom_sf(data=disco_madrid$osm_points,color=5,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

cinema_map=street_map + 
  geom_sf(data=cinema_madrid$osm_points,color=6,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

library_map=street_map + 
  geom_sf(data=library_madrid$osm_points,color=7,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

entertainment_map=ggarrange(gym_map,park_map,stadium_map,disco_map,cinema_map,library_map,
                            labels=c('Gym','Park','Stadium','Disco','Cinema','Library'),vjust=3,nrow=2,ncol=3)
annotate_figure(entertainment_map,top=text_grob('Entertainment',color='blue',size=20,vjust=1))


## tourism

historic_madrid=getbb(place_name='Madrid') %>%  # cinema
  opq(timeout=100) %>%
  add_osm_feature(key='historic',value='building') %>%
  osmdata_sf()
historic_madrid 

coord_historic=historic_madrid['osm_points']
coord_historic=do.call(rbind,coord_historic) %>% dplyr::select('osm_id','geometry')
coord_historic=st_coordinates(coord_historic)


attraction_madrid=getbb(place_name='Madrid') %>%  # cinema
  opq(timeout=100) %>%
  add_osm_feature(key='tourism',value='attraction') %>%
  osmdata_sf()
attraction_madrid 

coord_attraction=attraction_madrid['osm_points']
coord_attraction=do.call(rbind,coord_attraction) %>% dplyr::select('osm_id','geometry')
coord_attraction=st_coordinates(coord_attraction)

historic_map=street_map + 
  geom_sf(data=historic_madrid$osm_points,color=2,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

attraction_map=street_map + 
  geom_sf(data=attraction_madrid$osm_points,color=3,size=1,inherit.aes=FALSE) +
  coord_sf(ylim=c(40.3,40.55),expand=FALSE)

tourism_map=ggarrange(historic_map,attraction_map,labels=c('Historic','Attraction'),vjust=10,nrow=1)
annotate_figure(tourism_map,top=text_grob('Tourism',color='blue',size=20,vjust=4))


# distances

house_coord=as.data.frame(cbind(1:nrow(data_coord),data_coord))
names(house_coord)=c('id','long','lat')

min_dist=function(loc){
  from=house_coord[house_coord$id==loc,]
  distance=distHaversine(from[,2:3],to)
  min=data.frame(id=loc,dist=min(distance))
  return(min)
}

to=coord_supermarket
d_supermarket=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_supermarket)='d_supermarket'

to=coord_hospital
d_hospital=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_hospital)='d_hospital'

to=coord_pharmacy
d_pharmacy=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_pharmacy)='d_pharmacy'

to=coord_post
d_post=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_post)='d_post'

to=coord_bank
d_bank=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_bank)='d_bank'

to=coord_university
d_university=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_university)='d_university'

to=coord_school
d_school=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_school)='d_school'

to=coord_kindergarten
d_kindergarten=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_kindergarten)='d_kindergarten'

to=coord_train
d_train=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_train)='d_train'

to=coord_bus
d_bus=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_bus)='d_bus'

to=coord_airport
d_airport=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_airport)='d_airport'

to=coord_gym
d_gym=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_gym)='d_gym'

to=coord_park
d_park=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_park)='d_park'

to=coord_stadium
d_stadium=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_stadium)='d_stadium'

to=coord_library
d_library=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_library)='d_library'

to=coord_disco
d_disco=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_disco)='d_disco'

to=coord_cinema
d_cinema=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_cinema)='d_cinema'

to=coord_historic
d_historic=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_historic)='d_historic'

to=coord_attraction
d_attraction=bind_rows(lapply(house_coord$id,min_dist))[2]
colnames(d_attraction)='d_attraction'

data_distance=as.data.frame(cbind(d_supermarket,d_hospital,d_pharmacy,d_post,d_bank,d_university,
                              d_school,d_kindergarten,d_train,d_bus,d_airport,d_gym,d_park,
                              d_stadium,d_disco,d_cinema,d_library,d_historic,d_attraction))
data_distance=round(data_distance,0)
#View(data_distance)

#write_xlsx(data_distance,'data_distance.xlsx')