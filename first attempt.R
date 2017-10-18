setwd("/Users/p-condemine/Documents/kaggle moskitoes/")
library(data.table)
require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("dplyr")
require("rgdal")
db=fread("/Users/p-condemine/Documents/kaggle moskitoes/train.csv")
db[,c("NumMosquitos","WnvPresent","count"):=list(sum(NumMosquitos),max(WnvPresent),.N),
   by=c("Date","Longitude","Latitude","Species")]
db=unique(db)
map=readShapePoly("poldist")
map@data$id = rownames(map@data)
map.points = fortify(map, region="id")
map.df = join(map.points, map@data, by="id")
coordinates(db) <- ~ Longitude + Latitude
proj4string(db) <- proj4string(map)
db=cbind(db,over(x=db,y = map))
db=data.table(db)

map.df=data.table(map.df)
map.df=map.df[map.df$order,]
ggplot(data=map.df)+geom_polygon(aes(long,lat,group=group,fill=group))



test=fread("/Users/p-condemine/Documents/kaggle moskitoes/test.csv")
test[,c("count"):=list(.N),
   by=c("Date","Longitude","Latitude","Species")]
coordinates(test) <- ~ Longitude + Latitude
proj4string(test) <- proj4string(map)
test=cbind(test,over(x=test,y = map))
test=data.table(test)
spray=fread("/Users/p-condemine/Documents/kaggle moskitoes/spray.csv")
weather=fread("/Users/p-condemine/Documents/kaggle moskitoes/weather.csv")
weather$Tmax=as.numeric(weather$Tmax)
weather$Tmin=as.numeric(weather$Tmin)
weather$AvgSpeed=as.numeric(weather$AvgSpeed)
weather$ResultSpeed=as.numeric(weather$ResultSpeed)
weather$PrecipTotal=as.numeric(weather$PrecipTotal)
weather$PrecipTotal=ifelse(weather$PrecipTotal==0,-1,weather$PrecipTotal)
weather$PrecipTotal=ifelse(is.na(weather$PrecipTotal),0,weather$PrecipTotal)
#Ecart à la moyenne mensuelle
weather$Depart=as.numeric(weather$Depart)
#Indicateur de pression : température d'ébulition
weather$DewPoint=as.numeric(weather$DewPoint)
#même genre
weather$WetBulb=as.numeric(weather$WetBulb)
weather

weather=select(weather,-Water1,-SnowFall,-Depth,-Heat,-Cool,-Sunrise,-Sunset,)
weather[as.numeric(weather$)>0]
summary(factor(weather$CodeSum))

# list of patterns
library(pbapply)

patterns=c("TS ","RA","DZ","HZ","BR","FU","FG ","FG+","TSRA","GR","VCTS")
weather$Date_num=as.numeric(as.Date(weather$Date))
for (pattern in patterns){
  weather$x=1*grepl(pattern=pattern,x=weather$CodeSum)
  temp=subset(weather,weather$x==1)
  temp1=subset(temp,temp$Station=="1")$Date_num
  temp2=subset(temp,temp$Station=="2")$Date_num
  
  weather$last_x=(pbapply(weather[,c("Date_num","Station"),with=F],1,function(y){
    if(y[2]==1){
      if(length(temp1[temp1<=y[1]])>0){
      print(print(y[1]-max(temp1[temp1<=y[1]])))
      } else NA
    } else {
      if(length(temp2[temp2<=y[1]])>0){
      print(y[1]-max(temp2[temp2<=y[1]]))
      } else NA
    }
  }))  
  setnames(weather,c("x","last_x"),c(pattern,paste0("last_",pattern)))
}

str(weather)


"DewPoint","WetBuld","TS "           "TS "          
"RA"            "DZ"            "HZ"            "BR"            "FU"            "FG "           "FG+"           "TSRA"         
"GR"            "VCTS"          "Date_num"      "x"             "Precip_10days"
# volume de précitipitations : 
  
temp=weather[,c("Date_num","PrecipTotal"),with=F]
temp$PrecipTotal=ifelse(temp$PrecipTotal<0,0,temp$PrecipTotal)
weather$Precip_10days=pbsapply(weather$Date_num,function(x){
  sum(subset(temp,temp$Date_num<=x&temp$Date_num>x-10)$PrecipTotal)
})






# Station 1: CHICAGO O'HARE INTERNATIONAL AIRPORT Lat: 41.995 Lon: -87.933 Elev: 662 ft. above sea level
# Station 2: CHICAGO MIDWAY INTL ARPT Lat: 41.786 Lon: -87.752 Elev: 612 ft. above sea level
weather$Longitude=ifelse(weather$Station==1,-87.933,-87.752)
weather$Latitude=ifelse(weather$Station==1,41.995,41.786)
library(ggplot2)
library(ggmap)

mapdata <- readRDS(file.path("mapdata_copyright_openstreetmap_contributors.rds"))
local=db[,list("risk"=sum(NumMosquitos),"risk2"=mean(WnvPresent)),by=c("Longitude","Latitude")]
library(Hmisc)
local$risk_range=cut2(local$risk2,cuts = quantile(local$risk2,c(0.2,0.5,0.8,0.9,0.95)))
g<-ggmap(get_googlemap(center = c(-87.72,41.86),zoom = 11))
g+geom_point(data=local,aes(x=Longitude,y=Latitude,color=factor(risk_range),size=log(log(1+risk))))+
  geom_text(data=local,aes(x=Longitude,y=Latitude,label=paste0(round(10000*risk2/sqrt(risk)))))

db$month=month(as.Date(db$Date))
db$weekday=factor(weekdays(as.Date(db$Date)))
db$week=factor(round(((as.numeric(as.Date(db$Date))-13662)%%365)/7))
db$Species=factor(db$Species)
db$Trap=factor(db$Trap)
db$year=year(as.Date(db$Date))
db$Species_coherent=db$Species
db$day=(as.numeric(as.Date(db$Date))-13662)%%365
# db$year_spray=1*(year(as.Date(db$Date))>=2010)


test$month=month(as.Date(test$Date))
test$weekday=factor(weekdays(as.Date(test$Date)))
test$week=factor(round(((as.numeric(as.Date(test$Date))-13662)%%365)/7))
test$Species=factor(test$Species)
test$Trap=factor(test$Trap)
test$year=year(as.Date(test$Date))
test$Species_coherent=test$Species
test$day=(as.numeric(as.Date(test$Date))-13662)%%365
# test$year_spray=1*(year(as.Date(test$Date))>=2010)



levels(db$Species_coherent)<-c(NA,"CULEX PIPIENS","CULEX PIPIENS/RESTUANS", "CULEX RESTUANS",NA,NA,NA)
# risk in previous year ?

# CREE DE L'OVERFITTING
# ownrisk=subset(db,year(as.Date(db$Date))<2013)[,list("risk_wnv"=sum(WnvPresent),"risk_freq"=100*mean(WnvPresent),
#                  "risk_freq_w"=10000*sum(WnvPresent)/sum(NumMosquitos),
#                  "risk_freq_exposed"=10000*sum(WnvPresent)/log(sum(NumMosquitos))),
#   by=c("Longitude","Latitude")]
# ownrisk[ownrisk==0]<-NA
# db=merge(db,ownrisk,by=c("Longitude","Latitude"),all.x=T,all.y=F)
# 

# Distance time & space to last 
# -alert
# -seasonality
# KNN risk
# gradient of risk to neighbors
# Add data about PoI
KNN=get.knnx(data=weather[1:2,c("Longitude","Latitude"),with=F],query=db[,c("Longitude","Latitude"),with=F],k=2)
dist_station=dist(y=weather[1:2,c("Longitude","Latitude"),with=F],x=db[,c("Longitude","Latitude"),with=F])
weather$Date_next=as.numeric(as.Date(weather$Date))-1
db$Date_next=as.numeric(as.Date(db$Date))
db$dist_station1=dist_station[,1]
db$dist_station2=dist_station[,2]
db$Station=KNN$nn.index[,1]
db=merge(db,weather,by=c("Date_next","Station"),all.x=T,all.y=F)
weather$Date.x=as.numeric(as.Date(weather$Date))
db$Date.x=as.numeric(as.Date(db$Date.x))
db=merge(db,weather,by=c("Date.x","Station"),all.x=T,all.y=F)

KNN2=get.knnx(data=weather[1:2,c("Longitude","Latitude"),with=F],query=test[,c("Longitude","Latitude"),with=F],k=2)
dist_station=dist(y=weather[1:2,c("Longitude","Latitude"),with=F],x=test[,c("Longitude","Latitude"),with=F])
test$Date_next=as.numeric(as.Date(test$Date))
test$dist_station1=dist_station[,1]
test$dist_station2=dist_station[,2]
test$Station=KNN2$nn.index[,1]
test=merge(test,select(weather,-Date.x),by=c("Date_next","Station"),all.x=T,all.y=F)
test$Date.x=as.numeric(as.Date(test$Date.x))
test=merge(test,weather,by=c("Date.x","Station"),all.x=T,all.y=F)


db[,c("year_risk"):=list(mean(WnvPresent)),by=year]
db$year_factor=factor(db$year)
# train=db

train=subset(db,year(db$Date.y)%in%c("2007","2009","2011"))
validation=subset(db,year(db$Date.y)%in%c("2013"))

# cut=sample(1:nrow(db))
# train=db[cut[1:5000]]
# validation=db[cut[5001:nrow(db)]]
library(gbm)
keep=c("WnvPresent","month","weekday","week","Longitude","Latitude",
       "Species_coherent","ownrisk_range","day","count",
       "AvgSpeed.y","PrecipTotal.y",
       "PrecipTotal.x","Tmin.x","Tmax.x","Tmin.y","Tmax.y","ResultSpeed.y","year_factor")
#        "risk_freq_w","risk_freq_exposed")
param=c(ntree=300,depth=15,shrinkage=0.005,train.fraction=0.8,minsize=10)
modeldp=gbm(WnvPresent~.,cv.folds = 10,
            data=train[,which(colnames(train)%in%keep),with=F],
            n.trees = param[1],interaction.depth = param[2],
            shrinkage = param[3],train.fraction = param[4],
            n.minobsinnode=param[5],bag.fraction=0.5,verbose = T)
,weights=log(train$NumMosquitos)
summary(modeldp)
colnames(train[,which(colnames(train)%in%keep),with=F])
plot(modeldp,i.var=c(2,1),type="response")
plot(modeldp,i.var=6,type="response")
n.trees=200
library(verification)
roc=roc.plot(x=validation$WnvPresent,
         pred=predict(modeldp,newdata = validation[,which(colnames(validation)%in%keep),
                                                   with=F],type="response"))
roc$roc.vol



pSubmit<-predict(modeldp, newdata = test, type = "response")
## look at the predicted distribution (AUC doesn't care about probabilities; just ordering. It's still a good diagnostic)
summary(modeldp)

submissionFile<-cbind(test$Id,pSubmit)
colnames(submissionFile)<-c("Id","WnvPresent")
options("scipen"=100, "digits"=8)
write.csv(submissionFile,"submitGBM.csv",row.names=FALSE,quote=FALSE)












# ANNEXES

# SPRAY IS USELESS BECAUSE WE DON'T KNOW THE USE IN TEST SET

i=3
lev=levels(factor(spray$Date))
spray_sub=subset(spray,spray$Date==lev[i])
ggmap(mapdata) + 
#   geom_point(data=spray_sub,aes(x=Longitude,y=Latitude))+
  geom_point(data=local,aes(x=Longitude,y=Latitude,color=factor(risk_range),
                            size=log(log(1+risk))))



sub=subset(db,abs(as.numeric(as.Date(db$Date))-as.numeric(as.Date(lev[i])))<15)
# sub=subset(sub,(sub$Longitude<max(spray_sub$Longitude)&sub$Longitude>min(spray_sub$Longitude))&
#              (sub$Latitude<max(spray_sub$Latitude)&sub$Latitude>min(spray_sub$Latitude)))
sub$dist_spray=get.knnx(query=sub[,c("Longitude","Latitude"),with=F],
                        data=spray_sub[,c("Longitude","Latitude"),with=F],
                        k=1)$nn.dist[,1]
sub=subset(sub,sub$dist_spray<0.01)
sub$before=1*(as.numeric(as.Date(sub$Date))<as.numeric(as.Date(lev[i])))
sub[,list("alerts"=sum(WnvPresent),"mosquitos"=sum(NumMosquitos),"counts"=.N),by="before"]
sub[sub$WnvPresent==1]
lev[i]
################################################################
# WEATHER PATTERNS ?
temp=data.table(pattern=c("RAS",patterns))
for (y in c(2007,2009,2011,2013)){
  counts=length(dbtemp[grepl(pattern = " ",x = dbtemp$CodeSum.y,fixed = TRUE)]$WnvPresent)        
  freqs=mean(dbtemp[grepl(pattern = " ",x = dbtemp$CodeSum.y,fixed = TRUE)]$WnvPresent)
  dbtemp=db[db$year==y]
  for (i in 1:length(patterns)){
    pattern=patterns[i]
    counts=c(counts,length(dbtemp[grepl(pattern = pattern,x = dbtemp$CodeSum.y,fixed = FALSE)]$WnvPresent))
    freqs=c(freqs,mean(dbtemp[grepl(pattern = pattern,x = dbtemp$CodeSum.y,fixed = FALSE)]$WnvPresent))
  }
  temp=data.table(cbind(temp,freq=freqs,count=counts))
  setnames(temp,c("freq","count"),c(paste("freq",y),paste("count",y)))
}              
temp=temp[order(temp[["freq 2011"]],decreasing=T)]
temp
db[,list(mean(WnvPresent),sum(NumMosquitos),.N),by=year]
Ecart important entre les années... 
Il faut essayer de comprendre pourquoi en faisant un clustering sur la météo ?
quelques stats simples devraient suffire ?
ca vient des aéroports ? donc flux migratoires à chicago par année ? ou budget prévention ?



summary(test[,list("number"=.N),by=c("Date","Species")]$number)
summary(test[,list("number"=.N),by=c("Date","Species")]$number)

