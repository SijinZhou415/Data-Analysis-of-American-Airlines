dev.off() # Clear the graph window
cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

getwd()

library(RCurl)
library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)

df<-jsonlite::fromJSON("fall2019-survey-M08.json")
str(df)

sum(is.na(df$Departure.Delay.in.Minutes))
sum(is.na(df$Arrival.Delay.in.Minutes))
sum(is.na(df$Flight.time.in.minutes))

for (i in 1:length(df[1,])) {
  
  if (is.numeric(df[,i]) | is.integer(df[,i])){
    if(length(df[is.na(df[,i]),i]) != 0){
      n <- length(df[is.na(df[,i]),i])
      df[is.na(df[,i]),i] <- mean(df[,i], na.rm = TRUE)
      print(paste("there are ",n," missing data values in column: ",names(df[i])))
    }
  }
  else df[,i] <- df[,i]
}

df$Departure.Delay.in.Minutes <- mean(df$Departure.Delay.in.Minutes, na.rm = TRUE)
df$Arrival.Delay.in.Minutes <- mean(df$Flight.time.in.minutes, na.rm = TRUE)
df$Flight.time.in.minutes <- mean(df$Flight.time.in.minutes, na.rm = TRUE)

df_clean <- df

sum(is.na(df_clean$Departure.Delay.in.Minutes))
sum(is.na(df_clean$Arrival.Delay.in.Minutes))
sum(is.na(df_clean$Flight.time.in.minutes))

dfNumeric <- Filter(is.numeric, df_clean)

df_clean$Departure.Delay.Severity <- ifelse(df_clean$Departure.Delay.in.Minutes <16,"0 to 15", 
                                      (ifelse(df_clean$Departure.Delay.in.Minutes <31, "16 to 30",
                                              (ifelse(df_clean$Departure.Delay.in.Minutes <46, "31 to 45",
                                                      (ifelse(df_clean$Departure.Delay.in.Minutes <61, "46 to 60", "GreaterThanOneHour")))))))

df_clean$Arrival.Delay.Severity <- ifelse(df_clean$Arrival.Delay.in.Minutes <16,"0 to 15", 
                                    (ifelse(df_clean$Arrival.Delay.in.Minutes <31, "16 to 30",
                                            (ifelse(df_clean$Arrival.Delay.in.Minutes <46, "31 to 45",
                                                    (ifelse(df_clean$Arrival.Delay.in.Minutes <61, "46 to 60", "GreaterThanOneHour")))))))

df_clean$PromotionStance <- ifelse(df_clean$Likelihood.to.recommend > 8,"Promoter", 
                             (ifelse(df_clean$Likelihood.to.recommend > 6, "Passive", "Detractor")))
str(df_clean$PromotionStance)
df_clean$PromotionStance<-as.factor(df_clean$PromotionStance)

dfFactor <- Filter(is.character, df_clean)
str(dfFactor)

dfFactor <- dfFactor[ , c(-1,-2,-7,-10,-11,-13)]

dfFactor$Airline.Status <- as.factor(dfFactor$Airline.Status)
dfFactor$Gender <- as.factor(dfFactor$Gender)
dfFactor$Type.of.Travel <- as.factor(dfFactor$Type.of.Travel)
dfFactor$Class <- as.factor(dfFactor$Class)
dfFactor$Partner.Name <- as.factor(dfFactor$Partner.Name)
dfFactor$Partner.Code <- as.factor(dfFactor$Partner.Code)
dfFactor$Flight.cancelled <- as.factor(dfFactor$Flight.cancelled)
dfFactor$Departure.Delay.Severity <- as.factor(dfFactor$Departure.Delay.Severity)
dfFactor$Arrival.Delay.Severity <- as.factor(dfFactor$Arrival.Delay.Severity)
dfFactor$PromotionStance <- as.factor(dfFactor$PromotionStance)
str(dfFactor)

dfPromoters <- df_clean%>% filter(Likelihood.to.recommend > 8) %>% 
  arrange(desc(Partner.Name))

dfPassives <- df_clean %>% filter(between(Likelihood.to.recommend, 7, 8)) %>% 
  arrange(desc(Partner.Name))

dfDetracters <- df_clean %>% filter(Likelihood.to.recommend < 7) %>% 
  arrange(desc(Partner.Name))

us<-map_data("state")
View(us)

maptheme <- theme(panel.grid = element_blank()) +
  theme(axis.text = element_blank()) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title = element_blank()) +
  theme(legend.position = "bottom") +
  theme(panel.grid = element_blank()) +
  theme(panel.background = element_rect(fill = "#FFFFFF")) +
  theme(plot.margin = unit(c(0, 0, 0.5, 0), 'cm'))

country_shapes <- geom_polygon(aes(x = long, y = lat, group = group),
                               data = map_data("state"),
                               fill = "#CECECE", color = "#515151",
                               size = 0.20)
mapcoords <- coord_fixed(xlim = c(-165, -60), ylim = c(10, 70))

View(dfDetracters)


dfDetracters1<-unite(dfDetracters, "O_D", c("Origin.City","Destination.City"))
dfDetracters_count<-merge(dfDetracters1,dfUnion,by="O_D")
dfDetracters<-merge(dfDetracters,dfDetracters_count,by=c("olong","olat","dlong","dlat"))
View(dfDetracters)
names(dfDetracters)
names(dfDetracters)[67]<-"routes"

dfDetracters_Cheapest<-dfDetracters %>% filter(Partner.Name=="Cheapseats Airlines Inc.") %>% filter(Likelihood.to.recommend<=2)
str(dfDetracters_Cheapest)



dfDetracters_Cool<-dfDetracters %>% filter(Partner.Name=="Cool&Young Airlines Inc.")
dfDetracters_Enjoy<-dfDetracters %>% filter(Partner.Name=="EnjoyFlying Air Services")
dfDetracters_FlyFast<-dfDetracters %>% filter(Partner.Name=="FlyFast Airways Inc.")
dfDetracters_FlyHere<-dfDetracters %>% filter(Partner.Name=="FlyHere Airways")
dfDetracters_FlyToSun<-dfDetracters %>% filter(Partner.Name=="FlyToSun Airlines Inc.")
dfDetracters_GoingNorth<-dfDetracters %>% filter(Partner.Name=="GoingNorth Airlines Inc.")
dfDetracters_Northwest<-dfDetracters %>% filter(Partner.Name=="Northwest Business Airlines Inc.")
dfDetracters_OnlyJets<-dfDetracters %>% filter(Partner.Name=="OnlyJets Airlines Inc.")
dfDetracters_Oursin<-dfDetracters %>% filter(Partner.Name=="Oursin Airlines Inc.")
dfDetracters_Paul<-dfDetracters %>% filter(Partner.Name=="Paul Smith Airlines Inc.")
dfDetracters_Sigma<-dfDetracters %>% filter(Partner.Name=="Sigma Airlines Inc.")
dfDetracters_Southeast<-dfDetracters %>% filter(Partner.Name=="Southeast Airlines Co.")
dfDetracters_West<-dfDetracters %>% filter(Partner.Name=="West Airways Inc.")
View(dfDetracters_West)

dfDetracters_Cheapest<-dfDetracters %>% filter(Partner.Name=="Cheapseats Airlines Inc.") %>% filter(Likelihood.to.recommend.x<2)
g<-ggplot(dfDetracters_Cheapest) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=routes),data = dfDetracters_Cheapest,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Cheapest$olong,y=dfDetracters_Cheapest$olat))
g<-g+geom_point(aes(x=dfDetracters_Cheapest$dlong,y=dfDetracters_Cheapest$dlat))
g<-g+geom_text(aes(x=dfDetracters_Cheapest$olong,y=dfDetracters_Cheapest$olat,label=dfDetracters_Cheapest$Origin.City),hjust=0,vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Cheapest$dlong,y=dfDetracters_Cheapest$dlat,label=dfDetracters_Cheapest$Destination.City),hjust=0,vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Cheapseats Airlines Inc.")
g

dfDetracters_Cool<-dfDetracters %>% filter(Partner.Name=="Cool&Young Airlines Inc.")
g<-ggplot(dfDetracters_Cool) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data = dfDetracters_Cool,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Cool$olong,y=dfDetracters_Cool$olat))
g<-g+geom_point(aes(x=dfDetracters_Cool$dlong,y=dfDetracters_Cool$dlat))
g<-g+geom_text(aes(x=dfDetracters_Cool$olong,y=dfDetracters_Cool$olat,label=dfDetracters_Cool$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Cool$dlong,y=dfDetracters_Cool$dlat,label=dfDetracters_Cool$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Cool&Young Airlines Inc.")
g

dfDetracters_Enjoy<-dfDetracters %>% filter(Partner.Name=="EnjoyFlying Air Services") %>% filter(Likelihood.to.recommend<=2)
g<-ggplot(dfDetracters_Enjoy) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data = dfDetracters_Enjoy,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Enjoy$olong,y=dfDetracters_Enjoy$olat))
g<-g+geom_point(aes(x=dfDetracters_Enjoy$dlong,y=dfDetracters_Enjoy$dlat))
g<-g+geom_text(aes(x=dfDetracters_Enjoy$olong,y=dfDetracters_Enjoy$olat,label=dfDetracters_Enjoy$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Enjoy$dlong,y=dfDetracters_Enjoy$dlat,label=dfDetracters_Enjoy$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of EnjoyFlying Air Services")
g

dfDetracters_FlyFast<-dfDetracters %>% filter(Partner.Name=="FlyFast Airways Inc.") %>% filter(Likelihood.to.recommend==1)
g<-ggplot(dfDetracters_FlyFast) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data = dfDetracters_FlyFast,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_FlyFast$olong,y=dfDetracters_FlyFast$olat))
g<-g+geom_point(aes(x=dfDetracters_FlyFast$dlong,y=dfDetracters_FlyFast$dlat))
g<-g+geom_text(aes(x=dfDetracters_FlyFast$olong,y=dfDetracters_FlyFast$olat,label=dfDetracters_FlyFast$Origin.City),hjust=0, vjust=0,size=2)
g<-g+geom_text(aes(x=dfDetracters_FlyFast$dlong,y=dfDetracters_FlyFast$dlat,label=dfDetracters_FlyFast$Destination.City),hjust=0, vjust=0,size=2)
g<-g+mapcoords+ggtitle("FlyFast Airways Inc.")
g

dfDetracters_FlyHere<-dfDetracters %>% filter(Partner.Name=="FlyHere Airways")%>% filter(Likelihood.to.recommend<=3)
g<-ggplot(dfDetracters_FlyHere) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data = dfDetracters_FlyHere,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_FlyHere$olong,y=dfDetracters_FlyHere$olat))
g<-g+geom_point(aes(x=dfDetracters_FlyHere$dlong,y=dfDetracters_FlyHere$dlat))
g<-g+geom_text(aes(x=dfDetracters_FlyHere$olong,y=dfDetracters_FlyHere$olat,label=dfDetracters_FlyHere$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_FlyHere$dlong,y=dfDetracters_FlyHere$dlat,label=dfDetracters_FlyHere$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of FlyHere Airways")
g

dfDetracters_FlyToSun<-dfDetracters %>% filter(Partner.Name=="FlyToSun Airlines Inc.") %>% filter(Likelihood.to.recommend<=3)
g<-ggplot(dfDetracters_FlyToSun) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_FlyToSun,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_FlyToSun$olong,y=dfDetracters_FlyToSun$olat))
g<-g+geom_point(aes(x=dfDetracters_FlyToSun$dlong,y=dfDetracters_FlyToSun$dlat))
g<-g+geom_text(aes(x=dfDetracters_FlyToSun$olong,y=dfDetracters_FlyToSun$olat,label=dfDetracters_FlyToSun$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_FlyToSun$dlong,y=dfDetracters_FlyToSun$dlat,label=dfDetracters_FlyToSun$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of FlyToSun Airlines Inc.")
g

dfDetracters_GoingNorth<-dfDetracters %>% filter(Partner.Name=="GoingNorth Airlines Inc.") %>% filter(Likelihood.to.recommend<=3)
g<-ggplot(dfDetracters_GoingNorth) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_GoingNorth,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_GoingNorth$olong,y=dfDetracters_GoingNorth$olat))
g<-g+geom_point(aes(x=dfDetracters_GoingNorth$dlong,y=dfDetracters_GoingNorth$dlat))
g<-g+geom_text(aes(x=dfDetracters_GoingNorth$olong,y=dfDetracters_GoingNorth$olat,label=dfDetracters_GoingNorth$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_GoingNorth$dlong,y=dfDetracters_GoingNorth$dlat,label=dfDetracters_GoingNorth$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of GoingNorth Airlines Inc.")
g

dfDetracters_Northwest<-dfDetracters %>% filter(Partner.Name=="Northwest Business Airlines Inc.") %>% filter(Likelihood.to.recommend==1)
g<-ggplot(dfDetracters_Northwest) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_Northwest,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Northwest$olong,y=dfDetracters_Northwest$olat))
g<-g+geom_point(aes(x=dfDetracters_Northwest$dlong,y=dfDetracters_Northwest$dlat))
g<-g+geom_text(aes(x=dfDetracters_Northwest$olong,y=dfDetracters_Northwest$olat,label=dfDetracters_Northwest$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Northwest$dlong,y=dfDetracters_Northwest$dlat,label=dfDetracters_Northwest$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Northwest Business Airlines Inc.")
g

dfDetracters_OnlyJets<-dfDetracters %>% filter(Partner.Name=="OnlyJets Airlines Inc.") %>% filter(Likelihood.to.recommend<=2)
g<-ggplot(dfDetracters_OnlyJets) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_OnlyJets,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_OnlyJets$olong,y=dfDetracters_OnlyJets$olat))
g<-g+geom_point(aes(x=dfDetracters_OnlyJets$dlong,y=dfDetracters_OnlyJets$dlat))
g<-g+geom_text(aes(x=dfDetracters_OnlyJets$olong,y=dfDetracters_OnlyJets$olat,label=dfDetracters_OnlyJets$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_OnlyJets$dlong,y=dfDetracters_OnlyJets$dlat,label=dfDetracters_OnlyJets$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of OnlyJets Airlines Inc.")
g

dfDetracters_Oursin<-dfDetracters %>% filter(Partner.Name=="Oursin Airlines Inc.") %>% filter(Likelihood.to.recommend<=2)
g<-ggplot(dfDetracters_Oursin) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_Oursin,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Oursin$olong,y=dfDetracters_Oursin$olat))
g<-g+geom_point(aes(x=dfDetracters_Oursin$dlong,y=dfDetracters_Oursin$dlat))
g<-g+geom_text(aes(x=dfDetracters_Oursin$olong,y=dfDetracters_Oursin$olat,label=dfDetracters_Oursin$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Oursin$dlong,y=dfDetracters_Oursin$dlat,label=dfDetracters_Oursin$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Oursin Airlines Inc.")
g

dfDetracters_Paul<-dfDetracters %>% filter(Partner.Name=="Paul Smith Airlines Inc.") %>% filter(Likelihood.to.recommend<=2)
g<-ggplot(dfDetracters_Paul) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_Paul,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Paul$olong,y=dfDetracters_Paul$olat))
g<-g+geom_point(aes(x=dfDetracters_Paul$dlong,y=dfDetracters_Paul$dlat))
g<-g+geom_text(aes(x=dfDetracters_Paul$olong,y=dfDetracters_Paul$olat,label=dfDetracters_Paul$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Paul$dlong,y=dfDetracters_Paul$dlat,label=dfDetracters_Paul$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Smith Airlines Inc.")
g

dfDetracters_Sigma<-dfDetracters %>% filter(Partner.Name=="Sigma Airlines Inc.") %>% filter(Likelihood.to.recommend==1)
g<-ggplot(dfDetracters_Sigma) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_Sigma,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Sigma$olong,y=dfDetracters_Sigma$olat))
g<-g+geom_point(aes(x=dfDetracters_Sigma$dlong,y=dfDetracters_Sigma$dlat))
g<-g+geom_text(aes(x=dfDetracters_Sigma$olong,y=dfDetracters_Sigma$olat,label=dfDetracters_Sigma$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Sigma$dlong,y=dfDetracters_Sigma$dlat,label=dfDetracters_Sigma$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Sigma Airlines Inc.")
g

dfDetracters_Southeast<-dfDetracters %>% filter(Partner.Name=="Southeast Airlines Co.") %>% filter(Likelihood.to.recommend<=2)
g<-ggplot(dfDetracters_Southeast) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=Flight.date),data =dfDetracters_Southeast,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_Southeast$olong,y=dfDetracters_Southeast$olat))
g<-g+geom_point(aes(x=dfDetracters_Southeast$dlong,y=dfDetracters_Southeast$dlat))
g<-g+geom_text(aes(x=dfDetracters_Southeast$olong,y=dfDetracters_Southeast$olat,label=dfDetracters_Southeast$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_Southeast$dlong,y=dfDetracters_Southeast$dlat,label=dfDetracters_Southeast$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of Southeast Airlines Co.")
g

dfDetracters_West<-dfDetracters %>% filter(Partner.Name=="West Airways Inc.")
g<-ggplot(dfDetracters_West) +country_shapes
g<-g+geom_curve(aes(x=olong,y=olat,xend=dlong,yend=dlat,color=dfDetracters_West$Flight.date),data =dfDetracters_West,curvature = 0.25,alpha=0.5)
g<-g+geom_point(aes(x=dfDetracters_West$olong,y=dfDetracters_West$olat))
g<-g+geom_point(aes(x=dfDetracters_West$dlong,y=dfDetracters_West$dlat))
g<-g+geom_text(aes(x=dfDetracters_West$olong,y=dfDetracters_West$olat,label=dfDetracters_West$Origin.City),hjust=0, vjust=0,size=3)
g<-g+geom_text(aes(x=dfDetracters_West$dlong,y=dfDetracters_West$dlat,label=dfDetracters_West$Destination.City),hjust=0, vjust=0,size=3)
g<-g+mapcoords+ggtitle("Routes of West Airways Inc.")
g

dfDetracters %>% group_by(Departure.Delay.Severity) %>% summarise(count=n())

dfDetracters %>% group_by(Arrival.Delay.Severity) %>% summarise(count=n())

dfDetracters %>% group_by(Flight.date) %>% summarise(count=n())








