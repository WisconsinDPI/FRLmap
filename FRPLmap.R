#######################################
##      FRPL MAP                    ##
## Create a series of maps plotting ##
## FRPL proportions from Wisconsin  ##
## Publicly available data          ##
######################################

# Version 0.7
# Date: 08/21/2012
# Author: Jared E. Knowles, Policy Research Advisor DPI
# Data sources: WINSS, wisemaps (http://wisemaps.dpi.wi.gov)


# Setup packages
require(maptools)
require(RColorBrewer)
require(gridExtra)
require(grid)
require(ggplot2)
require(mapproj)
gpclibPermit()

# Read in data
# Loop through CSV files of public data on FRL populations
for(i in 2001:2012){
  eval(parse(text=paste("frl",i,"<-read.csv('frl",i,".csv')",sep="")))
  eval(parse(text=paste("frl",i,"<-frl",i,"[,c(1,7,11,13:17)]",sep="")))
}

# Combine into one
frl<-rbind(frl2001,frl2002)
for(i in 2003:2012){
  eval(parse(text=paste("frl<-rbind(frl,frl",i,")",sep="")))
}

# Clean up
for(i in 2001:2012){
  eval(parse(text=paste("rm(frl",i,")",sep="")))
}

# Function to make data numeric
destring<-function(x){
  x<-as.numeric(as.character(x))
}

# Remove non-character data
frl<-apply(frl,2,as.character)
frl<-data.frame(year=as.numeric(frl[,1]),district_number=as.numeric(frl[,2]),
                district_name=as.character(frl[,3]),total_enrollment=as.numeric(frl[,4]),
                econ_disadv_count=as.numeric(frl[,5]),econ_disadv_per=as.numeric(frl[,6]),
                not_econ_disadv_count=as.numeric(frl[,7]),not_econ_disadv_per=as.numeric(frl[,8]))
dataerrors<-subset(frl,econ_disadv_per>100 | econ_disadv_per< 0) # 4 data errors

frl<-subset(frl,total_enrollment<200000 & econ_disadv_per<101) # Remove errors and state

# Set cuts
frl$cut<-cut(frl$econ_disadv_per,breaks=c(-1,15,25,35,50,100),
             labels=c('Less than 15%','15-25%','25-35%','35-50%','More than 50%'))
# Alternate cut
frl$cut2<-cut(frl$econ_disadv_per,breaks=c(-1,5,15,25,35,50,100),
              labels=c('Less than 5%','5-15%','15-25%','25-35%','35-50%','More than 50%'))
# Alternate cut 3
frl$cut3<-cut(frl$econ_disadv_per,breaks=c(-1,10,20,30,40,50,100),
              labels=c('Less than 10%','10-20%','20-30%','30-40%','40-50%','More than 50%'))

# Reshape wide for ggplot2 mapping
frl_w<-reshape(frl,timevar='year',
               idvar=c('district_number','district_name'),direction='wide')

# Read in GIS libraries
library(maptools)
library(RColorBrewer)
library(ggplot2)
library(mapproj)
gpclibPermit()

# Acquire shapefile
dir.create("/shapefile")



# Read in shapefile
distall<-readShapePoly("shapefile/publicshapefileUHS")
# Read in map themes
source('ggplot2themes.R')



#Merge
y<-frl_w
d = distall@data
d$sort=1:nrow(d)
y<-y[order(y$district_number),]
di<-merge(y,d,by.x="district_number",by.y="CODE",all.y=TRUE)
di<-di[order(di$sort),]
##Drop extraneous data##
di$sort2<-di$sort-1
row.names(di)<-di$sort2
#row.names(di)<-di$sort
distall2<-spCbind(distall,di)
#rm(y,VAmath)


df.points = fortify(distall2,region='CODE')
df.points=merge(df.points,distall2@data,by.x='id',by.y='CODE')

df.points<-df.points[order(df.points$group,df.points$piece,
                           df.points$order),]


cnames<-cbind(coordinates(distall2),distall2@data$CODE,
              as.character(distall2@data$NAME))
cnames<-data.frame(long=as.numeric(cnames[,1]),lat=as.numeric(cnames[,2]),
                   distid=cnames[,3],
                   distname=cnames[,4])

rm(d,di,frl,y,distall,distall2)

gc()

# Fix colors across years at 10% intervals

cols3<-c('Less than 10%'='#F1EEf6','10-20%'='#D4B9DA','20-30%'='#C994C7','30-40%'='#DF65B0',
         '40-50%'='#DD1C77','More than 50%'='#980043')

# Create a destination to stick the output images into:
dir.create("plots")

# Make PNG files
library(png)
for(i in 2001:2012){
  eval(parse(text=paste("ineq3",i,"<-ggplot(data=df.points,aes(long,lat))+
  geom_polygon(aes(group=group,fill=cut3.",i,"))+
                        geom_path(aes(group=group),color='black',size=.10,linetype=1)+
                        coord_equal()+
                        scale_fill_manual(values=cols3)+
                        geom_text(data=cnames,aes(x=long,y=lat,label=distname),size=.95)+
                        guides(fill = guide_legend(title='% FRL',ncol=1))+
                        opts(title='Proportion of Students FRL ",i,"')+
                        theme_dpi_mapPNG()",sep="")))
  eval(parse(text=paste("png('plots/evenFRLmap",i,".png',height=1200,width=1000)",sep="")))
  eval(parse(text=paste("print(ineq3",i,")",sep="")))
  dev.off()
  eval(parse(text=paste("rm(ineq3",i,")",sep="")))
  
}

# Fix colors (same palette as above)
cols3<-c('Less than 10%'='#F1EEf6','10-20%'='#D4B9DA','20-30%'='#C994C7','30-40%'='#DF65B0',
         '40-50%'='#DD1C77','More than 50%'='#980043')

# Make PDF files

for(i in 2001:2012){
  eval(parse(text=paste("ineq3",i,"<-ggplot(data=df.points,aes(long,lat))+
  geom_polygon(aes(group=group,fill=cut3.",i,"))+
                        geom_path(aes(group=group),color='black',size=.10,linetype=1)+
                        coord_equal()+
                        scale_fill_manual(values=cols3)+
                        geom_text(data=cnames,aes(x=long,y=lat,label=distname),size=.95)+
                        guides(fill = guide_legend(title='% FRL',ncol=1))+
                        opts(title='Proportion of Students FRL ",i,"')+
                        theme_dpi_map2()",sep="")))
  eval(parse(text=paste("pdf('plots/evenFRLmap",i,".pdf',height=10,width=8)",sep="")))
  eval(parse(text=paste("print(ineq3",i,")",sep="")))
  dev.off()
  eval(parse(text=paste("rm(ineq3",i,")",sep="")))
}


