#libraries
library(shiny)
library(rgdal)
library(fillmap)
library(viridis)
library(gdata)
library(maptools)

#local data
#UScnty <- readOGR("data/tl_2017_us_county2.shp")
#UScntyPop <- read.csv("C:\\Users\\carrollr\\OneDrive - UNC-Wilmington\\COVID19\\UScntyPop.csv")
#NCcnty <- readOGR("data/tl_2017_nc_county.shp")

NCcnty <- readOGR(system.file("shapes/sids.shp", package="maptools")[1])
NCcnty=NCcnty[order(NCcnty$FIPS),]
NCcnty.sub=NCcnty[c(10,24,65,67,71),]

#JHU and other data set up
UScntyPop=read.csv("https://raw.githubusercontent.com/carrollrm/UScntyPop/master/UScntyPop.csv", header=T)

dates=format(seq(as.Date("03-23-2020",format="%m-%d-%Y"),
                 as.Date(Sys.time())-1, 
                 by="days"),format="%m-%d-%Y")
us.ts.c=us.ts.d=us.ts.r=
  us.ts.c.pop=us.ts.d.pop=us.ts.r.pop=matrix(0,nrow=length(dates),ncol=3141)
nc.ts.c=nc.ts.d=nc.ts.r=
  nc.ts.c.pop=nc.ts.d.pop=nc.ts.r.pop=matrix(0,nrow=length(dates),ncol=100)
for (i in 1:length(dates)){
  nm=read.csv(paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",dates[i],".csv",sep=""), header=T)
  nmus=nm[which(nm$Country_Region=="US"),]
  nmus=nmus[order(nmus$FIPS),]
  nmus=nmus[-which(nmus$FIPS==60000|nmus$FIPS==66000|
                     nmus$FIPS==69000|nmus$FIPS==78000|
                     nmus$FIPS>78999|nmus$FIPS==66|
                     nmus$FIPS==72|nmus$FIPS==78|
                     nmus$FIPS==2016|is.na(nmus$FIPS)),]
  if(dim(nmus[which(nmus$FIPS==32007|nmus$FIPS==35013|nmus$FIPS==53071),])[1]==6){
    nmus=nmus[-c(1750,1804,2988),]  
  } else if (dim(nmus[which(nmus$FIPS==32007|nmus$FIPS==35013),])[1]==4){
    nmus=nmus[-c(1750,1804),]  
  } else if (dim(nmus[which(nmus$FIPS==35013),])[1]==2){
    nmus=nmus[-1803,]  
  } else if (dim(nmus[which(nmus$FIPS==49053),])[1]==2){
    nmus$Confirmed[which(nmus$FIPS==49053)[1]]=sum(nmus$Confirmed[which(nmus$FIPS==49053)])
    nmus=nmus[-which(nmus$FIPS==49053)[2],]  
  } else {nmus=nmus}
  if (dim(nmus)[1]<3141){
    nmus1=nmus
    nmus=nmus.old
    nmus$Confirmed[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Confirmed
    nmus$Recovered[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Recovered
    nmus$Deaths[which(nmus$FIPS%in%nmus1$FIPS)]=nmus1$Deaths
  }
  nmus$pop=UScntyPop$pop
  nmnc=nmus[which(nmus$FIPS<38000&nmus$FIPS>37000),]
  us.ts.c[i,]=(nmus$Confirmed)
  us.ts.d[i,]=(nmus$Deaths)
  us.ts.r[i,]=(nmus$Recovered)
  nc.ts.c[i,]=(nmnc$Confirmed)
  nc.ts.d[i,]=(nmnc$Deaths)
  nc.ts.r[i,]=(nmnc$Recovered)
  us.ts.c.pop[i,]=(nmus$Confirmed)/nmus$pop*100
  us.ts.d.pop[i,]=(nmus$Deaths)/nmus$pop*100
  us.ts.r.pop[i,]=(nmus$Recovered)/nmus$pop*100
  nc.ts.c.pop[i,]=(nmnc$Confirmed)/nmnc$pop*100
  nc.ts.d.pop[i,]=(nmnc$Deaths)/nmnc$pop*100
  nc.ts.r.pop[i,]=(nmnc$Recovered)/nmnc$pop*100
  
  nmus.old=nmus[,-13]
  nmnc.old=nmnc[,-13]
}




###plots

shinyServer(function(input,output){
  output$text <- renderText({
    paste("You have selected to display",
          ifelse(input$adj=="None",
                 "the number of confirmed cases",
                 "the number of confirmed cases as a percent of the population"),
          "for",
          ifelse(input$loc=="State",
                 "the entire state of North Carolina",
                 "the region surrounding New Hanover County"),
          "on",dates[input$day],".")
  })
output$map <- renderPlot({
  if(input$loc=="State"){
    if(input$adj=="Percent of Population"){
      data <- nc.ts.c.pop
      yscl <- seq(0,max(data),.001)
    }
    else {
      data <- nc.ts.c
      yscl <- 0:max(data)
    }
    
    fillmap2(NCcnty,"",data[input$day,],map.lty = 0,y.scl=yscl,
             main.cex = 1,leg.cex = 1, main.line=-3,leg.rnd=2)

  }
  else {
    if(input$adj=="Percent of Population"){
      data <- nc.ts.c.pop[,c(10,24,65,67,71)]
      yscl <- seq(0,max(data),.001)
    }
    else {
      data <- nc.ts.c[,c(10,24,65,67,71)]
      yscl <- 0:max(data)
      }
    fillmap2(NCcnty.sub,"",data[input$day,],map.lty = 0,y.scl=yscl,
           main.cex = 1,leg.cex = 1, main.line=-3,leg.rnd=2)
  }
 })
  output$barplot <- renderPlot({
    if (input$loc=="State"){
       if(input$adj=="Percent of Population"){
         data <- nc.ts.c.pop
         sclr <- seq(0,max(data),.001)
         cols <- viridis(length(sclr), direction = -1)
         shading = cols[round(sort(data[input$day,],decreasing=T)[1:10],3)*1000]
       }
       else {
         data <- nc.ts.c
         sclr <- 0:max(data)
         cols <- viridis(length(sclr), direction = -1)
         shading = cols[sort(data[input$day,],decreasing=T)[1:10]]
       }
       
       par(mar=c(6.5,3,3,1))
       barplot(sort(data[input$day,],decreasing=T)[1:10],
                names.arg=NCcnty$NAME[order(data[input$day,],decreasing=T)][1:10],
                ylim=c(0,max(data)),las=2,col=shading)
       } 
    else {
      if(input$adj=="Percent of Population"){
        data <- nc.ts.c.pop[,c(10,24,65,67,71)]
        sclr <- seq(0,max(data),.001)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[round(sort(data[input$day,],decreasing=T),3)*1000]
      }
      else {
        data <- nc.ts.c[,c(10,24,65,67,71)]
        sclr <- 0:max(data)
        cols <- viridis(length(sclr), direction = -1)
        shading = cols[sort(data[input$day,],decreasing=T)]
      }
      
      par(mar=c(6.5,3,3,1))
      barplot(sort(data[input$day,],decreasing=T),
              names.arg=NCcnty.sub$NAME[order(data[input$day,],decreasing=T)],
              ylim=c(0,max(data)),las=2,col=shading)
    }
  })
  })
