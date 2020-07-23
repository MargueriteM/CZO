############################################################
# Kimberly Wind Rose data to help with Site selection      #
# code by: Marguerite Mauritz    memauritz@utep.edu        #
# data share by: Dave Bjorneberg dave.bjorneberg@usda.gov  #
# June 2020                                                #
############################################################
library(data.table)
library(lubridate)

setwd("~/Desktop/TweedieLab/Projects/CZO/Site_Selection")

# USBR Pacific Northwest Region		
# Hydromet/AgriMet Data Access		
# Although the US Bureau of Reclamation makes efforts to maintain the accuracy		
# of data found in the Hydromet system databases	 the data is largely unverified	
# and should be considered preliminary and subject to change.  Data and services		
# are provided with the express understanding that the United States Government		
# makes no warranties	 expressed or implied	 concerning the accuracy
# completeness	 usability or suitability for any particular purpose of the information	
# or data obtained by access to this computer system	 and the United States	
# shall be under no liability whatsoever to any individual or group entity by		
# reason of any use made thereof. 		

# daily data
wind.kimb <- fread("Wind_TWFI.csv", sep=",", dec=".", header=TRUE, skip=13, na.strings = c('NO RECORD'))

wind.kimb <- wind.kimb[DATE!="END DATA"]

wind.kimb[,date := as.Date(DATE, "%m/%d/%y")]
wind.kimb[,':=' (year=year(date),
                 month=month(date),
                 doy=yday(date))]
# remove all NAs
wind.kimb <- na.omit(wind.kimb)


# hourly data from Kimberly South Farm
# AgriMet Data for TWFI station located at Kimberly ARS main facility
# https://www.usbr.gov/pn/agrimet/webaghrread.html
# 2015 to 2019
# windspeed in mph
kimb.agrimet.h <- fread("Kimberly_SouthFarm_Agrimet_wind.csv", sep=",", dec=".", col.names = c("timestamp","w_dir","w_sp_mph"), skip=7, na.strings = c('NO RECORD'))
kimb.agrimet.h[,date_time := as.POSIXct(timestamp, format="%m/%d/%y %H:%M")]
# extract date/times and convert windspeed from mph to m/s (1 mph = 0.44704 m/s)
kimb.agrimet.h[,':=' (w_sp_ms = w_sp_mph*0.44704,
  year=year(date_time),
                 month=month(date_time),
  month_label=month(date_time,label=TRUE,abbr=TRUE),
                 doy=yday(date_time),
                 hour=hour(date_time))]
# remove all NAs
kimb.agrimet.h <- na.omit(kimb.agrimet.h)

# create day/night categories
kimb.agrimet.h[hour>20 & hour<=23, time_of_day := "night"]
kimb.agrimet.h[hour>=0 & hour<7, time_of_day := "night"]

kimb.agrimet.h[hour>=7 & hour<=20, time_of_day := "day"]

# create season categories
kimb.agrimet.h[month %in% c("10","11","12","1","2","3","4"), season := "Oct - Apr"]
kimb.agrimet.h[month %in% c("5","6","7","8","9"), season := "May - Sep"]


# hourly ARS wind
kimb.ars.h <- fread("Kimberly_SouthFarm_ARS_wind.csv",sep=",", dec=".", header=TRUE, skip=0, na.strings = c('NO RECORD'))
# extract date/times and convert windspeed from mph to m/s (1 mph = 0.44704 m/s)
kimb.ars.h[,date_time := as.POSIXct(timestamp, format="%m/%d/%y %H:%M")]
kimb.ars.h[,':=' (year=year(date_time),
                      month=month(date_time),
                  month_label=month(date_time,label=TRUE,abbr=TRUE),
                      doy=yday(date_time),
                  hour=hour(date_time))]

# remove all NAs
kimb.ars.h <- na.omit(kimb.ars.h)

# create day/night categories
kimb.ars.h[hour>20 & hour<=23, time_of_day := "night"]
kimb.ars.h[hour>=0 & hour<7, time_of_day := "night"]

kimb.ars.h[hour>=7 & hour<=20, time_of_day := "day"]

# create season categories
kimb.ars.h[month %in% c("10","11","12","1","2","3","4"), season := "Oct - Apr"]
kimb.ars.h[month %in% c("5","6","7","8","9"), season := "May - Sep"]


# WINDROSE FUNCTION
################
require(ggplot2)
require(RColorBrewer)

plot.windrose <- function(data,
                          spd,
                          dir,
                          spdres = 2,
                          dirres = 30,
                          spdmin = 2,
                          spdmax = 20,
                          spdseq = NULL,
                          palette = "YlGnBu",
                          countmax = NA,
                          debug = 0){
  
  
  # Look to see what data was passed in to the function
  if (is.numeric(spd) & is.numeric(dir)){
    # assume that we've been given vectors of the speed and direction vectors
    data <- data.frame(spd = spd,
                       dir = dir)
    spd = "spd"
    dir = "dir"
  } else if (exists("data")){
    # Assume that we've been given a data frame, and the name of the speed 
    # and direction columns. This is the format we want for later use.    
  }  
  
  # Tidy up input data ----
  n.in <- NROW(data)
  dnu <- (is.na(data[[spd]]) | is.na(data[[dir]]))
  data[[spd]][dnu] <- NA
  data[[dir]][dnu] <- NA
  
  # figure out the wind speed bins ----
  if (missing(spdseq)){
    spdseq <- seq(spdmin,spdmax,spdres)
  } else {
    if (debug >0){
      cat("Using custom speed bins \n")
    }
  }
  # get some information about the number of bins, etc.
  n.spd.seq <- length(spdseq)
  n.colors.in.range <- n.spd.seq - 1
  
  # create the color map
  spd.colors <- colorRampPalette(brewer.pal(min(max(3,
                                                    n.colors.in.range),
                                                min(9,
                                                    n.colors.in.range)),                                               
                                            palette))(n.colors.in.range)
  
  if (max(data[[spd]],na.rm = TRUE) > spdmax){    
    spd.breaks <- c(spdseq,
                    max(data[[spd]],na.rm = TRUE))
    spd.labels <- c(paste(c(spdseq[1:n.spd.seq-1]),
                          '-',
                          c(spdseq[2:n.spd.seq])),
                    paste(spdmax,
                          "-",
                          max(data[[spd]],na.rm = TRUE)))
    spd.colors <- c(spd.colors, "grey50")
  } else{
    spd.breaks <- spdseq
    spd.labels <- paste(c(spdseq[1:n.spd.seq-1]),
                        '-',
                        c(spdseq[2:n.spd.seq]))    
  }
  data$spd.binned <- cut(x = data[[spd]],
                         breaks = spd.breaks,
                         labels = spd.labels,
                         ordered_result = TRUE)
  # clean up the data
  data. <- na.omit(data)
  
  # figure out the wind direction bins
  dir.breaks <- c(-dirres/2,
                  seq(dirres/2, 360-dirres/2, by = dirres),
                  360+dirres/2)  
  dir.labels <- c(paste(360-dirres/2,"-",dirres/2),
                  paste(seq(dirres/2, 360-3*dirres/2, by = dirres),
                        "-",
                        seq(3*dirres/2, 360-dirres/2, by = dirres)),
                  paste(360-dirres/2,"-",dirres/2))
  # assign each wind direction to a bin
  dir.binned <- cut(data[[dir]],
                    breaks = dir.breaks,
                    ordered_result = TRUE)
  levels(dir.binned) <- dir.labels
  data$dir.binned <- dir.binned
  
  # Run debug if required ----
  if (debug>0){    
    cat(dir.breaks,"\n")
    cat(dir.labels,"\n")
    cat(levels(dir.binned),"\n")       
  }  
  
  # deal with change in ordering introduced somewhere around version 2.2
  if(packageVersion("ggplot2") > "2.2"){    
    cat("Hadley broke my code\n")
    data$spd.binned = with(data, factor(spd.binned, levels = rev(levels(spd.binned))))
    spd.colors = rev(spd.colors)
  }
  
  # create the plot ----
  p.windrose <- ggplot(data = data,
                       aes(x = dir.binned,
                           fill = spd.binned)) +
    geom_bar() + 
    scale_x_discrete(drop = FALSE,
                     labels = waiver()) +
    coord_polar(start = -((dirres/2)/360) * 2*pi) +
    scale_fill_manual(name = "Wind Speed (m/s)", 
                      values = spd.colors,
                      drop = FALSE) +
    theme(axis.title.x = element_blank())
  
  # adjust axes if required
  if (!is.na(countmax)){
    p.windrose <- p.windrose +
      ylim(c(0,countmax))
  }
  
  # print the plot
  print(p.windrose)  
  
  # return the handle to the wind rose
  return(p.windrose)
}
########

# Plot daily windroses
ggplot(wind.kimb[year!=2020], aes(TWFI_UD, fill=factor(month)))+
  labs(x="Wind Direction (degrees)")+
  geom_histogram()+facet_wrap(year~.)

plot.windrose(wind.kimb,wind.kimb$TWFI_UA,wind.kimb$TWFI_UD)
plot.windrose(wind.kimb,wind.kimb$TWFI_UA,wind.kimb$TWFI_UD)+facet_wrap(wind.kimb$month~.)


c1 <- copy(wind.kimb[year>=2000&year<=2005,])

c2 <- copy(wind.kimb[year>=2011&year<=2015,])

c2 <- copy(wind.kimb[year>=2011&year<=2015,])



plot.windrose(c1,c1$TWFI_UA,c1$TWFI_UD)+
  facet_grid(c1$year~c1$month)

# Look at data representation for Agrimet plot histogram of hours by month and year
ggplot(kimb.agrimet.h, aes(factor(hour), fill=factor(month)))+
  labs(x="Hour of the day")+
  geom_histogram(stat="count")+facet_grid(month~year)+
  labs(title = "Kimberly South Field Agrimet, Hourly Histogram")

# plot hourly windroses for agrimet
# max wind speed = 13
ggplot(kimb.agrimet.h, aes(w_dir, fill=factor(month)))+
  labs(x="Wind Direction (degrees)")+
  geom_histogram()+facet_wrap(hour~.)+
  labs(title = "Kimberly South Field Agrimet, Hourly Histogram")

plot.windrose(kimb.agrimet.h[time_of_day=="day"],
              kimb.agrimet.h[time_of_day=="day"]$w_sp_ms,
              kimb.agrimet.h[time_of_day=="day"]$w_dir,spdmax=13)+
  facet_wrap(kimb.agrimet.h[time_of_day=="day"]$hour~.)+theme_bw()+
  labs(title = "Kimberly South Field Agrimet, Day time")

plot.windrose(kimb.agrimet.h[time_of_day=="night"],
              kimb.agrimet.h[time_of_day=="night"]$w_sp_ms,
              kimb.agrimet.h[time_of_day=="night"]$w_dir,spdmax=13)+
  facet_wrap(kimb.agrimet.h[time_of_day=="night"]$hour~.)+theme_bw()+
  labs(title = "Kimberly South Field Agrimet, Night time")


# windrose by time of day and month
plot.windrose(kimb.agrimet.h[season=="Oct - Apr"],
              kimb.agrimet.h[season=="Oct - Apr"]$w_sp_ms,
              kimb.agrimet.h[season=="Oct - Apr"]$w_dir,spdmax=13)+
  facet_grid(kimb.agrimet.h[season=="Oct - Apr"]$time_of_day~
               kimb.agrimet.h[season=="Oct - Apr"]$month_label)+
  labs(title = "Kimberly South Field Agrimet, October to April")+
  theme_bw()

plot.windrose(kimb.agrimet.h[season=="May - Sep"],
              kimb.agrimet.h[season=="May - Sep"]$w_sp_ms,
              kimb.agrimet.h[season=="May - Sep"]$w_dir,spdmax=13)+
  facet_grid(kimb.agrimet.h[season=="May - Sep"]$time_of_day~
               kimb.agrimet.h[season=="May - Sep"]$month_label)+
  labs(title = "Kimberly South Field Agrimet, May to September")+
  theme_bw()

# histogram by time of day and month
ggplot(kimb.agrimet.h, aes(w_dir))+
  labs(x="wind dir")+
  geom_histogram()+facet_grid(time_of_day~season)+
  labs(title = "Kimberly South Field Agrimet, Season and Time of Day")

# Look at data representation for ARS. plot histogram of hours by month and year
ggplot(kimb.ars.h, aes(factor(hour), fill=factor(month)))+
     labs(x="Hour of the day")+
     geom_histogram(stat="count")+facet_grid(month~year)+
     labs(title = "Kimberly ARS, Hourly Histogram")

# plot hourly windroses for ARS
# max wind speed = 18
ggplot(kimb.ars.h, aes(wid_dir, fill=factor(month)))+
  labs(x="Wind Direction (degrees)")+
  geom_histogram()+facet_wrap(hour~.)+
  labs(title = "Kimberly ARS, Hourly Histogram")

plot.windrose(kimb.ars.h,kimb.ars.h$wind_sp_ms,kimb.ars.h$wid_dir,spdmax=18)+
  facet_wrap(kimb.ars.h$hour~.)+theme_bw()+
  labs(title = "Kimberly ARS, Hourly")+

  plot.windrose(kimb.ars.h[time_of_day=="day"],
                kimb.ars.h[time_of_day=="day"]$wind_sp_ms,
                kimb.ars.h[time_of_day=="day"]$wid_dir,spdmax=18)+
  facet_wrap(kimb.ars.h[time_of_day=="day"]$hour~.)+theme_bw()+
  labs(title = "Kimberly ARS, Day time")

plot.windrose(kimb.ars.h[time_of_day=="night"],
              kimb.ars.h[time_of_day=="night"]$wind_sp_ms,
              kimb.ars.h[time_of_day=="night"]$wid_dir,spdmax=18)+
  facet_wrap(kimb.ars.h[time_of_day=="night"]$hour~.)+theme_bw()+
  labs(title = "Kimberly ARS, Night time")


  
# windrose by time of day and month
plot.windrose(kimb.ars.h[season=="Oct - Apr"],
              kimb.ars.h[season=="Oct - Apr"]$wind_sp_ms,
              kimb.ars.h[season=="Oct - Apr"]$wid_dir,spdmax=18)+
  facet_grid(kimb.ars.h[season=="Oct - Apr"]$time_of_day~
               kimb.ars.h[season=="Oct - Apr"]$month_label)+
  labs(title = "Kimberly ARS, October to April")+
  theme_bw()

plot.windrose(kimb.ars.h[season=="May - Sep"],
              kimb.ars.h[season=="May - Sep"]$wind_sp_ms,
              kimb.ars.h[season=="May - Sep"]$wid_dir,spdmax=18)+
  facet_grid(kimb.ars.h[season=="May - Sep"]$time_of_day~
               kimb.ars.h[season=="May - Sep"]$month_label)+
  labs(title = "Kimberly ARS, May to September")+
  theme_bw()

# histogram by time of day and month
ggplot(kimb.ars.h, aes(wid_dir))+
  labs(x="wind dir")+
  geom_histogram()+facet_grid(time_of_day~season)+
  labs(title = "Kimberly ARS, Season and Time of Day")
