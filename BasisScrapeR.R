#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# BasisScrapeR.R
# Author: 	Ryan Bower
#		ryanthomasbower.com
# Version: 	0.4
# Purpose: 	R code for pulling the basis data is based on code written by
#		Bob Troia [quantifiedbob.com]
# Source:	https://github.com/btroia/basis-data-export 
# License:	Just cite me and pass it on...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(RCurl)
library(rjson)
library(corrgram)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHANGE THIS TO YOUR BASIS CODE:
my_code <- "x10101010101010101010101"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHANGE THESE DATES TO THE ONE YOU WANT!
# The more data, the longer it will take to pull (duh...)
start_date <- "2013-07-01"
end_date <- "2013-07-31"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




full_data <- NA #initialize a data frame that will be used to old all the data
daily_sums <- NA #initialize a daily totals data frame

# Setup the rest of the URL data
url_part_one <- "https://app.mybasis.com/api/v1/chart/"
url_part_two <- ".json?summary=true&interval=60&units=ms&start_date="
url_part_three <- "&heartrate=true&steps=true&calories=true&gsr=true&skin_temp=true&air_temp=true&bodystates=true"

#set some new colors
rb_gray <- '#646464'
rb_lt_blue <- '#6E92A8'
rb_navy_blue <- '#35434D'
rb_teal <- '#A7DBDA'
rb_blue_gray <- '#646B86'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Now loop through the specified days

day_count <- as.Date(end_date) - as.Date(start_date)
for (x in 0:day_count) {
	basis_date <- as.Date(start_date) + x
	print(basis_date)

	url_combo <- paste(url_part_one, paste(my_code, url_part_two, sep=""), sep="")
	url_combo <- paste(url_combo, paste(basis_date, url_part_three, sep=""), sep="")
	raw_data <- getURL(url_combo, ssl.verifypeer = FALSE)	

	dat <- fromJSON(raw_data)
	analysis_data <- lapply(dat[["metrics"]], `[[`, "values")

	analysis_data$skin_temp <- gsub("NULL", "NA", analysis_data$skin_temp)
	skin_temp <- as.numeric(unlist(analysis_data$skin_temp))
	avg_skin <- mean(na.omit(skin_temp))

	analysis_data$heartrate <- gsub("NULL", "NA", analysis_data$heartrate)
	heartrate <- as.numeric(unlist(analysis_data$heartrate))
	avg_heart <- mean(na.omit(heartrate))

	analysis_data$air_temp <- gsub("NULL", "NA", analysis_data$air_temp)
	air_temp <- as.numeric(unlist(analysis_data$air_temp))
	avg_temp <- mean(na.omit(air_temp))

	analysis_data$calories <- gsub("NULL", "NA", analysis_data$calories)
	calories <- as.numeric(unlist(analysis_data$calories))
	tot_calories <- sum(na.omit(calories))

	analysis_data$gsr <- gsub("NULL", "NA", analysis_data$gsr)
	gsr <- as.numeric(unlist(analysis_data$gsr))
	avg_gsr <- mean(na.omit(gsr))

	analysis_data$steps <- gsub("NULL", "NA", analysis_data$steps)
	steps <- as.numeric(unlist(analysis_data$steps))
	tot_steps <- sum(na.omit(steps))

	new_data <- cbind(skin_temp, heartrate, air_temp, calories, gsr, steps)	
	today_sums <- cbind(basis_date, avg_skin, avg_heart, avg_temp, tot_calories, avg_gsr, tot_steps)

	full_data <- rbind(full_data, new_data)
	daily_sums <- rbind(daily_sums, today_sums)	
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Some sample time-series code
daily_ts <- ts(full_data)
plot(daily_ts) #plot all data for every day

pdf("one_day.pdf")
	one_day <- ts(full_data[1:1440,])
	plot(one_day, col=rb_blue_gray)
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot and export Heart Rate as a time series
pdf("heart_rate_ts.pdf")
plot.ts(daily_ts[,1], ylab="Average Heart Rateby Minute", col=rb_blue_gray, 
	frame=F, xaxt='n', xlab="July 2013", main="MINUTE-BY-MINUTE HEART RATE")
axis(1, at=seq(1,44701,by=1440), labels=seq(1,44701,by=1440), las=TRUE, col=rb_gray, cex.axis=1.2, col.axis=rb_gray)
for(i in 0:31) {
	abline(v=1440*i, col=rb_teal)
}
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot skin temperature with a normal curve
pdf("skin_temp.pdf")
skin_temp <- na.omit(full_data[,1])
h<-hist(skin_temp, breaks=10, col=rb_blue_gray, xlab="Skin Temperature (July, 2013)", 
  	 main="DISTRIBUTION OF SKIN TEMPERATURE AND HEART RATE", ylab="", yaxt="n") 
xfit<-seq(min(skin_temp),max(skin_temp),length=40) 
yfit<-dnorm(xfit,mean=mean(skin_temp),sd=sd(skin_temp)) 
yfit <- yfit*diff(h$mids[1:2])*length(skin_temp) 
lines(xfit, yfit, col=rb_lt_blue, lwd=2)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot heart rate with a normal curve
pdf("heart_rate.pdf")
heart_rate <- na.omit(full_data[,2])
h<-hist(heart_rate, breaks=10, col=rb_blue_gray, xlab="Heart Rate (July 2013)", 
  	 main="", ylab="", yaxt="n") 
xfit<-seq(min(heart_rate),max(heart_rate),length=40) 
yfit<-dnorm(xfit,mean=mean(heart_rate),sd=sd(heart_rate)) 
yfit <- yfit*diff(h$mids[1:2])*length(heart_rate) 
lines(xfit, yfit, col=rb_lt_blue, lwd=2)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot GSR with a normal curve
pdf("gsr_dis.pdf")
gsr <- na.omit(full_data[,4])
h<-hist(gsr, breaks=10, col=rb_blue_gray, xlab="Galvonic Skin Response, July 2013", 
  	 main="Distribution of GSR", ylab="", yaxt="n") 
xfit<-seq(min(gsr),max(gsr),length=40) 
yfit<-dnorm(xfit,mean=mean(gsr),sd=sd(gsr)) 
yfit <- yfit*diff(h$mids[1:2])*length(gsr) 
lines(xfit, yfit, col=rb_lt_blue, lwd=2)
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot steps taken with a normal curve
pdf("steps_taken_distribution.pdf")
steps_taken <- na.omit(full_data[,5])
h<-hist(steps_taken, breaks=10, col=rb_blue_gray, xlab="Steps Taken per Minute, July 2013", 
  	 main="Distribution of Steps Taken", ylab="", yaxt="n") 
xfit<-seq(min(steps_taken),max(steps_taken),length=40) 
yfit<-dnorm(xfit,mean=mean(steps_taken),sd=sd(steps_taken)) 
yfit <- yfit*diff(h$mids[1:2])*length(steps_taken) 
lines(xfit, yfit, col=rb_lt_blue, lwd=2)
dev.off()



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a plot of correlations among variables
new_labels <- c("Skin Temp", "Air Temp", "GSR", "Steps", "Heart Rate", "Calories")

pdf("corrgram_output.pdf")
corrgram(full_data, lower.panel=panel.shade,
   	upper.panel=panel.pie, text.panel=panel.txt, labels=new_labels,
	main="Correlation of Variables Tracked by Basis One",
	order=TRUE)
dev.off()
