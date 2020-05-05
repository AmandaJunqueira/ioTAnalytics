#ARIMA

#Loading Packages
library("RMySQL")
library("dplyr")
library("chron")
library("lubridate")
library("forecast")
library("tseries")
library("zoo")
library("xts")
library("tidyr")
library("ggplot2")
library("plotly")


#Connecting to the DataBase
con = dbConnect(MySQL(), 
                user='deepAnalytics', 
                password='Sqltask1234!', 
                dbname='dataanalytics2018', 
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com')

#all attributes
irisALL <- dbGetQuery(con, "SELECT * FROM iris")

#specific attributes
irisSELECT <- dbGetQuery(con, "SELECT SepalLengthCm, SepalWidthCm FROM iris")

#Tables from 2006 to 2010
tableList <- dbListTables(con) 
for (i in 1:length(tableList)) {
  if(tableList[i] == "iris") {
    assign(paste(tableList[i], "select", sep = ""), dbGetQuery(con, paste("select SepalLengthCm, SepalWidthCm FROM", tableList[i])))
    assign(paste(tableList[i], "all", sep = ""), dbGetQuery(con, paste("select * from", tableList[i])))
  }
  else {
    assign(tableList[i], dbGetQuery(con, paste("select Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3 FROM", tableList[i])))
  }
}

#Combine tables into one
newDF <- bind_rows(yr_2009)

# Combine Date and Time attribute values in a new attribute column
DateTime <-cbind(newDF,
                 paste(newDF$Date,newDF$Time), 
                 stringsAsFactors=FALSE)

# Give the new attribute in the 6th column a header name 
colnames(DateTime)[6] <-"TimeDate"

## Move the DateTime attribute within the dataset
DateTime <- DateTime[,c(ncol(DateTime), 1:(ncol(DateTime)-1))]

## Convert DateTime from POSIXlt to POSIXct 
DateTime$TimeDate <- as.POSIXct(DateTime$TimeDate, "%Y/%m/%d %H:%M:%S")


DateTime$Year <- year(DateTime$TimeDate)
DateTime$Month <- month(DateTime$TimeDate)
DateTime$Week <- week(DateTime$TimeDate)
DateTime$Day <- day(DateTime$TimeDate)
DateTime$Hour <- hour(DateTime$TimeDate)
DateTime$Minute <- minute(DateTime$TimeDate)
DateTime$weekDay <- weekdays(DateTime$TimeDate)


seasonal<- DateTime %>% 
  select(TimeDate, Sub_metering_1) %>% 
  group_by(week(DateTime$TimeDate))
  
seasonal1 <- ts(seasonal$sub3)
            

seasonalfit <- auto.arima(seasonal1)
plot(forecast(seasonalfit, h=10))
tsdisplay(seasonal1)

seasonal <- DateTime %>% 
group_by(week(TimeDate)) %>% 
  summarise(sub3 = sum(Sub_metering_1)) 


#PIE PLOT FORECAST
fig <- plot_ly(sumInv, labels = ~index, values = ~yr2007, type = 'pie')
fig <- fig %>% layout(title = '2007',
                      xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                      yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
fig





SubMeter1 <- select(DateTime, Month, Sub_metering_1)
SubMeter2 <- select(DateTime, TimeDate, Sub_metering_2)
SubMeter3 <- select(DateTime, TimeDate, Sub_metering_3)

#convert to time series
mycounts <- ts(SubMeter1$Sub_metering_1)
mycounts2 <- ts(SubMeter2$Sub_metering_2)
mycounts3 <- ts(SubMeter3$Sub_metering_3)




fit <- auto.arima(mycounts)
plot(forecast(fit,h=20))


dateTs2 <- ts(DateTime$Sub_metering_2,
              frequency=12)

houseWeekly <- filter(DateTime, 
                      weekDay == "Monday" & 
                        Hour == 5 & 
                        Minute == 1)

## Create TS object with SubMeters
tsWeekly3 <- ts(houseWeekly$Sub_metering_3, 
                frequency=52, 
                start=c(2007,1))
tsWeekly2 <- ts(houseWeekly$Sub_metering_2, 
                frequency=52, 
                start=c(2007,1))
tsWeekly1 <- ts(houseWeekly$Sub_metering_1, 
                frequency=52, 
                start=c(2007,1))  
  
  
  
  




