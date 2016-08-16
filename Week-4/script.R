#Script for Week 4 project on JHU/Coursera Reproducible research
#Needs 'downloader' and 'dplyr' packages from CRAN to be installed

getsets <- function(furl = '') {
    # downloads .zip file from location specified in furl,
    # unzips data and removes downloaded archive from disk
    library(downloader)
    zipfile <- 'StormData.csv.bz2'
    download(fileurl, dest = zipfile, mode = 'wb')
    #unzip(zipfile)
    #unlink(zipfile)
}

procexp <- function(x){
    switch (tolower(x),
            h = return(2),
            k = return(3),
            m = return(6),
            b = return(9)
    )
    is.numeric <- suppressWarnings(!is.na(as.numeric(x)))
    return(if (is.numeric) as.numeric(x) else 0)
}

fileurl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
if (!file.exists('StormData.csv.bz2')) {
    getsets(fileurl)
}

df <- read.csv('StormData.csv.bz2', stringsAsFactors = FALSE, na = 'NA')

#drop unnecessary columns
subdf <- df[,c("EVTYPE","BGN_DATE", "STATE", "FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP")]
rm(df)

#convert event dates from char to date format
dates <- as.Date(subdf$BGN_DATE, '%m/%d/%Y %H:%M:%S')
subdf$BGN_DATE <- dates
rm(dates)
subdf$EVTYPE <- tolower(subdf$EVTYPE)

disasters <- c('Astronomical Low Tide', 'Avalanche', 'Blizzard', 'Coastal Flood', 'Cold', 
               'Windhill', 'Debris Flow', 'Dense Fog', 'Dense Smoke', 'Drought', 'Dust Devil', 
               'Dust Storm', 'Excessive Heat', 'Extremeold', 'Windhill', 'Flash Flood', 
               'Flood', 'Frost', 'Freeze', 'Funnelloud', 'Freezing Fog', 'Hail', 'Heat', 
               'Heavy Rain', 'Heavy Snow', 'High Surf', 'High Wind', 'Hurricane', 'Typhoon', 
               'Ice Storm', 'Lake-Effect Snow', 'Lakeshore Flood', 'Lightning', 'Marine Hail',
               'Marine High Wind', 'Marine Strong Wind', 'Marine Thunderstorm Wind', 
               'Ripurrent', 'Seiche', 'Sleet', 'Storm Surge', 'Tide', 'Strong Wind', 
               'Thunderstorm Wind', 'Tornado', 'Tropical Depression', 'Tropical Storm', 
               'Tsunami', 'Volcanic Ash', 'Waterspout', 'Wildfire', 'Winter Storm', 
               'Winter Weather')
disasters <- tolower(disasters)

for (disaster in disasters) {
    regex <- paste('\\<',disaster, sep = '')
    regex <- paste(regex,'\\>', sep = '')
    ssub <- grepl(pattern = regex, subdf$EVTYPE)
    #exclude marines
    ssub <- ssub & !grepl(pattern = '[Mm]arine', subdf$EVTYPE)
    #exclude excessive for 'heat'
    ssub <- ssub & !grepl(pattern = '[Ee]xcessive', subdf$EVTYPE)
    #exclude flash for 'flood'
    ssub <- ssub & !grepl(pattern = '[Ff]lash', subdf$EVTYPE)
    subdf$EVTYPE[ssub] <- disaster
}

# separate handling for tstm -> tsunami
ssub <- grepl(pattern = 'tstm', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'tsunami'

# separate handling for thun* -> thunderstorm wind, exclude marines
ssub <- grepl(pattern = 'thun', subdf$EVTYPE)
ssub <- ssub & !grepl(pattern = '[Mm]arine', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'thunderstorm wind'

# separate handling for tunderst* -> thunderstorm winds, exclude marines
ssub <- grepl(pattern = 'tunder', subdf$EVTYPE)
ssub <- ssub & !grepl(pattern = '[Mm]arine', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'thunderstorm wind'

# separate handling for torndao/tornados/tornadoes -> tornado
ssub <- grepl(pattern = '(torndao|tornados|tornadoes)', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'tornado'

# separate handling for duststorm -> dust storm
ssub <- grepl(pattern = 'duststorm', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'dust storm'

# separate handling for high winds -> high wind, exclude marines
ssub <- grepl(pattern = 'high winds', subdf$EVTYPE)
ssub <- ssub & !grepl(pattern = '[Mm]arine', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'high wind'

# separate handling for record high -> excessive heat
ssub <- grepl(pattern = 'record high', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'excessive heat'

# separate handling for windchill -> wind chill
ssub <- grepl(pattern = 'record high', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'excessive heat'

# separate handling for shower -> heavy rain
ssub <- grepl(pattern = 'shower', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heavy rain'

# separate handling for hail* -> hail
ssub <- grepl(pattern = 'hail', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'hail'

# separate handling for heavy surf -> high surf
ssub <- grepl(pattern = 'heavy surf', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'high surf'

# separate handling for snowfall -> heavy snow
ssub <- grepl(pattern = 'snowfall', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heavy snow'

# separate handling for flash floo* -> flash flooding
ssub <- grepl(pattern = 'flash floo', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'flash flood'

# separate handling for *smoke* -> dense smoke
ssub <- grepl(pattern = 'smoke', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'dense smoke'

# separate handling for dry*|dri* -> drought
ssub <- grepl(pattern = '(dry|dri)', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'drought'

# separate handling for rainfall -> heavy rain
regex <- paste('\\<', 'rainfall', sep = '')
regex <- paste(regex,'\\>', sep = '')
ssub <- grepl(pattern = regex, subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heavy rain'

# separate handling for rain -> heavy rain
regex <- paste('\\<', 'rain', sep = '')
regex <- paste(regex,'\\>', sep = '')
ssub <- grepl(pattern = regex, subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heavy rain'

# separate handling for hot/warm -> heat
regex <- paste('\\<', '(hot|warm)', sep = '')
regex <- paste(regex,'\\>', sep = '')
ssub <- grepl(pattern = regex, subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heat'

# separate handling for snow -> heavy snow
regex <- paste('\\<', 'snow', sep = '')
regex <- paste(regex,'\\>', sep = '')
ssub <- grepl(pattern = regex, subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'heavy snow'

#assign typhoon to hurricane and then hurricane to 'hurricane (typhoon)'
ssub <- grepl(pattern = 'typhoon', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'hurricane'
ssub <- grepl(pattern = 'hurricane', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'hurricane (typhoon)'

#assign tide to storm surge and then storm surge to 'storm surge/tide', exclude astronomical low tides
ssub <- grepl(pattern = 'tide', subdf$EVTYPE)
ssub <- ssub & !grepl(pattern = '[Aa]stronom', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'storm surge'
ssub <- grepl(pattern = 'storm surge', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'storm surge/tide'

#assign freeze to frost and then frost to 'frost/freeze'
ssub <- grepl(pattern = 'freeze', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'frost'
ssub <- grepl(pattern = 'frost', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'frost/freeze'

#assign wind chill to cold and then cold to 'cold/wind chill'
ssub <- grepl(pattern = 'wind chill', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'cold'
ssub <- grepl(pattern = 'cold', subdf$EVTYPE)
subdf$EVTYPE[ssub] <- 'cold/wind chill'


disasters <- append(disasters, c('cold/wind chill', 'frost/freeze', 'storm surge/tide', 'hurricane (typhoon)'))

ssub <- subdf$EVTYPE %in% disasters
subdf$EVTYPE[!ssub] <- 'other'

fatalities <- aggregate(subdf$FATALITIES, by = list(subdf$EVTYPE), FUN = sum)
shrFatalities <- sum(fatalities$x) / 100
fatalities$share <- fatalities$x / shrFatalities
fatalities[order(-fatalities$x),][c(1:15),]

injuries <- aggregate(subdf$INJURIES, by = list(subdf$EVTYPE), FUN = sum)
shrInjuries <- sum(injuries$x) / 100
injuries$share <- injuries$x / shrInjuries
injuries[order(-injuries$x),][c(1:15),]

library(ggplot2)
library(data.table)

setnames(fatalities, old=c("x", "Group.1"), new=c("fatalities", "event_type"))
setnames(injuries, old=c("x", "Group.1"), new=c("injuries", "event_type"))
victims <- merge(fatalities, injuries, by="event_type")
victims$total <- victims$fatalities + victims$injuries
victimsTop <- victims[order(-victims$total),][c(1:10),]
victimsTopMelt <- melt(victimsTop, id.vars=c("event_type"), 
                       measure.vars=c("fatalities", "injuries"))

ggplot(data=victimsTopMelt, 
       aes(x=reorder(victimsTopMelt$event_type, -victimsTopMelt$value), 
           y=victimsTopMelt$value, fill=variable)) +
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ylab("Total victims") +
    xlab("Event type") +
    theme(text = element_text(size = 15)) +
    ggtitle("Most harmful weather events in USA since 1950")

subdf$propnumexp <- sapply(as.character(subdf$PROPDMGEXP), FUN = procexp)
table(subdf$propnumexp)
subdf$cropnumexp <- sapply(as.character(subdf$CROPDMGEXP), FUN = procexp)
table(subdf$cropnumexp)

propdamage <- aggregate(subdf$PROPDMG * 10^subdf$propnumexp, 
                        by = list(subdf$EVTYPE), FUN = sum)
shrPropdamage <- sum(propdamage$x) / 100
propdamage$share <- propdamage$x / shrPropdamage
propdamage[order(-propdamage$x),][c(1:15),]

cropdamage <- aggregate(subdf$CROPDMG * 10^subdf$cropnumexp, 
                        by = list(subdf$EVTYPE), FUN = sum)
shrCropdamage <- sum(cropdamage$x) / 100
cropdamage$share <- cropdamage$x / shrCropdamage
cropdamage[order(-cropdamage$x),][c(1:15),]

setnames(propdamage, old=c("x", "Group.1"), new=c("properties", "event_type"))
setnames(cropdamage, old=c("x", "Group.1"), new=c("crops", "event_type"))
damages <- merge(propdamage, cropdamage, by="event_type")
damages$total <- damages$properties + damages$crops
damagesTop <- damages[order(-damages$total),][c(1:10),]
damagesTopMelt <- melt(damagesTop, id.vars=c("event_type"), 
                       measure.vars=c("properties", "crops"))

ggplot(data=damagesTopMelt, 
       aes(x=reorder(damagesTopMelt$event_type, -damagesTopMelt$value), 
           y=damagesTopMelt$value / 10^9, fill=variable)) +
    geom_bar(stat="identity") +
    theme(axis.text.x=element_text(angle=90)) +
    ylab("Total damage, $bln") +
    xlab("Event type") +
    theme(text = element_text(size = 15)) +
    ggtitle("Losses from weather events in USA since 1950")