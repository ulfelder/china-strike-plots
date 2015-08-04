library(httr)
library(readxl)
library(dplyr)
library(tidyr)
library(scales)

# Get year and month for last complete month to use in upcoming query; tricky part is dealing with Januarys
yr <- ifelse(as.numeric(substr(Sys.Date(), 6, 7)) - 1 == 1, 
             as.character(as.numeric(substr(Sys.Date(), 1, 4)) - 1),
             substr(Sys.Date(), 1, 4))
mo <- ifelse(as.numeric(substr(Sys.Date(), 6, 7)) - 1 == 1,
             "12",
             as.character(as.numeric(substr(Sys.Date(), 6, 7)) - 1))

# Query API, save result to local file, then read back into R
baseURL <- paste0("http://strikemap.clb.org.hk/strikes/api.v4/export?FromYear=2011&FromMonth=1&ToYear=",
                  yr, "&ToMonth=", mo, "&_lang=en")
queryList <- parse_url(baseURL)
clb <- GET(build_url(queryList), write_disk("clb.temp.xlsx", overwrite=TRUE))
CLB <- read_excel("clb.temp.xlsx")

# Convert weird date numbers to proper dates and then get year and month as text; I found origin by trial and error
CLB <- CLB %>%
    mutate(Date = as.Date(Date, format = "%Y-%m-%d", origin = "1899-12-30"),
           year =  as.numeric(substr(as.character(Date), 1, 4)),
           month = as.numeric(substr(as.character(Date), 6, 7)))

# Generate monthly counts of events by province
CLB.mo.pro <- CLB %>%
    group_by(Location, year, month) %>% # define groups for counting
    tally() %>% # count events in each group
    left_join(expand(select(., -n)), .) %>% # merge the tallies with a grid that contains all possible province-year-months
    filter(., year < as.numeric(yr) | (year == as.numeric(yr) & month <= as.numeric(mo))) %>% # drop rows for months that haven't happened yet
    filter(., is.na(Location) == FALSE) %>% # drop rows with missing province id
    mutate(n = replace(n, which(is.na(n)), 0)) # replace NA counts with 0s
    
# Generate monthly counts by industry
CLB.mo.ind <- CLB %>%
    group_by(Industry, year, month) %>% # define groups for counting
    tally() %>% # count events in each group
    left_join(expand(select(., -n)), .) %>% # merge the tallies with a grid that contains all possible province-year-months
    filter(., year < as.numeric(yr) | (year == as.numeric(yr) & month <= as.numeric(mo))) %>% # drop rows for months that haven't happened yet
    filter(., is.na(Industry) == FALSE) %>% # drop rows with missing province id
    mutate(n = replace(n, which(is.na(n)), 0)) # replace NA counts with 0s

# BY CLAIM

# Create categorical variables identifying types of claims from sloppily coded Demands variable.

# Run through the demand field iteratively, looking for specific strings. This starts with strsplit to break
# the Demands field into strings separated by commas. Next, it uses grep to look within the vectors that creates
# for each row for the specified string (e.g., "wage arrear"). isTRUE converts the result to a logical that ifelse
# can understand. If the specified word or phrase is there, it gets a 1; otherwise 0. I thought about doing this in
# a single claim.type variable, but that gets screwy with cases that fit multiple categories. Then this would keep
# replacing the value sequentially, ultimately only showing the last one checked for in this sequence.

# Get list with vectors of comma-separated, lowercase demand strings for each record
demands <- strsplit(tolower(CLB$Demands), ",")

# Now make dummy variables indicating presence of one or more key phrases in each demand field
CLB$wage.arrears <- sapply(demands, function(x) ifelse(sum(grepl("wage arrear", x)) > 0, 1, 0))
CLB$social.security <- sapply(demands, function(x) ifelse(sum(grepl("social security", x)) > 0, 1, 0))
CLB$work.conditions <- sapply(demands, function(x) ifelse(sum(grepl("work conditions", x)) > 0, 1, 0))
CLB$pay <- sapply(demands, function(x) ifelse(sum(grepl("pay", x)) > 0, 1, 0))
CLB$compensation <- sapply(demands, function(x) ifelse(sum(grepl("compensation", x)) > 0, 1, 0))
CLB$pension <- sapply(demands, function(x) ifelse(sum(grepl("pension", x)) > 0, 1, 0))
CLB$relocation <- sapply(demands, function(x) ifelse(sum(grepl("relocation", x)) > 0, 1, 0))
CLB$corruption <- sapply(demands, function(x) ifelse(sum(grepl("corruption", x)) > 0, 1, 0))
CLB$prices <- sapply(demands, function(x) ifelse(sum(grepl("prices", x)) > 0, 1, 0))
CLB$overtime <- sapply(demands, function(x) ifelse(sum(grepl("ot", x)) > 0, 1, 0))
CLB$layoffs <- sapply(demands, function(x) ifelse(sum(grepl("layoff", x)) > 0, 1, 0))
CLB$bonus <- sapply(demands, function(x) ifelse(sum(grepl("bonus", x)) > 0, 1, 0))
CLB$merger <- sapply(demands, function(x) ifelse(sum(grepl("merger", x)) > 0, 1, 0))
CLB$housing <- sapply(demands, function(x) ifelse(sum(grepl("housing", x)) > 0, 1, 0))
CLB$regulation <- sapply(demands, function(x) ifelse(sum(grepl("regulation", x)) > 0, 1, 0))
CLB$leave <- sapply(demands, function(x) ifelse(sum(grepl("leave", x)) > 0, 1, 0))
CLB$contract <- sapply(demands, function(x) ifelse(sum(grepl("contract", x)) > 0, 1, 0))
CLB$management <- sapply(demands, function(x) ifelse(sum(grepl("management", x)) > 0, 1, 0))
CLB$violence <- sapply(demands,
    function(x) ifelse(sum(grepl("violence", x)) > 0 | sum(grepl("attack", x)) > 0 | sum(grepl("thug", x)) > 0 |
        sum(grepl("beat", x)) > 0 | sum(grepl("kill", x)) > 0,
    1, 0))
CLB$transport <- sapply(demands,
    function(x) ifelse(sum(grepl("taxi", x)) > 0 | sum(grepl("cabs", x)) > 0 | sum(grepl("uber", x)) > 0 |
        sum(grepl("car", x)) > 0 | sum(grepl("rickshaw", x)) > 0,
    1, 0))

CLB.monthly <- CLB %>%
    group_by(year, month) %>%
    summarise(total = n(),
              pay = sum(wage.arrears, pay, compensation, bonus, overtime),
              conditions = sum(work.conditions, housing, leave),
              layoffs = sum(layoffs),
              transport = sum(transport),
              welfare = sum(social.security, pension))

# PLOTTING FUNCTIONS

# Function to plot by province
plotit.province <- function(name) {
  z <- subset(CLB.mo.pro, Location == name)
  with(z, plot(n, type = "n", xlab = "", ylab = "", ylim=c(0,50), axes=FALSE))
  mtext(name, side=2, line=1, las=2, cex=1)
  segments(x0=rep(1,3), x1=rep(length(z$n),3), y0=seq(0,50,25), y1=seq(0,50,25), 
           col=alpha("gray50", 0.5), lwd=0.5)
  with(z, lines(n, col="gray25", lwd = 2))
  axis(4, at=c(0,25,50), tick=FALSE, pos=length(z$n), las=2)
  axis(1, at=seq(1, length(z$n), 12), labels=seq(2011, max(z$year), 1), tick=FALSE, pos=5)
}

# Function to plot by industry. The y-axis range is set to accommodate the maximum
# value for any one industry and is standardized to facilitate comparison across industries.
plotit.industry <- function(name) {
  z <- subset(CLB.mo.ind, Industry == name)
  with(z, plot(n, type = "n", xlab = "", ylab = "", ylim=round(range(CLB.mo.ind$n), -1),
               axes=FALSE))
  mtext(name, side=2, line=1, las=2, cex=1)
  segments(x0=rep(1,3), x1=rep(length(z$n),3), y0=seq(0,120,60), y1=seq(0,120,60), 
           col=alpha("gray50", 0.5), lwd=0.5)
  with(z, lines(n, col="gray25", lwd = 2))
  axis(4, at=c(0,60,120), tick=FALSE, pos=length(z$n), las=2)
  axis(1, at=seq(1, length(z$n), 12), labels=seq(2011, max(z$year), 1), tick=FALSE, pos=15)
}

# MAKE STUFF

shinyServer(function(input, output) {
  
  output$overall <- renderPlot({
    
    par(cex.axis=1, mai=c(0.5, 1, 0.1, 1))
    with(CLB.monthly, plot(total, type = "n", xlab = "", ylab = "", axes = FALSE))
    axis(2, tick = FALSE, las = 2)
    abline(h = seq(0, round(max(CLB.monthly$total, -1)), 50), lwd = 0.5,
           col = alpha("gray50", alpha = 1/2))
    axis(1, at=seq(1, length(CLB.monthly$total), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=15)
    with(CLB.monthly, lines(total, col = "gray25", lwd = 2))
    
  })
  
  output$province <- renderPlot({
    
    par(mai=c(0.2, 1.5, 0.1, 0.25), cex.axis=1,
        mfrow=c(ceiling(length(unique(CLB.mo.pro$Location))/2),2))
    for (i in 1:length(unique(CLB.mo.pro$Location)))
      plotit.province(as.character(unique(CLB.mo.pro$Location)[i]))
    
  })
  
  output$industry <- renderPlot({
    
    par(mai=c(0.2, 1.5, 0.1, 0.25), cex.axis=1,
        mfrow=c(ceiling(length(unique(CLB.mo.ind$Industry))/2),2))
    for (i in 1:length(unique(CLB.mo.ind$Industry)))
      plotit.industry(as.character(unique(CLB.mo.ind$Industry)[i]))
    
  })
  
  output$claim <- renderPlot({
    
    par(mai=c(0.2, 1.5, 0.1, 0.25), cex.axis=0.8, mfrow=c(3,2))
    
    with(CLB.monthly, plot(pay, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
    mtext("Pay", side=2, line=1, las=2, cex=1)
    segments(x0=rep(1,3), x1=rep(length(CLB.monthly$pay),3), y0=seq(0,200,100), y1=seq(0,200,100), 
             col=alpha("gray50", 0.5), lwd=0.5)
    axis(1, at=seq(1, length(CLB.monthly$pay), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=40)
    with(CLB.monthly, lines(pay, col="gray25", lwd = 2))
    axis(4, at=seq(0,200,100), tick=FALSE, pos=length(CLB.monthly$pay), las=2)
    
    with(CLB.monthly, plot(welfare, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
    mtext("Social security", side=2, line=1, las=2, cex=1)
    segments(x0=rep(1,3), x1=rep(length(CLB.monthly$welfare),3), y0=seq(0,200,100), y1=seq(0,200,100), 
             col=alpha("gray50", 0.5), lwd=0.5)
    axis(1, at=seq(1, length(CLB.monthly$pay), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=40)
    with(CLB.monthly, lines(welfare, col="gray25", lwd = 2))
    axis(4, at=seq(0,200,100), tick=FALSE, pos=length(CLB.monthly$welfare), las=2)
    
    with(CLB.monthly, plot(conditions, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
    mtext("Work conditions", side=2, line=1, las=2, cex=1)
    segments(x0=rep(1,3), x1=rep(length(CLB.monthly$conditions),3), y0=seq(0,200,100), y1=seq(0,200,100), 
             col=alpha("gray50", 0.5), lwd=0.5)
    axis(1, at=seq(1, length(CLB.monthly$conditions), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=40)
    with(CLB.monthly, lines(conditions, col="gray25", lwd = 2))
    axis(4, at=seq(0,200,100), tick=FALSE, pos=length(CLB.monthly$conditions), las=2)
    
    with(CLB.monthly, plot(layoffs, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
    mtext("Layoffs", side=2, line=1, las=2, cex=1)
    segments(x0=rep(1,3), x1=rep(length(CLB.monthly$layoffs),3), y0=seq(0,200,100), y1=seq(0,200,100), 
             col=alpha("gray50", 0.5), lwd=0.5)
    axis(1, at=seq(1, length(CLB.monthly$layoffs), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=40)
    with(CLB.monthly, lines(layoffs, col="gray25", lwd = 2))
    axis(4, at=seq(0,200,100), tick=FALSE, pos=length(CLB.monthly$layoffs), las=2)
    
    with(CLB.monthly, plot(transport, type = "n", xlab = "", ylab = "", ylim=c(0,260), axes=FALSE))
    mtext("Transportation", side=2, line=1, las=2, cex=1)
    segments(x0=rep(1,3), x1=rep(length(CLB.monthly$transport),3), y0=seq(0,200,100), y1=seq(0,200,100), 
             col=alpha("gray50", 0.5), lwd=0.5)
    axis(1, at=seq(1, length(CLB.monthly$transport), 12), labels=seq(2011, max(CLB.monthly$year), 1),
         tick=FALSE, pos=40)
    with(CLB.monthly, lines(transport, col="gray25", lwd = 2))
    axis(4, at=seq(0,200,100), tick=FALSE, pos=length(CLB.monthly$transport), las=2)
    
  })
  
})

