# dependencies
library(xml2)
library(rvest)
library(stringr)
library(ggplot2)
library(reshape2)
<<<<<<< Updated upstream

### data frame build ###
scotusURL <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
scotusTable <- scotusURL %>%
      html() %>%
      html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
      html_table()
scotusTable <- scotusTable[[1]]
head(scotusTable)


# select required columns
justices <- scotusTable[, 2:9]
### clean ###
# birth/death dates
colnames(justices) <- c("Judge", "State", "BornDied", "ActiveService", "ChiefJustice", "Retirement", "AppointedBy", "TerminationReason")
=======
library(data.table)
library(plyr)
library(RCurl)
library(XML)
<<<<<<< HEAD

### data frame build ###

scotusURL <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
sctusTable <- readHTMLTable(scotusURL, which = 1, stringsAsFactors = FALSE)

## pull in the data to a google sheets and save as a .csv using
## =importhtml("https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States", "table", 2)
#read in data
scotus <- read.csv("supremeCourt.csv", stringsAsFactors = FALSE)
=======
library(rvest)

### data frame build ###
scotusURL <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
scotusTable <- scotusURL %>%
        html() %>%
        html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
        html_table()
scotusTable <- scotusTable[[1]]
head(scotusTable)

>>>>>>> origin/master

#select required columns
justices <- scotusTable[, 2:9]
#clean
colnames(justices) <- c("Judge", "State", "BornDied", "ActiveService", "ChiefJustice", "Retirement", "AppointedBy", "Termination
                        Reason")
>>>>>>> Stashed changes
justices$Born <- str_sub(justices$BornDied, start = 1, end = 4)
justices$Died <- str_sub(justices$BornDied, start = -4, end = -1)
justices[103:114, 10] <- ""
justices$BornDied <- NULL

<<<<<<< Updated upstream
# tenure dates
=======
>>>>>>> Stashed changes
justices1 <- data.frame(justices, colsplit(justices[, 3], pattern = "\\(", names = c("StartEnd", "Tenure")))
justices1$StartDate <- str_sub(justices1$StartEnd, start = 1, end = 4)
justices1$EndDate <- str_sub(justices1$StartEnd, start = -5, end = -2)
justices1$EndDate <- sub(pattern = "sent", replacement = "2015", x = justices1$EndDate)
justices1$Tenure <- sub(pattern = ")", replacement = "", x = justices1$Tenure)
justices1$ActiveService <- NULL
justices1$StartEnd <- NULL

# tenure dates to as.Date. current serving justices will be the current date.
justices1 <- data.frame(justices1, colsplit(justices1[, 1], pattern = ",", names = c("Justice", "Name")))
justices1$StartDate <- as.Date(justices1$StartDate, "%Y")
justices1$EndDate <- as.Date(justices1$EndDate, "%Y")
justices1$Name <- NULL

<<<<<<< Updated upstream
# chief justice tenure and name correction
justices1$chiefStart <- str_sub(justices1$ChiefJustice, start = 1, end = 4)
justices1$chiefEnd <- str_sub(justices1$ChiefJustice, start = -4, end = -1)
justices1$chiefStart <- as.Date(justices1$chiefStart, "%Y")
justices1$chiefEnd <- as.Date(justices1$chiefEnd, "%Y")
justices1$Judge <- str_sub(justices1$Judge, start = 0, end = -nchar(justices1$Judge)/2 - 1)
justices1$Retirement <- NULL
justices1$ChiefJustice <- NULL
=======
justices1$chiefStart <- str_sub(justices1$ChiefJustice, start = 1, end = 4)
justices1$chiefEnd <- str_sub(justices1$ChiefJustice, start = -4, end = -1)
justices1$chiefStart <- as.Date(justices1$chiefStart, "%Y")
justices1$chiefEnd <- as.Date(justices1$chiefEnd, "Y")

justices1$AppointedBy[102] = "Nixon"


>>>>>>> Stashed changes

justices1$AppointedBy[56] = "Cleveland"
justices1$AppointedBy[74] = "Coolidge"
justices1$AppointedBy[102] = "Nixon"

### initial graphic build ###

<<<<<<< Updated upstream
scotusPlot <- data.frame(President = justices1$AppointedBy, judge = justices1$Justice, StartDate = justices1$StartDate,
                     EndDate = justices1$EndDate)

scotusModern <- scotusPlot[scotusPlot$StartDate >= "1900-01-01", ]
scotusModernAlt <- scotusPlot[scotusPlot$EndDate >= "1900-01-01", ]
=======
scotusPlot <- data.frame(president = justices1$AppointedBy, judge = justices1$Justice, StartDate = justices1$StartDate,
                     EndDate = justices1$EndDate)

#it depends on how you want to depict it...but perhaps the "modern era" starts prior to FDR?
scotusModern <- scotusPlot[scotusPlot$StartDate >= "1900-01-01", ]
>>>>>>> Stashed changes

ggplot(scotusModern) +
        geom_segment(aes(x = StartDate, xend = EndDate, y = judge, yend = judge, colour = President, order = StartDate), size = 5) +
        facet_grid(StartDate ~ President ~., scale = "free_y", space = "free_x") +
        scale_colour_discrete(breaks = c("Roosevelt, T.", "Taft", "Wilson", "Harding", "Coolidge", "Hoover", "Roosevelt, F.", 
                                 "Truman", "Eisenhower", "Kennedy", "Johnson, L.", "Nixon", "Ford", "Reagan", "Bush, G. H. W.",
                                 "Clinton", "Bush, G. W.", "Obama")) +      
        xlab("Year") + ylab("Justice") + 
        scale_x_date(breaks = "8 years", minor_breaks = "4 years") +
        ggtitle("Supreme Court Justice Tenure in the Modern Era") 


### landmark dataframe build ###
<<<<<<< Updated upstream
# 1) same as above...
# 2) build the dataframe on cases with the parent group as the chief justices (warren court, reignquist etc)
# 3) build a new facet plot
# 4) add an additional geom showing if/when landmarks are overturned
=======
# 1) same as above...rvest the table and dump the tags
# 2) build the dataframe on cases with the parent group as the chief justices (warren court, reignquist etc)
# 3) add them as point plots to the original scotus graphic
# 4) add an additional geom showing if/when landmarks are overturned.....or generate a seperate graph to depict that
# in and of itself
>>>>>>> Stashed changes

landmarkURL <- "http://en.wikipedia.org/wiki/List_of_landmark_court_decisions_in_the_United_States"
landmarkTable <- landmarkURL %>%
  html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/ul[2]') %>%
  html_table()
landmarkTable <- landmarkTable[[1]]
head(landmarkTable)








