# dependencies
library(xml2)
library(rvest)
library(stringr)
library(ggplot2)
library(reshape2)

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
justices$Born <- str_sub(justices$BornDied, start = 1, end = 4)
justices$Died <- str_sub(justices$BornDied, start = -4, end = -1)
justices[103:114, 10] <- ""
justices$BornDied <- NULL

# tenure dates
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

# chief justice tenure and name correction
justices1$chiefStart <- str_sub(justices1$ChiefJustice, start = 1, end = 4)
justices1$chiefEnd <- str_sub(justices1$ChiefJustice, start = -4, end = -1)
justices1$chiefStart <- as.Date(justices1$chiefStart, "%Y")
justices1$chiefEnd <- as.Date(justices1$chiefEnd, "%Y")
justices1$Judge <- str_sub(justices1$Judge, start = 0, end = -nchar(justices1$Judge)/2 - 1)
justices1$Retirement <- NULL
justices1$ChiefJustice <- NULL

justices1$AppointedBy[56] = "Cleveland"
justices1$AppointedBy[74] = "Coolidge"
justices1$AppointedBy[102] = "Nixon"

### initial graphic build ###

scotusPlot <- data.frame(President = justices1$AppointedBy, judge = justices1$Justice, StartDate = justices1$StartDate,
                     EndDate = justices1$EndDate)

scotusModern <- scotusPlot[scotusPlot$StartDate >= "1900-01-01", ]
scotusModernAlt <- scotusPlot[scotusPlot$EndDate >= "1900-01-01", ]

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
# 1) same as above...rvest the table and dump the tags
# 2) build the dataframe on cases with the parent group as the chief justices (warren court, reignquist etc)
# 3) build a new facet plot
# 4) add an additional geom showing if/when landmarks are overturned

landmarkURL <- "http://en.wikipedia.org/wiki/List_of_landmark_court_decisions_in_the_United_States"
landmarkTable <- landmarkURL %>%
  html() %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/ul[2]') %>%
  html_table()
landmarkTable <- landmarkTable[[1]]
head(landmarkTable)








