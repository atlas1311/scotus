# dependencies
library(ggplot2)
library(stringr)
library(reshape2)
library(data.table)
library(plyr)
library(RCurl)
library(XML)
library(rvest)

### data frame build ###
scotusURL <- "https://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
scotusTable <- scotusURL %>%
        html() %>%
        html_nodes(xpath = '//*[@id="mw-content-text"]/table[2]') %>%
        html_table()
scotusTable <- scotusTable[[1]]
head(scotusTable)


#select required columns
justices <- scotusTable[, 2:9]
#clean
colnames(justices) <- c("Judge", "State", "BornDied", "ActiveService", "ChiefJustice", "Retirement", "AppointedBy", "Termination
                        Reason")
justices$Born <- str_sub(justices$BornDied, start = 1, end = 4)
justices$Died <- str_sub(justices$BornDied, start = -4, end = -1)
justices[103:114, 10] <- ""
justices$BornDied <- NULL

justices1 <- data.frame(justices, colsplit(justices[, 3], pattern = "\\(", names = c("StartEnd", "Tenure")))
justices1$StartDate <- str_sub(justices1$StartEnd, start = 1, end = 4)
justices1$EndDate <- str_sub(justices1$StartEnd, start = -5, end = -2)
justices1$EndDate <- sub(pattern = "sent", replacement = "2015", x = justices1$EndDate)
justices1$Tenure <- sub(pattern = ")", replacement = "", x = justices1$Tenure)
justices1$ActiveService <- NULL
justices1$StartEnd <- NULL

justices1 <- data.frame(justices1, colsplit(justices1[, 1], pattern = ",", names = c("Justice", "Name")))
justices1$StartDate <- as.Date(justices1$StartDate, "%Y")
justices1$EndDate <- as.Date(justices1$EndDate, "%Y")
justices1$Name <- NULL

justices1$chiefStart <- str_sub(justices1$ChiefJustice, start = 1, end = 4)
justices1$chiefEnd <- str_sub(justices1$ChiefJustice, start = -4, end = -1)
justices1$chiefStart <- as.Date(justices1$chiefStart, "%Y")
justices1$chiefEnd <- as.Date(justices1$chiefEnd, "Y")

justices1$AppointedBy[102] = "Nixon"




### initial graphic build ###

scotusPlot <- data.frame(president = justices1$AppointedBy, judge = justices1$Justice, StartDate = justices1$StartDate,
                     EndDate = justices1$EndDate)

#it depends on how you want to depict it...but perhaps the "modern era" starts prior to FDR?
scotusModern <- scotusPlot[scotusPlot$StartDate >= "1900-01-01", ]

ggplot(scotusModern) +
        geom_segment(aes(x = StartDate, xend = EndDate, y = judge, yend = judge, colour = president, order = EndDate), size = 3) +
        facet_grid(president ~ ., scale = "free_y", space = "free_x") +
        xlab("Year") + ylab("Justice") #+
        #scale_x_continuous(breaks = 1933:2015)




### landmark dataframe build ###
# 1) same as above...rvest the table and dump the tags
# 2) build the dataframe on cases with the parent group as the chief justices (warren court, reignquist etc)
# 3) add them as point plots to the original scotus graphic
# 4) add an additional geom showing if/when landmarks are overturned.....or generate a seperate graph to depict that
# in and of itself

landmarkURL <- "http://en.wikipedia.org/wiki/List_of_landmark_court_decisions_in_the_United_States"
landmarkHTML <- htmlParse(landmarkURL, trim = TRUE, )
landmark <- readHTMLTable(landmarkHTML, stringsAsFactors = FALSE, header = TRUE) [[2]]
landmark <- readHTMLList(landmarkHTML, trim = TRUE, 




