# dependencies
library(XML)
library(ggplot2)
library(stringr)
library(reshape2)
library(data.table)
library(plyr)
library(timeline)

scotusURL <- "http://en.wikipedia.org/wiki/List_of_Justices_of_the_Supreme_Court_of_the_United_States"
scotusHTML <- htmlParse(scotusURL)
justices <- readHTMLTable(scotusHTML, stringsAsFactors = FALSE) [[2]]
justices1 <- justices[, c("Judge", "Born/Died", "Active service", "Chief Justice", "Appointed by", "Reason for\ntermination")]
names(justices1) <- c("Judge", "BornDied", "Active Service", "Chief Justice", "Appointed By", "Reason for Termination")

justices1$Born <- str_sub(justices1$BornDied, start = 1, end = 4)
justices1$Died <- str_sub(justices1$BornDied, start = -4, end = -1)
justices1[103:114, 8] <- "2015"
justices1$BornDied <- NULL

justices1 <- data.frame(justices1, colsplit(justices1[, 2], pattern = "\\(", names = c("StartEnd", "Tenure")))
justices1$StartDate <- str_sub(justices1$StartEnd, start = 1, end = 4)
justices1$EndDate <- str_sub(justices1$StartEnd, start = -4, end = -1)
justices1$EndDate <- sub(pattern = "sent", replacement = "2015", x = justices1$EndDate)
justices1$Tenure <- sub(pattern = ")", replacement = "", x = justices1$Tenure)
justices1$Active.Service <- NULL
justices1$StartEnd <- NULL


justices1 <- data.frame(justices1, colsplit(justices1[, 1], pattern = ",", names = c("Justice", "Name")))
justices1$Name <- NULL
justices1$StartDate <- as.Date(justices1$StartDate, "%Y")
justices1$EndDate <- as.Date(justices1$EndDate, "%Y")


scotus <- data.frame(president = justices1$Appointed.By, judge = justices1$Justice, StartDate = justices1$StartDate,
                     EndDate = justices1$EndDate)

scotusModern <- scotus[scotus$StartDate >= "1933-01-01", ]
timeline(scotusModern, label.col = "judge", group.col = "president", start.col = "StartDate", end.col = "EndDate",
        num.label.steps = 15)

timeline(scotus, label.col = "judge", group.col = "president", start.col = "StartDate", end.col = "EndDate",
         num.label.steps = 15)



ggplot(scotusModern) +
        geom_segment(aes(x = StartDate, xend = EndDate, y = judge, yend = judge, colour = president), size = 3) +
        facet_grid(president ~ ., scale = "free_y", space = "free_x") +
        xlab("Year") + ylab("Justice")










