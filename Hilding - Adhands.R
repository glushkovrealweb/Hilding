#DATA
library(httr)
clientID <- "2118"

token <- content(GET("https://api.adhands.ru/getToken/?login=glushkov@realweb.ru&applicationId=13&grantType=password&password=uyf0p2hb&callback=docs.google.com"), "parsed",  "application/json")[1]
LNXdateStart <- as.numeric(as.POSIXct(paste(dateStart,"0:00:00")))
LNXdateEnd <- as.numeric(as.POSIXct(paste(dateEnd,"23:59:59")))
# CONTEXT NAMES
test<-POST("https://api.adhands.ru/?requestType=json",
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
{
                          "method": "getCampaigns",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "clientId":[%s],
                          "periodStart":%s,
                          "periodEnd":%s
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
for (i in 1:length(rowdata)) {rowdata[[i]]<-rowdata[[i]][c(1,3,4)]}
campaignNames <- do.call(rbind.data.frame, rowdata)
# CONTEXT DATA
test<-POST("https://api.adhands.ru/?requestType=json",
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
                          
{
                          "method": "getStats",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "type": "contextCv",
                          "clientId":[%s],
                          "timeStart":%s,
                          "timeEnd":%s,
                          "groupBy": [
                          "campaign"
                          ]
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
contextdf <- do.call(rbind.data.frame, rowdata)
mergedcontextdf <- merge(campaignNames, contextdf, by.x = "id", by.y = "row.names")

#TARGET MAIL
TMcampaigns <- paste(campaignNames[campaignNames$engine == "mailTarget",][,1],collapse=",")
test<-POST("https://api.adhands.ru/?requestType=json",
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
{
                          "method": "getContextAds",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "campaignId":[%s]
                          }
}
                          ', token, TMcampaigns, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
for (i in 1:length(rowdata)) {rowdata[[i]]<-rowdata[[i]][c(6,7)]}
TMNames <- do.call(rbind.data.frame, rowdata)
TMNames <- TMNames[ifelse(str_detect(TMNames$url, "sedu.adhands"),F,T),]
TMNames$url <- str_replace(TMNames$url, ".+?utm_campaign=(.+?)_adhands&.+", "\\1")
x2 <- y[match(x, y[,1]),2]
dim(x2) <- dim(x)
table(x, x2)
mergedcontextdf[mergedcontextdf$engine == "mailTarget",][,1] <- TMNames[match(mergedcontextdf[mergedcontextdf$engine == "mailTarget",][,1], TMNames[,2]),1]

# RTB DATA
test<-POST("https://api.adhands.ru/?requestType=json",
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
                          
{
                          "method": "getStats",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "type": "rtbCv",
                          "clientId":[%s],
                          "timeStart":%s,
                          "timeEnd":%s,
                          "groupBy": [
                          "campaignremote"
                          ]
                          }
}
                          ', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
RTBdf <- do.call(rbind.data.frame, unlist(rowdata,recursive=FALSE))
row.names(RTBdf) <- str_replace(row.names(RTBdf), ".+\\.", "")
RTBdf$id <-row.names(RTBdf)
rownames(RTBdf) <- NULL
# MEDIA
test<-POST("https://api.adhands.ru/?requestType=json",
           add_headers(Accept = "application/vnd.realweb.adhands-v2+json"),
           body = sprintf('
{
                          "method": "getStats",
                          "applicationId": 13,
                          "login": "glushkov@realweb.ru",
                          "token": "%s",
                          "args":{
                          "type": "mediaCv",
                          "clientId":[%s],
                          "timeStart":%s,
                          "timeEnd":%s,
                          "groupBy": [
                          "placement"
                          ]
                          }
}', token, clientID, LNXdateStart, LNXdateEnd))
stop_for_status(test)
rowdata <- content(test, "parsed", "application/json")
mediadf <- do.call(rbind.data.frame, rowdata)
mediadf$id <-row.names(mediadf)
rownames(mediadf) <- NULL
 
mediadf <- rbind(mediadf, RTBdf)
adhandsSummary <- rbind.fill(mergedcontextdf, mediadf)
 
# MERGING WITH ANALYTICS 
 
campaigndata$campaign <- sub("_adhands","",campaigndata$campaign)
mergdata<-merge(campaigndata, adhandsSummary, by = "campaign", by.y = "id", all.x = TRUE)
# if google by names
mergdata <- subset(mergdata, source.medium != "google / cpc")
googleAdh <- subset(adhandsSummary, engine == "google")
googleAdh <- merge(campaigndata, googleAdh, by = "campaign", by.y = "name")
googleAdh$name <- googleAdh$campaign
googleAdh$id <- NULL
mergdata <- rbind(mergdata, googleAdh)
mergdata[c((ncol(mergdata)-3):ncol(mergdata))][is.na(mergdata[c((ncol(mergdata)-3):ncol(mergdata))])]<-0
mergdata[c((ncol(mergdata)-3):ncol(mergdata))] <- sapply(sapply(mergdata[c((ncol(mergdata)-3):ncol(mergdata))], as.character), as.numeric)
mergdata <-mergdata[order(-mergdata$visits.x),]
#AGREGATED DATA
summBysm <- mergdata[-c((ncol(mergdata)-5):(ncol(mergdata)-4))]
summBysm <- aggregate(cbind(summBysm[c(5:ncol(summBysm))]), list(Group.date = summBysm$source.medium), FUN=sum)
summBysm <-summBysm[order(-summBysm$visits.x),]