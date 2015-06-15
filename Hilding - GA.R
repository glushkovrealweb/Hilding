#Хилдинг - Отчёт#
# загрузка библиотеки
library(RGA)
library("sqldf")
library("reshape2")
library("gplots")
# авторизация

ga_token <- authorize(client.id = "44510395713-cngu0lhjk5rud2o8s3noj6qj17f7obng.apps.googleusercontent.com", client.secret = "FHsO_3tCeb4FgFXjw-GPbCTW")

hilding <- get_ga(profile.id = "82899357",
                  start.date = "2015-05-11", 
                  end.date = "2015-06-11", 
                  metrics = "ga:hits", 
                  dimensions = "ga:pagePath,ga:eventLabel",
                  filter = "ga:eventCategory=~scroll;ga:hits>1"
                  
)