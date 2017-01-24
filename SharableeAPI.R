library(rjson)
library(jsonlite)
jsonResult<-system('curl "http://api.shareablee.com/v1.4/user/1379912/facebook/time_series/?start_date=2016-11-01&end_date=2016-11-30" -H "Authorization: Basic YOURKEY" > out.json')

out <- fromJSON("out.json", flatten=TRUE)

out1<-as.data.frame(out)

head(out1)

View(out1)

write.csv(out1,file = "out.csv",row.names = T)


getwd()

jsonResult=system('curl "http://api.shareablee.com/v1.4/user/1379912/facebook/time_series/?start_date=2016-11-01&end_date=2016-11-30" -H "Authorization: Basic YOURKEY"', intern = TRUE)
out <- fromJSON(jsonResult, flatten=TRUE)
View(as.data.frame(out))
