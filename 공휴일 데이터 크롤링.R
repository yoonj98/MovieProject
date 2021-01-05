library(glue)
library(XML)
library(stringr)

api.key <- 'qu9mUBECqmbjV1RDmrqw%2FtW8i3%2BF52mw4W5EogF4Yz5227TfAhQsC3gz7hFxUbhBLgXXIN6prRQh5Oq2hUYF4A%3D%3D'
url.format <- 'http://apis.data.go.kr/B090041/openapi/service/SpcdeInfoService/getRestDeInfo?serviceKey={key}&solYear={year}&solMonth={month}'

holiday.request <- function(key, year, month) glue(url.format)
holidays <- data.frame(name=character(), date=character())
data <- xmlToList(holiday.request(api.key, 2017, str_pad(12, 2, pad=0)))
items <- data$body$items

for(item in items){
  if(item$isHoliday == 'Y') 
    holidays <- rbind(holidays, data.frame(item$dateName, item$locdate))
}


# request and read data
for(y in 2014:2018){
  for(m in 1:12){
    data <- xmlToList(holiday.request(api.key, y, str_pad(m, 2, pad=0)))
    items <- data$body$items
    
    for(item in items){
      if(item$isHoliday == 'Y') 
        holidays <- rbind(holidays, data.frame(item$dateName, item$locdate))
    }
  }
}

View(holidays)
colnames(holidays)=c("holidays","day")

holidays_sort<-holidays[order(holidays$day),]

write.csv(holidays_sort,"C:/Users/tldus/Desktop/regression/data/reddays.csv")
