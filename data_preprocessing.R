install.packages('dplyr')
install.packages('plyr')
install.packages('stringr')
install.packages('ggplot2')
install.packages('psych')

library(dplyr)
library(stringr)
library(plyr)
library(ggplot2)
library(psych)

write.csv(data_maxaudi,'C:/Users/yoonj/Desktop/regression/data/new_datamart.csv')

#######네이버 평점 (크롤링 코드 python)
mvscore=read.csv('C:/Users/yoonj/Desktop/regression/data/codes_result.csv')
#######네이버 보고싶어요(크롤링 코드 r)
like=read.csv('C:/Users/yoonj/Desktop/regression/data/movie_like.csv')
like=as.data.frame(like[,-1])
names(like)[1]<-c("movielike")

data=read.csv('C:/Users/yoonj/Desktop/regression/data/movie_final_data.csv')
data_maxaudi=read.csv('C:/Users/yoonj/Desktop/regression/data/movie_final_maxaudiacc.csv')

data=data[,-1]
data_maxaudi=data_maxaudi[,-1]

###########################################
#크롤링할 데이터 기간 설정
startdate <- as.Date("2014-01-01")
enddate <- as.Date("2018-12-31")
date <- seq(startdate, enddate, by="1 days")
date <- format(date, format = "%Y%m%d")
###########################################
#감독, 배우 score
#배우

mvdata <- data[,c("movieCd","actor","gen","direct","distributor", "nation", "Grade")]
max.acc<-ddply(data,.(movieCd),summarise,audiAcc=max(as.numeric(audiAcc)))
t.data<-merge(max.acc,mvdata,by='movieCd')

tmp<-NULL
for(i in 1:nrow(t.data)){
  tmp_1<-t.data$audiAcc[i]
  tmp_2<-unlist(str_split(t.data$actor[i],","))[c(1,2)]
  tmp_3<-merge(tmp_2,tmp_1)
  tmp<-rbind(tmp,tmp_3)
}

tmp_actor<-ddply(tmp,.(x),summarise,sum_Acc=sum(y)) # 5년동안의 누적관객수 합
actor<-na.omit(tmp_actor) # 애니, 다큐장르의 경우 배우가 존재하지 않아 NA발생
actor$x<-as.character(actor$x)
actor<-filter(actor,x!="")

for(i in 1:nrow(data)){
  tmp_1<-filter(actor,actor$x==str_split(data$actor,",")[[i]][1])$sum_Acc
  tmp_2<-filter(actor,actor$x==str_split(data$actor,",")[[i]][2])$sum_Acc
  tryCatch(data$actorscore[i]<-tmp_1+tmp_2, error=function(e){}) # 주연 배우 2명의 누적관객수의 합
}

#감독
tmp<-NULL
for(i in 1:nrow(t.data)){
  tmp_1<-t.data$audiAcc[i]
  tmp_2<-unlist(str_split(t.data$direct[i],","))[c(1,2)]
  tmp_3<-merge(tmp_2,tmp_1)
  tmp<-rbind(tmp,tmp_3)
}

tmp_director<-ddply(tmp,.(x),summarise,sum_Acc=sum(y))
director<-na.omit(tmp_director)
director<-filter(director, x!="")
director$x<-as.character(director$x)

for(i in 1:nrow(data)){
  tmp_1<-filter(director,director$x==str_split(data$direct,",")[[i]][1])$sum_Acc
  tryCatch(data$directorscore[i]<-tmp_1, error=function(e){})
}


#배급사
tmp<-NULL
for(i in 1:nrow(t.data)){
  tmp_1<-t.data$audiAcc[i]
  tmp_2<-unlist(str_split(t.data$distributor[i],","))[c(1,2)]
  tmp_3<-merge(tmp_2,tmp_1)
  tmp<-rbind(tmp,tmp_3)
}

tmp_distributor<-ddply(tmp,.(x),summarise,sum_Acc=sum(y))
distributor<-na.omit(tmp_distributor)
distributor<-filter(distributor, x!="")
distributor$x<-as.character(distributor$x)

for(i in 1:nrow(data)){
  tmp_1<-filter(distributor,distributor$x==str_split(data$distributor,",")[[i]][1])$sum_Acc
  tryCatch(data$distributorscore[i]<-tmp_1, error=function(e){})
}




###########################################
#네이버 영화 개봉전 보고싶어요 지수 크롤링
#1.네이버 영화 코드 크롤링
url1 <- "http://movie.naver.com/movie/search/result.nhn?query="
url2 <- "&section=all&ie=utf8"

movieNm <- as.character(unique(data$movieNm))

code <- NULL

for(i in 1:length(movieNm)){
  
  url <- paste(url1,URLencode(iconv(movieNm[i], to="UTF-8")),url2,sep="")
  txt <- readLines(url)
  
  tmp <- txt[which(str_detect(txt,"class=\"result_thumb\""))+1]
  code <- str_sub(tmp,str_locate(tmp, "code=")[,2]+1,str_locate(tmp, "\"><img")[,1]-1)
  
  tmp1 <- txt[which(str_detect(txt,"cuser_cnt"))]
  join <- str_sub(tmp1,str_locate(tmp1,"참여")[,2]+2,str_locate(tmp1,"명")[,1]-1)  #영화명 중복일 때에는 참여자 수 많은 걸로 실시
  
  tryCatch(codes[i]<-code[which.max(join)], error=function(e){})
  #cat(movieNm[i],": 완료\n")
  
}

##네이버 보고싶어요 지수 크롤링
url1<-"http://movie.naver.com/movie/bi/mi/point.nhn?code="
url2<-as.numeric(codes)
like <- NULL

for(i in 1:length(codes)){
  
  url<-paste0(url1,url2[i],sep="")
  txt<-readLines(url, encoding="UTF-8")
  tmp<-txt[which(str_detect(txt,"exp_info"))+2]
  tryCatch(like[i]<-str_sub(tmp,str_locate(tmp,"보고싶어요</em>")[,2]+1,str_length(tmp)-7),
           error=function(e){})
  #cat(movieNm[i], ": 완료\n")
  
}

mvcd<-cbind(movieNm,codes)
mvlike <- cbind(mvcd,like)
data<- merge(data,mvlike, by="movieNm")

###########################################
#장르, 배급사 정리 (2개 이상으로 존재하는 data 쪼갬)
for(i in 1:nrow(data)){
  
  data$distributor[i] <- str_split(data$company, ",")[[i]][1]
  data$gen[i] <- str_split(data$genre, ",")[[i]][1]
  data$nation[i] <- str_split(data$nations, ",")[[i]][1]
  data$Grade[i] <- str_split(data$watchGrade, ",")[[i]][1]
  
}

data=data[,-c(15,17,18,19)]  #위에서 정리한 중복된 column 제거 (company, genre, nations,watchGrage)
write.csv(data,'C:/Users/yoonj/Desktop/regression/data/month/movie_final_like.csv')

data=read.csv('C:/Users/yoonj/Desktop/regression/data/month/movie_final_like.csv')
data=data[,-1]


###########################################
#전처리
##column format을 date로 변경
data$date<-as.Date(as.character(data$date),"%Y%m%d")
data$openDt <- as.Date(data$openDt)

## 개봉 후 며칠이 지났는지 확인하는 변수
days = as.numeric(data$date - data$openDt +1)
data<-cbind(data,days)

## 시사회로 집계된것 제거(+재개봉)
data<-data[-which(data$days<1),]

##분석 기간 이전에 개봉한 것 제거
data <- data[which(as.Date(data$openDt) >= as.Date("2014-01-01")),]

## 개봉후 상영일수 7일 미만 영화 제거
tmp <- count(data$movieNm)
tmp <- tmp[which(count(data$movieNm)$freq >= 7),]$x
data <- data[which(data1$movieNm %in% tmp == T),] 

#### 개봉후 1주차 2주차 3주차 4주차 5주차이상 5개범주로 나누기
data$weeks <- ifelse(data$days <=7, 1,
                     ifelse(data$days <=14, 2,
                            ifelse(data$days <=21, 3,
                                   ifelse(data$days <=28, 4, 5))))

#like 변환
data<-data[-which(is.na(data$like)),]  #like지수 NA 삭제

##변수 내 특문 제거 및 데이터 정제
data$distributor <- gsub("\\(주\\)","", data$distributor)
data$distributor <- gsub("\\(유\\)","", data$distributor)
data$distributor <- gsub("\\(재\\)","", data$distributor)
data$distributor <- gsub("\\(NEW\\)","", data$distributor)
data$distributor <- gsub("[A-Za-z]","", data$distributor)
data$distributor <- gsub("\\&","", data$distributor)
data$distributor <- gsub("㈜","", data$distributor)
data$distributor <- gsub(" ","", data$distributor)
data$distributor <- as.factor(data$distributor)
data$gen <- gsub("\\(호러\\)","", data$gen)
data$gen <- gsub("/로맨스","", data$gen)
data$gen <- as.factor(data$gen)
data$nation <- as.factor(data$nation)
data$Grade <- as.factor(data$Grade)

###########################################
genre<-unique(data$gen)  #12
distributor<-unique(data$distributor) #33
movieNm<-unique(data$movieNm) #817
grade<-unique(data$Grade) #4
nation<-unique(data$nation) #8

#movieNm, date 순으로 정렬
datamart<-as.data.frame(datamart %>% arrange(movieNm, date))
write.csv(datamart,'C:/Users/yoonj/Desktop/regression/data/movie_final_datamart.csv') #15756

##3주차까지 데이터자르기
data_week1<-data
data_week1<-data_week1[which(data_week1$days <= 7),]

data_week2<-data
data_week2<-data_week2[which(data_week2$days <= 14),]

data_week3<-data
data_week3<-data_week3[which(data_week3$days <= 21),]

##3주차까지의 누적 관객수중 최대치
data_week1<-ddply(data_week1, .(movieNm), mutate, maxaudiacc=max(audiAcc))
data_week1<-data_week1[which(data_week1$audiAcc == data_week1$maxaudiacc),]
data_week1<-data_week1[,-27]

data_week2<-ddply(data_week2, .(movieNm), mutate, maxaudiacc=max(audiAcc))
data_week2<-data_week2[which(data_week2$audiAcc == data_week2$maxaudiacc),]
data_week2<-data_week2[,-27]

data_week3<-ddply(data_week3, .(movieNm), mutate, maxaudiacc=max(audiAcc))
data_week3<-data_week3[which(data_week3$audiAcc == data_week3$maxaudiacc),]
data_week3<-data_week3[,-27]

datamax<-ddply(data, .(movieNm), mutate, maxaudiacc=max(audiAcc))
datamax<-datamax[which(datamax$audiAcc == datamax$maxaudiacc),]
datamax<-datamax[,-27]

##제자, 옥한흠이라는 영화가 2주차부터 차트인해서 1주차에 대한 자료가 없음
data_week2<-data_week2[-639,]
data_week3<-data_week3[-639,]
datamax<-datamax[-639,]

##3주차까지의 데이터가 유의미함을 히스토그램으로 보임.
tmp <- ddply(datamart, .(movieNm), summarise, audiAcc = max(audiAcc))
ggplot(data = tmp, mapping = aes(x=audiAcc)) +
  geom_histogram(binwidth = 100000) +  #binwidth : 일정구간으로 할당
  scale_x_continuous(breaks= seq(0, 20000000, by=2500000)) + # 최저/최고 : 0 ~2000만, 250만 단위로 표현
  scale_y_continuous(breaks= seq(0, 150, by=25)) # 최저/최고 : 0 ~150, 25 단위로 표현

tmp_1<-ddply(data_week1, .(movieNm), summarise, ratio=data_week1$audiAcc/datamax$audiAcc)
tmp_1<-tmp_1[which(tmp_1$ratio<=1),]

tmp_2<-ddply(data_week2, .(movieNm), summarise, ratio=data_week2$audiAcc/datamax$audiAcc)
tmp_3<-ddply(data_week3, .(movieNm), summarise, ratio=data_week3$audiAcc/datamax$audiAcc)

p1 <- ggplot(data = tmp_1, mapping = aes(x = ratio)) + geom_histogram() + ggtitle("1주차") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkgray"))
p2 <- ggplot(data = tmp_2, mapping = aes(x = ratio)) + geom_histogram() + ggtitle("2주차") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkgray"))
p3 <- ggplot(data = tmp_3, mapping = aes(x = ratio)) + geom_histogram() + ggtitle("3주차") + theme(plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = 15, color = "darkgray"))

plot(p1)
plot(p2)
plot(p3)

###########################################
##더미변수 설정 -> 국가, 장르 (,관람가)
#nondate
data_nondate_dummy<-data_nondate

#국가 더미변수 
data_nondate_dummy$nation<-as.character(data_nondate_dummy$nation)

for (i in 1:817){
  if (grepl("미국",data_nondate_dummy$nation[i])){

  }
  else if (grepl("한국",data_nondate_dummy$nation[i])){

  }
  else{
    data_nondate_dummy$nation[i]="그외"
   }
}

data_nondate_dummy$nation<-as.factor(data_nondate_dummy$nation)
contrasts(data_nondate_dummy$nation)<-contr.treatment(3, base = 3)

#장르 더미변수
data_nondate_dummy$gen<-as.character(data_nondate_dummy$gen)

for (i in 1:817){
  if (grepl("코미디",data_nondate_dummy$gen[i])){}
  else if (grepl("어드벤쳐",data_nondate_dummy$gen[i])){}
  else if (grepl("액션",data_nondate_dummy$gen[i])){}
  else if (grepl("드라마",data_nondate_dummy$gen[i])){}
  else if (grepl("스릴러",data_nondate_dummy$gen[i])){}
  else if (grepl("멜로",data_nondate_dummy$gen[i])){}
  else{data_nondate_dummy$gen[i]="그외"}
}

data_nondate_dummy$gen<-as.factor(data_nondate_dummy$gen)
contrasts(data_nondate_dummy$gen)<-contr.treatment(6, base = 6)

#date
data_dummy<-data_nondate

#국가 더미변수
data_dummy<-data
data_dummy$nation<-as.character(data_dummy$nation)

for (i in 1:15756){
  if (grepl("미국",data_dummy$nation[i])){}
  else if (grepl("한국",data_dummy$nation[i])){ }
  else{
    data_dummy$nation[i]="그외"
  }
}

data_dummy$nation<-as.factor(data_dummy$nation)
contrasts(data_dummy$nation)<-contr.treatment(3, base = 3)

#장르 더미변수
data_dummy$gen<-as.character(data_dummy$gen)

for (i in 1:15756){
  if (grepl("코미디",data_dummy$gen[i])){}
  else if (grepl("어드벤쳐",data_dummy$gen[i])){}
  else if (grepl("액션",data_dummy$gen[i])){}
  else if (grepl("드라마",data_dummy$gen[i])){}
  else if (grepl("스릴러",data_dummy$gen[i])){}
  else if (grepl("멜로",data_dummy$gen[i])){}
  else{data_dummy$gen[i]="그외"}
}

data_dummy$gen<-as.factor(data_dummy$gen)
contrasts(data_dummy$gen)<-contr.treatment(6, base = 6)

###########################################
##데이터마트
#date
datamart <- select(data_dummy, "movieNm","date","weeks","days","audiCnt","audiAcc","scrnCnt","showCnt",
                   "actor","direct","distributor","actorscore","directorscore","distributorscore","like","gen","nation","Grade","salesAmt","salesAcc")
str(datamart)

#nondate
datamart_nondate <- select(data_nondate_dummy, "movieNm","date","weeks","days","audiCnt","audiAcc","scrnCnt","showCnt",
                   "actor","direct","distributor","actorscore","directorscore","distributorscore","like","gen","nation","Grade","salesAmt","salesAcc")
str(datamart_nondate)

write.csv(datamart,'C:/Users/yoonj/Desktop/regression/data/datamart.csv')
write.csv(datamart_nondate,'C:/Users/yoonj/Desktop/regression/data/datamart_nondate.csv')
write.table(datamart_nondate,'C:/Users/yoonj/Desktop/regression/data/codes.txt', sep='\t', quote = FALSE, fileEncoding = "UTF-8")


###########################################
##data handling
datamart_nondate%>%head
dim(datamart_nondate)
colnames(datamart)
str(datamar)

data_small<-datamart_nondate[,c(1:8)]

##movie_day
movie_day<-ddply(data, .(movieNm), transform, movie_days=max(days))
movie_day<-movie_day[which(movie_day$day==movie_day$movie_days),]

##Use table count > 영화 총 상영일수  
a<-data.frame(table(data$movieNm))
a%>%head
unique(a$Var1)

movie_days<-a

names(movie_days)[2]<-c("moviedays")

datamart_nondate=cbind(datamart_nondate,movie_days$moviedays)
names(datamart_nondate)[21]<-c("moviedays")


##첫날 스크린수 생성 
first_scrn<-ddply(data, .(movieNm), transform, first_day=min(days))
first_scrn<-first_scrn[which(first_scrn$first_day==first_scrn$days),]
first_scrn<-as.data.frame(first_scrn)

#동명 영화 제거
first_scrn=first_scrn[-323,]
first_scrn=first_scrn[-135,]


data_maxaudi<-datamart_nondate
data_maxaudi<-cbind(data_maxaudi, first_scrn$scrnCnt)
names(data_maxaudi)[22]<-c("firstscrnCnt")


#data_maxaudi = data_nondate랑 같은 데 test 차원
data_maxaudi<-cbind(data_maxaudi, mvscore$movieScore)
names(data_maxaudi)[23]<-c("movieScore")
data_maxaudi<-cbind(data_maxaudi,like$movielike)
names(data_maxaudi)[24]<-c("movielike")

#진짜로 쓸 변수만 뺌
data_small<-data_maxaudi[,c(6,12:18,20:24)]

#'보고싶어요'지수 factor요소에서 numeric으로 변환
data_logsmall$like<-as.character(data_logsmall$like)
#str(data_logmulti$like)
data_logsmall$like <- gsub(",","",data_logsmall$like)
data_logsmall$like<-as.numeric(data_logsmall$like)

#'보고싶어요'지수 factor요소에서 numeric으로 변환
data_logsmall$movielike<-as.character(data_logsmall$movielike)
#str(data_logmulti$like)
data_logsmall$movielike <- gsub(",","",data_logsmall$movielike)
data_logsmall$movielike<-as.numeric(data_logsmall$movielike)

data_logmulti<-data_logsmall[,-c(4:5,8:10)]

###########################################
audiacc<-as.data.frame(data_maxaudi$audiAcc)
data_logmulti$audiAcc<-audiacc

data_select<-data_logmulti
data_select<-data_select[,-c(9:10)]

names(data_select[,1])='audiacc'
data_select<-data_select[which(data_select$movielike != 0),]