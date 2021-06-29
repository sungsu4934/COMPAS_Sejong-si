
setwd("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001")

library(dplyr)
library(stringr)

################################################## 가격지수 ##################################################
########## 매매지수--- ##########

##### 아파트 매매지수 ##### 
# 세종시 아파트 매매 실거래가 불러오기
apt0.own <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_아파트(매매)_실거래가.csv")
apt0.own <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종아파트실거래가.csv")

# 필요 변수 남기기 및 변수명 변경(편의상)
apt1.own <- apt0.own %>%
  filter(is.na(해제사유발생일)==TRUE) %>%
  select(-c(계약일,단지명,건축년도,해제사유발생일)) %>%
  rename("sale_date"=계약년월,"address"=시군구,"bon"=본번,"bu"=부번,"m_usearea"=전용면적...,"price"=거래금액.만원.,"floor"=층)

# 필요 변수 추가 생성 : 단위가격, 동일주택가정 변수(면적그룹,층그룹)
apt1.own$unit_price <- apt1.own$price / apt1.own$m_usearea      #단위가격 생성 : 거래금액/전용면적

 # 동일주택가정 변수(층그룹)
apt1.own <- transform(apt1.own,
                      g.floor = ifelse(apt1.own$floor<=5,"저층",
                                ifelse(apt1.own$floor>5 & apt1.own$floor<=20,"중층","고층"))
                      )

 # 동일주택 가정 match코드 생성 : 동읍면+본번+부번+층그룹+면적
apt1.own$matchid <- as.factor(paste(apt1.own$address,apt1.own$bon,apt1.own$bu,apt1.own$g.floor,apt1.own$m_usearea,sep="/"))

# 거래시점에 1부터 순서대로 숫자 부여(편의상 만드는 거임)
nmonth <- unique(select(apt1.own,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:48)

# apt1 데이터 & nmonth1 데이터 병합
apt2.own <- merge(apt1.own,nmonth,key="sale_date")
apt2.own <- select(apt2.own,-c(address,bon,bu))

# 1회 거래 주택 사례 제거
sale.freq_aptown <- data.frame(table(apt2.own$matchid))     # matchid별 거래횟수
names(sale.freq_aptown) <- c("matchid","freq")

table.freq_aptown <- data.frame(table(sale.freq_aptown$freq))  # 거래횟수별 빈도수 check(1회 거래:271개,98% 이용)

apt3.own <- merge(apt2.own,sale.freq_aptown,key="matchid")  # apt2데이터에 거래횟수 병합
apt3.own <- subset(apt3.own,freq>1)                         # 거래횟수 1회 matchid 제거
apt3.own <- arrange(apt3.own,matchid,nmonth)

# 반복거래쌍 구성
apt3.own <- mutate(apt3.own,
               log_unit_price=log(unit_price),
               lag_matchid=lag(matchid,1),
               lag_sale_date=lag(sale_date,1),
               lag_nmonth=lag(nmonth),
               lag_log_unit_price=lag(log_unit_price,1)
               )
apt4.own <- filter(apt3.own, matchid==lag_matchid)                               # matchid 불일치 행 & 시점 동일 행 제거
apt4.own$lnp_growth <- apt4.own$log_unit_price - apt4.own$lag_log_unit_price     # 거래쌍 간 가격차이 계산

# 변수 정리(여기서부터터)
index.aptown <- select(apt4.own, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.aptown) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.aptown$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.aptown$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1               #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.aptown_rs <- cbind(index.aptown, d_time)
index.aptown_rs <- select(index.aptown_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기


# OLS 반복매매지수 산정 
sejong.aptown_model <- lm(lnp_growth~.-1, data=index.aptown_rs)
result_sejong.aptown <- data.frame(sejong.aptown_model[1])

 #결과
result_sejong.aptown$nmonth <- c(2:48)
result_sejong.aptown <- merge(result_sejong.aptown,nmonth,key="nmonth")
result_sejong.aptown$apt.index <- exp(result_sejong.aptown$coefficients)*100    #월별 지수
result_sejong.aptown$pre.index <- lag(result_sejong.aptown$apt.index)           #월별 가격변동률 산정
result_sejong.aptown[1,5] <- 100
result_sejong.aptown$rate.aptindex <- ((result_sejong.aptown$apt.index/result_sejong.aptown$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.own_apt <- result_sejong.aptown[,c("sale_date","apt.index","rate.aptindex")]
sejong.own_apt <- rbind(index.first,sejong.own_apt)

write_csv(sejong.own_apt, "D:/진/LH COMPAS 공모전_세종/매매지수_아파트_동일주택가정1.csv")


##### 단독다가구 매매지수 #####
# 세종시 단독다가구 매매 실거래가 불러오기
dd0.own <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_단독다가구(매매)_실거래가.csv")

# 필요 변수 남기기 및 변수명 변경(편의상)
dd1.own <- dd0.own %>%
  filter(is.na(해제사유발생일)==TRUE) %>%
  select(-c(계약일,해제사유발생일,건축년도)) %>%
  rename("sale_date"=계약년월,"address"=시군구,"m_landarea"=대지면적...,"price"=거래금액.만원.)

# 필요 변수 추가 생성 : 단위가격, 동일주택가정 변수
dd1.own$unit_price <- dd1.own$price / dd1.own$m_landarea          #단위가격 생성 : 거래금액/대지면적

# 동일주택 가정 match코드 생성 : 동읍면
dd1.own$matchid <- dd1.own$address

# 거래시점에 1부터 순서대로 숫자 부여
nmonth <- unique(select(dd1.own,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:46)

# dd1.own 데이터 & nmonth1 데이터 병합
dd2.own <- merge(dd1.own,nmonth,key="sale_date")
dd2.own <- select(dd2.own,-c(address))

#2개월 중첩용
dd2.own_2m <- filter(dd2.own,nmonth<46)       #마지막 시점 제거
dd2.own_2m$nmonth.2m <- dd2.own_2m$nmonth+1

nmonth.2m <- rename(nmonth,"sale_date.2m"=sale_date,"nmonth.2m"=nmonth)
dd2.own_2m <- merge(dd2.own_2m,nmonth.2m,key="nmonth.2m")
dd2.own_2m <- select(dd2.own_2m,-c(nmonth,sale_date))
dd2.own_2m <- rename(dd2.own_2m,"sale_date"=sale_date.2m,"nmonth"=nmonth.2m)

dd2.own <- rbind(dd2.own,dd2.own_2m)

# 1회 거래 주택 사례 제거
sale.freq_ddown <- data.frame(table(dd2.own$matchid))     # matchid별 거래횟수
names(sale.freq_ddown) <- c("matchid","freq")

table.freq_own <- data.frame(table(sale.freq_ddown$freq))  # 거래횟수별 빈도수 check(1회 거래:12개)

dd3.own <- merge(dd2.own,sale.freq_ddown,key="matchid")    # dd2.own데이터에 거래횟수 병합
dd3.own <- subset(dd3.own,freq>1)                          # 거래횟수 1회 matchid 제거
dd3.own <- arrange(dd3.own,matchid,nmonth)

# 반복거래쌍 구성
dd3.own <- mutate(dd3.own,
                  log_unit_price=log(unit_price),
                  lag_matchid=lag(matchid,1),
                  lag_sale_date=lag(sale_date,1),
                  lag_nmonth=lag(nmonth),
                  lag_log_unit_price=lag(log_unit_price,1)
)
dd4.own <- filter(dd3.own, matchid==lag_matchid)                             # matchid 불일치 행 제거
dd4.own$lnp_growth <- dd4.own$log_unit_price - dd4.own$lag_log_unit_price    # 거래쌍 간 가격차이 계산

# 변수 정리
index.ddown <- select(dd4.own, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.ddown) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.ddown$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.ddown$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1               #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.ddown_rs <- cbind(index.ddown, d_time)
index.ddown_rs <- select(index.ddown_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기

# OLS 반복매매지수 산정 
sejong.ddown_model <- lm(lnp_growth~.-1, data=index.ddown_rs)
result_sejong.ddown <- data.frame(sejong.ddown_model[1])

#결과
result_sejong.ddown$nmonth <- c(2:46)
result_sejong.ddown <- merge(result_sejong.ddown,nmonth,key="nmonth")
result_sejong.ddown$dd.index <- exp(result_sejong.ddown$coefficients)*100    #월별 지수
result_sejong.ddown$pre.index <- lag(result_sejong.ddown$dd.index)           #월별 가격변동률 산정
result_sejong.ddown[1,5] <- 100
result_sejong.ddown$rate.ddindex <- ((result_sejong.ddown$dd.index/result_sejong.ddown$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.own_dd <- result_sejong.ddown[,c("sale_date","dd.index","rate.ddindex")]
sejong.own_dd <- rbind(index.first,sejong.own_dd)

write.csv(sejong.own_dd, "D:/진/LH COMPAS 공모전_세종/2개월중첩매매지수_단독다가구_동일주택가정1.csv")





########## 전세지수--- ##########

##### 아파트 전세지수 ##### 
apt0.noown <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_아파트(전월세)_실거래가.csv")
apt0.ch <- filter(apt0.noown,전월세구분=="전세")

# 필요 변수 남기기 및 변수명 변경(편의상)
apt1.ch <- apt0.ch %>%
  select(-c(계약일,단지명,월세.만원.,건축년도)) %>% 
  rename("sale_date"=계약년월,"address"=시군구,"bon"=본번,"bu"=부번,"m_usearea"=전용면적...,"deposit"=보증금.만원.,"floor"=층)

# 필요 변수 추가 생성 : 단위가격, 동일주택가정 변수(층그룹)
apt1.ch$unit_deposit <- apt1.ch$deposit / apt1.ch$m_usearea         #단위보증금 생성 : 거래보증금/전용면적

# 동일주택가정 변수(면적그룹, 층그룹)
apt1.ch <- transform(apt1.ch,
                      g.floor = ifelse(apt1.ch$floor<=5,"저층",
                                ifelse(apt1.ch$floor>5 & apt1.ch$floor<=20,"중층","고층"))
)

# 동일주택 가정 match코드 생성 : 동읍면+본번+부번+층그룹+면적
apt1.ch$matchid <- as.factor(paste(apt1.ch$address, apt1.ch$bon, apt1.ch$bu, apt1.ch$g.floor, apt1.ch$m_usearea, sep="/"))

# 거래시점에 1부터 순서대로 숫자 부여
nmonth <- unique(select(apt1.ch,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:48)

# apt1 데이터 & nmonth1 데이터 병합
apt2.ch <- merge(apt1.ch,nmonth,key="sale_date")
apt2.ch <- select(apt2.ch,-c(address,bon,bu))

# 1회 거래 주택 사례 제거
sale.freq_aptch <- data.frame(table(apt2.ch$matchid))     # matchid별 거래횟수
names(sale.freq_aptch) <- c("matchid","freq")

table.freq_aptch <- data.frame(table(sale.freq_aptch$freq))  # 거래횟수별 빈도수 check(1회 거래:305개_)

apt3.ch <- merge(apt2.ch,sale.freq_aptch,key="matchid")      # apt2데이터에 거래횟수 병합
apt3.ch <- subset(apt3.ch,freq>1)                      # 거래횟수 1회 matchid 제거
apt3.ch <- arrange(apt3.ch,matchid,nmonth)

# 반복거래쌍 구성
apt3.ch <- mutate(apt3.ch,
                  log_unit_deposit=log(unit_deposit),
                  lag_matchid=lag(matchid,1),
                  lag_sale_date=lag(sale_date,1),
                  lag_nmonth=lag(nmonth),
                  lag_log_unit_deposit=lag(log_unit_deposit,1)
)
apt4.ch <- filter(apt3.ch, matchid==lag_matchid)                                 # matchid 불일치 행 & 시점 동일 행 제거
apt4.ch$lnp_growth <- apt4.ch$log_unit_deposit - apt4.ch$lag_log_unit_deposit    # 거래쌍 간 가격차이 계산

# 변수 정리
index.aptch <- select(apt4.ch, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.aptch) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.aptch$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.aptch$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1                  #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.aptch_rs <- cbind(index.aptch, d_time)
index.aptch_rs <- select(index.aptch_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기

# OLS 반복매매지수 산정 
sejong.aptch_model <- lm(lnp_growth~.-1, data=index.aptch_rs)
result_sejong.aptch <- data.frame(sejong.aptch_model[1])

#결과
result_sejong.aptch$nmonth <- c(2:48)
result_sejong.aptch <- merge(result_sejong.aptch,nmonth,key="nmonth")
result_sejong.aptch$apt.index <- exp(result_sejong.aptch$coefficients)*100    #월별 지수
result_sejong.aptch$pre.index <- lag(result_sejong.aptch$apt.index)           #월별 가격변동률 산정
result_sejong.aptch[1,5] <- 100
result_sejong.aptch$rate.aptindex <- ((result_sejong.aptch$apt.index/result_sejong.aptch$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.ch_apt <- result_sejong.aptch[,c("sale_date","apt.index","rate.aptindex")]
sejong.ch_apt <- rbind(index.first,sejong.ch_apt)

write_csv(sejong.ch_apt, "D:/진/LH COMPAS 공모전_세종/전세지수_아파트_동일주택가정1.csv")



##### 단독다가구 전세지수 #####
dd0.noown <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_단독다가구(전월세)_실거래가.csv")
dd0.ch <- filter(dd0.noown,전월세구분=="전세")

# 필요 변수 남기기 및 변수명 변경(편의상)
dd1.ch <- dd0.ch %>%
  select(-c(계약일,도로조건,전월세구분,월세.만원.,건축년도)) %>% 
  rename("sale_date"=계약년월,"address"=시군구,"m_area"=계약면적...,"deposit"=보증금.만원.)

# 필요 변수 추가 생성 : 단위가격, 동일주택가정 변수
dd1.ch$unit_deposit <- dd1.ch$deposit / dd1.ch$m_area     #단위보증금 생성 : 거래보증금/전용면적

# 동일주택 가정 match코드 생성 : 동읍면
dd1.ch$matchid <- dd1.ch$address

# 거래시점에 1부터 순서대로 숫자 부여
nmonth <- unique(select(dd1.ch,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:48)

# apt1 데이터 & nmonth1 데이터 병합
dd2.ch <- merge(dd1.ch,nmonth,key="sale_date")
dd2.ch <- select(dd2.ch,-c(address))


# 1회 거래 주택 사례 제거
sale.freq_ddch <- data.frame(table(dd2.ch$matchid))     # matchid별 거래횟수
names(sale.freq_ddch) <- c("matchid","freq")



table.freq_ddch <- data.frame(table(sale.freq_ddch$freq))  # 거래횟수별 빈도수 check(1회 거래:28개_99%)

dd3.ch <- merge(dd2.ch,sale.freq_ddch,key="matchid")      # dd2.ch데이터에 거래횟수 병합
dd3.ch <- subset(dd3.ch,freq>1)                      # 거래횟수 1회 matchid 제거
dd3.ch <- arrange(dd3.ch,matchid,nmonth)

# 반복거래쌍 구성
dd3.ch <- mutate(dd3.ch,
                 log_unit_deposit=log(unit_deposit),
                 lag_matchid=lag(matchid,1),
                 lag_sale_date=lag(sale_date,1),
                 lag_nmonth=lag(nmonth),
                 lag_log_unit_deposit=lag(log_unit_deposit,1)
)
dd4.ch <- filter(dd3.ch, matchid==lag_matchid)                                # matchid 불일치 행 & 시점 동일 행 제거
dd4.ch$lnp_growth <- dd4.ch$log_unit_deposit - dd4.ch$lag_log_unit_deposit    # 거래쌍 간 가격차이 계산

summary(dd4.ch)

# 변수 정리
index.ddch <- select(dd4.ch, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.ddch) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.ddch$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.ddch$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1                  #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.ddch_rs <- cbind(index.ddch, d_time)
index.ddch_rs <- select(index.ddch_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기

summary(index.ddch_rs$lnp_growth)
summary(index.ddch_rs)

# OLS 반복매매지수 산정 
sejong.ddch_model <- lm(lnp_growth~.-1, data=index.ddch_rs)
result_sejong.ddch <- data.frame(sejong.ddch_model[1])

#결과
result_sejong.ddch$nmonth <- c(2:48)
result_sejong.ddch <- merge(result_sejong.ddch,nmonth,key="nmonth")
result_sejong.ddch$dd.index <- exp(result_sejong.ddch$coefficients)*100    #월별 지수
result_sejong.ddch$pre.index <- lag(result_sejong.ddch$dd.index)           #월별 가격변동률 산정
result_sejong.ddch[1,5] <- 100
result_sejong.ddch$rate.ddindex <- ((result_sejong.ddch$dd.index/result_sejong.ddch$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.ch_dd <- result_sejong.ddch[,c("sale_date","dd.index","rate.ddindex")]
sejong.ch_dd <- rbind(index.first,sejong.ch_dd)

summary(index.ddch_rs)

write_csv(sejong.ch_dd, "D:/진/LH COMPAS 공모전_세종/전세지수_단독다가구_동일주택가정1.csv")



########## 월세지수--- ##########

##### 아파트 월세지수 #####
apt0.noown <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_아파트(전월세)_실거래가.csv")  #전월세전환율 위해 월세 분리는 이후에

# 필요 변수 남기기 및 변수명 변경(편의상)
apt1.noown <- apt0.noown %>%
  select(-c(계약일,단지명,건축년도)) %>% 
  rename("sale_date"=계약년월,"address"=시군구,"bon"=본번,"bu"=부번,"m_usearea"=전용면적...,"deposit"=보증금.만원.,"rent"=월세.만원.,"floor"=층)

# 동일주택가정 및 중위전세용 변수(면적그룹, 층그룹)
apt1.noown <- transform(apt1.noown,
                        g.area = ifelse(apt1.noown$m_usearea<=40,"소형",
                                 ifelse(apt1.noown$m_usearea>40 & apt1.noown$m_usearea<=60,"중소형",
                                 ifelse(apt1.noown$m_usearea>60 & apt1.noown$m_usearea<=85,"중형",
                                 ifelse(apt1.noown$m_usearea>85 & apt1.noown$m_usearea<=135,"중대형","대형")))),
                        g.floor = ifelse(apt1.noown$floor<=5,"저층",
                                  ifelse(apt1.noown$floor>5 & apt1.noown$floor<=20,"중층","고층"))
)

# 동일주택 가정 & 증위전세 match코드 생성 : 동일주택(동읍면+본번+부번+층그룹+면적), 중위전세(거래시점,주소,면적그룹)
apt1.noown$matchid <- as.factor(paste(apt1.noown$address, apt1.noown$bon, apt1.noown$bu, apt1.noown$g.floor, apt1.noown$m_usearea, sep="/"))
apt1.noown$mid.matchid <- as.factor(paste(apt1.noown$sale_date,apt1.noown$address,apt1.noown$g.area,sep="/"))

# 중위전세 도출 : mid.matchid별 중위전세값(행정동-면적그룹별)
deposit_apt <- apt1.noown %>% filter(전월세구분=="전세") %>% select(sale_date,deposit,mid.matchid)
mid.deposit_apt <- deposit_apt %>% group_by(mid.matchid) %>% summarise(mid.deposit=median(deposit))

check <- filter(mid.deposit_apt,mid.deposit<10000)
check <- filter(apt2.w,dr.change<0)

apt2.noown <- merge(apt1.noown,mid.deposit_apt,key="mid.matchid")

# 월세 데이터 남기기
apt2.w <- filter(apt2.noown,전월세구분=="월세")
apt2.w$dr.rate <- apt2.w$deposit/apt2.w$rent   # 보증금/월세 비율 산정하여 전세 가까운 데이터 제거
apt2.w <- filter(apt2.w,dr.rate<=300)          # 이창무 외 2인(2020)을 참고하여 보증금/월세 비율 300배 이하만 활용 : 이상치 제거

# 전월세전환율 산정 : 보증금->월세 전환
apt2.w$dr.change <- (apt2.w$rent*12)/(apt2.w$mid.deposit-apt2.w$deposit)  # (월세x12)/(중위전세-보증금)을 통해 전월세전환율 산정 : 이창무,이현석,정의철(2002) 참고
apt2.w$std.rent <- apt2.w$deposit*apt2.w$dr.change/12 + apt2.w$rent       # 표준월세 산정= 보증금x전월세전환율/12+월세
apt2.w$unit_std.rent <- apt2.w$std.rent/apt2.w$m_usearea                  # 단위면적당 월세 산정

# 거래시점에 1부터 순서대로 숫자 부여
nmonth <- unique(select(apt2.w,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:48)

# apt1 데이터 & nmonth1 데이터 병합
apt3.w <- merge(apt2.w,nmonth,key="sale_date")
apt3.w <- select(apt3.w,-c(address,bon,bu))

# 1회 거래 주택 사례 제거
sale.freq_aptw <- data.frame(table(apt3.w$matchid))     # matchid별 거래횟수
names(sale.freq_aptw) <- c("matchid","freq")

table.freq_aptw <- data.frame(table(sale.freq_aptw$freq))  # 거래횟수별 빈도수 check(1회 거래:594개_95%이용)

apt4.w <- merge(apt3.w,sale.freq_aptw,key="matchid")      # apt2데이터에 거래횟수 병합
apt4.w <- subset(apt4.w,freq>1)                      # 거래횟수 1회 matchid 제거
apt4.w <- arrange(apt4.w,matchid,nmonth)

# 반복거래쌍 구성
apt4.w <- mutate(apt4.w,
                  log_unit_std.rent=log(unit_std.rent),
                  lag_matchid=lag(matchid,1),
                  lag_sale_date=lag(sale_date,1),
                  lag_nmonth=lag(nmonth),
                  lag_log_unit_std.rent=lag(log_unit_std.rent,1)
)
apt5.w <- filter(apt4.w, matchid==lag_matchid)                                 # matchid 불일치 행 & 시점 동일 행 제거
apt5.w$lnp_growth <- apt5.w$log_unit_std.rent - apt5.w$lag_log_unit_std.rent   # 거래쌍 간 가격차이 계산
apt5.w <- na.omit(apt5.w)

# 변수 정리
index.aptw <- select(apt5.w, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.aptw) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.aptw$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.aptw$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1                  #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.aptw_rs <- cbind(index.aptw, d_time)
index.aptw_rs <- select(index.aptw_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기
index.aptw_rs <- filter(index.aptw_rs,is.infinite(index.aptw_rs$lnp_growth)==FALSE)

# OLS 반복매매지수 산정 
sejong.aptw_model <- lm(lnp_growth~.-1, data=index.aptw_rs)
result_sejong.aptw <- data.frame(sejong.aptw_model[1])

#결과
result_sejong.aptw$nmonth <- c(2:48)
result_sejong.aptw <- merge(result_sejong.aptw,nmonth,key="nmonth")
result_sejong.aptw$apt.index <- exp(result_sejong.aptw$coefficients)*100    #월별 지수
result_sejong.aptw$pre.index <- lag(result_sejong.aptw$apt.index)           #월별 가격변동률 산정
result_sejong.aptw[1,5] <- 100
result_sejong.aptw$rate.aptindex <- ((result_sejong.aptw$apt.index/result_sejong.aptw$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.w_apt <- result_sejong.aptw[,c("sale_date","apt.index","rate.aptindex")]
sejong.w_apt <- rbind(index.first,sejong.w_apt)

write_csv(sejong.w_apt, "D:/진/LH COMPAS 공모전_세종/월세지수_아파트_동일주택가정1.csv")


 # 월별 전월세전환율(지번-면적그룹별 중위값)
mid.drchange_apt <- apt2.w %>% group_by(sale_date) %>% summarise(mid.drchange=median(dr.change))
write_csv(mid.drchange_apt, "D:/진/LH COMPAS 공모전_세종/전월세전환율_아파트_동일주택가정1.csv")



##### 단독다가구 월세지수 #####
dd0.noown <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_단독다가구(전월세)_실거래가.csv")  #전월세전환율 위해 월세 분리는 이후에

# 필요 변수 남기기 및 변수명 변경(편의상)
dd1.noown <- dd0.noown %>%
  select(-c(계약일,건축년도)) %>% 
  rename("sale_date"=계약년월,"address"=시군구,"m_area"=계약면적...,"deposit"=보증금.만원.,"rent"=월세.만원.)

# 동일주택가정 및 중위전세용 변수
dd1.noown$matchid <- dd1.noown$address

# 중위전세 도출 : mid.matchid별 중위전세값(행정동-면적그룹별)
deposit_dd <- dd1.noown %>% filter(전월세구분=="전세") %>% select(sale_date,deposit,matchid)
mid.deposit_dd <- deposit_dd %>% group_by(matchid) %>% summarise(mid.deposit=median(deposit))

dd2.noown <- merge(dd1.noown,mid.deposit_dd,key="mid.matchid")

# 월세 데이터 남기기
dd2.w <- filter(dd2.noown,전월세구분=="월세")
dd2.w$dr.rate <- dd2.w$deposit/dd2.w$rent   # 보증금/월세 비율 산정하여 전세 가까운 데이터 제거
dd2.w <- filter(dd2.w,dr.rate<=300)         # 이창무 외 2인(2020)을 참고하여 보증금/월세 비율 300배 이하만 활용 : 이상치 제거

# 전월세전환율 산정 : 보증금->월세 전환
dd2.w$dr.change <- (dd2.w$rent*12)/(dd2.w$mid.deposit-dd2.w$deposit)  # (월세x12)/(중위전세-보증금)을 통해 전월세전환율 산정 : 이창무,이현석,정의철(2002) 참고
dd2.w$std.rent <- dd2.w$deposit*dd2.w$dr.change/12 + dd2.w$rent       # 표준월세 산정= 보증금x전월세전환율/12+월세
dd2.w$unit_std.rent <- dd2.w$std.rent/dd2.w$m_area                    # 단위면적당 월세 산정

# 거래시점에 1부터 순서대로 숫자 부여
nmonth <- unique(select(dd2.w,sale_date))
nmonth <- arrange(nmonth,sale_date)
nmonth$nmonth <- c(1:48)

# apt1 데이터 & nmonth1 데이터 병합
dd3.w <- merge(dd2.w,nmonth,key="sale_date")
dd3.w <- select(dd3.w,-c(address))

#2개월 중첩용
dd3.w_2m <- filter(dd3.w,nmonth<46)       #마지막 시점 제거
dd3.w_2m$nmonth.2m <- dd3.w_2m$nmonth+1

nmonth.2m <- rename(nmonth,"sale_date.2m"=sale_date,"nmonth.2m"=nmonth)
dd3.w_2m <- merge(dd3.w_2m,nmonth.2m,key="nmonth.2m")
dd3.w_2m <- select(dd3.w_2m,-c(nmonth,sale_date))
dd3.w_2m <- rename(dd3.w_2m,"sale_date"=sale_date.2m,"nmonth"=nmonth.2m)

dd3.w <- rbind(dd3.w,dd3.w_2m)


# 1회 거래 주택 사례 제거
sale.freq_ddw <- data.frame(table(dd3.w$matchid))     # matchid별 거래횟수
names(sale.freq_ddw) <- c("matchid","freq")

table.freq_ddw <- data.frame(table(sale.freq_ddw$freq))  # 거래횟수별 빈도수 check(1회 거래:16개_98%사용)

dd4.w <- merge(dd3.w,sale.freq_ddw,key="matchid")      # dd2데이터에 거래횟수 병합
dd4.w <- subset(dd4.w,freq>1)                      # 거래횟수 1회 matchid 제거
dd4.w <- arrange(dd4.w,matchid,nmonth)

# 반복거래쌍 구성
dd4.w <- mutate(dd4.w,
                 log_unit_std.rent=log(unit_std.rent),
                 lag_matchid=lag(matchid,1),
                 lag_sale_date=lag(sale_date,1),
                 lag_nmonth=lag(nmonth),
                 lag_log_unit_std.rent=lag(log_unit_std.rent,1)
)
dd5.w <- filter(dd4.w, matchid==lag_matchid)                                 # matchid 불일치 행 & 시점 동일 행 제거
dd5.w$lnp_growth <- dd5.w$log_unit_std.rent - dd5.w$lag_log_unit_std.rent   # 거래쌍 간 가격차이 계산
dd5.w <- na.omit(dd5.w)

# 변수 정리
index.ddw <- select(dd5.w, matchid, sale_date, lag_sale_date, lnp_growth)
names(index.ddw) <- c("matchid","sale_date","pre_sale_date","lnp_growth")

# 시점더미 생성
f2.month <- factor(index.ddw$sale_date)       #2번째 거래 = 1 else = 0
f1.month <- factor(index.ddw$pre_sale_date)   #1번째 거래 = 1 else = 0

d_time2 <- model.matrix(~f2.month)
d_time1 <- model.matrix(~f1.month)
d_time <- d_time2 - d_time1                  #2번째 거래 = 1 1번째 거래 = -1 else = 0
d_time <- data.frame(d_time)

index.ddw_rs <- cbind(index.ddw, d_time)
index.ddw_rs <- select(index.ddw_rs, -c(matchid, sale_date, pre_sale_date, X.Intercept.))    # 종속변수(가격변동), 시점더미만 남기기
index.ddw_rs <- filter(index.ddw_rs,is.infinite(index.ddw_rs$lnp_growth)==FALSE)

# OLS 반복매매지수 산정 
sejong.ddw_model <- lm(lnp_growth~.-1, data=index.ddw_rs)
result_sejong.ddw <- data.frame(sejong.ddw_model[1])

#결과
result_sejong.ddw$nmonth <- c(2:48)
result_sejong.ddw <- merge(result_sejong.ddw,nmonth,key="nmonth")
result_sejong.ddw$dd.index <- exp(result_sejong.ddw$coefficients)*100    #월별 지수
result_sejong.ddw$pre.index <- lag(result_sejong.ddw$dd.index)           #월별 가격변동률 산정
result_sejong.ddw[1,5] <- 100
result_sejong.ddw$rate.ddindex <- ((result_sejong.ddw$dd.index/result_sejong.ddw$pre.index)-1)*100

index.first <- c(201701,100,NA)   #첫 번째 시점 추가(2017.01)
sejong.w_dd <- result_sejong.ddw[,c("sale_date","dd.index","rate.ddindex")]
sejong.w_dd <- rbind(index.first,sejong.w_dd)

write.csv(sejong.w_dd, "D:/진/LH COMPAS 공모전_세종/2개월 중첩월세지수_단독다가구_동일주택가정1.csv")


# 월별 전월세전환율(지번-면적그룹별 중위값)
mid.drchange_dd <- dd2.w %>% group_by(sale_date) %>% summarise(mid.drchange=median(dr.change))
write_csv(mid.drchange_dd, "D:/진/LH COMPAS 공모전_세종/전월세전환율_단독다가구_동일주택가정1.csv")





################################################## 인구이동--- ##################################################

### 국내인구이동통계(2017~2019) : 세종 제외 이동데이터 사용
# 전입,전출 데이터 불러오기(데이터 크기 때문에 cbind 안되는듯)
move.17 <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/국내인구이동통계_2017.csv")
move.18 <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/국내인구이동통계_2018.csv")
move.19 <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/국내인구이동통계_2019.csv")

# 전입전출지별 이동량 산정
 # 2017년
move.17 <- select(move.17,전입행정_시도,전입행정_시군구,전입년,전출행정_시도,전출행정_시군구)
move.17$des_code <- paste(move.17$전입행정_시도,move.17$전입행정_시군구,sep="")
move.17$org_code <- paste(move.17$전출행정_시도,move.17$전출행정_시군구,sep="")
move.17$od_code <- paste(move.17$org_code,move.17$des_code,sep="")

flow.17 <- data.frame(table(move.17$od_code))                  #od_code별 이주량 산정(od_code별 빈도)
flow.17 <- rename(flow.17,"od_code"=Var1,"flow17"=Freq)
flow.17$org_code <- as.numeric(substr(flow.17$od_code,1,5))    #전출지 변수 분리
flow.17$des_code <- as.numeric(substr(flow.17$od_code,6,10))   #전입지 변수 변리

 # 2018년
move.18 <- select(move.18,전입행정_시도,전입행정_시군구,전입년,전출행정_시도,전출행정_시군구)
move.18$des_code <- paste(move.18$전입행정_시도,move.18$전입행정_시군구,sep="")
move.18$org_code <- paste(move.18$전출행정_시도,move.18$전출행정_시군구,sep="")
move.18$od_code <- paste(move.18$org_code,move.18$des_code,sep="")

flow.18 <- data.frame(table(move.18$od_code))                  #od_code별 이주량 산정(od_code별 빈도)
flow.18 <- rename(flow.18,"od_code"=Var1,"flow18"=Freq)
flow.18$org_code <- as.numeric(substr(flow.18$od_code,1,5))    #전출지 변수 분리
flow.18$des_code <- as.numeric(substr(flow.18$od_code,6,10))   #전입지 변수 변리

 # 2019년
move.19 <- select(move.19,전입행정_시도,전입행정_시군구,전입년,전출행정_시도,전출행정_시군구)
move.19$des_code <- paste(move.19$전입행정_시도,move.19$전입행정_시군구,sep="")
move.19$org_code <- paste(move.19$전출행정_시도,move.19$전출행정_시군구,sep="")
move.19$od_code <- paste(move.19$org_code,move.19$des_code,sep="")

flow.19 <- data.frame(table(move.19$od_code))                  #od_code별 이주량 산정(od_code별 빈도)
flow.19 <- rename(flow.19,"od_code"=Var1,"flow19"=Freq)
flow.19$org_code <- as.numeric(substr(flow.19$od_code,1,5))    #전출지 변수 분리
flow.19$des_code <- as.numeric(substr(flow.19$od_code,6,10))   #전입지 변수 변리


flow0 <- merge(flow.17,flow.18,key="od_code")                   #2017-2019년 인구이동 데이터 병합
flow0 <- merge(flow0,flow.19,key="od_code")
flow0 <- flow0[,c("od_code","org_code","des_code","flow17","flow18","flow19")]


##### 세종시 수요권 파악 #####
# 시군구별 전출률, 전입률 산정
org.outflow <- flow0 %>% group_by(org_code) %>%    #시군구별 총 전출량
  summarise(total.outflow17=sum(flow17),
            total.outflow18=sum(flow18),
            total.outflow19=sum(flow19))

des.inflow <- flow0 %>% group_by(des_code) %>%     #시군구별 총 전입량
  summarise(total.inflow17=sum(flow17),
            total.inflow18=sum(flow18),
            total.inflow19=sum(flow19))

flow1 <- merge(flow0,org.outflow,key="org_code")   #총 전출량,전입량 병합
flow1 <- merge(flow1,des.inflow,key="des_code")

flow1 <- mutate(flow1,                             #전출률, 전입율 산정 
                prob_out17=flow17/total.outflow17, prob_out18=flow18/total.outflow18, prob_out19=flow19/total.outflow19,
                prob_in17=flow17/total.inflow17, prob_in18=flow18/total.inflow18, prob_in19=flow19/total.inflow19
                )

 # 시군구별 재고,신규주택 호수 불러오기
nhouse <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/전국 시군구별 신규,재고주택 호수.csv")
nhouse <- rename(nhouse,"des_code"=법정동명코드,
                 "des_stock17"=재고주택_2017,"des_stock18"=재고주택_2018,"des_stock19"=재고주택_2019,
                 "des_new17"=신규주택_2017,"des_new18"=신규주택_2018,"des_new19"=신규주택_2019)

flow2 <- merge(flow1,nhouse,key="des_code")        #전출,전입율 데이터에 재고,신규주택 데이터 병합


 # 재고주택, 신규주택 회전율 산정(수용가능주택 산정용) : 연도별로 산정
select.sejong <- flow2 %>%
  select(des_code,total.outflow17,total.outflow18,total.outflow19,des_stock17,des_stock18,des_stock19,des_new17,des_new18,des_new19) %>%
  filter(des_code==36110)

select17.sejong <- lm(total.outflow17 ~ des_stock17 + des_new17 - 1, data = select.sejong)   #2017년 재고 회전율 : 0.22
select18.sejong <- lm(total.outflow18 ~ des_stock18 + des_new18 - 1, data = select.sejong)   #2018년 재고 회전율 : 0.20
select19.sejong <- lm(total.outflow19 ~ des_stock19 + des_new19 - 1, data = select.sejong)   #2019년 재고 회전율 : 0.18

result.select17 <- data.frame(select17.sejong[1])
result.select18 <- data.frame(select18.sejong[1])
result.select19 <- data.frame(select19.sejong[1])

result.select17 <- rename(result.select17,"rotate17.stock"=coefficients)
result.select18 <- rename(result.select18,"rotate18.stock"=coefficients)
result.select19 <- rename(result.select19,"rotate19.stock"=coefficients)

result.select <- cbind(result.select17,result.select18,result.select19)
result.select <- result.select[1,]
result.select <- mutate(result.select,
                        des_code=36110,
                        rotate17.new=1-rotate17.stock,
                        rotate18.new=1-rotate18.stock,
                        rotate19.new=1-rotate19.stock
                        )

# 세종시 수용가능주택 1호 선택확률 산정(3개년 평균)
flow3 <- merge(flow2,result.select,key="des_code")    #flow2에 산정한 회전율 병합

flow3 <- mutate(flow3,
                des_select17=rotate17.stock*des_stock17+rotate17.new*des_new17,
                des_select18=rotate18.stock*des_stock18+rotate18.new*des_new17,
                des_select19=rotate19.stock*des_stock19+rotate19.new*des_new17,
                prob_new17=prob_out17/des_select17,
                prob_new18=prob_out18/des_select18,
                prob_new19=prob_out19/des_select19,
                prob_new=(prob_new17+prob_new18+prob_new19)/3,
                prob_out=(prob_out17+prob_out18+prob_out19)/3,
                prob_in=(prob_in17+prob_in18+prob_in19)/3
                )

# 세종시 수용가능주택 1호 선택확률 기준 세종시 수요권 파악
org_name <- nhouse %>%                          #전입지 지역명 생성
  select(시도,시군구,des_code) %>%
  rename("org_code"=des_code) %>%
  mutate(org_name=paste(시도,시군구,sep=" "))
org_name <- org_name[,c(3,4)]

demand.sejong <- select(flow3,des_code,org_code,prob_new,prob_in)
demand.sejong <- merge(demand.sejong,org_name,key="org_code")
demand.sejong <- demand.sejong[,c("org_code","org_name","des_code","prob_new","prob_in")]
demand.sejong <- arrange(demand.sejong,desc(prob_new))

write_csv(demand.sejong, "D:/진/LH COMPAS 공모전_세종/세종시 수요권.csv")


##### 세종시 연도별 전입량,전출량(가구단위) #####
outflow.17 <- flow.17 %>% filter(org_code==36110) %>% summarise(out.sejong17=sum(flow17))
outflow.18 <- flow.18 %>% filter(org_code==36110) %>% summarise(out.sejong18=sum(flow18))
outflow.19 <- flow.19 %>% filter(org_code==36110) %>% summarise(out.sejong19=sum(flow19))
outflow.sejong <- cbind(outflow.17,outflow.18,outflow.19)

inflow.17 <- flow.17 %>% filter(des_code==36110) %>% summarise(des.sejong17=sum(flow17))
inflow.18 <- flow.18 %>% filter(des_code==36110) %>% summarise(des.sejong18=sum(flow18))
inflow.19 <- flow.19 %>% filter(des_code==36110) %>% summarise(des.sejong19=sum(flow19))
inflow.sejong <- cbind(inflow.17,inflow.18,inflow.19)

str(inflow.sejong)
str(outflow.sejong)




##### 분양권 가격 추이 및 거래량 #####
own0 <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_분양권_실거래가.csv")

# 필요 변수 남기기 및 변수명 변경(편의상)
own1 <- own0 %>%
  filter(is.na(해제사유발생일)==TRUE) %>%
  select(-c(계약일,분.입구분,해제사유발생일)) %>%
  rename("sale_date"=계약년월,"address"=시군구,"ad_name"=단지명,"m_usearea"=전용면적...,"price"=거래금액.만원.,"floor"=층)

own1$unit_price <- own1$price / own1$m_usearea     #단위가격 생성 : 거래금액/전용면적

# 연월별 평균 단위면적당가격(지수화)
own1.uprice <- own1 %>% group_by(sale_date) %>% summarise(avg_unit.price=mean(unit_price))   #연월별 평균 단위면적당가격 산정
own1.uprice$index <- own1.uprice$avg_unit.price/413.9555*100                                 #지수화

# 연월별 분양권 거래량
own1.freq <- data.frame(table(own1$sale_date))
names(own1.freq) <- c("sale_date","freq")

own2 <- merge(own1.uprice,own1.freq,key="sale_date")
write_csv(own2, "D:/진/LH COMPAS 공모전_세종/세종시 분양권 평당가격 지수화 & 거래량.csv")


### 읍면동별 주택유형 분포 ###
#세종시 표제부 데이터 불러오기
htype0 <- read.csv("D:/진/LH COMPAS 공모전_세종/SBJ_2102_001/세종시_표제부.csv")

#주택만 남기기(단독주택, 공동주택)
htype1 <- htype0 %>%
  select(대지위치, 법정동코드,주용도코드명) %>%
  filter(주용도코드명 %in% c("단독주택","공동주택")) %>% rename("address"=대지위치,"ad.code"=법정동코드,"htype"=주용도코드명)

htype1$dong <- substr(htype1$address,1,12)
htype1$matchid <- paste(htype1$dong,htype1$htype,sep="/")
htype1 <- select(htype1,-address)

#matchid별 빈도 구하기 : 읍면동별 단독주택, 공동주택 수
htype1.freq <- data.frame(table(htype1$matchid))
names(htype1.freq) <- c("matchid","freq")

htype1.freq$htype <- substr(htype1.freq$matchid,14,15)
htype1.freq <- arrange(htype1.freq,htype1.freq$htype,desc(htype1.freq$freq))

write.csv(htype1.freq, "D:/진/LH COMPAS 공모전_세종/세종시 읍면동별 주택유형 분포.csv")


