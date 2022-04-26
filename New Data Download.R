options('CombinedHarvesterCheckForUpdates' = TRUE)
library(CombinedHarvester)
library(tidyverse)
library(tseries)
library(fredr)
library(moments)

##Tokens

fredr_set_key("d80031904a0f06ff89e0dada3ccd6056")
authWithToken("eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJqaGN3YWxzaCIsImNsaWVudElkIjoxOTEzLCJoZmRiUElkIjoxMjA5NSwidGhpcmRQYXJ0eVVzYWdlIjpmYWxzZSwiaXBBZGRyZXNzIjoiNjcuMTY5Ljg1LjQyIiwiYXBwbGljYXRpb24iOjIsImFwcCI6IkNBU1RMRTIiLCJpc3MiOiJBbGJvdXJuZSIsImlhdCI6MTY1MDUxNTYzNywiZXhwIjoxNjUxNzI1MjM3fQ._urCW7UJpjOph5684FTF2C3zG__62s1D2_OHDLPneHc4UbrS6gaEaOib1HclrWXhjIZt0fFHpn5tJWR8xUtO2g","eyJhbGciOiJIUzUxMiJ9.eyJzdWIiOiJqaGN3YWxzaCIsImNsaWVudElkIjoxOTEzLCJoZmRiUElkIjoxMjA5NSwidGhpcmRQYXJ0eVVzYWdlIjpmYWxzZSwiaXBBZGRyZXNzIjoiNjcuMTY5Ljg1LjQyIiwiYXBwbGljYXRpb24iOjIsImFwcCI6IkNBU1RMRTIiLCJpc3MiOiJBbGJvdXJuZSIsImlhdCI6MTY1MDUxNTYzNywiZXhwIjoxNjY2MjQwNDM3fQ.iqFvBkWj7boF644qOzgitsI6QnVZ2gZzQYs5y7rjzHfcxnSVWc-aU50SS0FjeKFAvppNuINAQFwR9Xm12dF07g")

##Functions

#This calculates stress beta using APP_Equity
stressbeta<-function(x){
    fit<-lm(x~APP_Equity,data = stress.t)
    summary(fit)$coefficients[2]
}
##This calculates the beta
beta<-function(x){
    fit<-lm(x~APP_Equity,data=HedgeRSReturns.t)
    x<-summary(fit)$coefficients[2]
}

#Defining the series

index_Directional<-c(109706,109701,109707,109702,109828)
names_Directional<-c("CTA_EW","EM_Global_Macro_EW","GAA_EW","Global_Macro_EW","Insurance")

index_LongShort<-c(109738,109740,109742,109744,109746,109748)
names_LongShort<-c("LS_AsiaPacific_EW","LS_Emerging_EW","LS_Europe_EW","LS_Global_EW","LS_Japan_EW","LS_US_EW")

index_Event<-c(109777,109774,109775,109779)
names_Event<-c("Activist_EW","Distressed_EW","EM_FI_EW","Risk_Arb_EW")

index_RV<-c(109807,109795,109813,109797,109801,109805,109817)
names_RV<-c("CB_Arb_EW","RV_Credit_EW","RV_FI_EW","FEMN_EW","QEMN_EW","Stat_Arb_EW","Struct_Credit_EW")

index_Other<-c(109831,109785,109726,109821,109791,109789)
names_Other<-c("HedgeRS_EW","Multi_Event_EW","Multi_EW","Multi_RV_EW","Multi_Div_EW","Multi_Opp_EW")

index_APP<-c(1991,2133,2136,2134,2151)
names_APP<-c("APP_Equity","APP_FI","APP_Credit","APP_Commodities","APP_Vol")

indexID<-c(index_Directional,index_LongShort,index_Event,index_RV,index_Other,index_APP)
namesID<-c("date",names_Directional,names_LongShort,names_Event,names_RV,names_Other,names_APP)

start_date<-"2010-12-31"
end_date<-"2022-03-31"

#Download series
data_series<-getHedgersIndexReturns(currency="USD",start_date,indexID,end_date)  #downloads all series

#convert to dataframe
dt<-unlist(data_series$dates[[1]])
equal_weighted<-as.data.frame(dt)


for (i in 1:nrow(data_series)){
    temp<-unlist(data_series$returns[i])
    equal_weighted<-cbind(equal_weighted,temp)
}

colnames(equal_weighted)<-namesID

equal_weighted$date <- as.Date(as.POSIXct(equal_weighted$date/1000, origin = "1970-01-01"))


VIX<-fredr(
    series_id = "VIXCLS",
    observation_start = as.Date(start_date),
    observation_end = as.Date(end_date),
    frequency = "m"
)
names<-c("","","VIX")
colnames(VIX)<-names

Cash<-fredr(
    series_id = "DTB3",
    observation_start = as.Date(start_date),
    observation_end = as.Date(end_date),
    frequency = "m"
)
names<-c("","","Cash")
colnames(Cash)<-names

Cash<-(1+Cash[3]/100)^(1/12)-1

equal_weighted<-cbind(equal_weighted,VIX[3],Cash)
equal_weighted.t<-as_tibble(equal_weighted)

save(equal_weighted.t,file="EW_2022.RData")

##Asset Weighted

index_Directional<-c(109710,109711,109713,109714,109827)
names_Directional<-c("CTA_AW","EM_Global_Macro_AW","GAA_AW","Global_Macro_AW","Insurance_AW")

index_LongShort<-c(109739,109741,109743,109745,109747,109749)
names_LongShort<-c("LS_AsiaPacific_AW","LS_Emerging_AW","LS_Europe_AW","LS_Global_AW","LS_Japan_AW","LS_US_AW")

index_Event<-c(109778,109773,109776,109780)
names_Event<-c("Activist_AW","Distressed_AW","EM_FI_AW","Risk_Arb_AW")

index_RV<-c(109808,109796,109814,109798,109802,109806,109818)
names_RV<-c("CB_Arb_AW","RV_Credit_AW","RV_FI_AW","FEMN_AW","QEMN_AW","Stat_Arb_AW","Struct_Credit_AW")

index_Other<-c(109832,109786,109727,109822,109790,109792)
names_Other<-c("HedgeRS_AW","Multi_Event_AW","Multi_AW","Multi_RV_AW","Multi_Div_AW","Multi_Opp_AW")

index_APP<-c(1991,2133,2136,2134,2151)
names_APP<-c("APP_Equity","APP_FI","APP_Credit","APP_Commodities","APP_Vol")

indexID<-c(index_Directional,index_LongShort,index_Event,index_RV,index_Other,index_APP)
namesID<-c("date",names_Directional,names_LongShort,names_Event,names_RV,names_Other,names_APP)

#Download series
data_series<-getHedgersIndexReturns(currency="USD",start_date,indexID,end_date)  #downloads all series

#convert to dataframe
dt<-unlist(data_series$dates[[1]])
asset_weighted<-as.data.frame(dt)


for (i in 1:nrow(data_series)){
    temp<-unlist(data_series$returns[i])
    asset_weighted<-cbind(asset_weighted,temp)
}

colnames(asset_weighted)<-namesID

asset_weighted$date <- as.Date(as.POSIXct(asset_weighted$date/1000, origin = "1970-01-01"))

asset_weighted<-cbind(asset_weighted,VIX[3],Cash)
asset_weighted.t<-as_tibble(asset_weighted)

save(asset_weighted.t,file="AW_2022.RData")

equal_mean<-equal_weighted.t %>%
    summarise_if(is.numeric,mean)

equal_sd<-equal_weighted.t %>%
    summarise_if(is.numeric,sd)

equal_skew<-equal_weighted.t %>%
    summarise_if(is.numeric,skewness)

equal_kurt<-equal_weighted.t %>%
    summarise_if(is.numeric,kurtosis)

equal_stats<-cbind(t(equal_mean*12),t(equal_sd*(12^0.5)),t(equal_skew),t(equal_kurt))

colnames(equal_stats)<-c("mean","sd","skew","kurt")


asset_mean<-asset_weighted.t %>%
    summarise_if(is.numeric,mean)

asset_sd<-asset_weighted.t %>%
    summarise_if(is.numeric,sd)

asset_skew<-asset_weighted.t %>%
    summarise_if(is.numeric,skewness)

asset_kurt<-asset_weighted.t %>%
    summarise_if(is.numeric,kurtosis)

asset_stats<-cbind(t(asset_mean*12),t(asset_sd*(12^0.5)),t(asset_skew),t(asset_kurt))

colnames(equal_stats)<-c("mean","sd","skew","kurt")



