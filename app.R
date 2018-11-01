#Author : Navya Mote
#Date   : 7/24/2018
#Title  : Parameter-wise Analysis
#---------------------------------------------------------------------------------------------
#Libraries
library(expss)
library(dplyr)
library(data.table)
library(openxlsx)
library(knitr)
library(DT)
library(grDevices)
library(tidyverse)
# library(shiny)
#---------------------------------------------------------------------------------------------
#To read the CSV files for all parameters except DO and the Grades
data <- read.csv('Rivers.csv',fileEncoding="UTF-8-BOM")
# grade <- read.csv('Grades_r.csv',fileEncoding="UTF-8-BOM")
#---------------------------------------------------------------------------------------------
#To read the CSV files for DO
data_7206 <- read.csv('7206.csv',fileEncoding="UTF-8-BOM")
data_7506 <- read.csv('7506.csv',fileEncoding="UTF-8-BOM")
data_7805 <- read.csv('7805.csv',fileEncoding="UTF-8-BOM")
data_7904 <- read.csv('7904.csv',fileEncoding="UTF-8-BOM")
data_8516 <- read.csv('8516.csv',fileEncoding="UTF-8-BOM")
data_43807 <- read.csv('43807.csv',fileEncoding="UTF-8-BOM")
data_43829 <- read.csv('43829.csv',fileEncoding="UTF-8-BOM")
data_45415 <- read.csv('45415.csv',fileEncoding="UTF-8-BOM")
data_do <- rbind(data_7206,data_7506,data_7805,data_7904,data_8516,data_43807,data_43829,data_45415)
data_do <- data_do[complete.cases(data_do$Value), ]
#-------------------------------------------------------------------------------------------
# To combine all data
data<-rbind(data,data_do)
data$Site<-paste(data$Site,"_",data$Station) #To concatenate Site and SiteId
#To get the list of unique parameters
df_pr<-as.character(unique(data$Parameter))
df_param<-df_pr[df_pr != ""]
df_summary_am<-data.frame()
df_summary_do<-data.frame()
df_summary_ec<-data.frame()
df_summary_zns<-data.frame()
df_summary_ton<-data.frame()
df_summary_cus<-data.frame()
# ---------------------------------------------------------------------------------------------
#Function to set a flag if "<" found
Get_Detect_lt<-function(data_input){
  data_input$Detect_count_lt<-ifelse(data_input$DetectFlag=="<", 1,0)
}
#-----------------------------------------------------------------------------------------------
#Function to set a flag if ">" found
Get_Detect_gt<-function(data_input){
  data_input$Detect_count_gt<-ifelse(data_input$DetectFlag==">", 1,0)
}
#-----------------------------------------------------------------------------------------------
#Function to get samples per year
Get_Yearly<-function(df_per_yr){
  df_summary <- summarize(df_per_yr, Samples_yearly = n(), Censored = round(length(which(Detect_count_lt == 1))/n(),2)*100,
                          Median_yearly = round(median(Value),2), Max_yearly = round(max(Value),2),
                          Min_yearly = round(min(Value),2), Exceed_540_yearly = round(sum(Value>540)/n()*100,0),
                          Exceed_260_yearly = round(sum(Value>260)/n()*100,0))
}
#-----------------------------------------------------------------------------------------
#Function to get the Censored% values for all years
Get_All_Years<-function(df_all_years){
  df_all_yr<-summarize(df_all_years,All_Years = n(), Censored_val = length(which(Detect_count_lt == 1)),
                       Censored_prct = round(Censored_val/All_Years,2)*100, Median_all_yr = round(median(Value),2),
                       Max_all_yr = round(max(Value),2), Min_All_Years = round(min(Value),2), 
                       Exceed_540_all_yr = round(sum(Value>540)/n()*100,0), Exceed_260_all_yr = round(sum(Value>260)/n()*100,0))
}
# ----------------------------------------------------------------------------------------------
#Function to get the min value of samples per day for DO
Get_do_min<-function(df_all_days){
  df_summary <- summarize(df_all_days, Min_daily = round(min(Min_Val),2))
}
#------------------------------------------------------------------------------------------
#Function to get samples per year for DO
Get_Samples_Yearly<-function(df_samples_yr){
  df_summary <- summarize(df_samples_yr, Samples_yearly = n())
}
# --------------------------------------------------------------------------------------------
# Function to get 7 day mean for DO
Get_avg<-function(df_mvg_avg){
  df_summary <- summarize(df_mvg_avg, Avg_yrly = round(min(Rolling.Average,na.rm=TRUE),2))
}
#-------------------------------------------------------------------------------------------
#Looping at the parameters
for (param in df_param){
  if(param!="pH")
  {
    data_input<-filter(data,Parameter == param) #To get parameter specific data
    # -----------------------------------------------------------------------------------------------
    # Ammonia logic to link with pH using Sample Number
    if(param=="Ammonia (total)"){
      data_ip<-filter(data,Parameter == "Ammonia (total)")
      data_ph<-filter(data,Parameter == "pH")
      # data_input1<-merge(data_ip,data_ph,by = 'SampleNumber', all.SampleNumber = TRUE)
      # write.csv(data_input1,"data_input1.csv")
      data_input1<-merge(data_ip, data_ph, by=c("SampleNumber","Site"))
      df_uph<-data_input1[!duplicated(data_input1[1:5]),]
      df_uph$ratio<- ifelse(df_uph$Value.y>9,0.2, -0.029809630224*df_uph$Value.y^6 + 1.2641450933*df_uph$Value.y^5 - 22.08947826*df_uph$Value.y^4 + 203.57964573*df_uph$Value.y^3 - 1043.9962833*df_uph$Value.y^2 + 2825.5522036*df_uph$Value.y - 3151.0179751)
      df_uph$Concph8<-df_uph$Value.x/df_uph$ratio
      data_input<-data.frame("Watershed"=df_uph$Watershed.x,"Site"=df_uph$Site, "Date"=df_uph$Date.x,"Value"=df_uph$Concph8,
                             "DetectFlag"=df_uph$DetectFlag.x,"Parameter"=df_uph$Parameter.x)
    }
    # --------------------------------------------------
    #To get the list of unique watersheds
    df_ws<-as.character(unique(data_input$Watershed))
    df_wshed<-df_ws[df_ws != ""]
    for (ws in df_wshed) {
      data_ws<-filter(data_input,Watershed == ws) #To get watershed specific data
      if( ws == "Tamaki"){
        grade <- read.csv('Grades_Tamaki.csv',fileEncoding="UTF-8-BOM")
      }else if(ws == "Wairoa"){
        grade <- read.csv('Grades_Wairoa.csv',fileEncoding="UTF-8-BOM")
      }else{
        grade <- read.csv('Grades_r.csv',fileEncoding="UTF-8-BOM")
      }
      # To convert from mg/L to µ/L 
      if (param=="Copper (soluble)"){
        data_ws$Value=data_ws$Value*1000
      }else if(param=="Copper (total)"){
        data_ws$Value=data_ws$Value*1000
      }else if(param=="Zinc (soluble)"){
        data_ws$Value=data_ws$Value*1000
      }else if(param=="Zinc (total)"){
        data_ws$Value=data_ws$Value*1000
      }
      data_grade<-filter(grade, Parameter == param)#To get parameter specific grades
      # ----------------------------------------------------------------------------------------------
      # ---------------------------------------------------------------------------------------------
      #To get the date in required format
      df_date <- data.table(date = as.IDate(data_ws$Date,"%m/%d/%Y"))
      #---------------------------------------------------------------------------------------------
      if(param == "DO"){
        #To group the dataset based on Watershed, Site and Date
        data_ws$Date <- as.Date(data_ws$Date,"%m/%d/%Y")
        data_min<-group_by(data_ws,Watershed,Site,Date)
        df_min<-summarise(data_min, Min_Val = round(min(Value),2))
        df_min_date <- data.table(date = as.IDate(df_min$Date,"%m/%d/%Y"))
        #---------------------------------------------------------------------------------------------
        #To get the water year based on the range in which the date falls
        year<-substring(df_min_date$date,1,4)
        df_yr<-data.frame("Year"=unique(year))
        df_yr[, 1] <- as.numeric(as.character( df_yr[, 1] ))
        df_yr$ID <- seq.int(nrow(df_yr))
        #To build the range of start date and end date
        df_yr$St_yr<-paste(df_yr$Year,"11-01", sep="-")
        df_yr$ed_yr<-paste(df_yr$Year+1,"04-30", sep="-")
        # To build an interval for Water Year
        interval.dt <- data.table(ID = df_yr$ID, start = df_yr$St_yr, end = df_yr$ed_yr)
        findYearIndex <- function(year) {
          interval.dt[,which((interval.dt$start) <= year & year <= (interval.dt$end))]
        }
        dt_min <- data.table(year = df_min_date$date)
        dt_min$YearIndex <- sapply(dt_min$year, findYearIndex)
        df_min$Year<-as.numeric(dt_min$YearIndex)
        df_min <- df_min[complete.cases(df_min$Year), ]
        # --------------------------------------------------------------------------------------------
        # To get daily and Yearly min
        df_all_days<-group_by(df_min,Watershed,Site,Year)
        df_min_daily<-Get_do_min(df_all_days)
        df_min_yr<-mutate(df_min_daily,Min_yr = round(min(Min_daily),2))
        # To get Yearly and All year Samples
        df_samples_yr<-group_by(df_min,Watershed,Site,Year)
        df_samples_yrly<-Get_Samples_Yearly(df_samples_yr)
        df_samp_yr<-mutate(df_samples_yrly, Samples_All_yr = sum(Samples_yearly))
        #---------------------------------------------------------------------------------------------
        # To get the 7 day mean for a Year and All Year
        require(zoo)
        df_avg<-data.table(df_min)
        df_avg<-group_by(df_avg,Watershed,Site,Year)
        df_avg<-data.table(df_avg)
        setkey(df_avg,"Watershed","Site", "Year")
        df_avg[, Rolling.Average := rollmean(Min_Val, 7, fill = NA), by = Year]
        df_mvg_avg<-group_by(df_avg,Watershed,Site,Year)
        df_mov_avg<-Get_avg(df_mvg_avg)
        df_avg_yr<-mutate(df_mov_avg, Avg_All_yr = min(Avg_yrly))
        # To get the Year based on Water year
        df_avg_yr$Year[df_avg_yr$Year=="1"]<-"2013-2014"
        df_avg_yr$Year[df_avg_yr$Year=="2"]<-"2014-2015"
        df_avg_yr$Year[df_avg_yr$Year=="3"]<-"2015-2016"
        df_avg_yr$Year[df_avg_yr$Year=="4"]<-"2016-2017"
        df_avg_yr$Year[df_avg_yr$Year=="5"]<-"2017"
        # To place all the data into final table
        df_summary<-data.frame("Watershed"=df_avg_yr$Watershed,"Site"=df_avg_yr$Site,"Year"=df_avg_yr$Year,
                               "Samples_yearly"=df_samp_yr$Samples_yearly,"Samples_All_Years"=df_samp_yr$Samples_All_yr,
                               "Mean_Yearly"=df_avg_yr$Avg_yrly,"Mean_All_Years"=df_avg_yr$Avg_All_yr,
                               "Min_Yearly"=df_min_yr$Min_daily,"Min_All_Years"=df_min_yr$Min_yr)
        df_summary$Censored<-"0"
        df_summary$Mean_Yearly[df_summary$Mean_Yearly=="Inf"]<-0
        # --------------------------------------------------------------------------------------------
      }else{
        #To set 1 when < is present and 0 if not for DetectFlag_lt for Censored% data
        data_ws$Detect_count_lt<-Get_Detect_lt(data_ws)
        #---------------------------------------------------------------------------------------------
        #To set 1 when > is present and 0 if not for DetectFlag_gt for Censored% data
        data_ws$Detect_count_gt<-Get_Detect_gt(data_ws)
        #---------------------------------------------------------------------------------------------
        # For values that are "below the detection limit" (that have a "<" flag associated with the value), 
        # make the reported number half the value
        data_ws$Value<- ifelse(data_ws$Detect_count_lt == 1, data_ws$Value/2, data_ws$Value)
        # --------------------------------------------------------------------------------------------
        # For values that are "above the detection limit" (that have a ">" flag associated with the value), 
        # make the reported number +1 the value
        data_ws$Value<- ifelse(data_ws$Detect_count_gt == 1, data_ws$Value+1, data_ws$Value)
        # --------------------------------------------------------------------------------------------
        #To get the water year based on the range in which the date falls
        year<-substring(df_date$date,1,4)
        df_yr<-data.frame("Year"=unique(year))
        df_yr[, 1] <- as.numeric(as.character( df_yr[, 1] ))
        df_yr$ID <- seq.int(nrow(df_yr))
        #To build the range of start date and end date
        df_yr$St_yr<-paste(df_yr$Year,"01-01", sep="-")
        df_yr$ed_yr<-paste(df_yr$Year,"12-31", sep="-")
        # To build an interval for Water Year
        interval.dt <- data.table(ID = df_yr$ID, start = df_yr$St_yr, end = df_yr$ed_yr)
        findYearIndex <- function(year) {
          interval.dt[,which((interval.dt$start) <= year & year <= (interval.dt$end))]
        }
        dt <- data.table(year = df_date$date)
        dt$YearIndex <- sapply(dt$year, findYearIndex)
        data_ws$Year<-as.numeric(dt$YearIndex)
        data_ws<-data_ws[!(is.na(data_ws$Year) | data_ws$Year==""), ]
        #To assign the year range based on the Water Year  
        data_ws$year <- df_yr[data_ws$Year,]$Year
        # ---------------------------------------------------------------------------------------------
        #To group the dataset based on Watershed and Site
        df_all_yrs<-group_by(data_ws,Watershed,Site)
        #To group the dataset based on Watershed,Site and Year
        df_per_yr<-group_by(data_ws,Watershed,Site,year)
        # --------------------------------------------------------------------------------------------
        # To get the yearly Samples
        df_Yearly <-Get_Yearly(df_per_yr)
        # --------------------------------------------------------------------------------------------
        # To get sort the yearly grouped data in ascending order
        df_per_yr<-arrange(df_per_yr,Value,.by_group = TRUE)
        # --------------------------------------------------------------------------------------------
        # To get the yearly Hazen 95th Percentile
        # Hazen logic:
        df_hazen<-mutate(df_per_yr,count=n(),id = seq_along(Value),rHazen=(0.5+(0.95*count)),ri=trunc(rHazen),rf=rHazen - ri,
                         rin = trunc(rHazen)+1,r1 = ifelse(count==ri, trunc(rHazen), trunc(rHazen)),r1n = ifelse(count==rin, trunc(rHazen)+1, ifelse(count<rin,trunc(rHazen),trunc(rHazen))))
        df_haz <- data.frame("Watershed"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$Watershed,"Site"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$Site,
                             "year"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$year,"rf"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$rf,"r1"= ifelse(df_hazen[ which(df_hazen$id==df_hazen$r1),]$count>=10,df_hazen[ which(df_hazen$id==df_hazen$r1),]$Value,0),
                             "r1n"= ifelse(df_hazen[ which(df_hazen$id==df_hazen$r1),]$count>=10,df_hazen[ which(df_hazen$id==df_hazen$r1n),]$Value,0)) 
        df_haz$rhazen<-((1-df_haz$rf)*df_haz$r1)+(df_haz$rf*df_haz$r1n)
        # --------------------------------------------------------------------------------------------
        # To place all the yearly values in the Summary dataframe
        df_summary<-data.frame("Watershed"=df_Yearly$Watershed,"Site"=df_Yearly$Site,"Samples_yearly"=df_Yearly$Samples_yearly,
                               "Censored"=df_Yearly$Censored,"Median_yearly"=df_Yearly$Median_yearly,"Percentile_Yearly"=round(df_haz$rhazen,2),
                               "Max_Yearly"=df_Yearly$Max_yearly,"Min_Yearly"=df_Yearly$Min_yearly, "Year"=df_Yearly$year,
                               "Exceed_540"=df_Yearly$Exceed_540_yearly, "Exceed_260"=df_Yearly$Exceed_260_yearly)
        # --------------------------------------------------------------------------------------------
        # To group the summary table w.r.t Watershed and Site
        df_all_years<-group_by(data_ws, Watershed, Site)
        # ---------------------------------------------------------------------------------------------
        # To get the all years
        df_All_Years<-Get_All_Years(df_all_years)
        # ---------------------------------------------------------------------------------------------
        # To get the all year Hazen 95th Percentile
        df_all_yrs<-arrange(df_all_yrs,Value,.by_group = TRUE)
        df_hazen1<-mutate(df_all_yrs,count=n(),id = seq_along(Value),rHazen=(0.5+(0.95*count)),ri=trunc(rHazen),rf=rHazen - ri,
                          rin = trunc(rHazen)+1)
        df_haz1 <- data.frame("Watershed"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Watershed,"Site"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Site,
                              "year"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$year,"rf"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$rf,"r1"= ifelse(df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$count>=10,df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Value,0),
                              "r1n"= ifelse(df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$count>=10,df_hazen1[ which(df_hazen1$id==df_hazen1$rin),]$Value,0)) 
        df_haz1$rhazen<-round((((1-df_haz1$rf)*df_haz1$r1)+(df_haz1$rf*df_haz1$r1n)),2)
        # -----------------------------------------------------------------------------------------------
        
        df_w_s_y<-data.frame("Watershed"=df_All_Years$Watershed,"Site"=df_All_Years$Site,"All_Years"=df_All_Years$All_Years, "Censored_prct"= df_All_Years$Censored_prct,"Median_all_yr"=df_All_Years$Median_all_yr,
                             "Exceed_540_all_yr"=df_All_Years$Exceed_540_all_yr, "Exceed_260_all_yr"=df_All_Years$Exceed_260_all_yr,
                             "Max_all_yr"=df_All_Years$Max_all_yr, "Min_All_Years"=df_All_Years$Min_All_Years)
        # df_uwsy<-unique(df_w_s_y,nomatch = 0)
        df_uwsy = merge(df_summary,df_w_s_y, by=c("Watershed","Site"), all.x=TRUE)
        df_uwsy = merge(df_uwsy,df_haz1, by=c("Watershed","Site"), all.x=TRUE)
        #To sort the data w.r.t Watersheds in ascending order
        df_uwsy<-df_uwsy[order(df_uwsy$Watershed),]
        df_summary$Censored_All_Years = df_uwsy$Censored_prct
        df_summary$Samples_All_Years = df_uwsy$All_Years
      }
      #To get the unique functions(Median, Percentile, Max, Min, Mean, Exceed 260, Exceed 540)
      df_function<-as.character(unique(data_grade$Function))
      df_function<-df_function[df_function != ""]
      #Looping at the functions to get the grades
      for(func in df_function){
        # --------------------------------------------------------------------------------------------------------
        Gr_func = filter(data_grade, Function == func)
        interval.gr <- data.table(Grade = Gr_func$Grade, gt = Gr_func$gt, lt = Gr_func$lt)
        # df_summary$Censored = df_All_Years$Censored_prct
        # df_summary$Samples_All_Years = df_All_Years$All_Years
        # To get the Median attribute state yearly and all years
        if(func == "Median")
        {
          findIndex <- function(Median_yearly) 
          {
            interval.gr[,which((interval.gr$gt) < Median_yearly & Median_yearly <= (interval.gr$lt))]
          }
          df_summary$Index <- sapply(df_summary$Median_yearly, findIndex)
          df_summary$Median_Attr_state_yearly <- Gr_func[df_summary$Index,]$Grade
          findIndex1 <- function(Median_all_yr) 
          {
            interval.gr[,which((interval.gr$gt) < Median_all_yr & Median_all_yr <= (interval.gr$lt))]
          }
          df_summary$Index1 <- sapply(df_uwsy$Median_all_yr, findIndex1)
          df_summary$Median_Attr_state_all_years <- Gr_func[df_summary$Index1,]$Grade
          df_summary$Median_All_Years<-df_uwsy$Median_all_yr
        }
        # -----------------------------------------------------------------------------------------------------------
        # To get the Percentile attribute state yearly and all years
        else if(func == "Percentile")
        {
          findIndex <- function(Percentile_Yearly) 
          {
            interval.gr[,which((interval.gr$gt) < Percentile_Yearly & Percentile_Yearly <= (interval.gr$lt))]
          }
          df_summary$Index2 <- sapply(df_summary$Percentile_Yearly, findIndex)
          df_summary$Percentile_Attr_state_yearly <- Gr_func[df_summary$Index2,]$Grade
          findIndex1 <- function(rhazen) 
          {
            interval.gr[,which((interval.gr$gt) < rhazen & rhazen <= (interval.gr$lt))]
          }
          df_summary$Index3 <- sapply(df_uwsy$rhazen, findIndex1)
          df_summary$Percentile_Attr_state_all_years <- Gr_func[df_summary$Index3,]$Grade
          if(param=="E.coli"){
            df_summary$Percentile_All_Years=round(df_uwsy$rhazen,0)
            # df_summary$Percentile_Yearly<-ifelse(df_summary$Percentile_Yearly==0,"NA",round(as.numeric(df_summary$Percentile_Yearly),0))
            df_summary$Percentile_Yearly<-round(as.numeric(df_summary$Percentile_Yearly),0)
          }else{
            df_summary$Percentile_All_Years=round(df_uwsy$rhazen,2)
            # df_summary$Percentile_Yearly<-ifelse(df_summary$Percentile_Yearly==0,"NA",round(as.numeric(df_summary$Percentile_Yearly),2))
            df_summary$Percentile_Yearly<-round(as.numeric(df_summary$Percentile_Yearly),2)
          }
          df_summary$Percentile_Attr_state_yearly<-ifelse(df_summary$Percentile_Yearly=="NA","NA",as.character(df_summary$Percentile_Attr_state_yearly))
        }
        # ------------------------------------------------------------------------------------------------------------
        # To get the Min attribute state yearly and all years
        else if(func == "Min")
        {
          findIndex <- function(Min_Yearly)
          {
            interval.gr[,which((interval.gr$gt) <= Min_Yearly & Min_Yearly < (interval.gr$lt))]
          }
          df_summary$Index4 <- sapply(df_summary$Min_Yearly, findIndex)
          df_summary$Min_Attr_state_yearly <- Gr_func[df_summary$Index4,]$Grade
          findIndex1 <- function(Min_All_Years)
          {
            interval.gr[,which((interval.gr$gt) < Min_All_Years & Min_All_Years <= (interval.gr$lt))]
          }
          df_summary$Index5 <- sapply(df_summary$Min_All_Years, findIndex1)
          df_summary$Min_Attr_state_all_years <- Gr_func[df_summary$Index5,]$Grade
          # df_summary$Min_All_Years<-df_summary$Min_All_Years
        }
        # ------------------------------------------------------------------------------------------------------------
        # To get the Min attribute state yearly and all years
        else if(func == "Mean")
        {
          findIndex <- function(Mean_Yearly)
          {
            interval.gr[,which((interval.gr$gt) <= Mean_Yearly & Mean_Yearly < (interval.gr$lt))]
          }
          df_summary$Index6 <- sapply(as.numeric(df_summary$Mean_Yearly), findIndex)
          df_summary$Mean_Attr_state_yearly <- Gr_func[df_summary$Index6,]$Grade
          findIndex1 <- function(Mean_All_Years)
          {
            interval.gr[,which((interval.gr$gt) < Mean_All_Years & Mean_All_Years <= (interval.gr$lt))]
          }
          df_summary$Index7 <- sapply(df_summary$Mean_All_Years, findIndex1)
          df_summary$Mean_Attr_state_all_years <- Gr_func[df_summary$Index7,]$Grade
          # df_summary$Mean_All_Years<-df_summary$Mean_All_Years
        }      
        # -----------------------------------------------------------------------------------------------------------
        # To get the Max attribute state yearly and all years
        else if(func == "Max")
        {
          findIndex <- function(Max_Yearly) 
          {
            interval.gr[,which((interval.gr$gt) < Max_Yearly & Max_Yearly <= (interval.gr$lt))]
          }
          df_summary$Index6 <- sapply(df_summary$Max_Yearly, findIndex)
          df_summary$Max_Attr_state_yearly <- Gr_func[df_summary$Index6,]$Grade
          findIndex1 <- function(Max_all_yr) 
          {
            interval.gr[,which((interval.gr$gt) < Max_all_yr & Max_all_yr <= (interval.gr$lt))]
          }
          df_summary$Index7 <- sapply(df_uwsy$Max_all_yr, findIndex1)
          df_summary$Max_Attr_state_all_years <- Gr_func[df_summary$Index7,]$Grade
          df_summary$Max_All_Years<-df_uwsy$Max_all_yr
          
        }
        # ------------------------------------------------------------------------------------------------------------------
        # # To get the 260 cfu / 100 mL exceedances attribute state yearly and all years
        else if(func == "Exceed_260")
        {
          findIndex <- function(Exceed_260) 
          {
            interval.gr[,which((interval.gr$gt) < Exceed_260 & Exceed_260 <= (interval.gr$lt))]
          }
          df_summary$Index8 <- sapply(df_summary$Exceed_260, findIndex)
          df_summary$Exceed_260_Attribute_state_yearly <- Gr_func[df_summary$Index8,]$Grade
          findIndex1 <- function(Exceed_260_all_yr) 
          {
            interval.gr[,which((interval.gr$gt) < Exceed_260_all_yr & Exceed_260_all_yr <= (interval.gr$lt))]
          }
          df_summary$Index9 <- sapply(df_uwsy$Exceed_260_all_yr, findIndex1)
          df_summary$Exceed_260_Attr_state_all_years <- Gr_func[df_summary$Index9,]$Grade
          df_summary$Exceed_260_All_Years<-df_uwsy$Exceed_260_all_yr
        }
        # ----------------------------------------------------------------------------------------------------------------
        # To get the 540 cfu / 100 mL exceedances attribute state yearly and all years
        else if(func == "Exceed_540")
        {
          findIndex <- function(Exceed_540) 
          {
            interval.gr[,which((interval.gr$gt) < Exceed_540 & Exceed_540 <= (interval.gr$lt))]
          }
          df_summary$Index11 <- sapply(df_summary$Exceed_540, findIndex)
          df_summary$Exceed_540_Attribute_state_yearly <- Gr_func[df_summary$Index11,]$Grade
          findIndex1 <- function(Exceed_540_all_yr) 
          {
            interval.gr[,which((interval.gr$gt) < Exceed_540_all_yr & Exceed_540_all_yr <= (interval.gr$lt))]
          }
          df_summary$Index12 <- sapply(df_uwsy$Exceed_540_all_yr, findIndex1)
          df_summary$Exceed_540_Attr_state_all_years <- Gr_func[df_summary$Index12,]$Grade
          df_summary$Exceed_540_All_Years<-df_uwsy$Exceed_540_all_yr
        }
        # -------------------------------------------------------------------------------------------------------------
      }
      if(param == "Ammonia (total)"){
        df_summary_am <- rbind(df_summary_am, df_summary)
      }
      else if( param == "DO"){
        df_summary_do <- rbind(df_summary_do, df_summary)
      }
      else if( param == "E.coli"){
        df_summary_ec <- rbind(df_summary_ec, df_summary)
      }else if ( param == "Total Oxidized Nitrogen"){
        df_summary_ton <- rbind(df_summary_ton, df_summary)
      }else if ( param == "Zinc (soluble)"){
        df_summary_zns <- rbind(df_summary_zns, df_summary)
      }else if ( param == "Copper (soluble)"){
        df_summary_cus <- rbind(df_summary_cus, df_summary)
      }
    }
    # --------------------------------------------------------------------------------------------------------
    library(tidyr)
    if(param == "Ammonia (total)"){
      df_summary<-df_summary_am
    }
    else if( param == "DO"){
      df_summary<-df_summary_do
    }
    else if( param == "E.coli"){
      df_summary<-df_summary_ec
    }else if ( param == "Zinc (soluble)"){
      df_summary<-df_summary_zns
    }else if ( param == "Total Oxidized Nitrogen"){
      df_summary<-df_summary_ton
    }else if ( param == "Copper (soluble)"){
      df_summary<-df_summary_cus
    }
    # df_summary<- df_summary_ws
    df_summary<-df_summary %>%
      separate(Site, c("Site", "SiteId"), "_")
    # --------------------------------------------------------------------------------------------------------
    # To get the summary row
    df_sum<-df_summary[!duplicated(df_summary[c(1,2)]),]
    df_sum$Samples_yearly<-df_sum$Samples_All_Years
    df_sum$Year<-"All"
    if(param == "Ammonia (total)"){
      df_sum$Censored<-df_sum$Censored_All_Years
      df_sum$Max_Yearly<-df_sum$Max_All_Years
      df_sum$Max_Attr_state_yearly<-df_sum$Max_Attr_state_all_years
      df_sum$Median_yearly<-df_sum$Median_All_Years
      df_sum$Median_Attr_state_yearly<-df_sum$Median_Attr_state_all_years
    }
    else if( param == "DO"){
      df_sum$Min_Yearly<-df_sum$Min_All_Years
      df_sum$Min_Attr_state_yearly<-df_sum$Min_Attr_state_all_years
      df_sum$Mean_Yearly<-df_sum$Mean_All_Years
      df_sum$Mean_Attr_state_yearly<-df_sum$Mean_Attr_state_all_years
      df_sum$Yearal_Yearly_Samples<-df_sum$Yearal_All_Year_Samples
    }
    else if( param == "E.coli"){
      df_sum$Censored<-df_sum$Censored_All_Years
      df_sum$Exceed_540<-df_sum$Exceed_540_All_Years
      df_sum$Exceed_260<-df_sum$Exceed_260_All_Years
      df_sum$Exceed_260_Attribute_state_yearly<-df_sum$Exceed_260_Attr_state_all_years
      df_sum$Exceed_540_Attribute_state_yearly<-df_sum$Exceed_540_Attr_state_all_years
      df_sum$Median_yearly<-df_sum$Median_All_Years
      df_sum$Median_Attr_state_yearly<-df_sum$Median_Attr_state_all_years
      df_sum$Percentile_Yearly<-df_sum$Percentile_All_Years
      df_sum$Percentile_Attr_state_yearly<-df_sum$Percentile_Attr_state_all_years
    }else{
      df_sum$Censored<-df_sum$Censored_All_Years
      df_sum$Percentile_Yearly<-df_sum$Percentile_All_Years
      df_sum$Percentile_Attr_state_yearly<-df_sum$Percentile_Attr_state_all_years
      df_sum$Median_yearly<-df_sum$Median_All_Years
      df_sum$Median_Attr_state_yearly<-df_sum$Median_Attr_state_all_years
    }
    df_tot<-rbind(df_summary,df_sum)
    df_tot<-arrange(df_tot, Watershed, Site)
    df_summary<-df_tot
    df_summary<-filter(df_summary, Year == "All")
    df_summary$Year<-NULL
    # ---------------------------------------------------------------------------------------------------------------
    # To get the overall attribute state for all parameters
    if (param=="Ammonia (total)"){
      df_summary$Max_value<-ifelse(df_summary$Max_Attr_state_yearly=="A",1,
                                   ifelse(df_summary$Max_Attr_state_yearly=="B",2,
                                          ifelse(df_summary$Max_Attr_state_yearly=="C",3,
                                                 ifelse(df_summary$Max_Attr_state_yearly=="D",4,"NA"))))
      df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Median_Attr_state_yearly=="D",4,
                                                      ifelse(df_summary$Median_Attr_state_yearly=="E",5,"NA")))))
      df_max<-data.frame(df_summary$Med_val,df_summary$Max_value)
    }else if(param=="E.coli"){
      df_summary$ex260_val<-ifelse(df_summary$Exceed_260_Attribute_state_yearly=="A",1,
                                   ifelse(df_summary$Exceed_260_Attribute_state_yearly=="B",2,
                                          ifelse(df_summary$Exceed_260_Attribute_state_yearly=="C",3,
                                                 ifelse(df_summary$Exceed_260_Attribute_state_yearly=="D",4,
                                                        ifelse(df_summary$Exceed_260_Attribute_state_yearly=="E",5,
                                                               "NA")))))
      df_summary$ex540_val<-ifelse(df_summary$Exceed_540_Attribute_state_yearly=="A",1,
                                   ifelse(df_summary$Exceed_540_Attribute_state_yearly=="B",2,
                                          ifelse(df_summary$Exceed_540_Attribute_state_yearly=="C",3,
                                                 ifelse(df_summary$Exceed_540_Attribute_state_yearly=="D",4,
                                                        ifelse(df_summary$Exceed_540_Attribute_state_yearly=="E",5,
                                                               "NA")))))
      df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Median_Attr_state_yearly=="D",4,
                                                      ifelse(df_summary$Median_Attr_state_yearly=="E",5,"NA")))))
      df_summary$Per_val<-ifelse(df_summary$Percentile_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Percentile_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Percentile_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Percentile_Attr_state_yearly=="D",4,
                                                      ifelse(df_summary$Percentile_Attr_state_yearly=="E",5,
                                                             ifelse(df_summary$Percentile_Attr_state_yearly=="NA",1,"NA"))))))
      df_max<-data.frame(df_summary$Med_val,df_summary$ex540_val, df_summary$ex260_val, df_summary$Per_val)
    }else if(param=="DO"){
      df_summary$Min_val<-ifelse(df_summary$Min_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Min_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Min_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Min_Attr_state_yearly=="D",4,
                                                      ifelse(df_summary$Min_Attr_state_yearly=="E",5,"NA")))))
      df_summary$Mean_val<-ifelse(df_summary$Mean_Attr_state_yearly=="A",1,
                                  ifelse(df_summary$Mean_Attr_state_yearly=="B",2,
                                         ifelse(df_summary$Mean_Attr_state_yearly=="C",3,
                                                ifelse(df_summary$Mean_Attr_state_yearly=="D",4,
                                                       ifelse(df_summary$Mean_Attr_state_yearly=="E",5,
                                                              ifelse(df_summary$Mean_Attr_state_yearly=="NA",1,"NA"))))))
      df_max<-data.frame(df_summary$Min_val,df_summary$Mean_val)
    }else{
      if(param != "DO"){
        df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                   ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                          ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                                 ifelse(df_summary$Median_Attr_state_yearly=="D",4,
                                                        ifelse(df_summary$Median_Attr_state_yearly=="E",5,"NA")))))
        df_summary$Per_val<-ifelse(df_summary$Percentile_Attr_state_yearly=="A",1,
                                   ifelse(df_summary$Percentile_Attr_state_yearly=="B",2,
                                          ifelse(df_summary$Percentile_Attr_state_yearly=="C",3,
                                                 ifelse(df_summary$Percentile_Attr_state_yearly=="D",4,
                                                        ifelse(df_summary$Percentile_Attr_state_yearly=="E",5,
                                                               ifelse(df_summary$Percentile_Attr_state_yearly=="NA",1,"NA"))))))
        df_max<-data.frame(df_summary$Med_val,df_summary$Per_val)
      }
    }
    df_summary$Max_val<-apply(X=df_max, MARGIN=1, FUN=max)
    df_summary$Overall_Attribute_State<-ifelse(df_summary$Max_val==1,"A",
                                               ifelse(df_summary$Max_val==2,"B",
                                                      ifelse(df_summary$Max_val==3,"C",
                                                             ifelse(df_summary$Max_val==4,"D",
                                                                    ifelse(df_summary$Max_val==5,"E",
                                                                           "NA")))))
    # --------------------------------------------------------------------------------------------------------
    # To place the parameters into separate dataframes
    if (param=="Total Oxidized Nitrogen"){
      df_ton_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                     Median_yearly, Percentile_Yearly,
                     Median_Attr_state_yearly, Percentile_Attr_state_yearly,
                     Overall_Attribute_State)
    }else if(param=="Ammonia (total)"){
      df_amm_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                     Median_yearly, Max_Yearly,
                     Median_Attr_state_yearly, Max_Attr_state_yearly,
                     Overall_Attribute_State)
    }
    else if(param=="DO"){
      df_do_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                    Mean_Yearly, Min_Yearly, 
                    Mean_Attr_state_yearly,Min_Attr_state_yearly,Overall_Attribute_State)
    }
    else if(param=="Copper (soluble)"){
      df_cus_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                     Median_yearly, Percentile_Yearly,
                     Median_Attr_state_yearly, Percentile_Attr_state_yearly,
                     Overall_Attribute_State)
    }
    # else if(param=="Copper (total)"){
    #   df_cut_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
    #                  Median_yearly, Percentile_Yearly,
    #                  Median_Attr_state_yearly, Percentile_Attr_state_yearly,
    #                  Overall_Attribute_State)
    # }
    else if(param=="Zinc (soluble)"){
      df_zns_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                     Median_yearly, Percentile_Yearly,
                     Median_Attr_state_yearly, Percentile_Attr_state_yearly,
                     Overall_Attribute_State)
    }
    # else if(param=="Zinc (total)"){
    #   df_znt_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
    #                  Median_yearly, Percentile_Yearly,
    #                  Median_Attr_state_yearly, Percentile_Attr_state_yearly,
    #                  Overall_Attribute_State)
    # }
    else if(param=="E.coli"){
      df_ecoli_r<-select(df_summary, Watershed, Site, SiteId, Samples_yearly, Censored,
                       Exceed_540, Exceed_260,
                       Median_yearly, Percentile_Yearly,
                       Exceed_540_Attribute_state_yearly,
                       Exceed_260_Attribute_state_yearly,
                       Median_Attr_state_yearly, Percentile_Attr_state_yearly,
                       Overall_Attribute_State)
    }
  }
  # -----------------------------------------------------------------------------------------------------------
}
#To write parameters to separate tabs in an excel file as output
l <- list("TON" = df_ton_r, "Ammonia (total)" = df_amm_r, 
          "DO" = df_do_r,
          "Copper (soluble)" = df_cus_r,
          # "Copper (total)" = df_cut_r, 
          "Zinc (soluble)" = df_zns_r, 
          # "Zinc (total)" = df_znt_r, 
          "E.coli" = df_ecoli_r)
write.xlsx(l, file = "Parameter_Grades_Analysis-Rivers.xlsx")
# spiderr_ecoli<-data.frame("Watershed"=df_ecoli_r$Watershed, "Site"=df_ecoli_r$Site,
#                           "E.coli"=df_ecoli_r$Overall_Attribute_State)
# spiderr_Zn_soluble<-data.frame("Watershed"=df_zns_r$Watershed, "Site"=df_zns_r$Site,
#                          "Zn_soluble"=df_zns_r$Overall_Attribute_State)
# spiderr_ton<-data.frame("Watershed"=df_ton_r$Watershed, "Site"=df_ton_r$Site,
#                         "TON"=df_ton_r$Overall_Attribute_State)
# spiderr_amm<-data.frame("Watershed"=df_amm_r$Watershed, "Site"=df_amm_r$Site,
#                         "Ammonia"=df_amm_r$Overall_Attribute_State)
# spiderr_Cu_soluble<-data.frame("Watershed"=df_cus_r$Watershed, "Site"=df_cus_r$Site,
#                           "Cu_soluble"=df_cus_r$Overall_Attribute_State)
# spiderr<-merge(spiderr_ecoli, spiderr_Zn_soluble, by=c('Watershed','Site'), all.x = TRUE)
# spiderr<-merge(spiderr, spiderr_ton, by=c('Watershed','Site'), all.x = TRUE)
# spiderr<-merge(spiderr, spiderr_amm, by=c('Watershed','Site'), all.x = TRUE)
# spiderr<-merge(spiderr, spiderr_Cu_soluble, by=c('Watershed','Site'), all.x = TRUE)
# spiderr$E.coli<-ifelse(spiderr$E.coli=="A",1,
#                        ifelse(spiderr$E.coli=="B",2,
#                               ifelse(spiderr$E.coli=="C",3,
#                                      ifelse(spiderr$E.coli=="D",4,
#                                             ifelse(spiderr$E.coli=="E",5,
#                                                    "NA")))))
# spiderr$Cu_soluble<-ifelse(spiderr$Cu_soluble=="A",1,
#                            ifelse(spiderr$Cu_soluble=="B",2,
#                                   ifelse(spiderr$Cu_soluble=="C",3,
#                                          ifelse(spiderr$Cu_soluble=="D",4,
#                                                 ifelse(spiderr$Cu_soluble=="E",5,
#                                                        "NA")))))
# spiderr$TON<-ifelse(spiderr$TON=="A",1,
#                     ifelse(spiderr$TON=="B",2,
#                            ifelse(spiderr$TON=="C",3,
#                                   ifelse(spiderr$TON=="D",4,
#                                          ifelse(spiderr$TON=="E",5,
#                                                 "NA")))))
# spiderr$Ammonia<-ifelse(spiderr$Ammonia=="A",1,
#                         ifelse(spiderr$Ammonia=="B",2,
#                                ifelse(spiderr$Ammonia=="C",3,
#                                       ifelse(spiderr$Ammonia=="D",4,
#                                              ifelse(spiderr$Ammonia=="E",5,
#                                                     "NA")))))
# spiderr$Zn_soluble<-ifelse(spiderr$Zn_soluble=="A",1,
#                            ifelse(spiderr$Zn_soluble=="B",2,
#                                   ifelse(spiderr$Zn_soluble=="C",3,
#                                          ifelse(spiderr$Zn_soluble=="D",4,
#                                                 ifelse(spiderr$Zn_soluble=="E",5,
#                                                        "NA")))))
# 
# spiderr_first<-data.frame("Watershed"=" ", "Site"="1",
#                           "E.coli"="1", "Zn_soluble" = "1" ,"TON"="1","Ammonia"="1", 
#                           "Cu_soluble"="1")
# spiderr_second<-data.frame("Watershed"=" ", "Site"="2",
#                           "E.coli"="5", "Zn_soluble" = "5" ,"TON"="5","Ammonia"="5", 
#                           "Cu_soluble"="5")
# spiderr <- rbind( spiderr_second,spiderr)
# spiderr <- rbind( spiderr_first, spiderr)
# spiderr<-spiderr[c(37,38), c(1:36)]
# -------------------------------------------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
#To read the CSV files for all parameters and Grades
data <- read.csv('Lakes.csv',fileEncoding="UTF-8-BOM")
grade <- read.csv('Grades_l.csv',fileEncoding="UTF-8-BOM")
# --------------------------------------------------------------------------------------------
#To get the list of unique parameters
df_pr<-as.character(unique(data$Parameter))
df_param<-df_pr[df_pr != ""]
# --------------------------------------------------------------------------------------------
# To get the list of unique lakes
df_lk<-as.character(unique(data$Site))
df_lakes<-df_lk[df_lk != ""]
# --------------------------------------------------------------------------------------------
#Function to get the avg value of samples per day
Get_avg<-function(df_sum){
  df_summary <- summarize(df_sum, Value = round(mean(Value),3))
}
#------------------------------------------------------------------------------------------
#Function to get the max value of samples per day
Get_max<-function(df_sum){
  df_summary <- summarize(df_sum, Value = round(max(Value),2))
}
#------------------------------------------------------------------------------------------
# To identify deep(1) and shallow(0) lakes
for (lake in df_lakes) {
  if(lake == "Tomarata")
  {
    data_input1<-filter(data,Site == lake) #To get lake specific data
    data_input1$DepthRange[data_input1$Depth<=1]<-0
    data_input1$DepthRange[data_input1$Depth>1]<-1
  }else if(lake == "Pupuke")
  {
    data_input2<-filter(data,Site == lake) #To get lake specific data
    data_input2$DepthRange[data_input2$Depth<=15]<-0
    data_input2$DepthRange[data_input2$Depth>15]<-1
  }else if(lake == "Wainamu")
  {
    data_input3<-filter(data,Site == lake) #To get lake specific data
    data_input3$DepthRange[data_input3$Depth<=4]<-0
    data_input3$DepthRange[data_input3$Depth>4]<-1
  }else if(lake == "Ototoa")
  {
    data_input4<-filter(data,Site == lake) #To get lake specific data
    data_input4$DepthRange[data_input4$Depth<=15]<-0
    data_input4$DepthRange[data_input4$Depth>15]<-1
  }else if(lake == "Kuwakatai")
  {
    data_input5<-filter(data,Site == lake) #To get lake specific data
    data_input5$DepthRange[data_input5$Depth<=6]<-0
    data_input5$DepthRange[data_input5$Depth>6]<-1
  }
}
# To combine all lakes data
data_input<-rbind(data_input1,data_input2,data_input3,data_input4,data_input5)
# -----------------------------------------------------------------------------------------
# To get shallow samples only
data_input<-filter(data_input, DepthRange == 0)
#-------------------------------------------------------------------------------------------
#Function to get Yearly data
Get_Yearly<-function(df_yearly){
  df_summary <- summarize(df_yearly, Samples_yearly = n(),
                          Median_yearly = round(median(Value),3), Max_yearly = round(max(Value),3),
                          Exceed_540_yearly = round(sum(Value>540)/n()*100,0),
                          Exceed_260_yearly = round(sum(Value>260)/n()*100,0))
}
#-----------------------------------------------------------------------------------------
#Function to get All Years data
Get_All_Days<-function(df_All_Days){
  df_all_yr<-summarise(df_All_Days,
                       Yearrange = "All",
                       Daily_Samples_Yearly = n(),
                       Median_Yearly = round(median(Value),3),
                       Max_yearly = round(max(Value),3),
                       Exceed_540_yearly = round(sum(Value>540)/n()*100,0), 
                       Exceed_260_yearly = round(sum(Value>260)/n()*100,0))
}
# ----------------------------------------------------------------------------------------------
#Function to get values for all samples
Get_All_Samples<-function(df_all_samples){
  df_all_yr<-summarise(df_all_samples,All_Samples_Yearly = n(),
                       Censored = round(length(which(Detect_count_lt == 1))/All_Samples_Yearly,2)*100)
}
# ----------------------------------------------------------------------------------------------
#Function to set a flag if "<" found
Get_Detect_lt<-function(dt_param){
  dt_param$Detect_count_lt<-ifelse(dt_param$DetectFlag=="<", 1,0)
}
#-----------------------------------------------------------------------------------------------
#Function to set a flag if ">" found
Get_Detect_gt<-function(dt_param){
  dt_param$Detect_count_gt<-ifelse(dt_param$DetectFlag==">", 1,0)
}
#-----------------------------------------------------------------------------------------------
#Looping at the parameters
for (param in df_param){
  if(param!="pH")
  {
    dt_param<-filter(data_input,Parameter == param) #To get parameter specific data
    data_grade<-filter(grade, Parameter == param)#To get parameter specific grades
    dt_param$Date <- as.Date(dt_param$Date,"%m/%d/%Y")
    if(param == "Total Nitrogen"){
      dt_param$Value<-dt_param$Value*1000
    }else if(param == "Total Phosphorus (P)"){
      dt_param$Value<-dt_param$Value*1000
    }else if(param == "Chlorophyll"){
      dt_param$Value<-dt_param$Value*1000
    }
    # Ammonia logic to link with pH using Sample Number
    if(param=="Ammonia (total)"){
      data_ip<-filter(data_input,Parameter == "Ammonia (total)")
      data_ph<-filter(data_input,Parameter == "pH")
      data_input1<-merge(data_ip, data_ph, by=c("SampleNumber","Site"))
      df_uph<-data_input1[!duplicated(data_input1[1:5]),]
      df_uph$ratio<- ifelse(df_uph$Value.y>9,0.2, -0.029809630224*df_uph$Value.y^6 + 1.2641450933*df_uph$Value.y^5 - 22.08947826*df_uph$Value.y^4 + 203.57964573*df_uph$Value.y^3 - 1043.9962833*df_uph$Value.y^2 + 2825.5522036*df_uph$Value.y - 3151.0179751)
      df_uph$Concph8<-df_uph$Value.x/df_uph$ratio
      dt_param<-data.frame("Watershed"=df_uph$Watershed.x,"Site"=df_uph$Site, "Lake Type"=df_uph$Lake.Type.x, "Date"=df_uph$Date.x,"Value"=df_uph$Concph8,
                           "DetectFlag"=df_uph$DetectFlag.x,"Parameter"=df_uph$Parameter.x)
    }
    # ---------------------------------------------------------------------------------------------
    # To group by Watershed, Site, Lake Type and Date
    df_sum<-group_by(dt_param,Watershed,Site,Lake.Type, Date)
    # -------------------------------------------------------------------------------------------
    dt_param$Detect_count_lt<-Get_Detect_lt(dt_param)
    dt_param$Detect_count_gt<-Get_Detect_gt(dt_param)
    df_all_samples<-group_by(dt_param, Watershed, Site, Lake.Type)
    df_All_Samples<-Get_All_Samples(df_all_samples)
    if(param == "E.coli"){
      # To get max of all values for a date for E.Coli
      df_tot<-Get_max(df_sum)
    }else{
      # To get avg of all values for a date for all other Parameters
      df_tot<-Get_avg(df_sum)
    }
    # ---------------------------------------------------------------------------------------------
    #To get the date in required format
    df_date <- data.table(date = as.IDate(df_tot$Date,"%m/%d/%Y"))
    # -------------------------------------------------------------------------------------------
    #To get the water year based on the range in which the date falls
    year<-substring(df_date$date,1,4)
    df_yr<-data.frame("Year"=unique(year))
    df_yr["Year"]<-data.frame(df_yr[order(df_yr$Year),])
    df_yr[, 1] <- as.numeric(as.character( df_yr[, 1] ))
    df_yr$ID <- seq.int(nrow(df_yr))
    #To build the range of start date and end date
    df_yr$St_yr<-paste(df_yr$Year,"01-01", sep="-")
    df_yr$ed_yr<-paste(df_yr$Year+1,"12-31", sep="-")
    df_yr <- df_yr[-nrow(df_yr),]
    df_tot1<-dt_param #For all samples
    data_sum_date <- data.table(date = as.IDate(df_tot$Date,"%m/%d/%Y"))
    data_All_Samples<- data.table(date = as.IDate(df_tot1$Date,"%m/%d/%Y"))
    # To build an interval for Water Year
    interval.dt <- data.table(ID = df_yr$ID, start = df_yr$St_yr, end = df_yr$ed_yr)
    findYearIndex <- function(year) {
      interval.dt[,which((interval.dt$start) <= year & year <= (interval.dt$end))]
    }
    dt_year <- data.table(year = data_sum_date$date)
    dt_year1 <- data.table(year = data_All_Samples$date)
    dt_year$YearIndex <- sapply(dt_year$year, findYearIndex)
    dt_year1$YearIndex <- sapply(dt_year1$year, findYearIndex)
    dt_year$Year<-as.character(dt_year$YearIndex)
    dt_year1$Year<-as.character(dt_year1$YearIndex)
    df_tot$Year<-dt_year$Year
    df_tot1$Year<-dt_year1$Year
    s <- strsplit(df_tot$Year, split = ":")
    s1 <- strsplit(df_tot1$Year, split = ":")
    df_All_Days<-group_by(df_tot, Watershed, Site, Lake.Type)
    df_All_Days_Samples<-Get_All_Days(df_All_Days)
    # To get a dataframe for max/avg samples with Water Year
    df_tot<-data.frame(Watershed = rep(df_tot$Watershed, sapply(s, length)),
                       Site = rep(df_tot$Site, sapply(s, length)),
                       Lake.Type = rep(df_tot$Lake.Type, sapply(s, length)),
                       Date = rep(df_tot$Date, sapply(s, length)),
                       Value = rep(df_tot$Value, sapply(s, length)),
                       Year = unlist(s))
    # To get a dataframe for All samples with Water Year
    df_tot1<-data.frame(Watershed = rep(df_tot1$Watershed, sapply(s1, length)),
                        Site = rep(df_tot1$Site, sapply(s1, length)),
                        Lake.Type = rep(df_tot1$Lake.Type, sapply(s1, length)),
                        Date = rep(df_tot1$Date, sapply(s1, length)),
                        Value = rep(df_tot1$Value, sapply(s1, length)),
                        Year = unlist(s1))
    
    df_yearly<-group_by(df_tot,Watershed, Site, Lake.Type, Year)
    df_yearly1<-group_by(df_tot1,Watershed, Site, Lake.Type, Year)
    df_Yearly<-Get_Yearly(df_yearly)
    # ------------------------------------------------------------------------------------------
    # To get sort the yearly grouped data in ascending order
    df_per_yr<-arrange(df_yearly,Value,.by_group = TRUE)
    # --------------------------------------------------------------------------------------------
    # To get the yearly Hazen 95th Percentile
    df_hazen<-mutate(df_per_yr,count=n(),id = seq_along(Value),rHazen=(0.5+(0.95*count)),ri=trunc(rHazen),rf=rHazen - ri,
                     rin = trunc(rHazen)+1,r1 = ifelse(count==ri, trunc(rHazen), trunc(rHazen)),r1n = ifelse(count==rin, trunc(rHazen)+1, ifelse(count<rin,trunc(rHazen),trunc(rHazen))))
    df_haz <- data.frame("Watershed"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$Watershed,"Site"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$Site,
                         "year"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$Year,"rf"=df_hazen[ which(df_hazen$id==df_hazen$r1),]$rf,"r1"= ifelse(df_hazen[ which(df_hazen$id==df_hazen$r1),]$count>=10,df_hazen[ which(df_hazen$id==df_hazen$r1),]$Value,0),
                         "r1n"= ifelse(df_hazen[ which(df_hazen$id==df_hazen$r1),]$count>=10,df_hazen[ which(df_hazen$id==df_hazen$r1n),]$Value,0)) 
    df_haz$rhazen<-((1-df_haz$rf)*df_haz$r1)+(df_haz$rf*df_haz$r1n)
    # -------------------------------------------------------------------------------------------
    # To get the season based on Water year
    df_Yearly$Yearrange[df_Yearly$Year=="1"]<-"Jan 2013- Dec 2014"
    df_Yearly$Yearrange[df_Yearly$Year=="2"]<-"Jan 2014- Dec 2015"
    df_Yearly$Yearrange[df_Yearly$Year=="3"]<-"Jan 2015- Dec 2016"
    df_Yearly$Yearrange[df_Yearly$Year=="4"]<-"Jan 2016- Dec 2017"
    df_Yearly1<-Get_Yearly(df_yearly1)
    # ------------------------------------------------------------------------------------------
    # To get the All year Hazen 95th Percentile
    df_all_yrs<-arrange(df_All_Days,Value,.by_group = TRUE)
    df_hazen1<-mutate(df_all_yrs,count=n(),id = seq_along(Value),rHazen=(0.5+(0.95*count)),ri=trunc(rHazen),rf=rHazen - ri,
                      rin = trunc(rHazen)+1)
    df_haz1 <- data.frame("Watershed"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Watershed,"Site"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Site,
                          "year"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Year,"rf"=df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$rf,"r1"= ifelse(df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$count>=10,df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$Value,0),
                          "r1n"= ifelse(df_hazen1[ which(df_hazen1$id==df_hazen1$ri),]$count>=10,df_hazen1[ which(df_hazen1$id==df_hazen1$rin),]$Value,0)) 
    df_haz1$rhazen<-round((((1-df_haz1$rf)*df_haz1$r1)+(df_haz1$rf*df_haz1$r1n)),2)
    # -----------------------------------------------------------------------------------------------
    df_All_Samples$rhazen<-df_haz1$rhazen
    df_All_Samples1 <- subset( df_All_Samples, select = -All_Samples_Yearly )
    # To combine Yearly and "All" dataframes
    df_All_Samples1<-merge(df_Yearly,df_All_Samples1,by = c('Watershed','Site'))
    df_summary<-data.frame(Watershed = df_Yearly1$Watershed,
                           Lake_Name = df_Yearly1$Site,
                           Lake_Type = df_Yearly1$Lake.Type,
                           Year_Range = df_Yearly$Yearrange,
                           All_Samples_Yearly = df_Yearly1$Samples_yearly,
                           Censored = df_All_Samples1$Censored,
                           Daily_Samples_Yearly = df_Yearly$Samples_yearly,
                           Median_Yearly = df_Yearly$Median_yearly,
                           Max_Yearly = df_Yearly$Max_yearly,
                           Percentile_Yearly=round(df_haz$rhazen,2),
                           Exceed_540=df_Yearly$Exceed_540_yearly,
                           Exceed_260=df_Yearly$Exceed_260_yearly)
    df_all<-data.frame(Watershed = df_All_Days_Samples$Watershed,
                       Lake_Name = df_All_Days_Samples$Site,
                       Lake_Type = df_All_Days_Samples$Lake.Type,
                       Year_Range = df_All_Days_Samples$Yearrange,
                       All_Samples_Yearly = df_All_Samples$All_Samples_Yearly,
                       Censored = df_All_Samples$Censored,
                       Daily_Samples_Yearly = df_All_Days_Samples$Daily_Samples_Yearly,
                       Median_Yearly = df_All_Days_Samples$Median_Yearly,
                       Max_Yearly = df_All_Days_Samples$Max_yearly,
                       Percentile_Yearly=round(df_All_Samples$rhazen,2),
                       Exceed_540=df_All_Days_Samples$Exceed_540_yearly,
                       Exceed_260=df_All_Days_Samples$Exceed_260_yearly)
    df_final<-rbind(df_summary,df_all)
    df_final<-arrange(df_final, Watershed, Lake_Name)
    df_summary<-df_final
    df_summary<-filter(df_summary, Year_Range == "All")
    df_summary$Year_Range<-NULL
    # ------------------------------------------------------------------------------------------
    #To get the unique functions(Median, Percentile, Max, Exceed 260, Exceed 540)
    df_function<-as.character(unique(data_grade$Function))
    df_function<-df_function[df_function != ""]
    #Looping at the functions to get the grades
    for(func in df_function){
      # --------------------------------------------------------------------------------------------------------
      Gr_func = filter(data_grade, Function == func)
      interval.gr <- data.table(Grade = Gr_func$Grade, gt = Gr_func$gt, lt = Gr_func$lt)
      # To get the Median attribute state yearly and all years
      if(func == "Median")
      {
        findIndex <- function(Median_Yearly) 
        {
          interval.gr[,which((interval.gr$gt) < Median_Yearly & Median_Yearly <= (interval.gr$lt))]
        }
        df_summary$Index <- sapply(df_summary$Median_Yearly, findIndex)
        df_summary$Median_Attr_state_yearly <- Gr_func[df_summary$Index,]$Grade
      }
      # ----------------------------------------------------------------------------------------------------------
      if(func == "Polymictic")
      {
        findIndex <- function(Median_Yearly) 
        {
          interval.gr[,which((interval.gr$gt) < Median_Yearly & Median_Yearly <= (interval.gr$lt))]
        }
        df_summary$Index1 <- sapply(df_summary$Median_Yearly, findIndex)
      }
      # ----------------------------------------------------------------------------------------------------------
      if(func == "Seasonally Stratified")
      {
        findIndex <- function(Median_Yearly) 
        {
          interval.gr[,which((interval.gr$gt) < Median_Yearly & Median_Yearly <= (interval.gr$lt))]
        }
        df_summary$Index3 <- sapply(df_summary$Median_Yearly, findIndex)
      }
      # ----------------------------------------------------------------------------------------------------------
      if(param == "Total Nitrogen"){
        df_summary$Median_Attr_state_yearly<- ifelse(df_summary$Lake_Type =="Polymictic", Gr_func[df_summary$Index1,]$Grade,
                                                     ifelse(df_summary$Lake_Type =="Seasonally Stratified", Gr_func[df_summary$Index3,]$Grade, "NA"))
        df_summary$Median_Attr_state_yearly<-ifelse(df_summary$Median_Attr_state_yearly=="1","A",
                                                    ifelse(df_summary$Median_Attr_state_yearly=="2","B",
                                                           ifelse(df_summary$Median_Attr_state_yearly=="3","C",
                                                                  ifelse(df_summary$Median_Attr_state_yearly=="4","D","NA"))))
      }
      # -----------------------------------------------------------------------------------------------------------
      # To get the Percentile attribute state yearly and all years
      else if(func == "Percentile")
      {
        findIndex <- function(Percentile_Yearly) 
        {
          interval.gr[,which((interval.gr$gt) < Percentile_Yearly & Percentile_Yearly <= (interval.gr$lt))]
        }
        df_summary$Index2 <- sapply(df_summary$Percentile_Yearly, findIndex)
        df_summary$Percentile_Attr_state_yearly <- Gr_func[df_summary$Index2,]$Grade
        df_summary$Percentile_Attr_state_yearly<-ifelse(df_summary$Percentile_Yearly=="NA","NA",as.character(df_summary$Percentile_Attr_state_yearly))
      }
      # -----------------------------------------------------------------------------------------------------------
      # To get the Max attribute state yearly and all years
      else if(func == "Max")
      {
        findIndex <- function(Max_Yearly) 
        {
          interval.gr[,which((interval.gr$gt) < Max_Yearly & Max_Yearly <= (interval.gr$lt))]
        }
        df_summary$Index6 <- sapply(df_summary$Max_Yearly, findIndex)
        df_summary$Max_Attr_state_yearly <- Gr_func[df_summary$Index6,]$Grade
        
      }
      # ------------------------------------------------------------------------------------------------------------------
      # To get the 260 cfu / 100 mL exceedances attribute state yearly and all years
      else if(func == "Exceed_260")
      {
        findIndex <- function(Exceed_260) 
        {
          interval.gr[,which((interval.gr$gt) < Exceed_260 & Exceed_260 <= (interval.gr$lt))]
        }
        df_summary$Index8 <- sapply(df_summary$Exceed_260, findIndex)
        df_summary$Exceed_260_Attribute_state_yearly <- Gr_func[df_summary$Index8,]$Grade
      }
      # ----------------------------------------------------------------------------------------------------------------
      # To get the 540 cfu / 100 mL exceedances attribute state yearly and all years
      else if(func == "Exceed_540")
      {
        findIndex <- function(Exceed_540) 
        {
          interval.gr[,which((interval.gr$gt) < Exceed_540 & Exceed_540 <= (interval.gr$lt))]
        }
        df_summary$Index11 <- sapply(df_summary$Exceed_540, findIndex)
        df_summary$Exceed_540_Attribute_state_yearly <- Gr_func[df_summary$Index11,]$Grade
      }
      # -------------------------------------------------------------------------------------------------------------
    }
    # To get the Overall Attribute State
    if(param == "Ammonia (total)"){
      df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Median_Attr_state_yearly=="D",4,"NA"))))
      df_summary$Max_value<-ifelse(df_summary$Max_Attr_state_yearly=="A",1,
                                   ifelse(df_summary$Max_Attr_state_yearly=="B",2,
                                          ifelse(df_summary$Max_Attr_state_yearly=="C",3,
                                                 ifelse(df_summary$Max_Attr_state_yearly=="D",4,"NA"))))
      df_max<-data.frame(df_summary$Med_val,df_summary$Max_value)
    }else if(param == "Total Nitrogen"){
      df_summary$Overall_Attribute_State<-df_summary$Median_Attr_state_yearly
    }else if(param == "Total Phosphorus (P)"){
      df_summary$Overall_Attribute_State<-df_summary$Median_Attr_state_yearly
    }else if(param == "Chlorophyll"){
      df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Median_Attr_state_yearly=="D",4,"NA"))))
      df_summary$Max_value<-ifelse(df_summary$Max_Attr_state_yearly=="A",1,
                                   ifelse(df_summary$Max_Attr_state_yearly=="B",2,
                                          ifelse(df_summary$Max_Attr_state_yearly=="C",3,
                                                 ifelse(df_summary$Max_Attr_state_yearly=="D",4,"NA"))))
      df_max<-data.frame(df_summary$Med_val,df_summary$Max_value)
    }else if(param == "E.coli"){
      df_summary$ex260_val<-ifelse(df_summary$Exceed_260_Attribute_state_yearly=="A",1,
                                   ifelse(df_summary$Exceed_260_Attribute_state_yearly=="B",2,
                                          ifelse(df_summary$Exceed_260_Attribute_state_yearly=="C",3,
                                                 ifelse(df_summary$Exceed_260_Attribute_state_yearly=="D",4,
                                                        ifelse(df_summary$Exceed_260_Attribute_state_yearly=="E",5,
                                                               "NA")))))
      df_summary$ex540_val<-ifelse(df_summary$Exceed_540_Attribute_state_yearly=="A",1,
                                   ifelse(df_summary$Exceed_540_Attribute_state_yearly=="B",2,
                                          ifelse(df_summary$Exceed_540_Attribute_state_yearly=="C",3,
                                                 ifelse(df_summary$Exceed_540_Attribute_state_yearly=="D",4,
                                                        ifelse(df_summary$Exceed_540_Attribute_state_yearly=="E",5,
                                                               "NA")))))
      df_summary$Med_val<-ifelse(df_summary$Median_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Median_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Median_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Median_Attr_state_yearly=="D",4,"NA"))))
      df_summary$Per_val<-ifelse(df_summary$Percentile_Attr_state_yearly=="A",1,
                                 ifelse(df_summary$Percentile_Attr_state_yearly=="B",2,
                                        ifelse(df_summary$Percentile_Attr_state_yearly=="C",3,
                                               ifelse(df_summary$Percentile_Attr_state_yearly=="D",4,
                                                      ifelse(df_summary$Percentile_Attr_state_yearly=="E",5,
                                                             ifelse(df_summary$Percentile_Attr_state_yearly=="NA",1,"NA"))))))
      df_max<-data.frame(df_summary$Med_val,df_summary$ex540_val, df_summary$ex260_val, df_summary$Per_val)
    }
    if((param != "Total Nitrogen") & (param != "Total Phosphorus (P)")){
      df_summary$Max_val<-apply(X=df_max, MARGIN=1, FUN=max)
      df_summary$Overall_Attribute_State<-ifelse(df_summary$Max_val==1,"A",
                                                 ifelse(df_summary$Max_val==2,"B",
                                                        ifelse(df_summary$Max_val==3,"C",
                                                               ifelse(df_summary$Max_val==4,"D",
                                                                      ifelse(df_summary$Max_val==5,"E",
                                                                             "NA")))))
    }
    if(param == "Ammonia (total)"){
      df_amm_l<-select(df_summary, Watershed, Lake_Name, Lake_Type, All_Samples_Yearly, Censored, Daily_Samples_Yearly,
                     Median_Yearly,Max_Yearly, Median_Attr_state_yearly, Max_Attr_state_yearly, Overall_Attribute_State)
    }else if(param == "Total Nitrogen"){
      df_ton_l<-select(df_summary, Watershed, Lake_Name, Lake_Type, All_Samples_Yearly, Censored, Daily_Samples_Yearly,
                     Median_Yearly, Median_Attr_state_yearly, Overall_Attribute_State)
    }else if(param == "Total Phosphorus (P)"){
      df_phos_l<-select(df_summary, Watershed, Lake_Name, Lake_Type, All_Samples_Yearly, Censored, Daily_Samples_Yearly,
                      Median_Yearly, Median_Attr_state_yearly, Overall_Attribute_State)
    }else if(param == "Chlorophyll"){
      df_chlor_l<-select(df_summary, Watershed, Lake_Name, Lake_Type, All_Samples_Yearly, Censored, Daily_Samples_Yearly,
                       Median_Yearly, Max_Yearly, Median_Attr_state_yearly, Max_Attr_state_yearly, Overall_Attribute_State)
    }else if(param == "E.coli"){
      df_ecoli_l<-select(df_summary, Watershed, Lake_Name, Lake_Type, All_Samples_Yearly, Censored, Daily_Samples_Yearly,
                       Exceed_540, Exceed_260, Median_Yearly, Percentile_Yearly, Exceed_540_Attribute_state_yearly, Exceed_260_Attribute_state_yearly,
                       Median_Attr_state_yearly, Percentile_Attr_state_yearly, Overall_Attribute_State)
    }
  }
}
# #To write parameters to separate tabs in an excel file as output
l <- list("E.coli" = df_ecoli_l, "Total Phosphorus (P)" = df_phos_l, "TON" = df_ton_l, "Ammonia (total)" = df_amm_l, 
           "Chlorophyll" = df_chlor_l)
write.xlsx(l, file = "Parameter_Grades_Analysis-Lakes.xlsx")

# spiderl_ecoli<-data.frame("Watershed"=df_ecoli_l$Watershed, "Lake_Name"=df_ecoli_l$Lake_Name,
#                          "E.coli"=df_ecoli_l$Overall_Attribute_State)
# spiderl_phos<-data.frame("Watershed"=df_phos_l$Watershed, "Lake_Name"=df_phos_l$Lake_Name,
#                          "Phosphorus"=df_phos_l$Overall_Attribute_State)
# spiderl_ton<-data.frame("Watershed"=df_ton_l$Watershed, "Lake_Name"=df_ton_l$Lake_Name,
#                         "TON"=df_ton_l$Overall_Attribute_State)
# spiderl_amm<-data.frame("Watershed"=df_amm_l$Watershed, "Lake_Name"=df_amm_l$Lake_Name,
#                        "Ammonia"=df_amm_l$Overall_Attribute_State)
# spiderl_chlor<-data.frame("Watershed"=df_chlor_l$Watershed, "Lake_Name"=df_chlor_l$Lake_Name,
#                        "Chlorophyll"=df_chlor_l$Overall_Attribute_State)
# spiderl<-merge(spiderl_ecoli, spiderl_phos, by=c('Watershed','Lake_Name'))
# spiderl<-merge(spiderl, spiderl_ton, by=c('Watershed','Lake_Name'))
# spiderl<-merge(spiderl, spiderl_amm, by=c('Watershed','Lake_Name'))
# spiderl<-merge(spiderl, spiderl_chlor, by=c('Watershed','Lake_Name'))
# spiderl$E.coli<-ifelse(spiderl$E.coli=="A",1,
#                                            ifelse(spiderl$E.coli=="B",2,
#                                                   ifelse(spiderl$E.coli=="C",3,
#                                                          ifelse(spiderl$E.coli=="D",4,
#                                                                 ifelse(spiderl$E.coli=="E",5,
#                                                                        "NA")))))
# spiderl$Phosphorus<-ifelse(spiderl$Phosphorus=="A",1,
#                       ifelse(spiderl$Phosphorus=="B",2,
#                              ifelse(spiderl$Phosphorus=="C",3,
#                                     ifelse(spiderl$Phosphorus=="D",4,
#                                            ifelse(spiderl$Phosphorus=="E",5,
#                                                   "NA")))))
# spiderl$TON<-ifelse(spiderl$TON=="A",1,
#                       ifelse(spiderl$TON=="B",2,
#                              ifelse(spiderl$TON=="C",3,
#                                     ifelse(spiderl$TON=="D",4,
#                                            ifelse(spiderl$TON=="E",5,
#                                                   "NA")))))
# spiderl$Ammonia<-ifelse(spiderl$Ammonia=="A",1,
#                       ifelse(spiderl$Ammonia=="B",2,
#                              ifelse(spiderl$Ammonia=="C",3,
#                                     ifelse(spiderl$Ammonia=="D",4,
#                                            ifelse(spiderl$Ammonia=="E",5,
#                                                   "NA")))))
# spiderl$Chlorophyll<-ifelse(spiderl$Chlorophyll=="A",1,
#                       ifelse(spiderl$Chlorophyll=="B",2,
#                              ifelse(spiderl$Chlorophyll=="C",3,
#                                     ifelse(spiderl$Chlorophyll=="D",4,
#                                            ifelse(spiderl$Chlorophyll=="E",5,
#                                                   "NA")))))
# spiderl_first<-data.frame("Watershed"=" ", "Lake_Name"="1",
#                           "E.coli"="1", "Phosphorus" = "1" ,"TON"="1","Ammonia"="1", 
#                           "Chlorophyll"="1")
# spiderl_second<-data.frame("Watershed"=" ", "Lake_Name"="2",
#                            "E.coli"="5", "Phosphorus" = "5" ,"TON"="5","Ammonia"="5", 
#                            "Chlorophyll"="5")
# spiderl <- rbind( spiderl_second,spiderl)
# spiderl <- rbind( spiderl_first, spiderl)
# ---------------------------------------------------------------------------------------------
library("fmsb")
# For Rivers
spider <- read.csv("spider_ABCD_all_r.csv",fileEncoding="UTF-8-BOM")
names <- as.character(unique(spider$X.1[-(1:2)]))
for (i in names) {
  spider <- read.csv("spider_ABCD_all_r.csv",fileEncoding="UTF-8-BOM")
  #pull site name, and first two rows to format the data frame (min and max of plot)
  spider <- spider[spider$X.1==i | spider$X.1==1 | spider$X.1==2,]
  spider <- spider[,-1]
  spider <- sapply( spider, as.numeric )
  rownames(spider) <- c("1","2",i)
  
  spider <- spider[,-1]
  spider <- as.data.frame(spider)
  
  #rename the parameters we are plotting
  # names(spider) <- c("E.coli", "Ammonia", "TON", "Cu (soluble)", "Cu (total)","Zn (soluble)", "Zn (total)", "Dissolved \nOxygen")
  # spider <- spider[,c(1:4,6)] #subset of parameters
  spider <- spider[,c(1:5)] #subset of parameters
  names(spider) <- c("E.coli","Copper", "Nitrate", "Ammonia", "Zinc")
  par(bg=NA) #transparent background
  if(i == "Nukumea @ Upper "){
    hc_nu<-spider
  }else if(i == "Okura Creek "){
    hc_oc<-spider
  }else if(i == "Vaughn Stream "){
    hc_vs<-spider
  }else if(i == "Waiwera Stream "){
    hc_ws<-spider
  }else if(i == "West Hoe Stream "){
    hc_wh<-spider
  }else if(i == "Cascades @ Whakanewh "){
    is_cw<-spider
  }else if(i == "Onetangi @ Waiheke R "){
    is_ow<-spider
  }else if(i == "Kaukapakapa @Taylors "){
    ka_kt<-spider
  }else if(i == "Kumeu River "){
    ka_kr<-spider
  }else if(i == "Makarau @ Railway "){
    ka_mr<-spider
  }else if(i == "Riverhead Stream "){
    ka_rs<-spider
  }else if(i == "Mahurangi River FHQ "){
    ma_mf<-spider
  }else if(i == "Mahurangi River WS "){
    ma_mw<-spider
  }else if(i == "Ngakaroa Stream "){
    mh_ns<-spider
  }else if(i == "Papakura @ Alfriston "){
    mh_pa<-spider
  }else if(i == "Papakura Stream "){
    mh_ps<-spider
  }else if(i == "Puhinui Stream "){
    mh_pu<-spider
  }else if(i == "Waitangi Falls Br. "){
    mh_wa<-spider
  }else if(i == "Whangamaire Woodhous "){
    mh_ww<-spider
  }else if(i == "Matakana River "){
    ne_mr<-spider
  }else if(i == "Omaru @ Maybury "){
    ta_om<-spider
  }else if(i == "Otaki Creek "){
    ta_oc<-spider
  }else if(i == "Otara Ck East Tamaki "){
    ta_ot<-spider
  }else if(i == "Otara Ck Kennel Hill "){
    ta_ok<-spider
  }else if(i == "Pakuranga Ck Botany "){
    ta_pc<-spider
  }else if(i == "Pakuranga Ck Greenmt "){
    ta_pg<-spider
  }else if(i == "Wairoa @ Caitchons "){
    wa_wc<-spider
  }else if(i == "Wairoa River "){
    wa_wr<-spider
  }else if(i == "Avondale Stream @ Sh "){
    wt_as<-spider
  }else if(i == "Lucas Creek "){
    wt_lc<-spider
  }else if(i == "Oakley Creek "){
    wt_oc<-spider
  }else if(i == "Opanuku Stream "){
    wt_os<-spider
  }else if(i == "Oteha Stream "){
    wt_ot<-spider
  }else if(i == "Parrs Cross "){
    wt_pc<-spider
  }else if(i == "Rangitopuni River "){
    wt_rr<-spider
  }else if(i == "Woodside Res "){
    wt_wr<-spider
  }else if(i == "Cascade Stream "){
    wc_cs<-spider
  }
}
# -------------------------------------------------------------------------------------------------------------
# For Lakes
spider <- read.csv("spider_ABCD_all_l.csv",fileEncoding="UTF-8-BOM")
names <- as.character(unique(spider$X.1[-(1:2)]))
for (i in names) {
  spider <- read.csv("spider_ABCD_all_l.csv",fileEncoding="UTF-8-BOM")
  #pull site name, and first two rows to format the data frame (min and max of plot)
  spider <- spider[spider$X.1==i | spider$X.1==1 | spider$X.1==2,]
  spider <- spider[,-1]
  spider <- sapply( spider, as.numeric )
  rownames(spider) <- c("1","2",i)
  
  spider <- spider[,-1]
  spider <- as.data.frame(spider)
  spider <- spider[,c(1:5)] #subset of parameters
  names(spider) <- c("E.coli", "Phosphorus", "Nitrate", "Ammonia", "Chlorophyll")
  par(bg=NA) #transparent background
  if(i == "Pupuke"){
    hc_pu<-spider
  }else if(i == "Tomarata"){
    ne_to<-spider
  }else if(i == "Kuwakatai"){
    wc_kw<-spider
  }else if(i == "Ototoa"){
    wc_oa<-spider
  }else if(i == "Wainamu"){
    wc_wu<-spider
  }
}
# -----------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  
  # App title ----
  titlePanel("Auckland Current State Tables for Rivers and Lakes"),
    selectInput(
      # Inputs excluded for brevity
    inputId = "wb", label = "Choose:",
      choices = c("Lakes", "Rivers")
    ),
  
  # Main panel for displaying outputs ----
  mainPanel(conditionalPanel(condition="input.wb == 'Rivers'",
                     tabsetPanel(
      id = 'dataset',
      tabPanel("E.coli", DT::dataTableOutput("mytable1")),
      tabPanel("Copper (soluble)", DT::dataTableOutput("mytable2")),
      # tabPanel("Copper (total)", DT::dataTableOutput("mytable3")),
      tabPanel("Zinc (soluble)", DT::dataTableOutput("mytable3")),
      # tabPanel("Zinc (total)", DT::dataTableOutput("mytable5")),
      tabPanel("Total Oxidized Nitrogen", DT::dataTableOutput("mytable4")),
      tabPanel("Ammonia (total)", DT::dataTableOutput("mytable5")),
      tabPanel("DO", DT::dataTableOutput("mytable6"))
    )
  )
 ),
 mainPanel(conditionalPanel(condition="input.wb == 'Lakes'",
   tabsetPanel(
     id = 'dataset',
     tabPanel("E.coli", DT::dataTableOutput("mytable7"),
              # WHERE YOUR FOOTER GOES
              hr(),
              print("*No data available for year 2014-2015 for sites - Pupuke, Tomarata, Kuwakatai and Ototoa")),
     tabPanel("Total Phosphorus", DT::dataTableOutput("mytable8")),
     tabPanel("Total Nitrogen", DT::dataTableOutput("mytable9")),
     tabPanel("Total Ammonia", DT::dataTableOutput("mytable10")),
     tabPanel("Chlorophyll", DT::dataTableOutput("mytable11"))
 )
)
)
)
# Define server logic to plot various variables
server <- function(input, output, session) {
  # --------------------------------------------------------------------
  output$myplot1<-renderPlot({
    radarchart( hc_nu  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nNukumea @ Upper"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot2<-renderPlot({
    radarchart( hc_oc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOkura Creek"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot3<-renderPlot({
    radarchart( hc_vs  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nVaughn Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot4<-renderPlot({
    radarchart( hc_ws  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWaiwera Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot5<-renderPlot({
    radarchart( hc_wh  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWest Hoe Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot6<-renderPlot({
    radarchart( is_cw  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nCascades @ Whakanewh"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot7<-renderPlot({
    radarchart( is_ow  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOnetangi @ Waiheke R"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot8<-renderPlot({
    radarchart( ka_kt  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nKaukapakapa @Taylors"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot9<-renderPlot({
    radarchart( ka_kr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nKumeu River"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot10<-renderPlot({
    radarchart( ka_mr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nMakarau @ Railway"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot11<-renderPlot({
    radarchart( ka_rs  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nRiverhead Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot12<-renderPlot({
    radarchart( ma_mf  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nMahurangi River FHQ"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot13<-renderPlot({
    radarchart( ma_mw  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nMahurangi River WS"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot14<-renderPlot({
    radarchart( mh_ns  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nNgakaroa Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot15<-renderPlot({
    radarchart( mh_pa  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPapakura @ Alfriston"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot16<-renderPlot({
    radarchart( mh_ps  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPapakura Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot17<-renderPlot({
    radarchart( mh_pu  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPuhinui Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot18<-renderPlot({
    radarchart( mh_wa  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWaitangi Falls Br."),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot19<-renderPlot({
    radarchart( mh_ww  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWhangamaire Woodhous"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot20<-renderPlot({
    radarchart( ne_mr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nMatakana River"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot21<-renderPlot({
    radarchart( ta_om  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOmaru @ Maybury"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot22<-renderPlot({
    radarchart( ta_oc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOtaki Creek"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot23<-renderPlot({
    radarchart( ta_ot  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOtara Ck East Tamaki"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot24<-renderPlot({
    radarchart( ta_ok  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOtara Ck Kennel Hill"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot25<-renderPlot({
    radarchart( ta_pc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPakuranga Ck Botany"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot26<-renderPlot({
    radarchart( ta_pg  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPakuranga Ck Greenmt"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot27<-renderPlot({
    radarchart( wa_wc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWairoa @ Caitchons"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot28<-renderPlot({
    radarchart( wa_wr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWairoa River"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot29<-renderPlot({
    radarchart( wt_as  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nAvondale Stream @ Sh"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot30<-renderPlot({
    radarchart( wt_lc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nLucas Creek"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot31<-renderPlot({
    radarchart( wt_oc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOakley Creek"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot32<-renderPlot({
    radarchart( wt_os  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOpanuku Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot33<-renderPlot({
    radarchart( wt_ot  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOteha Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot34<-renderPlot({
    radarchart( wt_pc  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nParrs Cross"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot35<-renderPlot({
    radarchart( wt_rr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nRangitopuni River"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot36<-renderPlot({
    radarchart( wt_wr  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWoodside Res"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot37<-renderPlot({
    radarchart( wc_cs  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nCascade Stream"),
                #custom polygon
                 pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                 caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot38<-renderPlot({
    radarchart( hc_pu  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nPupuke"),
                #custom polygon
                pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot39<-renderPlot({
    radarchart( ne_to  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nTomarata"),
                #custom polygon
                pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot40<-renderPlot({
    radarchart( wc_kw  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nKuwakatai"),
                #custom polygon
                pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot41<-renderPlot({
    radarchart( wc_oa  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nOtotoa"),
                #custom polygon
                pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  output$myplot42<-renderPlot({
    radarchart( wc_wu  , axistype=1 , na.itp=FALSE,
                title=paste("5-year Numeric Attribute States for\nWainamu"),
                #custom polygon
                pcol=rgb(255/255,182/255,193/255,1), pfcol=rgb(255/255,182/255,193/255,.2) ,
                #custom the grid
                cglcol="darkgrey", cglty=1, axislabcol="darkred", cglwd=1, pty=18, plwd=3, plty=9,
                #custom labels
                vlcex=1,
                #axis labels for grades
                caxislabels=c("E","D", "C", "B", "A"), calcex=1.5)
  })
  # --------------------------------------------------------------------
  # To place TON into html datatable
  output$mytable4<-renderDataTable({datatable(df_ton_r,
                                                      style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 4: Total Oxidized Nitrogen'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median', '5-Year 95th Percentile','5-Year Median A.S.', "5-Year 95th Percentile A.S.",'Overall Attribute State')
                                                      # , rownames = FALSE,
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                              )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Percentile_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato'))) %>% 
      formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------------
  # # To place Ammonia into html datatable
  output$mytable5<-DT::renderDataTable({DT::datatable(df_amm_r,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;','Table 5: Ammonia (total)'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median', "5-Year Max",'5-Year Median A.S.', "5-Year Max A.S.",'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Max_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------
  # To place DO into a html datatable
  output$mytable6<-DT::renderDataTable({DT::datatable(df_do_r,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 6: DO'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%','5-Year Mean', '5-Year Min', '5-Year Mean A.S.','5-Year Min A.S.', 
                                                                   'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                                    Shiny.onInputChange('click', Math.random());});")
                                                      )%>% 
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Min_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Mean_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato'))
      )%>% formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------
  # To place Copper(Soluble) into a html datatable
  output$mytable2<-DT::renderDataTable({DT::datatable(df_cus_r,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 2: Copper (soluble)'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median',"5-Year 95th Percentile",'5-Year Median A.S.', "5-Year 95th Percentile A.S.",'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                                    Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Percentile_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # ----------------------------------------------------------------------------------------------------------------------
  # To place Copper(Total) into a html datatable
  # output$mytable3<-DT::renderDataTable({DT::datatable(df_cut_r,style="default",
  #                                                     options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
  #                                                     caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 3: Copper (total)'),
  #                                                     colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median', "5-Year 95th Percentile",'5-Year Median A.S.', "5-Year 95th Percentile A.S.",'Overall Attribute State')
  #                                                     ,selection = 'none',
  #                                                     callback = JS("table.on('click.dt', 'td', function() {
  #                                                                   Shiny.onInputChange('click', Math.random());});")
  #                                                     )%>%
  #     formatStyle(
  #       'Overall_Attribute_State',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(
  #       'Median_Attr_state_yearly',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(
  #       'Percentile_Attr_state_yearly',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------
  # To place Zinc(Soluble) into a html datatable
  output$mytable3<-DT::renderDataTable({DT::datatable(df_zns_r,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 3: Zinc (soluble)'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median', "5-Year 95th Percentile",'5-Year Median A.S.', "5-Year 95th Percentile A.S.",'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Percentile_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # ----------------------------------------------------------------------------------------------------------------
  # To place Zinc(total) into a html datatable
  # output$mytable5<-DT::renderDataTable({DT::datatable(df_znt_r,style="default",
  #                                                     options=list(columnDefs = list(list(className = 'dt-center', targets=1:10))),
  #                                                     caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 5: Zinc(Total)'),
  #                                                     colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year Median', "5-Year 95th Percentile",'5-Year Median A.S.',  "5-Year 95th Percentile per A.S.",'Overall Attribute State')
  #                                                     ,selection = 'none',
  #                                                     callback = JS("table.on('click.dt', 'td', function() {
  #                                                     Shiny.onInputChange('click', Math.random());});")
  #                                                     )%>%
  #     formatStyle(
  #       'Overall_Attribute_State',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(
  #       'Median_Attr_state_yearly',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(
  #       'Percentile_Attr_state_yearly',
  #       target = 'cell',
  #       backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
  #     formatStyle(2, cursor = 'pointer')})
  # ----------------------------------------------------------------------------------------------------------------
  # To place E.Coli into a html datatable
  output$mytable1<-DT::renderDataTable({DT::datatable(df_ecoli_r,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:14))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 1: E.Coli'),
                                                      colnames = c('Watershed', 'Site', 'SiteId', 'Sample Count', 'Censored%', '5-Year % Exceeding 540', '5-Year % Exceeding 260', '5-Year Median', "5-Year 95th Percentile",'5-Year % Exceeding 540 A.S.', '5-Year % Exceeding 260 A.S.', '5-Year Median A.S.', "5-Year 95th Percentile A.S.", 'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                                    Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Exceed_540_Attribute_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Exceed_260_Attribute_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Percentile_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # ---------------------------------------------------------------------------------------------------------------
  # Tables for Lakes
  # To place TON into html datatable
  output$mytable10<-DT::renderDataTable({DT::datatable(df_amm_l,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:11))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 4: Total Ammonia'),
                                                      colnames = c('Watershed', 'Lake Name', 'Lake Type', 'Sample Count', 'Censored%', 'Daily Samples','5-Year Median', "5-Year Max",'5-Year Median A.S.', "5-Year Max A.S.",'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Max_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------------
  # # To place Ammonia into html datatable
  output$mytable9<-DT::renderDataTable({DT::datatable(df_ton_l,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:9))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;','Table 3: Total Nitrogen'),
                                                      colnames = c('Watershed', 'Lake Name', 'Lake Type', 'Sample Count', 'Censored%', 'Daily Samples', '5-Year Median', '5-Year Median A.S.', 'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------
  # To place DO into a html datatable
  output$mytable8<-DT::renderDataTable({DT::datatable(df_phos_l,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:9))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 2: Total Phosphorus'),
                                                      colnames = c('Watershed', 'Lake Name', 'Lake Type', 'Sample Count', 'Censored%', 'Daily Samples', '5-Year Median', '5-Year Median A.S.', 'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # --------------------------------------------------------------------------------------------------------------------
  # To place Copper(Soluble) into a html datatable
  output$mytable11<-DT::renderDataTable({DT::datatable(df_chlor_l,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:11))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 5: Chlorophyll'),
                                                      colnames = c('Watershed', 'Lake Name', 'Lake Type', 'Sample Count', 'Censored%', 'Daily Samples','5-Year Median', "5-Year Max",'5-Year Median A.S.', "5-Year Max A.S.",'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato')))%>% 
      formatStyle(
        'Max_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D"), c('greenyellow','yellow','orange','tomato'))) %>% 
      formatStyle(2, cursor = 'pointer')})
  # ----------------------------------------------------------------------------------------------------------------
  # To place E.Coli into a html datatable
  output$mytable7<-DT::renderDataTable({DT::datatable(df_ecoli_l,style="default",
                                                      options=list(columnDefs = list(list(className = 'dt-center', targets=1:14))),
                                                      caption = htmltools::tags$caption(style = 'text-align: left; color:black; font-size: 20px;', 'Table 1: E.Coli'),
                                                      colnames = c('Watershed', 'Lake Name', 'Lake Type', 'Sample Count', 'Censored%', 'Daily Samples', '5-Year % Exceeding 540', '5-Year % Exceeding 260', '5-Year Median', "5-Year 95th Percentile",'5-Year % Exceeding 540 A.S.', '5-Year % Exceeding 260 A.S.', '5-Year Median A.S.', "5-Year 95th Percentile A.S.", 'Overall Attribute State')
                                                      ,selection = 'none',
                                                      callback = JS("table.on('click.dt', 'td', function() {
                                                      Shiny.onInputChange('click', Math.random());});")
                                                      )%>%
      formatStyle(
        'Overall_Attribute_State',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Exceed_540_Attribute_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Exceed_260_Attribute_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Median_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>%
      formatStyle(
        'Percentile_Attr_state_yearly',
        target = 'cell',
        backgroundColor = styleEqual(c("A","B","C","D","E"), c('greenyellow','yellow','orange','tomato','tomato')))%>% 
      formatStyle(2, cursor = 'pointer')})
  # -------------------------------------------------------------------------------
  # define modal
  plotModal1 <- function() {
    modalDialog(
      plotOutput("myplot1")
    )
  }
  plotModal2 <- function() {
    modalDialog(
      plotOutput("myplot2")
    )
  }
  plotModal3 <- function() {
    modalDialog(
      plotOutput("myplot3")
    )
  }
  plotModal4 <- function() {
    modalDialog(
      plotOutput("myplot4")
    )
  }
  plotModal5 <- function() {
    modalDialog(
      plotOutput("myplot5")
    )
  }
  plotModal6 <- function() {
    modalDialog(
      plotOutput("myplot6")
    )
  }
  plotModal7 <- function() {
    modalDialog(
      plotOutput("myplot7")
    )
  }
  plotModal8 <- function() {
    modalDialog(
      plotOutput("myplot8")
    )
  }
  plotModal9 <- function() {
    modalDialog(
      plotOutput("myplot9")
    )
  }
  plotModal10 <- function() {
    modalDialog(
      plotOutput("myplot10")
    )
  }
  plotModal11 <- function() {
    modalDialog(
      plotOutput("myplot11")
    )
  }
  plotModal12 <- function() {
    modalDialog(
      plotOutput("myplot12")
    )
  }
  plotModal13 <- function() {
    modalDialog(
      plotOutput("myplot13")
    )
  }
  plotModal14 <- function() {
    modalDialog(
      plotOutput("myplot14")
    )
  }
  plotModal15 <- function() {
    modalDialog(
      plotOutput("myplot15")
    )
  }
  plotModal16 <- function() {
    modalDialog(
      plotOutput("myplot16")
    )
  }
  plotModal17 <- function() {
    modalDialog(
      plotOutput("myplot17")
    )
  }
  plotModal18 <- function() {
    modalDialog(
      plotOutput("myplot18")
    )
  }
  plotModal19 <- function() {
    modalDialog(
      plotOutput("myplot19")
    )
  }
  plotModal20 <- function() {
    modalDialog(
      plotOutput("myplot20")
    )
  }
  plotModal21 <- function() {
    modalDialog(
      plotOutput("myplot21")
    )
  }
  plotModal22 <- function() {
    modalDialog(
      plotOutput("myplot22")
    )
  }
  plotModal23 <- function() {
    modalDialog(
      plotOutput("myplot23")
    )
  }
  plotModal24 <- function() {
    modalDialog(
      plotOutput("myplot24")
    )
  }
  plotModal25 <- function() {
    modalDialog(
      plotOutput("myplot25")
    )
  }
  plotModal26 <- function() {
    modalDialog(
      plotOutput("myplot26")
    )
  }
  plotModal27 <- function() {
    modalDialog(
      plotOutput("myplot27")
    )
  }
  plotModal28 <- function() {
    modalDialog(
      plotOutput("myplot28")
    )
  }
  plotModal29 <- function() {
    modalDialog(
      plotOutput("myplot29")
    )
  }
  plotModal30 <- function() {
    modalDialog(
      plotOutput("myplot30")
    )
  }
  plotModal31 <- function() {
    modalDialog(
      plotOutput("myplot31")
    )
  }
  plotModal32 <- function() {
    modalDialog(
      plotOutput("myplot32")
    )
  }
  plotModal33 <- function() {
    modalDialog(
      plotOutput("myplot33")
    )
  }
  plotModal34 <- function() {
    modalDialog(
      plotOutput("myplot34")
    )
  }
  plotModal35 <- function() {
    modalDialog(
      plotOutput("myplot35")
    )
  }
  plotModal36 <- function() {
    modalDialog(
      plotOutput("myplot36")
    )
  }
  plotModal37 <- function() {
    modalDialog(
      plotOutput("myplot37")
    )
  }
  plotModal38 <- function() {
    modalDialog(
      plotOutput("myplot38")
    )
  }
  plotModal39 <- function() {
    modalDialog(
      plotOutput("myplot39")
    )
  }
  plotModal40 <- function() {
    modalDialog(
      plotOutput("myplot40")
    )
  }
  plotModal41 <- function() {
    modalDialog(
      plotOutput("myplot41")
    )
  }
  plotModal42 <- function() {
    modalDialog(
      plotOutput("myplot42")
    )
  }
  observeEvent(input$mytable4_cell_clicked, {
    info = input$mytable4_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable5_cell_clicked, {
    info = input$mytable5_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  # observeEvent(input$mytable6_cell_clicked, {
  #   info = input$mytable6_cell_clicked
  #   # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
  #   if (is.null(info$value) || info$col != 2) return()
  #   if(info$value == "Nukumea @ Upper "){
  #     removeModal()
  #     showModal(plotModal1())
  #   }else if(info$value == "Okura Creek "){
  #     removeModal()
  #     showModal(plotModal2())  
  #   }else if(info$value == "Vaughn Stream "){
  #     removeModal()
  #     showModal(plotModal3()) 
  #   }else if(info$value == "Waiwera Stream "){
  #     removeModal()
  #     showModal(plotModal4())
  #   }else if(info$value == "West Hoe Stream "){
  #     removeModal()
  #     showModal(plotModal5())
  #   }else if(info$value == "Cascades @ Whakanewh "){
  #     removeModal()
  #     showModal(plotModal6())
  #   }else if(info$value == "Onetangi @ Waiheke R "){
  #     removeModal()
  #     showModal(plotModal7())
  #   }else if(info$value == "Kaukapakapa @Taylors "){
  #     removeModal()
  #     showModal(plotModal8())
  #   }else if(info$value == "Kumeu River "){
  #     removeModal()
  #     showModal(plotModal9())
  #   }else if(info$value == "Makarau @ Railway "){
  #     removeModal()
  #     showModal(plotModal10())
  #   }else if(info$value == "Riverhead Stream "){
  #     removeModal()
  #     showModal(plotModal11())
  #   }else if(info$value == "Mahurangi River FHQ "){
  #     removeModal()
  #     showModal(plotModal12())
  #   }else if(info$value == "Mahurangi River WS "){
  #     removeModal()
  #     showModal(plotModal13())
  #   }else if(info$value == "Ngakaroa Stream "){
  #     removeModal()
  #     showModal(plotModal14())
  #   }else if(info$value == "Papakura @ Alfriston "){
  #     removeModal()
  #     showModal(plotModal15())
  #   }else if(info$value == "Papakura Stream "){
  #     removeModal()
  #     showModal(plotModal16())
  #   }else if(info$value == "Puhinui Stream "){
  #     removeModal()
  #     showModal(plotModal17())
  #   }else if(info$value == "Waitangi Falls Br. "){
  #     removeModal()
  #     showModal(plotModal18())
  #   }else if(info$value == "Whangamaire Woodhous "){
  #     removeModal()
  #     showModal(plotModal19())
  #   }else if(info$value == "Matakana River "){
  #     removeModal()
  #     showModal(plotModal20())
  #   }else if(info$value == "Omaru @ Maybury "){
  #     removeModal()
  #     showModal(plotModal21())
  #   }else if(info$value == "Otaki Creek "){
  #     removeModal()
  #     showModal(plotModal22())
  #   }else if(info$value == "Otara Ck East Tamaki "){
  #     removeModal()
  #     showModal(plotModal23())
  #   }else if(info$value == "Otara Ck Kennel Hill "){
  #     removeModal()
  #     showModal(plotModal24())
  #   }else if(info$value == "Pakuranga Ck Botany "){
  #     removeModal()
  #     showModal(plotModal25())
  #   }else if(info$value == "Pakuranga Ck Greenmt "){
  #     removeModal()
  #     showModal(plotModal26())
  #   }else if(info$value == "Wairoa @ Caitchons "){
  #     removeModal()
  #     showModal(plotModal27())
  #   }else if(info$value == "Wairoa River "){
  #     removeModal()
  #     showModal(plotModal28())
  #   }else if(info$value == "Avondale Stream @ Sh "){
  #     removeModal()
  #     showModal(plotModal29())
  #   }else if(info$value == "Lucas Creek "){
  #     removeModal()
  #     showModal(plotModal30())
  #   }else if(info$value == "Oakley Creek "){
  #     removeModal()
  #     showModal(plotModal31())
  #   }else if(info$value == "Opanuku Stream "){
  #     removeModal()
  #     showModal(plotModal32())
  #   }else if(info$value == "Oteha Stream "){
  #     removeModal()
  #     showModal(plotModal33())
  #   }else if(info$value == "Parrs Cross "){
  #     removeModal()
  #     showModal(plotModal34())
  #   }else if(info$value == "Rangitopuni River "){
  #     removeModal()
  #     showModal(plotModal35())
  #   }else if(info$value == "Woodside Res "){
  #     removeModal()
  #     showModal(plotModal36())
  #   }else if(info$value == "Cascade Stream "){
  #     removeModal()
  #     showModal(plotModal37())
  #   }
  # })
  observeEvent(input$mytable2_cell_clicked, {
    info = input$mytable2_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable3_cell_clicked, {
    info = input$mytable3_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable3_cell_clicked, {
    info = input$mytable3_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable5_cell_clicked, {
    info = input$mytable5_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable1_cell_clicked, {
    info = input$mytable1_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Nukumea @ Upper "){
      removeModal()
      showModal(plotModal1())
    }else if(info$value == "Okura Creek "){
      removeModal()
      showModal(plotModal2())  
    }else if(info$value == "Vaughn Stream "){
      removeModal()
      showModal(plotModal3()) 
    }else if(info$value == "Waiwera Stream "){
      removeModal()
      showModal(plotModal4())
    }else if(info$value == "West Hoe Stream "){
      removeModal()
      showModal(plotModal5())
    }else if(info$value == "Cascades @ Whakanewh "){
      removeModal()
      showModal(plotModal6())
    }else if(info$value == "Onetangi @ Waiheke R "){
      removeModal()
      showModal(plotModal7())
    }else if(info$value == "Kaukapakapa @Taylors "){
      removeModal()
      showModal(plotModal8())
    }else if(info$value == "Kumeu River "){
      removeModal()
      showModal(plotModal9())
    }else if(info$value == "Makarau @ Railway "){
      removeModal()
      showModal(plotModal10())
    }else if(info$value == "Riverhead Stream "){
      removeModal()
      showModal(plotModal11())
    }else if(info$value == "Mahurangi River FHQ "){
      removeModal()
      showModal(plotModal12())
    }else if(info$value == "Mahurangi River WS "){
      removeModal()
      showModal(plotModal13())
    }else if(info$value == "Ngakaroa Stream "){
      removeModal()
      showModal(plotModal14())
    }else if(info$value == "Papakura @ Alfriston "){
      removeModal()
      showModal(plotModal15())
    }else if(info$value == "Papakura Stream "){
      removeModal()
      showModal(plotModal16())
    }else if(info$value == "Puhinui Stream "){
      removeModal()
      showModal(plotModal17())
    }else if(info$value == "Waitangi Falls Br. "){
      removeModal()
      showModal(plotModal18())
    }else if(info$value == "Whangamaire Woodhous "){
      removeModal()
      showModal(plotModal19())
    }else if(info$value == "Matakana River "){
      removeModal()
      showModal(plotModal20())
    }else if(info$value == "Omaru @ Maybury "){
      removeModal()
      showModal(plotModal21())
    }else if(info$value == "Otaki Creek "){
      removeModal()
      showModal(plotModal22())
    }else if(info$value == "Otara Ck East Tamaki "){
      removeModal()
      showModal(plotModal23())
    }else if(info$value == "Otara Ck Kennel Hill "){
      removeModal()
      showModal(plotModal24())
    }else if(info$value == "Pakuranga Ck Botany "){
      removeModal()
      showModal(plotModal25())
    }else if(info$value == "Pakuranga Ck Greenmt "){
      removeModal()
      showModal(plotModal26())
    }else if(info$value == "Wairoa @ Caitchons "){
      removeModal()
      showModal(plotModal27())
    }else if(info$value == "Wairoa River "){
      removeModal()
      showModal(plotModal28())
    }else if(info$value == "Avondale Stream @ Sh "){
      removeModal()
      showModal(plotModal29())
    }else if(info$value == "Lucas Creek "){
      removeModal()
      showModal(plotModal30())
    }else if(info$value == "Oakley Creek "){
      removeModal()
      showModal(plotModal31())
    }else if(info$value == "Opanuku Stream "){
      removeModal()
      showModal(plotModal32())
    }else if(info$value == "Oteha Stream "){
      removeModal()
      showModal(plotModal33())
    }else if(info$value == "Parrs Cross "){
      removeModal()
      showModal(plotModal34())
    }else if(info$value == "Rangitopuni River "){
      removeModal()
      showModal(plotModal35())
    }else if(info$value == "Woodside Res "){
      removeModal()
      showModal(plotModal36())
    }else if(info$value == "Cascade Stream "){
      removeModal()
      showModal(plotModal37())
    }
  })
  observeEvent(input$mytable10_cell_clicked, {
    info = input$mytable10_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Pupuke"){
      removeModal()
      showModal(plotModal38())
    }else if(info$value == "Tomarata"){
      removeModal()
      showModal(plotModal39())  
    }else if(info$value == "Kuwakatai"){
      removeModal()
      showModal(plotModal40()) 
    }else if(info$value == "Ototoa"){
      removeModal()
      showModal(plotModal41())
    }else if(info$value == "Wainamu"){
      removeModal()
      showModal(plotModal42())
    }
  })
  observeEvent(input$mytable9_cell_clicked, {
    info = input$mytable9_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Pupuke"){
      removeModal()
      showModal(plotModal38())
    }else if(info$value == "Tomarata"){
      removeModal()
      showModal(plotModal39())  
    }else if(info$value == "Kuwakatai"){
      removeModal()
      showModal(plotModal40()) 
    }else if(info$value == "Ototoa"){
      removeModal()
      showModal(plotModal41())
    }else if(info$value == "Wainamu"){
      removeModal()
      showModal(plotModal42())
    }
  })
  observeEvent(input$mytable8_cell_clicked, {
    info = input$mytable8_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Pupuke"){
      removeModal()
      showModal(plotModal38())
    }else if(info$value == "Tomarata"){
      removeModal()
      showModal(plotModal39())  
    }else if(info$value == "Kuwakatai"){
      removeModal()
      showModal(plotModal40()) 
    }else if(info$value == "Ototoa"){
      removeModal()
      showModal(plotModal41())
    }else if(info$value == "Wainamu"){
      removeModal()
      showModal(plotModal42())
    }
  })
  observeEvent(input$mytable11_cell_clicked, {
    info = input$mytable11_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Pupuke"){
      removeModal()
      showModal(plotModal38())
    }else if(info$value == "Tomarata"){
      removeModal()
      showModal(plotModal39())  
    }else if(info$value == "Kuwakatai"){
      removeModal()
      showModal(plotModal40()) 
    }else if(info$value == "Ototoa"){
      removeModal()
      showModal(plotModal41())
    }else if(info$value == "Wainamu"){
      removeModal()
      showModal(plotModal42())
    }
  })
  observeEvent(input$mytable7_cell_clicked, {
    info = input$mytable7_cell_clicked
    # do nothing if not clicked yet, or the clicked cell is not in the 2nd column
    if (is.null(info$value) || info$col != 2) return()
    if(info$value == "Pupuke"){
      removeModal()
      showModal(plotModal38())
    }else if(info$value == "Tomarata"){
      removeModal()
      showModal(plotModal39())  
    }else if(info$value == "Kuwakatai"){
      removeModal()
      showModal(plotModal40()) 
    }else if(info$value == "Ototoa"){
      removeModal()
      showModal(plotModal41())
    }else if(info$value == "Wainamu"){
      removeModal()
      showModal(plotModal42())
    }
  })
}
shinyApp(ui, server)