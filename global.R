library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(readxl)
library(plotly)
library(DT)
library(rio)
library(magrittr)
library(tidyr)


Apr <- import("Data_Apr.xlsx")
May <- import("Data_May.xlsx")
Jun <- import("Data_June.xlsx")
Jul <- import("Data_July.xlsx")
Aug <- import("Data_aug.xlsx")
Sep <- import("Data_Sep.xlsx")
Oct <- import("Oct18.xlsx")
Nov <- import("Nov18.xlsx")
Dec <- import("Dec18.xlsx")
Jan <- import("Data_Jan.xlsx")
Feb <- import("Data_Feb.xlsx")
Mar <- import("Data_Mar19.xlsx")

Apr$Month <- "Apr"
May$Month <- "May"
Jun$Month <- "Jun"
Jul$Month <- "Jul"
Aug$Month <- "Aug"
Sep$Month <- "Sep"
Oct$Month <- "Oct"
Nov$Month <- "Nov"
Dec$Month <- "Dec"
Jan$Month <- "Jan"
Feb$Month <- "Feb"
Mar$Month <- "Mar"

Apr$Year <- 2018
May$Year <- 2018
Jun$Year <- 2018
Jul$Year <- 2018
Aug$Year <- 2018
Sep$Year <- 2018
Oct$Year <- 2018
Nov$Year <- 2018
Dec$Year <- 2018
Jan$Year <- 2019
Feb$Year <- 2019
Mar$Year <- 2019

colnames(Apr) <- colnames(May)
colnames(Jan) <- colnames(Mar)

Master <- rbind(Mar,Feb,Jan,Dec,Nov,Oct,Sep,Aug,Jul,Jun,May,Apr)

Master$`Total Stock`<- as.numeric(Master$`Total Stock`)
Master$`Total Value` <-as.numeric(Master$`Total Value`)

Master <- Master[-which(is.na(Master$`Total Value`)|is.na(Master$`Total Stock`)),]

list1_Pune <- Master %>% group_by(Month) %>% summarise(Total_Parts = n(),Total_Stock = round(sum(`Total Stock`,na.rm =T)/10^6,2),Total_Value = round(sum(`Total Value`,na.rm =T)/10^7,2))
list2_Pune <- Master %>% group_by(Month,`Val. Class`) %>% summarise(Total_Parts = n(),Total_Stock = round(sum(`Total Stock`,na.rm =T)/10^7,2),Total_Value = round(sum(`Total Value`,na.rm =T)/10^7,2))

list2_Pune <- list2_Pune %>% subset(`Val. Class` == 110 |`Val. Class` == 120 |`Val. Class` == 230 |`Val. Class` ==240 |`Val. Class` ==250 |`Val. Class` ==530|`Val. Class` ==550)


Masterlist1 <- Master %>% group_by(Month,`Val. Class`) %>% summarise(Total_Parts = n(),Total_Stock = round(sum(`Total Stock`,na.rm =T)/10^7,2),
                                                                     Total_Value = round(sum(`Total Value`,na.rm =T)/10^7,2),
                                                                     Days_30 = round(sum(as.numeric(`Value(Rs) in Period( 0 - 30 )`,na.rm = T)/10^7,2)),
                                                                     Days30_60 = round(sum(as.numeric(`Value(Rs) in Period( 31 - 60 )`,na.rm = T)/10^7,2)),
                                                                     Days60_90 = round(sum(as.numeric(`Value(Rs) in Period( 61 - 90 )`,na.rm = T)/10^7,2)),
                                                                     Days90_180 = round(sum(as.numeric(`Value(Rs) in Period( 91 - 180 )`,na.rm = T)/10^7,2)),        
                                                                     Days180_365 = round(sum(as.numeric(`Value(Rs) in Period( 181 - 365 )`,na.rm = T)/10^7,2)),
                                                                     Year1_2 = round(sum(as.numeric(`Value(Rs) in Period( 1 - 2 Yr )`,na.rm = T)/10^7,2)),
                                                                     Year_2 = round(sum(as.numeric(`Value(Rs) in Period( > 2 Yrs )`,na.rm = T)/10^7,2)))
colnames(Masterlist1)[2] <- "Val.Class"
Masterlist1 <- Masterlist1%>% subset(Val.Class == 110 |Val.Class == 120 |Val.Class == 230 |Val.Class ==240 |Val.Class ==250 |Val.Class ==530|Val.Class ==550)
Masterlist1 <- data.frame(Masterlist1)

Masterlist2 <- Masterlist1%>% group_by(Month) %>% summarise(Total_Value = sum(Total_Value),Days30 =sum(Days_30),Days30_60 =sum(Days30_60),Days60_90 = sum(Days60_90),
                                                            Days90_180 =sum(Days90_180),Days180_365 =sum(Days180_365),Year1_2 =sum(Year1_2),Year2More=sum(Year_2))
Masterlist2 <- data.frame(Masterlist2)

################################################################################################
#Inv_Value <- data.frame(Inventory.Age = c("Days<30","30<Days<60","60<Days<90","90<Days<180","180<Days<365","1<Year<2","2<Year"),
# Inventory.Value = c(val2,val3,val4,val5,val6,val7,val8))

#Inv_Value$Inventory.Age <- as.factor(Inv_Value$Inventory.Age)
#Inv_Value$Inventory.Value <- as.numeric(Inv_Value$Inventory.Value )

######################################

  

###############################################
g <- Master %>% subset(`Val. Class` == 110 |`Val. Class` == 120 |`Val. Class` == 230 |`Val. Class` ==240 |`Val. Class` ==250 |`Val. Class` ==530|`Val. Class` ==550) %>% group_by(`Val. Class`,Month) %>% summarise(Total_Value =round(sum(`Total Value`)/10000000,digits = 2), Total_Stock = sum(`Total Stock`)/1000)
colnames(g)[1] <- "Value_Class"

##################################
Inv_Class <- Master %>% filter(Master$`Qty ( > 2 Yrs )` > 0  &  Master$`Qty in Days( 0 - 30 )` > 0 ) %>% select(3:5,7,9:11,13:15,17,29:30) %>% arrange(-`Total Value`)

#######################
Top10TV <- Mar  %>% filter(Mar$`Val. Class` != 530 & Mar$`Val. Class` !=550)

###########################Data Accuracy #############################33
Master_Rate <- Mar[which((Master$Rate == 0) & (Master$`Total Value`>0)),] #There are 12 records in Mar month
Master_Rate <- Master_Rate %>% select(1:11,-2,-6,30)


Master$mis_Value <- (as.numeric(Master$`Total Stock`)* as.numeric(Master$Rate)) - as.numeric((Master$`Total Value`))
Miss_value <- Master[which(Master$mis_Value>1 | Master$mis_Value < -1),]
Miss_value <- Miss_value %>% select(1:15,30)

##########################
