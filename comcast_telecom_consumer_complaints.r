#loading required libraries

library(stringi)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)

setwd("/Users/apeksha/Downloads/")
getwd()

# Import data into R.

comcast_df <- read.csv("Comcast Telecom Complaints data.csv",header = TRUE,stringsAsFactors = F)

#view data sets

head(comcast_df)
names(comcast_df)

# Data set observational study

na_vector <- is.na(comcast_df)
length(na_vector[na_vector==T])

#Task : Provide the trend chart for the number of complaints at monthly and 
#daily granularity levels.

comcast_df$Date<- dmy(comcast_df$Date)

m_count<- summarise(group_by(comcast_df,Month =as.integer(month(Date))),Count = n())
d_count<- summarise(group_by(comcast_df,Date),Count =n())
m_count<-arrange(m_count,Month)





ggplot(data = m_count,aes(Month,Count,label = Count))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  scale_x_continuous(breaks = m_count$Month)+
  labs(title = "Trend chart for the number of complaints at monthly levels",x= "Months",y ="Number of complaints")+
  theme(plot.title = element_text(hjust = 0.5))


ggplot(data = d_count,aes(as.POSIXct(Date),Count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "1 weeks",date_labels = "%d/%m")+
  labs(title = "Trend chart for the number of complaints at daily levels",x= "Days",y ="Number of complaints")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))



#Task : Provide a table with the frequency of complaint types.



ctype_network=grep('network',comcast_df$Customer.Complaint,ignore.case = T)
ctype_internet=grep('internet',comcast_df$Customer.Complaint,ignore.case = T)
ctype_billing=grep('bill',comcast_df$Customer.Complaint,ignore.case = T)
ctype_email=grep('email',comcast_df$Customer.Complaint,ignore.case = T)
ctype_charges=grep('charge',comcast_df$Customer.Complaint,ignore.case = T)


comcast_df$ComplaintType[ctype_internet]<- "Internet"
comcast_df$ComplaintType[ctype_network]<- "Network"
comcast_df$ComplaintType[ctype_billing]<- "Billing"
comcast_df$ComplaintType[ctype_email]<- "Email"
comcast_df$ComplaintType[ctype_charges]<- "Charges"

comcast_df$ComplaintType[-c(ctype_internet,ctype_network,
                            ctype_billing,ctype_charges,ctype_email)]<- "Others"

table(comcast_df$ComplaintType)

#TASK: Create a new categorical variable with value as Open and Closed. 
#Open & Pending is to be categorized as Open and Closed & Solved 
#is to be categorized as Closed

open_complaints<- (comcast_df$Status == "Open"| comcast_df$Status =="Pending")
closed_complaints<-(comcast_df$Status == "Closed"| comcast_df$Status =="Solved")
comcast_df$ComplaintStatus[ open_complaints]<-"Open" 
comcast_df$ComplaintStatus[closed_complaints]<- "Closed" 


#TASK :Provide state wise status of complaints in a stacked bar chart. 
#Use the categorized variable from Q3. Provide insights on:
#TASK : Which state has the maximum complaints

#to overcome the casesensitivity issue

comcast_df$State=str_to_title(comcast_df$State)



comcast_df<- group_by(comcast_df,State,ComplaintStatus)
chart_data<- summarise(comcast_df,Count = n())
ggplot(as.data.frame(chart_data) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.95)+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        title = element_text(size = 16,colour = "#000000"),
        plot.title = element_text(hjust =  0.5))+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets",
       fill= "Status")




#TASK  Which state has the highest percentage of unresolved complaints

chart_data%>%
  filter(ComplaintStatus == "Open")->
  open_complaints
open_complaints[open_complaints$Count == max(open_complaints$Count),c(1,3)]

# TASK :Provide the percentage of complaints resolved till date,
# which were received through the Internet and customer care calls.
options(dplyr.summarise.inform = FALSE)
resolved_data <- group_by(comcast_df,ComplaintStatus)
total_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)*100)) 
total_resloved

resolved_data <- group_by(comcast_df,Received.Via,ComplaintStatus)
Category_resloved<- summarise(resolved_data ,percentage =(n()/nrow(resolved_data)*100)) 
Category_resloved


#End 


