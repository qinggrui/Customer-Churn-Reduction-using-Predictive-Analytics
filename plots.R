library(data.table)
library(ggplot2)
library(ggcorrplot)
library(class)
library(dplyr)
library(pdp)
library(vip)
library(reshape)
library(tidyquant)
library(zoo)
library(Metrics)
library(MLmetrics)
library(tidyverse)
library(corrr)

setwd("C:/Users/Qing Rui/Desktop/BC2407 Analytics II/Project/Project data")
data1 = fread("data_combined.csv",stringsAsFactors = T)
View(data1)
summary(data1)

getMode = function(x){
  test = table(x) 
  if(length(test) > 1){
    test = table(x) %>% sort(decreasing = T) %>% as.data.frame(stringsAsFactors = F) %>%
      mutate(x = as.numeric(x)) %>% filter(Freq == max(Freq))
    test$x
  } else {
    NULL
  }
}
modeAge = getMode(data1$Age)

churn1 = subset(data1, data1$Churn == '1')
churn0 = subset(data1, data1$Churn == '0')

ggplot(data1, aes(x= Age), show.legend = TRUE) +geom_density() +
  geom_density(data=churn1,aes(x=Age), color="darkgreen", linetype = "dashed", show.legend = TRUE)+
  geom_density(data=churn0,aes(x=Age), color="orange", linetype = "dashed", show.legend = TRUE)+
  geom_vline(aes(xintercept=mean(Age)),color="blue", linetype="solid", size=1, show.legend = TRUE) +
  geom_vline(aes(xintercept=median(Age)),color="red", linetype="solid", size=1, show.legend = TRUE)+ 
  geom_vline(data = as.data.frame(modeAge),aes(xintercept=modeAge, color = "Mode Overall Age"), show.legend = TRUE, size = 1, linetype="solid")+
  scale_color_manual(name="Legend", values = c(`Mean Overall Age`="blue", `Median Overall Age`="red", `Mode Overall Age` = "green", `Dist. for Overall Age`="black",
                                               `Dist. for Churn Yes` = "darkgreen", `Dist. for Churn No` = "orange"))+
  theme(legend.position = c(0.22,0.3)) +
  labs(title = "Density plot of Age in data01")


data2 = fread("data_encoded.csv",stringsAsFactors = T)
View(data2)
Only_variables = subset(data2,select= -c(City, V1, Latitude, Longitude, Under30, `Offer_Offer A`, `Offer_Offer B`,
                                              `Offer_Offer C`, `Offer_Offer D`,`Offer_Offer E`,Offer_None, ZipCode, TenureinMonths
                                              )) #Filter out columns such that only variables remain

model.matrix(~0+., data=Only_variables) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2) #Corrplot



ggplot(data1, aes(x= Age, y = CLTV), show.legend = TRUE) +
  scale_color_manual(name="Legend", values = c(`Mean Overall Age`="blue", `Median Overall Age`="red", `Mode Overall Age` = "green", `Dist. for Overall Age`="black",
                                               `Dist. for Churn Yes` = "darkgreen", `Dist. for Churn No` = "orange"))+
  theme(legend.position = c(0.22,0.3)) +
  labs(title = "Density plot of Age in data01")



ggplot(data=data1,aes(x= Age,y= CLTV)) + facet_wrap(~Churn)+
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "CLTV vs Age Smoothing")


ggplot(data1, aes(x=gender, y= tenure, color=gender), show.legend = TRUE) +geom_point() + facet_wrap(~Churn) + geom_jitter()

## ggplot for tenure vs CLTV
sp = ggplot(data1, aes(x=CLTV, y= tenure, color=Churn), show.legend = TRUE) +geom_point() + facet_wrap(~Churn) + geom_jitter()

sp + geom_density_2d()
# Gradient color
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")
# Change the gradient color
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red") +  labs(title = "Density plot of Tenure (Months) vs CLTV")

sp+ geom_bin2d(bins=10)


###ggplot for churn vs customer service request
sp = ggplot(data1, aes(x=TotalCustomerSvcRequests, y= tenure, color=Churn)) +geom_point() + facet_wrap(~Churn) +
  geom_jitter() +  labs(title = "Bin Counts of Churn vs Service Requests and Tenure")

sp+ geom_bin2d(bins=10)



## ggplot for CLTV and distance charge
coeff <- 10
ggplot(data1, aes(x=CLTV, y= MonthlyCharges, color=Churn)) + 
  geom_smooth(method = "loess", se = TRUE)+ facet_wrap(~Churn)

ggplot(data1, aes(x=CLTV) +
  
  geom_line( aes(y= MonthlyCharges)) + 
  geom_line( aes(y=TotalCharges/coeff)) + # Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "First Axis",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Second Axis")
  ))

library(patchwork) # To display 2 charts together
library(hrbrthemes)

ggplot(data1, aes(x=CLTV)) +  
  
  geom_line( data = data1, aes(y=MonthlyCharges),stat ='identity', size=1,fun = mean) + 
  geom_line( data= data1, aes(y=TotalCharges / coeff), size=1, fun = mean) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Temperature (Celsius °)",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Price ($)")
  ) + 
  
  theme_ipsum() +
  
  theme(
    axis.title.y = element_text( size=13),
    axis.title.y.right = element_text(size=13)
  )


ggplot(data1, aes(x=CLTV, y= MonthlyCharges, color=Churn)) + 
  geom_smooth(method = "loess", se = TRUE)+ facet_wrap(~Churn)+ 
  labs(title = "Higher Monthly Charges for Churned Customers ")

ggplot(data1, aes(x=CLTV, y= TotalCharges, color=Churn)) + 
  geom_smooth(method = "loess", se = TRUE)+ facet_wrap(~Churn)+ 
  labs(title = "Lower Total Charges for Churned customers ")


