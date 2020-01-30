library(foreign)
library(haven)
library(dplyr)
library(Hmisc)
library(reshape)
library(plm)
library(ggplot2)
library(tidyr)
library(lmtest)
library(sandwich)
library(multilevel)
library(stargazer)
setwd("C:/")
# данные с Population и Unenmployment
data <- read.csv("World_Development_Indicators.csv") 
# данные с Voice и Corruption
data1 <- read.csv("Worldwide_Governance_Indicators.csv")
# данные с Trust
data2 <- read.csv("Trust.csv")
class(data2$Indicator)
class(data2$Subindicator.Type)
data2$Indicator <- as.character(data2$Indicator)
data2$Subindicator.Type <- as.character(data2$Subindicator.Type)
data2 <- data2[which(data2$Indicator == "Public trust in politicians" & data2$Subindicator.Type == "1-7 Best"),] 
class(data2$Country.Name)
data2$Country.Name <- as.character(data2$Country.Name)
data2 <- subset(data2, Country.Name == "Austria" | Country.Name == "Belgium" | 
                  Country.Name == "Czech Republic" |
                  Country.Name == "Estonia" | Country.Name == "Finland" |  
                  Country.Name == "Greece" | Country.Name == "Iceland" | 
                  Country.Name == "Italy" | Country.Name == "Latvia" |
                  Country.Name == "Lithuania" | 
                  Country.Name == "Montenegro" | Country.Name == "Netherlands" | Country.Name == "Norway" |
                  Country.Name == "Poland" | Country.Name == "Portugal" | 
                  Country.Name == "Slovenia" | Country.Name == "Spain" |
                  Country.Name == "Sweden" |  Country.Name == "United Kingdom" ,
                select=c(Country.Name, Indicator, X2010.2011, X2012.2013, X2014.2015, X2016.2017)) 
data$Country.Code <- data$Series.Code <- NULL
data1$Country.Code <- data1$Series.Code <- NULL
class(data$п.їCountry.Name)
class(data$Series.Name)
class(data1$п.їCountry.Name)
class(data1$Series.Name)
data$Series.Name <- as.character(data$Series.Name)
data$п.їCountry.Name <- as.character(data$п.їCountry.Name)
data$Series.Name <- as.character(data$Series.Name)
data$п.їCountry.Name <- as.character(data$п.їCountry.Name)
data <- rename(data,
               c("п.їCountry.Name" = "Country" ,
                 "Series.Name" = "Indicator"))
data1 <- rename(data1,
               c("п.їCountry.Name" = "Country" ,
                 "Series.Name" = "Indicator"))
data2 <- rename(data2,
                c("Country.Name" = "Country" ,
                  "X2010.2011" = "2010",
                  "X2012.2013" = "2012",
                  "X2014.2015" = "2014",
                  "X2016.2017" = "2016"))
# выбираю нужные мне страны
data <- subset(data,  Country == "Austria" | Country == "Belgium" | 
                  Country ==  "Croatia" | Country == "Czech Republic" |
                  Country == "Estonia" | Country == "Finland" | 
                  Country == "Greece" | Country == "Iceland" | 
                  Country == "Italy" | Country == "Latvia" |
                  Country == "Lithuania"  |
                  Country == "Montenegro" | Country == "Netherlands" | Country == "Norway" |
                  Country == "Poland" | Country == "Portugal" | 
                  Country == "Slovenia" | Country == "Spain" |
                  Country == "Sweden" |  Country == "United Kingdom" ,
                select=c(Country, Indicator, X2010..YR2010., X2012..YR2012., X2014..YR2014., X2016..YR2016.))
data1 <- subset(data1,  Country == "Austria" | Country == "Belgium" | 
                 Country ==  "Croatia" | Country == "Czech Republic" |
                 Country == "Estonia" | Country == "Finland" | 
                 Country == "Greece" | Country == "Iceland" | 
                 Country == "Italy" | Country == "Latvia" |
                 Country == "Lithuania"  |
                 Country == "Montenegro" | Country == "Netherlands" | Country == "Norway" |
                 Country == "Poland" | Country == "Portugal" | 
                 Country == "Slovenia" | Country == "Spain" |
                 Country == "Sweden" |  Country == "United Kingdom" ,
               select=c(Country, Indicator, X2010..YR2010., X2012..YR2012., X2014..YR2014., X2016..YR2016.))
data3 <- subset(data1, Indicator == "Control of Corruption: Estimate", select = c(Country, Indicator, X2010..YR2010., X2012..YR2012., X2014..YR2014., X2016..YR2016.))
data1 <- data1 %>% dplyr::filter(row_number() %% 2 != 1)
data4 <- subset(data, Indicator == "Unemployment, total (% of total labor force) (national estimate)", select = c(Country, Indicator, X2010..YR2010., X2012..YR2012., X2014..YR2014., X2016..YR2016.))
data <- data %>% dplyr::filter(row_number() %% 2 != 1)
data <- rename(data,
               c("X2010..YR2010." = "2010",
                 "X2012..YR2012." = "2012",
                 "X2014..YR2014." = "2014",
                 "X2016..YR2016." = "2016"))
data1 <- rename(data1,
               c("X2010..YR2010." = "2010",
                 "X2012..YR2012." = "2012",
                 "X2014..YR2014." = "2014",
                 "X2016..YR2016." = "2016"))
data3 <- rename(data3,
                c("X2010..YR2010." = "2010",
                  "X2012..YR2012." = "2012",
                  "X2014..YR2014." = "2014",
                  "X2016..YR2016." = "2016"))
data4 <- rename(data4,
                c("X2010..YR2010." = "2010",
                  "X2012..YR2012." = "2012",
                  "X2014..YR2014." = "2014",
                  "X2016..YR2016." = "2016"))
newdata <- gather(data, year, Indicator, "2010":"2016")
newdata1 <- gather(data1, year, Indicator, "2010":"2016")
newdata2 <- gather(data2, year, Indicator, "2010":"2016")
newdata3 <- gather(data3, year, Indicator, "2010":"2016")
newdata4 <- gather(data4, year, Indicator, "2010":"2016")
newdata<- newdata[order(newdata$Country),]
newdata1<- newdata1[order(newdata1$Country),]
newdata2 <- newdata2[order(newdata2$Country),]
newdata3 <- newdata3[order(newdata3$Country),]
newdata4 <- newdata4[order(newdata4$Country),]
newdata <- rename(newdata, c("Indicator" = "Population"))
newdata1 <- rename(newdata1, c("Indicator" = "Voice"))
newdata2 <- rename(newdata2, c("Indicator" = "Trust"))
newdata3 <- rename(newdata3, c("Indicator" = "Corruption"))
newdata4 <- rename(newdata4, c("Indicator" = "Unemployment"))
total_data <- cbind(newdata, newdata1, newdata2, newdata3, newdata4)
total_data <- total_data[,c(1,2,3,6,9,12,15)]
class(total_data$Country)
class(total_data$year)
total_data$year <- as.numeric(total_data$year)
total_data$year[total_data$year == 2010] <- 1
total_data$year[total_data$year == 2012] <- 2
total_data$year[total_data$year == 2014] <- 3
total_data$year[total_data$year == 2016] <- 4
class(total_data$Voice)
class(total_data$Trust)
class(total_data$Corruption)
class(total_data$Population)
class(total_data$Unemployment)
total_data <- rename(total_data, c("year" = "Period"))
stargazer(total_data, type = "text", title="Descriptive statistics", digits=1, out="table1.txt")
# fe на пространственные единицы
LSDV <- lm(Trust~Voice+Population+Unemployment+as.factor(Country), data = total_data)
stargazer(LSDV, type = "text",
          dep.var.labels = "Trust", covariate.labels = "Voice", out = "LSDV.txt")
femodel <- plm(Trust~Voice+Population+Unemployment, data = total_data, index=c("Country", "Period"), effect = "individual", model="within")
stargazer(femodel, type = "text",
          dep.var.labels = "Trust", covariate.labels = "Voice", out = "femodel.txt")
LSDV_time <- lm(Trust~Voice+Population+Unemployment+as.factor(Period), data = total_data)
stargazer(LSDV_time, type = "text",
          dep.var.labels = "Trust", covariate.labels = "Voice", out = "LSDV_time.txt")
fe_time_model <- plm(Trust~Voice+Population+Unemployment, data = total_data, index = c("Country", "Period"), effect = "time", model = "within")
stargazer(fe_time_model, type = "text",
          dep.var.labels = "Trust", covariate.labels = "Voice", out = "fe_time_model.txt")
summary(fixef(fe_time_model))
# fe-twoways
fe_twoways <- plm(Trust ~ Voice+Population+Unemployment, data = total_data, index=c("Country", "Period"), effect = "twoways", model = "within")
stargazer(fe_twoways, type = "text",
          dep.var.labels = "Trust", covariate.labels = "Voice", out = "fe_twoways.txt")
# проверка на устойчивость femodel
LSDV <- lm(Trust~Voice+Population+Unemployment+as.factor(Country), data = total_data)
ypredicted <- LSDV$fitted
total_data1 <- data.frame(total_data, ypredicted)
merged <- total_data1 %>% group_by(Country)%>% summarise(., cor(Trust, ypredicted))%>% merge(total_data1, ., by="Country")
merged$new <- ifelse(abs(merged$`cor(Trust, ypredicted)`)<0.3,1,0)
fe_2 <- plm(Trust ~ Voice+Population+Unemployment, merged[merged$new == 0,], index=c("Country", "Period"), effect = "individual", model = "within")
coeftest(fe_2, vcov = vcovHC, type = "HC3")
# проверка на устойчивость fe-twoways
LSDV_twoways <- lm(Trust~Voice+Population+Unemployment+Country+as.factor(Period), data = total_data)
ypredicted1 <- LSDV_twoways$fitted
total_data2 <- data.frame(total_data, ypredicted1)
merged <- total_data2 %>% group_by(Country)%>% summarise(., cor(Trust, ypredicted1))%>% merge(total_data2, ., by="Country")
merged$new <- ifelse(abs(merged$`cor(Trust, ypredicted1)`)<0.3,1,0)
fe_twoways2 <- plm(Trust ~ Voice+Population+Unemployment, merged[merged$new == 0,], index=c("Country", "Period"), effect = "twoways")
coeftest(fe_twoways2, vcov = vcovHC, type = "HC3")
# ols
olsmodel <- lm(Trust~Voice+Population+Unemployment, data = total_data)
stargazer(olsmodel, type = "text",
                    dep.var.labels = "Trust", covariate.labels = "Voice", out = "olsmodel.txt")
pFtest(femodel, olsmodel)
pFtest(fe_twoways, olsmodel)

