#Installing Packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("moderndive")
install.packages("ggplot2")
install.packages("tibble")

#Importing Libraries
library("dplyr")
library("tidyverse")
library("moderndive")
library("ggplot2")
library("tibble")

#Adding Files
X2015 <- read_csv("Data/2015.csv")
X2016 <- read_csv("Data/2016.csv")
X2017 <- read_csv("Data/2017.csv")
X2018 <- read_csv("Data/2018.csv")
X2019 <- read_csv("Data/2019.csv")
X2020 <- read_csv("Data/WHR20_DataForFigure2.1.csv")
ImmData <- read_csv("Data/asylum-decisions.csv")

#Combining Happiness Charts
Regions <- X2015 %>%
  select(Country, Region)

Y2015 <- X2015 %>%
  select(Country, `Happiness Score`,`Economy (GDP per Capita)`,Family, `Health (Life Expectancy)`, Freedom, `Trust (Government Corruption)`, Generosity, `Dystopia Residual`)
colnames(Y2015) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2015$Year <- 2015

Y2016 <- X2016 %>%
  select(Country, `Happiness Score`,`Economy (GDP per Capita)`,Family, `Health (Life Expectancy)`, Freedom, `Trust (Government Corruption)`, Generosity, `Dystopia Residual`)
colnames(Y2016) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2016$Year <- 2016

Y2017 <- X2017 %>%
  select(Country, Happiness.Score, Economy..GDP.per.Capita., Family, Health..Life.Expectancy., Freedom, Trust..Government.Corruption., Generosity, Dystopia.Residual)
colnames(Y2017) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2017$Year <- 2017

X2018$`Perceptions of corruption`[X2018$`Perceptions of corruption` == 'N/A'] <- as.numeric(0)
X2018$Dystopia <- as.numeric(X2018$Score) - (as.numeric(X2018$`GDP per capita`) + as.numeric(X2018$`Social support`) + as.numeric(X2018$`Healthy life expectancy`) + as.numeric(X2018$`Freedom to make life choices`) + as.numeric(X2018$Generosity) + as.numeric(X2018$`Perceptions of corruption`))
Y2018 <- X2018 %>%
  select(`Country or region`, Score, `GDP per capita`, `Social support`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity, Dystopia)
colnames(Y2018) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2018$Year <- 2018

X2019$Dystopia <- as.numeric(X2018$Score) - (as.numeric(X2018$`GDP per capita`) + as.numeric(X2018$`Social support`) + as.numeric(X2018$`Healthy life expectancy`) + as.numeric(X2018$`Freedom to make life choices`) + as.numeric(X2018$Generosity) + as.numeric(X2018$`Perceptions of corruption`))
Y2019 <- X2019 %>%
  select(`Country or region`, Score, `GDP per capita`, `Social support`, `Healthy life expectancy`, `Freedom to make life choices`, `Perceptions of corruption`, Generosity, Dystopia)
colnames(Y2019) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2019$Year <- 2019

Y2020 <- X2020 %>%
  select(`Country name`, `Ladder score`,`Explained by: Log GDP per capita`, `Explained by: Social support`, `Explained by: Healthy life expectancy`, `Explained by: Freedom to make life choices`, `Explained by: Perceptions of corruption`, `Explained by: Generosity`, `Dystopia + residual`)
colnames(Y2020) <- c("Country", "Happiness Score", "Economy", "Social", "Health", "Freedom", "Trust", "Generosity", "Dystopia")
Y2020$Year <- 2020

ZHappiness <- rbind(Y2015,Y2016,Y2017,Y2018,Y2019,Y2020)

#Cleaning up Immigration Data
ImmData_to <- ImmData %>%
  group_by(`Country of asylum`,Year) %>%
  summarise(`Total Accepted Immigrants` = sum(`Recognized decisions`+`Complementary protection`),
            `Total Rejected Immigrants` = sum(`Rejected decisions` + `Otherwise closed`),
            `Total Immigration Attempts` = sum(`Total decisions`),
            `Total Emmigration Attempts` = 0)
colnames(ImmData_to) <- c("Country","Year","Total Accepted Immigrants","Total Rejected Immigrants","Total Immigration Attempts","Total Emmigration Attempts")
ImmData_away <- ImmData %>%
  group_by(`Country of origin`,Year) %>%
  summarise(`Total Accepted Immigrants` = 0,
            `Total Rejected Immigrants` = 0,
            `Total Immigration Attempts` = 0,
            `Total Emmigration Attempts` = sum(`Total decisions`))
colnames(ImmData_away) <- c("Country","Year","Total Accepted Immigrants","Total Rejected Immigrants","Total Immigration Attempts","Total Emmigration Attempts")
ImmData_both <- rbind(ImmData_to,ImmData_away)
ImmData_Clean <- ImmData_both %>%
  group_by(Country,Year) %>%
  summarise(`Total Accepted Immigrants` = sum(`Total Accepted Immigrants`),
            `Total Rejected Immigrants` = sum(`Total Rejected Immigrants`),
            `Total Immigration Attempts` = sum(`Total Immigration Attempts`),
            `Total Emmigration Attempts` = sum(`Total Emmigration Attempts`)) %>%
  filter(Year >= 2015 & Year <= 2020)

#Combining WHR Data with Immigration Data
`ZHappiness + Region` <- ZHappiness %>%
  inner_join(Regions, by = c("Country" = "Country"))

`Combined WHR + Imm Data + Region` <- ImmData_Clean %>% 
  inner_join(`ZHappiness + Region`, by = c("Country" = "Country", "Year" = "Year"))

#Removing Unnecessary Data Frames
rm(X2015,X2016,X2017,X2018,X2019,X2020)
rm(Y2015,Y2016,Y2017,Y2018,Y2019,Y2020)
rm(ImmData,ImmData_to,ImmData_away,ImmData_both)
