library(data.table)
library(ggplot2)
library(lubridate)
library(aod)
library(sqldf)
library(stringr)
library(bit64)
library(gdata)

install.packages("gdata")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#workplace injuries by industry and incident types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('./datasets/NUS/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$incident_agent)

DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 1, ifelse(DATA$degree_of_injury == 'Major', 2, 3))
DATA$degree_of_injury <- factor(DATA$degree_of_injury)

DATA$industry <- factor(DATA$industry)

DATA$incident_agent <- factor(DATA$incident_agent)

DATA$injury_count <- ifelse(DATA$no._of_injuries > 2, 1, 0)

DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury=degree_of_injury,
                   industry=industry,
                   incident_agent=incident_agent
                   )]
mylogit <- glm(injury_count ~ degree_of_injury + industry + incident_agent, data = DATA, family = "binomial")
summary(mylogit)
