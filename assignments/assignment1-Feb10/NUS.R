
library(data.table)
library(ggplot2)
library(lubridate)
library(aod)
library(sqldf)
library(stringr)
library(bit64)
library(gdata)

#install.packages("gdata")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#workplace injuries by industry and incident types
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('D:/github/NUS_EBAC/assignments/assignment1-Feb10/data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$incident_agent)
unique(DATA$no._of_injuries)
unique(DATA$sub_industry)
unique(DATA$incident_agent_sub_type)
unique(DATA$incident_agent)
unique(DATA$incident_type)
#DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 1, ifelse(DATA$degree_of_injury == 'Major', 2, 3))
#DATA$degree_of_injury <- factor(DATA$degree_of_injury)

DATA$industry <- factor(DATA$industry)

DATA$incident_agent <- factor(DATA$incident_agent)

df <- rbind(data.frame(x=DATA$no._of_injuries, y=DATA$degree_of_injury, cor="IC-DI"),
            data.frame(x=DATA$no._of_injuries, y=DATA$industry, cor="IC-IND"),
           data.frame(x=DATA$no._of_injuries, y=DATA$incident_agent, cor="IC-IA"))

ggplot(df, aes(x,y)) + geom_point() + facet_wrap(~cor, scales = "free")

DATA$injury_count <- ifelse(DATA$no._of_injuries > 2, 1, 0)

df1 <- rbind(data.frame(x=DATA$injury_count, y=DATA$degree_of_injury, cor="IC-DI"),
            data.frame(x=DATA$injury_count, y=DATA$industry, cor="IC-IND"),
            data.frame(x=DATA$injury_count, y=DATA$incident_agent, cor="IC-IA"))

ggplot(df1, aes(x,y)) + geom_abline() + facet_wrap(~cor, scales = "free")

DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury=degree_of_injury,
                   industry=industry,
                   incident_agent=incident_agent
                   )]
mylogit <- glm(injury_count ~ degree_of_injury + industry + incident_agent, data = DATA, family = "binomial")
summary(mylogit)
