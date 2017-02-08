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
DATA <- fread('./data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$incident_type)
threshold <- 0.5
DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 1, 0)
DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 3, ifelse(DATA$degree_of_injury == 'Major', 2, 1))
DATA$degree_of_injury <- factor(DATA$degree_of_injury)

DATA$industry <- factor(DATA$industry)

DATA$incident_agent <- factor(DATA$incident_agent)

DATA$incident_type <- factor(DATA$incident_type)

DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury=degree_of_injury,
                   industry=industry,
                   incident_type=incident_type,
                   incident_agent=incident_agent
                   )]

mylogit <- glm(injury_count ~ ., data = DATA, family = "binomial")

summary(mylogit)
prob <- predict(mylogit, type = 'response')

predict <- ifelse(prob>threshold, 1, 0)

table(DATA$injury_count, predict)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('./data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$degree_of_injury)

DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 1, 0)
DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 3, ifelse(DATA$degree_of_injury == 'Major', 2, 1))

DATA$degree_of_injury1 <- ifelse(DATA$degree_of_injury == 1, 1, 0)
DATA$degree_of_injury2 <- ifelse(DATA$degree_of_injury == 2, 1, 0)
DATA$degree_of_injury3 <- ifelse(DATA$degree_of_injury == 3, 1, 0)
DATA$Marine <- ifelse(DATA$industry == 'Marine', 1, 0)
DATA$Mining_Quarrying <- ifelse(DATA$industry == 'Mining & Quarrying', 1, 0)
DATA$Administrative_Support_Services <- ifelse(DATA$industry == 'Administrative & Support Services', 1, 0)
DATA$Agriculture_Fishing <- ifelse(DATA$industry == 'Agriculture & Fishing', 1, 0)
DATA$Community_Social_Services <- ifelse(DATA$industry == 'Community, Social & Personal Services', 1, 0)
DATA$Construction <- ifelse(DATA$industry == 'Construction', 1, 0)
DATA$Electricity_Gas_AC_Supply <- ifelse(DATA$industry == 'Electricity, Gas and Air-Conditioning Supply', 1, 0)
DATA$Financial_Insurance_Services <- ifelse(DATA$industry == 'Financial & Insurance Services', 1, 0)
DATA$Manufacturing <- ifelse(DATA$industry == 'Manufacturing', 1, 0)
DATA$Industry_Others <- ifelse(DATA$industry == 'Others', 1, 0)
DATA$Scientific_Technical_Activities <- ifelse(DATA$industry == 'Professional, Scientific & Technical Activities', 1, 0)
DATA$Transportation_Storage <- ifelse(DATA$industry == 'Transportation & Storage', 1, 0)
DATA$Water_Supply_Management <- ifelse(DATA$industry == 'Water Supply, Sewerage & Waste Management', 1, 0)
DATA$Industrial_Machines <- ifelse(DATA$incident_agent == 'Industrial Machines', 1, 0)
DATA$Lifting_Equipment <- ifelse(DATA$incident_agent == 'Lifting Equipment Including Cranes', 1, 0)
DATA$Pressurised_Equipments <- ifelse(DATA$incident_agent == 'Pressurised Equipments', 1, 0)
DATA$Crane_Related <- ifelse(DATA$incident_type == 'Crane-related', 1, 0)
DATA$Stabbed_Objects <- ifelse(DATA$incident_type == 'Cut/Stabbed by Objects', 1, 0)
DATA$Extreme_Temp <- ifelse(DATA$incident_type == 'Exposure to Extreme Temperatures', 1, 0)
DATA$Hazardous_Substance <- ifelse(DATA$incident_type == 'Exposure to Hazardous Substances', 1, 0)
DATA$Falls_Trips <- ifelse(DATA$incident_type == 'Falls - Slips, Trips & Falls', 1, 0)
DATA$Fire_Explosion <- ifelse(DATA$incident_type == 'Fires & Explosion', 1, 0)
DATA$Incident_Type_Others <- ifelse(DATA$incident_type == 'Others', 1, 0)
DATA$Strenuous_Movements <- ifelse(DATA$incident_type == 'Over-exertion/Strenuous Movements', 1, 0)
DATA$Stepping_Objects <- ifelse(DATA$incident_type == 'Stepping on Objects', 1, 0)
DATA$Striking_Against <- ifelse(DATA$incident_type == 'Striking against Objects', 1, 0)
DATA$Work_Traffic <- ifelse(DATA$incident_type == 'Work-related Traffic', 1, 0)

DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury1=degree_of_injury1,
                   degree_of_injury2=degree_of_injury2,
                   Marine=Marine,
                   Mining_Quarrying=Mining_Quarrying,
                   Administrative_Support_Services=Administrative_Support_Services,
                   Community_Social_Services=Community_Social_Services,
                   Construction=Construction,
                   Industry_Others=Industry_Others,
                   Financial_Insurance_Services=Financial_Insurance_Services,
                   Scientific_Technical_Activities=Scientific_Technical_Activities,
                   Water_Supply_Management=Water_Supply_Management,
                   Industrial_Machines=Industrial_Machines,
                   Lifting_Equipment=Lifting_Equipment,
                   Pressurised_Equipments=Pressurised_Equipments,
                   Crane_Related=Crane_Related,
                   Stabbed_Objects=Stabbed_Objects,
                   Extreme_Temp=Extreme_Temp,
                   Hazardous_Substance=Hazardous_Substance,
                   Falls_Trips=Falls_Trips,
                   Fire_Explosion=Fire_Explosion,
                   Incident_Type_Others=Incident_Type_Others,
                   Strenuous_Movements=Strenuous_Movements,
                   Stepping_Objects=Stepping_Objects,
                   Striking_Against=Striking_Against,
                   Work_Traffic=Work_Traffic)]
write.csv(DATA, './data/OUTPUT/workplace-injuries-by-industry-and-incident-types.csv', row.names = FALSE)

mylogit <- glm(injury_count ~ ., data = DATA, family = "binomial")
summary(mylogit)

DATA1 <- DATA
DATA1$rankP <- predict(mylogit, newdata = DATA1, type = "response")
write.csv(DATA1, './datasets/NUS/OUTPUT/RESULT.csv', row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('./data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$degree_of_injury)

DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 'Multiple', 'Single')
DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 1, 0)
DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 3, ifelse(DATA$degree_of_injury == 'Major', 2, 1))

DATA$degree_of_injury1 <- ifelse(DATA$degree_of_injury == 1, 1, 0)
DATA$degree_of_injury2 <- ifelse(DATA$degree_of_injury == 2, 1, 0)
DATA$degree_of_injury3 <- ifelse(DATA$degree_of_injury == 3, 1, 0)
DATA$Administrative_Support_Services <- ifelse(DATA$industry == 'Administrative & Support Services', 1, 0)
DATA$Agriculture_Fishing <- ifelse(DATA$industry == 'Agriculture & Fishing', 1, 0)
DATA$Community_Social_Services <- ifelse(DATA$industry == 'Community, Social & Personal Services', 1, 0)
DATA$Construction <- ifelse(DATA$industry == 'Construction', 1, 0)
DATA$Electricity_Gas_AC_Supply <- ifelse(DATA$industry == 'Electricity, Gas and Air-Conditioning Supply', 1, 0)
DATA$Financial_Insurance_Services <- ifelse(DATA$industry == 'Financial & Insurance Services', 1, 0)
DATA$Manufacturing <- ifelse(DATA$industry == 'Manufacturing', 1, 0)
DATA$Industry_Others <- ifelse(DATA$industry == 'Others', 1, 0)
DATA$Scientific_Technical_Activities <- ifelse(DATA$industry == 'Professional, Scientific & Technical Activities', 1, 0)
DATA$Transportation_Storage <- ifelse(DATA$industry == 'Transportation & Storage', 1, 0)
DATA$Water_Supply_Management <- ifelse(DATA$industry == 'Water Supply, Sewerage & Waste Management', 1, 0)
DATA$Industrial_Machines <- ifelse(DATA$incident_agent == 'Industrial Machines', 1, 0)
DATA$Lifting_Equipment <- ifelse(DATA$incident_agent == 'Lifting Equipment Including Cranes', 1, 0)
DATA$Pressurised_Equipments <- ifelse(DATA$incident_agent == 'Pressurised Equipments', 1, 0)
DATA$Crane_Related <- ifelse(DATA$incident_type == 'Crane-related', 1, 0)
DATA$Stabbed_Objects <- ifelse(DATA$incident_type == 'Cut/Stabbed by Objects', 1, 0)
DATA$Extreme_Temp <- ifelse(DATA$incident_type == 'Exposure to Extreme Temperatures', 1, 0)
DATA$Hazardous_Substance <- ifelse(DATA$incident_type == 'Exposure to Hazardous Substances', 1, 0)
DATA$Falls_Trips <- ifelse(DATA$incident_type == 'Falls - Slips, Trips & Falls', 1, 0)
DATA$Fire_Explosion <- ifelse(DATA$incident_type == 'Fires & Explosion', 1, 0)
DATA$Incident_Type_Others <- ifelse(DATA$incident_type == 'Others', 1, 0)
DATA$Strenuous_Movements <- ifelse(DATA$incident_type == 'Over-exertion/Strenuous Movements', 1, 0)
DATA$Stepping_Objects <- ifelse(DATA$incident_type == 'Stepping on Objects', 1, 0)
DATA$Striking_Against <- ifelse(DATA$incident_type == 'Striking against Objects', 1, 0)
DATA$Work_Traffic <- ifelse(DATA$incident_type == 'Work-related Traffic', 1, 0)
  
DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury1=degree_of_injury1,
                   degree_of_injury2=degree_of_injury2,
                   Administrative_Support_Services=Administrative_Support_Services,
                   Community_Social_Services=Community_Social_Services,
                   Construction=Construction,
                   Industry_Others=Industry_Others,
                   Financial_Insurance_Services=Financial_Insurance_Services,
                   Scientific_Technical_Activities=Scientific_Technical_Activities,
                   Water_Supply_Management=Water_Supply_Management,
                   Industrial_Machines=Industrial_Machines,
                   Lifting_Equipment=Lifting_Equipment,
                   Pressurised_Equipments=Pressurised_Equipments,
                   Fire_Explosion=Fire_Explosion,
                   Stepping_Objects=Stepping_Objects)]
write.csv(DATA, './data/OUTPUT/workplace-injuries-by-industry-and-incident-types.csv', row.names = FALSE)

mylogit <- glm(injury_count ~ ., data = DATA, family = "binomial")
summary(mylogit)
vcov(mylogit)
DATA1 <- DATA
DATA1$rankP <- predict(mylogit, newdata = DATA1, type = "response")
write.csv(DATA1, './data/OUTPUT/RESULT.csv', row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#MOdel 4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('./data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$degree_of_injury)

DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 1, 0)
DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 3, ifelse(DATA$degree_of_injury == 'Major', 2, 1))

DATA$degree_of_injury1 <- ifelse(DATA$degree_of_injury == 1, 1, 0)
DATA$degree_of_injury2 <- ifelse(DATA$degree_of_injury == 2, 1, 0)
DATA$degree_of_injury3 <- ifelse(DATA$degree_of_injury == 3, 1, 0)
DATA$Marine <- ifelse(DATA$industry == 'Marine', 1, 0)
DATA$Mining_Quarrying <- ifelse(DATA$industry == 'Mining & Quarrying', 1, 0)
DATA$Administrative_Support_Services <- ifelse(DATA$industry == 'Administrative & Support Services', 1, 0)
DATA$Agriculture_Fishing <- ifelse(DATA$industry == 'Agriculture & Fishing', 1, 0)
DATA$Community_Social_Services <- ifelse(DATA$industry == 'Community, Social & Personal Services', 1, 0)
DATA$Construction <- ifelse(DATA$industry == 'Construction', 1, 0)
DATA$Electricity_Gas_AC_Supply <- ifelse(DATA$industry == 'Electricity, Gas and Air-Conditioning Supply', 1, 0)
DATA$Financial_Insurance_Services <- ifelse(DATA$industry == 'Financial & Insurance Services', 1, 0)
DATA$Manufacturing <- ifelse(DATA$industry == 'Manufacturing', 1, 0)
DATA$Industry_Others <- ifelse(DATA$industry == 'Others', 1, 0)
DATA$Scientific_Technical_Activities <- ifelse(DATA$industry == 'Professional, Scientific & Technical Activities', 1, 0)
DATA$Transportation_Storage <- ifelse(DATA$industry == 'Transportation & Storage', 1, 0)
DATA$Water_Supply_Management <- ifelse(DATA$industry == 'Water Supply, Sewerage & Waste Management', 1, 0)
DATA$Industrial_Machines <- ifelse(DATA$incident_agent == 'Industrial Machines', 1, 0)
DATA$Lifting_Equipment <- ifelse(DATA$incident_agent == 'Lifting Equipment Including Cranes', 1, 0)
DATA$Pressurised_Equipments <- ifelse(DATA$incident_agent == 'Pressurised Equipments', 1, 0)
DATA$Crane_Related <- ifelse(DATA$incident_type == 'Crane-related', 1, 0)
DATA$Stabbed_Objects <- ifelse(DATA$incident_type == 'Cut/Stabbed by Objects', 1, 0)
DATA$Extreme_Temp <- ifelse(DATA$incident_type == 'Exposure to Extreme Temperatures', 1, 0)
DATA$Hazardous_Substance <- ifelse(DATA$incident_type == 'Exposure to Hazardous Substances', 1, 0)
DATA$Falls_Trips <- ifelse(DATA$incident_type == 'Falls - Slips, Trips & Falls', 1, 0)
DATA$Fire_Explosion <- ifelse(DATA$incident_type == 'Fires & Explosion', 1, 0)
DATA$Incident_Type_Others <- ifelse(DATA$incident_type == 'Others', 1, 0)
DATA$Strenuous_Movements <- ifelse(DATA$incident_type == 'Over-exertion/Strenuous Movements', 1, 0)
DATA$Stepping_Objects <- ifelse(DATA$incident_type == 'Stepping on Objects', 1, 0)
DATA$Striking_Against <- ifelse(DATA$incident_type == 'Striking against Objects', 1, 0)
DATA$Work_Traffic <- ifelse(DATA$incident_type == 'Work-related Traffic', 1, 0)

DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury1=degree_of_injury1,
                   degree_of_injury2=degree_of_injury2,
                   Marine=Marine,
                   Mining_Quarrying=Mining_Quarrying,
                   Administrative_Support_Services=Administrative_Support_Services,
                   Community_Social_Services=Community_Social_Services,
                   Construction=Construction,
                   Industry_Others=Industry_Others,
                   Financial_Insurance_Services=Financial_Insurance_Services,
                   Scientific_Technical_Activities=Scientific_Technical_Activities,
                   Water_Supply_Management=Water_Supply_Management,
                   Industrial_Machines=Industrial_Machines,
                   Lifting_Equipment=Lifting_Equipment,
                   Pressurised_Equipments=Pressurised_Equipments,
                   Crane_Related=Crane_Related,
                   Stabbed_Objects=Stabbed_Objects,
                   Extreme_Temp=Extreme_Temp,
                   Hazardous_Substance=Hazardous_Substance,
                   Falls_Trips=Falls_Trips,
                   Fire_Explosion=Fire_Explosion,
                   Incident_Type_Others=Incident_Type_Others,
                   Strenuous_Movements=Strenuous_Movements,
                   Stepping_Objects=Stepping_Objects,
                   Striking_Against=Striking_Against,
                   Work_Traffic=Work_Traffic)]
write.csv(DATA, './data/OUTPUT/workplace-injuries-by-industry-and-incident-types.csv', row.names = FALSE)

mylogit <- glm(injury_count ~ ., data = DATA, family = "binomial")
summary(mylogit)
vcov(mylogit)
library(corr)
DATA1 <- DATA
DATA1$rankP <- predict(mylogit, newdata = DATA1, type = "response")
write.csv(DATA1, './datasets/NUS/OUTPUT/RESULT.csv', row.names = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
DATA <- fread('./data/workplace-injuries-by-industry-and-incident-types.csv')
unique(DATA$degree_of_injury)

DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 'Multiple', 'Single')
DATA$injury_count <- ifelse(DATA$no._of_injuries > 1, 1, 0)
DATA$degree_of_injury <- ifelse(DATA$degree_of_injury == 'Fatal', 3, ifelse(DATA$degree_of_injury == 'Major', 2, 1))

DATA$degree_of_injury1 <- ifelse(DATA$degree_of_injury == 1, 1, 0)
DATA$degree_of_injury2 <- ifelse(DATA$degree_of_injury == 2, 1, 0)
DATA$degree_of_injury3 <- ifelse(DATA$degree_of_injury == 3, 1, 0)
DATA$Administrative_Support_Services <- ifelse(DATA$industry == 'Administrative & Support Services', 1, 0)
DATA$Agriculture_Fishing <- ifelse(DATA$industry == 'Agriculture & Fishing', 1, 0)
DATA$Community_Social_Services <- ifelse(DATA$industry == 'Community, Social & Personal Services', 1, 0)
DATA$Construction <- ifelse(DATA$industry == 'Construction', 1, 0)
DATA$Electricity_Gas_AC_Supply <- ifelse(DATA$industry == 'Electricity, Gas and Air-Conditioning Supply', 1, 0)
DATA$Financial_Insurance_Services <- ifelse(DATA$industry == 'Financial & Insurance Services', 1, 0)
DATA$Manufacturing <- ifelse(DATA$industry == 'Manufacturing', 1, 0)
DATA$Industry_Others <- ifelse(DATA$industry == 'Others', 1, 0)
DATA$Scientific_Technical_Activities <- ifelse(DATA$industry == 'Professional, Scientific & Technical Activities', 1, 0)
DATA$Transportation_Storage <- ifelse(DATA$industry == 'Transportation & Storage', 1, 0)
DATA$Water_Supply_Management <- ifelse(DATA$industry == 'Water Supply, Sewerage & Waste Management', 1, 0)
DATA$Industrial_Machines <- ifelse(DATA$incident_agent == 'Industrial Machines', 1, 0)
DATA$Lifting_Equipment <- ifelse(DATA$incident_agent == 'Lifting Equipment Including Cranes', 1, 0)
DATA$Pressurised_Equipments <- ifelse(DATA$incident_agent == 'Pressurised Equipments', 1, 0)
DATA$Crane_Related <- ifelse(DATA$incident_type == 'Crane-related', 1, 0)
DATA$Stabbed_Objects <- ifelse(DATA$incident_type == 'Cut/Stabbed by Objects', 1, 0)
DATA$Extreme_Temp <- ifelse(DATA$incident_type == 'Exposure to Extreme Temperatures', 1, 0)
DATA$Hazardous_Substance <- ifelse(DATA$incident_type == 'Exposure to Hazardous Substances', 1, 0)
DATA$Falls_Trips <- ifelse(DATA$incident_type == 'Falls - Slips, Trips & Falls', 1, 0)
DATA$Fire_Explosion <- ifelse(DATA$incident_type == 'Fires & Explosion', 1, 0)
DATA$Incident_Type_Others <- ifelse(DATA$incident_type == 'Others', 1, 0)
DATA$Strenuous_Movements <- ifelse(DATA$incident_type == 'Over-exertion/Strenuous Movements', 1, 0)
DATA$Stepping_Objects <- ifelse(DATA$incident_type == 'Stepping on Objects', 1, 0)
DATA$Striking_Against <- ifelse(DATA$incident_type == 'Striking against Objects', 1, 0)
DATA$Work_Traffic <- ifelse(DATA$incident_type == 'Work-related Traffic', 1, 0)
DATA <- DATA[,list(injury_count=injury_count,
                   degree_of_injury1=degree_of_injury1,
                   degree_of_injury2=degree_of_injury2,
                   Administrative_Support_Services=Administrative_Support_Services,
                   Community_Social_Services=Community_Social_Services,
                   Construction=Construction,
                   Industry_Others=Industry_Others,
                   Financial_Insurance_Services=Financial_Insurance_Services,
                   Scientific_Technical_Activities=Scientific_Technical_Activities,
                   Water_Supply_Management=Water_Supply_Management,
                   Industrial_Machines=Industrial_Machines,
                   Lifting_Equipment=Lifting_Equipment,
                   Pressurised_Equipments=Pressurised_Equipments,
                   Fire_Explosion=Fire_Explosion,
                   Stepping_Objects=Stepping_Objects)]
library(corrplot)
M <-cor(DATA)
corrplot(M, method = "square")

write.csv(DATA, './data/OUTPUT/workplace-injuries-by-industry-and-incident-types.csv', row.names = FALSE)

library(caTools)

# Generate a random number sequence that can be reproduced to check results thru the seed number.
set.seed(22)

# Randomly split data from Y into two sets in predefined ratio while preserving relative ratios of different values in Y.
split <- sample.split(DATA$injury_count, SplitRatio = 0.7)

# Get training and test data

mylogit <- glm(injury_count ~ ., data = trainset, family = "binomial")
summary(mylogit)

#Create the Confusion Matrix
prob <- predict(mylogit, type = 'response')
predict <- ifelse(prob>threshold, 1, 0)
table(DATA$injury_count, predict)
#Split Data

#DATA1$rankP <- predict(mylogit, newdata = DATA1, type = "response")
#write.csv(DATA1, './data/OUTPUT/RESULT.csv', row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
counts <- table(DATA$Work_Traffic, DATA$injury_count)
barplot(counts, main="Distribution by Injury Count and Stepping Objects",
        xlab="Number of injury", col=c("darkblue","red"),
        legend = c("Stepping Objects","Stepping Objects"))

counts <- table(DATA$injury_count, DATA$industry)
barplot(counts, main="Distribution by injury_count and degree_of_injury",
        xlab="Number of injury", col=c("darkblue","red"),
        legend = c("Absent","Present"))
