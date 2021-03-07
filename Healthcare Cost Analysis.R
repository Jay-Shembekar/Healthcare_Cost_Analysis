#1.	To record the patient statistics, the agency wants to find the age category of 
# people who frequently visit the hospital and has the maximum expenditure.

library(NCmisc)
get.ext("1555054100_hospitalcosts.xlsx")

library(readxl)
Healthcare <- read_excel("1555054100_hospitalcosts.xlsx")
head(Healthcare)

str(Healthcare)
summary(Healthcare)

AGE_Aggregated <- aggregate(TOTCHG ~ AGE, FUN = sum, data = Healthcare)
AGE_Aggregated

AGE_Aggregated[which.max(AGE_Aggregated$TOTCHG),]

paste("Maximum number of Patients for AGE Max expenditure:", max(AGE_Aggregated$TOTCHG))

hist(Healthcare$AGE, 
     main = "Frequency of Patients",
     col="#cc4d56", 
      xlab ="Age of Patients")



Healthcare$AGE <- as.factor(Healthcare$AGE)

AGE_Dataframe <- data.frame(summary(Healthcare$AGE))
AGE_Dataframe

paste("Max number of Patients for AGE - 0 are : ", max(AGE_Dataframe))

Healthcare$AGE <- as.numeric(Healthcare$AGE)
AGE_Aggregated <- aggregate(x= Healthcare$TOTCHG, by= list(Healthcare$AGE),FUN = sum)
AGE_Aggregated
var <- aggregate(TOTCHG~AGE,FUN=sum,data = Healthcare)
var
paste("Max expenditure: ", max(AGE_Aggregated))
max(AGE_Aggregated)

paste("Max expenditure: ", max(var$AGE))







plot(AGE_Aggregated$Group.1, 
     AGE_Aggregated$x, 
     type = "l",
     col = "#cc4d56",
     xlab = "AGE", 
     ylab = "TOTAL CHARGES", 
     main="Line Graph")

#2nd - In order of severity of the diagnosis and treatments and to 
# find out the expensive treatments, the agency wants to find the diagnosis-related group 
# that has maximum hospitalization and expenditure.

hist(Healthcare$APRDRG,col = "#cc4d56",main = "Frequency of Treatments",xlab = "Treatment Categories")

APRDRG <- as.factor(Healthcare$APRDRG)
APRDRG_Dataframe <- data.frame(summary(APRDRG))
head(APRDRG_Dataframe)

which.max(summary(APRDRG))

APRDG_Aggregated <- aggregate(TOTCHG ~ APRDRG, FUN = sum, data = Healthcare)
APRDG_Aggregated

APRDG_Aggregated[which.max(APRDG_Aggregated$TOTCHG),]

# To make sure that there is no malpractice, the agency needs to analyze if the
# race of the patient is related to the hospitalization costs. 

Healthcare_New <- na.omit(Healthcare) #first we remove "NA"values

colSums(is.na(Healthcare_New))

colSums(is.na(Healthcare))

Healthcare_New$RACE <- as.factor(Healthcare_New$RACE)

AOV_Model <- aov(TOTCHG ~ RACE, data = Healthcare_New)

AOV_Model #ANOVA RESULTS

summary(AOV_Model)

summary(Healthcare_New$RACE)

# To properly utilize the costs, the agency has to analyze the severity of the
# hospital costs by age and gender for proper allocation of resources. 

Healthcare$FEMALE <- as.factor(Healthcare$FEMALE)

LM_Model <- lm(TOTCHG~AGE + FEMALE, data = Healthcare_New) #calling Regression funtion

summary(LM_Model)

summary(Healthcare$FEMALE)

# Since the length of stay is the crucial factor for inpatients, the agency wants
# to find if the length of stay can be predicted from age, gender, and race. 

LM_Model_2 <- lm(LOS ~ RACE + FEMALE + AGE, data = Healthcare_New)

summary(LM_Model_2)


# To perform a complete analysis, the agency wants to find the variable that
# mainly affects the hospital costs. 

LM_Model_3 <- lm(TOTCHG ~ AGE + FEMALE + RACE + LOS + APRDRG, data = Healthcare_New)

summary(LM_Model_3)

summary(Healthcare$AGE)

