#Change this to match the most recent year of data to be included in the model
current_year <- 2018

#Data Location
TPM_filepath <- "~/TPM_SPM_Analysis/Data/TPM Teacher Data Raw1.xlsx"
SPM_filepath <- "~/TPM_SPM_Analysis/Data/Updated SPM_fnl.xlsx"

#Do not modify code below this line--------------------------------------------------------------------------------------------

###Packages###----
require(dplyr)
require(readxl)
require(data.table)
require(stringr)
require(ggplot2)
require(Rcpp)

###SPM CODE###----

#Read in county/region names

County_Names_and_Regions <- read_excel("~/TPM_SPM_Analysis/Data/County Names and Regions.xlsx")
CDC_CTE_REG <- read.csv("~/TPM_SPM_Analysis/Data/CDC_CTE_REG.csv")



#Read in SPM base spreadsheet columns 1 through 22
Updated_SPM_fnl <- read_excel(SPM_filepath, 
                              sheet = "base")[,c(1:22)]

#Create County Number

conc <- str_pad(string = Updated_SPM_fnl$COUNTY_DISTRICT_CODE,width = 6, side = "left",pad = "0")

Updated_SPM_fnl <- cbind(COUNTY = substr(conc,start = 1,stop = 3),Updated_SPM_fnl)

#Create unique value for KCS:

Updated_SPM_fnl$COUNTY <- as.character(Updated_SPM_fnl$COUNTY)

Updated_SPM_fnl[Updated_SPM_fnl$COUNTY_DISTRICT_CODE == 48078,]$COUNTY <- "KCS"

Updated_SPM_fnl$COUNTY <- as.factor(Updated_SPM_fnl$COUNTY)

#Add CTE_region

Updated_SPM_fnl <- merge(x = Updated_SPM_fnl, y = CDC_CTE_REG, all.x = TRUE)

#Convert to dataframe to long format by grade:

SPM_L <- melt(data = Updated_SPM_fnl,
              measure.vars =  names(Updated_SPM_fnl)[!names(Updated_SPM_fnl) %in% c("COUNTY_DISTRICT_CODE",
                                                                                                           "SCHOOL_CODE",
                                                                                                           "COUNTY",
                                                                                                           "YEAR",
                                                                                                           "DISTRICT_NAME",
                                                                                                           "SCHOOL_NAME",
                                                                                                           "CTE_Region")], 
              variable.name = "Grade")
SPM_L$Grade <- gsub(replacement = "", pattern = "ENROLLMENT_GRADES_",x = SPM_L$Grade)

#SPM_aggregated_grades <- SPM_L[SPM_L$Grade %in% c("Elementary Enrollment","High School Enrollment","Middle School Enrollment"),]
SPM_L <- SPM_L[!SPM_L$Grade %in% c("Elementary Enrollment","High School Enrollment","Middle School Enrollment"),]

#Replace K and PK with numbers and convert to numeric data type:

SPM_L[SPM_L$Grade == "PK" ,]$Grade <- "-01"
SPM_L[SPM_L$Grade == "K" ,]$Grade <- "00"

SPM_L$Grade <- as.numeric(SPM_L$Grade)

df <- SPM_L %>%
  group_by(YEAR,Grade,CTE_Region) %>%
  summarise(value = sum(value))

df <- df %>% mutate(CATEGORY = case_when(Grade < 1 ~ "Elementary",
                                         Grade >= 1 & Grade < 6 ~ "Elementary", 
                                         Grade >= 6 & Grade < 9 ~ "Middle School", 
                                         Grade >= 9 & Grade <= 12 ~ "High School",
                                         TRUE ~ "None"))

agged <- df %>%
  group_by(YEAR, CATEGORY) %>%
  summarise(value = sum(value))

#Summarizes by county

df <- SPM_L %>%
  group_by(YEAR,Grade,COUNTY) %>%
  summarise(value = sum(value))

remove(SPM_L)
remove(Updated_SPM_fnl)

#Creates enrollment "rate" for each grade level

df$RATE <- 0
for(i in seq(nrow(df))){
  indx <- which(df$COUNTY[i] == df$COUNTY & 
                  df$YEAR[i] == (df$YEAR + 1) & 
                  df$Grade[i] == (df$Grade + 1))
  if((n <- length(indx)) > 1)
    stop("Error, duplicate entry found.")
  else if(n == 1)
    df$RATE[i] <- df$value[i] / df$value[indx]
}

df$Grade <- as.factor(df$Grade)

#This creates 4 year averages:

mod1 <- lm(formula = RATE ~ as.factor(Grade)*COUNTY, data = filter(df, !RATE %in% c(Inf, NaN, NA), !YEAR <= (current_year - 4)), na.action = na.omit)

#Predicted Enrollment rates

forecastframe <- expand.grid(Grade = unique(df$Grade), COUNTY =unique(df$COUNTY), YEAR = seq(current_year + 1,current_year + 5,1))
forecastframe$RATE <- predict(object = mod1, newdata = forecastframe)
df <- merge(x = df, y = forecastframe, all = TRUE)

df <- df[order(df$YEAR),]

df$pred_enroll <- NA

for (i in 1:NROW(df)) {
  indx <- which(df$COUNTY[i] == df$COUNTY & 
                  df$YEAR[i] == (df$YEAR - 1) &
                  df$Grade[i] == (df$Grade))
  if((n <- length(indx)) > 1)
    stop("Error, duplicate entry found.")
  else if(n == 1){
    df$pred_enroll[indx] <- df$value[i] * df$RATE[indx]
    if(df$YEAR[i] >= current_year){
      df$value[indx] <- df$pred_enroll[indx]
    }
  }
}

#Mark school type
df$Grade <- as.numeric(df$Grade) - 2

df <- df %>% mutate(CATEGORY = case_when(Grade < 1 ~ "Elementary",
                                     Grade >= 1 & Grade < 6 ~ "Elementary", 
                                     Grade >= 6 & Grade < 9 ~ "Middle School", 
                                     Grade >= 9 & Grade <= 12 ~ "High School",
                                     TRUE ~ "None"))

#Aggregate Encrollment counts
df <- merge(df,County_Names_and_Regions, all.x = TRUE)

df_agg <- df %>% group_by(region, CATEGORY, YEAR) %>%
  filter(!is.na(region)) %>%
  summarise(enrollment_count = sum(value))


#
#
#
#
#
#

#TPM CODE###----

TPMfull <- read_excel(TPM_filepath)

##Format/create variables:

TPMfull$APPROPRIATELY_CERTIFIED_YES_OR_NO <- as.factor(TPMfull$APPROPRIATELY_CERTIFIED_YES_OR_NO)

TPMfull$CTE_Region <- TPMfull$SUPERVISOR_REGION_NAME

TPMfull$CTE_Region <- ifelse(TPMfull$COUNTY_DISTRICT_CODE == 48078, 
                             "Kansas City Schools", 
                             ifelse(TPMfull$COUNTY_DISTRICT_CODE == 115115, 
                                    "St. Louis City Schools", 
                                    ifelse(TPMfull$SCHOOL_CODE >= 1100 & TPMfull$SCHOOL_CODE <= 1199, 
                                           "XXOther", 
                                           TPMfull$CTE_Region)))

TPMfull$region_subject <- paste(TPMfull$CTE_Region, TPMfull$SUBJECT_AREA)

##Identify Leavers and new teachers
cppFunction('List findleaver(NumericVector year, NumericVector id){
    int n = year.size();
    
    NumericVector leaver(n);
    NumericVector nt(n);
    
    for(int i = 0; i < n; ++i){
        for(int j = 0; j < n; ++j){
            if(year[i] == year[j] - 1 & id[i] == id[j]){
                leaver[i] = 1;
            } else if(year[i] == year[j] + 1 & id[i] == id[j]){
                nt[i] = 1;
            } else{
                continue;
            }
            
        }
    }
    
    List res;
    res["leaver"] = leaver;
    res["nt"] = nt;
    return res;
}')

leaver <- findleaver(year = TPMfull$YEAR, id = as.numeric(paste(TPMfull$COUNTY_DISTRICT_CODE, TPMfull$EDUCATOR_ID, sep = "")))

TPMfull$leaver <- leaver$leaver
TPMfull$`New Teachers` <- leaver$nt

TPMfull$leaver <- ifelse(TPMfull$leaver == 1, 0, 1)
TPMfull$`New Teachers` <- ifelse(TPMfull$`New Teachers` == 1, 0, 1)

##This makes the current year blank for the "leaver" catagory
TPMfull$leaver <- ifelse(TPMfull$YEAR == current_year, NA, TPMfull$leaver)

##This makes the current year blank for the "new teacher" catagory
TPMfull$`New Teachers` <- ifelse(TPMfull$YEAR == min(TPMfull$YEAR), NA, TPMfull$`New Teachers`)

##Creates Summaries by new teacher, certified, and leaver

TPMSub <- TPMfull %>%
  group_by(CTE_Region, SUBJECT_AREA, CATEGORY, YEAR, .drop=FALSE) %>%
  summarise(n = n()) %>%
  mutate(region_subject = paste(CTE_Region, SUBJECT_AREA), region = tolower(CTE_Region))

TPMSubnt <- TPMfull %>%
  group_by(region_subject, CATEGORY, YEAR, `New Teachers`, .drop=FALSE) %>%
  summarise(n_new_teacher = n()) %>%
  filter(`New Teachers` == 1)

TPMSubLV <- TPMfull %>%
  group_by(region_subject, CATEGORY, YEAR, leaver, .drop=FALSE) %>%
  summarise(nleaver = n()) %>%
  filter(leaver == "1")

TPMSubcert <- TPMfull %>%
  group_by(region_subject, CATEGORY, YEAR, APPROPRIATELY_CERTIFIED_YES_OR_NO, .drop=FALSE) %>%
  summarise(n_not_certr = n()) %>%
  filter(APPROPRIATELY_CERTIFIED_YES_OR_NO == "No")

remove(TPMfull)
remove(leaver)

##Create new dataframe with aggrigated counts
TPMsubtransformed <- merge(TPMSub, TPMSubnt, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed <- merge(TPMsubtransformed, TPMSubLV, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed <- merge(TPMsubtransformed, TPMSubcert, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)

##Calculates various rates
TPMsubtransformed$attrition_rate <- TPMsubtransformed$nleaver/TPMsubtransformed$n
TPMsubtransformed[is.na(TPMsubtransformed$attrition_rate),]$attrition_rate <- 0
TPMsubtransformed$not_fully_cert_rate <- TPMsubtransformed$n_not_certr/TPMsubtransformed$n

TPMsubtransformed <- merge(TPMsubtransformed, df_agg, by = c("region", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed$ST_ratio <- TPMsubtransformed$n/TPMsubtransformed$enrollment_count


##Predictions

###Attrition
###This creates 4 year averages of attrition rate:
modTPM1 <- lm(formula = attrition_rate ~ as.factor(CATEGORY)*region_subject, 
              data = filter(TPMsubtransformed, !n %in% c(Inf, NaN, NA), !(YEAR <= (current_year - 5) | YEAR == current_year)), 
              na.action = na.omit)

###Merge Preds with Data frame
#TPMsubtransformed <- merge(x = TPMsubtransformed, y = forecastframeTPM1, all.x = TRUE)
TPMsubtransformed[is.na(TPMsubtransformed$attrition_rate_p),]$attrition_rate_p <- TPMsubtransformed[is.na(TPMsubtransformed$attrition_rate_p),]$attrition_rate

temp <- TPMsubtransformed[TPMsubtransformed$YEAR == current_year,]

for (i in 1:4) {
  temp$YEAR <- temp$YEAR + 1 
  TPMsubtransformed <- rbind(TPMsubtransformed, temp)
}

#Make predictions

TPMsubtransformed <- filter(TPMsubtransformed, !region %in% c("xxother", "null"))
TPMsubtransformed$preds <- predict(object = modTPM1, newdata = TPMsubtransformed)
TPMsubtransformed$Attrition <- ifelse(TPMsubtransformed$YEAR < current_year, TPMsubtransformed$attrition_rate, TPMsubtransformed$preds)
TPMsubtransformed$Attrition <- TPMsubtransformed$Attrition

###Remove Excess Columns

TPMsubtransformed <- subset(TPMsubtransformed, select = -c(leaver, APPROPRIATELY_CERTIFIED_YES_OR_NO, `New Teachers`, CTE_Region, preds))

#Save Files----

save.image("~/TPM_SPM_Analysis/Images/Output.RData")

write.csv(TPMsubtransformed,file = "~/TPM_SPM_Analysis/Data/Output/Processed_data.csv")

