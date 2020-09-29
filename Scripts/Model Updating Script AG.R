###Packages###----
require(dplyr)
require(readxl)
require(data.table)

###SPM CODE###----

#Read in county/region names

#County_Names_and_Regions <- read_excel("~/TPM_SPM_Analysis/Data/County Names and Regions.xlsx")

st <- Sys.time()

##Temp county/regions using sup areas

County_Names_and_Regions <- read_excel("Data/newCnR.xlsx")

County_Names_and_Regions$region <- tolower(x = County_Names_and_Regions$region)

#Read in SPM base spreadsheet columns 1 through 22
Updated_SPM_fnl <- read_excel(SPM_filepath, sheet = "base")[,c(1:22)]

#Compare new spreadsheet columns with previous columns, replace if different
SPM_cols <- names(Updated_SPM_fnl)
SPM_col_reference <- read.csv("Data/SPM_cols.csv")
newcolval <- which(!is.na(SPM_col_reference$newname))

test <- lapply(X = paste("^",SPM_col_reference[newcolval,]$newname,"$", sep = ""), FUN = grep, x = SPM_cols, ignore.case = TRUE)
test <- unlist(test)

match_location <- data.frame(x = test, new = as.character(SPM_col_reference[newcolval,]$name))

names(Updated_SPM_fnl)[match_location$x] <- as.vector(match_location$new)

#Create County Number

conc <- sprintf("%06d", Updated_SPM_fnl$COUNTY_DISTRICT_CODE)
Updated_SPM_fnl <- cbind(COUNTY = substr(conc,start = 1,stop = 3),Updated_SPM_fnl)

#Create unique value for KCS:

Updated_SPM_fnl$COUNTY <- as.character(Updated_SPM_fnl$COUNTY)

Updated_SPM_fnl[Updated_SPM_fnl$COUNTY_DISTRICT_CODE == 48078,]$COUNTY <- "KCS"

Updated_SPM_fnl$COUNTY <- as.factor(Updated_SPM_fnl$COUNTY)

#Convert to dataframe to long format by grade:

SPM_L <- melt(data = Updated_SPM_fnl,measure.vars =  names(Updated_SPM_fnl)[-c(1:6)], variable.name = "Grade")
SPM_L$Grade <- gsub(replacement = "", pattern = "ENROLLMENT_GRADES_",x = SPM_L$Grade)

#SPM_aggregated_grades <- SPM_L[SPM_L$Grade %in% c("Elementary Enrollment","High School Enrollment","Middle School Enrollment"),]
SPM_L <- SPM_L[!SPM_L$Grade %in% c("Elementary Enrollment","High School Enrollment","Middle School Enrollment"),]

#Replace K and PK with numbers and convert to numeric data type:

SPM_L[SPM_L$Grade == "PK" ,]$Grade <- "-01"
SPM_L[SPM_L$Grade == "K" ,]$Grade <- "00"

SPM_L$Grade <- as.numeric(SPM_L$Grade)

#Summarizes by county

df <- SPM_L %>%
  group_by(YEAR,Grade,COUNTY) %>%
  summarise(value = sum(value))

remove(SPM_L)
remove(Updated_SPM_fnl)


#Creates enrollment "rate" for each grade level

df <- df %>%
  group_by(COUNTY, Grade) %>%
  mutate(nv = lag(value))%>%
  group_by(COUNTY, YEAR) %>%
  mutate(RATE = value/lag(nv))
df$RATE <- ifelse(is.na(df$RATE),0,df$RATE)
df$Grade <- as.factor(df$Grade)

#This creates 4 year averages for progression rate:
mod1 <- lm(formula = RATE ~ as.factor(Grade)*COUNTY, data = filter(df, !RATE %in% c(Inf, NaN, NA), !YEAR <= (current_year - 4)), na.action = na.omit)

#2 year average of K

mod2 <- lm(formula = value ~ as.factor(Grade)*COUNTY, data = filter(df, !RATE %in% c(Inf, NaN, NA), !YEAR <= (current_year - 2)), na.action = na.omit)

#Trend line for pre-k

mod3 <- lm(formula = value ~ as.factor(Grade)*as.factor(COUNTY)*as.numeric(YEAR), data = filter(df, !RATE %in% c(Inf, NaN, NA)), na.action = na.omit)

#Predicted Enrollment rates

##The dataframe being used for predictions is based on the most recent year of data
df_pred <- df[df$YEAR == current_year,]
forecastframe <- expand.grid(Grade = unique(df_pred$Grade), COUNTY =unique(df_pred$COUNTY), YEAR = seq(current_year + 1,current_year + 5,1))
forecastframe2 <- forecastframe
forecastframe2$YEAR <- current_year + 1

##Make Predictions and merge them with the original dataframe
forecastframe$RATE <- predict(object = mod1, newdata = forecastframe)
forecastframe$enroll <- predict(object = mod2, newdata = forecastframe)
forecastframe$enroll_pk <- predict(object = mod3, newdata = forecastframe2)
df <- merge(x = df, y = forecastframe, all = TRUE)

df[df$YEAR >= current_year + 1 & df$Grade == -1,]$value <- df[df$YEAR >= current_year + 1 & df$Grade == -1,]$enroll_pk
df[df$YEAR >= current_year + 1 & df$Grade == 0,]$value <- df[df$YEAR >= current_year + 1 & df$Grade == 0,]$enroll

df <- df[order(df$YEAR),]

df$pred_enroll <- NA
df$matched_val <- NA
df$Grade <- as.numeric(df$Grade) - 2

for (i in 1:NROW(df)) {
  indx <- which(df$COUNTY[i] == df$COUNTY & 
                  df$YEAR[i] == (df$YEAR + 1) &
                  df$Grade[i] == (df$Grade + 1))
  if((n <- length(indx)) > 1)
    stop("Error, duplicate entry found.")
  else if(n == 1){
    df$pred_enroll[i] <- df$value[indx] * df$RATE[i]
    df$matched_val[i] <- df$value[indx]
    if(df$YEAR[i] > current_year & !df$Grade[i] %in% c(-1,0)){
      df$value[i] <- df$pred_enroll[i]
    }
  }
}

#Mark school type
df <- df %>% mutate(CATEGORY = case_when(Grade < 1 ~ "Elementary",
                                     Grade >= 1 & Grade < 6 ~ "Elementary", 
                                     Grade >= 6 & Grade < 9 ~ "Middle School", 
                                     Grade >= 9 & Grade <= 12 ~ "High School",
                                     TRUE ~ "None"))

#Aggregate Encrollment counts
df <- merge(df,County_Names_and_Regions, all.x = TRUE)
df$region <- ifelse(df$region == "kansas city schools", "kansas city", df$region)
df$region <- ifelse(df$region == "kcs", "kansas city schools", df$region)

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


#Compare new TPM spreadsheet columns with previous columns, replace if different
TPM_col_reference <- read.csv("Data/TPM_cols.csv")
TPM_cols <- names(TPMfull)
newcolval <- which(!is.na(TPM_col_reference$newname))

test <- lapply(X = paste("^",TPM_col_reference[newcolval,]$newname,"$", sep = ""), FUN = grep, x = TPM_cols, ignore.case = TRUE)
test <- unlist(test)

match_location <- data.frame(x = test, new = as.character(TPM_col_reference[newcolval,]$name))

names(TPMfull)[match_location$x] <- as.vector(match_location$new)

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
TPMfull <- TPMfull %>%
  group_by(COUNTY_DISTRICT_CODE,EDUCATOR_ID) %>%
  mutate(leaver = lead(EDUCATOR_ID),`New Teachers` = lag(EDUCATOR_ID), prevyear = lag(YEAR),nextyear = lead(YEAR))

TPMfull$`New Teachers` <- ifelse(is.na(TPMfull$`New Teachers` == TPMfull$EDUCATOR_ID),1,0)
TPMfull$`New Teachers` <- ifelse(TPMfull$YEAR > TPMfull$prevyear + 1 | is.na(TPMfull$prevyear),1,0)

TPMfull$leaver <- ifelse(is.na(TPMfull$leaver == TPMfull$EDUCATOR_ID),1,0)
TPMfull$leaver <- ifelse(TPMfull$YEAR < TPMfull$nextyear - 1 | is.na(TPMfull$nextyear),1,0)

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

##Create new dataframe with aggrigated counts
TPMsubtransformed <- merge(TPMSub, TPMSubnt, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed <- merge(TPMsubtransformed, TPMSubLV, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed <- merge(TPMsubtransformed, TPMSubcert, by = c("region_subject", "CATEGORY", "YEAR"), all.x = TRUE)

##Calculates various rates
TPMsubtransformed$attrition_rate <- TPMsubtransformed$nleaver/TPMsubtransformed$n
TPMsubtransformed[is.na(TPMsubtransformed$attrition_rate),]$attrition_rate <- 0
TPMsubtransformed$not_fully_cert_rate <- TPMsubtransformed$n_not_certr/TPMsubtransformed$n

##Predictions----

temp <- TPMsubtransformed[TPMsubtransformed$YEAR == current_year,]

for (i in 1:5) {
  temp$YEAR <- temp$YEAR + 1 
  TPMsubtransformed <- rbind(TPMsubtransformed, temp)
}

#Clean up data frame----

TPMsubtransformed <- merge(TPMsubtransformed, df_agg, by = c("region", "CATEGORY", "YEAR"), all.x = TRUE)
TPMsubtransformed$ST_ratio <- TPMsubtransformed$n/TPMsubtransformed$enrollment_count

###Remove Excess Columns

TPMsubtransformed <- subset(TPMsubtransformed, select = -c(leaver, APPROPRIATELY_CERTIFIED_YES_OR_NO, `New Teachers`, CTE_Region))

###Change Data Labels

names(TPMsubtransformed)[names(TPMsubtransformed) %in% c("region",
                                                         "CATEGORY",
                                                         "YEAR", 
                                                         "SUBJECT_AREA",
                                                         "n",
                                                         "n_new_teacher",
                                                         "nleaver",
                                                         "n_not_certr",
                                                         "attrition_rate",
                                                         "not_fully_cert_rate",
                                                         "ST_ratio")] <- c("Region",
                                                                                 "Category",
                                                                                 "Year", 
                                                                                 "Subject Area",
                                                                                 "Number of Teachers",
                                                                                 "Number of New Teachers",
                                                                                 "Number of Leavers",
                                                                                 "Number Not Certified",
                                                                                 "Attrition Rate",
                                                                                 "Not Fully Certified Rate",
                                                                                 "Student/Teacher Ratio")

TPMsubtransformed$Region <- tools::toTitleCase(TPMsubtransformed$Region)

#Save Files----

save.image("TPM_Dashboard/Output.RData")

write.csv(TPMsubtransformed, file = "Data/Output/Processed_data.csv")

paste("Updating Finished! Processing Time:",round(Sys.time() - st,1),"Minutes")
