
#98729
#year and ID

TPMfull <- TPMfull %>%
  group_by(COUNTY_DISTRICT_CODE,EDUCATOR_ID) %>%
  mutate(leaver = lead(EDUCATOR_ID),`New Teachers` = lag(EDUCATOR_ID), prevyear = lag(YEAR),nextyear = lead(YEAR))

TPMfull$`New Teachers` <- ifelse(is.na(TPMfull$`New Teachers` == TPMfull$EDUCATOR_ID),1,0)
TPMfull$`New Teachers` <- ifelse(TPMfull$YEAR > TPMfull$prevyear + 1 | is.na(TPMfull$prevyear),1,0)

TPMfull$leaver <- ifelse(is.na(TPMfull$leaver == TPMfull$EDUCATOR_ID),1,0)
TPMfull$leaver <- ifelse(TPMfull$YEAR < TPMfull$nextyear - 1 | is.na(TPMfull$nextyear),1,0)



##This makes the current year blank for the "leaver" catagory
test$leaver <- ifelse(test$YEAR == current_year, NA, test$leaver)

##This makes the current year blank for the "new teacher" catagory
test$`New Teachers` <- ifelse(test$YEAR == min(test$YEAR), NA, test$`New Teachers`)


