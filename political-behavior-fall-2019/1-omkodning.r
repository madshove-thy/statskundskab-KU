#### LOAD PACKAGES ####
library("tidyverse")
library("stargazer")
library("codebook")
library("coefplot")
library("ggplot2")
library("gridExtra")
library("gtable")
library("dplyr")
library("Hmisc") # Til at loade stata filer
library("foreign") # TIl at loade stata filer
library("Haven") #Ogsaa til at loade filer?
library("ri2") # Bruges til randomiseringsinferens
library("lfe") ## Til L2S regression
library("dataMaid") ## til codebook
library("sjlabelled") ## Convert labelled data to numeric

#### LOAD DATA ####
load("C:/Users/Bruger/Downloads/TAPSdata2015.R")
load("C:/Users/Bruger/Downloads/2016.R")
load("C:/Users/Bruger/Downloads/2017.R")
load("C:/Users/Bruger/Downloads/Complete Profiles Sorted.rdata")
load("/Users/Madshove/Downloads/TAPSdata2015.R")
load("/Users/Madshove/Downloads/2016.R")
load("/Users/Madshove/Downloads/2017.R")
load("/Users/Madshove/Downloads/Complete-Profiles-Sorted.rdata")

#### RELABEL OF DATAFRAME ####
D15 <- d2015
rm(d2015)

D16 <- dataset
rm(dataset)

D17 <- dataset
rm(dataset)

B <- Complete_Profiles_Sorted
rm(Complete_Profiles_Sorted)

## UNLABEL
D15 <- unlabel(D15, verbose = TRUE)
D16 <- unlabel(D16, verbose = TRUE)
D17 <- unlabel(D17, verbose = TRUE)
B <- unlabel(B, verbose = TRUE)


#### SELECT VARIABLES FOR NEW DATAFRAME ####

##2015
D15D <- D15[ , !duplicated(colnames(D15))]


D96 <- D15D %>%
  select(WUSTLID, ISSUESA4S48, ISSUESA5S48, tm.startS48, MIPS48)

## 2016


D16D <- D16[ , !duplicated(colnames(D16))]

D99 <- D16D %>%
  select(AUTH1S54, AUTH2S54, AUTH3S54, AUTH4S54, AUTH5S54, AUTH11S54, AUTH12S54, AUTH13S54, AUTH14S54,
         BEDROCK1S54, BEDROCK2S54, BEDROCK3S54, BEDROCK4S54, BEDROCK5S54, BEDROCK6S54, BEDROCK7S54,
         BEDROCK8S54, BEDROCK9S54, BEDROCK10S54, BEDROCK11S54, BEDROCK12S54, BEDROCK13S54, BEDROCK14S54,
         ISSUESA4GS55, ISSUESA5GS55, ISSUESA4GS57, # gun controltype 1 og same sex marriages i juni og august
         GUNS1S51, GUNS1S56, # gun control i februar og juli
         tm_startS54, tm_startS55, tm_startS56,
         LIBCON0S51, LIBCON0S54, LIBCON0S55, LIBCON0S56,
         IMMG1S54, IMMG1S56, IMMG2SS54, IMMG2SS56, IMMG4SS54, IMMG4SS56, IMMIG14SS54, IMMIG14SS56, IMMG20SS54, IMMG20SS56,
         ISSUESA10S51, ISSUESA10S55, # ISIS
         RREL1S54, # Race relation
         APPRCONGS58, APPRCONGS60, # Approvalratings congress
         MIPS55, MIPS57, MIPS58, MIPS59, MIPS60, # Saliens
         WUSTLID)

### OBS OBS AUGUST 2016 variable som er taget ud i forste omgang  tm_startS57, ISSUESA5GS57,

## 2017


D98 <- D17 %>%
  select(ISSUESA4S62, ISSUESA4S63, ISSUES4S67, ISSUES4S68, ISSUES4S69, # gun control type 1
         GUNS13S69, # gun control type 2 i november
         IMMG14SS67, IMMG20SS67, IMMG14SS68, IMMG20SS68, # crime + wall
         ISSUES10S67, ISSUES10S68, ISSUES10S69, # ISIS
         RREL1S66, # Race relation
         APPRCONGS62, APPRCONGS63, APPRCONGS65, APPRCONGS67, APPRCONGS69, # Approvalratings congress
         MIPS64, MIPS66, MIPS69, # Saliens
         WUSTLID,
         LIBCON0S67,
         tm_startS67, tm_startS68, tm_startS69,
         LIBCON0S67)

## Baggrund


B <- B %>%
  mutate(WUSTLID = wustlid)

B99 <- B %>%
  select(RACE1SP, WUSTLID, ppstate, ppgender, ppeducat)



#### Combine into one dataframe ####
## First we combine the two dataframes from 2016 and 2017
D97 <- inner_join(D99, D98, by = "WUSTLID")
## Then we combine the new datafram with the one in 2015
D95 <- inner_join(D97, D96, by = "WUSTLID")
## Then we combine that one with the dataframe with background variables into our final dataset D
D <- inner_join(D95, B99, by = "WUSTLID")

# Vi finder 229 rows der indeholder mindst en NA ser bagefter paa hvilke variable de fordeler sig paa
rowna <- apply(D, 1, function(X) any(is.na(X)))
sum(str_count(rowna, pattern = "TRUE"))
sapply(D, function(X) sum(is.na(X)))


######## RECODE VARIABLES ########
label_browser_static(D)

#### REMOVE NON-WHITES FROM DATASET ####
table(D$RACE1SP)

D <- D %>%
  filter(RACE1SP == 1)

#### GUN CONTROL ####

## Pre Orlando november 2015

class(D$ISSUESA4S48)
summary(D$ISSUESA4S48)
head(D$ISSUESA4S48)


D <- D %>%
  mutate(gun15_nov_mis = na_if(ISSUESA4S48, "1"))

summary(D$gun15_nov_mis)

D <- D %>%
  mutate(gun15_nov = recode(gun15_nov_mis, "2" = 1,
                            "3" = 0,
                            "4" = 0))
summary(D$gun15_nov)




## ORLANDO juni sporgsmaal TYPE 1 ##
class(D$ISSUESA4GS55)
summary(D$ISSUESA4GS55)
head(D$ISSUESA4GS55)

D <- D %>%
  mutate(gun16_jun = recode(ISSUESA4GS55, "support" = 1,
                            "oppose" = 0,
                            "no opinion" = 0))

summary(D$gun16_jun)

## ORLANDO august sporsmaal TYPE 1 ##

D <- D %>%
  mutate(gun16_aug = recode(ISSUESA4GS57, "support" = 1,
                            "oppose" = 0,
                            "no opinion" = 0))

summary(D$gun16_aug)

## ORLANDO februar sporgsmaal TYPE 2 ##
class(D$GUNS1S51)
summary(D$GUNS1S51)
head(D$GUNS1S51)

D <- D %>%
  mutate(gun16_feb = recode(GUNS1S51, "should be more strict" = 1,
                            "should be less strict" = 0,
                            "should be kept as they are now" = 0,
                            "don't know" = 0))

summary(D$gun16_feb)

## ORLANDO juli sporgsmaal TYPE 2 ##
class(D$GUNS1S56)
summary(D$GUNS1S56)
head(D$GUNS1S56)

D <- D %>%
  mutate(gun16_jul = recode(GUNS1S56, "should be more strict" = 1,
                            "should be less strict" = 0,
                            "should be kept as they are now" = 0,
                            "don't know" = 0))

summary(D$gun16_jul)

##Las Vegas Marts ##

D <- D %>%
  mutate(gun17_mar_mis = na_if(ISSUESA4S63, "-1"))

summary(D$gun17_mar_mis)

D <- D %>%
  mutate(gun17_mar = recode(gun17_mar_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))
summary(D$gun17_mar)



## Las Vegas august ##
class(D$ISSUES4S67)
summary(D$ISSUES4S67)
head(D$ISSUES4S67)

# Fjerne refused answers
D <- D %>%
  mutate(gun17_aug_mis = na_if(ISSUES4S67, "-1"))

summary(D$gun17_aug_mis)

D <- D %>%
  mutate(gun17_aug = recode(gun17_aug_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(D$gun17_aug)

## Las Vegas october ##
class(D$ISSUES4S68)
summary(D$ISSUES4S68)
head(D$ISSUES4S68)

# Remove refused answers
D <- D %>%
  mutate(gun17_oct_mis = na_if(ISSUES4S68, "-1"))

summary(D$gun17_oct_mis)

D <- D %>%
  mutate(gun17_oct = recode(gun17_oct_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(D$gun17_oct)

## Las Vegas november sporgsmaal TYPE 1 ##
class(D$ISSUES4S69)
summary(D$ISSUES4S69)
head(D$ISSUES4S69)

# Remove refused answers
D <- D %>%
  mutate(gun17_nov_mis = na_if(ISSUES4S69, "-1"))

summary(D$gun17_nov_mis)

D <- D %>%
  mutate(gun17_nov = recode(gun17_nov_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(D$gun17_nov)

## Las Vegas november sporgsmaal TYPE 2 ##
class(D$GUNS13S69)
summary(D$GUNS13S69)
head(D$GUNS13S69)

# Remove refused answers
D <- D %>%
  mutate(gun17_nov1_mis = na_if(GUNS13S69, "-1"))

summary(D$gun17_nov1_mis)

D <- D %>%
  mutate(gun17_nov1 = recode(gun17_nov1_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(D$gun17_nov1)

#### CREATE TIME VARIABLE ####
## Treatment time - AFTER MASS SHOOTING ##
# Adjust time variable #
D$time_june_16 <- as.numeric(format(as.POSIXct(D$tm_startS55, origin = "1582-10-15"), format = "%Y%m%d"))
D$time_october_17 <- as.numeric(format(as.POSIXct(D$tm_startS68, origin = "1582-10-15"), format = "%Y%m%d"))

# Create variable of before and after shootings #
D$shootingtime16 <- ifelse(D$time_june_16 >= 20160612, 1, 0)
D$shootingstime17 <- ifelse(D$time_october_17 >= 20171002, 1, 0)



#### RECODE OF LIB CON ####
#### RECODE IDEOLOGI ####
table(D$LIBCON0S51)
table(D$LIBCON0S54)
table(D$LIBCON0S55)
table(D$LIBCON0S56)
table(D$LIBCON0S67)

D <- D %>%
  mutate(libcon16feb = recode(LIBCON0S51, "Extremely liberal" = 5,
                           "Liberal" = 4,
                           "Slightly liberal" = 4,
                           "Moderate; middle of the road" = 3,
                           "Slightly conservative" = 2,
                           "Conservative" = 2,
                           "Extremely conservative" = 1,
                           "Don’t Know" = 3))

table(D$libcon16feb)

D <- D %>%
  mutate(libcon16maj = recode(LIBCON0S54, "very liberal" = 5,
                           "liberal" = 4,
                           "moderate;middle of the road" = 3,
                           "conservative" = 2,
                           "very conservative" = 1,
                           "don’t know" = 3))

table(D$libcon16maj)

D <- D %>%
  mutate(libcon16jun_mis = na_if(LIBCON0S55, "-1"))

D <- D %>%
  mutate(libcon16jun = recode(libcon16jun_mis, "1" = 5,
                              "2" = 4,
                              "3" = 3,
                              "4" = 2,
                              "5" = 1,
                              "6" = 3))

table(D$libcon16jun)

D <- D %>%
  mutate(libcon16jul = recode(LIBCON0S56, "very liberal" = 5,
                              "liberal" = 4,
                              "moderate;middle of the road" = 3,
                              "conservative" = 2,
                              "very conservative" = 1,
                              "don" = 3))

table(D$libcon16jul)

D <- D %>%
  mutate(libcon17aug_mis = na_if(LIBCON0S67, "-1"))

D <- D %>%
  mutate(libcon17aug = recode(libcon17aug_mis, "1" = 5,
                              "2" = 4,
                              "3" = 3,
                              "4" = 2,
                              "5" = 1,
                              "6" = 3))

table(D$libcon17aug)




#### OPINION TOWARDS IMMIGRANTS ####
####################################
### ILLEGAL IMMIGRANTS ARE THE CAUSE OF CRIME ###
## ORLANDO ##
## MAY ##
class(D$IMMIG14SS54)
summary(D$IMMIG14SS54)
head(D$IMMIG14SS54)

# Remove refused answers
D <- D %>%
  mutate(imm16_may_mis = na_if(IMMIG14SS54, "Refused"))

summary(D$imm16_may_mis)

D <- D %>%
  mutate(imm16_may = recode(imm16_may_mis, "very concerned" = 3,
                            "somewhat concerned" = 2,
                            "not concerned" = 1,
                            "don't know" = 2))

summary(D$imm16_may)

## JULY ##
class(D$IMMIG14SS56)
summary(D$IMMIG14SS56)
head(D$IMMIG14SS56)

# Deal with refused and "dont know" answers
D <- D %>%
  mutate(imm16_jul_mis = na_if(IMMIG14SS56, "Refused"))

summary(D$imm16_jul_mis)

D <- D %>%
  mutate(imm16_jul = recode(imm16_jul_mis, "very concerned" = 3,
                            "somewhat concerned" = 2,
                            "not concerned" = 1,
                            "don't know" = 2))

summary(D$imm16_jul)

## LAS VEGAS ##
## AUGUST ##
class(D$IMMG14SS67)
summary(D$IMMG14SS67)
head(D$IMMG14SS67)

# Deal with refused and "dont know" answers
D <- D %>%
  mutate(imm17_aug_mis = na_if(IMMG14SS67, "-1"))

summary(D$imm17_aug_mis)

D <- D %>%
  mutate(imm17_aug = recode(imm17_aug_mis, "1" = 3,
                            "2" = 2,
                            "3" = 1,
                            "4" = 2))

summary(D$imm17_aug)

## OKTOBER ##
class(D$IMMG14SS68)
summary(D$IMMG14SS68)
head(D$IMMG14SS68)

# Deal with refused and "dont know" answers
D <- D %>%
  mutate(imm17_okt_mis = na_if(IMMG14SS68, "-1"))

summary(D$imm17_okt_mis)

D <- D %>%
  mutate(imm17_okt = recode(imm17_okt_mis, "1" = 3,
                            "2" = 2,
                            "3" = 1,
                            "4" = 2))

summary(D$imm17_okt)


####IMMIGRATION GOOD OR BAD THING FOR USA TODAY####
##Omkodning##
##Maj 2016
table(D$IMMG4SS54)

D <- D %>%
  mutate(immgood16_may = recode(IMMG4SS54, "good thing" = 1,
                             "bad thing" = 0,
                             "don't know" = 0))

summary(D$immgood16_may)

## Juli 2016
table(D$IMMG4SS56)

D <- D %>%
  mutate(immgood16_jul = recode(IMMG4SS56, "good thing" = 1,
                                "bad thing" = 0,
                                "don't know" = 0))

summary(D$immgood16_jul)




#### YDERLIGERE KONTROLVARIABLE ####
####################################

label_browser_static(D)

### RACE RELATIONS ###
table(D$RREL1S54)
table(D$RREL1S66)

#2016
D <- D %>%
  mutate(racerelation16 = recode(RREL1S54, "very poor" = 5,
                                 "poor" = 4,
                                 "neither good nor poor" = 3,
                                 "good" = 2,
                                 "very good" = 1,
                                 "don't know" = 3))

table(D$racerelation16)


#2017
D <- D %>%
  mutate(racerelation17_mis = na_if(RREL1S66, "-1"))

summary(D$racerelation17_mis)

D <- D %>%
  mutate(racerelation17 = recode(racerelation17_mis, "1" = 1,
                                 "2" = 2,
                                 "3" = 3,
                                 "4" = 4,
                                 "5" = 5,
                                 "6" = 3))

table(D$racerelation17)


### SALIENS ###
# 2015: MIPS48,
table(D$MIPS48)

D <- D %>%
  mutate(saliens15_nov_mis = na_if(MIPS48, "1"))

D <- D %>%
  mutate(saliens15_nov = recode(saliens15_nov_mis, "15" = 1,
                                "2" = 0,
                                "3" = 0,
                                "4" = 0,
                                "5" = 0,
                                "6" = 0,
                                "7" = 0,
                                "8" = 0,
                                "9" = 0,
                                "10" = 0,
                                "11" = 0,
                                "12" = 0,
                                "13" = 0,
                                "14" = 0,
                                "16" = 0,
                                "17" = 0,
                                "18" = 0,
                                "19" = 0))

table(D$saliens15_nov)


# 2016: MIPS55, MIPS57, MIPS58, MIPS59, MIPS60,
table(D$MIPS55)
table(D$MIPS57)
table(D$MIPS58)
table(D$MIPS59)
table(D$MIPS60)

D <- D %>%
  mutate(saliens16_jun = recode(MIPS55, "National security" = 1,
                                "Abortion" = 0,
                                "Budget deficit" = 0,
                                "Cost of living" = 0,
                                "Crime" = 0,
                                "Fuel/gas/oil prices" = 0,
                                "Economy in general" = 0,
                                "Education" = 0,
                                "Environment" = 0,
                                "Healthcare" = 0,
                                "Poverty" = 0,
                                "Taxes" = 0,
                                "War against ISIS" = 0,
                                "Something else" = 0))

table(D$saliens16_jun)



D <- D %>%
  mutate(saliens16_aug = recode(MIPS57, "National security" = 1,
                                "Abortion" = 0,
                                "Budget deficit" = 0,
                                "Cost of living" = 0,
                                "Crime" = 0,
                                "Fuel/gas/oil prices" = 0,
                                "Economy in general" = 0,
                                "Education" = 0,
                                "Environment" = 0,
                                "Healthcare" = 0,
                                "Poverty" = 0,
                                "Taxes" = 0,
                                "War against ISIS" = 0,
                                "Something else" = 0))

table(D$saliens16_aug)



D <- D %>%
  mutate(saliens16_sep = recode(MIPS58, "National security" = 1,
                                "Abortion" = 0,
                                "Budget deficit" = 0,
                                "Cost of living" = 0,
                                "Crime" = 0,
                                "Fuel/gas/oil prices" = 0,
                                "Economy in general" = 0,
                                "Education" = 0,
                                "Environment" = 0,
                                "Healthcare" = 0,
                                "Poverty" = 0,
                                "Taxes" = 0,
                                "War against ISIS" = 0,
                                "Something else" = 0))

table(D$saliens16_sep)


D <- D %>%
  mutate(saliens16_oct = recode(MIPS59, "National security" = 1,
                                "Abortion" = 0,
                                "Budget deficit" = 0,
                                "Cost of living" = 0,
                                "Crime" = 0,
                                "Fuel/gas/oil prices" = 0,
                                "Economy in general" = 0,
                                "Education" = 0,
                                "Environment" = 0,
                                "Healthcare" = 0,
                                "Poverty" = 0,
                                "Taxes" = 0,
                                "War against ISIS" = 0,
                                "Something else" = 0))

table(D$saliens16_oct)



D <- D %>%
  mutate(saliens16_nov = recode(MIPS60, "National security" = 1,
                                "Abortion" = 0,
                                "Budget deficit" = 0,
                                "Cost of living" = 0,
                                "Crime" = 0,
                                "Fuel/gas/oil prices" = 0,
                                "Economy in general" = 0,
                                "Education" = 0,
                                "Environment" = 0,
                                "Healthcare" = 0,
                                "Poverty" = 0,
                                "Taxes" = 0,
                                "War against ISIS" = 0,
                                "Something else" = 0))

table(D$saliens16_nov)


# 2017: MIPS64, MIPS66, MIPS69,
summary(D$MIPS64)
summary(D$MIPS66)
summary(D$MIPS69)

D <- D %>%
  mutate(saliens17_apr = recode(MIPS64, "14" = 1,
                                "-1" = 0,
                                "1" = 0,
                                "2" = 0,
                                "3" = 0,
                                "4" = 0,
                                "5" = 0,
                                "6" = 0,
                                "7" = 0,
                                "8" = 0,
                                "9" = 0,
                                "10" = 0,
                                "11" = 0,
                                "12" = 0,
                                "13" = 0,
                                "15" = 0,
                                "16" = 0,
                                "17" = 0,
                                "18" = 0))

table(D$saliens17_apr)



D <- D %>%
  mutate(saliens17_jul = recode(MIPS66, "14" = 1,
                                "-1" = 0,
                                "1" = 0,
                                "2" = 0,
                                "3" = 0,
                                "4" = 0,
                                "5" = 0,
                                "6" = 0,
                                "7" = 0,
                                "8" = 0,
                                "9" = 0,
                                "10" = 0,
                                "11" = 0,
                                "12" = 0,
                                "13" = 0,
                                "15" = 0,
                                "16" = 0,
                                "17" = 0,
                                "18" = 0))

table(D$saliens17_jul)


D <- D %>%
  mutate(saliens17_nov = recode(MIPS69, "14" = 1,
                                "-1" = 0,
                                "1" = 0,
                                "2" = 0,
                                "3" = 0,
                                "4" = 0,
                                "5" = 0,
                                "6" = 0,
                                "7" = 0,
                                "8" = 0,
                                "9" = 0,
                                "10" = 0,
                                "11" = 0,
                                "12" = 0,
                                "13" = 0,
                                "15" = 0,
                                "16" = 0,
                                "17" = 0,
                                "18" = 0))

table(D$saliens17_nov)



##### LIBCON #######
#### RECODE OF LIB CON ####
#### RECODE IDEOLOGI ####
table(D$LIBCON0S51)
table(D$LIBCON0S54)
table(D$LIBCON0S55)
table(D$LIBCON0S56)
table(D$LIBCON0S67)

D <- D %>%
  mutate(libcon16feb = recode(LIBCON0S51, "Extremely liberal" = 5,
                              "Liberal" = 4,
                              "Slightly liberal" = 4,
                              "Moderate; middle of the road" = 3,
                              "Slightly conservative" = 2,
                              "Conservative" = 2,
                              "Extremely conservative" = 1,
                              "Don’t Know" = 3))

table(D$libcon16feb)


D <- D %>%
  mutate(libcon16maj = recode(LIBCON0S54, "very liberal" = 5,
                              "liberal" = 4,
                              "moderate;middle of the road" = 3,
                              "conservative" = 2,
                              "very conservative" = 1,
                              "don’t know" = 3))

table(D$libcon16maj)



D <- D %>%
  mutate(libcon16jun_mis = na_if(LIBCON0S55, "-1"))

D <- D %>%
  mutate(libcon16jun = recode(libcon16jun_mis, "1" = 5,
                              "2" = 4,
                              "3" = 3,
                              "4" = 2,
                              "5" = 1,
                              "6" = 3))

table(D$libcon16jun)


D <- D %>%
  mutate(libcon16jul = recode(LIBCON0S56, "very liberal" = 5,
                              "liberal" = 4,
                              "moderate;middle of the road" = 3,
                              "conservative" = 2,
                              "very conservative" = 1,
                              "don" = 3))

table(D$libcon16jul)


D <- D %>%
  mutate(libcon17aug_mis = na_if(LIBCON0S67, "-1"))

D <- D %>%
  mutate(libcon17aug = recode(libcon17aug_mis, "1" = 5,
                              "2" = 4,
                              "3" = 3,
                              "4" = 2,
                              "5" = 1,
                              "6" = 3))

table(D$libcon17aug)


#####################
#### INDEKSERING ####
#####################
library("corrplot")
library("caret")
library("sjt.itemanalysis") # Til cronbachs alpha bl.a.

##### RECODE DER LIGGER BAG INDEKSERING #####
#### Indeks for autoritarisme ####
## Variable: AUTH1S54, AUTH2S54, AUTH3S54, AUTH4S54, AUTH5S54, AUTH11S54, AUTH12S54, AUTH13S54, AUTH14S54
## Vi laver to indeks, et med afsaet i borneopdragelse og et med mere traditionel/progressiv tankegang
## Forst starter vi med borneopdragelse AUTH11S54, AUTH12S54, AUTH13S54, AUTH14S54
## Independence or respect for elders
class(D$AUTH11S54)
summary(D$AUTH11S54)
head(D$AUTH11S54)

D <- D %>%
  mutate(authb1 = recode(AUTH11S54, "independence" = 0,
                         "or" = 1))

summary(D$authb1)

## Curiosity or good manners
class(D$AUTH12S54)
summary(D$AUTH12S54)
head(D$AUTH12S54)

D <- D %>%
  mutate(authb2 = recode(AUTH12S54, "curiosity" = 0,
                         "or" = 1))

summary(D$authb2)

## Obedience or self reliance
class(D$AUTH13S54)
summary(D$AUTH13S54)
head(D$AUTH13S54)

D <- D %>%
  mutate(authb3 = recode(AUTH13S54, "obedience" = 1,
                         "or" = 0))

summary(D$authb3)

## Being considerate or being well behaved
class(D$AUTH14S54)
summary(D$AUTH14S54)
head(D$AUTH14S54)

D <- D %>%
  mutate(authb4 = recode(AUTH14S54, "being considerate" = 0,
                         "or" = 1))

summary(D$authb4)

#### Nu laver vi indeks for tradtionel autoritarisme med variablene AUTH1S54, AUTH2S54, AUTH3S54, AUTH4S54, AUTH5S54 ####
## There is no one right way to live life; everybody has to create their own way
class(D$AUTH1S54)
summary(D$AUTH1S54)
head(D$AUTH1S54)

# Deal with refused answers
D <- D %>%
  mutate(autht1 = recode(AUTH1S54, "strongly agree" = 1,
                         "agree" = 2,
                         "neither agree nor disagree" = 3,
                         "disagree" = 4,
                         "strongly disagree" = 5))

summary(D$autht1)

## Our country needs free thinkers who will have the courage to defy traditional ways, even if this upsets many people
class(D$AUTH2S54)
summary(D$AUTH2S54)
head(D$AUTH2S54)

# Deal with refused answers
D <- D %>%
  mutate(autht2 = recode(AUTH2S54, "strongly agree" = 1,
                         "agree" = 2,
                         "neither agree nor disagree" = 3,
                         "disagree" = 4,
                         "strongly disagree" = 5))

summary(D$autht2)

## The old-fashioned ways and old fashioned values still shows the best way to live
class(D$AUTH3S54)
summary(D$AUTH3S54)
head(D$AUTH3S54)

# Deal with refused answers
D <- D %>%
  mutate(autht3 = recode(AUTH3S54, "strongly agree" = 5,
                         "agree" = 4,
                         "neither agree nor disagree" = 3,
                         "disagree" = 2,
                         "strongly disagree" = 1))

summary(D$autht3)

## Our country will be great if we honor the ways of our forefathers, do what the authorities tell us to do and get rid of the rotten apples who are ruining everything
class(D$AUTH4S54)
summary(D$AUTH4S54)
head(D$AUTH4S54)

# Deal with refused answers
D <- D %>%
  mutate(autht4 = recode(AUTH4S54, "strongly agree" = 5,
                         "agree" = 4,
                         "neither agree nor disagree" = 3,
                         "disagree" = 2,
                         "strongly disagree" = 1))

summary(D$autht4)

## What our country really needs is a strong, determined leader who will crush evil and take us back to our true path
class(D$AUTH5S54)
summary(D$AUTH5S54)
head(D$AUTH5S54)

# Deal with refused answers
D <- D %>%
  mutate(autht5 = recode(AUTH5S54, "strongly agree" = 5,
                         "agree" = 4,
                         "neither agree nor disagree" = 3,
                         "disagree" = 2,
                         "strongly disagree" = 1))

summary(D$autht5)


##### INDEKSERING AF AUTORITARISME MED BORN SPORGSMAAL #####
## Lav ny dataframe
indeksauthb <- data.frame(D$authb1, D$authb2, D$authb3, D$authb4)

## Kod NA's paa medianen
preprocess_scheme <- preProcess(x = indeksauthb, method = c("medianImpute"))

indeksauthb <- predict(preprocess_scheme, indeksauthb)
sum(is.na(indeksauthb))

## Cronbachs alpha test
cor(indeksauthb) # Vaerdi under 30 for alle med authb4
table(indeksauthb)

## Smid indeks ind i dataframe som 1 kolonne - Skalaen gaar mod mere autoritativ
D$indeksauthb <- (indeksauthb$D.authb1 + indeksauthb$D.authb2 + indeksauthb$D.authb3 + indeksauthb$D.authb4)/4

## Recode Skalaen gaar mod mere autoritativ
D <- D %>%
  mutate(indeksauthborn = recode(indeksauthb, "0" = 1,
                                 "0.25" = 2,
                                 "0.5" = 3,
                                 "0.75" = 4,
                                 "1" = 5))

table(D$indeksauthborn)


##### INDEKSERING AF AUTORITARISME MED BORN SPORGSMAAL #####
## Lav ny dataframe
indeksauthb <- data.frame(D$authb1, D$authb2, D$authb3, D$authb4)

## Kod NA's paa medianen
preprocess_scheme <- preProcess(x = indeksauthb, method = c("medianImpute"))

indeksauthb <- predict(preprocess_scheme, indeksauthb)
sum(is.na(indeksauthb))

## Cronbachs alpha test
cor(indeksauthb)
table(indeksauthb)

## Smid indeks ind i dataframe som 1 kolonne - Skalaen gaar mod mere autoritativ
D$indeksauthb <- (indeksauthb$D.authb1 + indeksauthb$D.authb2 + indeksauthb$D.authb3 + indeksauthb$D.authb4)/4

## Recode Skalaen gaar mod mere autoritativ
D <- D %>%
  mutate(indeksauthborn = recode(indeksauthb, "0" = 1,
                                 "0.25" = 2,
                                 "0.5" = 3,
                                 "0.75" = 4,
                                 "1" = 5))

table(D$indeksauthborn)


##### INDEKSERING AF AUTORITARISME MED TRADITIONEL / PROGRESSIV SPORGSMAAL #####
## Lav ny dataframe
indeksautht <- data.frame(D$autht1, D$autht2, D$autht3, D$autht4, D$autht5)

## Kod NA's paa medianen
preprocess_scheme1 <- preProcess(x = indeksautht, method = c("medianImpute"))

indeksautht <- predict(preprocess_scheme1, indeksautht)
sum(is.na(indeksauthb))

## Cronbachs alpha test
cor(indeksautht)

## Smid indeks ind i dataframe som 1 kolonne - Skalaen gaar mod mere autoritativ
D$indeksautht <- (indeksautht$D.autht1 + indeksautht$D.autht2 + indeksautht$D.autht3 + indeksautht$D.autht4 + indeksautht$D.autht5)/5
table(D$indeksautht)


#####################################
#####################################
#####################################
##### SAMMENLAEGNING AF DATASET #####
#####################################
#####################################
#####################################


###########################################
############### GUN CONTROL ###############
###########################################
#### Gather gun control (in dataset DG) variables ####
DG <- D
DG <- gather(DG, "Q", "gun", gun15_nov, gun16_feb, gun16_jun, gun16_jul, gun17_mar, gun17_aug, gun17_oct, gun17_nov1)

DG <- DG %>%
  mutate(month = recode(Q, "gun15_nov" = "nov15",
                        "gun16_feb" = "feb16",
                        "gun16_jun" = "jun16",
                        "gun16_jul" = "jul16",
                        "gun17_mar" = "mar17",
                        "gun17_aug" = "aug17",
                        "gun17_oct" = "oct17",
                        "gun17_nov1" = "nov17"))

table(DG$month)

## Treatment spot - ORLANDO ##
DG$treatmentsted <- ifelse(DG$Q == "gun16_jun"|
                             DG$Q == "gun16_feb"|
                             DG$Q == "gun16_jul"|
                             DG$Q == "gun15_nov", 1, 0)

# Create treatment time #
DG$treatmenttid <- ifelse(DG$Q == "gun16_jun" & DG$shootingtime16 == "1"|
                            DG$Q == "gun16_jul"|
                            DG$Q == "gun17_nov1"|
                            DG$Q == "gun17_oct" & DG$shootingstime17 == "1", 1, 0)


#################################################################
########################### IMMIGRATION #########################
#################################################################
############### IMMIGRATION CRIME TIL TABEL 1 og 2 ################
#### Gather immigration (in DI dataset) variables ####
DI <- D
DI <- gather(DI, "Q", "immigration", imm16_may, imm16_jul, imm17_aug, imm17_okt)

## Treatment spot - ORLANDO ##
DI$treatmentsted <- ifelse(DI$Q == "imm16_may"|
                             DI$Q == "imm16_jul", 1, 0)

# Create treatment time #
DI$treatmenttid <- ifelse(DI$Q == "imm16_jul"|
                            DI$Q == "imm17_okt" & DI$shootingstime17 == "1", 1, 0)

# Omkodning af maaneder
DI <- DI %>%
  mutate(month = recode(Q, "imm16_may" = "feb16",
                        "imm16_jul" = "jul16",
                        "imm17_aug" = "aug17",
                        "imm17_okt" = "oct17"))

table(DI$month)


############### IMMIGRATION GOOD/BAD TIL AFSNIT ROBUSTHED ################
#### Gather variables ####
DB <- D
DB <- gather(DB, "Q", "immgood", immgood16_may, immgood16_jul)

# Create treatment time #
DB$treatmenttid <- ifelse(DB$Q == "immgood16_jul", 1, 0)

# Omkodning af maaneder
DB <- DB %>%
  mutate(month = recode(Q, "immgood16_may" = "feb16",
                        "immgood16_jul" = "jul16"))

table(DB$month)




##############################################
################ KONTROLVARIABLE #############
##############################################

########### IDEOLOGI ############
#### NYT DATASET TIL AT KOBLE IDEOLOGI PAA MAANEDERNE ####
DLIBCON <- D %>%
  dplyr::select(WUSTLID, libcon16feb, libcon16maj, libcon16jun, libcon16jul, libcon17aug)

#### Gather ideologi ####
DLIBCON$libcon15nov <- DLIBCON$libcon16feb
DLIBCON$libcon17mar <- DLIBCON$libcon16jul
DLIBCON$libcon17oct <- DLIBCON$libcon17aug
DLIBCON$libcon17nov <- DLIBCON$libcon17aug

DLIBCON$nov15 <- DLIBCON$libcon15nov
DLIBCON$feb16 <- DLIBCON$libcon16feb
DLIBCON$jun16 <- DLIBCON$libcon16jun
DLIBCON$jul16 <- DLIBCON$libcon16jul
DLIBCON$mar17 <- DLIBCON$libcon17mar
DLIBCON$aug17 <- DLIBCON$libcon17aug
DLIBCON$oct17 <- DLIBCON$libcon17oct
DLIBCON$nov17 <- DLIBCON$libcon17nov

DLIBCON = DLIBCON[,-c(2:10)]

DLIBCON = gather(DLIBCON, "month", "libcon", nov15, feb16, jun16, jul16, mar17, aug17, oct17, nov17)

################## FULL JOIN ML HHV GUN CONTROL OG IMMIGRATION MED LIBCON MM. ##################
#### JOINE DLIBCON OG DG TIL ET DATASET ####
DG <- full_join(DG, DLIBCON, by = c("WUSTLID", "month"))
DI <- left_join(DI, DLIBCON, by = c("WUSTLID", "month"))
DB <- left_join(DB, DLIBCON, by = c("WUSTLID", "month"))



########### RACE RELATIONS ############
#### NYT DATASET TIL AT KOBLE IDEOLOGI PAA MAANEDERNE ####
DRACEREL <- D %>%
  dplyr::select(WUSTLID, racerelation16, racerelation17)

#### Gather de to variable ####
DRACEREL$rr15nov <- DRACEREL$racerelation16
DRACEREL$rr16feb <- DRACEREL$racerelation16
DRACEREL$rr16jun <- DRACEREL$racerelation16
DRACEREL$rr16jul <- DRACEREL$racerelation16
DRACEREL$rr17mar <- DRACEREL$racerelation17
DRACEREL$rr17aug <- DRACEREL$racerelation17
DRACEREL$rr17oct <- DRACEREL$racerelation17
DRACEREL$rr17nov <- DRACEREL$racerelation17

DRACEREL$nov15 <- DRACEREL$rr15nov
DRACEREL$feb16 <- DRACEREL$rr16feb
DRACEREL$jun16 <- DRACEREL$rr16jun
DRACEREL$jul16 <- DRACEREL$rr16jul
DRACEREL$mar17 <- DRACEREL$rr17mar
DRACEREL$aug17 <- DRACEREL$rr17aug
DRACEREL$oct17 <- DRACEREL$rr17oct
DRACEREL$nov17 <- DRACEREL$rr17nov

DRACEREL = DRACEREL[,-c(2:11)]

DRACEREL = gather(DRACEREL, "month", "racerel", nov15, feb16, jun16, jul16, mar17, aug17, oct17, nov17)

#### SAMMENLAEGNING AF DATASAET ####
################## FULL JOIN ML HHV GUN CONTROL OG IMMIGRATION MED RACERELATIONS MM. ##################
#### JOINE DRACEREL OG DG TIL ET DATASET ####
DG <- full_join(DG, DRACEREL, by = c("WUSTLID", "month"))
DI <- left_join(DI, DRACEREL, by = c("WUSTLID", "month"))
DB <- left_join(DB, DRACEREL, by = c("WUSTLID", "month"))



########### SALIENS ############
#### NYT DATASET TIL AT KOBLE IDEOLOGI PAA MAANEDERNE ####
DSALI <- D %>%
  dplyr::select(WUSTLID, saliens15_nov, saliens16_jun, saliens16_aug, saliens17_apr, saliens17_jul, saliens17_nov)

#### Gather ####
DSALI$nov15 <- DSALI$saliens15_nov
DSALI$feb16 <- DSALI$saliens15_nov
DSALI$jun16 <- DSALI$saliens16_jun
DSALI$jul16 <- DSALI$saliens16_aug
DSALI$mar17 <- DSALI$saliens17_apr
DSALI$aug17 <- DSALI$saliens17_jul
DSALI$oct17 <- DSALI$saliens17_nov
DSALI$nov17 <- DSALI$saliens17_nov

DSALI = DSALI[,-c(2:7)]

DSALI = gather(DSALI, "month", "saliens", nov15, feb16, jun16, jul16, mar17, aug17, oct17, nov17)

################## FULL JOIN ML HHV GUN CONTROL OG IMMIGRATION MED LIBCON MM. ##################
#### JOINE DLIBCON OG DG TIL ET DATASET ####
DG <- full_join(DG, DSALI, by = c("WUSTLID", "month"))
DI <- left_join(DI, DSALI, by = c("WUSTLID", "month"))
DB <- left_join(DB, DSALI, by = c("WUSTLID", "month"))
