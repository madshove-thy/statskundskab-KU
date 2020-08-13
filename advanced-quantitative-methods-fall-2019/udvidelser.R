###### Udvidelser til paper ######

#### AFSNIT 3.1 ####
## Test for samme effekt med opdeling ml. 17 og 18 ##
## Recode
D1 <- D1 %>%
  mutate(drop17 = recode(drop_age, "17" = 1,
                         "12" = 0,
                         "14" = 0,
                         "15" = 0,
                         "16" = 0,
                         "18" = 0))

D1 <- D1 %>%
  mutate(drop18 = recode(drop_age, "18" = 1,
                         "12" = 0,
                         "14" = 0,
                         "15" = 0,
                         "16" = 0,
                         "17" = 0))

## Test
Demintention18 <-felm(presdem_intend_all ~ csl_16+drop17+drop18+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                        trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                      | factor(state) + factor(surveyyear)+factor(yearat14)
                      |0            
                      |state,               
                      data = D1)

summary(Demintention18)

## Dif-in-dif estimater
summary(glht(Demintention18, linfct = c("drop17-csl_16=0")))
summary(glht(Demintention18, linfct = c("drop18-csl_16=0")))
summary(glht(Demintention18, linfct = c("drop18-drop17=0")))



#### AFSNIT 3.2 ####
## Analyse af subgrupper ##
## Test af always-takers ##
# Genering af data
Male1 <- D1 %>%
  filter(male == 1)

Whitem1 <- Male1 %>%
  filter(black == 0)

Eduwm1 <- Whitem1 %>%
  filter(edu_cat > 2)

Eduall <- D1 %>%
  filter(edu_cat > 2)

Male2 <- D2 %>%
  filter(male == 1)

Whitem2 <- D2 %>%
  filter(black == 0)

Female1 <- D1 %>%
  filter(male == 0)

Female2 <- D2 %>%
  filter(male == 0)

Whitef1 <- Female1 %>%
  filter(black == 0)

Whitef2 <- Female2 %>%
  filter(black == 0)

Eduwf1 <- Whitef1 %>%
  filter(edu_cat > 2)

Eduwf2 <- Whitef2 %>%
  filter(above == 1)

## Effekt blandt always-takers test

## Hojt uddannede

# Reduced form
Demintentionalwaysedu <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                              trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                            | factor(state) + factor(surveyyear)+factor(yearat14)
                            |0        
                            |state,           
                            data = Eduall)
summary(Demintentionalwaysedu)

## Hvide Kvinder

# First-stage
alwayst_test_first_female<- lm(schooling ~ csl_16 + csl_g17 + factor(state) + factor(surveyyear)+factor(yearat14)+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                                 trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49, data = Whitef2)
summary(alwayst_test_first_female)

# Reduced form
Demintentionalwaysfem <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                               trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                             | factor(state) + factor(surveyyear)+factor(yearat14)
                             |0        
                             |state,           
                             data = Whitef1)
summary(Demintentionalwaysfem)

## Maend

#First-stage
alwayst_test_first_male<- lm(schooling ~ csl_16 + csl_g17 + factor(state) + factor(surveyyear)+factor(yearat14)+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                               trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49, data = Whitem2)
summary(alwayst_test_first_male)


# Reduced form
Demintentionalwaysmale <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                               trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                             | factor(state) + factor(surveyyear)+factor(yearat14)
                             |0        
                             |state,           
                             data = Whitem1)
summary(Demintentionalwaysmale)


#####REGIONSTEST ###########

Demintention_region<-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
              |factor(yearat14) + factor(state) + factor(surveyyear) + factor(yearat14):factor(region) #fixed effects for stat, alder og hvilken surveyrunde
              |0              # intet instrument
              |state,                # standardfejl korrigeret ved stat
              data = D1)

summary(Demintention_region)


##### TABEL TIL OPGAVE AFSNIT 3.1 #####
stargazer(inkre6, Demintention18,
          type = "text",
          keep.stat = c( "n", "rsq"),
          title= c("OLS - effekt af uddannelse på opbakning til demokraterne blandt always-takers"),
          dep.var.labels = "Intention om at stemme på demokraterne",
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år"),
          column.labels = c("Samlet model", "17+ opsplittet"),
          add.lines = list(c("Stat-fixed effects?", "Ja", "Ja"),
                           c("Tids-fixed effects?", "Ja", "Ja"),
                           c("Survey år fixed effects?", "Ja", "Ja"),
                           c("Trends?", "Ja", "Ja")),
          digits = 3)

summary(glht(Demintention18, linfct = c("drop17-csl_16=0")))
summary(glht(Demintention18, linfct = c("drop18-csl_16=0")))
summary(glht(Demintention18, linfct = c("drop18-drop17=0")))

##### LATEX 3.1 #####
stargazer(Demintention_ren3, Demintention18,
          type = "latex",
          keep = c("csl_16", "csl_g17", "drop17", "drop18"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af dropout lovgivning på intentionen om at stemme demokratisk"),
          dep.var.labels = "Intention om at stemme demokratisk",
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år", "Dropout lovgivning 17 år", "Dropout lovgivning 18 år"),
          column.labels = c("Klassisk", "Opdelt"),
          add.lines = list(c("Forskel: Dropout lovgivning 17+ - Dropout lovgivning 16", "-0.013"),
                           c("Forskel: Dropout lovgivning 17 - Dropout lovgivning 16", "", "-0.011"),
                           c("Forskel: Droput lovgivning 18 - Droput lovgivning 16", "", "-0.017"),
                           c("Forskel: Dropout lovgivning 18 - Dropout lovgivning 17", "", "-0.006"),
                           c("FE Stat", "Ja", "Ja"),
                           c("FE Tid", "Ja", "Ja"),
                           c("FE Surveår", "Ja", "Ja"),
                           c("FE Stat trends", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n")


##### TABEL TIL OPGAVE AFSNIT 3.2 #####

summary(glht(alwayst_test_first_female, linfct = c("csl_g17-csl_16=0")))
summary(glht(alwayst_test_first_male, linfct = c("csl_g17-csl_16=0")))
summary(glht(emintentionalwaysfem, linfct = c("csl_g17-csl_16=0")))
summary(glht(Demintentionalwaysmale, linfct = c("csl_g17-csl_16=0")))
summary(glht(Demintentionalwaysedu, linfct = c("csl_g17-csl_16=0")))


stargazer(alwayst_test_first_female, alwayst_test_first_male, Demintentionalwaysfem,Demintentionalwaysmale, Demintentionalwaysedu,
          type = "text",
          keep.stat = c( "n", "rsq"),
          title= c("OLS - effekt af uddannelse på opbakning til demokraterne blandt always-takers"),
          dep.var.labels = "Intention om at stemme på demokraterne",
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år"),
          column.labels = c("Kvinder first-stage", "Kvinder reduced form", "Højt uddannede reduced form"),
          add.lines = list(c("FE Stat", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Tid", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Surveår", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Stat trends", "Ja", "Ja", "Ja", "Ja", "Ja")),
          digits = 3)

##### LATEX 3.2 #####

stargazer(alwayst_test_first_female, alwayst_test_first_male, Demintentionalwaysfem,Demintentionalwaysmale, Demintentionalwaysedu, 
          type = "latex",
          keep = c("csl_16", "csl_g17"),
          keep.stat = c( "n","f", "wald"),
          model.numbers = FALSE,
          title= c("Effekten af dropout lovgivning på uddannelseslængde og intentionen om at stemme demokratisk"),
          column.labels = c("First-stage","Second-stage"),
          column.separate = c(2,3),
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år"),
          add.lines = list(c("FE Stat", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Tid", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Surveår", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Stat trends", "Ja", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


##### LATEX 4.3 ######

summary(glht(Demintention_ren3, linfct = c("csl_g17-csl_16=0")))
summary(glht(Demintention_region, linfct = c("csl_g17-csl_16=0")))


stargazer(inkre6, Demintention_region,
          type = "latex",
          keep = c("csl_16", "csl_g17"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af dropout lovgivning på intentionen om at stemme demokratisk"),
          dep.var.labels = "Intention om at stemme demokratisk",
          covariate.labels = c("DOL16", "DOL17+"),
          column.labels = c("Oprindelig", "Kontrol Region"),
          add.lines = list(c("Forskel: DOL17+ - DOL16", "-0.022", "-0.006"),
                           c("FE Stat", "Ja", "Ja"),
                           c("FE Tid", "Ja", "Ja"),
                           c("FE Surveår", "Ja", "Ja"),
                           c("FE Stat trends", "Ja", "Ja"),
                           c("FE Tid X Region", "Nej", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-dc-ta-s=n")



