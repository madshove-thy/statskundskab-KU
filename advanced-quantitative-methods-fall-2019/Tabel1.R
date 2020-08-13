##### TABEL 1 #####

### NAES(D1) og ACS (D2) data kan hentes paa: 
## https://dataverse.harvard.edu/dataset.xhtml;jsessionid=dba3e01363d806b1f27d425eabf8?persistentId=doi%3A10.7910%2FDVN%2FGPRVC6&version=&q=&fileTypeGroupFacet=&fileAccess=&fileSortField=size ####

#### INKREMENTEL TABEL 1 ####
inkre1 <-felm(presdem_intend_all ~ csl_16+csl_g17
                        |0 #fixed effects for stat, alder og hvilken surveyrunde
                        |0              # intet instrument
                        |state,                # standardfejl korrigeret ved stat
                        data = D1)

summary(inkre1)

inkre2 <-felm(presdem_intend_all ~ csl_16+csl_g17
                         | factor(state)#fixed effects for stat, alder og hvilken surveyrunde
                         |0              # intet instrument
                         |state,                # standardfejl korrigeret ved stat
                         data = D1)

summary(inkre2)


inkre3 <-felm(presdem_intend_all ~ csl_16+csl_g17
                         |factor(yearat14) #fixed effects for stat, alder og hvilken surveyrunde
                         |0              # intet instrument
                         |state,                # standardfejl korrigeret ved stat
                         data = D1)

summary(inkre3)

inkre4 <-felm(presdem_intend_all ~ csl_16+csl_g17
                         |factor(surveyyear) #fixed effects for stat, alder og hvilken surveyrunde
                         |0              # intet instrument
                         |state,                # standardfejl korrigeret ved stat
                         data = D1)

summary(inkre4)

inkre5 <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
              |0 #fixed effects for stat, alder og hvilken surveyrunde
              |0              # intet instrument
              |state,                # standardfejl korrigeret ved stat
              data = D1)

summary(inkre5)

inkre6 <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
              |factor(yearat14) + factor(state) + factor(surveyyear) #fixed effects for stat, alder og hvilken surveyrunde
              |0              # intet instrument
              |state,                # standardfejl korrigeret ved stat
              data = D1)

summary(inkre6)

### Diff-in-diff til tabel
summary(glht(inkre1, linfct = c("csl_g17-csl_16=0")))
summary(glht(inkre2, linfct = c("csl_g17-csl_16=0")))
summary(glht(inkre3, linfct = c("csl_g17-csl_16=0")))
summary(glht(inkre4, linfct = c("csl_g17-csl_16=0")))
summary(glht(inkre5, linfct = c("csl_g17-csl_16=0")))
summary(glht(inkre6, linfct = c("csl_g17-csl_16=0")))


#### Generering af tabel ####
stargazer(inkre1, inkre2, inkre3, inkre4, inkre5, inkre6,
          type = "latex",
          keep = c("csl_16", "csl_g17"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af dropout lovgivning på intentionen om at stemme demokratisk"),
          dep.var.labels = "Intention om at stemme demokratisk",
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år"),
          column.labels = c("Model I", "Model II", "Model III", "Model IV"),
          add.lines = list(c("Forskel: Dropout lovgivning 17+ - Dropout lovgivning 16", "-0.022", "-0.006", "-0.006", "-0.013"),
                           c("FE Stat", "Nej", "Ja", "Nej", "Nej", "Nej", "Ja"),
                           c("FE Tid", "Nej", "Nej", "Ja", "Nej", "Nej", "Ja"),
                           c("FE Surveår", "Nej", "Nej", "Nej", "Ja", "Nej", "Ja"),
                           c("FE Stat trends", "Nej", "Nej", "Nej", "Nej", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)


###### REPLIKERING AF TABEL 1 SOM DEN ER I MARSHALL ######
## Model 1, identifikation democrat
Demidentifikation_did <-felm(dem_all ~ csl_16 +csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                               trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49  
                             | factor(state) + factor(surveyyear)+factor(yearat14)     #fixed effects for stat, alder og hvilken surveyrunde                
                             |0 # intet instrument
                             |state, # standardfejl korrigeret ved stat
                             data = D1)
summary(Demidentifikation_did)

summary(glht(Demidentifikation_did, linfct = c("csl_g17-csl_16=0")))

## Model 2, intention democrat
Demintention_did <-felm(presdem_intend_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                          trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49  
                        | factor(state) + factor(surveyyear)+factor(yearat14) #fixed effects for stat, alder og hvilken surveyrunde
                        |0              # intet instrument
                        |state,                # standardfejl korrigeret ved stat
                        data = D1)

summary(Demintention_did)

summary(glht(Demintention_did, linfct = c("csl_g17-csl_16=0")))

demintention_dif <- (glht(Demintention_did, linfct = c("csl_g17-csl_16=0")))
summary(demintention_dif)
coef(demintention_dif)

## Model 3, Voted democrat
Demvote_did <-felm(presdem_all ~ csl_16+csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                     trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                   | factor(state) + factor(surveyyear)+factor(yearat14)
                   |0       
                   |state,   
                   data = D1)
summary(Demvote_did)

summary(glht(Demvote_did, linfct = c("csl_g17-csl_16=0")))

## Model 4, identification republican
Repidentifikation_did <-felm(rep_all ~ csl_16 +csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                               trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                             | factor(state) + factor(surveyyear)+factor(yearat14)                      
                             |0
                             |state,
                             data = D1)
summary(Repidentifikation_did)

summary(glht(Repidentifikation_did, linfct = c("csl_g17-csl_16=0")))

## Model 5, intention republican
Repintention_did <-felm(presrep_intend_all ~ csl_16 +csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                          trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                        | factor(state) + factor(surveyyear)+factor(yearat14)                       
                        |0
                        |state,
                        data = D1)
summary(Repintention_did)

summary(glht(Repintention_did, linfct = c("csl_g17-csl_16=0")))

## Model 6, voted republican
Repvote_did <-felm(presrep_all ~ csl_16 +csl_g17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                     trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                   | factor(state) + factor(surveyyear)+factor(yearat14)
                   |0
                   |state,
                   data = D1)
summary(Repvote_did)

summary(glht(Repvote_did, linfct = c("csl_g17-csl_16=0")))
