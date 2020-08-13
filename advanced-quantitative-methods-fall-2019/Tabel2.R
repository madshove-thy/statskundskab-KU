##### Tabel 2 #####

## Model 1 - first stage
first_stage_schooling <- felm(schooling ~ csl_16 + csl_g17 +trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                                trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                              |factor(state) + factor(surveyyear) + factor(yearat14)
                              |0
                              |state,
                              data = D2)

summary(first_stage_schooling)

D2$pred <- predict(first_stage_schooling) ## Warning message, no applicable method for predict applied to an object of class felm

diftabel2_1 <- summary(glht(first_stage_schooling, linfct = c("csl_g17-csl_16=0")))

## Model 2, tjek af IV effekt paa efter high school schooling
first_stage_grade <- felm(above ~ csl_16 + csl_g17 +trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                            trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                          |factor(state) + factor(surveyyear)+factor(yearat14)
                          |0
                          |state,
                          data = D2)

summary(first_stage_grade)

## Model 3, intend 2-stage
#Intend TS2sls - 16
-0.031/0.175
"-0.1771"
#Intend Ts2sls -17
-0.044/0.214
"-0.205"
#Intend mean
intend_mean <- (-0.1771-0.205)/2 ## -0,19105, difference paa ca. 0,004




###Diff-in-Diff
summary(glht(first_stage_schooling, linfct = c("csl_g17-csl_16=0")))
summary(glht(first_stage_grade, linfct = c("csl_g17-csl_16=0")))


##### GENERERING AF TABEL #####
stargazer(first_stage_schooling, first_stage_grade, 
          type = "latex",
          keep = c("csl_16", "csl_g17"),
          keep.stat = c( "n","f"),
          model.numbers = FALSE,
          title= c("Effekten af dropout lovgivning på uddannelseslængde og intentionen om at stemme demokratisk"),
          column.labels = c("First-stage","Second-stage"),
          column.separate = c(2,1),
          covariate.labels = c("Dropout lovgivning 16 år", "Droput lovgivning 17+ år"),
          add.lines = list(c("Forskel: Dropout lovgivning 17+ - Dropout lovgivning 16", "-0.022", "-0.006", "-0.006", "-0.013"),
                           c("FE Stat", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Tid", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Surveår", "Ja", "Ja", "Ja", "Ja"),
                           c("FE Stat trends", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


####Andet replikation


## Model 3, partisan 2-stage
#Partisan TS2sls - 16
-0.017/0.175
"-0,097"
#Partisan TS2sls - 17
-0.032/0.214
"-0,149"
#Partisan mean
(-0.097-0.149)/2 ## -0.123, difference paa ca. 0,020


## Model 5, voted 2-stage
#Vote Ts2sls -16
-0.014/0.175
"0.08"
#Vote Ts2sls -17
-0.033/0.214
"-0.1542"
#Vote mean
(-0.08-0.1542)/2 ## -0,1171, difference paa ca. 0,03

