##########################
#### VISUALISERINGER #####
##########################

################################################################
###### VISUALISERING AF HOLDNING TIL GUN CONTROL OVER TID ######
################################################################
DV15 <- D15D %>%
  select(WUSTLID,
         ISSUESA4S48) # november

DV16 <- D16D %>%
  select(WUSTLID,
         GUNS1S51, ISSUESA4GS55, GUNS1S56, ISSUESA4GS57, ISSUESGA4S59, ISSUESGA4S60) # Februar, juni, juli, august og oktober

DV17 <- D17 %>%
  select(ISSUESA4S62, ISSUESA4S63, ISSUES4S67, ISSUES4S68, ISSUES4S69, # Februar, marts, august, oktober, november
         GUNS13S69, # gun control type 2 i november
         WUSTLID) 

DV1 <- full_join(DV15, DV16, by = "WUSTLID")
DV <- full_join(DV1, DV17, by = "WUSTLID")
DVI <- inner_join(DV, B99, by = "WUSTLID")

###Omkodning

#### GUN CONTROL ####

## Pre Orlando november 2015

class(DVI$ISSUESA4S48)
summary(DVI$ISSUESA4S48)
head(DVI$ISSUESA4S48)


DVI <- DVI %>%
  mutate(gun15_nov_mis = na_if(ISSUESA4S48, "1"))

summary(D$gun15_nov_mis)

DVI <- DVI %>%
  mutate(gun15_nov = recode(gun15_nov_mis, "2" = 1,
                            "3" = 0,
                            "4" = 0))


## ORLANDO juni sporgsmaal TYPE 1 ##
class(DVI$ISSUESA4GS55)
summary(DVI$ISSUESA4GS55)
head(DVI$ISSUESA4GS55)

DVI <- DVI %>%
  mutate(gun16_jun = recode(ISSUESA4GS55, "support" = 1,
                            "oppose" = 0,
                            "no opinion" = 0))

summary(DVI$gun16_jun)

## ORLANDO august sporsmaal TYPE 1 ##
summary(DVI$ISSUESA4GS57)

DVI <- DVI %>%
  mutate(gun16_aug = recode(ISSUESA4GS57, "support" = 1,
                            "oppose" = 0,
                            "no opinion" = 0))

summary(DVI$gun16_aug)

## ORLANDO februar sporgsmaal TYPE 2 ##
class(DVI$GUNS1S51)
summary(DVI$GUNS1S51)
head(DVI$GUNS1S51)

DVI <- DVI %>%
  mutate(gun16_feb = recode(GUNS1S51, "should be more strict" = 1,
                            "should be less strict" = 0,
                            "should be kept as they are now" = 0,
                            "don't know" = 0))

summary(DVI$gun16_feb)

## ORLANDO juli sporgsmaal TYPE 2 ##
class(D$GUNS1S56)
summary(D$GUNS1S56)
head(D$GUNS1S56)

DVI <- DVI %>%
  mutate(gun16_jul = recode(GUNS1S56, "should be more strict" = 1,
                            "should be less strict" = 0,
                            "should be kept as they are now" = 0,
                            "don't know" = 0))

summary(DVI$gun16_jul)


##Orlando oktober##

summary(DVI$ISSUESGA4S60)

DVI <- DVI %>%
  mutate(gun16_okt = recode(DVI$ISSUESGA4S60, "support" = 1,
                            "oppose" = 0,
                            "not sure" = 0))
summary(DVI$gun16_okt)

label_browser_static(DVI)

##Las Vegas februar##

summary(DVI$ISSUESA4S62)
class(DVI$ISSUESA4S62)

DVI <- DVI %>%
  mutate(gun17_feb_mis = na_if(ISSUESA4S62, "-1"))

DVI <- DVI %>%
  mutate(gun17_feb = recode(gun17_feb_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))
summary(DVI$gun17_feb)

##Las Vegas Marts ##

DVI <- DVI %>%
  mutate(gun17_mar_mis = na_if(ISSUESA4S63, "-1"))

summary(D$gun17_mar_mis)

DVI <- DVI %>%
  mutate(gun17_mar = recode(gun17_mar_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))
summary(D$gun17_mar)



## Las Vegas august ##

# Fjerne refused answers
DVI <- DVI %>%
  mutate(gun17_aug_mis = na_if(ISSUES4S67, "-1"))

summary(DVI$gun17_aug_mis)

DVI <- DVI %>%
  mutate(gun17_aug = recode(gun17_aug_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(DVI$gun17_aug)

## Las Vegas october ##

# Remove refused answers
DVI <- DVI %>%
  mutate(gun17_oct_mis = na_if(ISSUES4S68, "-1"))

summary(DVI$gun17_oct_mis)

DVI <- DVI %>%
  mutate(gun17_oct = recode(gun17_oct_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(DVI$gun17_oct)

## Las Vegas november sporgsmaal TYPE 1 ##


# Remove refused answers
DVI <- DVI %>%
  mutate(gun17_nov_mis = na_if(ISSUES4S69, "-1"))

summary(DVI$gun17_nov_mis)

DVI <- DVI %>%
  mutate(gun17_nov = recode(gun17_nov_mis, "1" = 1,
                            "2" = 0,
                            "3" = 0))

summary(DVI$gun17_nov)

## Las Vegas november sporgsmaal TYPE 2 ##

# Remove refused answers
DVI <- DVI %>%
  mutate(gun17_nov1_mis = na_if(GUNS13S69, "-1"))

summary(DVI$gun17_nov1_mis)

DVI <- DVI %>%
  mutate(gun17_nov1 = recode(gun17_nov1_mis, "1" = 1,
                             "2" = 0,
                             "3" = 0))

summary(DVI$gun17_nov1)


DVI2 <- DVI %>%
  filter(RACE1SP == 1)

####Checking values ###
summary(DVI2$gun15_nov)
summary(DVI2$gun16_feb)
summary(DVI2$gun16_jun)
summary(DVI2$gun16_jul)
summary(DVI2$gun16_aug)
summary(DVI2$gun16_okt)
summary(DVI2$gun17_feb)
summary(DVI2$gun17_mar)
summary(DVI2$gun17_aug)
summary(DVI2$gun17_oct)
summary(DVI2$gun17_nov1)


DVI2 <- gather(DVI2, "Q", "gun", gun15_nov, gun16_feb, gun16_jun, gun16_jul, gun16_aug, gun16_okt, gun17_feb, gun17_mar, gun17_aug, gun17_oct, gun17_nov1)

DVI2 <- DVI2 %>%
  mutate(month = recode(Q, "gun15_nov" = 0,
                        "gun16_feb" = 3,
                        "gun16_jun" = 7,
                        "gun16_jul" = 8,
                        "gun16_aug" = 9,
                        "gun16_okt" =11,
                        "gun17_feb" = 15,
                        "gun17_mar" = 16,
                        "gun17_aug" = 21,
                        "gun17_oct" = 23,
                        "gun17_nov1" = 23))

table(DVI2$month)

graf <- DVI2 %>%
  na.omit(gun) %>%
  group_by(month) %>%
  summarise(gns = mean(gun))

udvikling <- graf %>%  
  ggplot(aes(x = month, y = gns)) +
  geom_line(alpha = 0.7) +
  geom_point() +
  geom_vline(xintercept = c(7), linetype = "dotted") +
  scale_x_continuous(breaks = c(0, 3, 7, 8, 9, 11, 15, 16, 21, 22),
                     labels = c("Nov 2015",
                                "Feb 2016",
                                "Jun 2016",
                                "Jul 2016",
                                "Aug 2016",
                                "Okt 2016",
                                "Feb 2017",
                                "Mar 2017",
                                "Aug 2017",
                                "Post Las Vegas")) +
  ylim(c(0.45, 0.60)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), plot.title = element_text(hjust = 0.5))

udvikling


udvikling <- graf %>%  
  ggplot(aes(x = month, y = gns)) +
  geom_line(alpha = 0.7) +
  geom_point() +
  geom_vline(xintercept = c(7), linetype = "dotted") +
  scale_x_continuous(breaks = c(0, 3, 7, 8, 9, 11, 15, 16, 21, 22),
                     labels = c("Nov 2015",
                                "Feb 2016",
                                "Jun 2016",
                                "Jul 2016",
                                "Aug 2016",
                                "Okt 2016",
                                "Feb 2017",
                                "Mar 2017",
                                "Aug 2017",
                                "Post Las Vegas")) +
  ylim(c(0.45, 0.60)) +
  theme_minimal() + ylab("Andel opbakning til mere restriktiv våbenlovgivning")+xlab("Tid")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), plot.title = element_text(hjust = 0.5))

pdf("/Users/Madshove/Desktop/udvikling.pdf", 10, 7)
udvikling
dev.off()



graf1 <- DG %>%
  drop_na(gun, imm16_may, imm16_jul, imm17_aug, imm17_okt) %>%
  group_by(treatmentsted, treatmenttid) %>%
  summarise(gns = mean(gun))

udvikling1 <- graf1 %>% 
  ggplot(aes(x= treatmenttid, y = gns, group = as.factor(treatmentsted), color = as.factor(treatmentsted))) + 
  geom_line() + geom_point() + 
  scale_x_continuous(breaks = c(0, 1), labels = c("Før", "Efter")) + 
  ylim(c(0.48, 0.62)) + scale_color_discrete(name = "Sted", labels = c("Las Vegas", "Orlando")) + 
  theme_minimal() + ylab("Andel opbakning til mere restriktiv våbenlovgivning")+xlab("Tid")

pdf("/Users/Madshove/Desktop/udvikling1.pdf", 7, 7)
udvikling1
dev.off()

## Immigration ## 
graf2 <- DI %>% na.omit(immigration) %>% group_by(treatmentsted, treatmenttid) %>% summarise(gns = mean(immigration)) 

udvikling2 <- graf2 %>% ggplot(aes(x= treatmenttid, y = gns, group = as.factor(treatmentsted), color = as.factor(treatmentsted))) + 
  geom_line() + 
  geom_point()+ 
  scale_x_continuous(breaks = c(0, 1), labels = c("Før", "Efter")) + 
  ylim(c(1.5, 2)) + 
  scale_color_discrete(name = "Sted", labels = c("Las Vegas", "Orlando")) + 
  theme_minimal()+
  xlab("Tid")+
  ylab("Holdning til immigration") + 
  theme(plot.title = element_text(color="black", size = "10", face="italic" ))

pdf("/Users/Madshove/Desktop/udvikling2.pdf", 7, 7)
udvikling2
dev.off()

pdf("/Users/Madshove/Desktop/udvikling3.pdf", 14, 7)
grid.arrange(udvikling1, udvikling2, ncol = 2)
dev.off()



