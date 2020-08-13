###### DESKRIPTIV STATISTIK #####
mean(DG$ppgender) # 47% maend
table(DG$ppeducat) # Klart flest hojtuddannede, 2 % less than highschool, 12% highschool, 30% some college, 56% college eller mere
table(DG$libcon)

###### TABEL 1 #######
library("survival")


#### Model 1 og model 2####
## Lav dataset for Orlando
GUN16 <- DG %>%
  drop_na(imm16_may, imm16_jul, imm17_aug, imm17_okt, gun) %>%
  filter(treatmentsted == 1)

## Regression u. kontrol
guno1 <- felm(gun ~ treatmenttid
             |factor(WUSTLID) # Fixed effects for enheder
             |0 # Ingen instrumentel variabel
             |ppstate, # Clustering paa stat
             data = GUN16) # Dataset til gun control for Orlando

summary(guno1)

## Regression m. kontrol
guno2 <- felm(gun ~ treatmenttid + libcon
              |factor(WUSTLID) # Fixed effects for enheder
              |0 # Ingen instrumentel variabel
              |ppstate, # Clustering paa stat
              data = GUN16) # Dataset til gun control for Orlando

summary(guno2)


#### Model 2 og model 4 ####
## Lav dataset for Las Vegas
GUN17 <- DG %>%
  drop_na(imm16_may, imm16_jul, imm17_aug, imm17_okt, gun) %>%
  filter(treatmentsted == 0)

# Regression u. kontrol
gunl1 <- felm(gun ~ treatmenttid
             |factor(WUSTLID)# Fixed effects for enheder
             |0 # Ingen instrumentel variabel
             |ppstate, # Clustering paa stat
             data = GUN17) # Dataset til gun control for Orlando

summary(gunl1)

# Regression m. kontrol
gunl2 <- felm(gun ~ treatmenttid + libcon 
              |factor(WUSTLID)# Fixed effects for enheder
              |0 # Ingen instrumentel variabel
              |ppstate, # Clustering paa stat
              data = GUN17) # Dataset til gun control for Orlando

summary(gunl2)

####### Latex #######
stargazer(guno1, guno2, gunl1, gunl2,
          type = "latex",
          keep = c("treatmenttid", "libcon"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning"),
          dep.var.labels = "Intention om at stemme demokratisk",
          covariate.labels = c("Tid", "Ideologisk selvplacering"),
          column.labels = c("Orlando", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


##### TABEL 2 ####
## Model 1 og model 2 og model 3 ##
DGdid <- DG %>%
  drop_na(imm16_may, imm16_jul, imm17_aug, imm17_okt,gun)

gun_did_1 <- felm(gun ~ treatmenttid*treatmentsted
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DGdid) # Dataset til gun control DG
summary(gun_did_1)

gun_did_2 <- felm(gun ~ treatmenttid*treatmentsted + libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DGdid) # Dataset til gun control DG
summary(gun_did_2)

gun_did_3 <- felm(gun ~ treatmenttid*treatmentsted + libcon + saliens + racerel
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DGdid) # Dataset til gun control DG
summary(gun_did_3)

## Model 4 og model 5 og model 6 ##
DIdid <- DI %>%
  drop_na(gun15_nov, gun16_feb, gun16_jun, gun16_jul, gun17_mar, gun17_aug, gun17_oct, gun17_nov1, immigration)


imm_did_1 <- felm(immigration ~ treatmenttid*treatmentsted
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DIdid) # Dataset til gun control DG
summary(imm_did_1)

imm_did_2 <- felm(immigration ~ treatmenttid*treatmentsted + libcon
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DIdid) # Dataset til gun control DG
summary(imm_did_2)

imm_did_3 <- felm(immigration ~ treatmenttid*treatmentsted + libcon + racerel + saliens
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DIdid) # Dataset til gun control DG
summary(imm_did_3)

######LATEX#######

stargazer(gun_did_1, gun_did_2, gun_did_3, imm_did_1, imm_did_2, imm_did_3,
          type = "latex",
          keep = c("treatmenttid:treatmentsted", "libcon", "racerel", "saliens"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = "Differences-in-difference regressioner",
          covariate.labels = c("Ideologisk selvplacering", "Forhold mellem racer", "Saliens", "DID-estimat"),
          column.labels = c("Holdning til v?benlovgivning", "Holdning til immigration"),
          column.separate= c(3,3),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"), 
                           c("Kontrol for saliens", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("Kontrol for relation mellem etniske grupper", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


########Autoritarisme#########

####V?ben####
bauth_gun_did <- felm(gun ~ treatmenttid*treatmentsted*indeksauthb + libcon + saliens + racerel
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DGdid) # Dataset til gun control DG

summary(bauth_gun_did)

tauth_gun_did <- felm(gun ~ treatmenttid*treatmentsted*indeksautht + libcon + saliens + racerel
                      |factor(WUSTLID) # Fixed effects for enheder
                      |0 # Ingen instrumentel variabel
                      |ppstate, # Clustering paa stat
                      data = DGdid) # Dataset til gun control DG
summary(tauth_gun_did)

####Immigration####

bauth_imm_did <- felm(immigration ~ treatmenttid*treatmentsted*indeksauthb + libcon + racerel + saliens
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DIdid) # Dataset til gun control DG
summary(bauth_imm_did)


tauth_imm_did <- felm(immigration ~ treatmenttid*treatmentsted*indeksautht + libcon + racerel + saliens
                      |factor(WUSTLID) # Fixed effects for enheder
                      |0 # Ingen instrumentel variabel
                      |ppstate, # Clustering paa stat
                      data = DIdid) # Dataset til gun control DG
summary(tauth_imm_did)

####LATEX#####

stargazer(bauth_gun_did, tauth_gun_did, bauth_imm_did, tauth_imm_did,
          type = "latex",
          keep = c("treatmenttid:treatmentsted:indeksauthb","treatmenttid:treatmentsted:indeksautht","libcon", "racerel", "saliens"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("V?benlovgivning", "Immigration"),
          covariate.labels = c("Ideologisk selvplacering", "Forhold mellem racer", "Saliens", "Interaktionsestimat", "Interaktionsestimat"),
          column.labels = c("Holdning til v?benlovgivning", "Holdning til immigration"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)




######### ROBUSTHEDSAFSNIT ##########
#####################################

#### IMMIGRATION MED GOOD/BAD SOM AFHAENGIG VARIABEL ####
GOOD16 <- DB %>%
  drop_na(imm16_may, imm16_jul, imm17_aug, imm17_okt, gun15_nov, gun16_feb, gun16_jun, gun16_jul, gun17_mar, gun17_aug, gun17_oct, gun17_nov1)

#### ORLANDO ####
imgood1 <- felm(immgood ~ treatmenttid
                |factor(WUSTLID)# Fixed effects for enheder
                |0 # Ingen instrumentel variabel
                |ppstate, # Clustering paa stat
                data = GOOD16) # Dataset til gun control for Orlando
summary(imgood1)

imgood2 <- felm(immgood ~ treatmenttid + libcon
                |factor(WUSTLID)# Fixed effects for enheder
                |0 # Ingen instrumentel variabel
                |ppstate, # Clustering paa stat
                data = GOOD16) # Dataset til gun control for Orlando
summary(imgood2)


stargazer(imgood1, imgood2,
          type = "latex",
          keep = c("treatmenttid:treatmentsted","libcon", "racerel", "saliens", "treatmenttid"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("Mur", "Immigration"),
          covariate.labels = c("Tid", "Ideologisk selvplacering", "Forhold mellem racer", "Saliens", "DID-estimat"),
          column.labels = c("Orlando", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)

###########Ideologi ############

libgun_did <- felm(gun ~ treatmenttid*treatmentsted*libcon + saliens + racerel
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DGdid) # Dataset til gun control DG
summary(libgun_did)

libguno <- felm(gun ~ treatmenttid*libcon
              |factor(WUSTLID) # Fixed effects for enheder
              |0 # Ingen instrumentel variabel
              |ppstate, # Clustering paa stat
              data = GUN16) # Dataset til gun control for Orlando

summary(libguno)


libgunl <- felm(gun ~ treatmenttid*libcon
                |factor(WUSTLID) # Fixed effects for enheder
                |0 # Ingen instrumentel variabel
                |ppstate, # Clustering paa stat
                data = GUN17) # Dataset til gun control for Orlando
summary(libgunl)

libimm_did <- felm(immigration ~ treatmenttid*treatmentsted*libcon + racerel + saliens
                  |factor(WUSTLID) # Fixed effects for enheder
                  |0 # Ingen instrumentel variabel
                  |ppstate, # Clustering paa stat
                  data = DIdid) # Dataset til gun control DG
summary(libimm_did)

stargazer(libguno, libgunl, libgun_did, libimm_did,
          type = "latex",
          keep = c("treatmenttid", "racerel", "saliens","treatmenttid:libcon","treatmenttid:treatmentsted:libcon"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("V?benlovgivning", "Immigration"),
          covariate.labels = c("Tid", "Forhold mellem racer", "Saliens", "Tid X Ideologi", "", "DID X Ideologi"),
          column.labels = c("Simpel", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)




#####BILAG 1 ######
### LOGISTISK REGRESSION ###

##Orlando

logguno <- clogit(gun ~ treatmenttid + strata(WUSTLID), data = GUN16)
summary(logguno)

## Vegas
loggunl <- clogit(gun ~ treatmenttid + strata(WUSTLID), data = GUN17)
summary(loggunl)


##DID

##V?ben
gunlogit1 <- clogit(gun ~ treatmenttid*treatmentsted + libcon + strata(WUSTLID), data = DGdid)
summary(gunlogit1)

##Immigration

immlogit1 <- clogit(immigration ~ treatmenttid*treatmentsted + strata(WUSTLID), data =DIdid)
summary(immlogit1)

stargazer(logguno, loggunl, gunlogit1, gunlogit1,
          type = "latex",
          keep = c("treatmenttid","treatmenttid:treatmentsted"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("V?benlovgivning", "Immigration"),
          covariate.labels = c("Tid", "DID estimat"),
          column.labels = c("Simpel", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)


#####BILAG 2 ######

stargazer(gun_did_1, gun_did_2, gun_did_3, imm_did_1, imm_did_2, imm_did_3,
          type = "latex",
          keep = c("treatmenttid", "treatmentsted", "treatmenttid:treatmentsted", "libcon", "racerel", "saliens"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = "Differences-in-difference regressioner",
          covariate.labels = c("Ideologisk selvplacering", "Forhold mellem racer", "Saliens", "DID-estimat"),
          column.labels = c("Holdning til v?benlovgivning", "Holdning til immigration"),
          column.separate= c(3,3),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"), 
                           c("Kontrol for saliens", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja"),
                           c("Kontrol for relation mellem etniske grupper", "Ja", "Ja", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


stargazer(bauth_gun_did, tauth_gun_did, bauth_imm_did, tauth_imm_did,
          type = "latex",
          keep = c("treatmenttid", "treatmentsted", "treatmenttid:treatmentsted", "treatmenttid:indeksauthb", "treatmentsted:indeksauthb", "treatmenttid:indeksautht", "treatmentsted:indeksautht", "treatmenttid:treatmentsted:indeksauthb","treatmenttid:treatmentsted:indeksautht","libcon", "racerel", "saliens"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("V?benlovgivning", "Immigration"),
          column.labels = c("Holdning til v?benlovgivning", "Holdning til immigration"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-d#-ta-s=n",
          align = TRUE)

##### BILAG 3

bauth_guno <- felm(gun ~ treatmenttid*indeksauthb+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = GUN16) # Dataset til gun control for Orlando

summary(bauth_guno)

tauth_guno <- felm(gun ~ treatmenttid*indeksautht+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = GUN16) # Dataset til gun control for Orlando

summary(tauth_guno)


bauth_gunl <- felm(gun ~ treatmenttid*indeksauthb+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = GUN17) # Dataset til gun control for Orlando

summary(bauth_gunl)

tauth_gunl <- felm(gun ~ treatmenttid*indeksautht+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = GUN17) # Dataset til gun control for Orlando

summary(tauth_gunl)




#####LATEX########

stargazer(bauth_guno, tauth_guno, bauth_gunl, tauth_gunl,
          type = "latex",
          keep = c("treatmenttid", "treatmenttid:indeksauthb","treatmenttid:indeksautht","libcon"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("Orlando", "Las Vegas"),
          column.labels = c("Orlando", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)

#### Immigration
DI16 <- DIdid %>%
filter(treatmentsted == 1)

bauth_immo <- felm(immigration ~ treatmenttid*indeksauthb+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DI16) # Dataset til gun control for Orlando

summary(bauth_immo)

tauth_immo <- felm(immigration ~ treatmenttid*indeksautht+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DI16) # Dataset til gun control for Orlando

summary(tauth_immo)

DI17 <- DIdid %>%
  filter(treatmentsted == 0)


bauth_immol <- felm(immigration ~ treatmenttid*indeksauthb+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DI17) # Dataset til gun control for Orlando

summary(bauth_immol)

tauth_immol <- felm(immigration ~ treatmenttid*indeksautht+libcon
                   |factor(WUSTLID) # Fixed effects for enheder
                   |0 # Ingen instrumentel variabel
                   |ppstate, # Clustering paa stat
                   data = DI17) # Dataset til gun control for Orlando

summary(tauth_immol)




#####LATEX########

stargazer(bauth_immo, tauth_immo, bauth_immol, tauth_immol,
          type = "latex",
          keep = c("treatmenttid","treatmenttid:indeksauthb","treatmenttid:indeksautht","libcon"),
          keep.stat = c( "n"),
          model.numbers = FALSE,
          title= c("Effekten af et masseskyderi p? holdningen til v?benlovgivning og immigration"),
          dep.var.labels = c("Orlando", "Las Vegas"),
          column.labels = c("Orlando", "Las Vegas"),
          column.separate= c(2,2),
          add.lines = list(c("FE Enhed", "Ja", "Ja", "Ja", "Ja")),
          digits = 3,
          style = "apsr",
          report = c("vcs*"),
          no.space = TRUE,
          table.layout ="-c#-ta-s=n",
          align = TRUE)


