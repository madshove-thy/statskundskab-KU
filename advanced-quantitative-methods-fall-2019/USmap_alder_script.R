#### USA MAP ####
library("usmap") # Til USA map plot
library("ggplot2") # Til plot

## CREATE DATA FRAME ##
?plot_usmap

label_browser_static(D1)

D1 <- D1 %>%
  mutate(state_map = recode(state, "1" = "AL",
                            "2" = "AR",
                            "3" = "AZ",
                            "4" = "CA",
                            "5" = "CO",
                            "6" = "CT",
                            "7" = "DC",
                            "8" = "DE",
                            "9" = "FL",
                              "10" = "GA",
                              "11" = "IA",
                              "12" = "ID",
                              "13" = "IL",
                              "14" = "IN",
                              "15" = "KS",
                              "16" = "KY",
                              "17" = "LA",
                              "18" = "MA",
                              "19" = "MD",
                              "20" = "ME",
                              "21" = "MI",
                              "22" = "MN",
                              "23" = "MO",
                              "24" = "MS",
                              "25" = "MT",
                              "26" = "NC",
                              "27" = "ND",
                              "28" = "NE",
                              "29" = "NH",
                              "30" = "NJ",
                              "31" = "NM",
                              "32" = "NV",
                              "33" = "NY",
                              "34" = "OH",
                              "35" = "OK",
                              "36" = "OR",
                              "37" = "PA",
                              "38" = "RI",
                              "39" = "SC",
                              "40" = "SD",
                              "41" = "TN",
                              "42" = "TX",
                              "43" = "UT",
                              "44" = "VA",
                              "45" = "VT",
                              "46" = "WA",
                              "47" = "WI",
                              "48" = "WV",
                              "49" = "WY"))

table(D1$state_map)

table(D1$drop_age)

D1 <- D1 %>%
  mutate(dropagebase = recode(drop_age, "12" = 1,
                              "14" = 1,
                              "15" = 1,
                              "16" = 0,
                              "17" = 0,
                              "18" = 0))

D1 <- D1 %>%
  mutate(dropage16 = recode(drop_age, "12" = 0,
                              "14" = 0,
                              "15" = 0,
                              "16" = 1,
                              "17" = 0,
                              "18" = 0))

D1 <- D1 %>%
  mutate(dropage17 = recode(drop_age, "12" = 0,
                            "14" = 0,
                            "15" = 0,
                            "16" = 0,
                            "17" = 1,
                            "18" = 1))

table(D1$dropagebase)
table(D1$dropage16)
table(D1$dropage17)

data_map1 <- D1 %>%
  dplyr::select(state_map, dropagebase)

data_map16 <- D1 %>%
  dplyr::select(state_map, dropage16)

data_map17 <- D1 %>%
  dplyr::select(state_map, dropage17)

# Rename state variable
data_map1 <- data_map1 %>%
  rename(state = state_map)

data_map16 <- data_map16 %>%
  rename(state = state_map)

data_map17 <- data_map17 %>%
  rename(state = state_map)

#Group by state
data_map1 <- data_map1 %>%
  group_by(state) %>%
  summarise(antal = sum(dropagebase))

data_map16 <- data_map16 %>%
  group_by(state) %>%
  summarise(antal = sum(dropage16))

data_map17 <- data_map17 %>%
  group_by(state) %>%
  summarise(antal = sum(dropage17))

# US map plot baselinegroup
usmapbaseline <- plot_usmap(data = data_map1, values = "antal", exclude = c("AK", "HI")) +
  scale_fill_continuous(low = "white", high = "red", name = "Antal observationer", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Geografisk fordeling af DOL12-15", subtitle = "Obligatorisk skolegang 12-15 år")

pdf("/Users/Madshove/Desktop/USmap_baseline.pdf", 7, 4)
usmapbaseline
dev.off()

# US map plot control group
usmapkontrol <- plot_usmap(data = data_map16, values = "antal", exclude = c("AK", "HI")) +
  scale_fill_continuous(low = "white", high = "red", name = "Antal observationer", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Geografisk fordeling af DOL16", subtitle = "Obligatorisk skolegang 16 år")

pdf("/Users/Madshove/Desktop/USmap_kontrol.pdf", 7, 4)
usmapkontrol
dev.off()

# US map plot treatment group
usmaptreatment <- plot_usmap(data = data_map17, values = "antal", exclude = c("AK", "HI")) +
  scale_fill_continuous(low = "white", high = "red", name = "Antal observationer", label = scales::comma) +
  theme(legend.position = "right") +
  labs(title = "Geografisk fordeling af DOL17+", subtitle = "Obligatorisk skolegang 17-18 år")

pdf("/Users/Madshove/Desktop/USmap_treatment.pdf", 7, 4)
usmaptreatment
dev.off()

# Kombineret US map plot
pdf("/Users/Madshove/Desktop/USmap_samlet.pdf", 10, 6)
grid.arrange(usmapbaseline, usmapkontrol, usmaptreatment, ncol = 2)
dev.off()


#### BAR PLOTS OVER ALDERSFORDELING ####
## Omkodning
D1 <- D1 %>%
  mutate(alderskat = case_when(between(yearat14, 1920, 1929) ~ 1,
                             between(yearat14, 1930, 1939) ~ 2,
                             between(yearat14, 1940, 1949) ~ 3,
                             between(yearat14, 1950, 1959) ~ 4,
                             between(yearat14, 1960, 1969) ~ 5,
                             between(yearat14, 1970, 1979) ~ 6,
                             between(yearat14, 1980, 1989) ~ 7,
                             between(yearat14, 1990, 1999) ~ 8))

table(D1$alderskat)
summary(D1$alderskat)

## Tjek af median
U16 <- D1 %>%
  dplyr::select(csl_16, csl_g17, state, yearat14, surveyyear, presdem_intend_all, naes_id, trend_2, trend_3, trend_4, trend_5, trend_6, trend_7, trend_8, trend_9, trend_10, trend_11, trend_12,trend_13, trend_14, trend_15, trend_16, trend_17, trend_18, trend_19, trend_20,
                       trend_21, trend_22, trend_23, trend_24, trend_25, trend_26, trend_27, trend_28, trend_29, trend_30, trend_31, trend_32, trend_33, trend_34, trend_35, trend_36, trend_37, trend_38, trend_39, trend_40, trend_41, trend_42, trend_43, trend_44, trend_45, trend_46, trend_47, trend_48, trend_49)

U16o <- na.omit(U16)
  
filter(drop_age == 16)

U17 <- D1 %>%
  filter(drop_age > 16)

U12 <- D1 %>%
  filter(drop_age < 16)

summary(U16$yearat14)
summary(U17$yearat14)
summary(U12$yearat14)


## Barplot DOL16
bar_age16 <- D1 %>%
  filter(csl_16 == 1) %>%
  ggplot(aes(x = alderskat)) +
  geom_bar(alpha = 0.7, fill = "darkred") +
  geom_text(aes(label = sprintf("%.02f %%", stat(prop)*100), group = 1), stat = 'count', 
            position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  geom_vline(xintercept = c(5), linetype = "dotted") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("1920-1929",
                                "1930-1939",
                                "1940-1949",
                                "1950-1959",
                                "1960-1969",
                                "1970-1979",
                                "1980-1989",
                                "1990-1999")) +
  ylim(c(0, 31000)) +
  labs(title = "DOL16 - Årti hvor respondenten er 14 år", x = "Årti - stiplet linje angiver median", y = "Antal observationer") +
  theme(legend.position = "top") +
  theme_classic()

bar_age16

## Barplot DOL17+
bar_age17 <- D1 %>%
  filter(csl_g17 == 1) %>%
  ggplot(aes(x = alderskat)) +
  geom_bar(alpha = 0.7, fill = "darkred") +
  geom_text(aes(label = sprintf("%.02f %%", stat(prop)*100), group = 1), stat = 'count', 
            position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  geom_vline(xintercept = c(6), linetype = "dotted") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("1920-1929",
                                "1930-1939",
                                "1940-1949",
                                "1950-1959",
                                "1960-1969",
                                "1970-1979",
                                "1980-1989",
                                "1990-1999")) +
  ylim(c(0, 31000)) +
  labs(title = "DOL17+ - Årti hvor respondenten er 14 år", x = "Årti - stiplet linje angiver median", y = "Antal observationer") +
  theme(legend.position = "top") +
  theme_classic()

bar_age17

## Barplot DOL12-15
bar_age12_15 <- D1 %>%
  filter(drop_age < 16) %>%
  ggplot(aes(x = alderskat)) +
  geom_bar(alpha = 0.7, fill = "darkred") +
  geom_text(aes(label = sprintf("%.02f %%", stat(prop)*100), group = 1), stat = 'count', 
            position = position_dodge(width = .9), vjust = -0.5, size = 4) +
  geom_vline(xintercept = c(3), linetype = "dotted") +
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8),
                     labels = c("1920-1929",
                                "1930-1939",
                                "1940-1949",
                                "1950-1959",
                                "1960-1969",
                                "1970-1979",
                                "1980-1989",
                                "1990-1999")) +
  ylim(c(0, 31000)) +
  labs(title = "DOL12-15 - Årti hvor respondenten er 14 år", x = "Årti - stiplet linje angiver median", y = "Antal observationer") +
  theme(legend.position = "top") +
  theme_classic()

bar_age12_15

## Samlet barplot for alder
pdf("/Users/Madshove/Desktop/alder_samlet.pdf", 13, 8)
grid.arrange(bar_age12_15, bar_age16, bar_age17, ncol = 2)
dev.off()

## Samlet plot for maps og alder
pdf("/Users/Madshove/Desktop/USmap_alder.pdf", 13, 14)
grid.arrange(usmapbaseline, bar_age12_15, usmapkontrol, bar_age16, usmaptreatment, bar_age17)
dev.off()

pdf("/Users/Madshove/Desktop/USmap_samlet.pdf", 10, 6)
grid.arrange(usmapbaseline, usmapkontrol, usmaptreatment, ncol = 2)
dev.off()
