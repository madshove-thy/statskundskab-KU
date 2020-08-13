#### Randomiseringsinferens ####
# Loade packages
library(permute)
library(parallel)
library(doParallel); library(foreach)

# Lave nyt dataset
used1 <- D1 %>%
  dplyr::select(csl_16, csl_g17, state, yearat14, surveyyear, presdem_intend_all, naes_id, trend_2, trend_3, trend_4, trend_5, trend_6, trend_7, trend_8, trend_9, trend_10, trend_11, trend_12,trend_13, trend_14, trend_15, trend_16, trend_17, trend_18, trend_19, trend_20,
                trend_21, trend_22, trend_23, trend_24, trend_25, trend_26, trend_27, trend_28, trend_29, trend_30, trend_31, trend_32, trend_33, trend_34, trend_35, trend_36, trend_37, trend_38, trend_39, trend_40, trend_41, trend_42, trend_43, trend_44, trend_45, trend_46, trend_47, trend_48, trend_49)

# Fjern missing values
used1 <- na.omit(used1)

# lav en numerisk identifier for stat
used1$idtr <- used1 %>%
  group_by(state) %>%
  group_indices()

# definer block-strukturen
CTRL <- how(blocks = factor(used1$idtr))

### Nu laver vi 1000 forskellige permuteringer paa 2017
# dvs. 1000 forskellige tidspunkter, som treatment kunne have vaeret tildelt

set.seed(185)
perm <- shuffleSet(used1$csl_g17,    # den treatment, der skal randomiseres 
                   nset = 1000,    # antallet af gange det skal gentages
                   control = CTRL) # definitionen af vores blocks


perm

# hver raekke i "perm" matrixen er en anderledes maade, hvorpaa treatment kunne have vaeret assigned

cl <- makeCluster(detectCores()-1) # create a cluster with 7 cores
registerDoParallel(cl) # register the cluster

## Forst finder vi koefficienten for DOL16 randomisering
res161 <- foreach(i = 1:nrow(perm),             # loop over alle raekkerne i perm
                 .combine = "rbind", 
                 .packages = "lfe") %dopar% {
                   
                   #brug de alternative indekseringer i perm til at lave en ny treatment
                   crnt_perm <- perm[i,]
                   used1$this_treat16 <- used1$csl_16[crnt_perm]
                   used1$this_treat17 <- used1$csl_g17[crnt_perm]
                   # brug den nye treatment i vores diff-in-diff
                   this_mod <-felm(presdem_intend_all ~ this_treat16+this_treat17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                                     trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                                   | factor(state) + factor(surveyyear)+factor(yearat14)
                                   |0            
                                   |state,               
                                   data = used1)
                   
                   coef(this_mod)[1]
                 }

# Lav dataframe
res161 <- as.data.frame(res161)


## Saa finder vi koeficienten for DOL17+ randomisering
res171 <- foreach(i = 1:nrow(perm),             # loop over alle r?kkerne i perm
                  .combine = "rbind", 
                  .packages = "lfe") %dopar% {
                    
                    #brug de alternative indekseringer i perm til at lave en ny treatment
                    crnt_perm <- perm[i,]
                    used1$this_treat16 <- used1$csl_16[crnt_perm]
                    used1$this_treat17 <- used1$csl_g17[crnt_perm]
                    # brug den nye treatment i vores diff-in-diff
                    this_mod1 <-felm(presdem_intend_all ~ this_treat16+this_treat17+trend_2+trend_3+trend_4+trend_5+trend_6+trend_7+trend_8+trend_9+trend_10+trend_11+trend_12+trend_13+trend_14+trend_15+trend_16+trend_17+trend_18+trend_19+trend_20+
                                      trend_21+trend_22+trend_23+trend_24+trend_25+trend_26+trend_27+trend_28+trend_29+trend_30+trend_31+trend_32+trend_33+trend_34+trend_35+trend_36+trend_37+trend_38+trend_39+trend_40+trend_41+trend_42+trend_43+trend_44+trend_45+trend_46+trend_47+trend_48+trend_49
                                    | factor(state) + factor(surveyyear)+factor(yearat14)
                                    |0            
                                    |state,               
                                    data = used1)
                    
                    coef(this_mod1)[2]
                  }

# Laver dataframe
res171 <- as.data.frame(res171)

# Stopper clusteren
stopCluster(cl)


## Inspicerer DOL16 i plot
p161 <- ggplot(res161, aes(x = this_treat16)) +
  geom_histogram(fill = "grey", colour = "white") +
  geom_vline(xintercept = coef(Demintention_ren3)[1], lty = 3) +
  theme_classic() + 
  labs(x = "Permuterede estimater",
       y = "Antal estimater") +
  annotate(geom = "text", x = -0.1126, y = 150,
           label = "Det faktiske\ndiff-in-diff-estimat") +
  scale_x_continuous(limits = c(-0.0500, 0.030))

p161

## Inspicerer DOL17+ i plot
p171 <- ggplot(res171, aes(x = this_treat17)) +
  geom_histogram(fill = "grey", colour = "white") +
  geom_vline(xintercept = coef(Demintention_ren3)[2], lty = 3) +
  theme_classic() + 
  labs(x = "Permuterede estimater",
       y = "Antal estimater") +
  annotate(geom = "text", x = -0.1126, y = 150,
           label = "Det faktiske\ndiff-in-diff-estimat") +
  scale_x_continuous(limits = c(-0.0500, 0.030))

p171

## Trækker de to dataframes fra hinanden for at faa dataframe vi kan bruge til endelig randomiseringsinferens
randomiseringsinferens <- res171-res161

## Laver plot for endelig randomiseringsinferens
prandomiseringsinferens <- ggplot(randomiseringsinferens, aes(x = this_treat17)) +
  geom_histogram(fill = "grey", colour = "white") +
  geom_vline(xintercept = coef(diftabel1)[1], lty = 3) +
  theme_classic() + 
  labs(x = "Permuterede estimater",
       y = "Antal estimater",
       title = "Randomiseringsinferens") +
  annotate(geom = "text", x = -0.1126, y = 150,
           label = "Det faktiske\ndiff-in-diff-estimat") +
  scale_x_continuous(limits = c(-0.030, 0.030))

## Udregn p-value
nrow(subset(randomiseringsinferens, this_treat17 > coef(diftabel1)))/nrow(randomiseringsinferens)
1-0.993

## Printer
pdf("/Users/Madshove/Desktop/randomiseringsinferens.pdf", 8, 6)
prandomiseringsinferens
dev.off()





