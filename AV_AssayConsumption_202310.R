#### SET UP ####

choicei <- read.csv("Input_Data/2023.10_Feeding Assays_Choice_IDs.csv")
nochoicei <- read.csv("Input_Data/2023.10_Feeding Assays_NoChoice_IDs.csv")

choicedat <- read.csv("Input_Data/2023.10_Choice_EelgrassSegmentMass.csv")
choicedat2 <- read.csv("Input_Data/2023.10_AV_Choice_AssaySegmentScanProcessing.csv")
choicedat2man <- read.csv("Input_Data/2023.10_AV_Choice_AssaySegmentScanProcessing_manualpost.csv")
nochoicedat <- read.csv("Input_Data/2023.10_NoChoice_EelgrassSegmentMass.csv")

avmassdat <- read.csv("Input_Data/2023.10.27_AV_Choice_AVmasspercontainer.csv")

library(tidyverse)
library(nlme)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggpubr)

#### Data Preparation ####

choiceid <- choicei %>% 
  pivot_longer(A:D,
               names_to = "treatment",
               values_to = "section_ID") %>% 
  relocate(c(treatment, section_ID), .after = AV_control)
#for the final, need to do a time calculation - number of hours the assay was run

nochoiceid <- nochoicei
#need to do a time column - number of hours the assay was run

#av mass
avmassdata <- avmassdat %>% 
  mutate(av_drymass_g = Boat_AV_dry_mass_g - Boat_mass_g) %>% 
  dplyr::select(-Boat_AV_dry_mass_g, -Boat_mass_g) %>% 
  rename(unit_ID = Container_ID)


#dataframes that join the data to IDs

choicedata <- left_join(choiceid, choicedat) %>% 
  left_join(choicedat2man) %>% 
  left_join(avmassdata, by = "unit_ID") %>% 
  mutate(AVeel_treat = paste(tankpH_treat, "_", treatment),
         pH_treat = ifelse(treatment %in% c("A", "B"), "ambient",
                           ifelse(treatment %in% c("C", "D"), "reduced", NA)),
         nut_treat = ifelse(treatment %in% c("A", "C"), "ambient",
                           ifelse(treatment %in% c("B", "D"), "increased", NA)),
         plant_ID = str_extract(section_ID, '[0-9]+.[0-9]+'),
         section_age = str_remove(section_ID, "^([^.]+.){2}"),
         area_change_adj = area_change/av_drymass_g) %>% 
  relocate(c(AVeel_treat, pH_treat, nut_treat), .after = treatment) %>% 
  relocate(c(AV_no, mass_mg, area_change, area_change_adj, plant_ID, section_age), .before = Notes)



#to represent AV consumption data relative to controls, make a DF with just mean mass of control eelgrass
#grouping by AV/eel crossed treatment

choicedata_ctrl <- choicedata %>% 
  filter(AV_control == "control") %>%
  dplyr::select(tankpH_treat, treatment, AVeel_treat, mass_mg) %>% 
  group_by(tankpH_treat, treatment,AVeel_treat) %>% 
  summarize(mean_mass = mean(mass_mg, na.rm = T),
            sd_mass = sd(mass_mg, na.rm = T),
            n = sum(!is.na(mass_mg))) %>% 
  mutate(se_mass = sd_mass/sqrt(n))

choicedata_adj <- left_join(choicedata, choicedata_ctrl) %>% 
  dplyr::select(-se_mass, -sd_mass, -n) %>% 
  mutate(adj_mass = mass_mg - mean_mass)

#### Plots - Mass ####

#mean control segment mass
plot1 <- ggplot(data = choicedata_ctrl,
                aes(x = treatment,
                    y = mean_mass,
                    color = tankpH_treat)) +
  geom_point(size = 4,
             position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean_mass - se_mass,
                    ymax = mean_mass + se_mass),
                position = position_dodge(0.5),
                width = 0.2,
                size = 0.8) +
  scale_color_manual(name = "Tank pH",
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  labs(x = "Eelgrass treatment",
       y = "Segment mass (mg)",
       title = "AV Choice - control eelgrass") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
plot1


#choice assay segment mass

boxplot1a <- ggplot(data = choicedata,
                   aes(x = treatment,
                       y = mass_mg,
                       color = tankpH_treat)) +
  geom_boxplot(position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9)) +
  scale_color_manual(name = "AV pH",
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  labs(x = "Eelgrass treatment",
       y = "Segment mass (mg)",
       title = "AV Choice") +
  facet_grid(tankpH_treat~., scales = "free_y") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"),
        legend.position = "none")
boxplot1a  

boxplot1b <- ggplot(data = choicedata,
                    aes(x = treatment,
                        y = mass_mg,
                        color = tankpH_treat)) +
  geom_boxplot(position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9)) +
  scale_color_manual(name = "AV pH",
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  labs(x = "Eelgrass treatment",
       y = "Segment mass (mg)",
       title = "AV Choice") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
boxplot1b  


#choice assay segment mass - adjusted by control mass

boxplot2 <- ggplot(data = subset(choicedata_adj, AV_control == "AV"),
                   aes(x = treatment,
                       y = adj_mass,
                       color = tankpH_treat)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_boxplot(position = position_dodge(0.9)) +
  geom_point(position = position_dodge(0.9)) +
  scale_color_manual(name = "AV pH",
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  labs(x = "Eelgrass treatment",
       y = "Segment mass difference from control (mg)",
       title = "AV Choice") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
boxplot2


#### Plots - Area ####


#bulk area loss
boxplota1 <- ggplot(data = choicedata[choicedata$AV_control == "AV",],
                    aes(x = treatment,
                        y = area_change,
                        color = tankpH_treat)) +
  geom_hline(yintercept = 0, 
             color = "black",
             size = 0.8) +
  geom_boxplot(position = position_dodge(0.8),
               size = 0.8) +
  geom_point(position = position_dodge(0.8),
             size = 2) +
  scale_color_manual(name = expression(paste(italic("A. valida") ~ "pH")),
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  scale_x_discrete(breaks = c("A", "B", "C", "D"),
                   labels = c("ambient pH\nambient nut.", "ambient pH\nincreased nut.",
                              "reduced pH\nambient nut.", "reduced pH\nincreased nut.")) +
  labs(x = "Eelgrass treatment",
       y = expression(paste("Change in segment area ("*cm^2*")")),
       title = "AV Choice") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
boxplota1 

#area loss adjusted by total mass of AV
boxplota2 <- ggplot(data = choicedata[choicedata$AV_control == "AV",],
                    aes(x = treatment,
                        y = area_change_adj,
                        color = tankpH_treat)) +
  geom_hline(yintercept = 0, 
             color = "black",
             size = 0.8) +
  geom_boxplot(position = position_dodge(0.8),
               size = 0.8) +
  geom_point(position = position_dodge(0.8),
             size = 2) +
  scale_color_manual(name = expression(paste(italic("A. valida") ~ "pH")),
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  scale_x_discrete(breaks = c("A", "B", "C", "D"),
                   labels = c("ambient pH\nambient nut.", "ambient pH\nincreased nut.",
                              "reduced pH\nambient nut.", "reduced pH\nincreased nut.")) +
  labs(x = "Eelgrass treatment",
       y = expression(paste("Change in segment area ("*cm^2*"/ g AV)")),
       title = "AV Choice") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
boxplota2 

#### Plots - controls ####

#bulk area loss
boxplotc1 <- ggplot(data = choicedata[choicedata$AV_control == "control",],
                    aes(x = treatment,
                        y = area_change,
                        color = tankpH_treat)) +
  geom_hline(yintercept = 0, 
             color = "black",
             size = 0.8) +
  geom_boxplot(position = position_dodge(0.8),
               size = 0.8) +
  geom_point(position = position_dodge(0.8),
             size = 2) +
  scale_color_manual(name = "Tank pH",
                     breaks = c("ambient", "reduced"),
                     values = c("grey70", "cyan4")) +
  scale_x_discrete(breaks = c("A", "B", "C", "D"),
                   labels = c("ambient pH\nambient nut.", "ambient pH\nincreased nut.",
                              "reduced pH\nambient nut.", "reduced pH\nincreased nut.")) +
  labs(x = "Eelgrass treatment",
       y = expression(paste("Change in segment area ("*cm^2*")")),
       title = "AV Choice - controls") +
  theme_bw(base_size = 20) +
  theme(axis.text = element_text(color = "black"))
boxplotc1 





#### Analysis - Mass ####


#lmm for segment mass

hist(choicedata$mass_mg)

glmm1 <- lmer(mass_mg ~ (pH_treat * nut_treat * tankpH_treat) + 
                (1 | tank_ID:unit_ID) + (1 | plant_ID),
              data = choicedata[choicedata$AV_control == "AV",])
glmm1
plot(glmm1)
anova(glmm1)
summary(glmm1)

emm1 <- emmeans(glmm1, ~ AV_control + (pH_treat * nut_treat * tankpH_treat), adjust = "tukey")
pairs(emm1)


#### Analysis - area ####

#bulk
hist((choicedata$area_change_adj[choicedata$AV_control == "AV"]))

glmm2 <- lmer((area_change) ~ (pH_treat * nut_treat * tankpH_treat) + 
                (1 | tank_ID:unit_ID) + (1 | plant_ID),
              data = choicedata[choicedata$AV_control == "AV",])
glmm2

shapiro.test(resid(glmm2)) #ns = good
hist(resid(glmm2))
plot(glmm2)
ggqqplot(resid(glmm2))


summary(glmm2, ddf = "Kenward-Roger")
anova(glmm2, type = 3, ddf = "Kenward-Roger")


#adjusted
#NEED TO LOOK INTO THIS MORE
#VERY Sig. Shapiro test
#but trends are similar to un-adjusted, so fine for now with abstract
hist((choicedata$area_change_adj[choicedata$AV_control == "AV"]))

glmm2 <- lmer((area_change) ~ (pH_treat * nut_treat * tankpH_treat) + 
                (1 | tank_ID) + (1 | plant_ID),
              data = choicedata[choicedata$AV_control == "AV",])
glmm2

shapiro.test(resid(glmm2)) #VERY significant
hist(resid(glmm2))
plot(glmm2)
ggqqplot(resid(glmm2))


summary(glmm2, ddf = "Kenward-Roger")
anova(glmm2, type = 3, ddf = "Satterthwaite")


#### Analysis - Controls - Area ####

hist((choicedata$area_change[choicedata$AV_control == "control"]))

glmmc1 <- lmer((area_change) ~ (pH_treat * nut_treat * tankpH_treat) +
                (1 | tank_ID:unit_ID) + (1 | plant_ID),
              data = choicedata[choicedata$AV_control == "control",])

glmmc1

shapiro.test(resid(glmmc1))
hist(resid(glmmc1))
plot(glmmc1)
ggqqplot(resid(glmmc1))

summary(glmmc1, ddf = "Kenward-Roger")
anova(glmmc1, type = 3, ddf = "Kenward-Roger")

#sig. interaction of nutrient treatment and tank pH
