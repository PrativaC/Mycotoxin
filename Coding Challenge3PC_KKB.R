library(tidyverse)
library(ggpubr)
library(ggrepel)
library(ggplot2)
mycotoxin
str(mycotoxin)
mycotoxin=read.csv("MycotoxinData.csv", na.strings = "na")# Ensure Treatment and Cultivar are factors
mycotoxin$Treatment <- as.factor(mycotoxin$Treatment)
mycotoxin$Cultivar <- as.factor(mycotoxin$Cultivar)

levels(mycotoxin$Treatment)

##### Q1
cbbPalette <- c("#56B4E9", "#009E73")
ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("") + # xlabel empty 
  ylab("DON (ppm)") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme

##### Q2


mycotoxin$Treatment<- factor(mycotoxin$Treatment, levels = c("NTC", "Fg", "Fg + 37", "Fg + 40", "Fg + 70"))

cbbPalette <- c("#56B4E9", "#009E73")
Plot1 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("") + # xlabel empty 
  ylab("DON (ppm)") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 
Plot1

##### Q3
 cbbPalette <- c("#56B4E9", "#009E73")
 Plot2 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("") + # xlabel empty 
  ylab("15XADON") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 

Plot2


cbbPalette <- c("#56B4E9", "#009E73")
Plot3 <- ggplot(mycotoxin, aes(x = Treatment, y = DON, fill = Cultivar)) +
  geom_boxplot() + # add boxplot layer
  xlab("") + # xlabel empty 
  ylab("Seed Mass (mg)") + # y label
  geom_point(alpha = 0.6,pch = 21, color = "black", position = position_jitterdodge()) +
  scale_fill_manual(values = cbbPalette)+ # transparency of the jittered points to 0.6. #Jitter points over the boxplot and fill the points and boxplots Cultivar with two colors from the cbbPallete 
  facet_wrap(~Cultivar)+ #faceted by Cultivar
  theme_classic() # for classic theme 
Plot3

figure=ggarrange(
  Plot1,
  Plot2,
  Plot3,
  labels = c("A", "B", "C"),
  nrow = 1,
  ncol = 3,
  common.legend=TRUE
)
figure
 

##### Q5

# ---- Plot 1: plot1 with pairwise comparisons ----
Plot1_pwc <- Plot1 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format") +  # Add pairwise t-tests
  theme_classic()

# ---- Plot 2: PLOT2 with pairwise comparisons ----
Plot2_pwc <- Plot2 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format") +  # Add pairwise t-tests
  theme_classic()

# ---- Plot 3: waterImbibed.cor with pairwise comparisons ----
Plot3_pwc <- Plot3 +
  geom_pwc(aes(group = Treatment), method = "t.test", label = "p.adj.format") +  # Pairwise t-tests across Crops
  theme_classic()

# ---- Combine all three plots with a common legend ----
figure_pwc =ggarrange(
  Plot1_pwc,
  Plot2_pwc,
  Plot3_pwc,
  labels = c("D", "E", "F"),
  nrow = 1,
  ncol = 3,
  common.legend=TRUE
)

# Display the combined figure
figure_pwc
