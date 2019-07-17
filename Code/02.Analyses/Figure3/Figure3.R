rm(list = ls())

# Libraries loading -------------------------------------------------------

library("readr")
library("dplyr")
library("ggplot2")

# Create output directory -------------------------------------------------

if (!dir.exists("Output/Figure3")) { dir.create("Output/Figure3") }

# Load and read data ------------------------------------------------------

load("Data/AQP_DataForAnalysis.RData")


# Create ridgeline plot ---------------------------------------------------

AQP.ancPct <- AQP.Data$AQP.ancestryPct
colnames(AQP.ancPct) <- gsub("_pct", "", colnames(AQP.ancPct))
Ridge.pct.long <- tidyr::gather(AQP.ancPct, key = Ancestry, value, 2:17)
Ridge.pct.long <- Ridge.pct.long %>% filter(value > 1)
Ridge.pct.long <- Ridge.pct.long %>% filter(Ancestry != "Unknown")
Ridge.pct.long$Ancestry <- factor(Ridge.pct.long$Ancestry, 
                                  levels = c("AmericanIndian",
                                             "Scandinavia",
                                             "PacificIslands",
                                             "NorthernAfrica",
                                             "SouthernEurope",
                                             "EasternEurope",
                                             "SubSaharanAfrica",
                                             "MiddleEast",
                                             "Caribbean",
                                             "SoutheastAsia",
                                             "CentralorSouthAmerica",
                                             "AfricanAmerican",
                                             "EastAsia",
                                             "WesternEurope",
                                             "SouthAsia"))


p1 <- ggplot(Ridge.pct.long, aes(x = value, y = Ancestry, fill = Ancestry)) +
  geom_density_ridges(scale = 5.6) + 
  theme_ridges(center_axis_labels = TRUE, grid = F) + 
  theme(legend.position = "none") +
  labs(x = "Salience Value", y = "Ancestry") + 
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100))

pdf(file = "Output/Figure3/Figure3.pdf", paper = "letter")
p1
dev.off()


# Summary statistics ------------------------------------------------------

stats.mean = aggregate(value ~ Ancestry, Ridge.pct.long, function(x) mean = mean(x))
colnames(stats.mean) <- c("Ancestry", "Mean")
stats.sd = aggregate(value ~ Ancestry, Ridge.pct.long, function(x) sd = sd(x))
colnames(stats.sd) <- c("Ancestry", "SD")
stats = stats.mean %>% inner_join(stats.sd) %>% arrange(Mean)
write_csv(stats, "Output/Figure3/stats.csv")


# Analysis incorporating familial ancestry --------------------------------

# 4 Grandparents American Indian vs 4 Grandparents South Asian
AmInd.4GP <- AQP.Data$AQP.ancestryFam %>% filter(mgmAmericanIndian == 1, mgfAmericanIndian == 1, pgmAmericanIndian == 1, pgfAmericanIndian == 1) %>%
  select(studyid, mgmAmericanIndian, mgfAmericanIndian, pgmAmericanIndian, pgfAmericanIndian)
AmInd.4GP.PASAmind <- Ridge.pct.long %>% filter(studyid %in% AmInd.4GP$studyid, Ancestry == "AmericanIndian")
mean(AmInd.4GP.PASAmind$value)

SouthAsia.4GP <- AQP.Data$AQP.ancestryFam %>% filter(mgmSouthAsia == 1, mgfSouthAsia == 1, pgmSouthAsia == 1, pgfSouthAsia == 1) %>%
  select(studyid, mgmSouthAsia, mgfSouthAsia, pgmSouthAsia, pgfSouthAsia)
SouthAsia.4GP.PASAmind <- Ridge.pct.long %>% filter(studyid %in% SouthAsia.4GP$studyid, Ancestry == "SouthAsia")
mean(SouthAsia.4GP.PASAmind$value)

test <- t.test(SouthAsia.4GP.PASAmind$value, AmInd.4GP.PASAmind$value)
print(test)
res <- test$p.value

# Among individuals who American Indian Race
AmInd.race <- AQP.Data$AQP.race %>% filter(Amin == 1)
nonAmInd.race <- AQP.Data$AQP.race %>% filter(Amin != 1)

AmInd.race.PASAmind <- Ridge.pct.long %>% filter(studyid %in% AmInd.race$studyid, Ancestry == "AmericanIndian")
mean(AmInd.race.PASAmind$value)

nonAmInd.race.PASAmind <- Ridge.pct.long %>% filter(studyid %in% nonAmInd.race$studyid, Ancestry == "AmericanIndian")
mean(nonAmInd.race.PASAmind$value)

test <- t.test(AmInd.race.PASAmind$value, nonAmInd.race.PASAmind$value)
print(test)
res <- test$p.value




