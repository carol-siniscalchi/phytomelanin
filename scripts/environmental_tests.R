#### Environamental variables vs phytomelanin ####

library(caper)
library(ggplot2)
library(ggpubr)

### load tree ####
tree <- read.tree("tree.tre")

### make data frame  ####

# read solar radiation
solar <- read.csv("wc21_30s_merged_asc.average.csv")

# read phytomelanin states
ftm <- read.csv("traits.bi.csv")

# read bios and elevation
bio1 <- read.csv("BIOCLIM_1.average.csv", header = FALSE, sep = " ")
colnames(bio1) <- c("species", "BIO1")
bio12 <- read.csv("BIOCLIM_12.average.csv", header = FALSE, sep = " ")
colnames(bio12) <- c("species", "BIO12")
bio3 <- read.csv("BIOCLIM_3.average.csv", header = FALSE, sep = " ")
colnames(bio3) <- c("species", "BIO3")
bio5 <- read.csv("BIOCLIM_5.average.csv", header = FALSE, sep = " ")
colnames(bio5) <- c("species", "BIO5")
bio14 <- read.csv("BIOCLIM_14.average.csv", header = FALSE, sep = " ")
colnames(bio14) <- c("species", "BIO14")
bio15 <- read.csv("BIOCLIM_15.average.csv", header = FALSE, sep = " ")
colnames(bio15) <- c("species", "BIO15")
bio4 <- read.csv("BIOCLIM_4.average.csv", header = FALSE, sep = " ")
colnames(bio4) <- c("species", "BIO4")
elev <- read.csv("GTOPO30_ELEVATION.average.csv", header = FALSE, sep = " ")
colnames(elev) <- c("species", "Elevation")

# merge
total <- merge(solar, ftm)
total <- merge(total, bio1)
total <- merge(total, bio12)
total <- merge(total, bio3)
total <- merge(total, bio5)
total <- merge(total, bio14)
total <- merge(total, bio15)
total <- merge(total, bio4)
total <- merge(total, elev)

# change variable type for phytomelanin states
total$FTM <- as.factor(total$FTM)
total$cortex <- as.factor(total$cortex)
total$pericycle <- as.factor(total$pericycle)
total$primary_phloem <- as.factor(total$primary_phloem)
total$secondary_phloem <- as.factor(total$secondary_phloem)
total$primary_xylem <- as.factor(total$primary_xylem)
total$secondary_xylem <- as.factor(total$secondary_xylem)

### plot some graphs ###
rad_plot <- ggplot(total, aes(x = FTM, y = radiation, fill = FTM)) + geom_violin(trim = TRUE) + 
            scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
            labs(y="Solar radiation (kJ m-2 day-1)", x = "Phytomelanin") +
            geom_boxplot(width=0.1) + theme(legend.position="none")

bio1_plot <- ggplot(total, aes(x = FTM, y = BIO1/10, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Annual mean temperature (ºC)", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

bio12_plot <- ggplot(total, aes(x = FTM, y = BIO12, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Annual precipitation (mm)", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

bio3_plot <- ggplot(total, aes(x = FTM, y = BIO3, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Isothermality", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

bio5_plot <- ggplot(total, aes(x = FTM, y = BIO5/10, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Max Temperature of Warmest Month (ºC)", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

bio14_plot <- ggplot(total, aes(x = FTM, y = BIO14, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Precipitation of Driest Month (mm)", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

bio15_plot <- ggplot(total, aes(x = FTM, y = BIO15, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Precipitation Seasonality", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")

elev_plot <- ggplot(total, aes(x = FTM, y = Elevation, fill = FTM)) + geom_violin(trim = TRUE) + 
  scale_fill_manual(values = c("#a6dba0","#7b3294"), name = "", labels = c("Absent", "Present")) +
  labs(y="Elevation (m)", x = "Phytomelanin") +
  geom_boxplot(width=0.1) + theme(legend.position="none")


pdf(file="enviornment_phytomelanin_new.pdf", width = 12, height = 8)
ggarrange(rad_plot, bio1_plot, bio12_plot, bio3_plot, bio5_plot, bio14_plot, bio15_plot, elev_plot,
          ncol = 4, nrow = 2)
dev.off()

## run some t-tests:
t.test(total$radiation~total$FTM)
t.test(total$BIO1~total$FTM)
t.test(total$BIO12~total$FTM)
t.test(total$BIO3~total$FTM)
t.test(total$BIO5~total$FTM)
t.test(total$BIO14~total$FTM)
t.test(total$BIO15~total$FTM)
t.test(total$BIO4~total$FTM)
t.test(total$Elevation~total$FTM)


## save matrix ##
write.csv(total, "variables_phytomelanin.csv")


### linear models ###

## all tissues together 
solarlin <- summary(lm(formula = radiation ~ FTM, data = total))
bio1 <- summary(lm(formula = BIO1 ~ FTM, data = total))
bio12 <- summary(lm(formula = BIO12 ~ FTM, data = total))
comb <- summary(lm(formula = radiation+BIO1+BIO12 ~ FTM, data = total))
bio3 <- summary(lm(formula = BIO3 ~ FTM, data = total))
bio5 <- summary(lm(formula = BIO5 ~ FTM, data = total))
bio14 <- summary(lm(formula = BIO14 ~ FTM, data = total))
bio15 <- summary(lm(formula = BIO15 ~ FTM, data = total))
bio4 <- summary(lm(formula = BIO4 ~ FTM, data = total))
elevlin <- summary(lm(formula = Elevation ~ FTM, data = total))

## cortex
summary(lm(formula = radiation ~ cortex, data = total))
summary(lm(formula = BIO1 ~ cortex, data = total))
summary(lm(formula = BIO12 ~ cortex, data = total))
summary(lm(formula = BIO3 ~ cortex, data = total))
summary(lm(formula = BIO5 ~ cortex, data = total))
summary(lm(formula = BIO14 ~ cortex, data = total))
summary(lm(formula = BIO15 ~ cortex, data = total))
summary(lm(formula = BIO4 ~ cortex, data = total))
summary(lm(formula = Elevation ~ cortex, data = total))

## pericycle
summary(lm(formula = radiation ~ pericycle, data = total))
summary(lm(formula = BIO1 ~ pericycle, data = total))
summary(lm(formula = BIO12 ~ pericycle, data = total))
summary(lm(formula = BIO3 ~ pericycle, data = total))
summary(lm(formula = BIO5 ~ pericycle, data = total))
summary(lm(formula = BIO14 ~ pericycle, data = total))
summary(lm(formula = BIO15 ~ pericycle, data = total))
summary(lm(formula = BIO4 ~ pericycle, data = total))
summary(lm(formula = Elevation ~ pericycle, data = total))

## primary phloem
summary(lm(formula = radiation ~ primary_phloem, data = total))
summary(lm(formula = BIO1 ~ primary_phloem, data = total))
summary(lm(formula = BIO12 ~ primary_phloem, data = total))
summary(lm(formula = BIO3 ~ primary_phloem, data = total))
summary(lm(formula = BIO5 ~ primary_phloem, data = total))
summary(lm(formula = BIO14 ~ primary_phloem, data = total))
summary(lm(formula = BIO15 ~ primary_phloem, data = total))
summary(lm(formula = BIO4 ~ primary_phloem, data = total))
summary(lm(formula = Elevation ~ primary_phloem, data = total))

## secondary phloem
summary(lm(formula = radiation ~ secondary_phloem, data = total))
summary(lm(formula = BIO1 ~ secondary_phloem, data = total))
summary(lm(formula = BIO12 ~ secondary_phloem, data = total))
summary(lm(formula = BIO3 ~ secondary_phloem, data = total))
summary(lm(formula = BIO5 ~ secondary_phloem, data = total))
summary(lm(formula = BIO14 ~ secondary_phloem, data = total))
summary(lm(formula = BIO15 ~ secondary_phloem, data = total))
summary(lm(formula = BIO4 ~ secondary_phloem, data = total))
summary(lm(formula = Elevation ~ secondary_phloem, data = total))

## secondary xylem
summary(lm(formula = radiation ~ secondary_xylem, data = total))
summary(lm(formula = BIO1 ~ secondary_xylem, data = total))
summary(lm(formula = BIO12 ~ secondary_xylem, data = total))
summary(lm(formula = BIO3 ~ secondary_xylem, data = total))
summary(lm(formula = BIO5 ~ secondary_xylem, data = total))
summary(lm(formula = BIO14 ~ secondary_xylem, data = total))
summary(lm(formula = BIO15 ~ secondary_xylem, data = total))
summary(lm(formula = BIO4 ~ secondary_xylem, data = total))
summary(lm(formula = Elevation ~ secondary_xylem, data = total))

#### PGLS #####

## make variance-covariance matrix
data_object <- comparative.data(tree, total, species, vcv=TRUE)

## all tissues
solarpgls <- summary(pgls(formula = as.numeric(radiation) ~ FTM, data = data_object))
bio1pgls <- summary(pgls(formula = as.numeric(BIO1) ~ FTM, data = data_object))
bio12pgls <- summary(pgls(formula = as.numeric(BIO12) ~ FTM, data = data_object))
combp <- summary(pgls(formula = radiation+BIO1+BIO12 ~ FTM, data = data_object))
bio3pgls <- summary(pgls(formula = as.numeric(BIO3) ~ FTM, data = data_object))
bio5pgls <- summary(pgls(formula = as.numeric(BIO5) ~ FTM, data = data_object))
bio14pgls <- summary(pgls(formula = as.numeric(BIO14) ~ FTM, data = data_object))
bio15pgls <- summary(pgls(formula = as.numeric(BIO15) ~ FTM, data = data_object))
bio4pgls <- summary(pgls(formula = as.numeric(BIO4) ~ FTM, data = data_object))
elevpgls <- summary(pgls(formula = as.numeric(Elevation) ~ FTM, data = data_object))

## cortex
summary(pgls(formula = radiation ~ cortex, data = data_object))
summary(pgls(formula = BIO1 ~ cortex, data = data_object))
summary(pgls(formula = BIO12 ~ cortex, data = data_object))
summary(pgls(formula = BIO3 ~ cortex, data = data_object))
summary(pgls(formula = BIO5 ~ cortex, data = data_object))
summary(pgls(formula = BIO14 ~ cortex, data = data_object))
summary(pgls(formula = BIO15 ~ cortex, data = data_object))
summary(pgls(formula = BIO4 ~ cortex, data = data_object))
summary(pgls(formula = Elevation ~ cortex, data = data_object))


## pericycle
summary(pgls(formula = radiation ~ pericycle, data = data_object))
summary(pgls(formula = BIO1 ~ pericycle, data = data_object))
summary(pgls(formula = BIO12 ~ pericycle, data = data_object))
summary(pgls(formula = BIO3 ~ pericycle, data = data_object))
summary(pgls(formula = BIO5 ~ pericycle, data = data_object))
summary(pgls(formula = BIO14 ~ pericycle, data = data_object))
summary(pgls(formula = BIO15 ~ pericycle, data = data_object))
summary(pgls(formula = BIO4 ~ pericycle, data = data_object))
summary(pgls(formula = Elevation ~ pericycle, data = data_object))

## primary phloem
summary(pgls(formula = radiation ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO1 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO12 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO3 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO5 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO14 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO15 ~ primary_phloem, data = data_object))
summary(pgls(formula = BIO4 ~ primary_phloem, data = data_object))
summary(pgls(formula = Elevation ~ primary_phloem, data = data_object))

## secondary phloem
summary(pgls(formula = radiation ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO1 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO12 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO3 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO5 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO14 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO15 ~ secondary_phloem, data = data_object))
summary(pgls(formula = BIO4 ~ secondary_phloem, data = data_object))
summary(pgls(formula = Elevation ~ secondary_phloem, data = data_object))

## secondary xylem
summary(pgls(formula = radiation ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO1 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO12 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO3 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO5 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO14 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO15 ~ secondary_xylem, data = data_object))
summary(pgls(formula = BIO4 ~ secondary_xylem, data = data_object))
summary(pgls(formula = Elevation ~ secondary_xylem, data = data_object))

