################################################################################
### Meta Analyse Linde, uitkomst: neovaginalengte (mean)
### Start: 09/03/2021
### Sander MJ van Kuijk
################################################################################

library(meta)

setwd("C:/Users/sande/Documents/Werk/LINDE")
d1 <- read.csv("vecchiettie_neo.csv", sep = ";")
names(d1)[1] <- "auteur"
d1 <- d1[-4, ]

d2 <- read.csv("davydov_neo.csv", sep = ";")
names(d2)[1] <- "auteur"

d3 <- read.csv("vecchiettie_fsfi.csv", sep = ";")
names(d3)[1] <- "auteur"

d4 <- read.csv("davydov_fsfi.csv", sep = ";")
names(d4)[1] <- "auteur"

# Selectie vanaf 6 maanden?

# d <- d[ 10:17, ]

# Email Lisanne: Fedele 2010 weglaten

# d <- d[-2, ]; d

# SD missing:
# d$SD[is.na(d$SD)] <- sum(d$SD*d$Aantal.patienten/
#                             sum(d$Aantal.patienten, na.rm = TRUE), na.rm = TRUE)

model1 <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = d1)
model1

model2 <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = d2)
model2

model3 <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = d3)
model3

model4 <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = d4)
model4

png("forest_vecchiettie_neo.png", width = 700, height = 500, pointsize = 24)
forest(model1, sortvar = d1$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(6, 11),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()

png("forest_davydov_neo.png", width = 700, height = 500, pointsize = 24)
forest(model2, sortvar = d2$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(6, 11),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()

png("forest_vecchiettie_fsfi.png", width = 700, height = 500, pointsize = 24)
forest(model3, sortvar = d3$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(20, 35),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()

png("forest_davydov_fsfi.png", width = 700, height = 500, pointsize = 24)
forest(model4, sortvar = d4$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(20, 35),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()

ds <- d3[-2, ]

models <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = ds)

png("forest_vecchiettie_fsfi_sensitivity.png", width = 700, height = 500, pointsize = 24)
forest(models, sortvar = ds$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(20, 35),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()
################################################################################
