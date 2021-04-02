################################################################################
### Meta Analyse Linde, uitkomst: neovaginalengte (mean)
### Start: 09/03/2021
### Sander MJ van Kuijk
################################################################################

library(meta)

setwd("C:/Users/sande/Documents/Werk/LINDE")
d <- read.csv("Davydov_neovaginalengte.csv", sep = ";")
names(d)[1] <- "auteur"

View(d)

# Selectie vanaf 6 maanden?

d <- d[ 10:17, ]

# Email Lisanne: Fedele 2010 weglaten

d <- d[-2, ]; d

# SD missing:
# d$SD[is.na(d$SD)] <- sum(d$SD*d$Aantal.patienten/
#                             sum(d$Aantal.patienten, na.rm = TRUE), na.rm = TRUE)

model <- metamean(n = Aantal.patienten, mean = Mean, sd = SD,
                  studlab = paste(auteur, "et al.", Jaar), data = d)
model

forest(model)
dev.off()

png("forest_neovaginalengte.png", width = 700, height = 500, pointsize = 24)
forest(model, sortvar = d$Jaar, xlab = "Length (cm)", comb.fixed = FALSE,
       print.tau2 = FALSE, print.pval.Q = FALSE, xlim = c(6, 10),
       smlab = "", rightlabs = c("Mean", "95% CI", "Weight"))
dev.off()

################################################################################
