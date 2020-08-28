# David P Dearnaley, Gordana Jovic, Isabel Syndikus et al. 
# Escalated-dose versus control-dose conformal radiotherapy for
# prostate cancer: long-term results from the
# MRC RT01randomized controlled trial. Lancet Oncol 2014; 15: 464–73. 
# Published Online 
# February 26, 2014http://dx.doi.org/10.1016/S1470-2045(14)70040-3
####################################
library(survival)
library(ggplot2)
library(survminer)

prostdata <- read.csv(file = "noCensored.csv", header = T, sep=",")
# variables in dataset prostdata
pts <- prostdata$pts
fu <- prostdata$fu

events <- prostdata$events
scenario <- prostdata$scenario
# all variables in dataset 

# scenario R = research arm, C = controle arm
surv_object <- Surv(time = fu , event = events)
fit1 <- survfit(surv_object ~ scenario, data = prostdata)
summary(fit1)

# ggsurvplot in action
j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumcensor", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j


j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumevents", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
######################################################################
# Fig. 1
#######################################################################
# first row is alive and death in Dexa group
# second row is alive and death in Control group
M <- as.table(rbind(c(245, 173), c(194, 224)))

# row names : Research and Control  =  scenario
# col names : Alive and death =  status
dimnames(M) <- list(scenario = c("Research", "Control"),
                    status = c("Alive","Death"))
M
addmargins(M)
Xsq <- chisq.test(M)  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$p.value
###################################################################

# SAME DATASET BUT WITH WITH CENSORED PATIENTS INCLUDED
library(survival)
library(ggplot2)
library(survminer)

prostdata <- read.csv(file = "Censored.csv", header = T, sep=",")
# variables in dataset prostdata
pts <- prostdata$pts
fu <- prostdata$fu

events <- prostdata$events
scenario <- prostdata$scenario
# all variables in dataset prostdata

# scenario R = research arm, C = controle arm
surv_object <- Surv(time = fu , event = events)
fit1 <- survfit(surv_object ~ scenario, data = prostdata)
summary(fit1)

# ggsurvplot in action
j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumcensor", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j


j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumevents", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
##############################################
# FIG. 2
##############################################
j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "absolute", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
j <- ggsurvplot(fit1, data = prostdata,
                xlim = c(0,13),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "percentage", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 2,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
