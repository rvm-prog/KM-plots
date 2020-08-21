# Ishan Paranjpe, BS, Valentin Fuster, MD, PhD, Anuradha Lala, MD et al.
# Association of Treatment Dose Anticoagulation with In-Hospital Survival 
# Among Hospitalized Patients With COVID-19. JACC 2020:76; 122-9.
#################################################
# 25 days of FU
library(survival)
library(ggplot2)
library(survminer)

MV <- read.csv(file = "AC-MV.csv", header = T, sep=",")
# variables in dataset anticoagulatian and mechanincal ventilation
pts <- MV$pts
fu <- MV$fu

events <- MV$events
scenario <- MV$scenario
# all variables in dataset prostdata

# scenario R = research arm, C = controle arm
surv_object <- Surv(time = fu , event = events)
fit1 <- survfit(surv_object ~ scenario, data = MV)
summary(fit1)

# ggsurvplot in action
j <- ggsurvplot(fit1, data = MV,
                xlim = c(0,28),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumcensor", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 5,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                # surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
###########################################
j <- ggsurvplot(fit1, data = MV,
                xlim = c(0,28),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumevents", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 5,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                # surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
######################################
j <- ggsurvplot(fit1, data = MV,
                xlim = c(0,28),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "absolute", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 5,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                # surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j
##############################################
# Fig. 4.
#############################################

