#  The RECOVERY Collaborative Group. Dexamethasone in Hospitalized Patients
#  with Covid-19 – Preliminary Report. NEJM 2020; DOI: 10.1056/NEJMoa2021436
###############################
library(survival)
library(ggplot2)
library(survminer)

dexa <- read.csv(file = "Dexamethason.csv", header = T, sep=",")
# variables in dataset dexamethason
pts <- dexa$pts
fu <- dexa$fu

events <- dexa$events
scenario <- dexa$scenario
# all variables in dataset prostdata

# scenario R = research arm, C = controle arm
surv_object <- Surv(time = fu , event = events)
fit1 <- survfit(surv_object ~ scenario, data = dexa)
summary(fit1)

# ggsurvplot in action
j <- ggsurvplot(fit1, data = dexa,
                xlim = c(0,30),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumcensor", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 7,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                # surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j


j <- ggsurvplot(fit1, data = dexa,
                xlim = c(0,30),
                #conf.int = TRUE,
                pval = TRUE, 
                pval.method = TRUE, 
                risk.table = "nrisk_cumevents", 
                legend.title = " ",
                palette = c("darkblue", "darkred"),
                tables.theme = theme_cleantable(), 
                break.time.by = 7,
                risk.table.fontsize = 5 ,
                risk.table.height = 0.20, 
                #surv.median.line = c("hv"), 
                censor.shape = c("|"), 
                censor.size = 6, 
                axes.offset = FALSE, 
                tables.y.text = FALSE)

j

######################################################################
# Fig.3 
####################################################################
# first row is alive and death in Dexa group
# second row is alive and death in Control group
M <- as.table(rbind(c(228, 96), c(400, 283)))

# row names : Dexa and Usual-Care  =  scenario
# col names : Alive and death =  status
dimnames(M) <- list(scenario = c("Dexa", "Usual-Care"),
                    status = c("Alive","Death"))
M
addmargins(M)
Xsq <- chisq.test(M)  # Prints test summary
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null
Xsq$p.value
####################################################################
