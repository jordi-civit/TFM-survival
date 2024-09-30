# AQUEST CODI CONTÉ LA FUNCIÓ "mesures", la qual proporciona els resultats
# per a cadascun dels mètodes implementats un cop hi passem les dades

# load("C:/Users/user/Desktop/TFM/Codis/dades_N1000_V10_C01.RData")
# dades <- dades3[[6]]$data

# En primer lloc carreguem els paquets pertinents

library(survival)        # Survival analysis
library(caret)           # ML
library(dplyr)           # Tidy data
library(ggplot2)         # Tidy graphics
library(survminer)       # Plot survival curves
library(pec)             # Measures of predictive performance
library(glmnet)          # Measures of predictive performance
library(rpart)           # Survival tree
library(partykit)        # Survival tree
library(randomForestSRC) # Random Forest
library(survivalsvm)     # Survival SVM
library(survivalmodels)  # deepsurv
library(reticulate)      # import python packages
library(MTLR)            # MTLR
# remotes::install_github("IyarLin/survXgboost")
library(survXgboost)     # survXgboost
library(xgboost)         # xgboost
library(data.table)      # objects of xgboost
library(Matrix)          # objects of xgboost
library(timeROC)
library(coxed)


# Carreguem la funció mesures, la qual restorna els reusltats obtinguts amb les
# diferents mesures. També retorna el temps necessari pel càlcul d'aquestes, 
# la modelització i la predicció per cadascun dels mètodes.

mesures <- function(data) {
  
# Carreguem i preparem les dades
dades <- data 
set.seed(1)
index_entrenament <- sample(1:nrow(dades), 0.7 * nrow(dades))
index_entrenament

dades_entrenament <- dades[index_entrenament,]
dades_test <- dades[- index_entrenament,]

y <- data.frame(cbind(time = dades_test$time, status = dades_test$status) )

# Creem la matriu on emmagatzarem les mesures de comparació
m <- matrix("", nrow = 9, ncol = 3)
colnames(m) <- c("C-Índex: riscos", "AUC", "IBS")
rownames(m) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                 "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")



##################################
###-- Model ---------------------------------------------------------------------
# fit_km   <- survfit(Surv(time, status) ~ 1, data = dades_entrenament)            # Kaplan-Meier survival curve
# plot(fit_km)
# mediana_temps <- summary(fit_km)$table['median']                            # median time
# (pred_km <- 1 - predictSurvProb(fit_km, dades_test, times = mediana_temps)[,1]) # prob of death in the median time (=0.5)
# (Cind_km_test <- Cindex(pred = pred_km, y = y))


temps <- rep(0,9)
names(temps) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori","Boosting", 
                  "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")
##################################
### COX 

t0_cox <- Sys.time()
model_cox <- coxph(Surv(time, status) ~ .,data=dades_entrenament, x = TRUE);

temps_unics_esdeveniment <- sort(unique(dades_entrenament$time[dades_entrenament$status == 1]))
prediccio_cox <- predictSurvProb(object = model_cox, newdata = dades_test, temps_unics_esdeveniment)



m["Cox",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                  Surv(dades_test$time, dades_test$status), 
                                  predict(object = model_cox, newdata = dades_test, type = "risk"))

auc_cox <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                     surv = prediccio_cox,
                                     times = temps_unics_esdeveniment)
m["Cox",2] <- auc_cox

m["Cox",3] <- IBS_cox_test <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), 
                                               sp_matrix = prediccio_cox)

temps["Cox"] <- difftime(Sys.time(), t0_cox, units = "secs")
print(paste("Cox fet. Temps:", round(temps["Cox"] ,1), "segons."))

##################################
### ARBRES

# Ho fem amb la funció ctree
t0_arbre <- Sys.time()
model_arbre_LR <- ctree(Surv(time, status) ~ . , data = dades_entrenament)

temps_unics_esdeveniment <- sort(unique(dades_entrenament$time[dades_entrenament$status == 1]))


prediccio_ctree <- predict(model_arbre_LR, newdata = dades_test, type = "response")
prediccio_ctree[prediccio_ctree == "Inf"] <- 501
m["Arbre LR",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                 Surv(dades_test$time, dades_test$status),
                                 -prediccio_ctree)


CIT_PEC <- pecCtree(Surv(time, status) ~ . , data = dades_entrenament)
prediccio_arbre_LR<- pec:::predictSurvProb(object=CIT_PEC, newdata=dades_test, times=temps_unics_esdeveniment)


auc_arbre <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                      surv = prediccio_arbre_LR,
                                      times = temps_unics_esdeveniment)
m["Arbre LR",2] <- auc_arbre
m["Arbre LR",3] <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), sp_matrix = prediccio_arbre_LR)



temps["Arbre LR"] <- difftime(Sys.time(), t0_arbre, units = "secs")
print(paste("Arbre LR fet. Temps:", round(temps["Arbre LR"] ,1), "segons."))

##################################
### BOSC ALEATORI

opt_param   <- tune(Surv(time, status) ~ ., 
                    data     = dades_entrenament, 
                    ntreeTry = 50)

t0_bosc <- Sys.time()


model_bosc <- rfsrc(Surv(time, status) ~ ., data = dades_entrenament, 
                    mtry      = opt_param$optimal['mtry'],
                    nodesize  = opt_param$optimal['nodesize'],
                    ntree = 500, splitrule="logrank")
prediccio_bosc  <- predict(model_bosc, dades_test)

m["Bosc aleatori",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                      Surv(dades_test$time, dades_test$status), 
                                      prediccio_bosc$predicted)

auc_bosc <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                      surv = prediccio_bosc$survival,
                                      times = model_bosc$time.interest)
m["Bosc aleatori",2] <- auc_bosc


m["Bosc aleatori",3] <- IBS_bosc_test <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), 
                                                          sp_matrix = prediccio_bosc$survival)


temps["Bosc aleatori"] <- difftime(Sys.time(), t0_bosc, units = "secs")
print(paste("Bosc aleatori fet. Temps:", round(temps["Bosc aleatori"] ,1), "segons."))

##################################
### BAGGING

t0_bagging <- Sys.time()
model_bagging <- rfsrc(Surv(time, status) ~ ., data = dades_entrenament,
                       mtry      = ncol(dades_entrenament)-2,
                       nodesize  = opt_param$optimal['nodesize'],
                       ntree = 500, splitrule="logrank", bootstrap = "none")
prediccio_bagging  <- predict(model_bagging, dades_test) 

m["Bagging",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                Surv(dades_test$time, dades_test$status), 
                                prediccio_bagging$predicted)

auc_bagging <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                      surv = prediccio_bagging$survival,
                                      times = model_bagging$time.interest)
m["Bagging",2] <- auc_bagging


m["Bagging",3] <- IBS_bagging_test <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), 
                                                                sp_matrix = prediccio_bagging$survival)


temps["Bagging"] <- difftime(Sys.time(), t0_bagging, units = "secs")
print(paste("Bagging fet. Temps:", round(temps["Bagging"] ,1), "segons."))


##################################
### POTENCIACIÓ PER GRADIENT 

##-- Prepare data -------------------------------------------------------------------
t0_potenciacio <- Sys.time()

label_train <- ifelse(dades_entrenament$status == 1, dades_entrenament$time, dades_entrenament$time)
label_test  <- ifelse(dades_test$status == 1,  dades_test$time,  -dades_test$time)

x_train <- as.matrix(dades_entrenament[, !names(dades_entrenament) %in% c("size","time", "status", "disease")]) # it does not work with factors
x_test0 <- as.matrix(dades_test[, !names(dades_entrenament) %in% c("size","time", "status", "disease")])

# x_test0 ha de ser numèric

x_test <- xgb.DMatrix(data  = as.matrix(x_test0), label = label_test)

##-- Model ---------------------------------------------------------------------
model_boosting <- xgb.train.surv(
  params = list(
    objective = "survival:cox",
    eval_metric = "cox-nloglik",
    eta = 0.05), # larger eta leads to algorithm not converging
  data      = x_train, 
  label     = label_train,
  watchlist = list(val2 = x_test),
  nrounds   = 1000, 
  early_stopping_rounds = 50
)


pred_boosting <- predict(model_boosting, newdata = x_test, type = "surv")

m["Boosting",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                       Surv(dades_test$time, dades_test$status), 
                                       predict(model_boosting, newdata = x_test, type = "risk"))

auc_boosting <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                      surv = pred_boosting,
                                      times = temps_unics_esdeveniment)
m["Boosting",2] <- auc_boosting



m["Boosting",3] <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), sp_matrix = pred_boosting)



temps["Boosting"] <- difftime(Sys.time(), t0_potenciacio, units = "secs")
print(paste("Boosting fet. Temps:", round(temps["Boosting"] ,1), "segons."))


##################################
### XARXES NEURONALS
# reticulate::py_install("pycox")
# reticulate::py_install("torch")
t0_xn <- Sys.time()
set.seed(1)
model_xn <- deepsurv(data           = dades_entrenament,           # data
                     frac           = 0.3,               # Fraction of data to validate dataset          
                     activation     = "relu",            # Activation function: REctified Linear Units. Piecewise linear function that will output the input directly if positive, otherwise, it will output zero 
                     num_nodes      = c(6, 4, 2),     # number of nodes in each layer
                     dropout        = 0.1,               # dropout fraction tuned over [0, 1]
                     early_stopping = TRUE,              # Early stopping
                     batch_size     = 32,                # Elements in each batch
                     epochs         = 100)               # number of epochs

#med_xn <- which(model_xn$y[,1] > mediana_temps)[1]  # Aquesta mediana és la de dades entrenament
set.seed(1);prediccio_xn <- predict(model_xn, type = "surv", dades_test)

predict(model_xn, dades_test, type = "risk") 

m["Xarxes Neuronals", 1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                                Surv(dades_test$time, dades_test$status), 
                                                predict(model_xn, newdata = dades_test, type = "risk"))

auc_xn <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                       surv = prediccio_xn,
                                       times = as.numeric(colnames(prediccio_xn)))
m["Xarxes Neuronals",2] <- auc_xn






set.seed(1)
m["Xarxes Neuronals", 3] <- IBS_xn_test <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), 
                                                            sp_matrix = prediccio_xn)



temps["Xarxes Neuronals"] <- difftime(Sys.time(), t0_xn, units = "secs")
print(paste("Xarxes Neuronals fet. Temps:", round(temps["Xarxes Neuronals"] ,1), "segons."))

##################################
# SVM
t0_svm_reg <- Sys.time()
model_svm_reg <- survivalsvm(Surv(time, status) ~ .,     # Formula
                             data     = dades_entrenament, # Only 500 observations 
                             type     = "regression",    # Type of SVM
                             gamma.mu = 1,               # Penalization parameter 
                             opt.meth = "quadprog",      # Optimization method 
                             kernel   = "lin_kernel")    # Kernel to map

prediccio_svm_reg <- predict(model_svm_reg, dades_test, type = "surv")$predicted

m["SVM Regressió", 1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                       Surv(dades_test$time, dades_test$status), 
                                       -prediccio_svm_reg)

temps["SVM Regressió"] <- difftime(Sys.time(), t0_svm_reg, units = "secs")
print(paste("SVM Regressió fet. Temps:", round(temps["SVM Regressió"] ,1), "segons."))

# SVM RÀNQUING
t0_svm_ranquing <- Sys.time()
model_svm_ranquing <- survivalsvm(Surv(time, status) ~ .,     # Formula
                                  data     = dades_entrenament, # Only 500 observations
                                  type     = "vanbelle1",     # Type of SVM
                                  diff.meth= "makediff1",
                                  gamma.mu = 1,               # Penalization parameter
                                  opt.meth = "quadprog",      # Optimization method
                                  kernel   = "lin_kernel")    # Kernel to map

pred_svm_ranquing <- predict(model_svm_ranquing, dades_test, type = "surv")$predicted[1,]

m["SVM Rànquing", 1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                       Surv(dades_test$time, dades_test$status),
                                       -pred_svm_ranquing)

temps["SVM Rànquing"] <- difftime(Sys.time(), t0_svm_ranquing, units = "secs")
print(paste("SVM Rànquing fet. Temps:", round(temps["SVM Rànquing"] ,1), "segons."))

##################################
### MULTITASCA
t0_multi <- Sys.time()
temps_unics_esdeveniment <- sort(unique(dades_entrenament$time[dades_entrenament$status == 1]))

model_mtlr   <- mtlr(Surv(time, status)~. , data = dades_entrenament, time_points =  temps_unics_esdeveniment)


prediccio_mtlr <- t(predict(model_mtlr, dades_test, type="surv"))
temps_mtlr <- prediccio_mtlr[1,]
prediccio_mtlr <- prediccio_mtlr[-c(1),]


m["Multitasca",1] <- survAUC::UnoC(Surv(dades_entrenament$time, dades_entrenament$status),
                                         Surv(dades_test$time, dades_test$status), 
                                         predict(model_mtlr, dades_test, type="prob_event"))

#predict(model_mtlr, dades_test, type="mean_time")

auc_multi <- survex::integrated_cd_auc(Surv(y$time, y$status),
                                       surv = prediccio_mtlr,
                                       times = temps_mtlr)
m["Multitasca",2] <- auc_multi

m["Multitasca",3] <- SurvMetrics::IBS(Surv(y[, "time"], y[,"status"]), sp_matrix = prediccio_mtlr )

temps["Multitasca"] <- difftime(Sys.time(), t0_multi, units = "secs")
print(paste("Multitasca fet. Temps:", round(temps["Multitasca"] ,1), "segons."))


return(list(m, temps))

}
