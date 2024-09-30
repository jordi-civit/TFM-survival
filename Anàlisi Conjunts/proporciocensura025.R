##############################
### PROPORCIÓ CENSURA 0.25 ###
##############################

# Codi per a l'ÀNALISI dels conjunts de dades simulats


# ESCENARI 1: 100% RISCOS PROPORCIONALS
#####
rm(list = ls())
source("C:/Users/user/Desktop/TFM/codis/mesures.R")
load("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Censura/dades_e1_N1000_V10_C025.RData")


t11 <- Sys.time()
resultats_1_N1000_V10_C025 <- matrix("", nrow = 9, ncol = 3)
colnames(resultats_1_N1000_V10_C025) <- c("C-Índex: riscos", "AUC", "IBS")
rownames(resultats_1_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                         "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")
temps_1_N1000_V10_C025 <- rep(0,9)
names(temps_1_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                  "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")

for (l in 1:100) {
  eval(parse(text = paste0("resultats_1_N1000_V10_C025_", l, " <- ", "mesures(dades1[[",l,"]]$data)")))
  print(paste("Portem", l, "conjunts de dades analitzats."))
}

for (i in 1:nrow(resultats_1_N1000_V10_C025)) {
  for (j in 1:ncol(resultats_1_N1000_V10_C025)) {
    vec <- c()
    vec_temps <- c()
    
    decimals <- 2
    if(j == 3) decimals <- 3
    
    for (k in 1:100) {
      
      vec <- c(vec, eval(parse(text = paste0("resultats_1_N1000_V10_C025_", k, "[[1]]")))[i,j]) 
      vec_temps <- c(vec_temps, eval(parse(text = paste0("resultats_1_N1000_V10_C025_", k, "[[2]]")))[i]) 
      #print(paste( "Fila:", i, ". Columna:", j, ". Vector:", vec))
      vec <- as.numeric(vec)
      vec_temps  <- as.numeric(vec_temps)
      
    }
    
    resultats_1_N1000_V10_C025[i,j] <- paste0(round(mean(vec),decimals), " (", round(sd(vec),decimals),")" )
    temps_1_N1000_V10_C025[i] <- paste0(round(mean(vec_temps*60),1), " (", round(sd(vec_temps*60),1),")" )
    
  }
  
}
resultats_1_N1000_V10_C025
temps_1_N1000_V10_C025

t12 <- Sys.time()
t12-t11
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Censura/resultats_e1_N1000_V10_C025.RData")



#####
# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
#####
rm(list = ls())
source("C:/Users/user/Desktop/TFM/codis/mesures.R")
load("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Censura/dades_e2_N1000_V10_C025.RData")

t21 <- Sys.time()



resultats_2_N1000_V10_C025 <- matrix("", nrow = 9, ncol = 3)
colnames(resultats_2_N1000_V10_C025) <- c("C-Índex: riscos", "AUC", "IBS")
rownames(resultats_2_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                         "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")
temps_2_N1000_V10_C025 <- rep(0,9)
names(temps_2_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                  "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")

for (l in 1:100) {
  eval(parse(text = paste0("resultats_2_N1000_V10_C025_", l, " <- ", "mesures(dades2[[",l,"]]$data)")))
  print(paste("Portem", l, "conjunts de dades analitzats."))
}

for (i in 1:nrow(resultats_2_N1000_V10_C025)) {
  for (j in 1:ncol(resultats_2_N1000_V10_C025)) {
    vec <- c()
    vec_temps <- c()
    
    decimals <- 2
    if(j == 3) decimals <- 3
    
    for (k in 1:100) {
      vec <- c(vec, eval(parse(text = paste0("resultats_2_N1000_V10_C025_", k, "[[1]]")))[i,j]) 
      vec_temps <- c(vec_temps, eval(parse(text = paste0("resultats_2_N1000_V10_C025_", k, "[[2]]")))[i]) 
      
      vec <- as.numeric(vec)
      vec_temps  <- as.numeric(vec_temps)
      
    }
    resultats_2_N1000_V10_C025[i,j] <- paste0(round(mean(vec),decimals), " (", round(sd(vec),decimals),")" )
    temps_2_N1000_V10_C025[i] <- paste0(round(mean(vec_temps),1), " (", round(sd(vec_temps),1),")" )
  }
  
}
resultats_2_N1000_V10_C025
temps_2_N1000_V10_C025
t22 <- Sys.time()
t22-t21
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Censura/resultats_e2_N1000_V10_C025.RData")

#####
# ESCENARI 3: 100% RISCOS NO PROPORCIONALS
#####
rm(list = ls())
source("C:/Users/user/Desktop/TFM/codis/mesures.R")
t31 <- Sys.time()
load("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Censura/dades_e3_N1000_V10_C025.RData")


resultats_3_N1000_V10_C025 <- matrix("", nrow = 9, ncol = 3)
colnames(resultats_3_N1000_V10_C025) <- c("C-Índex: riscos", "AUC", "IBS")
rownames(resultats_3_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                         "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")
temps_3_N1000_V10_C025 <- rep(0,9)
names(temps_3_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                  "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")


for (l in 1:100) {
  eval(parse(text = paste0("resultats_3_N1000_V10_C025_", l, " <- ", "mesures(dades3[[",l,"]]$data)")))
  print(paste("Portem", l, "conjunts de dades analitzats."))
}

for (i in 1:nrow(resultats_3_N1000_V10_C025)) {
  for (j in 1:ncol(resultats_3_N1000_V10_C025)) {
    vec <- c()
    vec_temps <- c()
    
    decimals <- 2
    if(j == 3) decimals <- 3
    
    for (k in 1:100) {
      vec <- c(vec, eval(parse(text = paste0("resultats_3_N1000_V10_C025_", k, "[[1]]")))[i,j]) 
      vec_temps <- c(vec_temps, eval(parse(text = paste0("resultats_3_N1000_V10_C025_", k, "[[2]]")))[i]) 
      
      vec <- as.numeric(vec)
      vec_temps  <- as.numeric(vec_temps)
    }
    
    resultats_3_N1000_V10_C025[i,j] <- paste0(round(mean(vec, na.rm = TRUE),decimals), " (", round(sd(vec, na.rm = TRUE),decimals),")" )
    temps_3_N1000_V10_C025[i] <- paste0(round(mean(vec_temps),1), " (", round(sd(vec_temps),1),")" )
    
  }
  
}
resultats_3_N1000_V10_C025
temps_3_N1000_V10_C025
t32 <- Sys.time()
t32-t31
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Censura/resultats_e3_N1000_V10_C025.RData")


# ESCENARI 4: 50% RISCOS NO PROPORCIONALS I 50% NO INFLUENTS  
rm(list = ls())
source("C:/Users/user/Desktop/TFM/codis/mesures.R")
load("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Censura/dades_e4_N1000_V10_C025.RData")

t41 <- Sys.time()
resultats_4_N1000_V10_C025 <- matrix("", nrow = 9, ncol = 3)
colnames(resultats_4_N1000_V10_C025) <- c("C-Índex: riscos", "AUC", "IBS")
rownames(resultats_4_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                         "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")
temps_4_N1000_V10_C025 <- rep(0,9)
names(temps_4_N1000_V10_C025) <- c("Cox", "Arbre LR", "Bagging", "Bosc aleatori", "Boosting", 
                                  "Xarxes Neuronals", "SVM Regressió", "SVM Rànquing", "Multitasca")

for (l in 1:100) {
  eval(parse(text = paste0("resultats_4_N1000_V10_C025_", l, " <- ", "mesures(dades4[[",l,"]]$data)")))
  print(paste("Portem", l, "conjunts de dades analitzats."))
}

for (i in 1:nrow(resultats_4_N1000_V10_C025)) {
  for (j in 1:ncol(resultats_4_N1000_V10_C025)) {
    vec <- c()
    vec_temps <- c()
    
    decimals <- 2
    if(j == 3) decimals <- 3
    
    for (k in 1:100) {
      vec <- c(vec, eval(parse(text = paste0("resultats_4_N1000_V10_C025_", k, "[[1]]")))[i,j]) 
      vec_temps <- c(vec_temps, eval(parse(text = paste0("resultats_4_N1000_V10_C025_", k, "[[2]]")))[i]) 
      
      vec <- as.numeric(vec)
      vec_temps  <- as.numeric(vec_temps)
      
    }
    resultats_4_N1000_V10_C025[i,j] <- paste0(round(mean(vec),decimals), " (", round(sd(vec),decimals),")" )
    temps_4_N1000_V10_C025[i] <- paste0(round(mean(vec_temps),1), " (", round(sd(vec_temps),1),")" )
    
  }
  
}
resultats_4_N1000_V10_C025
temps_4_N1000_V10_C025
t42 <- Sys.time()
t42-t41
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Censura/resultats_e4_N1000_V10_C025.RData")