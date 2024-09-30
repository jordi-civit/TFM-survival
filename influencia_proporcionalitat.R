# Carreguem les dades que volguem estudiar
# load("C:/Users/user/Desktop/TFM/Resultats simulació/Riscos/resultats_e1_N1000_V10_C01.RData")



nvariables <- 10 #Nomnre de variables del nostre conjunt de dades
significacio <- rep(0,100)
proporcionalitat <- rep(0,100)
censura <- rep(0,100)
ultim_temps <- rep(0,100)

for (i in 1:100) { # Estudiem la mitjana de 100 conjunts de dades
  dat <- eval(parse(text = paste0("(dades3[[",i,"]]$data)")))
  cox_dat <- coxph(Surv(time,status)~ ., dat)
  summary_cox_dat <- summary(cox_dat)
  pvalors <-summary_cox_dat$coefficients[1:nvariables,5]
  t1 <- table(pvalors < 0.05)
  significacio[i] <- as.numeric(t1["TRUE"])
  variables_influents <- which(pvalors < 0.05)
  
  phtest <- cox.zph(cox_dat)
  pvalors_riscos <- phtest$table[1:nvariables,3]
  pvalors_riscos <- pvalors_riscos[variables_influents]
  t2 <- table(pvalors_riscos > 0.05)
  proporcionalitat[i] <- as.numeric(t2["TRUE"])
  
  t3 <- table(dat$status)
  censura[i] <- as.numeric(t3["FALSE"])/nrow(dat)
  
  ultim_temps[i] <- nrow(dat[dat$time == 500 & dat$status == TRUE,])/nrow(dat)
  
}

proporcionalitat[is.na(proporcionalitat)] <- 0

print(paste(" Hi ha un",round(mean(significacio)/nvariables*100,1), "% de variables significatives"))

print(paste(" Un ",round(mean(proporcionalitat)/nvariables*100,1), "% del total de variables són PROPORCIONALS i influents"))
print(paste(" Un ",round((mean(significacio)-mean(proporcionalitat))/nvariables*100,1), "% de variables són NO PROPORCIONALS i influents"))

print(paste("Un",round( 100 - mean(significacio)/nvariables*100,1), "% de variables NO SÓN significatives"))


round(mean(censura), 2); min(censura)
mean(ultim_temps); max(ultim_temps)
