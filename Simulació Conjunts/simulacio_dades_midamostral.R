
# SIMULACIÓ DELS COJUNTS DE DADES ON VARIEM LA MIDA MOSTRAL


##########################
### MIDA MOSTRAL = 500 ###
##########################

my.hazard <- function(t){
  t^2/100000*2
}

set.seed(1)
m1 <- matrix(0.1, nrow =500, ncol = 10)%*%diag(c(50, 75, 100, 125, 150, -50, -75, -100, -125, -150))/10
dades1 <- coxed::sim.survdata(N = 500, T = 500, type = "tvbeta",
                              num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                              beta = m1,
                              xvars = 10,
                              hazard.fun = my.hazard,
                              mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                              censor = 0.1, censor.cond = FALSE)


for (i in 1:100)  colnames(dades1[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e1_N500_V10_C010.RData")


# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)
m2mig <- matrix(0.1, nrow =500, ncol = 5)%*%diag(c(50, 75, 100, 125, 150))/10
m2 <- cbind(m2mig, matrix(0, nrow = 500, ncol = 5))
dades2 <- sim.survdata(N = 500, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m2,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.10, censor.cond = FALSE)


for (i in 1:100) colnames(dades2[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e2_N500_V10_C010.RData")



#ESCENARI 3
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)

prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m3 <- cbind(matrix(prova +500, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1000, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1500, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-500, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-750, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1000, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1250, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1500, nrow = 500, ncol = 1)*10/10000)

dades3 <- sim.survdata(N = 500, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m3,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100)   colnames(dades3[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e3_N500_V10_C010.RData")


# ESCENARI 4
set.seed(1)
my.hazard <- function(t){
  t^2/100000*2
}


#prova <- c(-8/125*(1:500)^2+32*1:500 -2000)
prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m4mig <- cbind(matrix(prova +500, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1000, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1500, nrow = 500, ncol = 1)*10/10000)

m4 <- cbind(m4mig, matrix(0, nrow = 500, ncol = 5))
dades4 <- sim.survdata(N = 500, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m4,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100) colnames(dades4[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e4_N500_V10_C010.RData")



















### MIDA MOSTRAL N = 2000
################


my.hazard <- function(t){
  t^2/100000*2
}

set.seed(1)
m1 <- matrix(0.1, nrow =500, ncol = 10)%*%diag(c(50, 75, 100, 125, 150, -50, -75, -100, -125, -150))/10
dades1 <- coxed::sim.survdata(N = 2000, T = 500, type = "tvbeta",
                              num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                              beta = m1,
                              xvars = 10,
                              hazard.fun = my.hazard,
                              mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                              censor = 0.10, censor.cond = FALSE)


for (i in 1:100)  colnames(dades1[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e1_N2000_V10_C010.RData")


# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)
m2mig <- matrix(0.1, nrow =500, ncol = 5)%*%diag(c(50, 75, 100, 125, 150))/10
m2 <- cbind(m2mig, matrix(0, nrow = 500, ncol = 5))
dades2 <- sim.survdata(N = 2000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m2,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)


for (i in 1:100) colnames(dades2[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10","time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e2_N2000_V10_C010.RData")



# ESCENARI 3
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)

prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m3 <- cbind(matrix(prova +500, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1000, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1500, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-500, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-750, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1000, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1250, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1500, nrow = 500, ncol = 1)*10/10000)

dades3 <- sim.survdata(N = 2000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m3,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100)   colnames(dades3[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")

save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e3_N2000_V10_C010.RData")


# ESCENARI 4

set.seed(1)
my.hazard <- function(t){
  t^2/100000*2
}


#prova <- c(-8/125*(1:500)^2+32*1:500 -2000)
prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m4mig <- cbind(matrix(prova +500, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1000, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1500, nrow = 500, ncol = 1)*10/10000)

m4 <- cbind(m4mig, matrix(0, nrow = 500, ncol = 5))
dades4 <- sim.survdata(N = 2000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m4,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100) colnames(dades4[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Mida/dades_e4_N2000_V10_C010.RData")
