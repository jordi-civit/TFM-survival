
# SIMULACIÓ DELS COJUNTS DE DADES ON VARIEM EL NOMBRE DE VARIABLES

###############################
### NOMBRE DE VARIABLES = 4 ###
###############################

my.hazard <- function(t){
  t^2/100000*2
}

set.seed(1)
m1 <- matrix(0.1, nrow =500, ncol = 4)%*%diag(c(75, 125, -75, -125))/10
dades1 <- coxed::sim.survdata(N = 1000, T = 500, type = "tvbeta",
                              num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                              beta = m1,
                              xvars = 4,
                              hazard.fun = my.hazard,
                              mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                              censor = 0.1, censor.cond = FALSE)


for (i in 1:100)  colnames(dades1[[i]]$data) <- c("X1", "X2", "X3", "X4", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e1_N1000_V4_C010.RData")


# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)
m2mig <- matrix(0.1, nrow =500, ncol = 2)%*%diag(c(75, 125))/10
m2 <- cbind(m2mig, matrix(0, nrow = 500, ncol = 2))
dades2 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m2,
                       xvars = 4,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.10, censor.cond = FALSE)


for (i in 1:100) colnames(dades2[[i]]$data) <- c("X1", "X2", "X3", "X4", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e2_N1000_V4_C010.RData")



# ESCENARI 3

my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)

prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m3 <- cbind(matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
            matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-750, nrow = 500, ncol = 1)*10/10000,
            matrix(-prova-1250, nrow = 500, ncol = 1)*10/10000)

dades3 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m3,
                       xvars = 4,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100)   colnames(dades3[[i]]$data) <- c("X1", "X2", "X3", "X4", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e3_N1000_V4_C010.RData")

# 
# # ESCENARI 4
set.seed(1)
my.hazard <- function(t){
  t^2/100000*2
}


prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m4mig <- cbind(matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1250, nrow = 500, ncol = 1)*10/10000)

m4 <- cbind(m4mig, matrix(0, nrow = 500, ncol = 2))
dades4 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m4,
                       xvars = 4,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100) colnames(dades4[[i]]$data) <- c("X1", "X2", "X3", "X4", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e4_N1000_V4_C010.RData")

### NOMBRE DE VARIABLES = 40
################


my.hazard <- function(t){
  t^2/100000*2
}

set.seed(1)
m1 <- matrix(0.1, nrow =500, ncol = 40)%*%diag(rep(c(50, 75, 100, 125, 150, -50, -75, -100, -125, -150), 4))/10
dades1 <- coxed::sim.survdata(N = 1000, T = 500, type = "tvbeta",
                              num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                              beta = m1,
                              xvars = 40,
                              hazard.fun = my.hazard,
                              mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                              censor = 0.10, censor.cond = FALSE)


for (i in 1:100)  colnames(dades1[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", 
                                                  "X11", "X12", "X13", "X14", "X15","X16", "X17", "X18", "X19", "X20", 
                                                  "X21", "X22", "X23", "X24", "X25","X26", "X27", "X28", "X29", "X30", 
                                                  "X31", "X32", "X33", "X34", "X35","X36", "X37", "X38", "X39", "X40", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e1_N1000_V40_C010.RData")


# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
my.hazard <- function(t){
  t^2/100000*2
}
set.seed(1)
m2mig <- matrix(0.1, nrow =500, ncol = 20)%*%diag(rep(c(50, 75, 100, 125, 150),4))/10
m2 <- cbind(m2mig, matrix(0, nrow = 500, ncol = 20))
dades2 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m2,
                       xvars = 40,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)


for (i in 1:100) colnames(dades2[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", 
                                                 "X11", "X12", "X13", "X14", "X15","X16", "X17", "X18", "X19", "X20", 
                                                 "X21", "X22", "X23", "X24", "X25","X26", "X27", "X28", "X29", "X30", 
                                                 "X31", "X32", "X33", "X34", "X35","X36", "X37", "X38", "X39", "X40", "time", "status")
save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e2_N1000_V40_C010.RData")





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
m3 <- cbind(m3,m3,m3,m3)

dades3 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m3,
                       xvars = 40,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100)   colnames(dades3[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", 
                                                   "X11", "X12", "X13", "X14", "X15","X16", "X17", "X18", "X19", "X20", 
                                                   "X21", "X22", "X23", "X24", "X25","X26", "X27", "X28", "X29", "X30", 
                                                   "X31", "X32", "X33", "X34", "X35","X36", "X37", "X38", "X39", "X40", "time", "status")


save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e3_N1000_V40_C010.RData")





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
m4mig <- cbind(m4mig, m4mig, m4mig, m4mig)

m4 <- cbind(m4mig, matrix(0, nrow = 500, ncol = 20))
dades4 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m4,
                       xvars = 40,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

for (i in 1:100) colnames(dades4[[i]]$data) <- c("X1", "X2", "X3", "X4", "X5","X6", "X7", "X8", "X9", "X10", 
                                                 "X11", "X12", "X13", "X14", "X15","X16", "X17", "X18", "X19", "X20", 
                                                 "X21", "X22", "X23", "X24", "X25","X26", "X27", "X28", "X29", "X30", 
                                                 "X31", "X32", "X33", "X34", "X35","X36", "X37", "X38", "X39", "X40", "time", "status")

save.image("C:/Users/user/Desktop/TFM/Resultats simulació/Conjunts de dades/Variables/dades_e4_N1000_V40_C010.RData")
