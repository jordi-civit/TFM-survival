
# SIMULACIÓ DELS COJUNTS DE DADES DE REFERÈNCIA


## FUNICÓ DE RISC BASAL


my.hazard <- function(t){
  t^2/100000*2
}



# ESCENARI 1: 100% RISCOS PROPORCIONALS
set.seed(1)
m1 <- matrix(0.1, nrow =500, ncol = 10)%*%diag(c(50, 75, 100, 125, 150, -50, -75, -100, -125, -150))/10
dades1 <- coxed::sim.survdata(N = 1000, T = 500, type = "tvbeta",
                              num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                              beta = m1,
                              xvars = 10,
                              hazard.fun = my.hazard,
                              mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                              censor = 0.1, censor.cond = FALSE)


# ESCENARI 2: 50% RISCOS PROPORCIONALS I 50% NO INFLUENTS
set.seed(1)
m2mig <- matrix(0.1, nrow =500, ncol = 5)%*%diag(c(50, 75, 100, 125, 150))/10
m2 <- cbind(m2mig, matrix(0, nrow = 500, ncol = 5))
dades2 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 2, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m2,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)



# ESCENARI 3: 100% RISCOS NO PROPORCIONALS
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

dades3 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m3,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)


# ESCENARI 4: 50% RISCOS NO PROPORCIONALS I 50% NO INFLUENTS

prova <- c(seq(-1240,1250, 10), rev(seq(-1240,1250, 10)))

m4mig <- cbind(matrix(prova +500, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +750, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1000, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1250, nrow = 500, ncol = 1)*10/10000,
               matrix(prova +1500, nrow = 500, ncol = 1)*10/10000)

m4 <- cbind(m4mig, matrix(0, nrow = 500, ncol = 5))
dades4 <- sim.survdata(N = 1000, T = 500, type = "tvbeta",
                       num.data.frames = 100, fixed.hazard = TRUE, knots = 8, spline = TRUE, X = NULL,
                       beta = m4,
                       xvars = 10,
                       hazard.fun = my.hazard,
                       mu = 0, sd = 0.5, covariate = 1, low = 0, high = 1, compare = median,
                       censor = 0.1, censor.cond = FALSE)

