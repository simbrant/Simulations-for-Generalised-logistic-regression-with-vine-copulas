library(rvinecopulib)

#
# read parameters
#

mu_0 <- as.numeric(read.csv("parameters_20/mu_0.csv")[, 1])
mu_1 <- as.numeric(read.csv("parameters_20/mu_1.csv")[, 1])
sigma_0 <- as.numeric(read.csv("parameters_20/sigma_0.csv")[, 1])
sigma_1 <- as.numeric(read.csv("parameters_20/sigma_1.csv")[, 1])
load("parameters_20/vinecop_0.RData")
load("parameters_20/vinecop_1.RData")

set.seed(1992)

pi_y <- 0.5
p <- length(mu_1)
n <- 20000
xtype <- rep("c_a",p)

for (dset_no in 1:100) {
  y <- sample(c(1, 0), n , TRUE, c(pi_y, 1 - pi_y))
  U <- matrix(NA, n, p)
  U[y == 1, ] <- rvinecop(sum(y),vinecop=vinecop_1)
  U[y == 0, ] <- rvinecop(n-sum(y),vinecop=vinecop_0)
  X <- matrix(NA, n, p)
  X[y == 1] <- sapply(1:p, function(j) {
    if (xtype[j] == "c_a") {
      qnorm(U[y==1, j], mu_1[j], sqrt(sigma_1[j]))
    } else if (xtype[j] == "d_b") {
      qbinom(U[y == 1, j], 1, rho_1[j])
    }
  })
  X[y == 0] <- sapply(1:p, function(j) {
    if (xtype[j] == "c_a") {
      qnorm(U[y==0, j], mu_0[j], sqrt(sigma_0[j]))
    } else if (xtype[j] == "d_b") {
      qbinom(U[y == 0, j], 1, rho_0[j])
    }
  })
  write.csv(cbind(y, X), 
            file = paste0(dir,paste0(c("datasets/vinedat_1", p, n, dset_no), collapse = "_"),".csv"),
            row.names = FALSE)
}

set.seed(1992)

for (dset_no in 1:100) {
  y <- sample(c(1, 0), n , TRUE, c(pi_y, 1 - pi_y))
  U <- matrix(NA, n, p)
  U[y == 1, ] <- rvinecop(sum(y),vinecop=vinecop_1)
  U[y == 0, ] <- rvinecop(n-sum(y),vinecop=vinecop_0)
  X <- matrix(NA, n, p)
  X[y == 1] <- sapply(1:p, function(j) {
    if (xtype[j] == "c_a") {
      qnorm(U[y==1, j], mu_1[j], sqrt(sigma_1[j]))
    } else if (xtype[j] == "d_b") {
      qbinom(U[y == 1, j], 1, rho_1[j])
    }
  })
  X[y == 0] <- sapply(1:p, function(j) {
    if (xtype[j] == "c_a") {
      qnorm(U[y==0, j], mu_0[j], sqrt(sigma_1[j]))
    } else if (xtype[j] == "d_b") {
      qbinom(U[y == 0, j], 1, rho_0[j])
    }
  })
  write.csv(cbind(y, X), 
            file = paste0(dir,paste0(c("datasets/vinedat_2", p, n, dset_no), collapse = "_"),".csv"),
            row.names = FALSE)
}

