
# Read dataset (Strong dependencies gaussian model)
dset <- read.csv("datasets/gaussdat_1_8_20000_1.csv")

# Split into two parts
y_tr <- dset[1:1000, 1]
x_tr <- as.matrix(dset[1:1000, -1])

y_te <- dset[1001:2000, 1]
x_te <- as.matrix(dset[1001:2000, -1])

# Logistic regression + vine copula component
md <- LogisticCopula::fit_copula_interactions(
  y = y_tr, x = x_tr, xtype = rep("c_a", 8), verbose = TRUE
  )

# Compute prediction
phat <- predict(md, new_x = x_te)

# Logistic regression fit
md2 <- glm(y_tr ~ x_tr, family = binomial)
phat_2 <- plogis(md2$coefficients[1] + x_te %*% md2$coefficients[-1])

# Precision logistic regression + vine copula part
1 - mean(abs(as.numeric(phat >= 0.5) - y_te))

# Precision plain logistic regression
1 - mean(abs(as.numeric(phat_2 >= 0.5) - y_te))

