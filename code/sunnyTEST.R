## generate process error terms
# same process variance
Q <- runif(1, 0, 1.25)
w <- rnorm(1000, 0, Q)

# different process variance
Q_1 <- runif(1, 0, 1.25)
Q_2 <- runif(1, 0, 1.25) 

w_1 <- rnorm(500, 0, Q_1)
w_2 <- rnorm(500, 0, Q_2)

## generate observation error terms
R_a <- runif(1, 0, 1.25)
R_b <- runif(1, 0, 1.25) 
R_c <- runif(1, 0, 1.25)
R_d <- runif(1, 0, 1.25)

v_a <- rnorm(250, 0, R_a)
v_b <- rnorm(250, 0, R_b)
v_c <- rnorm(250, 0, R_c)
v_d <- rnorm(250, 0, R_d)
