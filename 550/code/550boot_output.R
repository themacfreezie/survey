library(here)

boot_chinM15 <- readRDS(here("550", "data", "boot_chinM15.rds"))
df <- boot_chinM15$boot.params
wdf <- data.frame(t(df))
wdf <- wdf[, -c(18:36)]
wdf_a <- wdf[, -c(9:17)]
wdf_r <- wdf[, -c(1:8)]

chin_a <- boxplot(wdf_a, range = 1.58)
abline(h = 0, col = "red")
title('Bias (Chinook)')

chin_r <- boxplot(wdf_r, range = 1.58)
abline(h = 0, col = "red")
title('Variance (Chinook)')

boot_cohoM10 <- readRDS(here("550", "data", "boot_cohoM10.rds"))
df <- boot_cohoM10$boot.params
wdf <- data.frame(t(df))
wdf <- wdf[, -c(22:49)]
wdf_a <- wdf[, -c(11:21)]
wdf_r <- wdf[, -c(1:10)]

coho_a <- boxplot(wdf_a, range = 1.58)
abline(h = 0, col = "red")
title('Bias (coho)')

coho_r <- boxplot(wdf_r, range = 1.58)
abline(h = 0, col = "red")
title('Variance (coho)')

boot_stelM14 <- readRDS(here("550", "data", "boot_stelM14.rds"))
df <- boot_stelM14$boot.params
wdf <- data.frame(t(df))
wdf <- wdf[, -c(18:41)]
wdf_a <- wdf[, -c(9:17)]
wdf_r <- wdf[, -c(1:8)]

stel_a <- boxplot(wdf_a, range = 1.58)
abline(h = 0, col = "red")
title('Bias (Steelhead)')

stel_r <- boxplot(wdf_r, range = 1.58)
abline(h = 0, col = "red")
title('Variance (Steelhead)')