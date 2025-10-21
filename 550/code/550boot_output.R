boot_chinM15 <- readRDS("~/dissertation/survey/github/550/data/boot_stelM14.rds")
View(boot_chinM15)
boot_chinM15$boot.params
df <- boot_chinM15$boot.params
wdf <- data.frame(t(df))
wdf <- wdf[, -c(18:36)]
derp <- boxplot(wdf[,1:17])
derp + abline(h = 0, col = "red")