

library(tidyverse)
library(broom)
library(Lahman)

# wrangle data
dat <- Teams %>% dplyr::select(yearID, franchID, W, L, AB, H, X2B, X3B, HR, BB, SF, HBP, IPouts, HA, BBA, FP, BPF, PPF, R) %>% 
	filter(yearID >= 1900) %>% 
	replace_na(list(SF=0, HBP = 0)) %>% 
	mutate(X1B = H - X2B - X3B - HR) %>% 
	mutate(OBP = (H + BB + HBP)/(AB + BB + HBP + SF)) %>%  
	mutate(SLG = (X1B + 2*X2B + 3*X3B + 4*HR)/AB) %>% 
	mutate(OPS = OBP + SLG) %>% 
	mutate(WHIP = 3*(HA + BBA)/IPouts ) %>% 
	mutate(Wpct = W / (W + L))


# fit initial model
m <- lm(R ~ OPS + WHIP + FP, data = dat, x = TRUE)
summary(m)

# problems with normality of errors
par(mfrow = c(2,2))
plot(m)
par(mfrow = c(1,1))
qqnorm(residuals(m))
qqline(residuals(m))
abline(h = -50, lty = 2, col = "red")

# problematic residuals cluster in certain seasons
# all of these seasons had a lower number of games played
# problem with rate statistic vs count statistic 
dat <- augment(m, dat)
dat %>% 
	filter(.resid < -100) %>% 
	select(yearID) %>% 
	table()

dat %>% 
	filter(.resid < -50) %>% 
	select(yearID) %>% 
	table()


dat <- dat %>% 
	mutate(year1981 = ifelse(yearID == 1981,1,0), 
				 year1994 = ifelse(yearID == 1994,1,0),
				 year1995 = ifelse(yearID == 1995,1,0),
				 year2020 = ifelse(yearID == 2020,1,0), 
				 year1918 = ifelse(yearID == 1918,1,0), 
				 year1919 = ifelse(yearID == 1919,1,0))

m2 <- lm(R ~ OPS + WHIP + FP + year1981 + year1994 + year1995 + year2020 + 
				 	year1918 + year1919, data = dat, x = TRUE)


par(mfrow=c(2,2))
plot(m2)

par(mfrow = c(1,1))
qqnorm(residuals(m2))
qqline(residuals(m2))

length(which(abs(residuals(m2)) > 50)) / nrow(dat)

dat <- augment(m2, dat)
dat %>% 
	filter(.resid > 50) %>% 
	select(yearID) %>% 
	table()

summary(m2)
summary(m)
anova(m, m2)

head(m$x)
head(m2$x)




predict(m2, data.frame(OPS = 0,
											 WHIP = 0,
											 FP = 0,
											 year1981 = 0,
											 year1994 = 0,
											 year1995 = 0,
											 year2020 = 0,
											 year1918 = 0,
											 year1919 = 0))
predict(m2, data.frame(OPS = 0,
											 WHIP = 0,
											 FP = 0,
											 year1981 = 0,
											 year1994 = 0,
											 year1995 = 0,
											 year2020 = 1,
											 year1918 = 0,
											 year1919 = 0))

# 2020 Dodgers
# predict(m2, data.frame(OPS = 0.8206627,
# 											 WHIP = 1.056312,
# 											 FP = 0.982,
# 											 year1981 = 0,
# 											 year1994 = 0,
# 											 year1995 = 0,
# 											 year2020 = 1,
# 											 year1918 = 0,
# 											 year1919 = 0))



m_small <- lm(R ~ OPS + year1981 + year1994 + year1995 + year2020 + 
							 	year1918 + year1919, data = dat)
anova(m_small, m2)





foo <- split(dat, f = as.factor(dat$yearID))
bar <- do.call(rbind, lapply(foo, function(xx){
	avgWHIP <- 3*(sum(xx$HA) + sum(xx$BBA))/sum(xx$IPouts)
	avgOBP <- (sum(xx$H + xx$BB + xx$HBP))/sum(xx$AB + xx$BB + xx$HBP + xx$SF)
	avgSLG <- sum(xx$X1B + 2*xx$X2B + 3*xx$X3B + 4*xx$HR)/sum(xx$AB)
	avgOPS <- avgOBP + avgSLG
	xx <- xx %>% mutate(WHIPscore = avgWHIP/ WHIP, 
											OPSscore = OPS / avgOPS, 
											FPscore = mean(xx$FP) / xx$FP)
	xx
}))




