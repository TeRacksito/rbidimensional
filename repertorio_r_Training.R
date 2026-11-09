load("Tviejo.Rdata")
attach(Training)
library(moments)

str(Training$lr)
str(Training$conv_status)
levels(Training$lr)

prop.table(table(red_level, lr)) * 100


boxplot(vram_gb ~ hw)


barplot(table(vram_level, hw), legend.text = TRUE)
mosaicplot(table(vram_level, hw))


table(red_level, net_complexity)
median(red_level, na.rm = TRUE)

tapply(vram_gb, net_complexity, quantile, probs = 0.75, na.rm = TRUE)

  
tapply(loss_final, conv_status, skewness, na.rm = TRUE)

tapply(loss_final, conv_status, summary)


# correlación 
cor(train_time, loss_final)

# correlación subgrupos
by(Training, Training$arch, function(sub) cor(sub$vram_gb, sub$loss_init))


# ajustes
mod_lin  <- lm(loss_final ~ train_time)
mod_quad <- lm(loss_final ~ train_time + I(train_time^2))

summary(mod_lin)
summary(mod_quad)
anova(mod_lin, mod_quad)

# ajuste subset
mod_lin_lstm  <- lm(loss_init ~ vram_gb, subset = (arch == "LSTM"))
mod_quad_lstm <- lm(loss_init ~ vram_gb + I(vram_gb^2), subset = (arch == "LSTM"))
summary(mod_lin_lstm)
summary(mod_quad_lstm)

comp <- anova(mod_lin_lstm, mod_quad_lstm)

# residuos linealidad e independencia
# plot(mod_lin, which = 1)

# residuos normalidad
# plot(mod_lin, which = 2)

# residuos homocedasticidad o igualdad de varianzas
# plot(mod_lin, which = 3)

# residuos atípicos
# plot(mod_lin, which = 4)

par(mfrow=c(2,2))
plot(mod_lin)


sh_test <- shapiro.test(residuals(mod_lin))
sh_test$statistic
sh_test$p.value


# predicción
mod_pred <- lm(loss_init ~ vram_gb)
predict(mod_pred, newdata = data.frame(vram_gb = 5), interval = "prediction")
