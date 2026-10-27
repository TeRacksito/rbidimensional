load("HIPER200.RData")
attach(HIPER200)

mod1 <- lm(peso ~ edad + I(edad^2))
summary(mod1)$coefficients["edad", "Estimate"]

mean(TAsist1[es_hiper == "no lo sabe"], na.rm = TRUE)

sd(TAsist0[act_fisi == "intensa"], na.rm = TRUE)

mod7 <- lm(TAdias1 ~ log(TAdias0))
summary(mod7)$r.squared

mod10_m <- lm(TAdias1 ~ TAdias0, subset = (genero == "masculino"))
mod10_f <- lm(TAdias1 ~ TAdias0, subset = (genero == "femenino"))
summary(mod10_m)$coefficients["TAdias0", "Estimate"]
summary(mod10_f)$coefficients["TAdias0", "Estimate"]

sub11 <- HIPER200[act_fisi == "moderada" & cafe == "no toma", ]
prop.table(table(sub11$cl_peso))["normal"] * 100

pregunta1 <- lm(TAsist0 ~ edad + I(edad^2))
summary(pregunta1)

pregutna4 <- mean(TAdias1[estudios == "estudios secundarios"], na.rm = TRUE)
print(pregutna4)

pregunta6 <- sd(edad[sit_labo == "autnomo"])
print(pregunta6)

pregunta7 <- lm(TAsist1 ~ log(TAdias0))
summary(pregunta7)

pregunta10a <- lm(TAsist1 ~ TAsist0, subset = (sal == "poca"))
summary(pregunta10a)
pregunta10b <- lm(TAsist1 ~ TAsist0, subset = (sal == "mucha"))
summary(pregunta10b)

pregunta11 <- HIPER200[cafe == "moderado" & cl_tabac == "no fumador", ]
prop.table(table(pregunta11$es_hiper))["si"] * 100

mod1 <- lm(TAsist1 ~ log(edad))
summary(mod1)$coefficients["(Intercept)", "Estimate"]
