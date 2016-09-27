library(ggplot2)

beavers <- read.csv("~/Beavers/nuevo archivo datos castores tierra del fuego.csv")
beavers <- beavers[1:66,]
beavers$Codigo.castorera <- as.factor(ifelse(grepl("BR",beavers$Codigo.castorera), "Bosque", ifelse(grepl("ER",beavers$Codigo.castorera), "Estepa", "Matorral")))
beavers$Edad <- as.numeric(as.character(beavers$Edad))
beavers$Peso..Kgs. <- as.numeric(as.character(beavers$Peso..Kgs.))

ggplot(beavers, aes(x = Edad, y = Peso..Kgs.)) + geom_point(aes(color = Codigo.castorera)) + stat_smooth(method = "lm", formula = y ~ log(x), aes(color = Codigo.castorera, fill = Codigo.castorera))
View(beavers)


PESO_EDAD <- glm(Peso..Kgs.~ Edad + I(log(Edad)) + I(Edad^2), data = beavers)

library(glmulti)

a <- glmulti::glmulti(PESO_EDAD,  level = 1, crit = "aicc", confsetsize = 5)

weightable(a)


b <- predict(glm(Peso..Kgs. ~ 1 + I(log(Edad)), data = beavers), newdata = data.frame(Edad = c(1:20)))

age_w<- data.frame(Weight = b, age = c(1:20))

ggplot(age_w, aes(x = age, y = Weight)) + geom_line()+ geom_smooth()