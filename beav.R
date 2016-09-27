library(ggplot2)

beavers <- read.csv("~/Beavers/nuevo archivo datos castores tierra del fuego.csv")
beavers <- beavers[1:66,]
Coord <- read.csv("~/Beavers/Coord.csv")
colnames(Coord)<- c("Codigo.castorera", "lat", "lon")
Coord$lat <- (Coord$lat*-1)
Coord$lon <- (Coord$lon* -1)
beavers <- merge(beavers, Coord)

beavers$Codigo.castorera <- as.factor(ifelse(grepl("BR",beavers$Codigo.castorera), "Forest", ifelse(grepl("ER",beavers$Codigo.castorera), "Steppe", "Shrubland")))
beavers$Edad <- as.numeric(as.character(beavers$Edad))
beavers$Peso..Kgs. <- as.numeric(as.character(beavers$Peso..Kgs.))
colnames(beavers) <- c("Habitat", "Beaver_number", "Sex", "Date", "Weight", "Expected_Weight", "stand_weight", "Length", "BMI", "Tail_length", "Tail_Width","Tail_Area", "Tail_Thick", "Tail_Vol", "Tail_Weight", "Age", "Ovaries", "Uterine_Scars", "Fetus", "lat", "lon")

ggplot(beavers, aes(x = Age, y = Weight)) + geom_point(aes(color = Habitat)) + stat_smooth(method = "lm", formula = y ~ log(x), aes(color = Habitat, fill = Habitat))

library(ggmap)

TDF <- get_map(location = "Cameron, Tierra del Fuego, Chile", maptype = "hybrid", zoom = 7)
ggmap(TDF) +geom_point(data= beavers, aes(x =lon, y = lat, color = Habitat)) + theme(legend.position="bottom")

PESO_EDAD <- glm(Weight~ Age + I(log(Age)) + I(Age
                                               ^2), data = beavers)

library(glmulti)

a <- glmulti::glmulti(PESO_EDAD,  level = 1, crit = "aicc", confsetsize = 5)

weightable(a)


b <- predict(glm(Peso..Kgs. ~ 1 + I(log(Edad)), data = beavers), newdata = data.frame(Edad = c(1:20)))

age_w<- data.frame(Weight = b, age = c(1:20))

ggplot(age_w, aes(x = age, y = Weight)) + geom_line()+ geom_smooth()