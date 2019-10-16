library(tidyverse)
library(broom)
Datos_crudos<-read.csv("HappinessAlcoholConsumption.csv")
view(Datos_crudos)

## selección de datos de interés ##

Tabla_1<-Datos_crudos %>% select(-Hemisphere,-HDI,-GDP_PerCapita)

Modelo_lm<-lm(HappinessScore~Beer_PerCapita+Spirit_PerCapita+Wine_PerCapita,data=Tabla_1)
summary(Modelo_lm)
Modelo_aov<-aov(HappinessScore~Beer_PerCapita+Spirit_PerCapita+Wine_PerCapita,data=Tabla_1)
summary(Modelo_aov)
glance(Modelo_lm)
view(glance(Modelo_lm))

tidy(Modelo_lm)



Tabla_2 <- Tabla_1 %>% gather(key = Alcohol_Type, value = Liter_per_capita, -Country, -Region, -HappinessScore)
view(Tabla_2)

ggplot(Tabla_2, aes(x = Liter_per_capita, y = HappinessScore))+ geom_point(aes(color = Alcohol_Type)) + 
  theme_classic() 

ggplot(Tabla_2, aes(x = Liter_per_capita, y = HappinessScore))+ geom_point(aes(color = Alcohol_Type)) + 
  theme_classic() + stat_smooth(aes(fill = factor(Alcohol_Type)), method = "lm", formula = y ~ 
                                x + I(x^2))

ggplot(Tabla_2, aes(x =Alcohol_Type, y = HappinessScore )) + geom_boxplot(aes(fill= Alcohol_Type),)+ geom_jitter(size = 0.7, shape = 8) + theme_classic()

Tipo_alcohol<-group_by(Tabla_2,Alcohol_Type)
Summary.Tipo_alcohol <- summarize(Tipo_alcohol, Promedio = round(mean(HappinessScore)), 
                               Desviacion_estandar = round(sd(HappinessScore)))

round((Summary.Tipo_alcohol[1,2]),digits = 3)

## Para graficar los datos en escala logarítmica ##

Tabla_3<-Tabla_2 %>% mutate(log_liter=log(Liter_per_capita))


ggplot(Tabla_3, aes(x = log_liter, y = HappinessScore)) + geom_point(aes(color = Alcohol_Type)) +
  theme_classic()

## Para graficar los datos asintóticas ## 


Tabla_3<-Tabla_2 %>% mutate(exp_liter=Liter_per_capita^2)


ggplot(Tabla_3, aes(x = exp_liter, y = HappinessScore)) + geom_point(aes(color = Alcohol_Type)) +
  theme_classic()

## Modelos lineales ##  

M1<-lm(HappinessScore~Alcohol_Type, data=Tabla_2)
M2<-lm(HappinessScore~Liter_per_capita, data=Tabla_2)
M3<-lm(HappinessScore~Region, data=Tabla_2)
M4<-lm(HappinessScore~Alcohol_Type + Liter_per_capita, data=Tabla_2)
M5<-lm(HappinessScore~Alcohol_Type + Region, data=Tabla_2)
M6<-lm(HappinessScore~Liter_per_capita + Region, data=Tabla_2)
M7<-lm(HappinessScore~Alcohol_Type + Liter_per_capita + Region, data=Tabla_2)

Modelo1 <- glance(M1) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M1")
Modelo2 <- glance(M2) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M2")
Modelo3 <- glance(M3) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M3")
Modelo4 <- glance(M4) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M4")
Modelo5 <- glance(M5) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M5")
Modelo6 <- glance(M6) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M6")
Modelo7 <- glance(M7) %>% dplyr::select(r.squared, AIC,df) %>% mutate(Modelo = "M7")

## Para juntar las tablas de los Modelos ln ##

Modelos <- bind_rows(Modelo1, Modelo2, Modelo3, Modelo4,Modelo5,Modelo6, Modelo7) %>% arrange(AIC) %>% mutate(DeltaAIC = AIC-min(AIC))
view(Modelos)

glance(M2)



### Modelo lineal generalizado con Gamma ###
M1G<-glm(HappinessScore~Alcohol_Type, data=Tabla_2, family = Gamma(link = "identity") )
M2G<-glm(HappinessScore~Liter_per_capita, data=Tabla_2, family = Gamma(link = "identity"))
M3G<-glm(HappinessScore~Region, data=Tabla_2, family = Gamma(link = "identity"))
M4G<-glm(HappinessScore~Alcohol_Type + Liter_per_capita, data=Tabla_2, family = Gamma(link = "identity"))
M5G<-glm(HappinessScore~Alcohol_Type + Region, data=Tabla_2, family = Gamma(link = "identity"))
M6G<-glm(HappinessScore~Liter_per_capita + Region, data=Tabla_2, family = Gamma(link = "identity"))
M7G<-glm(HappinessScore~Alcohol_Type + Liter_per_capita + Region, data=Tabla_2, family = Gamma(link = "identity"))

Modelo1G <- glance(M1G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M1G")
Modelo2G <- glance(M2G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M2G")
Modelo3G <- glance(M3G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M3G")
Modelo4G <- glance(M4G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M4G")
Modelo5G <- glance(M5G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M5G")
Modelo6G <- glance(M6G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M6G")
Modelo7G <- glance(M7G) %>% dplyr::select(AIC) %>% mutate(Modelo = "M7G")

Modelos_G <- bind_rows(Modelo1G, Modelo2G, Modelo3G, Modelo4G,Modelo5G,Modelo6G, Modelo7G) %>% arrange(AIC) %>% mutate(DeltaAIC = AIC-min(AIC))
view(Modelos_G)

## Comparación de ml y gml ## 
Modelos_lmygml <- bind_rows(Modelo1, Modelo2, Modelo3, Modelo4,Modelo5,Modelo6, Modelo7,Modelo1G, Modelo2G, Modelo3G, Modelo4G,Modelo5G,Modelo6G, Modelo7G) %>% arrange(AIC) %>% mutate(DeltaAIC = AIC-min(AIC))
view(Modelos_lmygml)

## Gráfica por región ##
Tabla_3<-Tabla_2 %>% mutate(log_liter=log(Liter_per_capita))

ggplot(Tabla_3, aes(x = Liter_per_capita, y = HappinessScore)) + stat_smooth(method = "glm", formula = y ~ I(x^2) + x) + geom_point() + facet_wrap(~ Region) + theme_bw()


ggplot(Tabla_3, aes(x = log_liter, y = HappinessScore)) + stat_smooth(method = "glm", formula = y ~ I(x^2) + x) + geom_point() + facet_wrap(~ Region) + theme_bw()


## Modelo M6G

SumM6G <- summary(M6G)

tabla.M6G<-broom::tidy(glm(M6G))
Tabla1_M6G<-tabla.M6G %>% select(term, estimate,p.value)
view(Tabla1_M6G)

## Tabla 10 países más felices

Paises<-group_by(Tabla_2,Country)
Paises2<-summarize(Paises,Liters_per_capita=sum(Liter_per_capita),felicidad=mean(HappinessScore))
Paises2 <- arrange(Paises2, -felicidad)
Paises3 <- Paises2%>% filter(felicidad >= 7.290)

Paises4 <- arrange(Paises2, -Liters_per_capita)
Paises4 <-Paises4 %>% filter(Liters_per_capita >= 600)

