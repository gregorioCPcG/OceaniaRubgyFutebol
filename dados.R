# now

pais <- c("NZ","I.Salomão", "N. Caledonia","Taiti","Fiji","Vanuatu","Papua Nova Guinea","I.Cook")
futebol <- c(1165.6, 1072.78,1035.12,1014.27,996.27,995.62,990.55,908) # ranking fifa dez 2021
# para ilhas cook ultima pontuação conhecida em Ago de 2019
rugby <- c(88.75,23.81,0,0,76.62,15.45,33.68,45.11) # ranking novembro 2021 https://www.world.rugby/tournaments/rankings/mru
pib <- c(204,1.3,8.85,14.33,5.06,0.863,20.54,0.384) # fontes diversas, dado mais atual encontrado
base <- data.frame(futebol,pib,rugby)

cor(base$futebol, base$rugby)
plot(base$futebol, base$rugby,xlab = "Força no Futebol", ylab = "Força no Rugby")

library(ggplot2)

f <- ggplot(base, aes(futebol, rugby))
f + geom_point() +  xlab("Apoio à esquerda em 2020") +
  ylab("% votos válidos Ciro 2018 - turno 1")
f + geom_text(aes(label = pais))

matriz <- cor(base)
require(corrplot)
corrplot(matriz, method="number", 
         type="upper", order="original",
         tl.col="black", tl.srt=45)

cor(base$futebol, base$rugby)
cor.test(base$futebol, base$rugby)
cor.test(base$futebol, base$pib)
cor.test(base$rugby, base$pib)

# regressão linear para futebol
log_pib <- log(pib)
base <- data.frame(futebol,log_pib,rugby)
model <- lm(futebol ~ rugby + log_pib, data = base)
summary(model)
model2 <- lm(rugby ~ futebol + log_pib, data = base)
summary(model2)
