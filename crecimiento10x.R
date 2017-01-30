library(magrittr)
c("dplyr", "tidyr", "xts", "forecast", "dygraphs", 
  "ggplot2", "lubridate", "astsa") %>% sapply(require, character.only=T)
####################################
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)
#                                      /|
####################################

###Cliente: SEAT  de México

# Análisis de datos -------------------------------------------------------
#### Número de usuarios que visitan más de 10 páginas en una visita
crecimiento10 = c(83533, 89085, 118360, 84614,115890, 96734,110060,110331,
                  88778, 100777, 121486)
meses = seq.POSIXt("2016-01-01" %>%  as.POSIXct, length.out = 11, by="month")
plot(meses, crecimiento10, type="l")
mes = seq_along(meses)
prediccion = seq(1,12)
modelo = lm(crecimiento10~mes)
summary(modelo)
plot(1:11, crecimiento10, type="l")
predict(modelo,mes=prediccion)

crecimiento10 = data.frame(meses, crecimiento10)
CTS10 = xts(crecimiento10[,-1], crecimiento10[,1])
CTS10 %>% acf2()
modelo = CTS10 %>%  ts %>% 
  Arima(order=c(1,0,0)) %>% 
  forecast(2) 
modelo$mean %>%  as.numeric()

meses = seq.POSIXt("2016-01-01" %>%  as.POSIXct, length.out = 13, by="month")
crecimiento10 = c(83533, 89085, 118360, 84614,115890, 96734,110060,110331,
                  88778, 100777, 121486, modelo$mean) %>%  round
data = data.frame(meses, crecimiento10)
data  %>% 
  ggplot(aes(x=meses, y= crecimiento10))+
  geom_point(color="darkred")+ geom_line(color="steelblue")+
  theme_bw()+
  geom_vline(xintercept =as.numeric(data[11,]), linetype=4)+
  annotate("text",x=as.Date(data[11,1]) %>%  as.POSIXct(), y=125000, 
           label="Inician pronósticos") +
  geom_text(aes(label = crecimiento10), 
            position=position_dodge(width=0.9), vjust=-0.35,
            col="darkred", size=3.5, fontface="bold")+
  xlab("Mes")+ ylab("Crecimiento")


  





























