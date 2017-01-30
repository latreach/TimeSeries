library(magrittr)
c("dplyr","tidyr", "xts", "astsa",  "forecast", "dygraphs", "lubridate", 
  "ggplot2", "lattice") %>%
  sapply(require, character.only = T)
# Carga de datos ----------------------------------------------------------
anio_2016 = read.csv("~/local/Sandra_TimeSeries/2016.csv", header = T)
anio_2016 = anio_2016 %>%   mutate(Date = as.Date(Date))
# septiembre = read.csv("~/local/Sandra_TimeSeries/Septiembre-Octubre.csv", header = T)
# septiembre = septiembre %>%  mutate(Date= as.Date(Date), mes = month(Date), anio= year(Date))
# septiembre %>%  head
# octubre = read.csv("~/local/Sandra_TimeSeries/Octubre2016.csv", header = T)
# octubre = octubre %>% mutate(Date= as.Date(Date, format = "%m/%d/%y"), 
#                              mes = month(Date), anio= year(Date))
# anio_2016 = rbind(anio_2016[-274:-287,], octubre)
# write.csv(anio_2016, "~/local/Sandra_TimeSeries/2016.csv", row.names = F)

TS = read.csv("~/local/Sandra_TimeSeries/2016.csv", row.names = 1) %>%
  select(Daily.Daily.count.of.fans.online) %>%
  rename(Fans_Online = Daily.Daily.count.of.fans.online) %>% 
  xts(order.by = as.Date(rownames(.))) %>%
  .["2016-02/"]

# anterior = read.csv("~/local/Sandra_TimeSeries/2016.csv", row.names = 1) %>%
#   select(Daily.Daily.count.of.fans.online) %>%
#   rename(Fans_Online = Daily.Daily.count.of.fans.online) %>% 
#   xts(order.by = as.Date(rownames(.))) %>%
#   .["2016-02/2016-09"]


# anterior = read.csv("~/local/Sandra_TimeSeries/2016.csv", row.names = 1) %>%
#   select(Daily.Daily.count.of.fans.online) %>%
#   rename(Fans_Online = Daily.Daily.count.of.fans.online) %>% 
#   xts(order.by = as.Date(rownames(.))) %>%
#   .["2016-02/2016-08"]


# anterior = read.csv("~/local/Sandra_TimeSeries/2016.csv", row.names = 1) %>%
#   select(Daily.Daily.count.of.fans.online) %>%
#   rename(Fans_Online = Daily.Daily.count.of.fans.online) %>% 
#   xts(order.by = as.Date(rownames(.))) %>%
#   .["2016-02/2016-06"]


# Tendencias (Normal y diferenciadas) -------------------------------------
#par(mfrow = c(3,1))
TS %>%  na.omit %>% plot
TS %>%  na.omit %>% diff %>% plot
TS %>%  na.omit %>% diff(7) %>% plot

# ACF Y PACF --------------------------------------------------------------
TS %>% acf2
TS %>%  na.omit %>% diff %>% acf2(na.action = na.omit)
TS %>%  na.omit%>% diff(7) %>% acf2(na.action = na.omit)

par(mfrow= c(2,1))
anterior %>%  diff %>%  plot
TS %>%  diff %>%  plot

par(mfrow= c(2,1))
anterior %>%  diff(7) %>%  plot
TS %>%  diff(7) %>% plot  

TS %>%  na.omit %>% 
   coredata %>% 
   ts(frequency = 7) %>% 
   auto.arima(
     stepwise = F, 
     allowdrift = T, 
    trace = T, 
     stationary = F
   )
 
######## sin tendencia
 TS %>%  na.omit() %>%
   coredata %>% 
   ts(frequency = 7) %>%
   Arima(order = c(1, 0, 1), seasonal = c(1, 1, 1)) %>% 
   forecast(h = 67) %>%
   plot
 #antes 122
#tendencioso
 TS %>%  na.omit %>%  
   coredata %>%  
   ts(frequency = 7) %>% 
   Arima(order=c(1,0,1), seasonal = c(1,1,1), include.drift = T) %>% 
   forecast(67) %>% 
   plot
 
##modelo 1
modelo1 = TS %>%  na.omit %>% 
   coredata %>%  
   ts(frequency = 7) %>% 
   Arima(order=c(1,0,1), seasonal = c(1,1,1)) %>% 
   forecast(h = 65) 
 
##modelo2
modelo2 = TS %>%  na.omit %>% 
    coredata %>%  
    ts(frequency = 7) %>% 
    Arima(order = c(1,0,1), seasonal = c(1,1,1), include.drift = T) %>% 
    forecast(h=65)


##creando las gráficas
ultimo = index(xts::last(TS)) %>% as.POSIXct()
pronostico1 = xts(modelo1$mean, seq.POSIXt(ultimo +1, length.out = 65, 
                                           by= "day"))
grafo1 = cbind(TS %>%  na.omit(), pronostico1)
names(grafo1) <- c("Crecimiento observado de Facebook", "Estimación")
dygraph(grafo1) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut= T) %>% 
  dyAxis("y", label="Fans online") %>% 
  dyOptions(drawGrid = F) %>% 
  dyEvent(ultimo, label= "Inician Pronósticos")

pronostico2 = xts(modelo2$mean, seq.POSIXt(ultimo + 1, length.out = 65,
                                           by="day"))
grafo2 = cbind(TS, pronostico2)
names(grafo2) <- c("Crecimiento observado de Facebook", "Estimación")
dygraph(grafo2)  %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut= T) %>% 
  dyAxis("y", label="Fans online") %>% 
  dyOptions(drawGrid = F) %>% 
  dyEvent(ultimo, label= "Inician Pronósticos")

# crecimiento_m1 = (1247060 -  1247625)/ 1247625*100
# paste("Sandy, tu crecimiento es de ", crecimiento_m1, "%")
crecimiento_m1 = (1256201 -  1247625)/ 1247625*100
paste("Sandy, tu crecimiento es de ", crecimiento_m1, "%")
# crecimiento_m2 = (1275088 -  1247625)/ 1247625*100
# paste("Sandy, tu crecimiento es de ", crecimiento_m2, "%")

crecimiento_m2 = (1272064 -  1247625)/ 1247625*100
paste("Sandy, tu crecimiento es de ", crecimiento_m2, "%")

crecimiento_m3 = (1259480 -  1247625)/ 1247625*100
paste("Sandy, tu crecimiento actual es de ", crecimiento_m3, "%")


grafo2 %>%  head
grafo2= grafo2[-243,]
grafo2_gg = cbind(fechas = time(grafo2), grafo2  %>%  data.frame)
names(grafo2_gg) <-c("Fecha", "Observado", "Estimado")
grafo2_gg =   grafo2_gg %>%  gather(tipo, valor, -Fecha) 

ggplot(grafo2_gg, aes(x = Fecha, y= valor, color = tipo)) + geom_line() +
  geom_point(size=0.1) + theme_bw() + ylab("Fans online") + xlab("Tiempo")+
  geom_vline(xintercept = as.numeric(grafo2_gg[603,]), linetype=4) +
   ggplot2::annotate("text",x=as.Date("2016-09-29"), y=1230000, 
            label="Inician pronósticos") + 
  scale_color_manual(values= c("#3B5998", "#8B9DC3"),name="Tipo")



grafo1= grafo1[-243,]
grafo1_gg = cbind(fechas =time(grafo1), grafo1 %>%  data.frame)
names(grafo1_gg)<-c("Fecha", "Observado", "Estimado")
grafo1_gg = grafo1_gg %>%  gather(tipo, valor, -Fecha)

ggplot(grafo1_gg, aes(x= Fecha, y=valor, color=tipo)) + geom_line() +
geom_point(size=0.1) + theme_bw() + ylab("Fans online") + xlab("Tiempo")+
  geom_vline(xintercept = as.numeric(grafo2_gg[603,]), linetype=4) +
  ggplot2::annotate("text",x=as.Date("2016-09-29"), y=1230000, 
           label="Inician pronósticos") +
  scale_color_manual(values= c("#3B5998", "#8B9DC3"),name="Tipo")



TS = read.csv("~/local/Sandra_TimeSeries/2016.csv", row.names = 1) %>%
  select(Daily.Daily.count.of.fans.online) %>%
  rename(Fans_Online = Daily.Daily.count.of.fans.online) %>% 
  xts(order.by = as.Date(rownames(.))) %>%
  .["2016-02/"]

##fans totales
datos_FB = loadWorkbook(
  "~/local/Sandra_TimeSeries/FB - Fans - SEAT México - 2016-10-26.xlsx")
datos_FB = readWorksheet(datos_FB, sheet = 2, startRow = 2)
datos_FB$Fans = datos_FB$Fans  %>%  as.numeric
datos_FB = datos_FB   %>% mutate(Date = as.Date(Date), anio = year(Date), 
                                 mes = month(Date), dia = day(Date))
seguidores = xts(datos_FB[,4], as.Date(datos_FB[,1]))
seguidores= seguidores
seguidores %>% dygraph()
dygraph(seguidores)  %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut= T) %>% 
  dyAxis("y", label="Fans Totales") %>% 
  dyOptions(drawGrid = F, colors = "darkblue") 

ggplot(datos_FB, aes(x = as.Date(Date),y = Fans)) +
  geom_line(col="darkblue")+
  scale_x_date(date_breaks = "2 months") + theme_bw()+
  xlab("Tiempo") + theme(axis.text.x = element_text(angle=45, hjust=1))


crecimiento_totales = (1603149 -  1574514)/ 1574514*100
crecimiento_totales






