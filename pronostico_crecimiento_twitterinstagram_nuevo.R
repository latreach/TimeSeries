# librerías ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(dygraphs)
library(xts)
library(lubridate)
library(XLConnect)
# Twitter -----------------------------------------------------------
XFEC= c(4083,7193,10228,16012,19026,22031,28035,34936,44457,56324,67554,
        78932,90672,102032,112258,117940,126038,136267,145316,153993,162819,
        173103,179550,189785,198571,203405,211850,214314,222214,230306,239369,
        245978,253638,262227,266832,272545,274919,277705,279705,281737,283622,
        285820,289047, 291477, 292951, 294510, 295990)
fechas = seq(ymd(20120801), ymd(20160601), by = "month")
Followers = xts(XFEC, as.Date(fechas))
Followers = cbind(Followers = Followers,
                  dFollowers = diff(Followers),
                  cambio = diff.xts(log(Followers)))
Followers %>%  dygraph
Followers2 = data.frame(fechas, XFEC)
ultimof = last(Followers2[,1])
TIME = seq_along(Followers2$fechas)
nlm = nls(Followers2$XFEC ~ K * P0 * exp(R*TIME) / (K + P0 * (exp(R * TIME) - 1)),
          start = list(P0 = min(Followers2$XFEC, na.rm = T),
                       K = max(Followers2$XFEC, na.rm = T),
                       R = 0.1))

TIME_PRED = 1:(length(TIME) + 12);
DATE_PRED = seq(ymd(20120801), by = "month", length.out = max(TIME_PRED))
pred.X = data.frame(fechas = DATE_PRED,
                    pronostico = predict(nlm, newdata = list(TIME = TIME_PRED)))
Followers3 = Followers2 %>% right_join(pred.X, by = "fechas")

XTSf = xts(Followers3[, -1], Followers3[, 1])
names(XTSf) <- c("Seguidores", "Pronóstico")
k2 = coef(nlm)[2]
modelototal1 = lm(Followers3[,2]~Followers3[,3])
predict(modelototal1, newdata = list(daily = k2))
max(predict(modelototal1, newdata = list(daily = k2)))
dygraph(XTSf)%>%
  dyOptions(drawGrid = F)


crecimiento_tw = (last(pred.X)[, 2] - Followers2[41, 2]) / Followers2[41, 2] * 100
paste("Sandy, tu crecimiento es de ", crecimiento_tw, "%")



# Instagram ---------------------------------------------------------------
XIN = c(1461, 1560,1664,1753,1808,2093,2115,2110,2154,2185,2258,2308,2311,2371,2456,2534,2607,2687,2791,
        2933,3166,3395,3628,3808,5676,7773,8949,13965, 16620, 19572, 21756)

antes = c(1461, 1560,1664,1753,1808,2093,2115,2110,2154,2185,2258,2308,2311,2371,2456,2534,2607,2687,2791,
        2933,3166,3395,3628,3808,5676,7773,8949,13965, NA, NA, NA)

fechasi = seq(ymd(20131201), by = "month", length.out = length(XIN))

seguidores= xts(XIN, as.Date(fechasi))

seguidores = cbind(seguidores = seguidores,
                   antes = antes,
                  diferenciadp = diff(seguidores),
                  cambio_seguidores = diff.xts(log(seguidores)))
seguidores %>%  head
seguidores[,1:2] %>%  dygraph


TIME = seq_along(XIN)

# Modelo exponencial ------------------------------------------------------
nlm = nls(XIN ~ P0 * exp(R * TIME), start = list(P0 = 1e3, R = 0.05))
TIME_PRED = 1:(length(TIME) + 6)
pred.XIN = data.frame(
  fechasi = seq(ymd(20131201), by = "month", length.out = length(XIN) + 6 ),
  pronostico =predict(nlm, newdata = list(TIME = TIME_PRED))
)
pred.XIN
plot(pred.XIN)
xts(XIN, as.Date(fechasi))
xts(pred.XIN[,2], pred.XIN[,2])

plot(pred.XIN[,2], type = "l")
lines(XIN, col= "red")


# diff(XIN[,2]) %>%  plot
# XIN[,2] %>%  points
# 
# par(mfrow = c(1,2))
# diff(XIN[,2]) %>%  plot
# XIN[,2] %>%  plot

XIN = data.frame(fechasi, followers = XIN)
XIN
#tomando como punto Junio 2016
  crecimiento = (last(pred.XIN)[, 2] - XIN[31, 2]) / XIN[31, 2] * 100
  paste("Sandy, tu crecimiento es de ", crecimiento, "%")
  XIN[31, 2] * 6.1265 + XIN[31, 2]

#tomando como punto diciembre de 2015
#sabiendo que teniamos 5676 seguidores en diciembre 2015
crecimiento = (last(pred.XIN)[, 2] - XIN[25, 2]) / XIN[25, 2] * 100
paste("Sandy, tu crecimiento es de ", crecimiento, "%")
XIN[25, 2] * 6.1265 + XIN[25, 2]

XIN = pred.XIN %>% left_join(XIN, by = "fechasi")
XTSI = xts(XIN[, -1], XIN[, 1])

dygraph(XTSI)

names(XTSI) = c("Pronóstico de seguidores","Seguidores")

dygraph(XTSI) %>%
  #   dySeries("Proporción de respuestas", axis = "y2") %>%
  #   dySeries("Estimación de proporción", axis = "y2") %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  # dyLegend(labelsSeparateLines = T, labelsDiv = "legendDiv") %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 


# Modelo logístico -----------------------------------------------------------
# tmp = XIN$followers %>% na.omit()
# tiempo = seq_along(tmp)
# nlm = nls(
#   tmp ~ K * P0 * exp(R*tiempo) / (K + P0 * (exp(R * tiempo) - 1)),
#   start = list(
#     P0 = min(XIN$followers, na.rm = T),
#     K = 7e4,
#     R = 0.05),
#   trace = T
# )
# 


# Instagram datos diarios -------------------------------------------------
datos_in = loadWorkbook("~/local/Sandra TimeSeries/IG - Followers - SEAT México - 2016-07-13.xlsx")
datos_in = readWorksheet(datos_in, sheet = 2, startRow = 2)
datos_in$Followers = datos_in$Followers %>%  as.numeric
seguidores = xts(datos_in[,4], as.Date(datos_in[,1]))
seguidores = seguidores["2013-12-13/"]
fechasi = seq(ymd(20131213), by = "day", length.out = length(seguidores[,1]))

##exploratorio
seguidores = cbind(seguidores = seguidores,
                   diferenciadp = diff(seguidores),
                   cambio_seguidores = diff.xts(log(seguidores)))
seguidores %>%  head
seguidores[,1:2] %>%  dygraph


# Modelo exponencial_2 ----------------------------------------------------
TIME = seq_along(seguidores[,1])
nlm = nls(seguidores[,1] ~ P0 * exp(R * TIME), start = list(P0 = 1e3, R = 0.005))

TIME_PRED = 1:(length(TIME) + 172)
pred.XIN = data.frame(
  fechasi = seq(ymd(20131213), by = "day", length.out = length(seguidores[,1]) + 172),
  pronostico =predict(nlm, newdata = list(TIME = TIME_PRED))
)
XIN = data.frame(fechasi, followers = seguidores[,1])
XIN =   pred.XIN %>% left_join(XIN, by = "fechasi")
XTSI = xts(XIN[, -1], XIN[, 1])
dygraph(XTSI)
crecimiento = (last(pred.XIN)[, 2] - XIN[749, 2]) / XIN[749, 3] * 100
paste("Sandy, tu crecimiento es de ", crecimiento, "%")

# Modelo logístico --------------------------------------------------------
tmp = XIN$seguidores %>% na.omit()
tiempo = seq_along(tmp)
nlm = nls(
    tmp ~ K * P0 * exp(R*tiempo) / (K + P0 * (exp(R * tiempo) - 1)),
    start = list(
      P0 = min(XIN$seguidores, na.rm = T),
      K = 22557,
      R = 0.1),
      trace = T
   )






