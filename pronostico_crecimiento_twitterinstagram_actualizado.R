# Cargando librerías ------------------------------------------------------
library(magrittr)  
c("dplyr","tidyr", "lubridate", "ggplot2", "lattice", "XLConnect",
  "dygraphs","xts") %>% sapply(require, character.only=T)

####################################   
#Creado por Fernando Dorantes Nieto <(°) 
#                                     ( >)
#                                      /| 
####################################
# Inicio Pronóstico de crecimiento de Twitter, Instagram, Facebook --------

# Instagram ---------------------------------------------------------------
# datos_in = loadWorkbook(
#   "~/local/Sandra_TimeSeries/IG - Followers - SEAT México - 2016-09-05.xlsx")
# datos_in = loadWorkbook(
#   "~/local/Sandra_TimeSeries/IG - Followers - SEAT México - 2016-10-04.xlsx")
# datos_in = loadWorkbook(
#   "~/local/Sandra_TimeSeries/IG - Followers - SEAT México - 2016-10-14.xlsx")
datos_in = loadWorkbook(
  "~/local/Sandra_TimeSeries/IG - Followers - SEAT México - 2016-10-28.xlsx")

datos_in = readWorksheet(datos_in, sheet = 2, startRow = 2)
datos_in$Followers = datos_in$Followers  %>%  as.numeric
seguidores = xts(datos_in[,4], as.Date(datos_in[,1]))
seguidores = seguidores["2013-12-13/"]
fechas_ins = seq(ymd(20131213), by="day", length.out = length(seguidores[,1]))
seguidores = cbind(seguidores= seguidores, 
                   diferenciado = diff(seguidores),
                    cambio_seguidores = diff.xts(log(seguidores)))
seguidores[,1] %>%  dygraph()

TIME_diario = seq_along(seguidores[,1])
nlm = nls(seguidores[,1] ~ P0 * exp(R * TIME_diario), start = list(P0 = 1e3, 
                                                                   R = 0.005))

# TIME_PRED = 1:(length(TIME_diario) + 122)
#TIME_PRED = 1:(length(TIME_diario) + 90)
# TIME_PRED = 1:(length(TIME_diario) + 79)
TIME_PRED = 1:(length(TIME_diario) + 65)

pred.XIN = data.frame(
  fechas_ins = seq(ymd(20131213), by = "day",
                   length.out = length(seguidores[,1]) + 65),
  pronostico =predict(nlm, newdata = list(TIME_diario= TIME_PRED))
)
XIN = data.frame(fechas_ins, followers = seguidores[,1])
XIN =   pred.XIN %>% left_join(XIN, by = "fechas_ins")
XTSI = xts(XIN[, -1], XIN[, 1])
dygraph(XTSI)
crecimiento = (xts::last(pred.XIN[, 2]) - XIN[749, 2]) / XIN[749, 3] * 100
paste("Sandy, tu crecimiento es de ", crecimiento, "%")
XTSI = xts(XIN[, -1], XIN[, 1])

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

XTSI_gg  = cbind(fechas = time(XTSI), XTSI %>%  data.frame())
names(XTSI_gg) <- c("fechas", "pronostico", "seguidores")
XTSI_gg = XTSI_gg %>%  gather(tipo, valor, -fechas) 
ggplot(XTSI_gg, aes(x = fechas, y= valor, color = tipo)) + geom_line() +
  geom_point(size=0.1) + theme_bw() + ylab("Seguidores") + xlab("Tiempo") +
  scale_color_manual(values=c("#966842","#f44747"), name="Tipo",
                     labels=c("Pronóstico", "Seguidores" ))


# Intento del modelo logístico --------------------------------------------

prueba = XTSI["2015-01-01/"] 
tmp = prueba$Seguidores %>%  na.omit()  %>% as.numeric
tiempo = seq_along(tmp)
nlm = nls(
  tmp ~K *P0 *exp(R*tiempo) / (K + P0 *(exp(R*tiempo)-1)),
  start = list(
    P0 = min(tmp, na.rm=T),
    K = max(tmp, na.rm=T),
    R =0.1
    ),
  trace = T
)

tmp = XTSI$Seguidores %>% na.omit %>%  as.numeric()
tiempo = seq_along(tmp)
logistico1 = nls(
  tmp~K*P0*exp(R*tiempo)/(K+P0*(exp(R*tiempo)-1)),
  start = list(
    P0=min(tmp, na.rm = T),
    K = max(tmp, na.rm = T),
    R=0.2
  ),
  trace=T
)


dic_2015 = XTSI["2015-12-31/"] 
tmp = dic_2015$Seguidores %>%  na.omit()  %>% as.numeric
tiempo = seq_along(tmp)
nlm = nls(
  tmp ~K *P0 *exp(R*tiempo) / (K + P0 *(exp(R*tiempo)-1)),
  start = list(
    P0 = min(tmp, na.rm=T),
    K = max(tmp, na.rm = T),
    R =0.1
  ),
  trace = T
)

time_log = 1:(length(tiempo) + 65)
date_log = seq(ymd(20151231), by="day", length.out = max(time_log))
pred.Ins = data.frame(fechas_ins = date_log,
                    pronostico = predict(nlm, newdata = list(tiempo = time_log)))


crecimiento_instal = (xts::last(pred.Ins[, 2]) - tmp[1]) / 
  tmp[1]* 100

crecimiento_actual = (27069 - tmp[1]) / 
  tmp[1]* 100

paste("Sandy, tu crecimiento es de ", crecimiento_instal, "%")
paste("Sandy, tu crecimiento actual es de", crecimiento_actual, "%")
insta_logis = pred.Ins %>%  left_join(XIN, by = "fechas_ins")

insta_logis = xts(insta_logis[, -1], insta_logis[, 1])
insta_logis = insta_logis[,-2]

names(insta_logis) = c("Pronóstico de seguidores","Seguidores")

dygraph(insta_logis) %>%
  #   dySeries("Proporción de respuestas", axis = "y2") %>%
  #   dySeries("Estimación de proporción", axis = "y2") %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  # dyLegend(labelsSeparateLines = T, labelsDiv = "legendDiv") %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 

insta_logis %>%  head
insta_logisgg = cbind(fechas = time(insta_logis), insta_logis %>%
                        data.frame())
names(insta_logisgg)<-c("fechas", "pronostico", "seguidores")
insta_logisgg = insta_logisgg  %>%  gather(tipo, valor, -fechas)
ggplot(insta_logisgg, aes(x= fechas, y=valor, color=tipo)) +geom_line() +
  geom_point(size=0.1) + theme_bw() + ylab("Seguidores") + xlab("Tiempo")+
  scale_color_manual(values=c("#966842","#f44747"), name="Tipo", 
                     labels=c("Pronóstico", "Seguidores"))




# Comparación de modelos --------------------------------------------------
exponencial = nls(seguidores[,1] ~ P0 * exp(R * TIME_diario), 
                  start = list(P0 = 1e3, R = 0.005))

logistico = nls(
  tmp ~K *P0 *exp(R*tiempo) / (K + P0 *(exp(R*tiempo)-1)),
  start = list(
    P0 = min(tmp, na.rm=T),
    K = max(tmp, na.rm = T),
    R =0.1
  ),
  trace = T
)

exponencial = nls(tmp~P0*exp(R*tiempo), start = list(P0 = 5e3,
                                                     R=0.005))

exponencial
logistico

anova(exponencial, logistico, test="F")
anova(exponencial, logistico, test="F") %>%  summary

anova(logistico, exponencial, test="F")
anova(logistico, exponencial, test="F") %>%  summary

AIC(exponencial, k=2)
AIC(logistico)

BIC(exponencial)
BIC(logistico)
###probando el exponencial
time_log = 1:(length(tiempo) + 90)
time_log = 1:(length(tiempo) + 79)
date_log = seq(ymd(20151231), by="day", length.out = max(time_log))
pred.Ins = data.frame(fechas_ins = date_log,
                      pronostico = predict(exponencial,
                                           newdata = list(tiempo = time_log)))

crecimiento_instal = (xts::last(pred.Ins[, 2]) - tmp[1]) / 
  tmp[1]* 100

crecimiento_actual = (27069 - tmp[1]) / 
  tmp[1]* 100

paste("Sandy, tu crecimiento es de ", crecimiento_instal, "%")
paste("Sandy, tu crecimiento actual es de", crecimiento_actual, "%")
insta_logis = pred.Ins %>%  left_join(XIN, by = "fechas_ins")

insta_logis = xts(insta_logis[, -1], insta_logis[, 1])
insta_logis = insta_logis[,-2]

names(insta_logis) = c("Pronóstico de seguidores","Seguidores")

dygraph(insta_logis) %>%
  #   dySeries("Proporción de respuestas", axis = "y2") %>%
  #   dySeries("Estimación de proporción", axis = "y2") %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  # dyLegend(labelsSeparateLines = T, labelsDiv = "legendDiv") %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 






# Modelo exponencial secundario -------------------------------------------
prueba2 = XTSI$Seguidores["2015-01-01/"] %>%  na.omit()
prueba2 = prueba2  %>%  as.numeric()

TIME_p = seq_along(prueba2)
nlm = nls(prueba2 ~ P0 * exp(R * TIME_p), 
          start = list(P0 = min(prueba2), R = 0.005))

TIME_predp = 1:(length(TIME_p) + 123)
pred.ins1 = data.frame(
  fechas_ins1 = seq(ymd(20150101), by = "day", length.out = length(prueba2) + 123),
  pronostico =predict(nlm, newdata = list(TIME_p= TIME_predp))
)
fechas_ins1 = seq(ymd(20150101), by="day", length.out = length(prueba2))

instp1 = data.frame(fechas_ins1, followers = prueba2)
instp1 =   pred.ins1 %>% left_join(instp1, by = "fechas_ins1")
intsp1= xts(instp1[, -1], instp1[, 1])
dygraph(intsp1)
crecimiento = (last(pred.ins1)[, 2] - intsp1[365, 2]) / instp1[365, 2] * 100
paste("Sandy, tu crecimiento es de ", crecimiento, "%")



# prueba3 = XTSI$Seguidores["2015-09-01/"] %>%  na.omit()
# prueba3 = prueba3  %>%  as.numeric()
# 
# TIME_p = seq_along(prueba3)
# nlm = nls(prueba3 ~ P0 * exp(R * TIME_p), 
#           start = list(P0 = min(prueba3), R = 0.005))
# 
# TIME_predp = 1:(length(TIME_p) + 123)
# pred.ins1 = data.frame(
#   fechas_ins1 = seq(ymd(20150901), by = "day", length.out = length(prueba3) + 123),
#   pronostico =predict(nlm, newdata = list(TIME_p= TIME_predp))
# )
# fechas_ins1 = seq(ymd(20150901), by="day", length.out = length(prueba3))
# 
# instp1 = data.frame(fechas_ins1, followers = prueba3)
# instp1 =   pred.ins1 %>% left_join(instp1, by = "fechas_ins1")
# intsp1= xts(instp1[, -1], instp1[, 1])
# dygraph(intsp1)
# crecimiento = (last(pred.ins1)[, 2] - intsp1[122, 2]) / instp1[122, 2] * 100
# paste("Sandy, tu crecimiento es de ", crecimiento, "%")






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

nlm = nls(
  tmp ~ K * P0 * exp( R*tiempo) / (K + P0 * (exp(R * tiempo) -1) ),
  start = list(
    P0 = min(XIN$seguidores, na.rm = T),
    K = 27069,
    R = 0.1),
  trace = T
)

# Instagram datos mensuales -----------------------------------------------
XIN = c(1461, 1560,1664,1753,1808,2093,2115,2110,2154,2185,2258,2308,2311,2371,
        2456,2534,2607,2687,2791,2933,3166,3395,3628,3808,
        5676,7773,8949,13965, 16620, 19572, 21756, 24628, 27069)

segundo_p = c(1461, 1560,1664,1753,1808,2093,2115,2110,2154,2185,2258,2308,
              2311,2371,2456,2534,2607,2687,2791,2933,3166,3395,3628,
              3808,5676,7773,8949,13965, 16620, 19572, 21756, NA, NA)

primer_p= c(1461, 1560,1664,1753,1808,2093,2115,2110,2154,2185,2258,2308,
            2311,2371,2456,2534,2607,2687,2791,2933,3166,3395,3628,3808,
            5676,7773,8949,13965, NA, NA, NA, NA, NA)

fechas_i = seq(ymd(20131201), by = "month", length.out = length(XIN))

seguidores_mes= xts(XIN, as.Date(fechas_i))

seguidores_mes = cbind(seguidores = seguidores_mes,
                   primer_p = primer_p,
                   segundo_p = segundo_p,
                   diferenciadp = diff(seguidores_mes),
                   cambio_seguidores = diff.xts(log(seguidores_mes)))
seguidores_mes %>%  head
seguidores_mes[,1:3] %>%  dygraph

TIME = seq_along(XIN)
TIME

nlm = nls(XIN ~ P0 * exp(R * TIME), start = list(P0 = 1e3, R = 0.05))
TIME_PRED = 1:(length(TIME) + 4)
pred.XIN = data.frame(
  fechas_i = seq(ymd(20131201), by = "month", length.out = length(XIN) + 4 ),
  pronostico =predict(nlm, newdata = list(TIME = TIME_PRED))
)
pred.XIN
plot(pred.XIN)
xts(XIN, as.Date(fechas_i))
xts(pred.XIN[,2], pred.XIN[,1]) %>%  dygraph()

plot(pred.XIN[,2], type = "l")
lines(XIN, col= "red")
XIN = data.frame(fechas_i, XIN)

XIN
crecimiento = (last(pred.XIN)[, 2] - XIN[25, 2]) / XIN[25, 2] * 100
paste("Sandy, tu crecimiento es de ", crecimiento, "%")
XIN[25, 2] * 6.1265 + XIN[25, 2]

XIN = pred.XIN %>% left_join(XIN, by = "fechas_i")
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



##Twitter
XFEC= c(4083,7193,10228,16012,19026,22031,28035,34936,44457,56324,67554,
        78932,90672,102032,112258,117940,126038,136267,145316,153993,162819,
        173103,179550,189785,198571,203405,211850,214314,222214,230306,239369,
        245978,253638,262227,266832,272545,274919,277705,279705,281737,283622,
        285820,289047, 291477, 292951, 294510, 295990, 298862, 301303)
fechas_t = seq(ymd(20120801), ymd(20160801), by="month")
seguidores_t = xts(XFEC, as.Date(fechas_t))
seguidores_t = cbind(seguidores_t = seguidores_t,
                      dseguidores_t = diff(seguidores_t),
                      cambio = diff.xts(log(seguidores_t)))


seguidores_t %>%  dygraph
seguidores_t[,2:3] %>%  dygraph()
seguidores_tw= data.frame(fechas_t, XFEC)
ultimo_t = last(seguidores_tw[,1])
TIME_t = seq_along(seguidores_tw[,1])
nlm = 
  nls(seguidores_tw$XFEC ~K*P0*exp(R*TIME_t)/(K+P0 *(exp(R*TIME_t)-1)),
      start = list(P0 = min(seguidores_tw$XFEC, na.rm = T),
                   K  = max(seguidores_tw$XFEC, na.rm = T),
                   R = 0.5
                   )
) 
TIME_PRED = 1:(length(TIME_t) + 4)
DATE_PRED = seq(ymd(20120801), by="month", length.out = max(TIME_PRED))
pred.X = data.frame(fechas_t = DATE_PRED,
                    pronostico = predict(nlm, newdata = list(TIME_t = TIME_PRED))) 



# Followers3 = Followers2 %>% right_join(pred.X, by = "fechas")
# XTSf = xts(Followers3[, -1], Followers3[, 1])
# names(XTSf) <- c("Seguidores", "Pronóstico")
# k2 = coef(nlm)[2]
# modelototal1 = lm(Followers3[,2]~Followers3[,3])
# predict(modelototal1, newdata = list(daily = k2))
# max(predict(modelototal1, newdata = list(daily = k2)))
# dygraph(XTSf)%>%
#   dyOptions(drawGrid = F)


crecimiento_tw = (xts::last(pred.X[,2]) - seguidores_tw[41, 2]) / 
                seguidores_tw[41, 2]* 100
paste("Sandy, tu crecimiento es de ", crecimiento_tw, "%")

twit_crec = pred.X %>% left_join(seguidores_tw, by = "fechas_t")
twtc = xts(twit_crec[, -1], twit_crec[, 1])


names(twtc) = c("Pronóstico de seguidores","Seguidores")

dygraph(twtc) %>%
  #   dySeries("Proporción de respuestas", axis = "y2") %>%
  #   dySeries("Estimación de proporción", axis = "y2") %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  # dyLegend(labelsSeparateLines = T, labelsDiv = "legendDiv") %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 
twtc_gg = cbind(fechas = time(twtc), twtc %>% data.frame)
names(twtc_gg) <-c("Fechas", "pronostico", "seguidores") 
twtc_gg = twtc_gg  %>%  gather(tipo, valor, -Fechas)

ggplot(twtc_gg, aes(x = Fechas, y= valor, color = tipo)) + geom_line() +
  geom_point(size=0.5) + theme_bw() + ylab("Seguidores") + xlab("Tiempo") +
  scale_color_manual(values= c("#326ada", "#433e90"), name="Tipo",
                     labels=c("Pronóstico", "Seguidores"))



seguidores_tw %>%  tail


TIME_t = seq_along(seguidores_tw[,1])
nlm = 
  nls(seguidores_tw$XFEC ~K*P0*exp(R*TIME_t)/(K+P0 *(exp(R*TIME_t)-1)),
      start = list(P0 = min(seguidores_tw$XFEC, na.rm = T),
                   K  = max(seguidores_tw$XFEC, na.rm = T),
                   R = 0.5
      )
  ) 

nuevo_modelo =   nls(seguidores_tw$XFEC ~K*P0*exp(R*TIME_t)/(K+P0 *(exp(R*TIME_t)-1)),
      start = list(P0 = min(seguidores_tw$XFEC, na.rm = T),
                   K  = max(seguidores_tw$XFEC, na.rm = T),
                   R = 0.6
      )
  ) 


TIME_PRED = 1:(length(TIME_t) + 4)
DATE_PRED = seq(ymd(20120801), by="month", length.out = max(TIME_PRED))
pred.X = data.frame(fechas_t = DATE_PRED,
                    pronostico = predict(nlm, newdata = list(TIME_t = TIME_PRED))) 


# Twitter diarios ---------------------------------------------------------
# datos_tw = loadWorkbook(
#   "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-09-06.xlsx")
# datos_tw = loadWorkbook(
#   "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-10-04.xlsx")
# datos_tw = loadWorkbook(
#   "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-10-14.xlsx")
# datos_tw = loadWorkbook(
#   "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-10-28.xlsx")
# datos_tw = loadWorkbook(
#   "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-11-10.xlsx")
datos_tw = loadWorkbook(
  "~/local/Sandra_TimeSeries/TW - Followers - SEAT México - 2016-11-23.xlsx"
)

datos_tw = readWorksheet(datos_tw, sheet = 2, startRow = 2)
datos_tw$Followers = datos_tw$Followers %>%  as.numeric()
seguidores_t = xts(datos_tw[,4], as.Date(datos_tw[,1]))
seguidores_t = seguidores_t["2012-08-31/"]
fechas_tw = seq(ymd(20120831), by="day", length.out = length(seguidores_t[,1]))
seguidores_t = cbind(seguidores= seguidores_t,
                   diferenciado= diff(seguidores_t),
                   cambio_seguidores = diff.xts(log(seguidores_t)))
ultimo_tw = index(xts::last(seguidores_t))
TIME_tw = seq_along(seguidores_t[,1])
seguidores_t = data.frame(fechas_tw, seguidores = 
                            seguidores_t$seguidores %>%  as.numeric())
numeros = seguidores_t$seguidores
logistico_tw =
  nls(
  numeros  ~
    K *P0 *exp(R*TIME_tw) / (K+ P0 *(exp(R*TIME_tw)-1)),
  start = list(
    P0 = min(seguidores_t$seguidores, na.rm = T),
    K  = max(seguidores_t$seguidores, na.rm = T),
    R = 0.1
      ),
  trace = T
  ) 

logistico_tw =
  nls(
  numeros  ~
    K *P0 *exp(R*TIME_tw) / (K+ P0 *(exp(R*TIME_tw)-1)),
  start = list(
    P0 = min(seguidores_t$seguidores, na.rm = T),
    K  = max(seguidores_t$seguidores, na.rm = T),
    R = 0.02
      ),
  trace = T
  ) 

logistico_tw=
  nls(
    numeros~K*P0*exp(R*TIME_tw)/(K+P0*(exp(R*TIME_tw)-1)),
    start = list(
      # P0= min(seguidores_t$seguidores, na.rm = T),
      P0= 1,
      K=max(seguidores_t$seguidores, na.rm = T),
      R=0.000001
    ),
    trace = T, algorithm = "port"
  )

logistico_tw %>%  summary()

# TIME_PTW = 1:(length(TIME_tw) + 90)
# TIME_PTW = 1:(length(TIME_tw) + 79)
# TIME_PTW = 1:(length(TIME_tw) + 65)
# TIME_PTW = 1:(length(TIME_tw) + 57)
TIME_PTW = 1:(length(TIME_tw) + 39)

DATE_Ptw = seq(ymd(20120831), by="day", length.out = max(TIME_PTW))
pred.tw = data.frame(fechas_tw = DATE_Ptw,
                    pronostico = predict(logistico_tw, 
                                         newdata = list(TIME_tw = TIME_PTW))) 

twitter_logis = pred.tw %>%  left_join(seguidores_t, by="fechas_tw")
twitter_logis = xts(twitter_logis[,-1], twitter_logis[,1])
names(twitter_logis) <- c("Pronóstico de  seguidores", "Seguidores")

dygraph(twitter_logis) %>%
  #   dySeries("Proporción de respuestas", axis = "y2") %>%
  #   dySeries("Estimación de proporción", axis = "y2") %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  # dyLegend(labelsSeparateLines = T, labelsDiv = "legendDiv") %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 

crecimiento_twitter = (xts::last(pred.tw[,2] - seguidores_t[1218,2])) /
  seguidores_t[1218,2]*100

# crecimiento_twitter = ((last(pred.tw[,2]) - seguidores_t[1218,2])) /
#   seguidores_t[1218,2]*100
# 
# crecimiento_tactual = (300620 - seguidores_t[1218,2]) / 
#   seguidores_t[1218,2]* 100

crecimiento_tactual = (xts::last(seguidores_t[,2]-seguidores_t[1218,2]))/
  seguidores_t[1218,2]*100

paste("Sandy, tu crecimiento es de ", crecimiento_twitter, "%")
paste("Sandy, tu crecimiento actual es de", crecimiento_tactual, "%")

twitter_logis_gg = cbind(time(twitter_logis), twitter_logis %>%  
  data.frame)
names(twitter_logis_gg) <- c("fechas", "pronostico", "seguidores")
twitter_logis_gg = twitter_logis_gg %>%  gather(tipo, valor, -fechas) 

ggplot(twitter_logis_gg, aes(x=fechas, y= valor, color = tipo)) +
  geom_line() + theme_bw() + ylab("Seguidores") + xlab("Tiempo")+
  scale_color_manual(values=c("#966842","#f44747"), name="Tipo",
                     labels=c("Pronóstico", "Seguidores" )) +
  scale_x_date(date_breaks = "1 year") +scale_y_continuous(labels = 
                                                             scales::comma)
  




# Pronóstico 2015 ---------------------------------------------------------
seguidores_t1 = xts(datos_tw[,4], as.Date(datos_tw[,1]))
seguidoresT15 = seguidores_t1["2015-12-01/"] 
fechas_tw15 = seq(ymd(20151201), by="day", 
                  length.out = length(seguidoresT15[,1]))
seguidoresT15 = cbind(seguidores = seguidoresT15, 
                      diferenciado = diff(seguidoresT15),
                      cambio_seguidores= diff.xts(log(seguidoresT15)))
ultimotw15 = index(xts::last(seguidoresT15))
TIME_tw15 = seq_along(seguidoresT15[,1])
seguidoresT15 = data.frame(fechas_tw = fechas_tw15, seguidores = 
                            seguidoresT15$seguidores %>%  as.numeric())
numeros15 = seguidoresT15$seguidores

logisticoT15 =
  nls(
    numeros15  ~
      K *P0 *exp(R*TIME_tw15) / (K+ P0 *(exp(R*TIME_tw15)-1)),
     start = list(
      P0 = min(seguidoresT15$seguidores, na.rm = T),
      K  = max(seguidoresT15$seguidores, na.rm = T),
      R = 0.001
    ),
    trace = T
  ) 
TIME_PTW15  = 1:(length(TIME_tw15)+39)
DATE_Ptw15 = seq(ymd(20151201), by="day", length.out = max(TIME_PTW15))
pred.tw15 = data.frame(fechas_tw=DATE_Ptw15,
                       pronostico = predict(logisticoT15,
                                            newdata = list(TIME_tw15 = TIME_PTW15)))
twitter_logis15 = pred.tw15 %>%  left_join(seguidoresT15, by="fechas_tw")
twitter_logis15 = xts(twitter_logis15[,-1], twitter_logis15[,1])
names(twitter_logis15) <- c("Pronóstico de  seguidores", "Seguidores")

crecimiento_twitter = (xts::last(pred.tw15[,2] - seguidoresT15[31,2])) /
  seguidoresT15[31,2]*100

crecimiento_tactual = (xts::last(seguidoresT15[,2]-seguidoresT15[31,2]))/
  seguidoresT15[31,2]*100

paste("Sandy, tu crecimiento es de ", crecimiento_twitter, "%")
paste("Sandy, tu crecimiento actual es de", crecimiento_tactual, "%")

dygraph(twitter_logis15) %>%
  dyLegend("always", labelsSeparateLines = T, hideOnMouseOut = T) %>% 
  dyHighlight(highlightSeriesOpts = list(strokeWidth = 3),
              highlightSeriesBackgroundAlpha = 0.2,
              hideOnMouseOut = T) %>%
  dyOptions(drawGrid = F) 
twitter_logis_gg15 = cbind(time(twitter_logis15), twitter_logis15 %>% 
                             data.frame())
names(twitter_logis_gg15) <- c("fechas", "pronostico", "seguidores")
twitter_logis_gg15= twitter_logis_gg15 %>%  gather(tipo, valor, -fechas) 

ggplot(twitter_logis_gg15, aes(x=fechas, y= valor, color = tipo)) +
  geom_line() + theme_bw() + ylab("Seguidores") + xlab("Tiempo")+
  scale_color_manual(values=c("#966842","#f44747"), name="Tipo",
                     labels=c("Pronóstico", "Seguidores" )) +
  scale_x_date(date_breaks = "2 months") +scale_y_continuous(labels = 
                                                             scales::comma)

