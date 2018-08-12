
#################################################################################################
##########                    PREPARACIÓN DE LOS DATOS: PREPROCESADO                     ########
#################################################################################################


# instalar las librerías que serán utilizadas en el preprocesado



#Preprocesado <- function() {


  #PAQUETES NECESARIOS

  
  #Set working directory
  setwd("C:/Users/yanay/Desktop/TFM/Proyecto R/Cars")
  
  #Cargar los datos
  library(RSQLite)
  con = dbConnect(RSQLite::SQLite(), dbname="./data/cars.db")
  alltables = dbListTables(con)
  p1 = dbGetQuery( con,'SELECT * FROM simplecars' )
  p2 = dbGetQuery( con,'SELECT * FROM fullcars' )
  p3 = dbGetQuery( con,'SELECT COUNT(*) FROM fullcars' )
  #p4 = dbExecute( con, "IF NOT EXISTS (SELECT * FROM sysobjects WHERE name='tbl_name') CREATE TABLE tbl_name ( Name VARCHAR(64) NOT NULL) GO")
  dbDisconnect(con)
  
  #segundo dataframe
  library(RSQLite)
  con = dbConnect(RSQLite::SQLite(), dbname="./data/cars2.db")
  alltables = dbListTables(con)
  p2.1 = dbGetQuery( con,'SELECT * FROM fullcars' )
  dbDisconnect(con)
  
  dim(p2.1)
  #Se crea el dataframe con el que se va a trabajar 
  cars.data = as.data.frame(p2.1)
  
  ###################################
  #------ Limpiar los datos: --------
  ###################################
  
  #Se eliminan las variables inútiles para el análisis
  #cars.data$superID <- NULL
  cars.data$pageID <- NULL
  cars.data$carID <- NULL
  #cars.data$link <- NULL
  
  #se eliminan temporalmente las variables description y equipment. Estas variables pueden ser utilizadas para text mining
  cars.data$description <- NULL
  cars.data$equipment <- NULL
  
  #Análisis inicial, idea general sobre los datos
  dim(cars.data) #observaciones y variables
  sum(is.na(cars.data)) # no missing values aparentemente
  names(cars.data)
  str(cars.data) #todos los datos han tomado el tipo char, se sabe q esto no es cierto
  
  
  
  #--------- Análisis y transformación (de ser necesario) de las variables: -----------
  
  library(ggplot2)
  
  ###### Variable:"brand" ###### 
  str(cars.data$brand) #tipo de datos char
  sort(table(cars.data$brand), decreasing = TRUE) #la muestra está sesgada a lo coches con mayor número de apariciones. Ejemplo Bmw,Audi,Citroen. Mientras que existen otros coches con muy pocas apariciones 
  
  ggplot(data=cars.data, aes(brand)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  cars.data$brand <- as.factor(cars.data$brand)
  
  ###### Variable:"sbrand" ###### Nota: esta variable está pendiente de análisis
  cars.data$sbrand <- as.factor(cars.data$sbrand)
  
  ###### Variable:"model" ###### Nota: esta variable está pendiente de análisis
  cars.data$model <- as.factor(cars.data$model)
  
  
  
  ###### Variable:"price" ######
  str(cars.data$price) #dice q la variable es de dipo char y esto no es cierto
  unique(cars.data$price)
  sum(cars.data$price == "") 
  
  price.cero <- cars.data[cars.data$price == "",] #estos registros tienen casi todos los valores en cero, se busca por el link y se descubre que ya no exixte ese link.
  cars.data$price[cars.data$price==""] <- NA #se convierte a NA los valores vacíos
  sum(is.na(cars.data$price))
  cars.data <- na.omit(cars.data) #elimiando toda el registro
  dim(cars.data)
  
  cars.data$price <- gsub("€", "", cars.data$price) #eliminando el símbolo de euro de los valores de la variable
  cars.data$price <- gsub("\\.", "", cars.data$price) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  
  cars.data$price <- as.numeric(cars.data$price) #convirtiendo los valores a numéricos
  
  summary(cars.data$price) #se observa un máximo muy elevado
  cars.data[cars.data$price==3259900,"link"] #es un Bugatti Veyron. 

  ggplot(data=cars.data, aes(price)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_vline(aes(xintercept=mean(price, na.rm=T)), color="darkblue", linetype="dashed",  size=1) #se observan gran cantidad de ourliers
  
  ggplot(data=cars.data, aes(1, price))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan gran cantidad de ourliers, el Bugatti Veyron detectado está demasiado alejado del resto de los valores, por tanto será eliminado
  
  cars.data <- cars.data[cars.data$price < 3259900,] #se elimina el registro de Bugatti Veyron. 
  summary(cars.data$price)
  ggplot(data=cars.data, aes(1, price))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan gran cantidad de ourliers, el Bugatti Veyron detectado está demasiado alejado del resto de los valores, por tanto será eliminado
  
  
  cars.data[cars.data$price==999999,"link"] #Ferrari 456 Gt (minimo-maximo precio encontrado en internet: 165.888-198.900) 
                                            #Ferrari Testarossa (minimo-maximo precio encontrado en internet: 117.700-187.800)
                                            #Maserati Shamal 3.2  (minimo-maximo precio encontrado en internet: 53.528-95.200)
                                            
  cars.data$price[cars.data$price=="999999" & cars.data$brand=="FERRARI" & cars.data$sbrand==456] <- mean(c(165888, 198900)) #poner en la variable el promedio de los valores posibles encontrados en internet
  cars.data$price[cars.data$price=="999999" & cars.data$brand=="FERRARI" & cars.data$sbrand=="TESTAROSSA"] <- mean(c(117700, 187800)) #idem
  cars.data$price[cars.data$price=="999999" & cars.data$brand=="MASERATI" & cars.data$sbrand=="SHAMAL"] <- mean(c(53528, 95200)) #idem
  summary(cars.data$price) 
  
  cars.data[cars.data$price >= 500000,"link"] #se verifica si es cierto que estos coches cuestan tan caro
  
  cars.data[cars.data$price >= 500000 & cars.data$brand=="LAMBORGHINI" & cars.data$sbrand=="AVENTADOR", "price"] #es un Lamborghini Aventador Roadster. (precios en internet: 382.210, 458.835, 509.344, 366.525, 407.250, 509.344) 
  cars.data$price[cars.data$price >= 500000 & cars.data$brand=="LAMBORGHINI" & cars.data$sbrand=="AVENTADOR"] <- mean(c(382210, 458835, 509344, 366525, 407250, 509344))
  
  cars.data[cars.data$price >= 500000 & cars.data$brand=="PORSCHE" & cars.data$sbrand=="911", "price"] #es un Porsche 911 Turbo Targa (precios: 218.334, 250.506, 135.437)
  cars.data$price[cars.data$price  >= 500000 & cars.data$brand=="PORSCHE" & cars.data$sbrand=="911" ] <- mean(c(218334, 250506, 135437, 102394, 109091, 205000, 326934))
  
  cars.data[cars.data$price >= 500000 & cars.data$brand=="FERRARI" & cars.data$sbrand=="458", ] 
  cars.data$price[cars.data$price  >= 500000 & cars.data$brand=="FERRARI" & cars.data$sbrand=="458" ] <- mean(c(257476, 257899, 245924, 216631))
  
  cars.data[cars.data$price >= 500000 & cars.data$brand=="MERCEDES" & cars.data$sbrand=="CLASE SLS AMG", ] 
  cars.data$price[cars.data$price  >= 500000 & cars.data$brand=="MERCEDES" & cars.data$sbrand=="CLASE SLS AMG" ] <- mean(c(595000, 226000, 269000, 279000))
  
  ggplot(data=cars.data, aes(price)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    geom_vline(aes(xintercept=mean(price, na.rm=T)), color="darkblue", linetype="dashed",  size=1) #se observan gran cantidad de ourliers
  
  ggplot(data=cars.data, aes(1, price))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan gran cantidad de ourliers, el Bugatti Veyron detectado está demasiado alejado del resto de los valores, por tanto será eliminado
  
  qqnorm(cars.data[,"price"], main = "Normal QQ Plot")
  qqline(cars.data[,"price"], col = "red")
  
  #tratamos de encontrar qué distribución sigue la variable
  library(fitdistrplus)
  library(logspline)
  
  descdist(cars.data$price, discrete = FALSE, boot=500) #la observación queda muy alejada de las posibles distribuciones. La más cercana es la distribución lognormal y gamma
  fit.log.normal <- fitdist(cars.data$price, "lnorm") #fit log-normal distribution
  plot(fit.log.normal)
  
  fit.gamma <- fitdist(cars.data$price, "gamma", method = "mme")  #fit log-normal distribution
  plot(fit.gamma)
  
  #observando el comportamiento de los outliers
  price.outlier.values <- boxplot.stats(cars.data$price)$out  # outlier values.
  price.cantidad.outliers <- length(price.outlier.values)
  price.porciento.outliers <- 100 * price.cantidad.outliers / nrow(cars.data)
  
  price.outliers.df <- cars.data[ cars.data$price > min(price.outlier.values), ] #dataframe con los valores de los outliers
  price.outliers.df <- price.outliers.df[with(price.outliers.df,order(-price)), ] #dataframe con los valores de los outliers ordenados descendentemente
  
  #comportamiento de la variable price sin outliers
  price.sin.outliers <- cars.data$price[cars.data$price<min(price.outlier.values)]
  qqnorm(price.sin.outliers, main = "Normal QQ Plot") 
  qqline(price.sin.outliers, col = "red") #ha mejorado pero sigue sin ser normal, evidentemente los outliers afectan el balance de la variable
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(price.sin.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión puede es,  beta, gamma, normal, lognormal y en menor medida uniforme
  
  #fit beta distribution
  normalized = (price.sin.outliers-min(price.sin.outliers))/(max(price.sin.outliers)-min(price.sin.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit gamma distribution, 
  fit.without.outliers.gamma <- fitdist(price.sin.outliers, "gamma", method = "mme") 

  #fit lognormal distribution 
  fit.without.outliers.lognormal <- fitdist(price.sin.outliers, "lnorm", method = "mle") 

  par(mfrow=c(2,2))
  plot.legend <- c("Gamma", "Log-normal")
  denscomp(list( fit.without.outliers.gamma, fit.without.outliers.lognormal), legendtext = plot.legend)
  cdfcomp (list( fit.without.outliers.gamma, fit.without.outliers.lognormal), legendtext = plot.legend)
  qqcomp  (list( fit.without.outliers.gamma, fit.without.outliers.lognormal), legendtext = plot.legend)
  ppcomp  (list( fit.without.outliers.gamma, fit.without.outliers.lognormal), legendtext = plot.legend)
  par(mfrow=c(1,1))
  
  ggplot(data=cars.data[cars.data$price<min(price.outlier.values),], aes(price)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(price, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 

  #probamos con el log de la variable con todos sus valores para observar su comportamiento
  log.price <- log(cars.data$price)
  
  #buscando que posible distribución sigue el logaritmo de la variable 
  descdist(log.price, discrete = FALSE, boot=500) #por orden de prioridad la observaión puede ser, lognormal y en menor medida logistic
  
  #fit lognormal distribution 
  fit.log.price.lognormal <- fitdist(log.price, "lnorm", method = "mle")
  plot(fit.log.price.lognormal)
  
  #fit normal distribution 
  fit.log.price.normal <- fitdist(log.price, "norm", method = "mle")
  plot(fit.log.price.normal)
  
  #observar el comportamiento del logaritmo de la variable vs la variable
  ggplot(data=cars.data, aes((price))) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean((price), na.rm=T)), color="darkblue", linetype="dashed", size=1) #con el logaritmo la variable describe aproximadamente una Normal
  
  ggplot(data=cars.data, aes(log(price))) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(log(price), na.rm=T)), color="darkblue", linetype="dashed", size=1) #con el logaritmo la variable describe aproximadamente una Normal
  
  
  
  ###### Variable:"fuel" ######
  str(cars.data$fuel) #variable categórica
  sort(table(cars.data$fuel), decreasing = TRUE) #la mayor cantidad de coches son de diesel
  
  ggplot(data=cars.data, aes(fuel)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  cars.data$fuel <- as.factor(cars.data$fuel)
  
  # Price vs fuel
  ggplot(cars.data,aes(x=fuel, y=price, group=fuel)) + geom_boxplot(aes(color=fuel)) +
    theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data,aes(x=fuel, y=log(price), group=fuel)) + geom_boxplot(aes(color=fuel)) +
    theme_bw() + theme(panel.border=element_blank())
  
  summary(lm(price ~ as.factor(fuel), data=cars.data)) #todos sus valores son significativos
  summary(lm(log(price) ~ as.factor(fuel), data=cars.data)) #con el logaritmo de la variable se aumenta la significatividad de una de las variables
  
  
  
  
  ###### Variable:"power" ######
  str(cars.data$power) #esta variable es de tipo char, será convertida a continua
  sort(table(cars.data$power), decreasing = TRUE) # 
  length(table(cars.data$power))
  
  cars.data$power <- gsub(" cv", "", cars.data$power) #eliminando el vocablo "cv" que de los valores de la variable
  
  cars.data$power <- as.numeric(cars.data$power) #convirtiendo los valores a numéricos
  
  summary(cars.data$power) 
  
  ggplot(data=cars.data, aes(power)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(power, na.rm=T)), color="darkblue", linetype="dashed", size=1) #se observan valores bastante alejados de la media
  
  ggplot(data=cars.data, aes(1, power)) +
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes de ourliers
  
  #buscando qué distribución sigue la variable
  descdist(cars.data$power, discrete = FALSE, boot=500) #la observación queda más cercana a la distribución lognormal y beta
  fit.power.log.normal <- fitdist(cars.data$power, "lnorm") #fit log-normal distribution
  plot(fit.power.log.normal) 
  
  #observando el comportamiento de los outliers
  power.outlier_values <- boxplot.stats(cars.data$power)$out  # outlier values.
  power.cantidad.outliers <- length(power.outlier_values)
  power.porciento.outliers <- 100 * power.cantidad.outliers / nrow(cars.data)

  #comportamiento de la variable sin outliers
  power.sin.outliers <- setdiff(na.omit(cars.data$price), power.outlier_values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(power.sin.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se acerca más a la distribución gamma
  
  #fit gamma distribution, 
  power.fit.without.outliers.gamma <- fitdist(power.sin.outliers, "gamma", method = "mme") 
  plot(power.fit.without.outliers.gamma)
  
  #probando con el logaritmo de la variable para observar su comportamiento logarítmico
  ggplot(data=cars.data, aes(log(power))) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(log(power), na.rm=T)), 
               color="darkblue", linetype="dashed", size=1) #con el logaritmo la variable se aproxima un poco a una Normal pero tiene muchos picos y la cola derecha muy alargada
  
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ power, data=cars.data)) #variable significativa individualmente
  summary(lm(log(price) ~ power, data=cars.data)) #variable significativa individualmente
  
  ggplot(cars.data, aes(x=power, y=(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=power, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(log(cars.data$price),cars.data$power, use = "pairwise.complete.obs")
  
  
  
  
  ###### Variable:"city" ######
  str(cars.data$city)
  sort(table(cars.data$city), decreasing = TRUE) #la variable está bastante sesgada a sobre Madrid
  
  cars.data$city <- gsub("\\,", "", cars.data$city) #se ha detectado una coma (,) en el valor Palmas, aquí será eliminando de los valores de la variable
  
  #
  cars.data$city[grepl("Alava", cars.data$city, fixed=TRUE) ] <- "01" 
  cars.data$city[grepl("Albacete", cars.data$city, fixed=TRUE) ] <- "02" 
  cars.data$city[grepl("Alicante", cars.data$city, fixed=TRUE) ] <- "03" 
  cars.data$city[grepl("Almeria", cars.data$city, fixed=TRUE) ] <- "04" 
  cars.data$city[grepl("Avila", cars.data$city, fixed=TRUE) ] <- "05" 
  cars.data$city[grepl("Badajoz", cars.data$city, fixed=TRUE) ] <- "06" 
  cars.data$city[grepl("Illes Balears", cars.data$city, fixed=TRUE) ] <- "07" 
  cars.data$city[grepl("Barcelona", cars.data$city, fixed=TRUE) ] <- "08" 
  cars.data$city[grepl("Burgos", cars.data$city, fixed=TRUE) ] <- "09" 
  cars.data$city[grepl("Caceres", cars.data$city, fixed=TRUE) ] <- "10" 
  cars.data$city[grepl("Cadiz", cars.data$city, fixed=TRUE) ] <- "11" 
  cars.data$city[grepl("Castellon", cars.data$city, fixed=TRUE) ] <- "12" 
  cars.data$city[grepl("Ciudad Real", cars.data$city, fixed=TRUE) ] <- "13" 
  cars.data$city[grepl("Cordoba", cars.data$city, fixed=TRUE) ] <- "14" 
  cars.data$city[grepl("Coruna A", cars.data$city, fixed=TRUE) ] <- "15" 
  cars.data$city[grepl("Cuenca", cars.data$city, fixed=TRUE) ] <- "16" 
  cars.data$city[grepl("Girona", cars.data$city, fixed=TRUE) ] <- "17" 
  cars.data$city[grepl("Granada", cars.data$city, fixed=TRUE) ] <- "18" 
  cars.data$city[grepl("Guadalajara", cars.data$city, fixed=TRUE) ] <- "19" 
  cars.data$city[grepl("Guipuzcoa", cars.data$city, fixed=TRUE) ] <- "20" 
  cars.data$city[grepl("Huelva", cars.data$city, fixed=TRUE) ] <- "21" 
  cars.data$city[grepl("Huesca", cars.data$city, fixed=TRUE) ] <- "22" 
  cars.data$city[grepl("Jaen", cars.data$city, fixed=TRUE) ] <- "23" 
  cars.data$city[grepl("Leon", cars.data$city, fixed=TRUE) ] <- "24" 
  cars.data$city[grepl("Lleida", cars.data$city, fixed=TRUE) ] <- "25" 
  cars.data$city[grepl("Rioja La", cars.data$city, fixed=TRUE) ] <- "26" 
  cars.data$city[grepl("Lugo", cars.data$city, fixed=TRUE) ] <- "27" 
  cars.data$city[grepl("Madrid", cars.data$city, fixed=TRUE) ] <- "28" 
  cars.data$city[grepl("Malaga", cars.data$city, fixed=TRUE) ] <- "29" 
  cars.data$city[grepl("Murcia", cars.data$city, fixed=TRUE) ] <- "30" 
  cars.data$city[grepl("Navarra", cars.data$city, fixed=TRUE) ] <- "31" 
  cars.data$city[grepl("Ourense", cars.data$city, fixed=TRUE) ] <- "32" 
  cars.data$city[grepl("Asturias", cars.data$city, fixed=TRUE) ] <- "33" 
  cars.data$city[grepl("Palencia", cars.data$city, fixed=TRUE) ] <- "34" 
  cars.data$city[grepl("Palmas Las", cars.data$city, fixed=TRUE) ] <- "35" 
  cars.data$city[grepl("Pontevedra", cars.data$city, fixed=TRUE) ] <- "36" 
  cars.data$city[grepl("Salamanca", cars.data$city, fixed=TRUE) ] <- "37" 
  cars.data$city[grepl("Tenerife", cars.data$city, fixed=TRUE) ] <- "38" 
  cars.data$city[grepl("Cantabria", cars.data$city, fixed=TRUE) ] <- "39" 
  cars.data$city[grepl("Segovia", cars.data$city, fixed=TRUE) ] <- "40"
  cars.data$city[grepl("Sevilla", cars.data$city, fixed=TRUE) ] <- "41" 
  cars.data$city[grepl("Soria", cars.data$city, fixed=TRUE) ] <- "42" 
  cars.data$city[grepl("Tarragona", cars.data$city, fixed=TRUE) ] <- "43" 
  cars.data$city[grepl("Teruel", cars.data$city, fixed=TRUE) ] <- "44" 
  cars.data$city[grepl("Toledo", cars.data$city, fixed=TRUE) ] <- "45" 
  cars.data$city[grepl("Valencia", cars.data$city, fixed=TRUE) ] <- "46" 
  cars.data$city[grepl("Valladolid", cars.data$city, fixed=TRUE) ] <- "47" 
  cars.data$city[grepl("Vizcaya", cars.data$city, fixed=TRUE) ] <- "48" 
  cars.data$city[grepl("Zamora", cars.data$city, fixed=TRUE) ] <- "49" 
  cars.data$city[grepl("Zaragoza", cars.data$city, fixed=TRUE) ] <- "50" 
  cars.data$city[grepl("Ceuta", cars.data$city, fixed=TRUE) ] <- "51" 
  cars.data$city[grepl("Melilla", cars.data$city, fixed=TRUE) ] <- "52"
  
  #finalmente quedan en la variable valores que nada tienen que ver con colores, estos serán missing values
  cars.data$city[! (cars.data$city=="01" | cars.data$city=="02" | cars.data$city=="03" | cars.data$city=="04"|
                      cars.data$city=="05" |cars.data$city=="06" |cars.data$city=="07" |cars.data$city=="08" |
                      cars.data$city=="09" |cars.data$city=="10" |cars.data$city=="11" |cars.data$city=="12" |
                      cars.data$city=="13" |cars.data$city=="14" |cars.data$city=="15" |cars.data$city=="16" |
                      cars.data$city=="17" |cars.data$city=="18" |cars.data$city=="19" |cars.data$city=="20" |
                      cars.data$city=="21" |cars.data$city=="22" |cars.data$city=="23" |cars.data$city=="24" |
                      cars.data$city=="25" |cars.data$city=="26" |cars.data$city=="27" |cars.data$city=="28" |
                      cars.data$city=="29" |cars.data$city=="30" |cars.data$city=="31" |cars.data$city=="32" |
                      cars.data$city=="33" |cars.data$city=="34" |cars.data$city=="35" |cars.data$city=="36" |
                      cars.data$city=="37" |cars.data$city=="38" |cars.data$city=="39" |cars.data$city=="40" |
                      cars.data$city=="41" |cars.data$city=="42" |cars.data$city=="43" |cars.data$city=="44" |
                      cars.data$city=="45" |cars.data$city=="46" |cars.data$city=="47" |cars.data$city=="48" |
                      cars.data$city=="49" |cars.data$city=="50" |cars.data$city=="51" |cars.data$city=="52") 
                    ] <- NA

  ggplot(data=cars.data, aes(city)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_vline(aes(xintercept=mean(city, na.rm=T)), 
      color="darkblue", linetype="dashed", size=1) 
  
  # Price vs city
  ggplot(cars.data,aes(x=city, y=price, group=city)) + geom_boxplot(aes(color=city)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data,aes(x=city, y=log(price), group=city)) + geom_boxplot(aes(color=city)) + theme_bw() + theme(panel.border=element_blank())
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ as.factor(city), data=cars.data)) #algunos valores de la variable son significativas
  summary(lm(log(price) ~ as.factor(city), data=cars.data)) #algunos valores de la variable son significativas
  
  cars.data$city <- as.factor(cars.data$city) #conviritiendo a factor la variable
  
  

  
  ###### Variable:"kms" ###### 
  str(cars.data$kms) #esta variable es de tipo char, lo que no es cierto.
  
  cars.data$kms <- gsub(" km", "", cars.data$kms) #eliminando el vocablo " km" de los valores de la variable
  cars.data$kms <- gsub("\\.", "", cars.data$kms) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  
  cars.data$kms <- as.numeric(cars.data$kms) #convirtiendo los valores a numéricos
  
  summary(cars.data$kms) #valor mínimo erroneo, no puede haber valores menores o iguales a cero
                         #valor máximo demasiado extremo y separado del resto de los valores
  
  cars.data[cars.data$kms==max(cars.data$kms), ] #el valor muy extremo parece real ya que se trata de un coche de mas de 10 años, no obstante se va a eliminar porque distorciona mucho la distribución de la variable
  cars.data <- cars.data[- which.max(cars.data$kms), ]  #eliminando el valor demasiado extremo 
  summary(cars.data$kms) #se observa un valor max de 9999999, generalmente este valor es erróneo
  
  cars.data[cars.data$kms==max(cars.data$kms), ] #efectivamente deben ser missing values porque son coches del 2017 y 2018, por tanto no pueden tener tantos kms recorridos 
  cars.data$kms[cars.data$kms == 9999999] <- NA
  summary(cars.data$km) #se mejora la distribución
  
  length(cars.data$kms[na.omit(cars.data$kms) < 0]) #observando los valores negativos de la variable
  cars.data$kms[cars.data$kms == -60000] <- 60000 #existe un valor negativo q se pasa a positivo
  cars.data[na.omit(cars.data$kms) == 0, "year"]
  cars.data$kms[cars.data$kms == 0] <- NA #no debe haber kms=0 porque si no fueran nuevos o seminuevos.
  
  ggplot(data=cars.data, aes(kms)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(kms, na.rm=T)), color="darkblue", linetype="dashed", size=1) #la distribución de los valores pudiera ajustarse como una distribución exponencial o chi-square
  
  ggplot(data=cars.data, aes(1, kms)) +
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #no se observan muchos de ourliers
  
  #buscando qué distribución sigue la variable
  descdist(as.vector(na.omit(cars.data$kms)), discrete = FALSE, boot=500) #la observación está cerca de la distribución lognormal
  
  
  #observando el comportamiento de los outliers
  kms.outlier_values <- boxplot.stats(cars.data$kms)$out  # outlier values.
  kms.cantidad.outliers <- length(kms.outlier_values)
  kms.porciento.outliers <- 100 * kms.cantidad.outliers / nrow(cars.data)

  #comportamiento de la variable sin outliers
  kms.sin.outliers <- setdiff(na.omit(cars.data$kms), kms.outlier_values)
  
  descdist(kms.sin.outliers, discrete = FALSE, boot=500) #se observa q la variable se sitúa sobre la distribución beta
  
  #fit beta distribution
  normalized = ( kms.sin.outliers - min( kms.sin.outliers ))/( max( kms.sin.outliers ) - min( kms.sin.outliers )) #normalize data because beta distribution works with values from 0 to 1
  power.fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(power.fit.without.outliers.beta)
  
  kms.fit.without.outliers.gamma <- fitdist(kms.sin.outliers, "gamma", method = "mme") #fit gamma distribution, 
  plot(kms.fit.without.outliers.gamma)

  ggplot(data=cars.data[cars.data$kms<min(kms.outlier_values),], aes(kms)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(kms, na.rm=T)), color="darkblue", linetype="dashed", size=1) #la distribución de los valores pudiera ajustarse como una distribución exponencial o chi-square
  
  #observando el logaritmo de la variable
  ggplot(data=cars.data, aes( log(kms) )) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(log(kms), na.rm=T)), color="darkblue", linetype="dashed", size=1) #la distribución de los valores pudiera ajustarse como una distribución exponencial o chi-square
  ggplot(data=cars.data, aes(1, log(kms))) +
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #no se observan muchos de ourliers
  
  
  # Price vs kms
  ggplot(cars.data, aes(x=kms, y=price)) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=kms, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=log(kms), y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  summary(lm((price) ~ kms, data=cars.data)) #significativa
  summary(lm(log(price) ~ kms, data=cars.data)) #significativa
  summary(lm(log(price) ~ log(kms), data=cars.data)) #el r2 empeora un poco con el log de la variable 
  
  cor(log(cars.data$price),cars.data$kms, use = "pairwise.complete.obs")
  cor(log(cars.data$price),log(cars.data$kms), use = "pairwise.complete.obs") #la correlación lineal empeora un poco con el logaritmo de la varible 
  
  
  
  
  
  ###### Variable:"doors" ###### 
  str(cars.data$doors) #la variable es de tipo char
  sort(table(cars.data$doors), decreasing = TRUE) #la mayoría de los coches presentan 5 puertas
                                                  #se observan coches con cero puertas, esto es erróneo?
  
  cars.data[cars.data$doors==0, "link"] #se refiere a renault-twizy 
  length(cars.data$doors[cars.data$doors==0]) #tenemos 7 coches de este tipo
  cars.data$doors[cars.data$brand=="RENAULT" & cars.data$sbrand=="TWIZY"] #se verifica si existe algun otro coche de este tipo que tiene un valor distinto de cero en el número de puertas
  length(cars.data$doors[cars.data$brand=="RENAULT" & cars.data$sbrand=="TWIZY"]) #también existen 7 valores de este tipo.
                                                                                  #conclusión: al parecer se dice que este tipo de coche tiene cero número de puertas 
  
  ggplot(data=cars.data, aes(doors)) + geom_bar(col="blue",fill="blue",alpha=.4) 
  
  cars.data$doors <- as.factor(cars.data$doors) #conviritiendo a factor la variable
  
  # Price vs doors
  ggplot(cars.data,aes(x=doors, y=log(price), group=doors)) +  geom_boxplot(aes(color=doors)) + theme_bw() + theme(panel.border=element_blank())

  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(log(price) ~ as.factor(doors), data=cars.data)) #solo un valor significativo
 
  
  
  
  
  
  
  ###### Variable:"emissions" ###### 
  str(cars.data$emissions) #la variable es de tipo char pero debe ser numérica
  
  cars.data$emissions <- gsub(" gr/m3", "", cars.data$emissions) #eliminando el vocablo " gr/m3" de los valores de la variable
  
  cars.data$emissions <- as.numeric(cars.data$emissions) #convirtiendo los valores a numéricos
  
  summary(cars.data$emissions) #el valor cero solo es real en los coches eléctricos, el resto de lo valores están erróneos
  
  length(cars.data$emissions[cars.data$emissions==0 & cars.data$fuel != "Eléctrico"]) 
  cars.data$emissions[cars.data$emissions==0 & cars.data$fuel != "Eléctrico"] <- NA #se convierte a NA los valores erróneos
  sum(is.na(cars.data$emissions)) #muchísismos missing values en esta variable
  
  ggplot(data=cars.data, aes(emissions)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(emissions, na.rm=T)), color="darkblue", linetype="dashed", size=1)  #se observan valores bastante alejados de las medidas de tendencia central
                                                                                                     
  ggplot(data=cars.data, aes(1, emissions)) +
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #aparentemente existen bastantes outliers superiores e inferiores

  #buscando qué distribución sigue la variable
  emissions.sin.missings <- as.vector(na.omit(cars.data$emissions))
  descdist(emissions.sin.missings, discrete = FALSE, boot=500) #la observación es lognormal o exponencial
  
  #observando el comportamiento de los outliers
  emi.outlier_values <- boxplot.stats(cars.data$emissions)$out  # outlier values.
  emi.cantidad.outliers <- length(emi.outlier_values)
  emi.porciento.outliers <- 100 * emi.cantidad.outliers / nrow(cars.data)
  
  #observando el comportamiento de la variable sin outliers ni missing data
  emissions.sin.missings.outliers <- setdiff(na.omit(cars.data$emissions), emi.outlier_values)
  descdist(emissions.sin.missings.outliers, discrete = FALSE, boot=500) #la distribución beta y la uniforme son las que mejor ajustan a la variable sin outliers
  
  #fit beta distribution
  normalized = (emissions.sin.missings.outliers - min( emissions.sin.missings.outliers ))/( max( emissions.sin.missings.outliers ) - min( emissions.sin.missings.outliers )) #normalize data because beta distribution works with values from 0 to 1
  emi.fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(emi.fit.without.outliers.beta)
  
  #fit uniform distribution, 
  emi.fit.without.outliers.unif <- fitdist(emissions.sin.missings.outliers, "unif", method = "mme") 
  plot(emi.fit.without.outliers.unif)
  
  #observemos la distribución de la variable sin los outliers
  ggplot(data=cars.data[cars.data$emissions %in% emissions.sin.missings.outliers,] , aes(emissions)) + 
    geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(emissions, na.rm=T)), color="darkblue", linetype="dashed", size=1)
  
  
  #observemos la distribución del logaritmo de la variable original
  ggplot(data=cars.data, aes(log(emissions)) )+ geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) #se han eliminado los datos con valor cero porque se han indefinido por el logaritmo
  
  # Price vs emissions
  ggplot(cars.data, aes(x=emissions, y=price)) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=emissions, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  summary(lm(price ~ emissions, data=cars.data)) #significativa
  summary(lm(log(price) ~ emissions, data=cars.data)) #significativa

  cor(log(cars.data$price),cars.data$emissions, use = "pairwise.complete.obs")
  

  
  
  ###### Variable:"seller" ###### 
  str(cars.data$seller) #la variable es de tipo char
  table(cars.data$seller) #solo se detectó un valor de la variable, por tanto la variable debe ser eliminada
  sum(is.na(cars.data$seller)) #no hay missing values
  
  ggplot(data=cars.data, aes(seller) )+ geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  # Price vs seller
  ggplot(cars.data,aes(x=seller, y=log(price), group=seller)) +  geom_boxplot(aes(color=seller)) + theme_bw() + theme(panel.border=element_blank())
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ as.factor(seller), data=cars.data)) #variable significativa individualmente
  
  cars.data$seller <- as.factor(cars.data$seller)
  
  

  
  
  ###### Variable: "color" ###### 
  str(cars.data$color) #la variable es de tipo categórica
  sort(table(cars.data$color),decreasing = TRUE) #se han detectado missing values. la variable tiene valores muy raros y será necesario limpiarla. 
  unique(cars.data$color)
  length(cars.data$color[cars.data$color==""]) # demasiados valores en blanco en la variable
  
  #Se asumirá unificar algunos colores para un mejor análisis, ejemplo se tomará como azul a toda la gama ya sea azul claro, oscuro o metalizado.
  #el motivo por el cual se hacen estas asunciones es porque los colores simples como azul, marrón, nego, etc. tienen gran cantidad de apariciones 
  #todo lo contrario ocurre con colores compuestos como azul claro o marron oscuro que tienen un pequeño número de apariciones, son casi nada influyentes y complejizan la variable por gusto
  #adicionalmente se detectan los mismos valores de la variable con mayusculas y minúsculas indistintamente, lo que se toma la decisión de unificar en un solo valor
  
  # color Blanco
  cars.data$color[grepl("BLANC",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #he puesto "blanc" porque engloba los valores "blanc" y "blanco" de la variable
  cars.data$color[grepl("WHITE",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco"
  cars.data$color[grepl("BIANCO",  toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("BLACO",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #se asume que este valor "Blaco" es falta de ortografía
  cars.data$color[grepl("ALPIN",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #
  cars.data$color[grepl("POLAR",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("WIT",     toupper(cars.data$color), fixed=TRUE) ] <- "Blanco"  #se asume que este valor "Wit" es "White"
  cars.data$color[grepl("WHITHE",  toupper(cars.data$color), fixed=TRUE) ] <- "Blanco"  #se asume que este valor "whithe" es "White"
  cars.data$color[grepl("CLARO",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("GLACIAR", toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("WEISS",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #alemán
  cars.data$color[grepl("ÁRTICO",  toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #alemán
  cars.data$color[grepl("BALTA",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #en lituano
  cars.data$color[grepl("ANCO",    toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" #palabras con faltas de ortografía
  cars.data$color[grepl("BANQUISE",toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" # en frances: hielo
  cars.data$color[grepl("HIELO",   toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("MOON",    toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("PALADIO", toupper(cars.data$color), fixed=TRUE) ] <- "Blanco" 
  cars.data$color[grepl("Weiß",    cars.data$color, fixed=TRUE) ] <- "Blanco" #
  

  # color Negro
  cars.data$color[grepl("NEGR",      toupper(cars.data$color), fixed=TRUE) ] <- "Negro" #he puesto "negr" porque engloba los valores "negr" y "negro" de la variable
  cars.data$color[grepl("BLACK",     toupper(cars.data$color), fixed=TRUE) ] <- "Negro"
  cars.data$color[grepl("SCHWARZ",   toupper(cars.data$color), fixed=TRUE) ] <- "Negro" # valor "Schwarz" es "negro" en alemán
  cars.data$color[grepl("CARBON",    toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("DARK GUN",  toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("GRAPHITE",  toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("GRAFITO",   toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("NOCHE",     toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("NIGHT",     toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("PEDERNAL",  toupper(cars.data$color), fixed=TRUE) ] <- "Negro"   
  cars.data$color[grepl("TENORITA",  toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("CAVIAR",    toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("MAGNETITA", toupper(cars.data$color), fixed=TRUE) ] <- "Negro" 
  cars.data$color[grepl("MEGRO",     toupper(cars.data$color), fixed=TRUE) ] <- "Negro" #error ortográfico
  cars.data$color[grepl("NERO",      toupper(cars.data$color), fixed=TRUE) ] <- "Negro" #en italiano
  cars.data$color[grepl("ZWART",     toupper(cars.data$color), fixed=TRUE) ] <- "Negro" #neerlandés

  cars.data$color[toupper(cars.data$color) == "OSCURO"] <- "Negro" #se asume que el valor "Oscuro" estricto es de color "Negro"
  
  
  # color Azul
  cars.data$color[grepl("AZU",          toupper(cars.data$color), fixed=TRUE) ] <- "Azul"  #se encuentra faltas de ortografía con este valor
  cars.data$color[grepl("BLU",          toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("CYAN",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("TURQUESA",     toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("CIAN",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("AXUL",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("BLEU",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("CANVANSITA",   toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("CELESTE",      toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("SAPPHIRE",     toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("SAFIRO",       toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("REFLEX",       toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("MARINO",       toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("MORNING GLORY",toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("BLAU",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("MAGNETIC",     toupper(cars.data$color), fixed=TRUE) ] <- "Azul" 
  cars.data$color[grepl("MAGNÉTICO",    toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("AGUA",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("AQUA",         toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("ALABASTR",     toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("BOURRASQUE",   toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("BEACH",        toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("NEPTUNO",      toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("PROFUNDO",     toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("SANTORINI",    toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  cars.data$color[grepl("ZAFIRO",       toupper(cars.data$color), fixed=TRUE) ] <- "Azul"
  
  
  # color Plata
  cars.data$color[grepl("PLATA",     toupper(cars.data$color), fixed=TRUE) ] <- "Plata"
  cars.data$color[grepl("SILVER",    toupper(cars.data$color), fixed=TRUE) ] <- "Plata"
  
  
  # color Rojo
  cars.data$color[grepl("ROJO",      toupper(cars.data$color), fixed=TRUE) ] <- "Rojo"
  cars.data$color[grepl("RED",       toupper(cars.data$color), fixed=TRUE) ] <- "Rojo"
  cars.data$color[grepl("ARMIN",     toupper(cars.data$color), fixed=TRUE) ] <- "Rojo"    
  cars.data$color[grepl("KARMESIN",  toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #se asume como el "color Karmesinrot" al rojo 
  cars.data$color[grepl("LAVA",      toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #se asume como el "color lava" al rojo
  cars.data$color[grepl("FUEGO",     toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #se asume como el "color fuego" al rojo
  cars.data$color[grepl("ROUGE",     toupper(cars.data$color), fixed=TRUE) ] <- "Rojo"  
  cars.data$color[grepl("ROSSO",     toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #en italiano
  cars.data$color[grepl("VERMELL",   toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("VINO",      toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("ROT",       toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("DINAMITA",  toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("RUBBY WINE",toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("RUBY WINE", toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("RUBI",      toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" #
  cars.data$color[grepl("VOLCAN",    toupper(cars.data$color), fixed=TRUE) ] <- "Rojo" 
  
  
  # color Amarillo
  cars.data$color[grepl("AMARILLO",toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #se ha dejado la variable en "yel" porque se ha detectado faltas de ortografía en el voablo "yellow" 
  cars.data$color[grepl("YEL",     toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo"
  cars.data$color[grepl("MOSTAZA", toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #se asume como el "color marro" al marron
  cars.data$color[grepl("AMBAR",   toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo"
  cars.data$color[grepl("GELB",    toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #en alemán
  cars.data$color[grepl("SAND",    toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #en alemán
  cars.data$color[grepl("SOLAR",   toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #en alemán
  cars.data$color[grepl("DUNA",    toupper(cars.data$color), fixed=TRUE) ] <- "Amarillo" #en alemán 

  
  # color Naranja
  cars.data$color[grepl("ARANJA",   toupper(cars.data$color), fixed=TRUE) ] <- "Naranja"
  cars.data$color[grepl("ORANGE",   toupper(cars.data$color), fixed=TRUE) ] <- "Naranja"
  cars.data$color[grepl("PITER",    toupper(cars.data$color), fixed=TRUE) ] <- "Naranja" #valores: júpiter y jupiter
  cars.data$color[grepl("TANGERINE",toupper(cars.data$color), fixed=TRUE) ] <- "Naranja"
  
  
  # color Verde
  cars.data$color[grepl("VERDE",     toupper(cars.data$color), fixed=TRUE) ] <- "Verde" 
  cars.data$color[grepl("VERT",      toupper(cars.data$color), fixed=TRUE) ] <- "Verde" 
  cars.data$color[grepl("GREEN",     toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  cars.data$color[grepl("EMERALD",   toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  cars.data$color[grepl("AMAZONITE", toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  cars.data$color[grepl("PEPEER",    toupper(cars.data$color), fixed=TRUE) ] <- "Verde" #del inglés Pepper, error ortográfico
  cars.data$color[grepl("PERA",      toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  cars.data$color[grepl("PISTACHO",  toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  cars.data$color[grepl("VERDOSO",   toupper(cars.data$color), fixed=TRUE) ] <- "Verde"
  
  
  # color Violeta
  cars.data$color[grepl("VIOLET",   toupper(cars.data$color), fixed=TRUE) ] <- "Violeta"
  cars.data$color[grepl("AMATISTA", toupper(cars.data$color), fixed=TRUE) ] <- "Violeta"
  cars.data$color[grepl("LILA",     toupper(cars.data$color), fixed=TRUE) ] <- "Violeta"
  cars.data$color[grepl("ASTER",    toupper(cars.data$color), fixed=TRUE) ] <- "Violeta"
  cars.data$color[grepl("BERENJENA",toupper(cars.data$color), fixed=TRUE) ] <- "Violeta"

  
  # color Purpura
  cars.data$color[grepl("PURPLE", toupper(cars.data$color), fixed=TRUE) ] <- "Purpura"
  cars.data$color[grepl("PURUR",  toupper(cars.data$color), fixed=TRUE) ] <- "Purpura"
  
  
  # color Morado
  cars.data$color[grepl("MORADO", toupper(cars.data$color), fixed=TRUE) ] <- "Morado"
  
  
  # color Beige
  cars.data$color[grepl("BEIG",            toupper(cars.data$color), fixed=TRUE) ] <- "Beige"
  cars.data$color[grepl("CANNA DI FUCILE", toupper(cars.data$color), fixed=TRUE) ] <- "Beige"
  cars.data$color[grepl("VISION",          toupper(cars.data$color), fixed=TRUE) ] <- "Beige"

  
  # color Marron
  cars.data$color[grepl("MARR",     toupper(cars.data$color), fixed=TRUE) ] <- "Marron" #se detectan muchos errores ortográficos en este valor por tanto solo lo dejamos en "marr"
  cars.data$color[grepl("BROWN",    toupper(cars.data$color), fixed=TRUE) ] <- "Marron"
  cars.data$color[grepl("CAMEL",    toupper(cars.data$color), fixed=TRUE) ] <- "Marron" 
  cars.data$color[grepl("CHOCOLATE",toupper(cars.data$color), fixed=TRUE) ] <- "Marron"  
  cars.data$color[grepl("TECA",     toupper(cars.data$color), fixed=TRUE) ] <- "Marron" 
  cars.data$color[grepl("TIERRA",   toupper(cars.data$color), fixed=TRUE) ] <- "Marron" 
  cars.data$color[grepl("CAPUCCINO",toupper(cars.data$color), fixed=TRUE) ] <- "Marron" 
  cars.data$color[grepl("BRAUN",    toupper(cars.data$color), fixed=TRUE) ] <- "Marron" #con faltas de ortografía
  cars.data$color[grepl("EBONY",    toupper(cars.data$color), fixed=TRUE) ] <- "Marron" #ébano
  cars.data$color[grepl("NOCCIOLA", toupper(cars.data$color), fixed=TRUE) ] <- "Marron" #del italiano: avellano
  cars.data$color[grepl("OLONG",    toupper(cars.data$color), fixed=TRUE) ] <- "Marron" 


  # color Plateado
  cars.data$color[grepl("PLAT",     toupper(cars.data$color), fixed=TRUE) ] <- "Plateado"
  cars.data$color[grepl("PLATEADO", toupper(cars.data$color), fixed=TRUE) ] <- "Plateado" #se agrega al valor "Plateado" al valor "Plata" de la variable que ya existe en el dataset
  cars.data$color[grepl("PLATINUM", toupper(cars.data$color), fixed=TRUE) ] <- "Plateado" #se agrega al valor "Plateado" al valor "Plata" de la variable que ya existe en el dataset
  cars.data$color[grepl("IRIDIO",   toupper(cars.data$color), fixed=TRUE) ] <- "Plateado" 
  cars.data$color[grepl("SILBER",   toupper(cars.data$color), fixed=TRUE) ] <- "Plateado" #con faltas de ortografía
  cars.data$color[grepl("ARGINT",   toupper(cars.data$color), fixed=TRUE) ] <- "Plateado" #en rumano

  
  # color Cobre
  cars.data$color[grepl("Cobre",     toupper(cars.data$color), fixed=TRUE) ] <- "Cobre"  #se asume el color "RAW COPPER" como color cobre
  cars.data$color[grepl("RAW COPPER",toupper(cars.data$color), fixed=TRUE) ] <- "Cobre"  #se asume el color "RAW COPPER" como color cobre
  cars.data$color[grepl("COOPER",    toupper(cars.data$color), fixed=TRUE) ] <- "Cobre"
  
 
  # color Oro
  cars.data$color[grepl("ORO",   toupper(cars.data$color), fixed=TRUE) ] <- "Dorado"
  cars.data$color[grepl("DORADO",toupper(cars.data$color), fixed=TRUE) ] <- "Dorado"    #se asume el dorado como color oro
  cars.data$color[grepl("GOLDEN",toupper(cars.data$color), fixed=TRUE) ] <- "Dorado" 

  
  # color Gris
  cars.data$color[grepl("GRIS",      toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("GRAY",      toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("GREY",      toupper(cars.data$color), fixed=TRUE) ] <- "Gris"   #error ortográfico
  cars.data$color[grepl("TITANIUM",  toupper(cars.data$color), fixed=TRUE) ] <- "Gris"   #el titanium es una tonalidad de gris brillante, por tanto se asume como gris
  cars.data$color[grepl("ARTENSE",   toupper(cars.data$color), fixed=TRUE) ] <- "Gris" 
  cars.data$color[grepl("PLOMO",     toupper(cars.data$color), fixed=TRUE) ] <- "Gris"   
  cars.data$color[grepl("ANTRACITA", toupper(cars.data$color), fixed=TRUE) ] <- "Gris"  
  cars.data$color[grepl("ANVIL",     toupper(cars.data$color), fixed=TRUE) ] <- "Gris" 
  cars.data$color[grepl("BASALTO",   toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("FER",       toupper(cars.data$color), fixed=TRUE) ] <- "Gris" 
  cars.data$color[grepl("GRIGIO",    toupper(cars.data$color), fixed=TRUE) ] <- "Gris" #en italiano
  cars.data$color[grepl("GRAU",      toupper(cars.data$color), fixed=TRUE) ] <- "Gris" #en ALEMÁN
  cars.data$color[grepl("ACIER",     toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("ANDRAFITA", toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("ARENA",     toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("AUTOMÁTICO",toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("CENIZA",    toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("NARDO",     toupper(cars.data$color), fixed=TRUE) ] <- "Gris"
  cars.data$color[grepl("PIEDRA",    toupper(cars.data$color), fixed=TRUE) ] <- "Gris"

  cars.data$color[toupper(cars.data$color) == "DELFIN"] <- "Gris" #se asume que el "color delfín" extricto sea el gris
  
  
  # color Granate
  cars.data$color[grepl("GRANATE", toupper(cars.data$color), fixed=TRUE) ] <- "Granate" 
  cars.data$color[grepl("BURDEO",  toupper(cars.data$color), fixed=TRUE) ] <- "Granate" 
  
  # color Granate
  cars.data$color[grepl("BRON",     toupper(cars.data$color), fixed=TRUE) ] <- "Granate"  # BRONCE = Bronceado = Bronzo (en italiano) = bronze
  cars.data$color[grepl("gunmetal", toupper(cars.data$color), fixed=TRUE) ] <- "Granate" # gunmetal (alemán)
  
  # color Cuero
  cars.data$color[grepl("CUERO", toupper(cars.data$color), fixed=TRUE) ] <- "Cuero"  #el cuero puede tomar diferentes colores, por tanto lo dejamos tal como está.
                                                                                      #Nota: dejarlo para el final por si se encuentra combinado con otro color como negro cuero y entonces se asume q el color es negro
  # color Metalizado 
  cars.data$color[grepl("METAL",    toupper(cars.data$color), fixed=TRUE) ] <- "Metalizado" #Nota: se pondrá al final por si se encuentra combinado con otro color como gris metalizado y entonces se asume q el color es gris
  cars.data$color[grepl("TALIZADA", toupper(cars.data$color), fixed=TRUE) ] <- "Metalizado"
  cars.data$color[grepl("ALUMINIO", toupper(cars.data$color), fixed=TRUE) ] <- "Metalizado"  
  cars.data$color[grepl("ALIMINIO", toupper(cars.data$color), fixed=TRUE) ] <- "Metalizado" #con falta de ortografía
  
  
  #Otro color
  cars.data$color[
        grepl("OBSIDIANA", toupper(cars.data$color), fixed=TRUE) | 
        grepl("MATE",      toupper(cars.data$color), fixed=TRUE) | 
        grepl("COSM",      toupper(cars.data$color), fixed=TRUE) |  # color Cosmos = Cosmic
        grepl("JUPITER",   toupper(cars.data$color), fixed=TRUE) | 
        grepl("MOKA",      toupper(cars.data$color), fixed=TRUE) | 
        grepl("VARIOS",    toupper(cars.data$color), fixed=TRUE) |
        grepl("COLOR",     toupper(cars.data$color), fixed=TRUE) |
        grepl("MONTA",     toupper(cars.data$color), fixed=TRUE) |  # color Montaña = Monta?a = MontaÑa
        grepl("CHAMP",     toupper(cars.data$color), fixed=TRUE) |  # Champagne = Champan = Champán = Champage = Champagñe = Champang
        grepl("NACAR",     toupper(cars.data$color), fixed=TRUE) |         
        grepl("PERLA",     toupper(cars.data$color), fixed=TRUE) |  # color Perlado = Perla = Perla + perla + perlado
        grepl("SHARK",     toupper(cars.data$color), fixed=TRUE) |
        grepl("DIAMANTE",  toupper(cars.data$color), fixed=TRUE) |  
        grepl("CIRR",      toupper(cars.data$color), fixed=TRUE) |  # Cirro = Cirrus
        grepl("SELENITA",  toupper(cars.data$color), fixed=TRUE) |
        grepl("ROSA",      toupper(cars.data$color), fixed=TRUE) |
        grepl("BRILLANTE", toupper(cars.data$color), fixed=TRUE) |
        grepl("MARFIL",    toupper(cars.data$color), fixed=TRUE) |
        grepl("TAPENADE",  toupper(cars.data$color), fixed=TRUE) |
        grepl("CREMA",     toupper(cars.data$color), fixed=TRUE) |
        grepl("KAIKOURA",  toupper(cars.data$color), fixed=TRUE) |
        grepl("BORRASKA",  toupper(cars.data$color), fixed=TRUE) |
        grepl("KAVA",      toupper(cars.data$color), fixed=TRUE) |
        grepl("CEREZA",    toupper(cars.data$color), fixed=TRUE) |
        grepl("BERRY",     toupper(cars.data$color), fixed=TRUE) |
        grepl("BRON",      toupper(cars.data$color), fixed=TRUE) |
        grepl("CAOBA",     toupper(cars.data$color), fixed=TRUE) |
        grepl("DAYTONA",   toupper(cars.data$color), fixed=TRUE) |
        grepl("OCRE",      toupper(cars.data$color), fixed=TRUE) |
        grepl("STAR DUST", toupper(cars.data$color), fixed=TRUE) |
        grepl("ABISAL",    toupper(cars.data$color), fixed=TRUE) |
        grepl("ARGENTA",   toupper(cars.data$color), fixed=TRUE) |
        grepl("AURORA",    toupper(cars.data$color), fixed=TRUE) |
        grepl("CALCIT",    toupper(cars.data$color), fixed=TRUE) |  #Calcit = Calcita
        grepl("CASIOPEA",  toupper(cars.data$color), fixed=TRUE) |
        grepl("EUCALIPT",  toupper(cars.data$color), fixed=TRUE) |
        grepl("CALCIT",    toupper(cars.data$color), fixed=TRUE) |
        grepl("GAMUZA",    toupper(cars.data$color), fixed=TRUE) |
        grepl("INDIO",     toupper(cars.data$color), fixed=TRUE) |
        grepl("KANGOO",    toupper(cars.data$color), fixed=TRUE) |
        grepl("MOCCA",     toupper(cars.data$color), fixed=TRUE) |
        grepl("PANAC",     toupper(cars.data$color), fixed=TRUE) |  #Panaccota + Panacota
        grepl("PANTERA",   toupper(cars.data$color), fixed=TRUE) |
        grepl("PIRINEOS",  toupper(cars.data$color), fixed=TRUE) |
        grepl("QUARTZ",    toupper(cars.data$color), fixed=TRUE) |  #Quartzite Y Quarzo
        grepl("SPIRIT",    toupper(cars.data$color), fixed=TRUE) |
        grepl("TANZANITA", toupper(cars.data$color), fixed=TRUE) |
        grepl("TERRACOTA", toupper(cars.data$color), fixed=TRUE) |
        grepl("VINILO",    toupper(cars.data$color), fixed=TRUE) |
        grepl("GLACE",     toupper(cars.data$color), fixed=TRUE) 
    ] <- "Otros"
  


  
  #finalmente quedan en la variable valores que nada tienen que ver con colores, estos serán missing values
  cars.data$color[ !( grepl("BLANCO",      toupper(cars.data$color), fixed=TRUE) | 
                        grepl("NEGRO",     toupper(cars.data$color), fixed=TRUE) |
                        grepl("AZUL",      toupper(cars.data$color), fixed=TRUE) |
                        grepl("ROJO",      toupper(cars.data$color), fixed=TRUE) |
                        grepl("AMARILLO",  toupper(cars.data$color), fixed=TRUE) |
                        grepl("NARANJA",   toupper(cars.data$color), fixed=TRUE) |
                        grepl("VERDE",     toupper(cars.data$color), fixed=TRUE) |
                        grepl("VIOLETA",   toupper(cars.data$color), fixed=TRUE) |
                        grepl("PURPURA",   toupper(cars.data$color), fixed=TRUE) |
                        grepl("MORADO",    toupper(cars.data$color), fixed=TRUE) |
                        grepl("BEIGE",     toupper(cars.data$color), fixed=TRUE) |
                        grepl("MARRON",    toupper(cars.data$color), fixed=TRUE) |
                        grepl("PLATEADO",  toupper(cars.data$color), fixed=TRUE) |
                        grepl("COBRE",     toupper(cars.data$color), fixed=TRUE) |
                        grepl("DORADO",    toupper(cars.data$color), fixed=TRUE) |
                        grepl("GRIS",      toupper(cars.data$color), fixed=TRUE) |
                        grepl("BRONCE",    toupper(cars.data$color), fixed=TRUE) |
                        grepl("GRANATE",   toupper(cars.data$color), fixed=TRUE) |
                        grepl("CUERO",     toupper(cars.data$color), fixed=TRUE) |
                        grepl("METALIZADO",toupper(cars.data$color), fixed=TRUE) |
                        grepl("OTROS",     toupper(cars.data$color), fixed=TRUE) )  
                    ] <- NA

  sort(table(cars.data$color),decreasing = TRUE) 
  sum(is.na(cars.data$color)) #se han generado demasiados missing values
  
  ggplot(data=cars.data, aes(color)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  NA.color.percent <- sum(is.na(cars.data$color)) / nrow(cars.data) * 100 #porcentaje de NA aproximadamente 5%
  
  cars.data$color <- as.factor(cars.data$color) #conviritiendo a factor la variable
  
  # Price vs color
  ggplot(cars.data,aes(x=color, y=price, group=color)) + geom_boxplot(aes(color=color)) + 
    theme_bw() + theme(panel.border=element_blank()) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggplot(cars.data,aes(x=color, y=log(price), group=color)) + 
    geom_boxplot(aes(color=color)) + theme_bw() + theme(panel.border=element_blank()) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ as.factor(color), data=cars.data)) #algunos colores son significativos
  summary(lm(log(price) ~ color, data=cars.data)) #mejora mucho el modelo
  
  

  
  ###### Variable:"warranty" ###### 
  str(cars.data$warranty) #la variable es de tipo char
  table(cars.data$warranty)
  
  ggplot(data=cars.data, aes(warranty)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  cars.data$warranty <- as.factor(cars.data$warranty)
  
  # Price vs warranty
  ggplot(cars.data,aes(x=warranty, y=price, group=warranty)) + geom_boxplot(aes(color=warranty)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data,aes(x=warranty, y=log(price), group=warranty)) + geom_boxplot(aes(color=warranty)) + theme_bw() + theme(panel.border=element_blank())
 
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ as.factor(warranty), data=cars.data)) #variable significativa individualmente
  summary(lm(log(price) ~ as.factor(warranty), data=cars.data)) 
  
  
  
  
  ###### Variable:"year" ###### 
  str(cars.data$year) #la variable es de tipo char
  table(cars.data$year)
  length(cars.data$year[cars.data$year==""]) #no hay campos vacíos
  
  cars.data$year <- gsub("^.*?/","",cars.data$year) #se toma la desición de eliminar los meses de la variable, porque no todos los valores tienen indicados los meses
  
  sort(table(cars.data$year), decreasing = TRUE) #se observan coches extremadamente antiguos
  
  ggplot(data=cars.data, aes(year)) + geom_bar(col="blue",fill="blue", alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  #se crea una variable, a partir del año de fabricación (variable year), que almacenará la antiguedad del coche
  cars.data$antiguedad <- as.numeric(strsplit(toString(Sys.Date()),"-")[[1]][1]) - as.numeric(cars.data$year) 
  str(cars.data$antiguedad)
  
  #se analizan los coches más antiguos
  coches.mas.10.annos <- cars.data[cars.data$antiguedad > 10, c("antiguedad", "price")]
  porciento.coches.mas.10.annos <- 100 * nrow(coches.mas.10.annos) / nrow(cars.data)
  print(porciento.coches.mas.10.annos) #25% de los datos tienen más de 10 años
  
  df_sorted_desc <- coches.mas.10.annos[with(coches.mas.10.annos, order(-antiguedad)), ]
  print(df_sorted_desc) #no se observa una relación de la antiguedad del coche con respecto al precio
  cor(df_sorted_desc) #no están correladas estas variables
  
  #volviendo a la variable original
  #observamos la variable y su comportamiento discreto. obviamente su comportamiento es el mismo que el año de confección del coche
  sort(table(cars.data$antiguedad),decreasing = TRUE) 
  
  ggplot(data=cars.data, aes(as.factor(antiguedad))) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  #la variable se puede tratar como contínua por su naturaleza, observemos su comportamiento
  cars.data$antiguedad <- as.numeric(cars.data$antiguedad)
  summary(cars.data$antiguedad) #el valor máximo está bastante alejado los valores de tendencia central, se tiene una variable con outliers muy marcados
  sum(is.na(cars.data$antiguedad))
  
  #buscando qué distribución sigue la variable
  descdist(cars.data$antiguedad, discrete = FALSE, boot=500) #la observación queda más cerca de la distribución lognormal. Los valores cero causan problemas en esta distribución
  
  ggplot(data=cars.data, aes(antiguedad)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(antiguedad, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #se observan valores bastante separados de la media, previsible
  
  ggplot(data=cars.data, aes(1, antiguedad))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  
  #observando el comportamiento de los outliers
  antig.outlier.values <- boxplot.stats(cars.data$antiguedad)$out  # outlier values.
  antig.cantidad.outliers <- length(antig.outlier.values)
  antig.porciento.outliers <- 100 * antig.cantidad.outliers / nrow(cars.data)
  
  #comportamiento de la variable sin outliers
  antig.sin.outliers <- cars.data$antiguedad[cars.data$antiguedad<min(antig.outlier.values)]
  descdist(antig.sin.outliers, discrete = FALSE, boot=500) #la observación está sobre de la distribución beta
  
  #fit beta distribution
  normalized = ( antig.sin.outliers - min( antig.sin.outliers ))/( max( antig.sin.outliers ) - min( antig.sin.outliers )) #normalize data because beta distribution works with values from 0 to 1
  antig.fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(antig.fit.without.outliers.beta)
  
  #visualizar la variable
  ggplot(data=cars.data[cars.data$antiguedad < min(antig.outlier.values), ], aes(antiguedad)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(antiguedad, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  #la distribución del logaritmo de la variable
  ggplot(data=cars.data, aes(log(emissions)) )+ geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  #cars.data$year <- as.factor(cars.data$year) #no se eliminará la variable year todavía por si se necesita en algún momento más adelante
  cars.data$year <- NULL
  
  # Price vs antiguedad
  ggplot(cars.data, aes(x=antiguedad, y=price)) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=antiguedad, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("") #se observa un comportamiento interesante: una disminución del precio y luego un aumento
                                                                                                                               #esto es un fenómeno que ocurre mucho: "las antiguedades cuestan caras", las compran los coleccionistas
  cor(cars.data$antiguedad, cars.data$price, use = "pairwise.complete.obs") # muy baja correlación lineal
  cor(cars.data$antiguedad,log(cars.data$price), use = "pairwise.complete.obs")  # mejora la correlación lineal
 
  summary(lm(price ~ antiguedad, data=cars.data)) #variable significativa con respecto a la VO
  summary(lm(log(price) ~ antiguedad, data=cars.data)) #mejora bastante el R2

  
  
  
  
  
  ###### Variable:"trunk" ###### 
  str(cars.data$trunk) #la variable es de tipo char pero debe ser numérica
  length(cars.data$trunk[cars.data$trunk==""]) #no hay campos vacíos
  
  cars.data$trunk <- gsub(" l", "", cars.data$trunk) #eliminando el vocablo " l" de los valores de la variable
  cars.data$trunk <- gsub("\\.", "", cars.data$trunk) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  
  cars.data$trunk <- as.numeric(cars.data$trunk)
  
  summary(cars.data$trunk) #se observa aparentemente balanceada la distribución de la variable exepto por el valor cero
                            #el valor cero será cierto? existe algún coche sin maletero?
  
  coches.maletero.cero <- cars.data[cars.data$trunk==0,] #se han escogido al azar varios coches identificandolos con marca y modelo y se han buscado en internet y todos han tenido maletero
                                                         #se debe seguir investigando pero probablemente los valores en cero de la variable sean missing values por tanto se trataran como tal
  nrow(coches.maletero.cero) #muchísimos coches sin maletero
  porciento.coches.maletero.cero <- 100 * nrow(coches.maletero.cero) / nrow(cars.data)
  print(porciento.coches.maletero.cero) #21% de los datos tienen más de 10 años
  
  #visualización
  ggplot(data=cars.data, aes(trunk)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(trunk, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #se observan valores bastante separados de la media
  
  #sin contar los valores cero de la variable
  ggplot(data=cars.data[cars.data$trunk>0,], aes(trunk)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(trunk, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #se observan valores bastante separados de la media
  
  ggplot(data=cars.data, aes(1, trunk))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  #poniendo en NA los valores cero de la variable
  cars.data$trunk[cars.data$trunk == 0]  <- NA
  sum(is.na(cars.data$trunk))
  
  descdist(as.vector(na.omit(cars.data$trunk)), discrete = FALSE, boot=500) #la observación queda bastnte alejada de las posibles distribuciones, se acerca un poco solo a la distribución lognormal
  
  #comportamiento de outliers
  trunk.outlier.values <- boxplot.stats(cars.data$trunk)$out  # outlier values.
  trunk.cantidad.outliers <- length(trunk.outlier.values)
  trunk.porciento.outliers <- 100 * trunk.cantidad.outliers / nrow(cars.data)
  
  ggplot(data=cars.data[cars.data$price<min(price.outlier.values),], aes(price)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(price, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  #comportamiento de la variable sin outliers
  trunk.sin.missings.outliers <- setdiff(na.omit(cars.data$trunk), trunk.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(trunk.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme
  
  #fit beta distribution
  normalized = (trunk.sin.missings.outliers-min(trunk.sin.missings.outliers))/(max(trunk.sin.missings.outliers)-min(trunk.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  trunk.without.outliers.unif <- fitdist(trunk.sin.missings.outliers, "unif", method = "mme") 
  plot(trunk.without.outliers.unif)
  
  #visualización
  ggplot(data=cars.data, aes(log(trunk))) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(log(trunk), na.rm=T)), color="darkblue", linetype="dashed", size=1) #con el logaritmo la variable describe aproximadamente una Normal
  
  # Price vs trunk
  ggplot(cars.data, aes(x=trunk, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=trunk, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$trunk, cars.data$price, use = "pairwise.complete.obs") #no hay correlación entre las variables
  cor(cars.data$trunk, log(cars.data$price), use = "pairwise.complete.obs")

  summary(lm(price ~ trunk, data=cars.data)) #la variable es significativa individuelmente con el price
  summary(lm(log(price) ~ trunk, data=cars.data)) 


  
  
  ###### Variable:"length" ###### 
  str(cars.data$length) #tipo de dato char pero debe ser numérico
  sum(cars.data$length=="")
  
  cars.data$length <- gsub(" cm", "", cars.data$length) #eliminando el vocablo " cm" de los valores de la variable
  cars.data$length <- as.numeric(cars.data$length) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$length) #aparenta tener una destribución bastante centrada, sin elementos demasiado elejados a las medidas de tendencia central
                            #aunque cabe notar que hay valores cero, lo cual no tiene sentido
  
  cars.data[cars.data$length==0,] #analizando los valores cero de la variable
  length(cars.data$length[cars.data$length==0]) #cantidad de valores cero de la variable. 
  cars.data$length[cars.data$length==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$length)) #pocos NAs 
  
  ggplot(data=cars.data, aes(length)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(length, na.rm=T)), color="darkblue", linetype="dashed", size=1)   
  
  ggplot(data=cars.data, aes(1, length))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  descdist(as.vector(na.omit(cars.data$length)), discrete = FALSE, boot=500) #la observación queda muy alejada de las posibles distribuciones.
  
  #comportamiento de outliers
  length.outlier.values <- boxplot.stats(cars.data$length)$out  # outlier values.
  length.cantidad.outliers <- length(length.outlier.values)
  length.porciento.outliers <- 100 * length.cantidad.outliers / nrow(cars.data) #se detectan bastantes outliers
  
  #observar los valores extremos de la variable
  cars.data[which(cars.data$length==min(cars.data$length, na.rm = T)),] #ok
  cars.data[which(cars.data$length==max(cars.data$length, na.rm = T)),] #ok
  
  length.sin.missings.outliers <- setdiff(na.omit(cars.data$length), length.outlier.values)
  
  descdist(as.vector(length.sin.missings.outliers), discrete = FALSE, boot=500) #la observación se encuentra muy cerca de la distribución unifomre y dentro del rango de la distribución beta
  
  #fit beta distribution
  normalized = (length.sin.missings.outliers-min(length.sin.missings.outliers))/(max(length.sin.missings.outliers)-min(length.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta) #se ajusta muy bien a la distribución beta
  
  #fit uniform distribution, 
  length.fit.without.outliers.unif <- fitdist(length.sin.missings.outliers, "unif", method = "mme") 
  plot(length.fit.without.outliers.unif) #se ajusta muy bien a la uniforme
  
  ggplot() + aes(length.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(length.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=length.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs length
  ggplot(cars.data, aes(x=length, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=length, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$length, cars.data$price, use = "pairwise.complete.obs") #no tienen correlación
  cor(cars.data$length, log(cars.data$price), use = "pairwise.complete.obs") #no tienen correlación

  summary(lm(price ~ length, data=cars.data)) #significativa.
  summary(lm(log(price) ~ length, data=cars.data)) #significativa. 

  
  
  
  
  ###### Variable:"height" ###### 
  str(cars.data$height) #tipo de dato char pero debe ser numérico
  sum(cars.data$height=="") #no hay valores en blanco
  
  cars.data$height <- gsub(" cm", "", cars.data$height) #eliminando el vocablo " cm" de los valores de la variable
  cars.data$height <- as.numeric(cars.data$height) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$height) #las medidas de tendencia central mustran q esta es una distribución no muy centrada
                            #además se observa que hay datos con valores cero, lo cual no tiene sentido
  
  cars.data[cars.data$height==0,] #analizando los valores cero de la variable
  length(cars.data$length[cars.data$height==0]) #cantidad de valores cero de la variable. 
  cars.data$height[cars.data$height==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$height))
  cars.data[is.na(cars.data$height),] 
  
  descdist(as.vector(na.omit(cars.data$height)), discrete = FALSE, boot=500) #La más distribucion más cercana es la lognormal
  
  height.fit.log.normal <- fitdist(as.vector(na.omit(cars.data$height)), "lnorm") #fit log-normal distribution
  
  height.fit.gamma <- fitdist(as.vector(na.omit(cars.data$height)), "gamma") #fit log-normal distribution

  par(mfrow=c(2,2))
  plot.legend <- c("Lognormal", "Gamma")
  denscomp(list(height.fit.log.normal, height.fit.gamma), legendtext = plot.legend)
  cdfcomp (list(height.fit.log.normal, height.fit.gamma), legendtext = plot.legend)
  qqcomp  (list(height.fit.log.normal, height.fit.gamma), legendtext = plot.legend)
  ppcomp  (list(height.fit.log.normal, height.fit.gamma), legendtext = plot.legend)
  par(mfrow=c(1,1))
  
  #visuallización de la variable
  ggplot(data=cars.data, aes(height)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(height, na.rm=T)), color="darkblue", linetype="dashed", size=1)   
  
  ggplot(data=cars.data, aes(1, height))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  #comportamiento de outliers
  height.outlier.values <- boxplot.stats(cars.data$height)$out  # outlier values.
  height.cantidad.outliers <- length(height.outlier.values)
  height.porciento.outliers <- 100 * height.cantidad.outliers / nrow(cars.data) #se detectan outliers muy alejados de la tendencia central, pero representan un bajo porcentaje del total de datos
  
  #comportamiento de la variable sin outliers
  height.sin.missings.outliers <- setdiff(na.omit(cars.data$height), height.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(height.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme
  
  #fit beta distribution
  normalized = (height.sin.missings.outliers-min(height.sin.missings.outliers))/(max(height.sin.missings.outliers)-min(height.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  height.without.outliers.unif <- fitdist(height.sin.missings.outliers, "unif", method = "mme") 
  plot(height.without.outliers.unif)

  ggplot() + aes(height.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(height.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  # Price vs height
  ggplot(cars.data, aes(x=height, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=height, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$height, cars.data$price, use = "pairwise.complete.obs") #no existe correlación
  cor(cars.data$height, log(cars.data$price), use = "pairwise.complete.obs") #no existe correlación
  
  summary(lm(price ~ height, data=cars.data)) #significativa. 
  summary(lm(log(price) ~ height, data=cars.data)) #significativa. 
 
  
  
  
  
  ###### Variable:"width" ###### 
  str(cars.data$width) #tipo de dato char pero debe ser numérico
  sum(cars.data$width=="")
  
  cars.data$width <- gsub(" cm", "", cars.data$width) #eliminando el vocablo " cm" de los valores de la variable
  cars.data$width <- as.numeric(cars.data$width) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$width) #las medidas de tendencia central mustran que es una distribución muy balanceada
                           #no obstante se observa que hay datos con valores cero, lo cual no tiene sentido
  
  cars.data[cars.data$width==0,] #analizando los valores cero de la variable
  length(cars.data$width[cars.data$width==0]) #cantidad de valores cero de la variable. 
  cars.data$width[cars.data$width==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$width))
  
  descdist(as.vector(na.omit(cars.data$width)), discrete = FALSE, boot=500) #la observación queda muy alejada de las posibles distribuciones. La más cercana es la distribución logistica y lognormal
  width.fit.logistic <- fitdist(as.vector(na.omit(cars.data$width)), "logis") #fit logistic distribution 
  plot(width.fit.logistic) #no se ajusta para nada con la distribución logística
  
  width.fit.log.normal <- fitdist(as.vector(na.omit(cars.data$width)), "lnorm") #se ajusta mejor 
  plot(width.fit.log.normal)
  
  ggplot(data=cars.data, aes(width)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(width, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, width))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  #comportamiento de outliers
  width.outlier.values <- boxplot.stats(cars.data$width)$out  # outlier values.
  width.cantidad.outliers <- length(width.outlier.values) #se detectan gran número de outliers
  width.porciento.outliers <- 100 * width.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje relativamente altodel total de datos
  
  #comportamiento de la variable sin outliers
  width.sin.missings.outliers <- setdiff(na.omit(cars.data$width), width.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(width.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme totalmente
  
  #fit beta distribution
  normalized = (width.sin.missings.outliers-min(width.sin.missings.outliers))/(max(width.sin.missings.outliers)-min(width.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  width.without.outliers.unif <- fitdist(width.sin.missings.outliers, "unif", method = "mme") 
  plot(width.without.outliers.unif) #se ajusta muy bien a la distribución uniforme
  
  ggplot() + aes(width.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(width.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=width.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs width
  ggplot(cars.data, aes(x=width, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(width, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$width, cars.data$price, use = "pairwise.complete.obs") #baja correlación
  cor(cars.data$width, log(cars.data$price),  use = "pairwise.complete.obs") #correlación media: 0.5134244
  
  summary(lm(price ~ width, data=cars.data)) #significativa
  summary(lm(log(price) ~ width, data=cars.data)) #significativa

  

  

  
  ###### Variable:"seats" ###### 
  str(cars.data$seats) #tipo de dato char 
  sum(cars.data$seats=="")
  table(cars.data$seats)
  
  sort(table(cars.data$seats),decreasing = TRUE) #La mayoría de los datos tienen valor 5 con mucha diferencia con el resto
                                                 #Existen dos valores muy extramos: 14 y 16
  
  cars.data[cars.data$seat ==14 | cars.data$seat ==16, "link"] #ok: son tipo bus
  
  ggplot(data=cars.data, aes(seats)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  

  cars.data$seats <- as.factor(cars.data$seats)
  
  # Price vs seats
  ggplot(cars.data,aes(x=seats, y=price, group=seats)) + geom_boxplot(aes(color=seats)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data,aes(x=seats, y=log(price), group=seats)) + geom_boxplot(aes(color=seats)) + theme_bw() + theme(panel.border=element_blank())
 
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ (seats), data=cars.data)) #no significativa
  summary(lm(log(price) ~ (seats), data=cars.data)) #no significativa, solo el intercepto es significativo
  
  
  
  
  
  ###### Variable: "deposit_size"  ###### 
  str(cars.data$deposit_size) #tipo de dato char pero debe ser numérico
  sum(cars.data$deposit_size=="")
  
  cars.data$deposit_size <- gsub(" l", "", cars.data$deposit_size) #eliminando el vocablo " l" de los valores de la variable
  cars.data$deposit_size <- as.numeric(cars.data$deposit_size) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$deposit_size) #las medidas de tendencia central mustran que es una distribución muy balanceada
                                  #se observan valores con cero, serán coches electricos?
  
  cars.data[cars.data$deposit_size==0,] #analizando los valores cero de la variable. 
  length(cars.data$deposit_size[cars.data$deposit_size==0]) #cantidad de valores cero de la variable. 
  
  #Existen datos con valor cero q no pertenecen a coches eléctricos, son valores irreales, por tanto se convertirán a missing values
  cars.data$deposit_size[cars.data$deposit_size==0 & cars.data$fuel != "Eléctrico"]  <- NA
  sum(is.na(cars.data$deposit_size))
  
  ggplot(data=cars.data, aes(deposit_size)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(deposit_size, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, deposit_size))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  descdist(as.vector(na.omit(cars.data$deposit_size)), discrete = FALSE, boot=500) # La distribución más cercana es la lognormal pero esta no se podrá aplicar porque la variable tiene valores cero
  
  #comportamiento de outliers
  deposit_size.outlier.values <- boxplot.stats(cars.data$deposit_size)$out  # outlier values.
  deposit_size.cantidad.outliers <- length(deposit_size.outlier.values) #se detectan gran número de outliers
  deposit_size.porciento.outliers <- 100 * deposit_size.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje relativamente bajo del total de datos
  
  #comportamiento de la variable sin outliers
  deposit_size.sin.missings.outliers <- setdiff(na.omit(cars.data$deposit_size), deposit_size.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(deposit_size.sin.missings.outliers, discrete = FALSE, boot=500) #la observaión se ajusta a las distribuciones beta y uniforme totalmente
  
  #fit beta distribution
  normalized = (deposit_size.sin.missings.outliers-min(deposit_size.sin.missings.outliers))/(max(deposit_size.sin.missings.outliers)-min(deposit_size.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  deposit.without.outliers.unif <- fitdist(deposit_size.sin.missings.outliers, "unif", method = "mme") 
  plot(deposit.without.outliers.unif) #se ajusta muy bien a la distribución uniforme
  
  ggplot() + aes(deposit_size.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(deposit_size.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=deposit_size.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs deposit_size
  ggplot(cars.data, aes(x=deposit_size, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=deposit_size, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$deposit_size, cars.data$price, use = "pairwise.complete.obs") #no hay correlacion lineal entre estas variables
  cor(cars.data$deposit_size, log(cars.data$price), use = "pairwise.complete.obs") #no hay correlacion lineal entre estas variables

  summary(lm(price ~ deposit_size, data=cars.data)) #significativa
  summary(lm(log(price) ~ deposit_size, data=cars.data)) #significativa


  
  
  
  ###### Variable: "weight"   ###### 
  str(cars.data$weight) #tipo de dato char pero debe ser numérico
  sum(cars.data$weight=="")
  
  cars.data$weight <- gsub(" kg", "", cars.data$weight) #eliminando el vocablo " kg" de los valores de la variable
  cars.data$weight <- gsub("\\.", "", cars.data$weight) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  cars.data$weight <- as.numeric(cars.data$weight) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$weight) #las medidas de tendencia central están relativamente cerca, indican que es una distribución bastante balanceada
                            #se detectan valores cero en la variable, esto es un error
  
  sum(cars.data$weight==0)
  cars.data$weight[cars.data$weight==0] <- NA
  sum(is.na(cars.data$weight))
   
  ggplot(data=cars.data, aes(weight)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(weight, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, weight))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
 
  descdist(as.vector(na.omit(cars.data$weight)), discrete = FALSE, boot=500) #la observación queda sobre la distribucioón lognormal exclusivamente y cerca a la distribución gamma 
  
  weight.log.normal <- fitdist(as.vector(na.omit(cars.data$weight)), "lnorm") #fit log-normal distribution
 
  weight.gamma <- fitdist(as.numeric(na.omit(cars.data$weight)), "gamma", method = "mme") 

  par(mfrow=c(2,2))
  plot.legend <- c( "Log-Normal", "Gamma")
  denscomp(list(weight.log.normal, weight.gamma), legendtext = plot.legend)
  cdfcomp (list(weight.log.normal, weight.gamma), legendtext = plot.legend)
  qqcomp  (list(weight.log.normal, weight.gamma), legendtext = plot.legend)
  ppcomp  (list(weight.log.normal, weight.gamma), legendtext = plot.legend)
  par(mfrow=c(1,1))
  
  #comportamiento de outliers
  weight.outlier.values <- boxplot.stats(cars.data$weight)$out  # outlier values.
  weight.cantidad.outliers <- length(weight.outlier.values) #se detectan bastantes outliers
  weight.porciento.outliers <- 100 * weight.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje aceptable del total de datos
  
  #comportamiento de la variable sin outliers
  weight.sin.missings.outliers <- setdiff(na.omit(cars.data$weight), weight.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(weight.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme 
  
  #fit beta distribution
  normalized = (weight.sin.missings.outliers-min(weight.sin.missings.outliers))/(max(weight.sin.missings.outliers)-min(weight.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  weight.without.outliers.unif <- fitdist(weight.sin.missings.outliers, "unif", method = "mme") 
  
  #fit normal distribution, 
  weight.without.outliers.normal <- fitdist(weight.sin.missings.outliers, "norm", method = "mle") 
  
  #fit lognormal distribution 
  weight.without.outliers.lognormal <- fitdist(weight.sin.missings.outliers, "lnorm", method = "mle") 
  
  par(mfrow=c(2,2))
  plot.legend <- c("Uniforme", "Normal", "Log-normal")
  denscomp(list( weight.without.outliers.unif, weight.without.outliers.normal, weight.without.outliers.lognormal), legendtext = plot.legend)
  cdfcomp (list( weight.without.outliers.unif, weight.without.outliers.normal, weight.without.outliers.lognormal), legendtext = plot.legend)
  qqcomp  (list( weight.without.outliers.unif, weight.without.outliers.normal, weight.without.outliers.lognormal), legendtext = plot.legend)
  ppcomp  (list( weight.without.outliers.unif, weight.without.outliers.normal, weight.without.outliers.lognormal), legendtext = plot.legend)
  par(mfrow=c(1,1))
  
  ggplot() + aes(weight.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(weight.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=weight.sin.missings.outliers, x= 1, geom = "boxplot") 

  # Price vs weight
  ggplot(cars.data, aes(x=weight, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=weight, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$weight, cars.data$price, use = "pairwise.complete.obs") # no mucha correlación lineal
  cor(cars.data$weight, log(cars.data$price), use = "pairwise.complete.obs") #un poco más cocrrelado pero no llega a la media: 0.4690257

  summary(lm(log(price) ~ weight, data=cars.data)) #significativa
  summary(lm(log(price) ~ log(weight), data=cars.data)) #significativa

  
  
  
  
  
  ###### Variable: "body_style"  ######  PENDIENTEEEEEE
  str(cars.data$body_style) #tipo de dato char
  sum(cars.data$body_style=="") #no hay campos vacíos
  
  table(cars.data$body_style) 
  unique(cars.data$body_style)
  
  # GENERAL
  cars.data$body_style[grepl(toupper("COMBI"),               toupper(cars.data$body_style), fixed=TRUE) ] <- "Monovolumen"
  cars.data$body_style[grepl(toupper("CHASIS DOBLE CABINA"), toupper(cars.data$body_style), fixed=TRUE) ] <- "Monovolumen"
  cars.data$body_style[grepl(toupper("Convertible"),         toupper(cars.data$body_style), fixed=TRUE) ] <- "Cabrio"
  cars.data$body_style[grepl(toupper("Pick-Up Doble Cabina"),toupper(cars.data$body_style), fixed=TRUE) ] <- "Pick-Up"
  cars.data$body_style[grepl(toupper("Stationwagon"),        toupper(cars.data$body_style), fixed=TRUE) ] <- "Berlina"
  cars.data$body_style[grepl(toupper("Roadster"),            toupper(cars.data$body_style), fixed=TRUE) ] <- "Cabrio"
  cars.data$body_style[grepl(toupper("Bus"),                 toupper(cars.data$body_style), fixed=TRUE) ] <- "Furgon"
  
  allbrands = unique(cars.data[c("brand")])
  
  # ABARTH
  cars.data$body_style[(cars.data$body_style=="Cabrio" & cars.data$brand=="ABARTH" & cars.data$sbrand=="500")] <- "Berlina"
  
  # ASTON MARTIN
  cars.data$body_style[(cars.data$body_style=="Cabrio" & cars.data$brand=="ASTON MARTIN" & cars.data$sbrand=="VANQUISH")] <- "Coupe"
  
  # AUDI
  cars.data$body_style[(cars.data$body_style=="Todo Terreno" & cars.data$brand=="AUDI" & cars.data$sbrand=="A6 ALLROAD")] <- "Monovolumen"
  cars.data$body_style[(cars.data$body_style=="Todo Terreno" & cars.data$brand=="AUDI" & cars.data$sbrand=="A4 ALLROAD")] <- "Monovolumen"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="AUDI" & cars.data$sbrand=="A2")] <- "Monovolumen"
  
  # BMW
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="BMW" & cars.data$sbrand=="SERIE 3")] <- "Monovolumen"
  
  # CHEVROLET
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="CHEVROLET" & cars.data$sbrand=="SPARK 3")] <- "Monovolumen"
  
  # CITROEN
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="CITROEN" & cars.data$sbrand=="BERLINGO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="CITROEN" & cars.data$sbrand=="JUMPER")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="CITROEN" & cars.data$sbrand=="JUMPY")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Todo Terreno" & cars.data$brand=="CITROEN" & cars.data$sbrand=="C5 CROSS TOURER")] <- "Monovolumen"
  
  # DS
  cars.data$body_style[(cars.data$body_style=="Todo Terreno" & cars.data$brand=="DS" & cars.data$sbrand=="DS4 CROSSBACK")] <- "Berlina"
  
  # FIAT
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="FIAT" & cars.data$sbrand=="TALENTO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="FIAT" & cars.data$sbrand=="DUCATO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="FIAT" & cars.data$sbrand=="SCUDO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="FIAT" & cars.data$sbrand=="DOBLO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="FIAT" & cars.data$sbrand=="SEDICI")] <- "Monovolumen"
  cars.data$body_style[(cars.data$body_style=="Cabrio" & cars.data$brand=="FIAT" & cars.data$sbrand=="PUNTO")] <- "Berlina"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="FIAT" & cars.data$sbrand=="CROMA")] <- "Monovolumen"
  
  # FORD
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="FORD" & cars.data$sbrand=="TRANSIT")] <- "Furgon"
  
  # HYUNDAI
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="HYUNDAI" & cars.data$sbrand=="H350")] <- "Furgon"
  
  # IVECO
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="IVECO" & cars.data$sbrand=="MASSIF COMERCIAL")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="IVECO" & cars.data$sbrand=="DAILY")] <- "Furgon"
  
  # JEEP
  cars.data$body_style[(cars.data$body_style=="Targa" & cars.data$brand=="JEEP" & cars.data$sbrand=="WRANGLER")] <- "Todo Terreno"
  
  # LADA
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="LADA" & cars.data$sbrand=="NIVA")] <- "Todo Terreno"
  
  # LAND ROVER
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="LAND ROVER" & cars.data$sbrand=="DEFENDER COMERCIAL")] <- "Todo Terreno"
  cars.data$body_style[(cars.data$body_style=="Pick-Up" & cars.data$brand=="LAND ROVER" & cars.data$sbrand=="DEFENDER")] <- "Todo Terreno"
  cars.data$body_style[(cars.data$body_style=="Pick-Up" & cars.data$brand=="LAND ROVER" & cars.data$sbrand=="DEFENDER COMERCIAL")] <- "Todo Terreno"
  
  # MERCEDES
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="MERCEDES" & cars.data$sbrand=="SPRINTER")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="MERCEDES" & cars.data$sbrand=="CLASE E CHASIS")] <- "Berlina"
  
  # MITSUBISHI
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="MITSUBISHI" & cars.data$sbrand=="CANTER")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="MITSUBISHI" & cars.data$sbrand=="SPACE STAR")] <- "Monovolumen"
  
  # NISSAN
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="NP300")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="ATLEON")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="L")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="PICK")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="CABSTAR")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="NISSAN" & cars.data$sbrand=="NV400")] <- "Furgon"
  
  # OPEL
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="OPEL" & cars.data$sbrand=="VIVARO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="OPEL" & cars.data$sbrand=="MOVANO")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="OPEL" & cars.data$sbrand=="SIGNUM")] <- "Monovolumen"
  
  # PEUGEOT
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="PEUGEOT" & cars.data$sbrand=="BOXER")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="PEUGEOT" & cars.data$sbrand=="PARTNER")] <- "Furgon"
  
  # PIAGGIO
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="PIAGGIO" & cars.data$sbrand=="PORTER")] <- "Furgon"
  
  # PORSCHE
  cars.data$body_style[(cars.data$body_style=="Targa" & cars.data$brand=="PORSCHE" & cars.data$sbrand=="911")] <- "Coupe"
  
  # RENAULT
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="RENAULT" & cars.data$sbrand=="MAXITY")] <- "Pick-Up"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="RENAULT" & cars.data$sbrand=="MASTER")] <- "Furgon"
  
  # TOYOTA
  cars.data$body_style[(cars.data$body_style=="Targa" & cars.data$brand=="TOYOTA" & cars.data$sbrand=="SUPRA")] <- "Coupe"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="TOYOTA" & cars.data$sbrand=="DYNA")] <- "Pick-Up"
  
  # SUZUKI
  cars.data$body_style[(cars.data$brand=="SUZUKI" & cars.data$sbrand=="JIMNY")] <- "Todo Terreno"
  cars.data$body_style[(cars.data$body_style=="Targa" & cars.data$brand=="SUZUKI" & cars.data$sbrand=="SAMURAI")] <- "Todo Terreno"
  cars.data$body_style[(cars.data$body_style=="Targa" & cars.data$brand=="SUZUKI" & cars.data$sbrand=="VITARA")] <- "Todo Terreno"
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="SUZUKI" & cars.data$sbrand=="BALENO")] <- "Monovolumen"
  
  # VOLKSWAGEN
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="VOLKSWAGEN" & cars.data$sbrand=="CRAFTER")] <- "Furgon"
  cars.data$body_style[(cars.data$body_style=="Chasis" & cars.data$brand=="VOLKSWAGEN" & cars.data$sbrand=="TRANSPORTER")] <- "Furgon"
  
  # VOLVO
  cars.data$body_style[(cars.data$body_style=="Berlina" & cars.data$brand=="VOLVO" & cars.data$sbrand=="V90")] <- "Monovolumen"
  
  #convertir la variable a factor
  cars.data$body_style <- as.factor(cars.data$body_style)
  
  ggplot(data=cars.data, aes(body_style)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  
  
  
  # Price vs body_style
  
  ggplot(cars.data,aes(x=body_style, y=price, group=body_style)) + geom_boxplot(aes(color=body_style)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data,aes(x=body_style, y=log(price), group=body_style)) + geom_boxplot(aes(color=body_style)) + theme_bw() + theme(panel.border=element_blank())

  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ body_style, data=cars.data)) # todos sus valores son significativos
  summary(lm(log(price) ~ body_style, data=cars.data)) #todos sus valores son significativos
  
  chisq.test(table(cars.data$body_style, as.factor()) )
  
  #body_style vs brand.sbrand.model
  
  brand.sbrand.model <- paste(cars.data$brand, cars.data$sbrand, cars.data$model,  sep = " ")
  
  brand.sbrand.model<-as.data.frame( brand.sbrand.model)
  brand.sbrand.model$brand.sbrand.model <- as.factor( brand.sbrand.model$ brand.sbrand.model)
  names( brand.sbrand.model)
  brand.sbrand.model$bs <- as.factor(cars.data$body_style)
  
  chisq.test(table( brand.sbrand.model) , simulate.p.value = TRUE)
  
  library(lsr) 
  cramersV( table( brand.sbrand.model ) )
  

  
  
  ###### Variable: "max_speed"   ###### 
  str(cars.data$max_speed) #tipo de dato char pero debe ser numérico
  
  cars.data$max_speed <- gsub(" km/h", "", cars.data$max_speed) #eliminando el vocablo " km/h" de los valores de la variable
  cars.data$max_speed <- gsub("\\.", "", cars.data$max_speed) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  cars.data$max_speed <- as.numeric(cars.data$max_speed) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$max_speed) #las media y la mediana tienen valores bastante cercanos, por tanto la distribución está aparentemente bien balanceada
                               #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$max_speed==0,] #analizando los valores cero de la variable. 
  length(cars.data$max_speed[cars.data$max_speed==0]) #cantidad de valores cero de la variable. 
  cars.data$max_speed[cars.data$max_speed==0]<-NA #los valores cero de la variable son convertidos a NA
  NA.max_speed.percent <- 100 * sum(is.na(cars.data)) / nrow(cars.data) 
  print(NA.max_speed.percent)  #porcentaje de NA bastante elevado sum(is.na(cars.data$max_speed)) 
  
  ggplot(data=cars.data, aes(max_speed)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(max_speed, na.rm=T)), color="darkblue", linetype="dashed", size=1) #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, max_speed))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan algunos ourliers
  
  # a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$max_speed)), discrete = FALSE, boot=500) #la observación queda cerca de la distribución lognomam 
  
  max_speed.log.normal <- fitdist(as.vector(na.omit(cars.data$max_speed)), "lnorm") #fit log-normal distribution
  plot(max_speed.log.normal)

  #comportamiento de outliers
  max_speed.outlier.values <- boxplot.stats(cars.data$max_speed)$out  # outlier values.
  max_speed.cantidad.outliers <- length(max_speed.outlier.values) #se detectan bastantes outliers
  max_speed.porciento.outliers <- 100 * max_speed.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje aceptable del total de datos
  
  #comportamiento de la variable sin outliers
  max_speed.sin.missings.outliers <- setdiff(na.omit(cars.data$max_speed), max_speed.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(max_speed.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme
  
  #fit beta distribution
  normalized = (max_speed.sin.missings.outliers-min(max_speed.sin.missings.outliers))/(max(max_speed.sin.missings.outliers)-min(max_speed.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  weight.without.outliers.unif <- fitdist(weight.sin.missings.outliers, "unif", method = "mme") 
  plot(weight.without.outliers.unif)
  
  ggplot() + aes(max_speed.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(max_speed.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=max_speed.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs max_speed
  ggplot(cars.data, aes(x=max_speed, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=max_speed, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$max_speed, cars.data$price, use = "pairwise.complete.obs") #correlación media
  cor(cars.data$max_speed, log(cars.data$price), use = "pairwise.complete.obs") #mejor correlacion, sigue siendo mdeia
 
  summary(lm(price ~ max_speed, data=cars.data)) #variable significativa
  summary(lm(log(price) ~ max_speed, data=cars.data)) #variable significativa

  
  
  
  
  
    
  
  ###### Variable:"urban_consume" ###### 
  str(cars.data$urban_consume) #tipo de dato char pero debe ser numérico
  sum(cars.data$urban_consume=="")#0
  
  cars.data$urban_consume <- gsub(" l", "", cars.data$urban_consume) #eliminando el vocablo " l" de los valores de la variable
  cars.data$urban_consume <- as.numeric(gsub(",", ".", cars.data$urban_consume, fixed = TRUE)) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$urban_consume) #las media y la mediana tienen valores bastante cercanos relativamente, por tanto la distribución está aparentemente bien balanceada
                                   #no obstante se observa un valor máximo bastante elevado con respecto al resto de los valores
                                   #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$urban_consume==0,] #analizando los valores cero de la variable. 
  length(cars.data$urban_consume[cars.data$urban_consume==0]) #cantidad de valores cero de la variable. 
  cars.data$urban_consume[cars.data$urban_consume==0 & cars.data$fuel!="Eléctrico"]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$urban_consume))
  
  ggplot(data=cars.data, aes(urban_consume)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(urban_consume, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, urban_consume))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media
  
  
  #a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$urban_consume)), discrete = FALSE, boot=500) #la observación queda cerca de la distrib gamma
  
  urban_consume.gamma <- fitdist(antig.sin.outliers, "gamma", method = "mme")
  plot(urban_consume.gamma)
  
  #comportamiento de outliers
  urban_consume.outlier.values <- boxplot.stats(cars.data$urban_consume)$out  # outlier values.
  urban_consume.cantidad.outliers <- length(urban_consume.outlier.values) #se detectan bastantes outliers
  urban_consume.porciento.outliers <- 100 * urban_consume.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje bastante elevado con respecto al total de datos
  print(urban_consume.cantidad.outliers)
  
  #comportamiento de la variable sin outliers
  urban_consume.sin.missings.outliers <- setdiff(na.omit(cars.data$urban_consume), urban_consume.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(urban_consume.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y uniforme
  
  #fit beta distribution
  normalized = (urban_consume.sin.missings.outliers-min(urban_consume.sin.missings.outliers))/(max(urban_consume.sin.missings.outliers)-min(urban_consume.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  urban_consume.without.outliers.unif <- fitdist(urban_consume.sin.missings.outliers, "unif", method = "mme") 
  plot(urban_consume.without.outliers.unif)
  
  ggplot() + aes(urban_consume.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(urban_consume.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=urban_consume.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs urban_consume
  ggplot(cars.data, aes(x=urban_consume, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=urban_consume, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$urban_consume, cars.data$price, use = "pairwise.complete.obs") #no correladas 
  cor(cars.data$urban_consume, log(cars.data$price), use = "pairwise.complete.obs") #no correladas 
    
  summary(lm(price ~ urban_consume, data=cars.data)) #significativa
  summary(lm(log(price) ~ urban_consume, data=cars.data)) #significativa
  
  
  
  
  
  
  ###### Variable:"rural_consume" ###### 
  str(cars.data$rural_consume) #tipo de dato char pero debe ser numérico
  sum(cars.data$rural_consume=="")
  
  cars.data$rural_consume <- gsub(" l", "", cars.data$rural_consume) #eliminando el vocablo " l" de los valores de la variable
  cars.data$rural_consume <- as.numeric(gsub(",", ".", cars.data$rural_consume, fixed = TRUE)) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$rural_consume) #las media y la mediana tienen valores bastante cerca relativamente, por tanto la distribución está aparentemente bien balanceada
                                   #no obstante se observa un valor máximo bastante elevado con respecto al resto de los valores
                                   #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$rural_consume==0,] #analizando los valores cero de la variable. 
  length(cars.data$rural_consume[cars.data$rural_consume==0]) #cantidad de valores cero de la variable. 
  cars.data$rural_consume[cars.data$rural_consume==0 & cars.data$fuel!="Eléctrico"]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$rural_consume))
  
  ggplot(data=cars.data, aes(rural_consume)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(rural_consume, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, rural_consume))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media
  
  #a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$rural_consume)), discrete = FALSE, boot=500) #la observación queda cerca de la distrib gamma
  
  plot(fitdist(as.vector(na.omit(cars.data$rural_consume)), "gamma", method = "mme"))
  
  #comportamiento de outliers
  rural_consume.outlier.values <- boxplot.stats(cars.data$rural_consume)$out  # outlier values.
  rural_consume.cantidad.outliers <- length(rural_consume.outlier.values) #se detectan bastantes outliers
  rural_consume.porciento.outliers <- 100 * rural_consume.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje bastante elevado con respecto al total de datos
  print(rural_consume.porciento.outliers)
  
  #comportamiento de la variable sin outliers
  rural_consume.sin.missings.outliers <- setdiff(na.omit(cars.data$rural_consume), urban_consume.outlier.values)
  summary(rural_consume.sin.missings.outliers)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(rural_consume.sin.missings.outliers, discrete = FALSE, boot=500) #la observaión se ajusta a las distribuciones gamma y beta
  
  #fit beta distribution
  normalized = (rural_consume.sin.missings.outliers-min(rural_consume.sin.missings.outliers))/(max(rural_consume.sin.missings.outliers)-min(rural_consume.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit unif distribution, 
  rural_consume.without.outliers.lnorm <- fitdist(rural_consume.sin.missings.outliers, "unif", method = "mme") 
  plot(rural_consume.without.outliers.lnorm)
  
  ggplot() + aes(rural_consume.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(rural_consume.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=rural_consume.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs rural_consume
  ggplot(cars.data, aes(x=rural_consume, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=rural_consume, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$rural_consume, cars.data$price, use = "pairwise.complete.obs") #no correladas
  cor(cars.data$rural_consume, log(cars.data$price), use = "pairwise.complete.obs") #no correladas
  
  summary(lm(price ~ rural_consume, data=cars.data)) #significativa
  summary(lm(log(price) ~ rural_consume, data=cars.data)) #significativa
  
  
  
  
  ###### Variable: "combined_consume"  ###### 
  str(cars.data$combined_consume) #tipo de dato char pero debe ser numérico
  sum(cars.data$combined_consume=="") #0
  
  cars.data$combined_consume <- gsub(" l", "", cars.data$combined_consume) #eliminando el vocablo " l" de los valores de la variable
  cars.data$combined_consume <- as.numeric(gsub(",", ".", cars.data$combined_consume, fixed = TRUE)) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$combined_consume) #las media y la mediana tienen valores bastante cercanos, por tanto la distribución está aparentemente bien balanceada
  #se detectan valores en cero, no puede ser esto posible si el coche no es eléctrico.
  
  cars.data[cars.data$combined_consume==0,] #analizando los valores cero de la variable. 
  length(cars.data$combined_consume[cars.data$combined_consume==0]) #cantidad de valores cero de la variable. 
  cars.data$combined_consume[cars.data$combined_consume==0 & cars.data$fuel!="Eléctrico"]<-NA #los valores cero de la variable que no son eléctrico son convertidos a NA
  sum(is.na(cars.data$combined_consume))
  
  ggplot(data=cars.data, aes(combined_consume)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(combined_consume, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, combined_consume))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media
  
  # a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$combined_consume)), discrete = FALSE, boot=500) #la observación queda cerca de la distrib gamma, beta y exp 
  
  combined_consume.fit.gamma <- fitdist(as.vector(na.omit(cars.data$combined_consume)), "gamma", method = "mme") #fit gamma distribution, 
  combined_consume.fit.exp <- fitdist(as.vector(na.omit(cars.data$combined_consume)), "exp", method = "mme") #fit exp distribution, 
  
  par(mfrow=c(2,2))
  plot.legend <- c("Gamma", "Exponencial")
  denscomp(list(combined_consume.fit.gamma, combined_consume.fit.exp), legendtext = plot.legend)
  cdfcomp (list(combined_consume.fit.gamma, combined_consume.fit.exp), legendtext = plot.legend)
  qqcomp  (list(combined_consume.fit.gamma, combined_consume.fit.exp), legendtext = plot.legend)
  ppcomp  (list(combined_consume.fit.gamma, combined_consume.fit.exp), legendtext = plot.legend)
  par(mfrow=c(1,1))
  
  #comportamiento de outliers
  combined_consume.outlier.values <- boxplot.stats(cars.data$combined_consume)$out  # outlier values.
  combined_consume.cantidad.outliers <- length(combined_consume.outlier.values) #se detectan bastantes outliers
  combined_consumeporciento.outliers <- 100 * combined_consume.cantidad.outliers / nrow(cars.data) #los outliers representan un porcentaje bastante alevado con respecto al total de datos
  print(combined_consumeporciento.outliers)
  
  #comportamiento de la variable sin outliers
  combined_consume.sin.missings.outliers <- setdiff(na.omit(cars.data$combined_consume), combined_consume.outlier.values)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(combined_consume.sin.missings.outliers, discrete = FALSE, boot=500) #beta y uniforme
  #fit beta distribution
  normalized = (combined_consume.sin.missings.outliers-min(combined_consume.sin.missings.outliers))/(max(combined_consume.sin.missings.outliers)-min(combined_consume.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  combined_consume.without.outliers.unif <- fitdist(combined_consume.sin.missings.outliers, "unif", method = "mme") 
  plot(combined_consume.without.outliers.unif)
  
  #visualización
  ggplot() + aes(combined_consume.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(combined_consume.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  qplot(y=combined_consume.sin.missings.outliers, x= 1, geom = "boxplot") 
  
  # Price vs combined_consume
  ggplot(cars.data, aes(x=combined_consume, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=combined_consume, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$combined_consume, cars.data$price, use = "pairwise.complete.obs")
  cor(cars.data$combined_consume, log(cars.data$price), use = "pairwise.complete.obs")
  
  summary(lm(price ~ combined_consume, data=cars.data)) #variables no correladas linealmente
  summary(lm(log(price) ~ combined_consume, data=cars.data)) #variables no correladas linealmente

  
  
  
  ###### Variable:"acceleration0to100" ######
  str(cars.data$acceleration0to100) #tipo de dato char pero debe ser numérico
  sum(cars.data$acceleration0to100=="")
  
  cars.data$acceleration0to100 <- gsub(" s", "", cars.data$acceleration0to100) #eliminando el vocablo " s" de los valores de la variable
  cars.data$acceleration0to100 <- as.numeric(gsub(",", ".", cars.data$acceleration0to100, fixed = TRUE)) #convirtiendo a numérico los valores de la variable
  
  summary(cars.data$acceleration0to100) #las media y la mediana tienen valores cercanos relativamente, por tanto la distribución está aparentemente bien balanceada
                                        #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$acceleration0to100==0,] #analizando los valores cero de la variable. 
  length(cars.data$acceleration0to100[cars.data$acceleration0to100==0]) #cantidad de valores cero de la variable. 
  cars.data$acceleration0to100[cars.data$acceleration0to100==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$acceleration0to100))
  
  ggplot(data=cars.data, aes(acceleration0to100)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(acceleration0to100, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, acceleration0to100))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media
  
  cars.data[which.max(cars.data$acceleration0to100),]#el registro es de un  BMW SERIE 1 125Ia, se a investigado y el valor real es aproximadamente 7 segundos
  cars.data$acceleration0to100[which.max(cars.data$acceleration0to100)] <- 7
  summary(cars.data$acceleration0to100)
  
  #a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$acceleration0to100)), discrete = FALSE, boot=500) #la observación queda alejado de cualquier distribución

  #comportamiento de outliers
  ac0to100.outlier.values <- boxplot.stats(cars.data$acceleration0to100)$out  # outlier values.
  ac0to100.cantidad.outliers <- length(ac0to100.outlier.values) #se detectan bastantes outliers
  ac0to100.porciento.outliers <- 100 * ac0to100.cantidad.outliers / nrow(cars.data)
  print(ac0to100.porciento.outliers)
  
  #comportamiento de la variable sin outliers
  acc0to100.sin.missings.outliers <- setdiff(na.omit(cars.data$acceleration0to100), ac0to100.outlier.values)
  summary(acc0to100.sin.missings.outliers)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(acc0to100.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones uniforme y beta
  
  #fit beta distribution
  normalized = (acc0to100.sin.missings.outliers-min(acc0to100.sin.missings.outliers))/(max(acc0to100.sin.missings.outliers)-min(acc0to100.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  acc0to100.without.outliers.unif <- fitdist(acc0to100.sin.missings.outliers, "unif", method = "mme") 
  plot(acc0to100.without.outliers.unif)
  
  ggplot() + aes(acc0to100.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(acc0to100.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  # Price vs acceleration0to100
  ggplot(cars.data, aes(x=acceleration0to100, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=acceleration0to100, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$acceleration0to100, cars.data$price, use = "pairwise.complete.obs") # correlación media
  cor(cars.data$acceleration0to100, log(cars.data$price), use = "pairwise.complete.obs") # correlación media, mejora el anterior
 
  summary(lm(price ~ acceleration0to100, data=cars.data)) #variable significativa
  summary(lm(log(price) ~ acceleration0to100, data=cars.data))  #variable significativa
  
  
  
  
  ###### Variable:"autonomy" ######  NOTA: PENDIENTE DE REVISAR PORQUE TODOS LOS VALORES TIENEN VALOR CERO
  str(cars.data$autonomy) #tipo de dato char pero debe ser numérico
  table(cars.data$autonomy) #todos los valores de la variable tienen valor cero 
  cars.data$autonomy <- NULL
  #cars.data$autonomy <- as.factor(cars.data$autonomy)
  
  
  
  
  ###### Variable: "cylinders"    ###### 
  str(cars.data$cylinders) #tipo de dato char
  sum(cars.data$cylinders=="")
  sort(table(cars.data$cylinders),decreasing = T) #la mayor cantidad de datos tienen el valor "4 en línea" con mucha diferencia con el resto
  
  cars.data$cylinders[cars.data$cylinders=='0 en no link (0008)']<-NA #el valor "0 en no link (0008)" no se conoce como valor correcto y se tomará como missing value
  sum(is.na(cars.data$cylinders))                                                
  
  ggplot(data=cars.data, aes(cylinders)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

  cars.data$cylinders <- as.factor(cars.data$cylinders)
  
  # Price vs cylinders
  ggplot(cars.data,aes(x=cylinders, y=price, group=cylinders)) + geom_boxplot(aes(color=cylinders)) + 
    theme_bw() + theme(panel.border=element_blank()) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  ggplot(cars.data,aes(x=cylinders, y=log(price), group=cylinders)) + geom_boxplot(aes(color=cylinders)) +
    theme_bw() + theme(panel.border=element_blank()) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) 

  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ cylinders, data=cars.data)) #la mayoría de los valores son significativos
  summary(lm(log(price) ~ cylinders, data=cars.data)) #menos valores significativos
  
  
  # table(cars.data$cylinders)
  # 
  # prueba <- cars.data
  # 
  # #let´s modify some parts of the dataset for better analisys
  # library(dplyr)
  # library(tidyr)
  # prueba <- prueba %>% separate(cylinders, c("cylinder_numb", "cylinders_pos"), " ") # separar los cilindros en numero de cilindros y su la posición
  # names(prueba)
  #
  # unique(prueba$cylinder_numb)
  # unique(prueba$cylinders_pos)
  # 
  # ggplot(data=prueba, aes(cylinder_numb)) + geom_bar(col="blue",fill="blue",alpha=.4) +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  
  
  
  
  
  ###### Variable:"displacement" ###### cilindrada
  str(cars.data$displacement) #tipo de dato char pero debe ser numérico
  
  cars.data$displacement <- gsub(" cm3", "", cars.data$displacement) #eliminando el vocablo " cm3" de los valores de la variable
  cars.data$displacement <- gsub("\\.", "", cars.data$displacement) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  
  cars.data$displacement <- as.numeric(cars.data$displacement) #convirtiendo los valores a numéricos
  
  
  summary(cars.data$displacement) #las medidas de tendencia central tienen valores bastante distanciados entre entre ellas, por tanto la distribución no está aparentemente bien desbalanciada
                                  #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$displacement==0,] #analizando los valores cero de la variable. 
  length(cars.data$displacement[cars.data$displacement==0]) #cantidad de valores cero de la variable. 
  cars.data$displacement[cars.data$displacement==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$displacement))
  
  ggplot(data=cars.data, aes(displacement)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(displacement, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, displacement))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media
  
  #a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$displacement)), discrete = FALSE) #la observación queda cerca de la distribución gamma
  
  fit.displacement <- fitdist(as.vector(na.omit(cars.data$displacement)), "gamma", method = "mme") 
  plot(fit.displacement)

  #comportamiento de outliers
  displacement.outlier.values <- boxplot.stats(cars.data$displacement)$out  # outlier values.
  displacement.cantidad.outliers <- length(displacement.outlier.values) #se detectan bastantes outliers
  displacement.porciento.outliers <- 100 * displacement.cantidad.outliers / nrow(cars.data)
  
  #comportamiento de la variable sin outliers
  displacement.sin.missings.outliers <- setdiff(na.omit(cars.data$displacement), displacement.outlier.values)
  summary(displacement.sin.missings.outliers)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(displacement.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones uniforme y beta
  
  #fit beta distribution
  normalized = (displacement.sin.missings.outliers-min(displacement.sin.missings.outliers))/(max(displacement.sin.missings.outliers)-min(displacement.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  displacement.without.outliers.unif <- fitdist(displacement.sin.missings.outliers, "unif", method = "mme") 
  plot(displacement.without.outliers.unif)
  
  ggplot() + aes(acc0to100.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(acc0to100.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  # Price vs displacement
  ggplot(cars.data, aes(x=displacement, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=displacement, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$displacement, cars.data$price, use = "pairwise.complete.obs") #correlación media
  cor(cars.data$displacement, log(cars.data$price), use = "pairwise.complete.obs") #correlación media, mejor al anterior
 
  summary(lm(price ~ displacement, data=cars.data)) #significativa
  summary(lm(log(price) ~ displacement, data=cars.data)) #significativa
  
  
  
  
  
  
  ###### Variable: "max_pairs"  ###### 
  str(cars.data$max_pairs) #tipo de dato char pero debe ser numérico
  
  cars.data$max_pairs <- gsub(" Nm", "", cars.data$max_pairs) #eliminando el vocablo " Nm" de los valores de la variable
  cars.data$max_pairs <- gsub("\\.", "", cars.data$max_pairs) #eliminando el punto de los valores de la variable para que no los entienda como decimales
  
  cars.data$max_pairs <- as.numeric(cars.data$max_pairs) #convirtiendo los valores a numéricos
  
  
  summary(cars.data$max_pairs) #las medidas de tendencia central tienen valores bastante distanciados entre entre ellas, por tanto la distribución está aparentemente bien desbalanciada
                               #se detectan valores en cero, esto no está bien
  
  cars.data[cars.data$max_pairs==0,] #analizando los valores cero de la variable. 
  length(cars.data$max_pairs[cars.data$max_pairs==0]) #cantidad de valores cero de la variable. 
  cars.data$max_pairs[cars.data$max_pairs==0]<-NA #los valores cero de la variable son convertidos a NA
  sum(is.na(cars.data$max_pairs))
  
  ggplot(data=cars.data, aes(max_pairs)) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))  + 
    geom_vline(aes(xintercept=mean(max_pairs, na.rm=T)), color="darkblue", linetype="dashed", size=1)   #efectivamente se tiene una distribución de la variable muy balanceada, no obstante se observan outliers
  
  ggplot(data=cars.data, aes(1, max_pairs))+
    geom_boxplot(fill='blue',color="blue", alpha=.5,outlier.color="red",outlier.fill="red") #se observan bastantes ourliers, y algunos muy alejados de la media

  #a cual distribución pertenece la variables?
  descdist(as.vector(na.omit(cars.data$max_pairs)), discrete = FALSE, boot=500) #la observación queda un poco cerca de la distribución lognormal 
  
  fit.max_pairs.lnorm <- fitdist(as.vector(na.omit(cars.data$displacement)), "lnorm", method = "mme") 
  plot(fit.max_pairs.lnorm)
  
  #comportamiento de outliers
  max_pairs.outlier.values <- boxplot.stats(cars.data$max_pairs)$out  # outlier values.
  max_pairs.cantidad.outliers <- length(max_pairs.outlier.values) #se detectan bastantes outliers
  max_pairs.porciento.outliers <- 100 * max_pairs.cantidad.outliers / nrow(cars.data)
  print(max_pairs.porciento.outliers)
  
  #comportamiento de la variable sin outliers
  max_pairs.sin.missings.outliers <- setdiff(na.omit(cars.data$max_pairs), max_pairs.outlier.values)
  summary(max_pairs.sin.missings.outliers)
  
  #buscando que posible distribución sigue la variable sin outliers
  descdist(max_pairs.sin.missings.outliers, discrete = FALSE, boot=500) #por orden de prioridad la observaión se ajusta a las distribuciones beta y un poco más alejado uniforme
  
  #fit beta distribution
  normalized = (max_pairs.sin.missings.outliers-min(max_pairs.sin.missings.outliers))/(max(max_pairs.sin.missings.outliers)-min(max_pairs.sin.missings.outliers)) #normalize data because beta distribution works with values from 0 to 1
  fit.without.outliers.beta <- fitdist(normalized, "beta", method = "mme") 
  plot(fit.without.outliers.beta)
  
  #fit uniform distribution, 
  max_pairs.without.outliers.unif <- fitdist(max_pairs.sin.missings.outliers, "unif", method = "mme") 
  plot(max_pairs.without.outliers.unif)
  
  ggplot() + aes(max_pairs.sin.missings.outliers) + geom_density(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_vline(aes(xintercept=mean(max_pairs.sin.missings.outliers, na.rm=T)), color="darkblue", linetype="dashed", size=1) #ejemplo eliminando los outliers 
  
  # Price vs max_pairs
  ggplot(cars.data, aes(x=max_pairs, y=price)) + geom_smooth() + ylab("price") + geom_point(alpha=0.8) + ggtitle("")
  ggplot(cars.data, aes(x=max_pairs, y=log(price))) + geom_smooth() + ylab("log price") + geom_point(alpha=0.8) + ggtitle("")
  
  cor(cars.data$max_pairs, cars.data$price, use = "pairwise.complete.obs") #correlación media
  cor(cars.data$max_pairs, log(cars.data$price), use = "pairwise.complete.obs") #correlación media, mejor que la anterior
  
  summary(lm(price ~ max_pairs, data=cars.data)) #significativa
  summary(lm(log(price) ~ max_pairs, data=cars.data))  #significativa
  

  
  
  
  
  ###### Variable: "speeds"   ######
  str(cars.data$speeds) #variable tipo char
  sort(table(cars.data$speeds),decreasing = T) #mayor cantidad de datos: cambios de marcha=6 
                                               #se observan 307 valores de velocidad "1", esto es es posible?
  
  cars.data[cars.data$speeds==1,"fuel"] #analizando los valores "1" de la variable. 
  length(cars.data[cars.data$speeds==1,"fuel"]) #se detectan 307 valores "1" de la variable que pertenecen a coches eléctricos por tanto el valor "1" es correcto
  
  ggplot(data=cars.data, aes(speeds)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  cars.data$speeds <- as.factor(cars.data$speeds)
  
  # Price vs speeds
  ggplot(cars.data, aes(x=speeds, y=price, group=speeds)) + geom_boxplot(aes(color=speeds)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data, aes(x=speeds, y=log(price), group=speeds)) + geom_boxplot(aes(color=speeds)) + theme_bw() + theme(panel.border=element_blank())
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ speeds, data=cars.data)) #significativa
  summary(lm(log(price) ~ speeds, data=cars.data))  #significativa
  
  
  
  
  
  ###### Variable:"transmission" ###### 
  str(cars.data$transmission) #variable tipo char
  sum(cars.data$transmission=="") 
  cars.data$transmission[cars.data$transmission==""] <- NA
  sum(is.na(cars.data$transmission)) 
  
  table(cars.data$transmission)
  unique(cars.data$transmission)
  
  ggplot(data=cars.data, aes(transmission)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  cars.data$transmission[cars.data$transmission=="Automática continua, secuencial" |
                         cars.data$transmission=="Automática secuencial"] <- "CVT"
  cars.data$transmission[cars.data$transmission=="Automática continua"] <- "Automática"
  cars.data$transmission[cars.data$transmission=="Manual automatizada"] <- "Robotizada"
  cars.data$transmission[cars.data$transmission=="Directo, sin caja de cambios"] <- "Sin caja"
  
  
  #convertir a factor la variable
  cars.data$transmission <- as.factor(cars.data$transmission)
  
  # Price vs transmission
  ggplot(cars.data, aes(x=transmission, y=price, group=transmission)) + 
    geom_boxplot(aes(color=transmission)) + theme_bw() + theme(panel.border=element_blank()) 
  
  ggplot(cars.data, aes(x=transmission, y=log(price), group=transmission)) + 
    geom_boxplot(aes(color=transmission)) + theme_bw() + theme(panel.border=element_blank()) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ transmission, data=cars.data)) #no todos los valores significativos
  summary(lm(log(price) ~ transmission, data=cars.data)) #más valores significativos y mejora el R2
  
  
  
  
  ###### Variable: "drive_position"   ######
  str(cars.data$drive_position) #variable tipo char
  sum(cars.data$drive_position=="")
  table(cars.data$drive_position)
  cars.data$drive_position[cars.data$drive_position=="No link (0005)"] <- NA
  
  cars.data$drive_position[cars.data$drive_position=="Todo terreno total conectable" |
                             cars.data$drive_position=="Todo terreno total permanente" |
                             cars.data$drive_position== "Total conectable" |
                             cars.data$drive_position== "Total permanente"
                           ] <- "Total"
  
  ggplot(data=cars.data, aes(drive_position)) + geom_bar(col="blue",fill="blue",alpha=.4) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) 
  
  
  
  cars.data$drive_position <- as.factor(cars.data$drive_position)
  
  # Price vs drive_position
  ggplot(cars.data, aes(x=drive_position, y=price, group=drive_position)) + geom_boxplot(aes(color=drive_position)) + theme_bw() + theme(panel.border=element_blank())
  ggplot(cars.data, aes(x=drive_position, y=log(price), group=drive_position)) + geom_boxplot(aes(color=drive_position)) + theme_bw() + theme(panel.border=element_blank())
  
  #analizando la regresión lineal simple entre el supuesto predictor y la variable objetivo
  summary(lm(price ~ drive_position, data=cars.data)) #significativa
  summary(lm(log(price) ~ drive_position, data=cars.data)) #significativa

  #revisando el dataset
  str(cars.data)
  
  
  ##########################################################################
  # ---     Tratamiento de outliers. Enfoque de modelo multivariante     ---   http://r-statistics.co/Outlier-Treatment-With-R.html
  ##########################################################################
  #Declaring an observation as an outlier based on a just one (rather unimportant) feature could lead to unrealistic inferences.
  #Cook’s distance is a measure computed with respect to a given regression model and therefore is impacted only by the X variables included in the model. 
  #Cook’s distance computes the influence exerted by each data point (row) on the predicted outcome.

  cars.analysis <- cars.data #creando un nuevo dataset para continuar con el análisis
  cars.analysis$link <- NULL #eliminando la variable link
  cars.analysis$superID <- NULL #eliminando la variable id
   
  #creando las fórmulas
  cars.formula <- as.formula(paste("price ~ ", paste( as.factor(names(cars.analysis[c(5:ncol(cars.analysis))])), collapse="+"))) #excluyendo marca, serie y modelo.
  cars.formula.log <- as.formula(paste("log(price) ~ ", paste( as.factor(names(cars.analysis[c(5:ncol(cars.analysis))])), collapse="+"))) #excluyendo marca, serie y modelo.
  
  #fit linear model
  summary(fit.lm <- lm(formula = cars.formula, data = cars.analysis)) #vO original
  summary(fit.lm.log <- lm(formula = cars.formula.log, data = cars.analysis)) #log VO
  
  #fittin gaussian model
  summary(fit.normal <- glm(cars.formula, data = cars.analysis, family = gaussian)) #VO original
  summary(fit.lm.log <- glm(cars.formula, data = cars.analysis, family = gaussian)) #log VO
  
  #inflm <- influence.measures(fit.lm.log)
  
  #buscar el mejor ajuste
  AIC(fit.lm, fit.lm.log, fit.normal) #fit.lm.log es el mejor ajuste
  
  #Cook’s distance
  cooksdist <- cooks.distance(fit.lm.log) #applying Cook’s distance
    
  #plot(cooksdist, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
  plot(cooksdist, main="Influential Obs by Cooks distance")  # plot cook's distance
  abline(h = 4*mean(cooksdist, na.rm=T), col="red")  # add cutoff line
  text(x=1:length(cooksdist)+1, y=cooksdist, labels=ifelse(cooksdist>4*mean(cooksdist, na.rm=T), names(cooksdist),""), col="red")  # add labels
  
  influentials <- as.numeric(names(cooksdist)[(cooksdist > 4*mean(cooksdist, na.rm=T))])  # influential row numbers
  cars.analysis[influentials, ]  # influential observations.
  nrow(na.omit(cars.analysis[influentials, ])) #754 outliers
  
  most.extreme.obs <- head(sort(cooksdist, decreasing = TRUE), n=10) #influentials ordered decresidently
  cars.analysis[names(most.extreme.obs), ] 
  
  the.most.extreme <- cars.analysis[names(which.max(most.extreme.obs)), ] #el outlier más extremo
  print(the.most.extreme)
  
  #create a new dataframe whitout extreme outliers
  cars.analysis.2 <- cars.analysis[-c(as.numeric(the.most.extreme)), ]
  md.log <- lm(formula = cars.formula.log, data = cars.analysis.2) #log VO
  AIC(md.log, fit.lm.log) #mejora el mejor de los modelos anteriores
  
  cars.analysis <- cars.analysis.2
  
  ##########################################################################
  # ---                 Tratamiento de missing valuess                   ---  https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
  ##########################################################################
  
  #······Buscando missing values con respecto a las variables········
  library(dplyr)
  percent.miss.col <- cars.analysis %>% summarise_all(funs(100*mean(is.na(.)))) #porcentaje de missing values en el dataset
  print(percent.miss.col)
  
  library(VIM) #se obtienen los mismos recultados con esta librería, además se visualiza el resultado
  missingXvariable <- aggr(cars.analysis, col=c('navyblue','yellow'),numbers=TRUE, sortVars=TRUE, labels=names(cars.analysis), cex.axis=.7,gap=3, ylab=c("Missing data","Pattern"))
  var.missings.ordered <- missingXvariable$missings[order(missingXvariable$missings$Count, decreasing = TRUE),] 
  var.missings.ordered[apply(var.missings.ordered !=0, 1, all),] #sin ceros


  #se eliminarán las variables con más de 25% de missing values
  cars.analysis <- cars.analysis[ , - which(colMeans(is.na(cars.analysis)) > 0.25)] 
  

  
  #······· buscando missing values con respecto a los registros ········
  percent.miss.row <- cars.analysis[rowSums(is.na(cars.analysis)) > round(ncol(cars.analysis)/3) ,] #detectamos los registros con mas de un 1/3 de missing values con respecto a las variables
  print(head(percent.miss.row))
  dim(percent.miss.row)[1] #193 registros
  100 * dim(percent.miss.row)[1] / dim(cars.analysis)[1] #porciento de registros con mas de 1/3 de missing data
  rownames(percent.miss.row)

  percent.miss.row <- cars.analysis[rowSums(is.na(cars.analysis)) > round(ncol(cars.analysis)/2) ,] #detectamos los registros con mas de un 1/3 de missing values con respecto a las variables
  print(head(percent.miss.row))
  dim(percent.miss.row)[1] #0 registros
  100 * dim(percent.miss.row)[1] / dim(cars.analysis)[1] #porciento de registros con mas de 1/3 de missing data
  rownames(percent.miss.row)
  
  #eliminando los registros con más de 1/2 del total de vairables de missing values
  cars.analysis <- cars.analysis[rowSums(is.na(cars.analysis)) < ncol(cars.analysis)/2, ]
  
  
  #utilizando MICE asumiendo los missing values se presentan de forma MAR (missing at random) 
  library(mice)
  
  #car.imputation <- mice(cars.data[c(4:ncol(cars.data))], m = 5, maxit = 50, method = 'rf', seed = 500) 
  car.imputation <- mice(cars.analysis[c(4:ncol(cars.analysis))], m = 1, maxit = 5, method = 'rf', seed = 500) 
  
  plot(car.imputation) 
  densityplot(car.imputation) #se observan valores de imputación muy diferentes a sus rangos de valores reales
  densityplot(cars.data$urban_consume)

  #densityplot(car.imputation, ~ deposit_size|.imp) 
  
  #finalmente se completan los missing values en el dataset 
  car.imp.MICE <- complete(car.imputation, 1) 
  head(car.imp.MICE)
  any(is.na(car.imp.MICE))
  
  cars.processed <- car.imp.MICE
  any(is.na(car.imp.MICE$fuel))
  

  ################################################################
  
  
  
  # Write CSV in R
  write.table(car.imp.MICE, file = "./Data/cars_preprocessed.csv", row.names=TRUE, col.names=TRUE, sep=",")
  
  library(readr)
  cars_preprocessed <- read_csv("Data/cars_preprocessed.csv")
  summary(cars_preprocessed)  
  
  
  ################################################################

  
  
  
  
  #NOTA: CUANDO CARGO EL DATASET ME DA MISSING VALUES EN LA VARIABLE FUEL Y ESTO NO ES CIERTO
  
#  return(TRUE)
  
#}
  
  
  
