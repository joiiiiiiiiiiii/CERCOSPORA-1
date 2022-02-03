# Variables de Configuración
etiquetasEjeX = c("01/01", "10/01", "20/01", "01/02", "10/02", "20/02",  "01/03", "10/03", "20/03",
				"01/04", "10/04", "20/04", "01/05", "10/05", "20/05","01/06", "10/06", "20/06",
				"01/07", "10/07", "20/07", "01/08", "10/08", "20/08","01/09", "10/09", "20/09",
				"01/10", "15/10", "20/10", "01/11", "10/11", "20/11","01/12", "10/12", "20/12")		

 # etiquetasEjeX = c("01/03", "10/03", "20/03", "01/04", "10/04", "20/04", "01/05", "10/05", "20/05",
				 # "01/06", "15/06", "01/07", "15/07", "01/08", "15/08", "01/09", "15/09", "01/10","15/10")		

colHumectUmbral = "deepskyblue"
colHR80 = "blue"
colHR85 = "orange"
colHR90 = "green"
colDIV =  "orange"
# Hola
#
ccc
# Funciones auxiliares


get_etiquetasEjeX_OLD <- function(longitudX, add31Dic = TRUE, diarios = FALSE){
	if(diarios) longitudX = longitudX*48
	if(longitudX > 62*48){
		etiquetasEjeX = c("1/01", "15/01", "1/02","15/02","1/03", "15/03", # Las que queremos pintar
							 "1/04", "15/04", "1/05", "15/05", "1/06", "15/06", 
							"1/07","15/07","1/08","15/08", "1/09", "15/09",
							 "1/10","15/10","1/11","15/11", "1/12","15/12")
	}else if(longitudX > 31*48){
		etiquetasEjeX = c("1/01", "10/01", "20/01", "1/02", "10/02", "20/02","1/03", "10/03", "20/03",
						"01/04", "10/04", "20/04", "1/05", "10/05", "20/05","01/06", "10/06", "20/06",
						"01/07", "10/07", "20/07", "1/08", "10/08", "20/08","01/09", "10/09", "20/09",
						"01/10", "10/10", "20/10", "1/11", "10/11", "20/11","01/12", "10/12", "20/12")	
	}else{
		etiquetasEjeX = c("1/01", "7/01", "15/01", "22/01", "1/02", "7/02", "15/02", "22/02",
						"1/03", "7/03", "15/03", "22/03", "1/04", "7/04", "15/04", "22/04",
						"1/05","7/05", "15/05", "22/05", "01/06", "7/06", "15/06", "22/06",
						"1/07", "7/07", "15/07", "22/07", "1/08", "7/08", "15/08", "22/08",
						"1/09", "7/09", "15/09", "22/09", "1/10", "7/10", "15/10", "22/10",
						"1/11", "7/11", "15/11", "22/11", "1/12", "7/12", "15/12", "22/12")
	}
	if(add31Dic) etiquetasEjeX = c(etiquetasEjeX, "31/12")
	
	return(etiquetasEjeX)
}

get_etiquetasEjeX <- function(longitudX, add31Dic = TRUE, diarios = FALSE, hacerMartes = TRUE){
	if(diarios) longitudX = longitudX*48
	if(longitudX > 62*48){
		etiquetasEjeX = c("1/01", "15/01", "1/02","15/02","1/03", "15/03", # Las que queremos pintar
							 "1/04", "15/04", "1/05", "15/05", "1/06", "15/06", 
							"1/07","15/07","1/08","15/08", "1/09", "15/09",
							 "1/10","15/10","1/11","15/11", "1/12","15/12")
	}else if(longitudX > 31*48){
		etiquetasEjeX = c("1/01", "10/01", "20/01", "1/02", "10/02", "20/02","1/03", "10/03", "20/03",
						"01/04", "10/04", "20/04", "1/05", "10/05", "20/05","01/06", "10/06", "20/06",
						"01/07", "10/07", "20/07", "1/08", "10/08", "20/08","01/09", "10/09", "20/09",
						"01/10", "10/10", "20/10", "1/11", "10/11", "20/11","01/12", "10/12", "20/12")	
	}else{
		unoEnero = paste0(strftime(Sys.Date(), '%Y'), '-01-01')
		nochevieja = paste0(strftime(Sys.Date(), '%Y'), '-12-31')
		X <- seq.Date(as.Date(unoEnero), as.Date(nochevieja), by = 7)
		
		etiquetasEjeX = strftime(X, '%d/%m')
					# c("1/01", "7/01", "15/01", "22/01", "1/02", "7/02", "15/02", "22/02",
						# "1/03", "7/03", "15/03", "22/03", "1/04", "7/04", "15/04", "22/04",
						# "1/05","7/05", "15/05", "22/05", "01/06", "7/06", "15/06", "22/06",
						# "1/07", "7/07", "15/07", "22/07", "1/08", "7/08", "15/08", "22/08",
						# "1/09", "7/09", "15/09", "22/09", "1/10", "7/10", "15/10", "22/10",
						# "1/11", "7/11", "15/11", "22/11", "1/12", "7/12", "15/12", "22/12")
	}

	if(hacerMartes)
	{
		unoEnero = paste0(strftime(Sys.Date(), '%Y'), '-01-01')
		nochevieja = paste0(strftime(Sys.Date(), '%Y'), '-12-31')
		X <- seq.Date(as.Date(unoEnero), as.Date(nochevieja), by = 1)
		
		X = X[strftime(X, '%u') == 2]
		etiquetasEjeX = strftime(X, '%d/%m')
	}
		
	
	if(add31Dic) etiquetasEjeX = c(etiquetasEjeX, "31/12")
	
	return(etiquetasEjeX)
}

# #Tests
# get_etiquetasEjeX(63*48)
# get_etiquetasEjeX(63*48, FALSE)
# get_etiquetasEjeX(32*48)
# get_etiquetasEjeX(0)
# get_etiquetasEjeX(33, diarios = TRUE)



generaMarcasEjeX <- function(listadoFechas, etiquetasEjeX, marcasDiarias = TRUE){
  
  listadoFechas <- as.Date(listadoFechas)
  primeraFecha <- listadoFechas[1]
  ultimaFecha <- last(listadoFechas)
  
	rangoFechasEje <- c() # Esto pieza de código permite generar las marcas en gráficos plurianuales
	anioInicio = as.integer(strftime(primeraFecha, "%Y"))
	anioFin = as.integer(strftime(ultimaFecha, "%Y"))
	for(anio in anioInicio:anioFin)
	{
		rangoFechasEjeX <-  c(rangoFechasEje, as.Date(paste(etiquetasEjeX, anio, sep = "/"), format = "%d/%m/%Y"))
	}
	
  
  # Ahora creamos los vectores que pintarán el eje X
  posicionMarcasEjeX <- c()
  for(fecha in rangoFechasEjeX){
     posicionMarcasEjeX <- c(posicionMarcasEjeX,
                            which(listadoFechas == as.Date(fecha, origin = "1970-01-01"))[1]) # cada dia tiene 24 valores, cogemos el primero, por eso el [1]
  }
  # Si la fecha real en la que empieza la serie es mayor o menor que el la primera etiqueta...
  if(primeraFecha < rangoFechasEjeX[1]){ #as.Date(rangoFechasEjeX[1], origin = "1970-01-01")){ # Si es menor añadir esta fecha al principio...
    etiquetasEjeX = c(strftime(primeraFecha, format = "%d/%m"), etiquetasEjeX) # de las etiquetas
    posicionMarcasEjeX = c(1, posicionMarcasEjeX) #		y al de las marcas
  }else{ # Si es mayor insertarla y borrar las fechas anteriores
    posicionInicial = length(which(rangoFechasEjeX[1] < primeraFecha))
    etiquetasEjeX[posicionInicial] = strftime(primeraFecha, format = "%d/%m")
    etiquetasEjeX = etiquetasEjeX[posicionInicial:length(etiquetasEjeX)]
    posicionMarcasEjeX[posicionInicial] = 1
    posicionMarcasEjeX = posicionMarcasEjeX[posicionInicial:length(posicionMarcasEjeX)]
  }
  
	if(ultimaFecha > last(rangoFechasEjeX)){
	}else{
		posicionFinal = length(which(as.Date(rangoFechasEjeX, origin = "1970-01-01") < ultimaFecha)) + 1
		etiquetasEjeX = etiquetasEjeX[1:posicionFinal]
		etiquetasEjeX[posicionFinal] = strftime(ultimaFecha, format = "%d/%m")
		posicionMarcasEjeX = posicionMarcasEjeX[1:posicionFinal]
		posicionMarcasEjeX[posicionFinal] = length(listadoFechas)
	}
  
  if(marcasDiarias){
	posicionMarcasEjeX2 <- c()
	for(fecha in seq.Date(primeraFecha, listadoFechas[length(listadoFechas)], "day")){
		posicionMarcasEjeX2 <- c(posicionMarcasEjeX2,
                            which(listadoFechas == fecha)[1]) # cada dia tiene 24 valores, cogemos el primero, por eso el [1]
	}
	etiquetasEjeX2 <- as.character(rep(NA, length(posicionMarcasEjeX2)))
	#etiquetasEjeX2[posicionMarcasEjeX] <- etiquetasEjeX # Esto debería funcionar pero no lo hace ¿?
	
	for(pos in 1:length(etiquetasEjeX)){
		etiquetasEjeX2[posicionMarcasEjeX[pos]] <- etiquetasEjeX[pos]
	}
	
	etiquetasEjeX <- etiquetasEjeX2
	posicionMarcasEjeX <- posicionMarcasEjeX2
  }
  
  return(list(posicionMarcasEjeX = posicionMarcasEjeX, etiquetasEjeX = etiquetasEjeX))
}

# TEST
#listadoFechas2 <- seq.POSIXt(as.POSIXct("2018-05-24"), as.POSIXct("2018-09-24"), by = "days")
#etiquetasEjeX2 = c("01/06", "15/06", "01/07","15/07","01/08","15/08","01/09","15/09","01/10","15/10") # Las que queremos pintar
#posicionMarcasEjeX2 <- generaMarcasEjeX(listadoFechas2, etiquetasEjeX)$posicionMarcasEjeX
#etiquetasEjeX3 <-  generaMarcasEjeX(listadoFechas2, etiquetasEjeX2)$etiquetasEjeX
# Uso dentro de función plot(..., xaxt = "n")  # no pinta eje X
# axis(side = 1, at = x[posicionMarcasEjeX], labels = etiquetasEjeX, tick = TRUE, pos = 0)
		

			
# Funciones Principales

dibuja_GrafRiesgoAcumulado <- function(fecha, DIV.2dias, DIV.7dias, DIV.14dias, DIV.21dias,
										DIV.28dias = NA, dibujaDIV.28dias = FALSE,
										ylim = c(0,10), dibujaEjeX = TRUE, ylab = '', dibujaLineasDia = TRUE){
	plot(fecha, DIV.2dias, ylim = c(0,10), col = "gold", type = "l", ylab = ylab, xaxt = 'n')
	posicion = length(fecha) - 48
	offsetTextoY = 0.2
	text(fecha[posicion], DIV.2dias[posicion] + offsetTextoY, labels = '2 días', col = "gold")
	lines(fecha, DIV.7dias, col = "green", type = "l")
	text(fecha[posicion], DIV.7dias[posicion] + offsetTextoY, labels = '7 días', col = "green")
	lines(fecha, DIV.14dias, col = "red", type = "l")
	text(fecha[posicion] + offsetTextoY, DIV.14dias[posicion], labels = '14 días', col = "red")
	lines(fecha, DIV.21dias, col = "blue", type = "l")
	text(fecha[posicion], DIV.21dias[posicion] + offsetTextoY, labels = '21 días', col = "blue")
	if(dibujaDIV.28dias)
	{
		lines(fecha, DIV_HumectUmbral.28dias, col = "black", type = "l")
		text(fecha[posicion], DIV.28dias[posicion] + offsetTextoY, labels = '28 días', col = "black")
	}
	
	
	if(dibujaEjeX)
	{
		fechasEjeX <-  as.POSIXct(paste(get_etiquetasEjeX(length(fecha)), strftime(fecha[1], "%Y"), sep = "/"), format = "%d/%m/%Y")
		etiquetasEjeX <- get_etiquetasEjeX(length(fecha))
  
		posFechas <- axis(side = 1, at = fechasEjeX, labels = etiquetasEjeX, tick = TRUE) #, pos = 0)
		if(dibujaLineasDia)
		{
			abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
		}
	}
	
	abline(h = 1, lty = "dotted", col = 'gray')
	abline(h = 3, lty = "dotted", col = 'gray')
	abline(h = 5, lty = "dotted", col = 'red')
	abline(h = 7, lty = "dotted", col = 'red')
}


# GRÁFICO DE DIV
pintaGraficaRiesgoDIV_HEATMAP_V12 <- function(resultado, etiquetasEjeX, # etiquetasEjeX, las fechas que queremos pintar)
											nombresTratamientos = NA, tituloGrafico = '',
											opcion = 'opcion1', cexTextosRiesgo = 0.8, ...)
{ 
	
	numeroResultados = (ncol(resultado) - 2) / 2

	tamanioExtra = if(numeroResultados < 3) 2 else 4
	
	ylim = c(0, 2*numeroResultados + tamanioExtra) # + tamanioExtra porque en la barra superior se pinta la leyenda del gráfico
	
	x = as.POSIXct(resultado$fecha)	
	x = c(x, last(x)+24*3600)
	
	plot(x, rep(NA,length(x)), type = "n", ylim = ylim, # ylim = c(0, 14),
			xlab = "", ylab = "", axes = FALSE)
	
	if(numeroResultados < 2)
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.25)
	}else if(numeroResultados < 3)
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.325)
	}else
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.5)
	}

	
	#Eje X
	generaEjeX <- function(x, etiquetasEjeX, pos = 0, color = 'black', tcl = -0.5,
							primeraFechaTck = TRUE, primeraFechaLbl = TRUE, marcasDiarias = TRUE, pintalineasVerticales = FALSE)
	{
		posicionMarcasEjeX <- generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$posicionMarcasEjeX
		etiquetasEjeX <-  generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$etiquetasEjeX
		
		atEjeX = if(primeraFechaTck) x[posicionMarcasEjeX] else x[posicionMarcasEjeX][-1]
		labelsEjeX = if(primeraFechaLbl) etiquetasEjeX else etiquetasEjeX[-1]

		if(length(atEjeX) != length(labelsEjeX)) labelsEjeX = c(NA, labelsEjeX)

		if(tcl == 0) labelsEjeX = rep(NA, length(atEjeX))

		posFechas <- axis(side = 1, at = atEjeX,
									labels = labelsEjeX,
							tick = TRUE, pos = pos, tcl = tcl,
							col = color, col.ticks = color,	col.axis = color)
		if(pintalineasVerticales) abline(v = posFechas, col = color, lty = "dotted", lwd = 0.5)
	}
	
	generaEjeX(x, etiquetasEjeX, pos = 0, tcl = -1, marcasDiarias = FALSE, primeraFechaTck = FALSE, primeraFechaLbl = FALSE) # Marcas grandes con fechas concretas
	generaEjeX(x, etiquetasEjeX, pos = 0, marcasDiarias = TRUE, primeraFechaTck = TRUE, primeraFechaLbl = FALSE) # Marcas pequeñas para cada día

	#abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
	
	#Eje Y # PINTABA PERO LOS RECTÁNGULOS DIBUJADOS DESPUÉS IMPEDIAN VERLO, si se quiere pintar el eje Y hay que hacerlo al final
		#axis(side = 2, at = c(2, 4, 6, 8),
		#		labels = c(NA, NA, NA, NA),
		#		las = 1, cex = 0.1, pos = x[1])
			
	
	
	par(xpd = NA)
	
	colorRiesgoDIV <- function(DIV){
	
		colDIV.AusenciaRiesgo = 'white'
		colDIV.SinRiesgo = "khaki" #grey(1) 
		colDIV1.InicioRiesgo = "greenyellow"  #"yellow3" #
		colDIV.RiesgoLeve = "yellowgreen" #"limegreen"# 
		colDIV.RiesgoMedio = "orange"
		colDIV.RiesgoModerado = "sienna1" #"tomato1" #
		colDIV.RiesgoAlto =  "red1"
		colDIV.RiesgoMuyAlto =  "red3"
		colDIV.InfeccionSegura = "purple"
		
		
		if(opcion == 'opcion1'){
			# Intervalos de riesgo DIV:
			# > 8 infección segura
			# < 2 riesgo muy bajo
			# 2-4 riesgo leve
			# 4-5 riesgo medio
			# 5-6 riesgo moderado
			# 6-7 riesgo alto
			# 7-8 riesgo muy alto	
			vectorColor = ifelse(DIV < 1, colDIV.SinRiesgo, NA)
			#vectorColor = ifelse(DIV >= 0.5 & DIV < 1, colDIV.SinRiesgo, vectorColor)
			vectorColor = ifelse(DIV >= 1 & DIV < 2, colDIV1.InicioRiesgo, vectorColor)
			vectorColor = ifelse(DIV >= 2 & DIV < 3, colDIV.RiesgoLeve, vectorColor)
			vectorColor = ifelse(DIV >= 3 & DIV < 4, colDIV.RiesgoMedio, vectorColor)
			vectorColor = ifelse(DIV >= 4 & DIV < 5, colDIV.RiesgoModerado, vectorColor)
			vectorColor = ifelse(DIV >= 5 & DIV < 6, colDIV.RiesgoAlto, vectorColor)
			vectorColor = ifelse(DIV >= 6 & DIV < 7, colDIV.RiesgoMuyAlto, vectorColor)
			vectorColor = ifelse(DIV >= 7, colDIV.InfeccionSegura, vectorColor)
			
			intervalosRiesgo = c(0, 1, 2, 3, 4, 5, 6, 7)
			labelsRiesgo = c("DIV_Ac < 1", "DIV_Ac < 2", "DIV_Ac < 3", "DIV_Ac < 4",
							"DIV_Ac < 5", "DIV_Ac < 6", "DIV_Ac < 7", "DIV_Ac >= 7")
			labelsRiesgo = c("DIV < 1", "<= DIV < 2", "<= DIV < 3", "<= DIV < 4",
							"<= DIV < 5", "<= DIV < 6", "<= DIV < 7", "<= DIV")
			
		}else{
			# Intervalos de riesgo DIV:
			# > 12 infección segura
			# < 2 riesgo muy bajo
			# 2-4 riesgo leve
			# 4-6 riesgo medio
			# 6-8 riesgo moderado
			# 8-10 riesgo alto
			# 10-12 riesgo muy alto
			vectorColor = ifelse(DIV < 2, colDIV.SinRiesgo, NA)
			vectorColor = ifelse(DIV >= 2 & DIV < 4, colDIV.RiesgoLeve, vectorColor)
			vectorColor = ifelse(DIV >= 4 & DIV < 6, colDIV.RiesgoMedio, vectorColor)
			vectorColor = ifelse(DIV >= 6 & DIV < 8, colDIV.RiesgoModerado, vectorColor)
			vectorColor = ifelse(DIV >= 8 & DIV < 10, colDIV.RiesgoAlto, vectorColor)
			vectorColor = ifelse(DIV >= 10 & DIV < 12, colDIV.RiesgoMuyAlto, vectorColor)
			vectorColor = ifelse(DIV >= 12, colDIV.InfeccionSegura, vectorColor)
			
			intervalosRiesgo = c(0, 2, 4, 6, 8, 10, 12)
			labelsRiesgo = c("DIV_Ac < 2", "2 <= DIV_Ac < 4", "4 <= DIV_Ac < 6",
							"6 <= DIV_Ac < 8", "8 <= DIV_Ac < 10", "10 <= DIV_Ac < 12", "DIV_Ac >= 12")
			
		}
		
		return(list(vectorColor = vectorColor, intervalosRiesgo = intervalosRiesgo, labelsRiesgo = labelsRiesgo))
		
		# return(unlist(sapply(DIV + 1, function(x) {switch(x,
										# colDIV0, colDIV1, colDIV2,
										# colDIV3, colDIV4, colDIV5,
										# colDIV6, colDIV7)})))
	}
		# #TEST
		#colorRiesgoDIV(c(2,4,6,8,10,12,14,16,18,20,22,30))
	
	# Pintar leyenda:
	
	# Dividir la escala de tiempo en el número de intervalos de riesgo
	intervalosRiesgo = colorRiesgoDIV(1:2)$intervalosRiesgo
	intervalosRiesgoFin = intervalosRiesgo + 1
	#timeSpan <- as.integer(difftime(x[length(x)], x[1], units = "secs") / length(intervalosRiesgo))
	timeSpan <- as.integer(difftime(x[length(x)], x[1], units = "secs") / max(intervalosRiesgoFin))
	
	i = 1:length(intervalosRiesgo)
	rect(x[1] + timeSpan * intervalosRiesgo, # + timeSpan*(i-1),
			2 * numeroResultados + tamanioExtra - 1.5, #2.5,
			x[1] + timeSpan * intervalosRiesgoFin, # + timeSpan*i
			2*numeroResultados + tamanioExtra - 0.5, #3.5,
			col = colorRiesgoDIV(intervalosRiesgo)$vectorColor, border = NA)
		
	text(x[1] + timeSpan * intervalosRiesgo,
		2 * numeroResultados + tamanioExtra - 1, #3,
		labels = intervalosRiesgo,
		cex = 1.5, adj = 0.5)	

	text(x[1] - timeSpan,
		2 * numeroResultados + tamanioExtra - 1, #3,
		labels = 'DIV = ',
		cex = 1.5, adj = 0.1, xpd = TRUE)
	
	if(tituloGrafico != '')
	{
		title(main = tituloGrafico, line = -5)	#main = paste(tituloGrafico, strftime(resultado$DIV$fecha[1], "%Y"))
		#title(main = tituloGraf, cex.main=0.9, line = 1)
	}
	
	# Acotaciones
	text(x[1] + timeSpan * c(0, 3, 5, 7.97), 2 * numeroResultados + 1.8,
		labels = '|', 
		cex = 1.5, adj = 0)
	
	# #Acotaciones inferiores
	#text(x[1] + timeSpan * c(0, 3, 5), 2 * numeroResultados + 1.8,
	#	labels = '|', #'<-', cex = 0.6
	#	cex = 1, adj = 0)
	
	# #Acotaciones Superiores
	#text(x[1] + timeSpan * c(3, 5, 7), 2 * numeroResultados + 1.8,
	#	labels = '|', #'->', cex = 0.6
	#	cex = 1, adj = 1)
	
	
	text(x[1] + timeSpan * 1.5, 2 * numeroResultados + 1.8,
		labels = 'Riesgo Bajo 0-3',
		cex = cexTextosRiesgo, adj = 0.5)			
	
	text(x[1] + timeSpan * 4, 2 * numeroResultados + 1.8,
		labels = 'Riesgo Medio 3-5',
		cex = cexTextosRiesgo*0.75/0.8, adj = 0.5)	#!!! HA CAMBIADO CEX		
	
	text(x[1] + timeSpan * 6.5, 2 * numeroResultados + 1.8,
		labels = 'Riesgo Alto (DIV > 5)',
		cex = cexTextosRiesgo, adj = 0.5)			
	
	
				# str(resultado)
	#      :'data.frame':        24 obs. of  8 variables:
  # ..$ fecha              : POSIXct[1:24], format: "2020-03-02 10:00:00" "2020-03-03 10:00:00" ...
  # ..$ estacion           : num [1:24] 2712 2712 2712 2712 2712 ...
  # ..$ DIV_HR85           : num [1:24] 0 3.43 3.23 4.29 3.14 ...
  # ..$ DIV_HR85Acum       : num [1:24] 0 3.43 6.66 7.52 7.43 ...
  # ..$ DIV_HR90           : num [1:24] 0 2.57 2.77 3.43 1.71 ...
  # ..$ DIV_HR90Acum       : num [1:24] 0 2.57 5.34 6.2 5.14 ...
  # ..$ DIV_HumectUmbral   : num [1:24] 0 0 2.31 6.86 4 ...
  # ..$ DIV_HumectUmbraAcum: num [1:24] 0 0 2.31 9.16 10.86 ...
 
 
 	x <- x[-length(x)]
	
	for(modelo in 1:numeroResultados)
	{
		rect(x, 0.1 + 2 * (modelo-1), x + 3600 * 24 - 1, 2*modelo,
				col = colorRiesgoDIV(resultado[,2 + 2*modelo])$vectorColor,
				border = NA)
		if(nombresTratamientos[modelo] == 'HR85*')
		{
			rect(x[1], 0.1 + 2 * (modelo-1), last(x) + 3600 * 24 - 1, 2*modelo,
				col = rgb(1, 1, 1, alpha = 0),
				border ='black', lwd = 1)
			mtext(text = "* el modelo calculado usando el umbral HR >= 85% es el empleado para la toma de decisiones de tratamiento",
				col = 'red', side = 1, cex = 0.6, line = 1, adj = 0.5,  xpd = TRUE)
		}
		if(is.na(nombresTratamientos))
		{
			text(x= x[1], y = 1 + 2*(modelo-1), labels = names(resultado)[2 + 2*modelo], cex = 1, adj = 0)
		}else{
			text(x= x[1], y = 1 + 2*(modelo-1), labels = nombresTratamientos[modelo],
			col = if(nombresTratamientos[modelo] == 'HR85*') 'red' else 'black',
			cex = 1, adj = 0, )
		}
	}
	
				
}

# pintaGraficaRiesgoDIV_HEATMAP_V12(resultado, etiquetasEjeX = etiquetasEjeX, nombresTratamientos = c('Humectacion', 'HR85', 'HR90'))
# pintaGraficaRiesgoDIV_HEATMAP_V12(resultado, etiquetasEjeX = etiquetasEjeX)
# pintaGraficaRiesgoDIV_HEATMAP_V12(resultado, etiquetasEjeX = etiquetasEjeX, opcion = 'opcion2')



pintaGraficaRiesgoDIV_HEATMAP_V12_Simplificado <- function(resultado, etiquetasEjeX, # etiquetasEjeX, las fechas que queremos pintar)
											nombresTratamientos = NA, tituloGrafico = '',
											opcion = 'opcion1', cexTextosRiesgo = 1.2, ...)
{ 
	
	numeroResultados = (ncol(resultado) - 2) / 2

	tamanioExtra = if(numeroResultados < 3) 1.5 else 4
	
	ylim = c(0, 2*numeroResultados + tamanioExtra) # + tamanioExtra porque en la barra superior se pinta la leyenda del gráfico
	
	x = as.POSIXct(resultado$fecha)	
	x = c(x, last(x)+24*3600)
	
	plot(x, rep(NA,length(x)), type = "n", ylim = ylim, # ylim = c(0, 14),
			xlab = "", ylab = "", axes = FALSE)
	
	if(numeroResultados < 2)
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.25)
	}else if(numeroResultados < 3)
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.325)
	}else
	{
		mtext(text = "DIV-Acumulado", side = 2, cex = 0.8, line = 1, adj = 0.5)
	}

	
	#Eje X
	generaEjeX <- function(x, etiquetasEjeX, pos = 0, color = 'black', tcl = -0.5,
							primeraFechaTck = TRUE, primeraFechaLbl = TRUE, marcasDiarias = TRUE, pintaFechasEnEjeX = TRUE, pintalineasVerticales = FALSE)
	{
		posicionMarcasEjeX <- generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$posicionMarcasEjeX
		etiquetasEjeX <-  generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$etiquetasEjeX
		
		atEjeX = if(primeraFechaTck) x[posicionMarcasEjeX] else x[posicionMarcasEjeX][-1]
		if(pintaFechasEnEjeX && tcl != 0)
		{
			labelsEjeX = if(primeraFechaLbl) etiquetasEjeX else etiquetasEjeX[-1]
			if(length(atEjeX) != length(labelsEjeX)) labelsEjeX = c(NA, labelsEjeX)
		}else
		{
			labelsEjeX = rep(NA, length(atEjeX))
		}

		posFechas <- axis(side = 1, at = atEjeX,
									labels = labelsEjeX,
							tick = TRUE, pos = pos, tcl = tcl,
							col = color, col.ticks = color,	col.axis = color)
		if(pintalineasVerticales) abline(v = posFechas, col = color, lty = "dotted", lwd = 0.5)
	}
	
	generaEjeX(x, etiquetasEjeX, pos = 0, tcl = -1, marcasDiarias = FALSE, primeraFechaTck = FALSE, primeraFechaLbl = FALSE) # Marcas grandes con fechas concretas
	generaEjeX(x, etiquetasEjeX, pos = 0, marcasDiarias = TRUE, primeraFechaTck = TRUE, primeraFechaLbl = FALSE, pintaFechasEnEjeX = FALSE) # Marcas pequeñas para cada día

	
	#Eje Y # PINTABA PERO LOS RECTÁNGULOS DIBUJADOS DESPUÉS IMPEDIAN VERLO, si se quiere pintar el eje Y hay que hacerlo al final
		#axis(side = 2, at = c(2, 4, 6, 8),
		#		labels = c(NA, NA, NA, NA),
		#		las = 1, cex = 0.1, pos = x[1])
			
	#par(xpd = NA)
	
	colorRiesgoDIV <- function(DIV){
	
		colDIV.AusenciaRiesgo = 'white'
		colDIV.SinRiesgo = "khaki" #grey(1) 
		colDIV1.InicioRiesgo = "greenyellow"  #"yellow3" #
		colDIV.RiesgoLeve = "yellowgreen" #"limegreen"# 
		colDIV.RiesgoMedio = "orange"
		colDIV.RiesgoModerado = "sienna1" #"tomato1" #
		colDIV.RiesgoAlto =  "red1"
		colDIV.RiesgoMuyAlto =  "red3"
		colDIV.InfeccionSegura = "purple"
		
		
		if(opcion == 'opcion1'){
			# Intervalos de riesgo DIV:
			# > 8 infección segura
			# < 2 riesgo muy bajo
			# 2-4 riesgo leve
			# 4-5 riesgo medio
			# 5-6 riesgo moderado
			# 6-7 riesgo alto
			# 7-8 riesgo muy alto	
			vectorColor = ifelse(DIV < 1, colDIV.SinRiesgo, NA)
			#vectorColor = ifelse(DIV >= 0.5 & DIV < 1, colDIV.SinRiesgo, vectorColor)
			vectorColor = ifelse(DIV >= 1 & DIV < 2, colDIV1.InicioRiesgo, vectorColor)
			vectorColor = ifelse(DIV >= 2 & DIV < 3, colDIV.RiesgoLeve, vectorColor)
			vectorColor = ifelse(DIV >= 3 & DIV < 4, colDIV.RiesgoMedio, vectorColor)
			vectorColor = ifelse(DIV >= 4 & DIV < 5, colDIV.RiesgoModerado, vectorColor)
			vectorColor = ifelse(DIV >= 5 & DIV < 6, colDIV.RiesgoAlto, vectorColor)
			vectorColor = ifelse(DIV >= 6 & DIV < 7, colDIV.RiesgoMuyAlto, vectorColor)
			vectorColor = ifelse(DIV >= 7, colDIV.InfeccionSegura, vectorColor)
			
			intervalosRiesgo = c(0, 1, 2, 3, 4, 5, 6, 7)
			labelsRiesgo = c("DIV_Ac < 1", "DIV_Ac < 2", "DIV_Ac < 3", "DIV_Ac < 4",
							"DIV_Ac < 5", "DIV_Ac < 6", "DIV_Ac < 7", "DIV_Ac >= 7")
			labelsRiesgo = c("DIV < 1", "<= DIV < 2", "<= DIV < 3", "<= DIV < 4",
							"<= DIV < 5", "<= DIV < 6", "<= DIV < 7", "<= DIV")
			
		}else{
			# Intervalos de riesgo DIV:
			# > 12 infección segura
			# < 2 riesgo muy bajo
			# 2-4 riesgo leve
			# 4-6 riesgo medio
			# 6-8 riesgo moderado
			# 8-10 riesgo alto
			# 10-12 riesgo muy alto
			vectorColor = ifelse(DIV < 2, colDIV.SinRiesgo, NA)
			vectorColor = ifelse(DIV >= 2 & DIV < 4, colDIV.RiesgoLeve, vectorColor)
			vectorColor = ifelse(DIV >= 4 & DIV < 6, colDIV.RiesgoMedio, vectorColor)
			vectorColor = ifelse(DIV >= 6 & DIV < 8, colDIV.RiesgoModerado, vectorColor)
			vectorColor = ifelse(DIV >= 8 & DIV < 10, colDIV.RiesgoAlto, vectorColor)
			vectorColor = ifelse(DIV >= 10 & DIV < 12, colDIV.RiesgoMuyAlto, vectorColor)
			vectorColor = ifelse(DIV >= 12, colDIV.InfeccionSegura, vectorColor)
			
			intervalosRiesgo = c(0, 2, 4, 6, 8, 10, 12)
			labelsRiesgo = c("DIV_Ac < 2", "2 <= DIV_Ac < 4", "4 <= DIV_Ac < 6",
							"6 <= DIV_Ac < 8", "8 <= DIV_Ac < 10", "10 <= DIV_Ac < 12", "DIV_Ac >= 12")
			
		}
		
		return(list(vectorColor = vectorColor, intervalosRiesgo = intervalosRiesgo, labelsRiesgo = labelsRiesgo))
		
		# return(unlist(sapply(DIV + 1, function(x) {switch(x,
										# colDIV0, colDIV1, colDIV2,
										# colDIV3, colDIV4, colDIV5,
										# colDIV6, colDIV7)})))
	}
		# #TEST
		#colorRiesgoDIV(c(2,4,6,8,10,12,14,16,18,20,22,30))
	
	# Pintar leyenda:
	
	# Dividir la escala de tiempo en el número de intervalos de riesgo
	intervalosRiesgo = colorRiesgoDIV(1:2)$intervalosRiesgo
	intervalosRiesgoFin = intervalosRiesgo + 1
	#timeSpan <- as.integer(difftime(x[length(x)], x[1], units = "secs") / length(intervalosRiesgo))
	timeSpan <- as.integer(difftime(x[length(x)], x[1], units = "secs") / max(intervalosRiesgoFin))
	
	i = 1:length(intervalosRiesgo)
	rect(x[1] + timeSpan * intervalosRiesgo, # + timeSpan*(i-1),
			2 * numeroResultados + tamanioExtra - 1, #2.5,
			x[1] + timeSpan * intervalosRiesgoFin, # + timeSpan*i
			2*numeroResultados + tamanioExtra - 0.5, #3.5,
			col = colorRiesgoDIV(intervalosRiesgo)$vectorColor, border = NA)
		
	text(x[1] + timeSpan * intervalosRiesgo,
		2 * numeroResultados + tamanioExtra - 0.75, #3,
		labels = intervalosRiesgo,
		cex = 1.5, adj = 0.5)	

	text(x[1] - timeSpan,
		2 * numeroResultados + tamanioExtra - 0.75, #3,
		labels = 'DIV = ',
		cex = 1.5, adj = 0.1, xpd = TRUE)
	
	if(tituloGrafico != '')
	{
		title(main = tituloGrafico, line = -6)	#main = paste(tituloGrafico, strftime(resultado$DIV$fecha[1], "%Y"))
		#title(main = tituloGraf, cex.main=0.9, line = 1)
	}
	
	# Acotaciones
	text(x[1] + timeSpan * c(0, 3, 5, 7.97), 2 * numeroResultados + tamanioExtra,
		labels = '|', 
		cex = 1.5, adj = 0)
	
	# #Acotaciones inferiores
	#text(x[1] + timeSpan * c(0, 3, 5), 2 * numeroResultados + 1.8,
	#	labels = '|', #'<-', cex = 0.6
	#	cex = 1, adj = 0)
	
	# #Acotaciones Superiores
	#text(x[1] + timeSpan * c(3, 5, 7), 2 * numeroResultados + 1.8,
	#	labels = '|', #'->', cex = 0.6
	#	cex = 1, adj = 1)
	
	
	text(x[1] + timeSpan * 1.5, 2 * numeroResultados + tamanioExtra,
		labels = 'Riesgo Bajo 0-3',
		cex = cexTextosRiesgo, adj = 0.5)			
	
	text(x[1] + timeSpan * 4, 2 * numeroResultados + tamanioExtra,
		labels = 'Riesgo Medio 3-5',
		cex = cexTextosRiesgo*0.75/0.8, adj = 0.5)	#!!! HA CAMBIADO CEX		
	
	text(x[1] + timeSpan * 6.5, 2 * numeroResultados + tamanioExtra,
		labels = 'Riesgo Alto (DIV > 5)',
		cex = cexTextosRiesgo, adj = 0.5)			
	
	
				# str(resultado)
	#      :'data.frame':        24 obs. of  8 variables:
  # ..$ fecha              : POSIXct[1:24], format: "2020-03-02 10:00:00" "2020-03-03 10:00:00" ...
  # ..$ estacion           : num [1:24] 2712 2712 2712 2712 2712 ...
  # ..$ DIV_HR85           : num [1:24] 0 3.43 3.23 4.29 3.14 ...
  # ..$ DIV_HR85Acum       : num [1:24] 0 3.43 6.66 7.52 7.43 ...
  # ..$ DIV_HR90           : num [1:24] 0 2.57 2.77 3.43 1.71 ...
  # ..$ DIV_HR90Acum       : num [1:24] 0 2.57 5.34 6.2 5.14 ...
  # ..$ DIV_HumectUmbral   : num [1:24] 0 0 2.31 6.86 4 ...
  # ..$ DIV_HumectUmbraAcum: num [1:24] 0 0 2.31 9.16 10.86 ...
 
 
 	x <- x[-length(x)]
	
	for(modelo in 1:numeroResultados)
	{
		rect(x, 0.1 + 2 * (modelo-1), x + 3600 * 24 - 1, 2*modelo,
				col = colorRiesgoDIV(resultado[,2 + 2*modelo])$vectorColor,
				border = NA)
		if(nombresTratamientos[modelo] == 'HR85*')
		{
			rect(x[1], 0.1 + 2 * (modelo-1), last(x) + 3600 * 24 - 1, 2*modelo,
				col = rgb(1, 1, 1, alpha = 0),
				border ='black', lwd = 1)
			#mtext(text = "* el modelo calculado usando el umbral HR >= 85% es el empleado para la toma de decisiones de tratamiento",
			#	col = 'red', side = 1, cex = 0.6, line = 1, adj = 0.5,  xpd = TRUE)
		}
		if(is.na(nombresTratamientos))
		{
			text(x= x[1], y = 1 + 2*(modelo-1), labels = names(resultado)[2 + 2*modelo], cex = 1, adj = 0)
		}else{
			text(x= x[1], y = 1 + 2*(modelo-1), labels = nombresTratamientos[modelo],
			col = if(nombresTratamientos[modelo] == 'HR85*') 'red' else 'black',
			cex = 1, adj = 0, )
		}
	}
	
	
	
				
}



dibujaFechasAplicacionTratamientos <- function(resultado, etiquetasEjeX,
												etiquetasEjeXDIV3 = NULL, etiquetasEjeXDIV5 = NULL, etiquetasEjeXAgricultor = NULL,
												textoDIV3, textoDIV5, textoAgricultor)
{
	
	columnaModelo = 4 #!!!OJO
	ylim = c(0, 6) # + tamanioExtra porque en la barra superior se pinta la leyenda del gráfico
	
	x = as.POSIXct(resultado$fecha)	
	x = c(x, last(x)+24*3600)
	
	plot(x, rep(NA,length(x)), type = "n", ylim = ylim, # ylim = c(0, 14),
			xlab = "", ylab = "", axes = FALSE)
	
	
	title(main = 'Fechas aplicación tratamientos fungicidas. ¡Variedades diferentes!', line = 0)
	
	dibujarRiesgos <- function(x,trueOrFalse, y, texto = '', color = 'black', colorRiesgo = 'grey')
	{
		x <- x[-length(x)]
	
		rect(x, y-0.25, x + 3600 * 24 - 1, y + 0.25,
			col = ifelse(trueOrFalse, colorRiesgo, NA), border = NA) #col = ifelse(trueOrFalse, colorRiesgo, 'white'), border = NA)
		text(x= x[1], y = y + 0.5, labels = texto,
			col = color,
			cex = 1, adj = 0, )
	}
	
	#Eje X
	generaEjeX <- function(x, etiquetasEjeX, pos = 0, color = 'black', tcl = -0.5,
							primeraFechaTck = TRUE, primeraFechaLbl = TRUE, marcasDiarias = TRUE, pintalineasVerticales = FALSE)
	{
		posicionMarcasEjeX <- generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$posicionMarcasEjeX
		etiquetasEjeX <-  generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$etiquetasEjeX
		
		atEjeX = if(primeraFechaTck) x[posicionMarcasEjeX] else x[posicionMarcasEjeX][-1]
		labelsEjeX = if(primeraFechaLbl) etiquetasEjeX else etiquetasEjeX[-1]

		if(length(atEjeX) != length(labelsEjeX)) labelsEjeX = c(NA, labelsEjeX)

		if(tcl == 0) labelsEjeX = rep(NA, length(atEjeX))

		posFechas <- axis(side = 1, at = atEjeX,
									labels = labelsEjeX,
							tick = TRUE, pos = pos, tcl = tcl,
							col = color, col.ticks = color,	col.axis = color)
		if(pintalineasVerticales) abline(v = posFechas, col = color, lty = "dotted", lwd = 0.5)
	}
	
	
	
	
	if(!is.null(etiquetasEjeXDIV3))
	{
		posEtiquetasEjeXDIV3 = if(!is.null(etiquetasEjeXDIV3) & is.null(etiquetasEjeXDIV5)) 4 else 5 # Si no se pintan los resultados del DIV5 bajar un poco la línea donde se dibujan los del DIV 3
		
		dibujarRiesgos(x, trueOrFalse = resultado[,columnaModelo] >= 3, y = posEtiquetasEjeXDIV3, texto = textoDIV3,  color = 'skyblue')
		if(length(etiquetasEjeXDIV3)) generaEjeX(x, etiquetasEjeXDIV3, pos = posEtiquetasEjeXDIV3, color = 'skyblue', marcasDiarias = FALSE, primeraFechaTck = FALSE, primeraFechaLbl = FALSE)
		generaEjeX(x, etiquetasEjeX, pos = posEtiquetasEjeXDIV3, color = 'skyblue', marcasDiarias = FALSE, primeraFechaTck = TRUE, primeraFechaLbl = FALSE, tcl = 0)
		
		
		
		#text(x[1], 5, labels = "Tratamientos DIV > 3", cex = 0.8, adj = 1, col = 'skyblue')	
		#abline(h = -0.8, col = 'skyblue', xpd = TRUE)
	}
	if(!is.null(etiquetasEjeXDIV5))
	{
		dibujarRiesgos(x, trueOrFalse = resultado[,columnaModelo] >= 5, y = 3, texto = textoDIV5,  color = 'red')
		if(length(etiquetasEjeXDIV5)) generaEjeX(x, etiquetasEjeXDIV5, pos = 3, color = 'red', marcasDiarias = FALSE, primeraFechaTck = FALSE, primeraFechaLbl = FALSE)
		generaEjeX(x, etiquetasEjeX, pos = 3, color = 'red', marcasDiarias = FALSE, primeraFechaTck = TRUE, primeraFechaLbl = FALSE, tcl = 0)
		
		#text(x[1], 3, labels = "Tratamientos DIV > 5", cex = 0.8, adj = 1, col = 'red')
		#abline(h = -1.6, col = 'red', xpd = TRUE)
	}
	if(!is.null(etiquetasEjeXAgricultor))
	{
		dibujarRiesgos(x, trueOrFalse = FALSE, y = 1, texto = textoAgricultor,  color = 'black')
		if(length(etiquetasEjeXAgricultor)) generaEjeX(x, etiquetasEjeXAgricultor, pos = 1, color = 'black', marcasDiarias = FALSE, primeraFechaTck = FALSE, primeraFechaLbl = FALSE)
		generaEjeX(x, etiquetasEjeX, pos = 1, color = 'black', marcasDiarias = FALSE, primeraFechaTck = TRUE, primeraFechaLbl = FALSE, tcl = 0)
		
		#text(x[1], 1, labels = "Tratamientos Agricultor", cex = 0.8, adj = 1, col = 'black')
		#abline(h = -1.6, col = 'red', xpd = TRUE)
	}
}


# GRÁFICO DE TEMPERATURA
# $ datosClimaticos:'data.frame':        576 obs. of  4 variables:
		# ..$ fecha      : POSIXct[1:576], format: "2020-03-01 11:00:00" "2020-03-01 12:00:00" ...
		# ..$ temperatura: num [1:576] 17.5 17.5 17.5 17.5 17.5 ...
		# ..$ humectacion: num [1:576] 0 0 0 0 0 0 0 0 0 0 ...
		# ..$ HR         : num [1:576] 62.2 62 61.8 61.8 61.9 ...

# umbralHumectacion: es el valor a partir del cual se acumulan minutos de humectación en el modelo
# en el caso de resistencias de humectación se suele considerar 5 minutos, umbralHumectacion = 5,
# en los modelos de HR se impone un umbral de HR, umbralHumectacion = 85
	
pintaGraficaTemperatura <- function(resultado, etiquetasEjeX, tipoGrafico, umbralHumectacion, pintarEjeX, pintarLineaDIV){

	if(exists('DEBUG')){
		if(DEBUG){
			tipoGrafico = 'Humectacion'
			umbralHumectacion = 5
			pintarEjeX = TRUE
			pintarLineaDIV = TRUE
		}	
	}
		
	datosClimaticos = resultado$datosClimaticos
	x = datosClimaticos$fecha
	temperaturas = datosClimaticos$temperatura
	
	posicionMarcasEjeX <- generaMarcasEjeX(x, etiquetasEjeX, FALSE)$posicionMarcasEjeX
	etiquetasEjeX <-  generaMarcasEjeX(x, etiquetasEjeX, FALSE)$etiquetasEjeX
	
	if(tipoGrafico == 'Humectacion'){
		valoresHumectacion = datosClimaticos$humectacion
		DIV_Horario = resultado$DIV_Horario$DIV_Horario_HumectUmbral
		DIVAcum =  cumsum(resultado$DIV_Horario$DIV_Horario_HumectUmbral)
		texto = "Temperatura (activa =  verde)\nSensor Humectación"
	}else{
		valoresHumectacion = datosClimaticos$HR
		if(umbralHumectacion >= 92.5)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR92.5 
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 92.5%"
		}else if(umbralHumectacion >= 90)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR90 
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 90%"
		}else if(umbralHumectacion >= 87.5)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR87.5
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 87.5%"
		}else if(umbralHumectacion >= 85)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR85
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 85%"
		}else if(umbralHumectacion >= 82.5)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR82.5
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 82.5%"
		}else if(umbralHumectacion >= 80)
		{
			DIV_Horario = resultado$DIV_Horario$DIV_Horario_HR80
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 80%"
		}else
		{
			DIV_Horario = resultado$DIV_Horario$DDIV_Horario_HR90 
			DIVAcum =  cumsum(DIV_Horario)
			texto = "Temperatura (activa = verde)\nSensor HR, umbral 90%"
		}
	}
	
	###### VARIABLES 
	
	umbralInferiorInfeccion = 15.5 #!!! es 2ºC???
	umbralSuperiorInfeccion = 35
	umbralReseteoInferior = 0
	umbralReseteoSuperior = 40
				
	colorAcumulacionTemperatura = 'darkgreen'
	colorNoAcumulacionTemperatura = 'gray'
	colorReseteoPorTemperatura = 'red'
				
	ltyAcumulacionTemperatura = "solid"
	ltyNoAcumulacionTemperatura = 'dashed' #'dotted'
	ltyReseteoPorTemperatura = "solid"	

	ylim = c(-5, 45)
	
	###### CALCULOS PREVIOS
	colorLinea <- ifelse(temperaturas > umbralInferiorInfeccion & temperaturas < umbralSuperiorInfeccion,
						ifelse(valoresHumectacion >= umbralHumectacion, colorAcumulacionTemperatura, colorNoAcumulacionTemperatura),
						#ifelse(DIV_Horario >= 0, colorAcumulacionTemperatura, colorNoAcumulacionTemperatura),
						ifelse(temperaturas > umbralReseteoInferior & temperaturas < umbralReseteoSuperior,
						colorNoAcumulacionTemperatura, colorReseteoPorTemperatura))
															
	#ltyLinea <- ifelse(temperaturas > umbralInferiorInfeccion & temperaturas < umbralSuperiorInfeccion,
	#									ifelse(valoresHumectacion >= umbralHumectacion, ltyAcumulacionTemperatura, ltyNoAcumulacionTemperatura),
	#									ifelse(temperaturas > umbralReseteoInferior & temperaturas < umbralReseteoSuperior,
	#											ltyNoAcumulacionTemperatura, ltyReseteoPorTemperatura))
	
	###### GRÁFICO
	plot(x, temperaturas, type = "l", col = colorNoAcumulacionTemperatura, lty = ltyNoAcumulacionTemperatura, ylim = ylim, xlab = "", ylab = "", axes = FALSE)
	mtext(text = texto, side = 2, cex = 0.6, line = 1, adj = 0)
	
	
	lines(x, ifelse(colorLinea == colorAcumulacionTemperatura, temperaturas, NA), col = colorAcumulacionTemperatura, lty = ltyAcumulacionTemperatura)
	lines(x, ifelse(colorLinea == colorReseteoPorTemperatura, temperaturas, NA), col = colorReseteoPorTemperatura, lty = ltyReseteoPorTemperatura)
		#points(x[colorLinea == colorAcumulacionTemperatura], temperaturas[colorLinea == colorAcumulacionTemperatura], col = colorAcumulacionTemperatura, pch = 16) #lty = ltyAcumulacionTemperatura)
		#points(x[colorLinea == colorReseteoPorTemperatura], temperaturas[colorLinea == colorReseteoPorTemperatura], col = colorReseteoPorTemperatura, pch = 16) #lty = ltyReseteoPorTemperatura)
				
		#		stationName <- if(length(nombreEstacion(resultado$ESTACION[1]) > 0)) nombreEstacion(resultado$ESTACION[1]) else resultado$ESTACION[1]
		#		title(main = paste(stationName, strftime(resultado$FECHA[1], "%Y"))) 
		#		mtext(text = "Temperatura Aire", side = 2, cex = 0.8, line = 1.5)				
	if(pintarLineaDIV){
		if(max(DIVAcum) < 50)
		{
			escalaEjeYDcha <- seq(0, 50, 10)
		}else if(max(DIVAcum) < 100)
		{
			escalaEjeYDcha <- seq(0, 100, 20)
		}else if(max(DIVAcum) < 150)
		{
			escalaEjeYDcha <- seq(0, 150, 30)
		}else if(max(DIVAcum) < 200)
		{
			escalaEjeYDcha <- seq(0, 200, 50)
		}else
		{
			escalaEjeYDcha <- c(0, 100, 200, 300, 400) # para pintarla necesitamos la escala traducida al eje izquierdo
		}
		# Pasándole un vector de valores "y", y su escala (min,max), devuelve un vector con los valores transformados a la escala real (0,3500)
		# Esta función la puedes usar siempre que quieras pintar cualquier valor relativo al balance, transformado a las Y reales
		transformaCoordAEscalaEjeY <- function(y, minValuey, maxValuey, minValueY, maxValueY){
			#maxYAbs: el máximo valor absoluto de la escala
			#maxValue: el máximo valor en la nueva escala
			#minValue: el mínimo valor en la nueva escala
			prop <- abs(maxValuey - minValuey)/abs(maxValueY - minValueY)
			Y <- (y - minValueY)*prop + minValuey
			return(Y)
		}
		
		#TEST
		#transformaCoordAEscalaEjeY(0, -5, 45, 0, 400)
			
		axis(side = 4, at = transformaCoordAEscalaEjeY(escalaEjeYDcha, ylim[1], ylim[2], first(escalaEjeYDcha), last(escalaEjeYDcha)),
			labels = escalaEjeYDcha, las = 1, pos =  x[length(x)], col = colDIV, col.axis = colDIV, col.ticks= colDIV) 
		
		lines(x, transformaCoordAEscalaEjeY(DIVAcum, ylim[1], ylim[2], first(escalaEjeYDcha), last(escalaEjeYDcha)),
			type = 'l', col = colDIV, lty = 'solid', cex = 2)
	}			
	
		
	#Eje X
	if(pintarEjeX) posFechas <- axis(side = 1, at = x[posicionMarcasEjeX], labels = etiquetasEjeX, tick = TRUE, pos = ylim[1])
	
	#Eje Y
	valoresEjeY = c(ylim[1], umbralReseteoInferior, umbralInferiorInfeccion, umbralSuperiorInfeccion, 30, umbralReseteoSuperior, ylim[2]) #c(-5, 0, 20, 23, 30, 40)
				
	axis(side = 2, at = valoresEjeY, labels = paste0(valoresEjeY, 'ºC'), cex.axis = 0.8, las = 1, cex = 0.4, pos = x[1])
			
	# Líneas paralelas para referenciar 
	clip(min(x), max(x), ylim[1], ylim[2])  # para que al pintar el abline no se salga de los límites
	# fechas
	if(pintarEjeX) abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
	# referencias del modelo
	abline(h = umbralReseteoInferior, col = "brown", lty = "dashed") # Línea de reseteo por temperaturas muy bajas
	abline(h = umbralInferiorInfeccion, col = "blue", lty = "solid") # Línea a partir de la cual el modelo comienza a acumular riesgo si hay condiciones
	abline(h = umbralSuperiorInfeccion, col = "blue", lty = "solid") # Línea a partir de la cual el modelo deja de acumular riesgo si hay condiciones
	abline(h = umbralReseteoSuperior, col = "brown", lty = "dashed") # Línea de reseteo por temperaturas muy altas
	# otras referencias
	abline(h = 10, col = "black", lty = "dashed") # Línea a 10ºC	
	abline(h = 20, col = "black", lty = "dashed") # Línea a 20ºC	
	abline(h = 30, col = "black", lty = "dashed") # Línea a 30ºC
}

# pintaGraficaTemperatura(resultado, posicionMarcasEjeXHorarios, etiquetasEjeXHorarios, tipoGrafico = 'Humectacion', umbralHumectacion = 5, pintarEjeX = TRUE, pintarLineaDIV = TRUE)
# pintaGraficaTemperatura(resultado, posicionMarcasEjeXHorarios, etiquetasEjeXHorarios, tipoGrafico = 'HR', umbralHumectacion = 85, pintarEjeX = TRUE, pintarLineaDIV = TRUE)

# pintaGraficaTemperatura(a, posicionMarcasEjeX, etiquetasEjeX, tipoGrafico = 'Humectacion', umbralHumectacion = 5, pintarEjeX = TRUE, pintarLineaDIV = TRUE)


# Funciones de integración de otras funciones

pintaGraficaRiesgoDIV_V17 <- function(resultado, tipoGrafico = "pdf", etiquetasEjeX, graficoCampania = FALSE,
									horaInicio = 10, fechaInicio ='2020-03-01', fechaFin = '2020-09-01',
									pintaDIV_AMERICANO = FALSE, pintaGraficoHeatmapSimplificado = FALSE,
									pintaGraficaTHR = TRUE, etiquetasEjeXDIV3 = NULL, etiquetasEjeXDIV5 = NULL, etiquetasEjeXAgricultor = NULL,
									textoDIV3 = NULL, textoDIV5 = NULL, textoAgricultor = NULL,
									evolucionEnfermedad = NULL, tituloGrafico = NULL)
{
	
	
	nombreEstacion = resultado$nombreEstacion
	
	nombreArchivo = paste0(if(is.null(tituloGrafico)) nombreEstacion else tituloGrafico,
							if(pintaDIV_AMERICANO)
							{
								"-RiesgoCercosporaModeloAmericano-"
							}else
							{
								"-RiesgoCercospora-"
							},
							if(!pintaGraficaTHR) '-sin temperatura-' else '',
							strftime(resultado$DIV$fecha[1], format = "%b%Y"))
		
	nombreArchivoPDF = paste0(nombreArchivo,".pdf")
	nombreArchivoPNG = paste0(nombreArchivo, ".png")
	
	
	
	if(tipoGrafico == "pdf"){
		nombreArchivo = nombreArchivoPDF # no editar, se devuelve esta variable
		pdf(nombreArchivo)
	}else if(tipoGrafico == "png"){
		nombreArchivo = nombreArchivoPNG # no editar, se devuelve esta variable
		png(nombreArchivo) #, res = 1200, pointsize = 6)
	}else{
		nombreArchivo = ""
	}

	# Los valores de x son comunes a todos los gráficos	
	if(graficoCampania){ #!!!! ESTO NO FUNCIONA
		x <- seq.POSIXt(from = as.POSIXct(strftime(paste0(fechaInicio, " ", horaInicio, ":00"), tz = "UTC")),
						to = as.POSIXct(strftime(paste0(fechaFin, " ", horaInicio, ":00"), tz = "UTC")),
						by = "day")
		xHorarios <- x
	}else{
		if(pintaDIV_AMERICANO)
		{
			x = as.POSIXct(resultado$DIV_AMERICANO$fecha)
			xHorarios <- resultado$datosClimaticos$fecha
		}else
		{
			x = as.POSIXct(resultado$DIV$fecha)
			xHorarios <- resultado$datosClimaticos$fecha
		}
	}
	
	if(is.null(tituloGrafico))
	{
		if(pintaDIV_AMERICANO)
		{
			tituloGrafico <- 'Resultado modelo americano'
		}else
		{
			tituloGrafico <- 'Resultado modelo Horario'
		}
		tituloGrafico <- paste(tituloGrafico, resultado$nombreEstacion)
	}
			
	
	# Generamos el eje X para todos los gráficos
	#posicionMarcasEjeXDiarios <- generaMarcasEjeX(x, etiquetasEjeX)$posicionMarcasEjeX
	#etiquetasEjeXDiarios <-  generaMarcasEjeX(x, etiquetasEjeX)$etiquetasEjeX
		
	# posicionMarcasEjeXHorarios <- generaMarcasEjeX(xHorarios, etiquetasEjeX)$posicionMarcasEjeX
	# etiquetasEjeXHorarios <-  generaMarcasEjeX(xHorarios, etiquetasEjeX)$etiquetasEjeX
	
	
	if(!pintaGraficoHeatmapSimplificado)
	{
		par(mfrow=c(1,3))
		# GRÁFICA DE MODELO
		par(fig=c(0,1,0.55,1), # tamaño relativo, c(dcha, izda, abajo, arriba)
			mar = c(2, 4, 0, 2) + 0.1) # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
	}else
	{
		par(mfrow=c(1,3))
		par(fig=c(0.07,0.93,0.6,0.97), mar = c(2, 4, 2, 2) + 0.1)
	}
			
			if(pintaDIV_AMERICANO)
			{
				if(!pintaGraficoHeatmapSimplificado)
				{
					#DIV_Americano no tiene la estructura que espera HeatMap_V12, se le pasan las columnas 1 y 2 
					# y aquellas que, de cada grupo de 4 son la 3 y 4
					numeroColumnas = (ncol(resultado$DIV_AMERICANO) - 2)
					columnasAPasar = 2 + sort(c(which(1:numeroColumnas %% 4 == 3), which(1:numeroColumnas %% 4 == 0))) #c(columnasAPasar, 2 + sort(c(which(1:numeroColumnas %% 4 == 3), which(1:numeroColumnas %% 4 == 0))))
				
					pintaGraficaRiesgoDIV_HEATMAP_V12(resultado$DIV_AMERICANO[,c(1,2, columnasAPasar)], etiquetasEjeX = etiquetasEjeX,
														nombresTratamientos = c('Humectacion', 'HR80', 'HR82.5', 'HR85', 'HR87.5', 'HR90', 'HR92.5'),
														tituloGrafico = tituloGrafico)
					print('SALIDA MODELO AMERICANO')
				}else
				{
					#DIV_Americano no tiene la estructura que espera HeatMap_V12, se le pasan las columnas 1 y 2 
					columnasAPasar = c(1,2)
					# y aquellas que, de cada grupo de 4 son la 3 y 4
					#numeroColumnas = (ncol(resultado$DIV_AMERICANO) - 2)
					#columnasAPasar = 2 + sort(c(which(1:numeroColumnas %% 4 == 3), which(1:numeroColumnas %% 4 == 0))) #c(columnasAPasar, 2 + sort(c(which(1:numeroColumnas %% 4 == 3), which(1:numeroColumnas %% 4 == 0))))
				
					pintaGraficaRiesgoDIV_HEATMAP_V12_Simplificado(resultado$DIV_AMERICANO[,c(1, 2, 17, 18)], etiquetasEjeX = etiquetasEjeX,
														nombresTratamientos = c('HR85'),
														tituloGrafico = tituloGrafico)
					print('SALIDA MODELO SIMPLIFICADO')
				
				}
			}else
			{	
				if(!pintaGraficoHeatmapSimplificado)
				{
					pintaGraficaRiesgoDIV_HEATMAP_V12(resultado$DIV, etiquetasEjeX = etiquetasEjeX,
														nombresTratamientos = c('Humectacion', 'HR80', 'HR82.5', 'HR85*', 'HR87.5', 'HR90', 'HR92.5'),
														tituloGrafico = tituloGrafico)
					print('SALIDA MODELO HORARIO')
				}else
				{
					resultado$DIV2 = resultado$DIV[,c(1,2,9,10)]#resultado$DIV[,c(1,2,3,4,9,10)]
					pintaGraficaRiesgoDIV_HEATMAP_V12_Simplificado(resultado$DIV2, etiquetasEjeX = etiquetasEjeX,
														nombresTratamientos = 'HR85*', #c('Humectacion', 'HR85*'),
														tituloGrafico = tituloGrafico)
														
					print('SALIDA MODELO HORARIO SIMPLIFICADO')
					
					if(!is.null(etiquetasEjeXDIV3) || !is.null(etiquetasEjeXDIV5) || !is.null(etiquetasEjeXAgricultor))
					{
						par(fig=c(0.07,0.93,0.35,0.65), # tamaño relativo, c(dcha, izda, abajo, arriba)
							mar = c(0, 4, 4, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
							new=TRUE)
						dibujaFechasAplicacionTratamientos(resultado$DIV2, etiquetasEjeX,
													etiquetasEjeXDIV3 = etiquetasEjeXDIV3, etiquetasEjeXDIV5 = etiquetasEjeXDIV5, etiquetasEjeXAgricultor = etiquetasEjeXAgricultor,
													textoDIV3, textoDIV5, textoAgricultor)
					}
					if(!is.null(evolucionEnfermedad))
					{
						par(fig=c(0.07,0.93,0.1,0.35), # tamaño relativo, c(dcha, izda, abajo, arriba)
							mar = c(0, 4, 4, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
							new=TRUE)
					
						dibujaEvolucionEnfermedad <- function(resultado, evolucionEnfermedad,  colorAimcraV5, colorAFA, colorHS)
						{
						
						x = as.POSIXct(resultado$DIV$fecha)	
						x = c(x, last(x)+24*3600)

						ylim = c(0, 10) # + tamanioExtra porque en la barra superior se pinta la leyenda del gráfico
	
						plot(x, rep(NA,length(x)), type = "n", ylim = ylim, # ylim = c(0, 14),
								xlab = "", ylab = "", axes = FALSE)

						title(main = 'Evolución enfermedad y acumulación riesgo', line = 1)

						
						
						lines(as.POSIXct(evolucionEnfermedad$fecha, format = '%d/%m/%Y'), evolucionEnfermedad$aimcraV6, col = colorAimcraV5, lwd = 2) # OJO LA VARIABLE CAMBIA DE NOMBRE aimcraV5 en 2020 y aimcraV6 en 2021
						lines(as.POSIXct(evolucionEnfermedad$fecha, format = '%d/%m/%Y'), evolucionEnfermedad$X.AFA/10, col = colorAFA, lwd = 2) # está en porcentaje y la escala es de 0 a 10
						lines(as.POSIXct(evolucionEnfermedad$fecha, format = '%d/%m/%Y'), evolucionEnfermedad$X.HS/10, col = colorHS, lwd = 2) # está en porcentaje y la escala es de 0 a 10
						lines(resultado$DIV2$fecha, unlist(cumsum(resultado$DIV2[3]/20)), col = 'darkgreen', lwd = 2)
						marcasDiarias = TRUE
						pos = -0.2
						color = 'black'
						tcl = -0.5
						primeraFechaTck = TRUE
						primeraFechaLbl = TRUE

						posicionMarcasEjeX <- generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$posicionMarcasEjeX
						etiquetasEjeX <-  generaMarcasEjeX(x, etiquetasEjeX, marcasDiarias = marcasDiarias)$etiquetasEjeX
		
						atEjeX = if(primeraFechaTck) x[posicionMarcasEjeX] else x[posicionMarcasEjeX][-1]
						labelsEjeX = if(primeraFechaLbl) etiquetasEjeX else etiquetasEjeX[-1]

						if(length(atEjeX) != length(labelsEjeX)) labelsEjeX = c(NA, labelsEjeX)

						if(tcl == 0) labelsEjeX = rep(NA, length(atEjeX))

						posFechas <- axis(side = 1, at = atEjeX,
									labels = labelsEjeX,
									tick = TRUE, pos = pos, tcl = tcl,
										col = color, col.ticks = color,	col.axis = color)
	
						#Ejes Y # PINTABA PERO LOS RECTÁNGULOS DIBUJADOS DESPUÉS IMPEDIAN VERLO, si se quiere pintar el eje Y hay que hacerlo al final
						axis(side = 2, at = seq(0,10, 2),
								labels = seq(0,10, 2), tcl = -0.8,
								las = 1, cex = 0.1, line = -1, col = colorAimcraV5, col.axis = colorAimcraV5)
						
						axis(side = 2, at = 0:10,
								labels = rep(NA, 10), tcl = -0.3,
								las = 1, cex = 0.1, cex.axis = 0.75, line = -1, col = colorAimcraV5)

						mtext(text = "Escala AIMCRA V6", side = 2, cex = 0.6, line = 0.5, adj = 0.5, col = colorAimcraV5)

						
						# axis(side = 2, at = seq(0,10, 2),
								# labels = seq(0,100, 20), tcl = -0.8,
								# las = 1, cex = 0.1, line = 2, col = colorAFA , col.axis = colorAFA )
						
							# axis(side = 2, at = 0:10,
								# labels = rep(NA, 10), tcl = -0.3,
								# las = 1, cex = 0.1, line = 2, col = colorAFA)

						mtext(text = "Area Foliar Afect. %", side = 2, cex = 0.6, line = 1.75, adj = 0.5, col = colorAFA)
					
						axis(side = 2, at = seq(0,10, 2),
								labels = seq(0,100, 20), mgp = c(3, -1.6, 0), tcl = 0.8,
								las = 1, cex = 0.1, cex.axis = 0.75, line = 4.25, col = colorAFA , col.axis = colorAFA )
												
						axis(side = 2, at = 0:10,
								labels = rep(NA, 10), tcl = 0.3,
								las = 1, cex = 0.1, line = 4.25, col = colorAFA)
								
								
						axis(side = 2, at = seq(0,10, 2),
								labels = seq(0,100, 20), tcl = -0.8,
								las = 1, cex = 0.1, cex.axis = 0.75, line = 4.25, col = colorHS, col.axis = colorHS)
						
							axis(side = 2, at = 0:10,
								labels = rep(NA, 10), tcl = -0.3,
								las = 1, cex = 0.1, line = 4.25, col = colorHS)
						
						mtext(text = "Hojas Sintomáticas %", side = 2, cex = 0.6, line = 6, adj = 0.5, col = colorHS)

						
						# EJE Y2
						axis(side = 4, at = seq(0,10, 2),
								labels = seq(0,150, 30), tcl = -0.8,
								las = 1, cex = 0.1, pos = last(x), col = 'darkgreen', col.axis = 'darkgreen')
						
							axis(side = 4, at = 0:10,
								labels = rep(NA, 10), tcl = -0.3,
								las = 1, cex = 0.1, pos = last(x), col = 'darkgreen')

						mtext(text = "DIV acumulado", side = 4, cex = 0.8, line = 1, adj = 0.5, col = 'darkgreen')
						
						clip(x[1], last(x), 0, 10)
						abline(h = c(2,4, 6, 8 ), lty = 'dotted', lwd = 2)
	}
						dibujaEvolucionEnfermedad(resultado, evolucionEnfermedad,  colorAimcraV5 = 'brown', colorAFA = 'salmon', colorHS = 'orange')
					}
				}
			}

		# GRÁFICA HUMECTACIONES Y HUMEDAD RELATIVA
		if(!pintaGraficaTHR)
		{
			print('NO SE PINTAN GRÁFICOS DE TEMPERATURA ') # se podrían importar de procesos del año anterior
		}else
		{
			#TEMPERATURA
			par(fig=c(0,1,0.3,.55), # tamaño relativo, c(dcha, izda, abajo, arriba)
				mar = c(0, 4, 4, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
				new=TRUE)
		
			pintaGraficaTemperatura(resultado, etiquetasEjeX, tipoGrafico = 'HR', umbralHumectacion = 85, pintarEjeX = TRUE, pintarLineaDIV = TRUE)		
		
			par(fig=c(0,1,0.05,.3), # tamaño relativo, c(dcha, izda, abajo, arriba)
				mar = c(0, 4, 4, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
				new=TRUE)
		
			pintaGraficaTemperatura(resultado, etiquetasEjeX, tipoGrafico = 'Humectacion', umbralHumectacion = 5, pintarEjeX = TRUE, pintarLineaDIV = TRUE)
		}
		
	if(tipoGrafico != "") dev.off() # cierra si es cualquier tipo de device
		
	return(nombreArchivo)
}

# ESTACION = 505
# fechaInicio <- as.POSIXct("2015-06-01 00:00 UTC")
# fechaFin <- as.POSIXct("2015-09-30 23:34:20 UTC")
# resultado <- calculaRiesgoDIV_V12(ESTACION, fechaInicio, fechaFin, matrizDIV, vectorDIVHorario, horaInicio)
# pintaGraficaRiesgoDIV_V12(resultado, tipoGrafico = '', etiquetasEjeX, graficoCampania = FALSE)
# pintaGraficaRiesgoDIV_V12(resultado, tipoGrafico = "pdf", etiquetasEjeX, graficoCampania = FALSE,
									# horaInicio = 10, fechaInicio ='2020-03-01', fechaFin = '2020-09-01')


borrar <- function(){
			# Pintar un gráfico vacío con las dimensiones apropiadas
			plot(xHorarios, rep(NA, length(xHorarios)), xlab = "", ylab = "", ylim = c(0,100), type = "n", xaxt='n', yaxt='n', bty="n") 
	
			# Ahora creamos los vectores que pintarán el eje X
			axis(side = 3, at = x[posicionMarcasEjeXHorarios], labels = etiquetasEjeXHorarios, tick = TRUE, las = 2, pos = 0)

			# Pintar Eje Y
			escalaEjeYIzq <- c(0, 20, 40, 60, 85, 90, 100)	# esta es la escala real considerada
			escalaEjeYDcha <- c(0, 10, 30, 60) # para pintarla necesitamos la escala traducida al eje izquierdo

		# Pasándole un vector de valores "y", y su escala (min,max), devuelve un vector con los valores transformados a la escala real (0,3500)
		# Esta función la puedes usar siempre que quieras pintar cualquier valor relativo al balance, transformado a las Y reales
		transformaCoordAEscalaEjeY <- function(y, minValue, maxValue, maxYAbs = 100){
			#maxYAbs: el máximo valor absoluto de la escala
			#maxValue: el máximo valor en la nueva escala
			#minValue: el mínimo valor en la nueva escala
			prop <- maxYAbs/(maxValue - minValue) 
			Y <- (y - minValue)*prop
			return(Y)
		}
			
			axis(side = 2, at = escalaEjeYIzq, labels = escalaEjeYIzq, las = 1, pos =  x[1])
			axis(side = 4, at = transformaCoordAEscalaEjeY(escalaEjeYDcha,-200,300), labels = escalaEjeYDcha, las = 1, pos =  x[length(x)]) 
		
			plot(xHorarios, resultado$datosClimaticos$temperatura, type = "l", col = colTemperatura, ylim = c(-5,40), xlab = "", ylab = "", axes = FALSE)
			mtext(text = "Temperatura Aire", side = 2, cex = 0.6, line = 2.5)				
		
			
		# str(a)
	# List of 3
 # $ DIV            :'data.frame':        24 obs. of  8 variables:
  # ..$ fecha              : POSIXct[1:24], format: "2020-03-02 10:00:00" "2020-03-03 10:00:00" ...
  # ..$ estacion           : num [1:24] 2712 2712 2712 2712 2712 ...
  # ..$ DIV_HR85           : num [1:24] 0 3.43 3.23 4.29 3.14 ...
  # ..$ DIV_HR85Acum       : num [1:24] 0 3.43 6.66 7.52 7.43 ...
  # ..$ DIV_HR90           : num [1:24] 0 2.57 2.77 3.43 1.71 ...
  # ..$ DIV_HR90Acum       : num [1:24] 0 2.57 5.34 6.2 5.14 ...
  # ..$ DIV_HumectUmbral   : num [1:24] 0 0 2.31 6.86 4 ...
  # ..$ DIV_HumectUmbraAcum: num [1:24] 0 0 2.31 9.16 10.86 ...
 # $ datosClimaticos:'data.frame':        576 obs. of  4 variables:
  # ..$ fecha      : POSIXct[1:576], format: "2020-03-01 11:00:00" "2020-03-01 12:00:00" ...
  # ..$ temperatura: num [1:576] 17.5 17.5 17.5 17.5 17.5 ...
  # ..$ humectacion: num [1:576] 0 0 0 0 0 0 0 0 0 0 ...
  # ..$ HR         : num [1:576] 62.2 62 61.8 61.8 61.9 ...
 # $ DIV_Horario    :'data.frame':        576 obs. of  3 variables:
  # ..$ DIV_Horario_HR90        : num [1:576] 0 0 0 0 0 0 0 0 0 0 ...
  # ..$ DIV_Horario_HR85        : num [1:576] 0 0 0 0 0 0 0 0 0 0 ...
  # ..$ DIV_Horario_HumectUmbral: num [1:576] 0 0 0 0 0 0 0 0 0 0 ...

		lines(x = xHorarios,  # x debe ser de tipo Date
				y = transformaCoordAEscalaEjeY(datosTAirMdMensual$TAirMd,0,33),
				col = "grey", type= "s", pch = 20, lwd = 2) 
	lines(x = as.Date(paste(anio,datosTAirMdMensual$mes,"01", sep="-")),  
				y = transformaCoordAEscalaEjeY(datosTAirMdMensual$TAirMd,0,33),
				col = "grey", type= "h", pch = 20, lwd = 2)

					
				
				
				
				lines(x, resultado$datosClimaticos$HR, type = "l", col = colHR80)
				lines(x, resultado$DIV$T_HR85, type = "l", col = colHR85)
				lines(x, resultado$DIV$T_HR90, type = "l", col = colHR90)
				
				
				
				
				
				
				
				
				
				
				
				#Eje X
				posFechas <- axis(side = 1, at = x[posicionMarcasEjeX], labels = rep("", length(posicionMarcasEjeX)), tick = TRUE, pos = 5) #, labels = etiquetasEjeX, tick = TRUE, pos = 5)
				abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
				#Eje Y
				axis(side = 2, at = c(5, 15.55, 35, 40),
								labels = c("5ºC", "15,55ºC", "35ºC", "40ºC"), las = 1, cex = 0.4, pos = x[1])
			
				# Líneas paralelas para referenciar
				clip(min(x), max(x), 5, 40)  # para que al pintar el abline no se salga de los límites
				abline(h = 15.55, col = "brown", lty = "dashed") # Línea de reseteo por temperaturas muy bajas
				abline(h = 35, col = "brown", lty = "dashed") # Línea de reseteo por temperaturas muy altas
				abline(h = 5, col = "black", lty = "solid", lwd = 1) # CHAPUZA TEMPORAL PARA NO TENER QUE REPARAR EL TEMA DEL EJE X QUE NO SE PINTA ENTERO
				
			# GRÁFICO HUMECTACIONES	
			par(fig=c(0,1,0.5,0.7), # tamaño relativo, c(dcha, izda, abajo, arriba)
				mar = c(2, 4, 0, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
				new=TRUE)
				
			plot(x, rep(NA, length(x)), type = "n", ylim = c(0,24), xlab = "", ylab = "", axes = FALSE)

			mtext(text = "Horas\nHumectacion", side = 2, cex = 0.6, line = 1.5)	
			
			# Se dibujan rectángulos, uno por cada humectacion, calculamos las coordenadas x de inicio y fin
				x1Ini = x+600
				x1Fin = x+3600*4
				x2Ini = x1Fin+600
				x2Fin = x1Fin+3600*4
				x3Ini = x2Fin+600
				x3Fin = x2Fin+3600*4
				x4Ini = x3Fin+600
				x4Fin = x3Fin+3600*4
				
				rect(x1Ini, 0, x1Fin, resultado$DIV$HorasHumectacion_HumectUmbral, col = colHumectUmbral, border = NA)
				rect(x2Ini, 0, x2Fin, resultado$DIV$HorasHumectacion_HR80, col = colHR80, border = NA)
				rect(x3Ini, 0, x3Fin, resultado$DIV$HorasHumectacion_HR85, col = colHR85, border = NA)
				rect(x4Ini, 0, x4Fin, resultado$DIV$HorasHumectacion_HR80, col = colHR90, border = NA)
				
				#Eje X
				axis(side = 1, at = x[posicionMarcasEjeX], labels = etiquetasEjeX, tick = TRUE, pos = 0)
				clip(min(x), max(x), 0, 24)  # para que al pintar el abline no se salga de los límites
				abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
				#Eje Y
				axis(side = 2, at = c(0, 12, 24), labels = c(0, 12, 24), las = 1, cex.axis = 1, pos = x[1])
				abline(h = 12, col = "black", lty = "dotted", lwd = 0.5)
				abline(h = 0, col = "black", lty = "solid", lwd = 1) # CHAPUZA TEMPORAL PARA NO TENER QUE REPARAR EL TEMA DEL EJE X QUE NO SE PINTA ENTERO
			
									
			# GRÁFICO DE DIV
			
			par(fig=c(0,1,0,0.45), # tamaño relativo, c(dcha, izda, abajo, arriba)
				mar = c(2, 4, 2, 2) + 0.1, # márgenes, c(bottom, left, top, right) default is c(5, 4, 4, 2) + 0.1
				new=TRUE)
				
			
				pintaGraficaRiesgoDIV_HEATMAP_V11(resultado$DIV, etiquetasEjeX = etiquetasEjeX)
								
		if(tipoGrafico != "") dev.off() # cierra si es cualquier tipo de device
		
		return(nombreArchivo)
}

# TEST
# ESTACION = 505
# fechaInicio <- as.POSIXct("2015-06-01 00:00 UTC")
# fechaFin <- as.POSIXct("2015-09-30 23:34:20 UTC")
# resultado <- calculaRiesgoDIV(ESTACION, fechaInicio, fechaFin, matrizDIV)
# pintaGraficaRiesgoDIV_V12(resultado, "") 
 
# pintaGraficaRiesgoDIV_V12(DIV505_2018, etiquetasEjeX = etiquetasEjeX)

  
				

pintaGraficoDIVAcumulados <- function(resultadoGrafico, offsetHR = 0, nombreEstacion,
										nombreArchivo = "", tipografico = "",
										incluirTitulo = TRUE, graficosADibujar = c(FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE),
										tituloUnSoloPDF = TRUE)
{

	siguienteImagen <- function(graficoActual, tamanioSegmento, mar =  c(2.1 , 4, 1, 1))
	{
		if(graficoActual * tamanioSegmento > 1) return()
		new = if(graficoActual == 1) FALSE else TRUE
		margenSuperior = 1 - (graficoActual - 1) * tamanioSegmento
		margenInferior = 1 - graficoActual * tamanioSegmento
		par(mar = mar, fig = c(0, 1, margenInferior, margenSuperior), new = new) #fig = c(x1, x2, y1, y2)  fig mandates plot(new = TRUE)#mar = c(5.1, 4.1, 4.1, 2.1) #
	}
	
	pintarTitulo <- function(graficoActual, incluirTitulo)
	{
		if(graficoActual == 1 & incluirTitulo)
		{
			title(main = tituloGrafico, line = -1)
		}
	}
			
	attach(resultadoGrafico)
	
		if(tipografico == "pdf") pdf(nombreArchivo)
			
		oldParMar <- par('mar')
			numeroGraficos = if(sum(graficosADibujar) > 3) sum(graficosADibujar) else 4
				tamanioSegmento = 1/numeroGraficos
					# Esta función nos configura par() partiendo las imágenes en trozos horizontales en función del número de gráficos
				
			 if(tituloUnSoloPDF)
			{
				tituloGrafico <- paste(strftime(as.Date(last(resultadoGrafico$fecha)), "%d/%m"), nombreEstacion)
			}else
			{
				tituloGrafico <- paste('Gráfico comparativo DIV Horarios calculados según diferentes umbrales de HR o Humectación' )
			}
				
			par(mfrow = c(numeroGraficos,1))
				
				graficoActual = 1			
				siguienteImagen(graficoActual, tamanioSegmento)
				
				if(graficosADibujar[1])
				{
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR92.5.2dias, DIV_HR92.5.7dias, DIV_HR92.5.14dias, DIV_HR92.5.21dias, ylab = paste0('DIVAcumulado HR > ',as.character(92.5 - offsetHR, '%'))) #, DIV_HR92.5.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[2])
				{			
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR90.2dias, DIV_HR90.7dias, DIV_HR90.14dias, DIV_HR90.21dias, ylab = paste0('DIVAcumulado HR > ', as.character(90 - offsetHR, '%'))) #, DIV_HR90.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[3])
				{			
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR87.5.2dias, DIV_HR87.5.7dias, DIV_HR87.5.14dias, DIV_HR87.5.21dias, ylab = paste0('DIVAcumulado HR > ',as.character(87.5 - offsetHR, '%'))) #, DIV_HR87.5.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[4])
				{				
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR85.2dias, DIV_HR85.7dias, DIV_HR85.14dias, DIV_HR85.21dias, ylab = paste0('DIVAcumulado HR > ',as.character(85 - offsetHR, '%'))) #, DIV_HR85.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[5])
				{
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR82.5.2dias, DIV_HR82.5.7dias, DIV_HR82.5.14dias, DIV_HR82.5.21dias, ylab = paste0('DIVAcumulado HR > ',as.character(82.5 - offsetHR, '%'))) #, DIV_HR82.5.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[6])
				{
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HR80.2dias, DIV_HR80.7dias, DIV_HR80.14dias, DIV_HR80.21dias, ylab = paste0('DIVAcumulado HR > ',as.character(80 - offsetHR, '%'))) #, DIV_HR80.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					graficoActual =  graficoActual + 1
					siguienteImagen(graficoActual, tamanioSegmento)
				}
				if(graficosADibujar[7])
				{				
					dibuja_GrafRiesgoAcumulado(fecha, DIV_HumectUmbral.2dias, DIV_HumectUmbral.7dias, DIV_HumectUmbral.14dias, DIV_HumectUmbral.21dias, ylab = 'DIVAcumulado Humectación') #, DIV_HumectUmbral.28dias, dibujaDIV.28dias = TRUE)
					pintarTitulo(graficoActual, incluirTitulo)
					#graficoActual =  graficoActual + 1
					#siguienteImagen(graficoActual, tamanioSegmento)
				}
			par(mar = oldParMar)
		if(tipografico == "pdf") dev.off()
	detach(resultadoGrafico)
	return(nombreArchivo)
}	


pintaGraficoVisualizacionComparativaHR <- function(resultadoGrafico, datosClimaticos, offsetHR = 0, resultado,
												tipoHR = 85, nombreArchivo = "", tipografico = "",
												incluirTitulo = TRUE, tituloUnSoloPDF = TRUE)
{
	options('encoding' = 'UTF-8')
	if(tipografico == "pdf") pdf(nombreArchivo)		
	oldParMar <- par('mar')
		par(mfrow = c(4,1))
			attach(resultadoGrafico)
				par('mar' = c(2.1 , 4, 1, 1), fig = c(0, 1, 0.67, 1), mgp = c(3,1,0)) #fig = c(x1, x2, y1, y2)  fig mandates plot(new = TRUE)#mar = c(5.1, 4.1, 4.1, 2.1) #
				
					if(tipoHR == 80)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR80.2dias, DIV_HR80.7dias, DIV_HR80.14dias, DIV_HR80.21dias, 
													ylab = paste0('Severidad infeccion con HR > ', as.character(80 - offsetHR),'%')) #, DIV_HR80.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(80 - offsetHR),'%')
					}else if(tipoHR == 82.5)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR82.5.2dias, DIV_HR82.5.7dias, DIV_HR82.5.14dias, DIV_HR82.5.21dias, 
													ylab = paste0('Severidad infeccion con HR > ', as.character(82.5 - offsetHR),'%')) #, DIV_HR82.5.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(82.5 - offsetHR),'%')
					}else if(tipoHR == 85)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR85.2dias, DIV_HR85.7dias, DIV_HR85.14dias, DIV_HR85.21dias, 
													ylab = paste0('Severidad infeccion con HR > ', as.character(85 - offsetHR),'%')) #, DIV_HR85.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(85 - offsetHR),'%')
					}else if(tipoHR == 87.5)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR87.5.2dias, DIV_HR87.5.7dias, DIV_HR87.5.14dias, DIV_HR87.5.21dias, 
													ylab = paste0('Severidad infeccion con HR > ', as.character(87.5 - offsetHR),'%')) #, DIV_HR87.5.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(87.5 - offsetHR),'%')
					}else if(tipoHR == 90)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR90.2dias, DIV_HR90.7dias, DIV_HR90.14dias, DIV_HR90.21dias,
													ylab = paste0('Severidad infeccion con HR > ', as.character(90 - offsetHR),'%')) #, DIV_HR90.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(90 - offsetHR),'%')
					}else if(tipoHR == 92.5)
					{
						dibuja_GrafRiesgoAcumulado(fecha, DIV_HR92.5.2dias, DIV_HR92.5.7dias, DIV_HR92.5.14dias, DIV_HR92.5.21dias,
													ylab = paste0('Severidad infeccion con HR > ', as.character(92.5 - offsetHR),'%')) #, DIV_HR92.5.28dias, dibujaDIV.28dias = TRUE)
						tituloGrafico = paste('Gráfico ilustrativo riesgo infección usando umbral de HR', as.character(92.5 - offsetHR),'%')
					}
					
					if(tituloUnSoloPDF) tituloGrafico <- paste(strftime(as.Date(last(resultadoGrafico$fecha), "%d/%m"), resultado$nombreEstacion))
						
					if(incluirTitulo)
					{
						title(main = tituloGrafico, line = -1)
					}
			
			detach(resultadoGrafico)
			
			attach(datosClimaticos)
				par('mar' = c(0, 4, 3, 1), fig = c(0, 1, 0.5, 0.66), mgp  = c(3, 1, 0), new = TRUE) #mar = c(bottom, left, top, right) 
					plot(fecha, PAcum, ylim = c(0,5), col = ifelse(lluviaEfectiva, 'blue', 'gray'),
							type = 'h', ylab = 'Lluvia (mm)', xlab = '', axes = FALSE, border = 'n')
					#lines(fecha, ifelse(lluviaEfectiva, 5, 0), col = "blue", type = "h")
					axis(side = 2, at = 0:5, labels = c(NA,'1', NA, '3', NA, '5'), etiquetasEjeX, tick = TRUE)
					
					
				par('mar' = c(2, 4, 1, 1), fig = c(0, 1, 0.37, 0.5), mgp  = c(1, 1, 0), new = TRUE) #4.1 4.1 2.1
					if(is.numeric(tipoHR))
					{
						plot(fecha, ifelse(HR > tipoHR - offsetHR, 1, 0), ylim = c(0,1), type = "h",
							col = ifelse(temperaturaEfectiva, 'black', ifelse(temperaturaEntre10y13, "gray", 'blanchedalmond')),
							ylab = paste0('HR > ', as.character(tipoHR - offsetHR), '%'), xlab = '',
							axes = FALSE, border = 'n')
					}else
					{
						warning('tipoHR debería ser un número, se usa tipoHR = 90 por defecto')
						plot(fecha, ifelse(HR > 90 - offsetHR, 1, 0), ylim = c(0,1), type = "h",
							col = ifelse(temperaturaEfectiva, 'red', ifelse(temperaturaEntre10y13, "orange", 'blanchedalmond')),
							ylab = paste0('HR > ', as.character(90 - offsetHR), '%'), xlab = '',
							yaxt = 'n', xaxt = 'n', border = 'n')
					}
					
					if(TRUE) #if(dibujaEjeX)
					{ 
						fechasEjeX <-  as.POSIXct(paste(get_etiquetasEjeX(length(fecha)), strftime(fecha[1], "%Y"), sep = "/"), format = "%d/%m/%Y")
						etiquetasEjeX <- get_etiquetasEjeX(length(fecha))
  
						posFechas <- axis(side = 1, at = fechasEjeX, labels = etiquetasEjeX, tick = TRUE, pos = 0)
						#abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
					}
	
			detach(datosClimaticos)	
			attach(resultadoGrafico)
				par('mar' = c(2.1 , 4, 1, 1), fig = c(0, 1, 0.1, 0.36), mgp  = c(1, 1, 0), new = TRUE) #fig = c(x1, x2, y1, y2)  fig mandates plot(new = TRUE)#mar = c(5.1, 4.1, 4.1, 2.1) #
					plot(fecha, rep(NA, nrow(resultadoGrafico)), ylim = c(0,1), type = "n",
									ylab = 'Condiciones Germinacion \n Posible Inicio Infeccion', xlab = '',
									axes = FALSE, border = 'n')
					if(TRUE)  #if(dibujaEjeX)
					{
						fechasEjeX <-  as.POSIXct(paste(get_etiquetasEjeX(length(fecha)), strftime(fecha[1], "%Y"), sep = "/"), format = "%d/%m/%Y")
						etiquetasEjeX <- get_etiquetasEjeX(length(fecha))
  
						posFechas <- axis(side = 1, at = fechasEjeX, labels = etiquetasEjeX, tick = TRUE, pos = 0)
						#abline(v = posFechas, col = "black", lty = "dotted", lwd = 0.5)
					}
			detach(resultadoGrafico)
			
			attach(resultado$fechasInfeccion)
					{# > str(resultadoAIMCRA2712$fechasInfeccion,1)
						# List of 6
						 # $ HR95.8horas       :'data.frame':     0 obs. of  3 variables:
						 # $ HR95.6horas       :'data.frame':     0 obs. of  3 variables:
						 # $ HR90.8horas       :'data.frame':     0 obs. of  3 variables:
						 # $ HR90.6horas       :'data.frame':     2 obs. of  3 variables:
						 # $ humectacion.8horas:'data.frame':     1 obs. of  3 variables:
						 # $ humectacion.6horas:'data.frame':     5 obs. of  3 variables:
						# >
					}
					if(nrow(humectacion.6horas) > 0){
						lines(humectacion.6horas$fechasInfeccion, rep(1, nrow(humectacion.6horas)), col = "darkgreen", lwd = 1, type = 'h')
						#lines(humectacion.6horas$fechasInfeccion + 24*3600*humectacion.6horas$diasHastaInfeccion, rep(1, nrow(humectacion.6horas)), col = "darkgreen", lwd = 1, lty = "dotted", type = 'h')
					}
					if(FALSE){ #if(nrow(humectacion.8horas) > 0){
						lines(humectacion.8horas$fechasInfeccion, rep(1, nrow(humectacion.8horas)), col = "darkgreen", lwd = 2, type = 'h')
						#lines(humectacion.8horas$fechasInfeccion + 24*3600*humectacion.8horas$diasHastaInfeccion, rep(1, nrow(humectacion.8horas)), col = "darkgreen", lwd = 2, lty = "dotted", type = 'h')
					}
					if(nrow(HR90.6horas) > 0){
						lines(HR90.6horas$fechasInfeccion, rep(1, nrow(HR90.6horas)), col = "orange", lwd = 1, type = 'h')
						#lines(HR90.6horas$fechasInfeccion + 24*3600*HR90.6horas$diasHastaInfeccion, rep(1, nrow(HR90.6horas)), col = "orange", lwd = 1, lty = "dotted", type = 'h')
					}
					if(FALSE){ #if(nrow(HR90.8horas) > 0){
						lines(HR90.8horas$fechasInfeccion, rep(1, nrow(HR90.8horas)), col = "orange", lwd = 2, type = 'h')
						#lines(HR90.8horas$fechasInfeccion + 24*3600*HR90.8horas$diasHastaInfeccion, rep(1, nrow(HR90.8horas)), col = "orange", lwd = 2, lty = "dotted", type = 'h')
					}
					
					if(nrow(HR95.6horas) > 0){
						lines(HR95.6horas$fechasInfeccion, rep(1, nrow(HR95.6horas)), col = "red", lwd = 2, type = 'h')
						#lines(HR95.6horas$fechasInfeccion + 24*3600*HR95.6horas$diasHastaInfeccion, rep(1, nrow(HR95.6horas)), col = "red", lwd = 1, lty = "dotted", type = 'h')
					}
					if(FALSE){ #if(nrow(HR95.8horas) > 0){
						lines(HR95.8horas$fechasInfeccion, rep(1, nrow(HR95.8horas)), col = "red", lwd = 2, type = 'h')
						#lines(HR95.8horas$fechasInfeccion + 24*3600*HR95.8horas$diasHastaInfeccion, rep(1, nrow(HR95.8horas)), col = "red", lwd = 2, lty = "dotted", type = 'h')
					}
								
				detach(resultado$fechasInfeccion)
		par('mar' = oldParMar)
	if(tipografico == "pdf") dev.off()
	return(nombreArchivo)
}


