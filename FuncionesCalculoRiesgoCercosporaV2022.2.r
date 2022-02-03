######################## FUNCIONES BÁSICAS DE CÁLCULO DE LOS DIV ########################
# Cálculos de los DIV
# Método americano:

calculaDIV_ModeloAmericano_HR <- function(temperaturas, HR, matrizDIV, umbralHR = 85, dataSamplingTime = 30)
{ 
	# temperaturas: un vector numérico con los valores de temperaturas (en ºC), pueden estar medidos en cualquier formato temporal
	# HR: vector numérico con los valores de humedar relativa (en tanto por ciento 0 a 100), en el mismo formato temporal que "temperaturas"
	# matrizDIV: matriz de DIV según modelo americano
	# dataSamplingTime = valor numérico que indica la granularidad de los datos, en minutos, si la estacion mide en horarios= 60, semihorarios = 30, quinceminutales = 15
	# umbralHR: valor numérico, indica el valor de HR por encima del cual se considera que hay agua libre en la atmósfera
	
	# Totalizar horas humectacion y redondearlas a un entero
	horasHumec <- round(sum(ifelse(HR > umbralHR, dataSamplingTime/60, 0)), digits = 0)
	# Calcular media temperaturas en horas en las que hay humectación
	tempMediaHumec <- round(mean(temperaturas[which(HR >  umbralHR)]), digits = 1)

	# Establecer qué valor de la matrizDIV corresponde a esa humectacion y a esa temperatura
	escalaFarenheit <- c(seq(from = 60, to = 94, by = 1), 95)
	escalaCelsius <- (escalaFarenheit-32) * 5/9

	if(tempMediaHumec < 15.2 || tempMediaHumec > 36 || horasHumec == 0)
	{
		DIV = 0
	}else
	{
		columnaTemperatura <- which.min(abs(escalaCelsius - tempMediaHumec))
		filaHumectacion <- (24 - horasHumec + 1)
		
		DIV = matrizDIV[filaHumectacion, columnaTemperatura]
	}
	
	return(list(DIV_HR = DIV,
				temperatura_HR = tempMediaHumec,
				horasHumectacion_HR = horasHumec))
}

calculaDIV_ModeloAmericano_HumectUmbral <- function(temperaturas, humectaciones, matrizDIV, valorHumectacionMinimo, dataSamplingTime = NULL)
{
	# temperaturas: un vector numérico con los valores de temperaturas (en ºC), pueden estar medidos en cualquier formato temporal
	# humectaciones: vector numérico con los valores de humectación (en minutos o porcentaje, ver "unidadesHumectacion"), en el mismo formato temporal que "temperaturas"
	# matrizDIV: matriz de DIV según modelo americano
	# valorHumectacionMinimo: Puede ser un tiempo, un porcentaje, etc, que indique qué valor ha de superarse para considerarse que el período de tiempo es húmedo
	# dataSamplingTime: no es necesario, pero si no se da se considera que el tiempo de medida es horario. Si se usa es de forma análoga a la función calculaDIV_ModeloAmericano_HR()
	
	# Totalizar horas humectacion y redondearlas a un entero
	if(is.null(dataSamplingTime))
	{
		horasHumec <- round(sum(humectaciones[humectaciones > valorHumectacionMinimo]/60), digits = 0)
	}else
	{ 
		horasHumec <- round(sum(ifelse(humectaciones > valorHumectacionMinimo, dataSamplingTime/60, 0)), digits = 0)
	}
	# Calcular media temperaturas en horas humectación
	tempMediaHumec <- round(mean(temperaturas[which(humectaciones > valorHumectacionMinimo)]), digits = 1)

	# Establecer qué valor de la matrizDIV corresponde a esa humectacion y a esa temperatura

	escalaFarenheit <- c(seq(from = 60, to = 94, by = 1), 95)
	escalaCelsius <- (escalaFarenheit-32) * 5/9
		
	if(tempMediaHumec < 15.28 || tempMediaHumec > 35.28 || horasHumec == 0)
	{ # por encima de estos valores o con cero humectacion no hay riesgo
		DIV = 0
	}else
	{
		columnaTemperatura <- which.min(abs(escalaCelsius - tempMediaHumec))
		filaHumectacion <- (24 - horasHumec + 1)
		
		DIV = matrizDIV[filaHumectacion, columnaTemperatura]
	}
		
		
	return(list(DIV_HumectUmbral = DIV,
				temperatura_HumectUmbral = tempMediaHumec,
				horasHumectacion_HumectUmbral = horasHumec))
}

# Método Horario, modelo Rioja:
calculaDIV_Horario_ModeloRioja <- function(temperaturas, humectaciones, vectorDIVHorario, valorUmbralMinimo, dataSamplingTime)
{
	# temperaturas: un vector numérico con los valores de temperaturas (en ºC), su período de tiempo viene dado por dataSamplingTime ejemplo: dataSamplingTime = 60 ==> 1 hora => datos horarios
	# humectaciones: un vector numérico con los valores de humectación (en minutos) ó humedad relativa (en tanto por ciento 0-100), idem temperaturas para dataSamplingTime
	# valorUmbralMinimo: Puede ser un tiempo p.ej: valorUmbralMinimo = 5; o un porcentaje de HR, p. ej. valorUmbralMinimo = 85, que indique qué valor ha de superarse para considerarse que el período de tiempo es húmedo
	# dataSamplingTime: valor numérico que indica la granularidad de los datos, en minutos, la tabla de DIV usada es horaria, si el dataSampling time es diferente se hace una regla de tres
	# vectorDIVHorario: vector de DIV según modelo Rioja
	
	
	matrizTempHumect <- data.frame(temperaturas = temperaturas, humectaciones = humectaciones)
		
	# Obtener qué horas han tenido humectación por encima del umbral
	matrizTempHumect$hayHumectacion <- matrizTempHumect$humectaciones > valorUmbralMinimo
				
	# Establecer qué valor de la matrizDIV corresponde a esa humectacion y a esa temperatura

	escalaFarenheit <- c(seq(from = 60, to = 94, by = 1), 95)
	escalaCelsius <- (escalaFarenheit-32) * 5/9
		
	matrizTempHumect$DIVHorario <- 0
	for(fila in 1:nrow(matrizTempHumect))
	{
		if(matrizTempHumect$temperaturas[fila] > escalaCelsius[1] &&			
			matrizTempHumect$temperaturas[fila] < (last(escalaCelsius) + 0.5) &&
			matrizTempHumect$hayHumectacion[fila])
		{
			matrizTempHumect$DIVHorario[fila] <- vectorDIVHorario$DIVHorario[which.min(abs(escalaCelsius - matrizTempHumect$temperaturas[fila]))] * dataSamplingTime/60 #dataSamplingTime/60 corrección a horas
		}
	}
		
	return(list(DIV_Horario_Acumulado = sum(matrizTempHumect$DIVHorario, na.rm = TRUE),
				DIV_Horario = matrizTempHumect$DIVHorario, matrizTempHumect = matrizTempHumect)) 
}
# # TEST
# calculaDIV_Horario_ModeloRioja_ModeloRioja(temperaturas = c(15, 16.5, 16, 36, 16, 0), humectaciones = c(30, 30, 0, 30, 30, 30),
						# vectorDIVHorario, valorUmbralMinimo = 5, dataSamplingTime = 60)
	


######################## FUNCIONES AUXILIARES PARA GENERAR OBJETOS CON LOS RESULTADOS CALCULADOS USANDO DISTINTOS MÉTODOS Y MÉTRICAS ########################
calculaDIVAcum <- function(DIV){ # Se usa en ambos modelos (americano y horario), para sumar el DIV diario de ayer y hoy
	res <- rep(NA, length(DIV)) # <- c()
	res[1] = DIV [1]
	
	for(posicion in 2:length(DIV))
	{
		res[posicion] = DIV [posicion] + DIV [posicion - 1]
	}
	return(res)
}

# Función para obtener un objeto (data.frame) con todos los resultados según el modelo americano
calculaRiesgoDIV_MODELO_AMERICANO <- function(datosClimaticos, diasCalculo, estacion, matrizDIV, horaInicio, offsetHR, dataSamplingTime, valorHumectacionMinimo)
{
	# horaInicio = "10", hora solar a la que se inician los cálculos
	# offsetHR = 0 por defecto se calcula el modelo con umbrales de HR de 80 a 92.5%. offsetHR permite modifificar los umbrales, por ejemplo offset = - 5 haría los cálculosa 75, 80 y 85% de umbral

		
	# Generar una data.frame igual a la que vamos a construir después en el for (Inicializar variable)
	DIV <- data.frame(fecha = as.POSIXct(character()),
					estacion = numeric(),
					DIV_HumectUmbral = numeric(), T_HumectUmbral = numeric(), HorasHumectacion_HumectUmbral = numeric(),
					DIV_HR80 = numeric(), T_HR80 = numeric(), HorasHumectacion_HR80 = numeric(),
					DIV_HR82.5 = numeric(), T_HR82.5 = numeric(), HorasHumectacion_HR82.5 = numeric(),
					DIV_HR85 = numeric(), T_HR85 = numeric(), HorasHumectacion_HR85 = numeric(),
					DIV_HR87.5 = numeric(), T_HR87.5 = numeric(), HorasHumectacion_HR87.5 = numeric(),
					DIV_HR90 = numeric(), T_HR90 = numeric(), HorasHumectacion_HR90 = numeric(),
					DIV_HR92.5 = numeric(), T_HR92.5 = numeric(), HorasHumectacion_HR92.5 = numeric()
					)
						
	DIV.names <- names(DIV)
	
	# Calcular el DIV para cada día de los solicitados
	for (dia in diasCalculo)
	{
		dia <- as.POSIXct(dia, origin = "1970-01-01")
		datos <- datosClimaticos[datosClimaticos$fecha > dia & datosClimaticos$fecha <= dia + 3600*24,] # extraer datos para el día de cálculo		
		
		if(nrow(datos) < (1440/dataSamplingTime - 288/dataSamplingTime)) next # dia Incompleto, se acepta la pérdida del 20% de datos
		
		res <- data.frame(fecha = dia + 3600*24, estacion = estacion,
					calculaDIV_ModeloAmericano_HumectUmbral(datos$temperatura, datos$humectacion, matrizDIV, valorHumectacionMinimo, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 80 - offsetHR, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 82.5 - offsetHR, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 85 - offsetHR, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 87.5 - offsetHR, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 90 - offsetHR, dataSamplingTime),
					calculaDIV_ModeloAmericano_HR(datos$temperatura, datos$HR, matrizDIV, umbralHR = 92.5 - offsetHR, dataSamplingTime)
					) # calcular DIV
		
		DIV <- rbind(DIV, res) # unir 
	}
	
	names(DIV) <- DIV.names
	
	# Insertar el DIV Acumulado
	DIV <- data.frame(fecha = DIV$fecha,
						estacion = DIV$estacion,
						T_HumectUmbral = DIV$T_HumectUmbral, HorasHumectacion_HumectUmbral = DIV$HorasHumectacion_HumectUmbral,
						DIV_HumectUmbral =  DIV$DIV_HumectUmbral,  DIV_HumectUmbralAcum = calculaDIVAcum(DIV$DIV_HumectUmbral),
						T_HR80 = DIV$T_HR80, HorasHumectacion_HR80 = DIV$HorasHumectacion_HR80,
						DIV_HR80 = DIV$DIV_HR80, DIV_HR80Acum = calculaDIVAcum(DIV$DIV_HR80),
						T_HR82.5 = DIV$T_HR82.5, HorasHumectacion_HR82.5 = DIV$HorasHumectacion_HR82.5,
						DIV_HR82.5 = DIV$DIV_HR82.5, DIV_HR82.5Acum = calculaDIVAcum(DIV$DIV_HR82.5),
						T_HR85 = DIV$T_HR85, HorasHumectacion_HR85 = DIV$HorasHumectacion_HR85,
						DIV_HR85 = DIV$DIV_HR85, DIV_HR85Acum = calculaDIVAcum(DIV$DIV_HR85),
						T_HR87.5 = DIV$T_HR87.5, HorasHumectacion_HR87.5 = DIV$HorasHumectacion_HR87.5,
						DIV_HR87.5 = DIV$DIV_HR87.5, DIV_HR87.5Acum = calculaDIVAcum(DIV$DIV_HR87.5),
						T_HR90 = DIV$T_HR90, HorasHumectacion_HR90 = DIV$HorasHumectacion_HR90,
						DIV_HR90 = DIV$DIV_HR90, DIV_HR90Acum = calculaDIVAcum(DIV$DIV_HR90),
						T_HR92.5 = DIV$T_HR92.5, HorasHumectacion_HR92.5 = DIV$HorasHumectacion_HR92.5,
						DIV_HR92.5 = DIV$DIV_HR92.5, DIV_HR92.5Acum = calculaDIVAcum(DIV$DIV_HR92.5)
						)
						
	return(DIV)
}

# Función para obtener un objeto (dos data.frames) con los resultados según el modelo Horario, un objeto permite comparar el resultado con el modelo americano
# el otro es el valor del DIV calculado para cada hora
calculaRiesgoDIV_MODELO_RIOJA  <- function(datosClimaticos, diasCalculo, estacion, vectorDIVHorario, offsetHR, dataSamplingTime, valorHumectacionMinimo){
	# GENERA DOS OBJETOS:
	# DIV, que es una data.frame con valores DIARIOS, es un remedo del DIV Americano, aunque no tiene mucho sentido, es sólo por hacer equiparables los conceptos, se calcula a horaInicio (que viene prefijado en diasCalculo)
	# DIV_Horario, que es una data.frame con valores HORARIOS, y tiene más sentido que lo anterior y permite integrar los cálculos del DIV para cada hora posteriormente
	
	# Generar data.frames iguales a las que vamos a construir después en el for
	DIV <- data.frame(fecha = as.POSIXct(character()),
					estacion = numeric(),
					DIV_HumectUmbral = numeric(),
					DIV_HR80 = numeric(),
					DIV_HR82.5 = numeric(),
					DIV_HR85 = numeric(),
					DIV_HR87.5 = numeric(),
					DIV_HR90 = numeric(),
					DIV_HR92.5 = numeric()											
					)
						
	DIV.names <- names(DIV)

	DIV_Horario <- data.frame(DIV_Horario_HumectUmbral = numeric(),
								DIV_Horario_HR80 = numeric(),
								DIV_Horario_HR82.5 = numeric(),
								DIV_Horario_HR85 = numeric(),
								DIV_Horario_HR87.5 = numeric(),
								DIV_Horario_HR90 = numeric(),
								DIV_Horario_HR92.5 = numeric()
								)									
						
	DIV_Horario.names <- names(DIV_Horario)
		
	for(dia in diasCalculo)  #[dias > first(datosClimaticos$fecha)]){
	{
		dia <- as.POSIXct(dia, origin = "1970-01-01")
		datos <- datosClimaticos[datosClimaticos$fecha > dia & datosClimaticos$fecha <= dia + 3600*24,] # extraer datos para el día de cálculo	(dia es un posixct con la hora de inicio del modelo incluida)	
	
		if(nrow(datos) == 0) next
		
		DIV_HumectUmbral = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$humectacion, vectorDIVHorario, valorUmbralMinimo = valorHumectacionMinimo, dataSamplingTime)
		DIV_HR80 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 80 - offsetHR, dataSamplingTime)
		DIV_HR82.5 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 82.5 - offsetHR, dataSamplingTime)
		DIV_HR85 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 85 - offsetHR, dataSamplingTime)
		DIV_HR87.5 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 87.5 - offsetHR, dataSamplingTime)
		DIV_HR90 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 90 - offsetHR, dataSamplingTime)
		DIV_HR92.5 = calculaDIV_Horario_ModeloRioja(datos$temperatura, datos$HR, vectorDIVHorario, valorUmbralMinimo = 92.5 - offsetHR, dataSamplingTime)
						
		res <- data.frame(fecha = dia + 3600*24, estacion = estacion,
					DIV_HumectUmbral$DIV_Horario_Acumulado,
					DIV_HR80$DIV_Horario_Acumulado,
					DIV_HR82.5$DIV_Horario_Acumulado,
					DIV_HR85$DIV_Horario_Acumulado,
					DIV_HR87.5$DIV_Horario_Acumulado,
					DIV_HR90$DIV_Horario_Acumulado,
					DIV_HR92.5$DIV_Horario_Acumulado	
				)
		
		DIV <- rbind(DIV, res)
		
		res2 <- data.frame(DIV_HumectUmbral$DIV_Horario,
							DIV_HR80$DIV_Horario,
							DIV_HR82.5$DIV_Horario,
							DIV_HR85$DIV_Horario,
							DIV_HR87.5$DIV_Horario,
							DIV_HR90$DIV_Horario,
							DIV_HR92.5$DIV_Horario							
						)

		DIV_Horario <- rbind(DIV_Horario, res2) 
	}
	
	
	names(DIV) <- DIV.names
	names(DIV_Horario) <- DIV_Horario.names
	
	# Insertar el DIV Acumulado
	DIV <- data.frame(fecha = DIV$fecha,
						estacion = DIV$estacion,
						DIV_HumectUmbral =  DIV$DIV_HumectUmbral,  DIV_HumectUmbralAcum = calculaDIVAcum(DIV$DIV_HumectUmbral),
						DIV_HR80 = DIV$DIV_HR80, DIV_HR80Acum = calculaDIVAcum(DIV$DIV_HR80),
						DIV_HR82.5 = DIV$DIV_HR82.5, DIV_HR82.5Acum = calculaDIVAcum(DIV$DIV_HR82.5),
						DIV_HR85 = DIV$DIV_HR85, DIV_HR85Acum = calculaDIVAcum(DIV$DIV_HR85),
						DIV_HR87.5 = DIV$DIV_HR87.5, DIV_HR87.5Acum = calculaDIVAcum(DIV$DIV_HR87.5),
						DIV_HR90 = DIV$DIV_HR90, DIV_HR90Acum = calculaDIVAcum(DIV$DIV_HR90),
						DIV_HR92.5 = DIV$DIV_HR92.5, DIV_HR92.5Acum = calculaDIVAcum(DIV$DIV_HR92.5)
						)

	return(list(DIV = DIV, DIV_Horario = DIV_Horario))
}


# Función que acumula los valores de DIV para un número de registros = numDatos
# IMPORTANTE: Los valores de temperaturas, humectación y umbral humectación NO SE USAN
# se pasan sólo por si acaso en el futuro se implementan reseteos en el modelo de acumulación
calculaDIVAcumHorario <- function(DIV, temperatura, humectacion, umbralHumectacion,  offsetHR = 0, numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime)
{
	DIVAcumHorario <- rep(NA, length(DIV))
	numHorasDIVesNulo <- rep(NA, length(DIV))
	
	numDatos = numDatos * 60/dataSamplingTime # Corrección para considerar el número de registros en cálculos no horarios
	
	for(posicion in 1:length(DIV))
	{
		if(DIV[posicion] == 0){# Para cada registro si el DIV es cero se acumulan las horas donde DIV == 0
			numHorasDIVesNulo[posicion] <- dataSamplingTime/60 + if(posicion > 1) numHorasDIVesNulo[posicion - 1] else 0
		}else{ # cuando es DIV != 0 se resetea la cuenta
			numHorasDIVesNulo[posicion] = 0
		}
		
		if(posicion >= numDatos){# Superado el número de datos mínimo se hace el cálculo del DIV
			origen = posicion - numDatos
			extracto <- data.frame(temperatura = temperatura,
									humectacion = humectacion,
									numHorasDIVesNulo = numHorasDIVesNulo,
									DIV = DIV)

			extracto <- extracto[origen:posicion, ]
			
			if(!is.na(numHorasReseteo)){ # este parámetro puede no estar definido
				# Comprobación por temperaturas extremas #!!!
					#if(any(extracto$temperatura) > umbralMaximo) etc
				# Comprobación por humectaciones bajas extremas #!!!
				# Comprobación por número de horas DIV > 0
				if(any(extracto$numHorasDIVesNulo == numHorasReseteo)){ # Si no hay reseteo la fórmula es como la de antes
					posicionReseteo <- last(which(extracto$numHorasDIVesNulo == nummHorasReseteo)) # Last() por si hay más de un reseteo
					extracto <- extracto[posicionReseteo:nrow(extracto),] # Detrás del reseteo se pueden acumular más horas secas pero solo se acumula DIV de ahí en adelante
				}
			}
			DIVAcumHorario[posicion] <- sum(extracto$DIV, na.rm = TRUE)
		}
	}

	return(DIVAcumHorario)
}

# Función que devuelve un vector de valores lógicos TRUE/FALSE que indican si la lluvia acumulada se considera efectiva o no.
esLluviaEfectiva <- function(PAcum)
{
	lluviaEfectiva <- logical() # Inicializar variable
		
	lluviaEfectivaHoraria = 1 * 60 / dataSamplingTime  # Si llueve más de un litro cada hora = 2 litros a la hora, la lluvia se considera efectiva
	lluviaEfectivaPorAcumulacion = 0.2 * 60 / dataSamplingTime # Si llueve en algo en una hora y también la siguiente, aunque sea poca cantidad, se considera efectiva
	lluviaEfectivaPorAcumulacion2HorasPrevias = 0.4  # Si llueve en esta hora y ha llovido más de 0.4 en las dos horas anteriores, se considera efectiva
		
	for(posicion in 1:length(PAcum))
	{
		lluviaEfectiva[posicion] = FALSE
			
		if(!is.na(PAcum[posicion]) & posicion < length(PAcum)) # si no es NA | En la última posición no se hace el cálculo
		{
			if(PAcum[posicion] >= lluviaEfectivaHoraria)  
			{# Si llueve más de un litro cada media hora = 2 litros a la hora, la lluvia se considera efectiva
				lluviaEfectiva[posicion] = TRUE
				next
			}
			if(PAcum[posicion] > lluviaEfectivaPorAcumulacion & sum(PAcum[posicion + 1] > lluviaEfectivaPorAcumulacion)) 
			{# Si llueve más de un litro cada media hora = 2 litros a la hora, la lluvia se considera efectiva
				lluviaEfectiva[posicion] = TRUE
				next
			}
			if(posicion > 2) # No se puede ejecutar este análisis en las dos primeras posiciones del vector de lluvias
			{# Si llueve en esta hora y ha llovido más de 0.4 en las dos horas anteriores, se considera efectiva
				if( PAcum[posicion] > 0 & (PAcum[posicion - 1] +  PAcum[posicion - 2]) > 0.4) 
				{
					lluviaEfectiva[posicion] = TRUE
					next
				}
			}
		}
	}
	
	return(lluviaEfectiva)
}

# Función que devuelve un vector de valores lógicos TRUE/FALSE que indican si la temperatura registrada se considera efectiva o no.
hayTemperaturaEfectiva <- function(temperatura, umbralTempMinima, umbralTempMaxima)
{
	hayTemperaturaEfectiva <- ifelse(temperatura >= umbralTempMinima & temperatura < umbralTempMaxima, TRUE, FALSE) 
	return(hayTemperaturaEfectiva)
}

# Función que devuelve un vector de valores lógicos TRUE/FALSE que indican si la temperatura registrada está entre 10 y 13ºC
estaTemperaturaEntre10y13Grados <- function(temperatura)
{
	hayTemperaturaEfectiva <- ifelse(temperatura >= 10 & temperatura < 13 , TRUE, FALSE) 
	return(hayTemperaturaEfectiva)
}

# Función para obtener un objeto (data.frame) con los resultados de las fechas que cumplen infección para ello se consideran los siguientes parámetros:
# umbralTempMinima ==> mínima temperatura necesaria para que se considere a la espora activa para infectar (13 ºC según bibliografía)
# umbralTempMaxima ==> máxima temperatura que la espora permite para mantener su poder infetivo (35 ºC según bibliografía)
# horasAcumuladasParaAparicionInfeccion  => horas necesarias para que la germinación sea satisfactoria y la infección se produzca
calculaMomentosInfeccion <-function(fecha, temperatura, humectacion, umbralHumectacion, umbralTempMinima = 13, umbralTempMaxima = 35, horasAcumuladasParaAparicionInfeccion = 8){
	res <- data.frame(fecha, temperatura, humectacion, stringsAsFactors = FALSE)
	
	res$hayHumectacion = ifelse(humectacion > umbralHumectacion, TRUE, FALSE)
	res$hayTemperaturaOptima = ifelse(temperatura >= umbralTempMinima & temperatura <= umbralTempMaxima, TRUE, FALSE)
	
	res$horasAcumuladas[1] = if(res$hayHumectacion[1] & res$hayTemperaturaOptima[1]) 1 else 0
	for(posicion in 2:nrow(res)){ # Si se dan condiciones de Humectación y temperatura en esta variable almaceno, para cada registro, el número de horas de condiciones apropiadas
		res$horasAcumuladas[posicion] = if(res$hayHumectacion[posicion] & res$hayTemperaturaOptima[posicion]) 1 + res$horasAcumuladas[posicion-1] else 0
	}
	
	fechasInfeccion = res$fecha[which(res$horasAcumuladas >= horasAcumuladasParaAparicionInfeccion)]
	
	
	calculaDiasHastaInfeccion <- function(tempMediaPosterior){ # esta funcion ofrece valores de 1 para T = 30 y 20 para T = 10, de forma  lineal
		if(tempMediaPosterior > 30) tempMediaPosterior = 30
		if(tempMediaPosterior < 10) tempMediaPosterior = NA
		tempMediaPosterior <- round(tempMediaPosterior, 0)
		diasHastaInfeccion = 30 - tempMediaPosterior + 1
		return(diasHastaInfeccion)
	}
	#calculaDiasHastaInfeccion(32); calculaDiasHastaInfeccion(30); calculaDiasHastaInfeccion(20); calculaDiasHastaInfeccion(10); calculaDiasHastaInfeccion(9)
	
	tempMediaPosterior = numeric()
	diasHastaInfeccion = integer()
	for(fecha in fechasInfeccion){
		temperaturasPosterioresInfeccion <- res$temperatura[res$fecha > fecha]
		
		if(length(temperaturasPosterioresInfeccion) > 20*24) temperaturasPosterioresInfeccion = temperaturasPosterioresInfeccion[1:20*24] # Eliminamos más allá de 20 días de datos
		tempMediaPosterior = c(tempMediaPosterior, mean(temperaturasPosterioresInfeccion))
		diasHastaInfeccion = if(length(temperaturasPosterioresInfeccion) > 0) calculaDiasHastaInfeccion(last(tempMediaPosterior)) else NA
	}
	
	return(data.frame(fechasInfeccion, tempMediaPosterior, diasHastaInfeccion))	
}



######################## FUNCION PRINCIPAL QUE REALIZA TODO EL CONTROL DEL PROCESO DE CÁLCULO DE VALORES DIV LLAMANDO A LAS DISTINTAS FUNCIONES AUXILIARES Y GENERANDO UN OBJETO DE RESPUESTA CON TODA LA INFORMACIÓN OBTENIDA ########################
calculaRiesgoDIV_V20 <- function(estacion, fechaInicio, fechaFin,
									matrizDIV, vectorDIVHorario,
									horaInicio = "10", offsetHR = 0,
									sensorHumect = "THumecta1",
									origenDato = "SWClima",
									nombreEstacion = NA,
									...)
{

# horaInicio = "10", hora solar a la que se inician los cálculos
# offsetHR = 0 por defecto se calcula el modelo con umbrales de HR de 85 y 90. offsetHR permite modifificar los umbrales, por ejemplo offset = - 5 haría los cálculosa 75, 80 y 85% de umbral
# sensorHumect = "THumecta1", solo aplicable a datos tipo "SWClima"; es previsible que se elimine, en la actualidad se extraen los dos sensores de humectacion del SIAR, el que se especifica queda marcado como el sensor 1 en el proceso, es un error lógico
# origenDato = "SWClima", "CESENS", etc

	# EXTRAER DATOS CLIMÁTICOS
	res <- extraeDatosClima(estacion, origenDato, fechaInicio, fechaFin)
	
	res <- fromJSON(res)
	
	attach(res)

	# Formatear variables
	datosClimaticos <- as.data.frame(datosClimaticos)
	datosClimaticos$fecha <- as.POSIXct(datosClimaticos$fecha, origin = "1970-01-01")
		
	if(!is.na(nombreEstacion)) nombreEstacionCalculada = nombreEstacion # se da prioridad a lo que traiga esta variable, CESENS no tiene una buena trazabilidad de los nombres ligados a los códigos de estaciones
	
	# Calcular el DIV para cada día entre fechaInicio y fechaFin
	# Obtener secuencia de los días a calcular
	# por convenio en modelos nunca empezamos el día en las 00:00 sino en una hora que agrícolamente tenga sentido, generalmente entre las 8 y las 10 UTC (Hora solar en España)
	diasCalculo <- seq.POSIXt(from = as.POSIXct(strftime(paste0(fechaInicio, " ", horaInicio, ":00"), tz = "UTC")),
						to = as.POSIXct(strftime(paste0(fechaFin, " ", horaInicio, ":00"), tz = "UTC")),
						by = "day")
						
	diasCalculo <- diasCalculo[-length(diasCalculo)] # 	quitamos el último día
	
	diasCalculo <- diasCalculo[diasCalculo > first(datosClimaticos$fecha)] # y también quitamos los días donde no haya datos procedentes de la estación
	
	DIV_AMERICANO <- calculaRiesgoDIV_MODELO_AMERICANO(datosClimaticos, diasCalculo, estacion, matrizDIV, horaInicio, offsetHR, dataSamplingTime, valorHumectacionMinimo)

	DIV_HORARIO <- calculaRiesgoDIV_MODELO_RIOJA(datosClimaticos, diasCalculo, estacion, vectorDIVHorario, offsetHR, dataSamplingTime, valorHumectacionMinimo)

	DIV <- DIV_HORARIO$DIV
	DIV_Horario <- DIV_HORARIO$DIV_Horario
	
	
	datosClimaticos$lluviaEfectiva <- esLluviaEfectiva(datosClimaticos$PAcum)
	datosClimaticos$temperaturaEfectiva <- hayTemperaturaEfectiva(datosClimaticos$temperatura, umbralTempMinima = 13, umbralTempMaxima = 35)
	datosClimaticos$temperaturaEntre10y13 <- estaTemperaturaEntre10y13Grados(datosClimaticos$temperatura)
	
	
	# Recortar datos climáticos
	datosClimaticos <- datosClimaticos[datosClimaticos$fecha > DIV$fecha[1]-24*3600,]
	datosClimaticos <- datosClimaticos[datosClimaticos$fecha <= last(DIV$fecha),]
	
	
	DIV_Horario <- data.frame(fecha = datosClimaticos$fecha,
							estacion = rep(estacion, length(datosClimaticos$fecha)),
							##################### 
							DIV_Horario_HumectUmbral = DIV_Horario$DIV_Horario_HumectUmbral,
							DIV_HumectUmbral.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HumectUmbral, datosClimaticos$temperatura,
																			datosClimaticos$humectacion, umbralHumectacion = valorHumectacionMinimo,
																			offsetHR = offsetHR, numDatos = 2 * 24,
																			numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HumectUmbral.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HumectUmbral, datosClimaticos$temperatura,
																			datosClimaticos$humectacion, umbralHumectacion = valorHumectacionMinimo,
																			offsetHR = offsetHR, numDatos = 7 * 24,
																			numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HumectUmbral.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HumectUmbral, datosClimaticos$temperatura,
																			datosClimaticos$humectacion, umbralHumectacion = valorHumectacionMinimo,
																			offsetHR = offsetHR, numDatos = 14 * 24,
																			numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HumectUmbral.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HumectUmbral, datosClimaticos$temperatura,
																			datosClimaticos$humectacion, umbralHumectacion = valorHumectacionMinimo,
																			offsetHR = offsetHR, numDatos = 21 * 24,
																			numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HumectUmbral.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HumectUmbral, datosClimaticos$temperatura,
																			datosClimaticos$humectacion, umbralHumectacion = valorHumectacionMinimo,
																			offsetHR = offsetHR, numDatos = 28 * 24,
																			numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR80 = DIV_Horario$DIV_Horario_HR80,
							DIV_HR80.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR80, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 80, offsetHR = offsetHR, 
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR80.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR80, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 80, offsetHR = offsetHR, 
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR80.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR80, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 80, offsetHR = offsetHR,
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR80.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR80, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 80, offsetHR = offsetHR,
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR80.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR80, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 80, offsetHR = offsetHR,
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR82.5 = DIV_Horario$DIV_Horario_HR82.5,
							DIV_HR82.5.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR82.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 82.5, offsetHR = offsetHR, 
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR82.5.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR82.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 82.5, offsetHR = offsetHR, 
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR82.5.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR82.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 82.5, offsetHR = offsetHR,
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR82.5.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR82.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 82.5, offsetHR = offsetHR,
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR82.5.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR82.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 82.5, offsetHR = offsetHR,
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR85 = DIV_Horario$DIV_Horario_HR85,
							DIV_HR85.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR85, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 85, offsetHR = offsetHR, 
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR85.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR85, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 85, offsetHR = offsetHR, 
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR85.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR85, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 85, offsetHR = offsetHR,
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR85.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR85, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 85, offsetHR = offsetHR,
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR85.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR85, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 85, offsetHR = offsetHR,
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR87. = DIV_Horario$DIV_Horario_HR87.5,
							DIV_HR87.5.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR87.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 87.5, offsetHR = offsetHR,
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR87.5.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR87.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 87.5, offsetHR = offsetHR,
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR87.5.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR87.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 87.5, offsetHR = offsetHR,
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR87.5.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR87.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 87.5, offsetHR = offsetHR,
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR87.5.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR87.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 87.5, offsetHR = offsetHR,
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR90 = DIV_Horario$DIV_Horario_HR90,
							DIV_HR90.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR90, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 90, offsetHR = offsetHR,
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR90.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR90, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 90, offsetHR = offsetHR,
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR90.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR90, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 90, offsetHR = offsetHR, 
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR90.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR90, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 90, offsetHR = offsetHR, 
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR90.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR90, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 90, offsetHR = offsetHR, 
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							##################### 
							DIV_Horario_HR92.5 = DIV_Horario$DIV_Horario_HR92.5,
							DIV_HR92.5.2dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR92.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 92.5, offsetHR = offsetHR,
																	numDatos = 2 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR92.5.7dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR92.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 92.5, offsetHR = offsetHR,
																	numDatos = 7 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR92.5.14dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR92.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 92.5, offsetHR = offsetHR, 
																	numDatos = 14 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR92.5.21dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR92.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 92.5, offsetHR = offsetHR, 
																	numDatos = 21 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime),
							DIV_HR92.5.28dias = calculaDIVAcumHorario(DIV_Horario$DIV_Horario_HR92.5, datosClimaticos$temperatura,
																	datosClimaticos$HR, umbralHumectacion = 92.5, offsetHR = offsetHR, 
																	numDatos = 28 * 24, numHorasReseteo = NA, dataSamplingTime = dataSamplingTime)
							)
	
	fechasInfeccionHR95.8horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura,
								datosClimaticos$HR, umbralHumectacion = 95, horasAcumuladasParaAparicionInfeccion = 8) 
	fechasInfeccionHR95.6horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura,
								datosClimaticos$HR, umbralHumectacion = 95, horasAcumuladasParaAparicionInfeccion = 6) 
	fechasInfeccionHR90.8horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura,
								datosClimaticos$HR, umbralHumectacion = 90, horasAcumuladasParaAparicionInfeccion = 8)
	fechasInfeccionHR90.6horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura,
								datosClimaticos$HR, umbralHumectacion = 90, horasAcumuladasParaAparicionInfeccion = 6)
	
	fechasInfeccionHumectacion.8horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura , datosClimaticos$humectacion, umbralHumectacion = 50)
	fechasInfeccionHumectacion.6horas <- calculaMomentosInfeccion(datosClimaticos$fecha, datosClimaticos$temperatura , datosClimaticos$humectacion, umbralHumectacion = 50, horasAcumuladasParaAparicionInfeccion = 6)
	
	
	
	variablesConfiguracion <- data.frame(valorDIV = 1, nombre = 'Sin Riesgo',  color = "khaki", stringsAsFactors = FALSE) 
	variablesConfiguracion <- rbind(variablesConfiguracion, list(valorDIV = 2, nombre = 'Inicio Riesgo',  color =  "greenyellow")) 
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = 3, nombre = 'Riesgo Leve',  color = "yellowgreen")) 
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = 4, nombre = 'Riesgo Medio',  color = "orange"))
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = 5, nombre = 'Riesgo Moderado',  color = "sienna1"))
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = 6, nombre = 'Riesgo Alto',  color = "tomato1"))
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = NA, nombre = 'Riesgo muy Alto',  color = "red1"))
	variablesConfiguracion <- rbind(variablesConfiguracion,	list(valorDIV = 7, nombre = 'Infección segura',  color = "red3"))
								
	row.names(variablesConfiguracion) = c('DIV.SinRiesgo', 'DIV1.InicioRiesgo', 'DIV.RiesgoLeve', 'DIV.RiesgoMedio',
										'DIV.RiesgoModerado', 'DIV.RiesgoAlto', 'DIV.RiesgoMuyAlto', 'DIV.InfeccionSegura')
	
	
	# Generar objeto respuesta
	objetoRespuesta = list(datosClimaticos = datosClimaticos[datosClimaticos$fecha > diasCalculo[1] & datosClimaticos$fecha <= last(diasCalculo) + 3600*24,],
				DIV = DIV,
				DIV_AMERICANO = DIV_AMERICANO,
				DIV_Horario = DIV_Horario,
				fechasInfeccion = list(HR95.8horas = fechasInfeccionHR95.8horas, HR95.6horas = fechasInfeccionHR95.6horas,
									HR90.8horas = 	fechasInfeccionHR90.8horas, HR90.6horas = 	fechasInfeccionHR90.6horas,
									humectacion.8horas = fechasInfeccionHumectacion.8horas, humectacion.6horas = fechasInfeccionHumectacion.6horas),
				variablesConfiguracion = variablesConfiguracion,
				fechaUltimoDato = tail(DIV_Horario$fecha, 1),
				origenDato = origenDato,
				estacion = estacion,
				nombreEstacion = nombreEstacionCalculada)
	
	detach(res)
	
	return(objetoRespuesta)
}


# Test
# 
# estacion = 505
# fechaInicio = as.POSIXct("2018-06-01", tz = "UTC")
# fechaFin =  as.POSIXct("2018-10-01", tz = "UTC") #Sys.time()
 # matrizDIV <- read.csv("Matriz_DIV.csv", row.names = 1)
 # vectorDIVHorario <- read.csv("Vector_DIVHorario.csv")
# DIV505_2018 <- calculaRiesgoDIV_V20(estacion, fechaInicio, fechaFin, matrizDIV, vectorDIVHorario)
 # head(DIV505_2018[,c(1,19, 21)])
  # tail(DIV505_2018[,c(1,19, 21)])
  # summary(DIV505_2018[,c(1,19, 21)])


	
						
