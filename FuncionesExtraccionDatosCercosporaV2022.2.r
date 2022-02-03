extraeDatosClima <- function(estacion, origenDato, fechaInicio,	fechaFin)
{
	if(origenDato == "SWClima")
	{
		res <- get_SIAR_DatosCercospora(estacion, fechaInicio, fechaFin) # Usa la API interna del SIAR, no se puede exportar
	
		if(res$success)
		{
			datosClimaticos <- res$datos
		}else
		{
			stop(paste("Error al recoger datos de", origenDato, ", estacion:", estacion))
		}
	
		dataSamplingTime = 30 # dataSamplingTime = variable que indica con qué frecuencia se hacen medidass, en minutos, si la estacion mide en horarios= 60, quinceminutales = 15
		unidadesHumectacion = "minutos" # unidadesHumectacion = la unidad en la que se mide la humectación, "minutos" o "porcentaje"
		valorHumectacionMinimo = 5 # valorHumectacionMinimo = valor que se usará en los análisis del modelo para considerar que el registro es húmedo 1/6 del dataSamplingTime
		nombreEstacionCalculada = get_SIAR_nombreEstacion(estacion) # Usa la API interna del SIAR, no se puede exportar
		
	}else if(origenDato == "CESENS")
	{
		res <- get_CESENS_DatosEnfermedades(as.numeric(estacion), fechaInicio, fechaFin) #!!! OJO se da por hecho que el objeto tokenCESENS existe en memoria, tokenCESENS hay que generarlo usando USER/PASSWORD
		
		if(res$success)
		{
			res <- res$datos
			datosClimaticos <- data.frame(fecha = as.POSIXct(res$fecha),
										temperatura = res$TAirMd,
										humectacion = res$THumectacion,
										HR = res$HRAirMd,
										PAcum = res$PAcum, ######################## EDITABLE
										stringsAsFactors = FALSE)
		}else
		{
			stop(paste("Error al recoger datos de", origenDato, ", estacion:", estacion))
		}
		
		if(!exists("dataSamplingTime")){ #!!! ¿Qué sentido tiene esto?
			dataSamplingTime = 60 # damos por hecho que el usuario no envía esta variable en ... entonces al llegar aquí consideramos este valor
		}
		unidadesHumectacion = "minutos"
		valorHumectacionMinimo = 9
			#Si se usan los datos quinceminutales de CESENS
			#unidadesHumectacion = "porcentaje"
			#valorHumectacionMinimo = 15
			# Si se usan los datos diezminutales de CESENS
			#unidadesHumectacion = "minutos"
			#valorHumectacionMinimo = 10
			# El PROBLEMA DE HACERLO DE LA SEGUNDA FORMA ES QUE HAY ESTACIONES DE CESENS CON SAMPLING_TIME VARIABLE Y, ADEMÁS NECESITARÍA CONFIGURAR MÁS LAS LLAMADAS A LAS FUNCIONES PARA CONFIGURAR ESTOS PARÁMETROS, get_SamplingTimeCESENS, getWetnessUnitCESENS
			nombreEstacionCalculada = get_stationName_CESENS(estacion)
			
	}else
	{
		res <- get_SIARNacional_DatosSemiHorarios(Estacion = estacion, FechaInicial = fechaInicio, FechaFinal = fechaFin)
		if(is.null(res$datosBrutos$MensajeRespuesta))
		{
			res <- res$datos
			datosClimaticos <- data.frame(fecha = as.POSIXct(res$Fecha),
										temperatura = res$TempMedia,
										humectacion = rep(0, nrow(res)),
										HR = res$HumedadMedia,
										PAcum = res$Precipitacion,
										stringsAsFactors = FALSE)
		}else
		{
			stop(paste("Error al recoger datos de", origenDato, ", estacion:", estacion))
		}
		
		datosClimaticos$temperatura <- sustituyeNAsXMedia(datosClimaticos$temperatura)
		datosClimaticos$HR <- sustituyeNAsXMedia(datosClimaticos$HR)
		datosClimaticos$PAcum <- sustituyeNAsXMedia(datosClimaticos$PAcum)
		
		dataSamplingTime = 30 # dataSamplingTime = 30, en minutos, si la estacion mide en horarios= 60, quinceminutales = 15
		unidadesHumectacion = NA #"minutos"
		valorHumectacionMinimo = NA #5
		nombreEstacionCalculada = get_SIARNacional_stationName(estacion)
	

	}
	
	return(toJSON(list(datosClimaticos = datosClimaticos,
						dataSamplingTime = dataSamplingTime,
						valorHumectacionMinimo = valorHumectacionMinimo,
						nombreEstacionCalculada = nombreEstacionCalculada)))
}


# PEGAR ESTA FUNCION EN LA API DEL SIAR
get_SIAR_DatosCercospora <- function(estacion, fechaInicio, fechaFin, sensorHumect = "THumecta1")
{
	if(estacion > 200) # Estaciones de la red general o geoestadísticas, completas
	{
		res <- extraeDatosSemiHorariosValidados(estacion,
												fechaInicio,
												fechaFin,
												parametro = c("TAirMd", "HRAirMd", "THumecta1", "THumecta2", "Pluv"), # en versiones anteriores se enviaba la variable sensorHumect: parametro = c("TAirMd","HRAirMd", sensorHumect, "Pluv")
												devolverFecha = TRUE)
		res$datos$PAcum = res$datos$Pluv # PAcum es más fácil de entender que Pluv !!! 
	}else # Estaciones específicas de enfermedades, sólo unos pocos sensores, no tienen pluviómetro
	{
		res <- extraeDatosSemiHorariosValidados(estacion,
												fechaInicio,
												fechaFin,
												parametro = c("TAirMd","HRAirMd", "THumecta1", "THumecta2"), # en versiones anteriores se enviaba la variable sensorHumect: parametro = c("TAirMd","HRAirMd", sensorHumect),
												devolverFecha = TRUE)
		if(nrow(res$datos) > 0)
		{
			res$datos$PAcum = NA
		}else
		{
			res$datos$PAcum = numeric()
		}
	}
	
	# Se eliminan valores con problemas, datos perdidos, series incompletas.
	res$datos$TAirMd <- sustituyeNAsXMedia(res$datos$TAirMd, extremos = TRUE, valorPorDefecto = 0) # 
	res$datos$HRAirMd <- sustituyeNAsXMedia(res$datos$HRAirMd, extremos = TRUE, valorPorDefecto = 0)
	res$datos$THumecta1 <- sustituyeNAsXMedia(res$datos$THumecta1, extremos = TRUE, valorPorDefecto = 0)
	res$datos$THumecta2 <- sustituyeNAsXMedia(res$datos$THumecta2, extremos = TRUE, valorPorDefecto = 0)
	
	return(list(datos = data.frame(fecha = as.POSIXct(res$fecha),
									temperatura = res$datos$TAirMd,
									humectacion = if(sensorHumect == "THumecta1") res$datos$THumecta1 else res$datos$THumecta2,
									humectacion_sensor2 = if(sensorHumect == "THumecta1") res$datos$THumecta2 else res$datos$THumecta1,
									HR = res$datos$HRAirMd,
									PAcum = res$datos$PAcum, ######################## EDITABLE
									stringsAsFactors = FALSE),
				success =  !res$noHayDatos))
}

# #Test
# res <- get_SIAR_DatosCercospora(501, Sys.Date()-1, Sys.time())
# str(res, 1)
# res <- get_SIAR_DatosCercospora(101, Sys.Date()-1, Sys.time())
# str(res, 1)