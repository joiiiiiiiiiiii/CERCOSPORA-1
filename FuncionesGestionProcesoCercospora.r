procesaEstacion <- function(estacion, fechaInicio, fechaFin,
								matrizDIV, vectorDIVHorario, horaInicio, origenDato,
								nombreEstacion = NA, # Si se da un valor a esta variable el nombre que aparece en el gráfico y en el nombre de los archivos incluye el valor de esta variable, sino lo obtiene automáticamente de la BBDD o API
								guardarCSV = FALSE, # Si está en FALSE sí que se guarda un archivo .csv con el resumen diario del resumen horario, si está en TRUE se guarda un .csv para los resultados diarios del modelo americano y del modelo horario, también se puede forzar en el código para que se guarden los resultados del modelo horario.
								unSoloPDF = TRUE, salidaAzucareraAB = TRUE,
								campania = NA, # si se aporta la campañia (un string) se añade a los nombres de los archivos de gráficos y .csv
								salidaGrafica = TRUE,  # Si es false sólo se calcula el modelo pero no se pintan los gráficos
								offsetHR = 0) # Se usó durante el proyecto para hacer pruebas con distintos umbrales de HR
{
	resultado <- calculaRiesgoDIV_V20(estacion = estacion, fechaInicio = fechaInicio, fechaFin,
										matrizDIV, vectorDIVHorario, horaInicio = horaInicio, offsetHR = offsetHR, origenDato = origenDato, nombreEstacion = nombreEstacion) #-> a
	
	
	mensajeLog = paste('DIVs de la estación', resultado$nombreEstacion, 'codigo', estacion, 'de', origenDato, 'calculados')
	print(mensajeLog)
		
	textoDIVs = paste0('RESULTADO MODELO PARA ESTACIÓN ', resultado$nombreEstacion, ":",
					'\n\t\t\tactualizado a las ', horaInicio, ' hora solar, ultimo dato disponible: ', strftime(resultado$fechaUltimoDato, '%H:%M'),
					if(as.Date(resultado$fechaUltimoDato) < Sys.Date()) paste(' ATENCION: DATOS NO ACTUALIZADOS, ÚLTIMA FECHA DISPONIBLE:', strftime(resultado$fechaUltimoDato, '%d/%m/%y')),
					"\n\n\tDIVacumulado USANDO HR > 90%:    ", as.character(round(sum(tail(resultado$DIV$DIV_HR90, 2)), 2)),
					"\n\tDIVacumulado USANDO HR > 85%:    ", as.character(round(sum(tail(resultado$DIV$DIV_HR85, 2)), 2)),
					"\n\tDIVacumulado USANDO HR > 80%:    ", as.character(round(sum(tail(resultado$DIV$DIV_HR80, 2)), 2)),
					"\n\n\tDIVacumulado USANDO HUMECTACIÓN: ", as.character(round(sum(tail(resultado$DIV$DIV_HumectUmbral, 2)), 2)))

	
	# GENERACIÓN GRÁFICOS
	archivos <- character()
	archivosBasicos <- character()
	nombreEstacion = resultado$nombreEstacion
	
	if(salidaGrafica)
	{
		archivo <- pintaGraficaRiesgoDIV_V17(resultado, etiquetasEjeX = etiquetasEjeX)
		archivos <- c(archivos, archivo) # Esta variable guardará todos los archivos que se generen en la función y se enviarán a expertos
		archivosBasicos = pintaGraficaRiesgoDIV_V17(resultado, etiquetasEjeX = etiquetasEjeX, pintaGraficaTHR = FALSE) # Esta variable guardará los archivos que se enviarán a todo el mundo
		
		archivo <- pintaGraficaRiesgoDIV_V17(resultado, etiquetasEjeX = etiquetasEjeX, pintaDIV_AMERICANO = TRUE, pintaGraficaTHR = FALSE)
		archivos <- c(archivos, archivo) # Esta variable guardará todos los archivos que se generen en la función
		archivosBasicos <- c(archivosBasicos, archivo)
	
			
		#
		if(nrow(resultado$DIV_Horario) > 5*24)
		{
			resultadoGrafico <- resultado$DIV_Horario[(5*24):nrow(resultado$DIV_Horario),] # PAra dibujarlos les quito los 5 primeros días que no hay valores de DIV acumulado7d
			datosClimaticos <- resultado$datosClimaticos[(5*24):nrow(resultado$datosClimaticos),] # Para dibujarlos les quito los 5 primeros días que no hay valores de DIV acumulado7d

			archivo <- pintaGraficoDIVAcumulados(resultadoGrafico, offsetHR = offsetHR, nombreEstacion = resultado$nombreEstacion,
												nombreArchivo = paste0(nombreEstacion, '-GraficoDIVAcumulados-1', '.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, graficosADibujar = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE), tituloUnSoloPDF = !unSoloPDF)
			archivos <- c(archivos, archivo)
			
			archivo <- pintaGraficoDIVAcumulados(resultadoGrafico, offsetHR = offsetHR, nombreEstacion = resultado$nombreEstacion,
												nombreArchivo = paste0(nombreEstacion, '-GraficoDIVAcumulados-2', '.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, graficosADibujar = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), tituloUnSoloPDF = !unSoloPDF)
			archivos <- c(archivos, archivo)		
		
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 80, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR80_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF) 
			archivos <- c(archivos, archivo)
				
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 82.5, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR82.5_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF) 
			archivos <- c(archivos, archivo)
				
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 85, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR85_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF)
			archivos <- c(archivos, archivo)
		
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 87.5, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR87.5_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF) 
			archivos <- c(archivos, archivo)	
		
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 90, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR90_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF) 
			archivos <- c(archivos, archivo)
		
			archivo <- pintaGraficoVisualizacionComparativaHR(resultadoGrafico, datosClimaticos, offsetHR = offsetHR, resultado = resultado,
												tipoHR = 92.5, nombreArchivo = paste0(nombreEstacion, '-Riesgo_y_Clima_HR92.5_V1.pdf'), tipografico = "pdf",
												incluirTitulo = TRUE, tituloUnSoloPDF = !unSoloPDF)  
			archivos <- c(archivos, archivo)
		}
		
		# UNIR TODOS LOS GRÁFICOS EN UNO SOLO
		 #!!! NO SE USA PORQUE LA FUNCIÓN DA UN PROBLEMA con solución no trivial descrita aquí: https://github.com/pridiltal/staplr/issues/45
		if(unSoloPDF)
		{
			nombreArchivoInforme = paste0(strftime(fechaFin, '%Y-%m-%d-'), nombreEstacion, if(!is.na(campania)) paste0('-Campaña-', campania) else '', '-InformeCercosporaV', numeroVersion, '.pdf')
			output_file <- file.path(getwd(), nombreArchivoInforme)
			input_files <- file.path(getwd(), archivos)
			staple_pdf(input_directory = NULL, input_files = input_files, output_filepath = output_file, overwrite = TRUE)
			archivos <- nombreArchivoInforme # Reseteamos la variable!
		}
		if(salidaAzucareraAB)
		{
			nombreArchivoInforme = paste0(strftime(fechaFin, '%Y-%m-%d-'), nombreEstacion, if(!is.na(campania)) paste0('-Campaña-', campania) else '', '-InformeCercospora-', strftime(Sys.Date(), "%d-%b-%Y"), '.pdf')
			output_file <- file.path(getwd(), nombreArchivoInforme)
			input_files <- file.path(getwd(), archivosBasicos)
			staple_pdf(input_directory = NULL, input_files = input_files, output_filepath = output_file, overwrite = TRUE)
			archivosBasicos <- nombreArchivoInforme # Reseteamos la variable!
		}
	}
	
	# GENERAR ARCHIVOS CSV
	if(guardarCSV)
	{
		if(TRUE)
		{	archivo <- paste0(paste(strftime(fechaFin, '%Y-%m-%d'),	'DatosHorariosDIVyClima', 'modeloHorario', nombreEstacion, origenDato, sep = '-'),
								if(!is.na(campania)) paste0('-Campaña-', campania) else '', '.csv')
			write.csv2(cbind(resultado$DIV_Horario[,c(1:3, 9, 15, 21, 27, 33, 39)], resultado$datosClimaticos[,2:5]), archivo, row.names = F)
		}
		archivos <- c(archivos, archivo)
		archivo <- paste0(paste(strftime(fechaFin, '%Y-%m-%d'), 'DatosDiariosDIV', 'modeloAmericano', nombreEstacion, origenDato, sep = '-'),
							if(!is.na(campania)) paste0('-Campaña-', campania) else '', '.csv')
			write.csv2(resultado$DIV_AMERICANO, archivo, row.names = F)
		archivos <- c(archivos, archivo)
		archivo <- paste0(paste(strftime(fechaFin, '%Y-%m-%d'), 'DatosDiariosDIV', 'modeloHorario', nombreEstacion, origenDato, sep = '-'),  # Es el mismo archivo que el enviado por e-mail (ver else)
							if(!is.na(campania)) paste0('-Campaña-', campania) else '', '.csv')
			write.csv2(resultado$DIV, archivo, row.names = F)
		archivos <- c(archivos, archivo)
	}else
	{
		if(TRUE)
		{	archivo <- paste0(paste(strftime(fechaFin, '%Y-%m-%d'),	'DatosDIVAcumulado', 'modeloHorario', nombreEstacion, origenDato, sep = '-'), '.csv')
								#if(!is.na(campania)) paste0('-Campaña-', campania) else '',
			write.csv2(resultado$DIV_Horario[strftime(resultado$DIV_Horario$fecha, '%H:%M') == '10:00', c(1:2, 5:7, 23:25)], archivo, row.names = F)
		}
		archivos <- c(archivos, archivo)
		nombreArchivoCSV <- paste0(paste(strftime(fechaFin, '%Y-%m-%d'), 'DatosDIV', 'modeloHorario', nombreEstacion, sep = '-'), '.csv') #origenDato, nombreEstacion, sep = '-'), '.csv')
		write.csv2(resultado$DIV, nombreArchivoCSV, row.names = FALSE)
		archivos <- c(archivos, nombreArchivoCSV)
		#archivosBasicos = c(archivosBasicos, nombreArchivoCSV)
	}
	
	
	#Encoding(textoDIVs) <- 'UTF-8'
	
	return(list(resultado = if(DEBUG) resultado else '', archivos = archivos, archivosBasicos = archivosBasicos, textoDIVs = textoDIVs, alertaRiesgo = if(sum(tail(resultado$DIV$DIV_HR85, 2)) > 2) TRUE else FALSE))
}



enviaEmail <- function(destinatariosEmail, archivos, zona = '', descripcion = '', bodyEmail = '', mensajeExtra = '', fechaFin = Sys.Date())
{
	
	
	subject = paste('Modelo Cercospora, GRAFICOS AIMCRA,', zona, ' actualizados a', strftime(fechaFin, '%d/%m/%y'))

	bodyEmail = paste0(bodyEmail, 'RESULTADO MODELO PARA ESTACIONES ', zona, ': ', descripcion, '\n\n') #,
	
	if(Sys.Date() == as.Date("2020-04-22"))
	{
		mensajeExtra = paste0("Buenas tardes/noches, esta es una prueba de envío de correos con los nuevos gráficos con las novedades que comentamos en la reunión de la pasada semana.\n\n",
							"Se adjuntan dos archivos:\n\t -Uno con las gráficas de DIV_Horario acumulado a 7 días (verde), 14 días (azul) y 21 días (rojo). ",
							"Las gráficas se calculan según diferentes condiciones de humedad (90% y 85%) y humectación\n\n",
							"\t -Otro con la gráfica de DIV Acumulado para HR > 85% y una serie de gráficos adicionales con datos climáticos. ",
							"Además, en la parte inferior aparece un gráfico de \"Condiciones de Infección\", que se calculan siguiendo distintos métodos; ",
							"en ese gráfico, en general, cuanto más roja o ancha sea la barra dibujada => más peligro existe\n\n",
							"Los gráficos enviados son provisionales, cualquier mejora que creáis necesaria hacednoala llegar a siar.cida@larioja.org\n\nUn saludo\n\n")
		
	}
	
	if(Sys.Date() == as.Date("2020-04-24"))
	{
		mensajeExtra = paste0("Buenas tardes\n\n",
							"Desde hoy adjuntamos los datos de las dos estaciones. Para la estación 2789 (Riego a pie), ",
							"las curvas de DIV acumulado a 14 y 21 días, irán apareciendo según vayan avanzando los días y haya ",
							"suficiente información para calcularla\n\n",
							"Tenemos pendientes algunas mejoras en los gráficos, como indicábamos el otro día estos ",
							"son provisionales, cualquier mejora que creáis necesaria hacednosla llegar a siar.cida@larioja.org\n\nUn saludo\n\n")
		
	}
	
	if(Sys.Date() == as.Date("2020-05-07"))
	{
		mensajeExtra = paste0("Buenos días\n\n",
							"Desde hoy adjuntamos los gráficos de acuerdo a una nueva especificación",
							"\n\n",
							"Como siempre cualquier mejora que creáis necesaria hacednosla llegar a siar.cida@larioja.org\n\nUn saludo\n\n")
		
	}
	
	if(Sys.Date() == as.Date("2020-06-07"))
	{
		mensajeExtra = paste0("Buenas tardes\n\n",
							"\tDesde hoy adjuntamos los gráficos de acuerdo a una nueva especificación. También comienzan a enviarse los correos con la información calculada para las estaciones CESENS en Castilla León ",
							'Los correos se enviarán todos los días a las 11:30 y en los próximos días se añadirán las dos estaciones que hemos instalado aquí en La Rioja.', 
							"\n\n",
							'\tAlgunos gráficos gráficos no tienen leyenda, nos queda pendiente, los que recibís estos datos por primera vez',
							'\n\n\tDespués de lo hablado sobre los cálculos con Manuel nos queda claro que sólo las HR > 85% son relevantes, por ello el modelo se calcula, a partir de ahora, ',
							'con los umbrales de humedad relativa de 85, 87.5, 90 y 92.5%.\n',
							'\tEn los gráficos de DIVs acumulados a 7, 14 y 21 días aparece ahora el acumulado de 2 días (en color dorado).\n\n',
							'\tUn tema pendiente es la pensar la posibilidad de representar los valores de DIV acumulados a 7, 14 y 21 días en el mismo gráfico donde ',
							'se representan actualmente el valor del DIV diario usando un código de color. En ese gráfico aparecen distintos DIVs calculados usando el sensor de humectación o bien distintos valores de HR. ',
							'Por un lado hay un problema de formato ya que los datos actuales representados en ese gráfico son diarios y estos otros son horarios, no parece complejo a priori; ',
							' por otro ese cuadro es un gráfico muy visual, pero es mucha información para dibujar: hay un resultado del modelo para método de cálculo usando la humectación o la humedad relativa considerando distintos umbrales: ',
							'85, 87.5, 90 y 92.5 y, para cada uno de ellos se obtiene para cada hora, el valor del DIV acumulado a 2, 7, 14 y 21 días. ',
							'A nosotros nos ayudará poner en común esta información y plantear ideas.\n\n',
							"\tEs un problema que aparezcan tantos gráficos, estoy buscando la manera de hacer un solo informe por estación, con varias páginas.\n\n",
							"\tAlgo de lo que hemos  hablado pero no hemos concretado en un procedimiento es incluir en estos gráficos la incidencia de la enfermedad observada en campo. Tenemos que definir un procedimiento y que nos envieis esa información periódicamente para que podamos contrastar la validez del modelo.\n\n",
							'\tComo siempre cualquier mejora que creáis necesaria hacednosla llegar a siar.cida@larioja.org\n\nUn saludo\n\n',
							'Nota: los gráficos de DIV acumulados sólo se generan cuando al menos hay 5 días de datos')						
	}
	if(Sys.Date() == as.Date("2020-06-09"))
	{
		mensajeExtra = paste0("Buenos días\n\n",
							"Desde hoy adjuntamos los gráficos de acuerdo a una nueva especificación. Se incluyen  todos los informes en un solo archivo y se ha reparado el error con los archivos adjuntos",
							"\n\n",
							"Como siempre cualquier mejora que creáis necesaria hacednosla llegar a siar.cida@larioja.org\n\nUn saludo\n\n")
		
	}
	
	if(Sys.Date() == as.Date("2020-10-07"))
	{
		mensajeExtra = paste0("Buenos días\n\n",
							"Las temperaturas en los últimos días han sido relativamente bajas, por debajo de 15ºC la mayor parte del día, por lo que",
							"los riesgos de ataque de cercospora (DIV) calculados según el modelo han sido cero o muy cercanos a cero.\n",
							"Si bien se prevén unos días con temperaturas más cálidas no prevemos que los DIVs calculados sean altos, además el cultivo de remolacha está cercano a su fin por lo que ",
							"este es el último correo que se enviará con el cálculo del riesgo de cercospora (DIV).\n",
							"\n\n",
							"Para cualquier sugerencia enviar correo electrónico a la dirección siar.cida@larioja.org\n\nUn saludo\n\n")
		
	}
	
	bodyEmail <- paste0(bodyEmail, mensajeExtra)
	
	Encoding(bodyEmail) <- 'UTF-8' #encoding #"iso-8859-1" #'UTF-8' # 
	Encoding(archivos) <- 'UTF-8' #encoding #'UTF-8'
	 
	
	enviaMailNoHTML(destinatariosEmail = destinatariosEmail,
					from = "cercospora@larioja.org",
					subject = subject,
					bodyEmail  = bodyEmail,
					files = archivos,#paste0(getwd(), '/', archivos), #
					encoding, smtp = smtp, mensajeLog ="Enviados Correos a ") #encoding = 'UTF--8'
}



procesaEstacionPresentacionAIMCRA <- function(estacion, fechaInicio, fechaFin,
										matrizDIV, vectorDIVHorario, horaInicio, offsetHR = 0, origenDato,
										etiquetasEjeXDIV3 = NULL, etiquetasEjeXDIV5 = NULL, etiquetasEjeXAgricultor = NULL, evolucionEnfermedad = NULL, tituloGrafico = NULL,
										textoDIV3 = 'Estrategia DIV > 3', textoDIV5 = 'Estrategia DIV > 5', textoAgricultor = 'Estrategia Agricultor',
										tipoGrafico = 'pdf', resultado = NA)
{
	if(is.na(resultado))
	{
		resultado <- calculaRiesgoDIV_V17(estacion = estacion, fechaInicio = fechaInicio, fechaFin,
										matrizDIV, vectorDIVHorario, horaInicio = horaInicio, offsetHR = offsetHR, origenDato = origenDato)#-> a
	}
	
	mensajeLog = paste('DIVs de la estación', estacion, origenDato, 'calculados')
	print(mensajeLog)
		
	# GENERACIÓN GRÁFICOS
	archivos <- character()
	
	archivo <- pintaGraficaRiesgoDIV_V17(resultado, etiquetasEjeX = etiquetasEjeX, graficoCampania = FALSE, pintaGraficaTHR = FALSE, pintaGraficoHeatmapSimplificado = TRUE, tipoGrafico = tipoGrafico,
				etiquetasEjeXDIV3 = etiquetasEjeXDIV3, etiquetasEjeXDIV5 = etiquetasEjeXDIV5, etiquetasEjeXAgricultor = etiquetasEjeXAgricultor,
				textoDIV3 = textoDIV3, textoDIV5 = textoDIV5, textoAgricultor = textoAgricultor,
				evolucionEnfermedad = evolucionEnfermedad, tituloGrafico = tituloGrafico)
	archivos <- c(archivos, archivo) # Esta variable guardará todos los archivos que se generen en la función y se enviarán a expertos
	
	archivo <- pintaGraficaRiesgoDIV_V17(resultado, etiquetasEjeX = etiquetasEjeX, pintaDIV_AMERICANO = TRUE, pintaGraficaTHR = FALSE, pintaGraficoHeatmapSimplificado = TRUE, tipoGrafico = tipoGrafico, tituloGrafico = tituloGrafico)
	archivos <- c(archivos, archivo) # Esta variable guardará todos los archivos que se generen en la función
	
	return(list(resultado = if(DEBUG) resultado else '', archivos = archivos))
}



uneDatosDIV_V17 <- function(resultado1, resultado2, estacion = NA, nombreEstacion = NA, origenDato = NA)
{
	datosClimatico <- rbind(resultado1$datosClimaticos, resultado2$datosClimaticos)
	DIV <- rbind(resultado1$DIV, resultado2$DIV)
	DIV_AMERICANO <- rbind(resultado1$DIV_AMERICANO, resultado2$DIV_AMERICANO)
	DIV_Horario <- rbind(resultado1$DIV_Horario, resultado2$DIV_Horario)
	HR95.8horas <- rbind(resultado1$fechasInfeccion$HR95.8horas, resultado2$fechasInfeccion$HR95.8horas)
	HR95.6horas <- rbind(resultado1$fechasInfeccion$HR95.6horas, resultado2$fechasInfeccion$HR95.6horas)
	HR90.8horas <- rbind(resultado1$fechasInfeccion$HR90.8horas, resultado2$fechasInfeccion$HR90.8horas)
	HR90.6horas <- rbind(resultado1$fechasInfeccion$HR90.6horas, resultado2$fechasInfeccion$HR90.6horas)
	humectacion.8horas <- rbind(resultado1$fechasInfeccion$humectacion.8horas, resultado2$fechasInfeccion$humectacion.8horas)
	humectacion.6horas <- rbind(resultado1$fechasInfeccion$humectacion.6horas, resultado2$fechasInfeccion$humectacion.6horas)
	fechaUltimoDato <- max(resultado1$fechaUltimoDato, resultado2$fechaUltimoDato)
	origenDato <- if(!is.na(origenDato))  origenDato else if(resultado1$origenDato == resultado2$origenDato) resultado1$origenDato else "error"
	estacion <- if(!is.na(estacion))  estacion else if(resultado1$estacion == resultado2$estacion) resultado1$estacion else "error"
	nombreEstacion <- if(!is.na(nombreEstacion))  nombreEstacion else if(resultado1$nombreEstacion == resultado2$nombreEstacion) resultado1$nombreEstacion else "error"
	
	return(list(datosClimatico = datosClimatico,
				DIV = DIV, DIV_AMERICANO = DIV_AMERICANO, DIV_Horario = DIV_Horario,
				fechasInfeccion = list(HR95.8horas = HR95.8horas, HR95.6horas = HR95.6horas, HR90.8horas = HR90.8horas, HR90.6horas = HR90.6horas,
										humectacion.8horas = humectacion.8horas, humectacion.6horas = humectacion.6horas),
				UltimoDato = fechaUltimoDato, origenDato = origenDato, estacion = estacion, nombreEstacion = nombreEstacion)
			)
}
		
