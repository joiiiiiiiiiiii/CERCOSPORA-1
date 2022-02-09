# Cargar PREVIAMENTE en memoria archivo configuracion


# Comprobar que existen variables necesarias para el proceso en memoria
variablesAComprobar = c("nombreProyecto", "numeroVersion",
						 "dirPPAL", "dirFuncionesComunes", "dirProyecto", "dirCodigo", "dirResultados", # configuración directorios
						 "DEBUG", "produccion", "enviarEmailsSoloSiHayAlerta", "enviarEmails", "salidaAzucarera", # variables de control
						 "datosAProcesar") # configuración ejecución modelos (estaciones, fechas, zonas, etc)

if(!any(sapply(variablesAComprobar, exists)))
{
	variablesQueNoExisten = variablesAComprobar[which(!sapply(variablesAComprobar, exists))]
	stop(paste("Variable/s:", paste(variablesQueNoExisten, collapse = ","), "no definida"))
}


# Cargar funciones Comunes a todos los procesos y variables de seguridad (contraseñas BBDD, acceso a internet, etc)
setwd(paste0(dirPPAL, dirFuncionesComunes))
	source("funcionesComunes-SIAR.R", encoding = "UTF-8")
	sourceUTF8("variablesComunes-Borrador.R") ###CORREGIR "variablesComunes-SIAR.R"
	##  CARGAR EN MEMORIA DATOS CONFIDENCIALES
		cargaPasswords(directorioSeguridad, archivoPasswords)
	sourceUTF8("Lanzador-extraccionDatosArcimisV01.R") # ###CORREGIR Existe versión 2 de esta API interna, actualizar
	
	# Después de tener los usuarios y contraseñas confidenciales 
	sourceUTF8("APICesensV02.R") # después de cargaPasswords ya que necesita userCESENS & passwCESENS
	sourceUTF8("APISIARNAcionalV01.R") # después de cargaPasswords ya que necesita API_KEY_SIAR_MAPA
	sourceUTF8("mensajesEMail_V11.R") # después de cargaPasswords ya que necesita la configuración del servidor de correo


# Cargar librerias y archivos de código con las funciones propias del proyecto
library(staplr)
library(rjson)

setwd(paste0(dirPPAL, dirProyecto))

setwd(dirCodigo)
	sourceUTF8("FuncionesExtraccionDatosCercosporaV2022.2.R")
	sourceUTF8("FuncionesCalculoRiesgoCercosporaV2022.2.R")
	matrizDIV <- read.csv("Matriz_DIV.csv", row.names = 1)
	vectorDIVHorario <- read.csv("Vector_DIVHorario.csv")
	sourceUTF8("FuncionesGraficadoRiesgoCercosporaV17.7.R")
	sourceUTF8('direccionesEmail.r')
	sourceUTF8('FuncionesGestionProcesoCercospora.r') #contiene funciones wrapper para facilitar la gestión del proceso: procesaEstacion, enviaEmail, procesaEstacionPresentacionAIMCRA, uneDatosDIV_V17
setwd("./..")



# horaInicio es la variable que indica la hora a la que el proceso se tiene que calcular, se expresa en horario UTC
# en el caso del modelo Rioja al ser un modelo horario los cálculos no dependen de esta variable
# pero la horaInicio sí que se usa para "agrupar" o "integrar" los cálculos a esa hora para poder hablar de un DIV diario.
# considerando las condiciones medias de humedad/humectación típicas en la meseta de la península ibérica, la horaInicio más adecuada para integrar son las 10 UTC
horaActual = as.integer(strftime(Sys.time(), '%H'))
if(horaActual < 7){
	horaInicio = "6"
}else if(horaActual < 9){
	horaInicio = "8"
}else{
	horaInicio = "10"
}


# Validar los registros a procesar
#1.- solo los que tengan toda la información de partida, columnas 2, 3, 5 y 6 (columna 4 FechaFin y columna 6 nombreEstacion permiten procesar estación)
datosAProcesar <- datosAProcesar[complete.cases(datosAProcesar[,c(1,2,3,5,6)]), ]
#2.- solo las estaciones que estén activas, campo $procesar (columna5) = TRUE
datosAProcesar <- datosAProcesar[datosAProcesar$procesar,]


setwd(dirResultados)

# Agrupar estaciones que se van a procesar
zonas = unique(datosAProcesar$zona)

for(zona in zonas)
{
	
	estacionesAProcesar = datosAProcesar[datosAProcesar$zona == zona, ]
	#InicializarVariables 
	archivos <- character()
	archivosBasicos <- character()
	textoDIVs = ''
	alertaRiesgo <- !enviarEmailsSoloSiHayAlerta
	
	# Calcular modelo y generar archivos con informes resultados
	for(i in 1:nrow(estacionesAProcesar))
	{
		resultadoMODELO <- procesaEstacion(estacion = estacionesAProcesar$estacion[i], origenDato = estacionesAProcesar$origenDato[i],
										fechaInicio = estacionesAProcesar$fechaInicio[i],
										fechaFin = if(is.na(estacionesAProcesar$fechaFin[i])) Sys.time() else estacionesAProcesar$fechaFin[i],
										nombreEstacion = estacionesAProcesar$nombreEstacion[i],
										matrizDIV, vectorDIVHorario,
										horaInicio = horaInicio)
		archivos <- c(archivos, resultadoMODELO$archivos)
		archivosBasicos <- c(archivosBasicos, resultadoMODELO$archivosBasicos)
		textoDIVs = paste0(textoDIVs, resultadoMODELO$textoDIVs, '\n\n')
		if(resultadoMODELO$alertaRiesgo) alertaRiesgo <- TRUE
		
	}

	# Define destinatarios Correo electrónico
	if(produccion) #  
	{
		destinatariosEmail =  destinatariosEmailInformeCompletoEnProduccion
		direccionesEmailAzucarera =  get_DestinatariosEmail(zona)
	}else
	{
		destinatariosEmail = destinatariosEmailInformeCompletoEnPruebas
		direccionesEmailAzucarera = destinatariosEmailInformeAzucareraEnPruebas
	}
	
	if(DEBUG) list(destinatariosEmail = destinatariosEmail, direccionesEmailAzucarera = direccionesEmailAzucarera)
	
	# Añadir algún mensaje extra en el body del correo electrónico
	if(Sys.Date() == as.Date("2022-01-15"))
	{	
		mensajeExtra = paste0("Buenos días\n\n",
							'Desde ayer 14 enero, se ha puesto en marcha el sistema de cálculo de DIV y envío de informes para la zona sur.', #añaden los resultados de las estaciones "El Trobal" y "A2085".',
							"\n\n",
							"Un saludo\n\n",
							"\nDebajo los resúmenes de riesgo registrados según el modelo Rioja para prevención de la cercospora en remolacha azucarera")
		mensajeExtra = paste(mensajeExtra, textoDIVs, sep = '\n\n')
		mensajeExtraAzucarera = mensajeExtra
	}else if(Sys.Date() == as.Date("2021-06-25"))
	{	
		mensajeExtra = paste0("Buenos días\n\n",
							"Desde hoy se deja de enviar los resultados de las estaciones desinstaladas: NombreEstacion1 y NombreEstacion2.",
							"\n\n",
							"Un saludo\n\n",
							"\nDebajo los resúmenes de riesgo registrados según el modelo horario")
		mensajeExtra = paste(mensajeExtra, textoDIVs, sep = '\n\n')
		mensajeExtraAzucarera = mensajeExtra
	}else
	{
		mensajeExtra = textoDIVs #''
		mensajeExtraAzucarera = textoDIVs #''
	
	}
	
	if(DEBUG) print(list(mensajeExtra = mensajeExtra, mensajeExtraAzucarera = mensajeExtraAzucarera))
	
	if(DEBUG) print(paste("Se realiza envio de e-mail:", enviarEmails && alertaRiesgo))
	if(DEBUG) print(paste("Se envian e-mails a Azucarera:", salidaAzucareraAB && (enviarEmails && alertaRiesgo)))
	
	if(enviarEmails && alertaRiesgo)
	{	
		enviaEmail(destinatariosEmail, archivos, zona = toupper(zona), descripcion = 'Modelos SIAR y Americano con fechas de infección probable (no fecha de aparición de síntomas)', mensajeExtra = mensajeExtra)
		if(salidaAzucareraAB)
		{
			enviaEmail(direccionesEmailAzucarera, archivosBasicos, zona = toupper(zona), descripcion = 'Modelos SIAR y Americano', mensajeExtra = mensajeExtraAzucarera)
		}
	}

	
	borrarArchivos = FALSE
	if(borrarArchivos)
	{
		BORRAR(archivos) # !!!por definir qué borra y como
	}
}
		
