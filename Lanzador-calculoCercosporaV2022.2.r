nombreProyecto = "ENF-Cercospora" # Se llamara como el directorio en el que esten los archivos, por ejemplo: ENF-StemfiliumPeral2016"
numeroVersion = "2022.2"

# Configurar directorios
dirPPAL <-"C:/ProcesosR/" # Servidor Desarrollo  y Servidor Producción
dirFuncionesComunes <- "00. FuncionesComunes/"
dirProyecto = nombreProyecto # Directorio general donde estarán los directorios con el código y los resultados procesados, en esta versión tiene que haber un .csv donde estará el listado de estaciones a procesar
dirCodigo = "./00. Codigo" # Directorio donde se almacenarán los archivos del código
dirResultados = "./01. Resultados" # Directorio donde se almacenarán los resultados obtenidos por el proeso


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


# Variables de control del proceso
DEBUG = FALSE # Si se hace = TRUE apareen algunos mensajes adicionales para controlar el proceso
produccion = FALSE # TRUE  # Dejar esto en false si no se quiere que lleguen los e-mails a todo el mundo
enviarEmailsSoloSiHayAlerta = FALSE #TRUE  # Dejar esto en true si se quiere que sólo lleguen los correos si el DIV de alguna de las estaciones procesadas en cada zona es superior a 2
enviarEmails = FALSE #TRUE # En false no se manda ningún e-mail
salidaAzucareraAB = TRUE #FALSE #  # Dejar esto en false si no se quiere que lleguen los e-mails a las personas de azucarera (se les envía un informe reducido)


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



#Inicialización
# Crear archivo configuración # Crear tabla
# configuracion2022 <- data.frame(estacion = c("SE03"), origenDato = "SIARNAcional", fechaInicio = as.POSIXct("2021-10-01 UTC"), fechaFin = Sys.time(), procesar = TRUE, zona = c('SEVILLA'), nombreEstacion = "Lebrija I", stringsAsFactors = F)
# configuracion2022 <- rbind(configuracion2022,data.frame(estacion = c("SE21"), origenDato = "SIARNAcional", fechaInicio = as.POSIXct("2021-10-01 UTC"), fechaFin = Sys.time(), procesar = FALSE, zona = c('SEVILLA'), nombreEstacion = "Los Palacios (IFAPA)"))
# configuracion2022 <- rbind(configuracion2022, list(estacion = c("2944"), origenDato = "CESENS", fechaInicio = NA, fechaFin = NA, procesar = FALSE, zona = c('SEVILLA'), nombreEstacion = "Cercospora Payuelos 2022"))
# write.csv(configuracion2022, "ArchivoConfiguracion2022.csv", row.names = F)
 

# Desde tablas:
# SELECT * FROM ENFERMEDADES.CONFIG_CERCOSPORA # CAMPOS: estacion, origenDato*, fechaInicio, fechaFin, procesar, zona*, nombreEstacion
# * claves foráneas

datosAProcesar = read.csv("ArchivoConfiguracion2022.csv", stringsAsFactors = FALSE)

# Agrupar estaciones que se van a procesar

# Validar los registros a procesar
#1.- solo los que tengan toda la información de partida, columnas 2, 3, 5 y 6 (columna 4 FechaFin y columna 6 nombreEstacion permiten procesar estación)
datosAProcesar <- datosAProcesar[complete.cases(datosAProcesar[,c(1,2,3,5,6)]), ]
#2.- solo las estaciones que estén activas, campo $procesar (columna5) = TRUE
datosAProcesar <- datosAProcesar[datosAProcesar$procesar,]

zonas = unique(datosAProcesar$zona)

setwd(dirResultados)

for(zona in zonas)
{
	
	estacionesAProcesar = datosAProcesar[datosAProcesar$zona == zona, ]
	#InicializarVariables <- function()
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
	
	# Añadir algún mensaje extra en el body del correo electrónico
	if(Sys.Date() == as.Date("2022-01-15"))
	{	
		mensajeExtra = paste0("Buenos días\n\n",
							'Desde ayer 14 enero, se ha puesto en marcha el sistema de cálculo de DIV y envío de informes para la zona sur.', #añaden los resultados de las estaciones "El Trobal" y "A2085".',
							"\n\n",
							"Un saludo\n\n",
							"Para comunicar con nosotros no usar la dirección remitente de este correo sino siar.cida@larioja.org\n",
							'\nDebajo los resúmenes de riesgo registrados según el modelo Rioja para prevención de la cercospora en remolacha azucarera')
		mensajeExtra = paste(mensajeExtra, textoDIVs, sep = '\n\n')
		mensajeExtraAzucarera = mensajeExtra
	}else if(Sys.Date() == as.Date("2021-06-25"))
	{	
		mensajeExtra = paste0("Buenos días\n\n",
							'Desde hoy se deja de enviar los resultados de las estaciones desinstaladas: "El Trobal" y "Melendo".',
							"\n\n",
							"Un saludo\n\n",
							'\nDebajo los resúmenes de riesgo registrados según el modelo horario')
		mensajeExtra = paste(mensajeExtra, textoDIVs, sep = '\n\n')
		mensajeExtraAzucarera = mensajeExtra
	}else
	{
		mensajeExtra = textoDIVs #''
		mensajeExtraAzucarera = textoDIVs #''
	
	}

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
	
break	
