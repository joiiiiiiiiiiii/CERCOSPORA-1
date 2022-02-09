# Variables de configuracion

nombreProyecto = "ENF-Cercospora" # Se llamara como el directorio en el que esten los archivos, por ejemplo: ENF-StemfiliumPeral2016"
numeroVersion = "2022.2"

# Configurar directorios
dirPPAL <-"C:/ProcesosR/" # Servidor Desarrollo  y Servidor Producción
dirFuncionesComunes <- "00. FuncionesComunes/"
dirProyecto = nombreProyecto # Directorio general donde estarán los directorios con el código y los resultados procesados, en esta versión tiene que haber un .csv donde estará el listado de estaciones a procesar
dirCodigo = "./00. Codigo" # Directorio donde se almacenarán los archivos del código
dirResultados = "./01. Resultados" # Directorio donde se almacenarán los resultados obtenidos por el proeso


# Variables de control del proceso
DEBUG = FALSE # Si se hace = TRUE apareen algunos mensajes adicionales para controlar el proceso
produccion = FALSE  #FALSE # TRUE  # Dejar esto en false si no se quiere que lleguen los e-mails a todo el mundo
enviarEmailsSoloSiHayAlerta = FALSE #TRUE  # Dejar esto en true si se quiere que sólo lleguen los correos si el DIV de alguna de las estaciones procesadas en cada zona es superior a 2
enviarEmails = TRUE # #TRUE # En false no se manda ningún e-mail
salidaAzucareraAB = TRUE #TRUE #FALSE #  # Dejar esto en false si no se quiere que lleguen los e-mails a las personas de azucarera (se les envía un informe reducido)

#Inicialización archivo Configuración (Desde R)
# Crear archivo configuración # Crear tabla
# configuracion2022 <- data.frame(estacion = c("SE03"), origenDato = "SIARNAcional", fechaInicio = as.POSIXct("2021-10-01 UTC"), fechaFin = Sys.time(), procesar = TRUE, zona = c('SEVILLA'), nombreEstacion = "Lebrija I", stringsAsFactors = F)
# configuracion2022 <- rbind(configuracion2022,data.frame(estacion = c("SE21"), origenDato = "SIARNAcional", fechaInicio = as.POSIXct("2021-10-01 UTC"), fechaFin = Sys.time(), procesar = FALSE, zona = c('SEVILLA'), nombreEstacion = "Los Palacios (IFAPA)"))
# configuracion2022 <- rbind(configuracion2022, list(estacion = c("2944"), origenDato = "CESENS", fechaInicio = NA, fechaFin = NA, procesar = FALSE, zona = c('SEVILLA'), nombreEstacion = "Cercospora Payuelos 2022"))
# write.csv(configuracion2022, "ArchivoConfiguracion2022.csv", row.names = F)

# Cargar archivo de configuracion
datosAProcesar = read.csv("ArchivoConfiguracion2022.csv", stringsAsFactors = FALSE)

# Desde tablas:
# SELECT * FROM ENFERMEDADES.CONFIG_CERCOSPORA # CAMPOS: estacion, origenDato*, fechaInicio, fechaFin, procesar, zona*, nombreEstacion
# * claves foráneas
# datosAProcesar = peticionSql("Select....")

# Lanzar proceso
source("Lanzador-calculoCercosporaV2022.2.r")




