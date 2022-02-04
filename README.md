# CERCOSPORA

Código para calcular riesgo de ataque de cercospora de acuerdo al modelo Rioja desarrollado por el GO de Enfermedades Foliares

Dentro de los trabajos desarrollados para el GO Enfermedades Foliares se desarrolló un código para realizar el cálculo del riesgo de ataque de Cercospora en remolacha azucarera

Este código está escrito en lenguaje R, se aloja en los sistemas informáticos del Gobierno de La Rioja, su funcionamiento permite obtener los datos de redes agroclimáticas (SIAR Ministerio Agricultura, CESENS, SIAR La Rioja), calcular el índice de riesgo (DIV) según el modelo Rioja y el modelo de Shang y Teng (1989), generar archivos gráficos con figuras y archivos de datos con tablas con información alfanumérica y enviar un e-mail a una lista de destinatarios con un informe en pdf y textos descriptivos en el asunto y en el cuerpo del e-mail.

El código usa una tabla de riesgo horario en función de la temperatura para calcular el DIV. Esta tabla ha sido generada durante el proyecto a partir de la matriz de DIV diario del modelo de Sheng and Teng (1989); para ser plenamente funcional tiene una serie de dependencias que consisten en distintas librerías:

    -clientes API para extraer los datos de las redes agroclimáticas;
    -sistemas para la generación de mapas, generación de informes y envío de correos electrónicos para la difusión de los resultados;
    -servicios auxiliares para la carga de contraseñas, configuración de directorios y configuración de accesos a servicios internos de la CAR: servidores FTP, servidores de correo electrónico.
