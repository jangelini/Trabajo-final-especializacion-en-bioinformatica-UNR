# Trabajo-Final

En este repositorio se encuentra el [trabajo final](escrito/TrabajoFinal_EBioinformatica_AngeliniJ.pdf) que he realizado para obtener el título de especialista en bioinformática. 

En el presente trabajo se crearon herramientas informáticas para el análisis para datos provenientes de ensayos multiambientales (EMA) a fin de facilitar la tarea de los mejoradores genéticos de cultivos. Por un lado, se desarrolló un paquete de R llamado geneticae que reúne metodologı́a ampliamente utilizada y recientemente publicada, y que está abierto a que nuevas propuestas para el estudio de la interacción genotipo ambiente sean incorporados. Por otro lado, se confeccionó una interfaz gráfica de usuario que permite analizar, visualizar y extraer los resultados desde una página web sin la necesidad de contar con conocimiento especı́fico de un lenguaje de programación.

Como resultados del presente trabajo fue posible:
* **Mostrar un flujo de trabajo reproducible para la construcción de paquetes de R**. El mismo se puede utilizar de ejemplo para el desarrollo de nuevos paquetes o imitar la
construcción del paquete geneticae objeto de este trabajo.

* **Construir un paquete de R llamado geneticae para el análisis de datos provenientes de EMA** . Hasta el momento (Junio 2023) el paquete cuenta con más de 5000 descargas. La gran utilidad del mismo se debe a que:
    * incluye metodologı́a recientemente publicada para ajustar el modelo AMMI en presencia de outliers y para el tratamiento de información faltante que no se encuentra disponible en R ası́ como tampoco en softwares comerciales,
    * ofrece mayor flexibilidad en el manejo de la estructura de los conjuntos de datos que en las herramientas disponibles hasta este momento, brinda la posibilidad de generar representaciones gráficas de los biplots de buena calidad y configurables, está acompañado por un manual de ayuda completo y por un tutorial (viñeta) para su uso.

* **Desarrollar una aplicación web Shiny denominada Geneticae**, la cual es de suma importancia para aquellos analistas no familiarizados con la programación. Esta es de libre
acceso mediante conexión a internet que permite realizar los principales análisis implemen tados en el paquete sin necesidad de escribir lı́neas de código.

* **Implementar una metodologı́a de desarrollo de software colaborativa y basada en el sistema de control de versiones Git y los servicios web de GitHub**, adhiriendo
a los principios de la investigación reproducible y de libre acceso.


