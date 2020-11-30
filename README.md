
<!-- README.md is generated from README.Rmd. Please edit that file -->

# suRgicalBox

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

El **armado de cajas quirúrgicas** es uno de los procesos que con mayor frecuencia es asociado a la ocurrencia de **eventos adversos hospitalarios**.
Esto se debe en mayor parte a que este proceso se realiza manualmente, y sin el apoyo de tecnología, en el cual el operador debe revisar
uno a uno los instrumentos quirúrgicos y decidir cuales son los instrumento que corresponde a la caja quirúrgica que está armando. Debido al gran volumen de instrumentos que deben ser revisados, el procedimiento es tedioso y repetitivo. Debido a esto, el error en el reconocimiento visual aumenta rápidamente a medida
que el operador avanza en su tarea, lo cual implica que la caja quirúrgica armada contendrá: más instrumentos de los necesarios, menos instrumentos de los requeridos y/o instrumentos que no corresponden a la caja quirúrgica, y finalmente terminará implicando que una intervención quirúrgica será cancelada, o lo que es peor,
podría poner en riesgo la vida del paciente.

El objetivo de `suRgicalBox` es **asistir** el armado de cajas quirúrgicas.
Esta aplicación web asiste, agiliza y facilita el armado de cajas
quirúrgicas, reduciendo a cero los errores de identificación de
instrumentos mediante el uso de redes neuronales profundas. Integra en
el flujo de trabajo la captura y preprocesamiento de imágenes, el
entrenamiento de redes neuronales siamesas, la inferencia de resultados
y la generación de reportes para hacer gestión.

Su uso está orientado para el armado de cajas quirúrgicas, sin embargo,
el proceso es genérico, por lo que puede extenderse su uso a otras
aplicaciones sin problemas.

## Instalación

Para tener acceso a modificar la configuración de las cámaras web, es necesario instalar `ffmpeg`. Para esto, descarga la
compilación estática de [`FFmpeg Builds`](http://ffmpeg-static.acyun.org/), descomprimela, cambia el
nombre de la carpeta a `FFmpeg` y pegala en `C:\`. Finalmente, agregua `C:\FFmpeg\bin` al PATH (ver [WikiHow](https://es.wikihow.com/instalar-FFmpeg-en-Windows)).

Puede instalar la versión actual de `suRgicalBox` desde
[GitHub](https://CRAN.R-project.org) usando:

    remotes::install_github("maalid/suRgicalBox@main")

## Código de Conducta

Tenga en cuenta que el proyecto `suRgicalBox` se publica con un [Código de
conducta para
colaboradores](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
Al contribuir a este proyecto, acepta cumplir sus términos.

Please note that the `suRgicalBox` project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
