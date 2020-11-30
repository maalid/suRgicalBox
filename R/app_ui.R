#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

# Global parameters ----
cameraWidth <- 224
cameraHeight <- 224
cameraQuality <- 100

imageWidth <- 240
imageHeight <- 240
brightness_low  <- 110
saturation_low <- 110
brightness_high  <- 120
saturation_high <- 120

puertos <- NULL

# Funcion showWebcam1 ----
showWebcam <- function(cameraWidth, cameraHeight, cameraQuality){

    paste8 <- function(..., sep = " ", collapse = NULL) {

        args <- c(lapply(list(...), enc2utf8),
                  list(sep = if (is.null(sep)) sep else enc2utf8(sep),
                       collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)))

        do.call(paste, args)

    }

    tags$div(tags$script(HTML(paste8(readLines(system.file("app/js/webcam.min.js", package = "suRgicalBox"), warn = FALSE, encoding = "UTF-8"), collapse = "\r\n"))),
    HTML(paste0('<div id="my_camera"></div>
                 <script language="JavaScript">
                 Webcam.set({width: ',(224/224)*cameraWidth,',
                             height: ',(224/224)*cameraHeight,',
	                         dest_width: ',cameraWidth,',
	                         dest_height: ',cameraHeight,',
                             image_format: \'jpeg\',
                             jpeg_quality: ',cameraQuality,',
                             flip_horiz: true});
                 Webcam.on("init", function () {
                                    Webcam.getCameras(function (cameras) {
                                            if (cameras.length > 0) {
                                                    Webcam.setAndInitCamera(cameras[cameras.length - 2].id)
                                                    }
                                            })
                           });
                 Webcam.attach( \'#my_camera\', true);
                 </script>'))
            )
}

# Funcion WebcamOff ----
webCamOff <- function(){
    tags$div(HTML(paste0('<script language="JavaScript">

         Webcam.reset(\'#my_camera\', true);

         </script>')))
}

# Funcion showWebcam2 ----
showWebcam2 <- function(cameraWidth, cameraHeight, cameraQuality){
    paste8 <- function(..., sep = " ", collapse = NULL) {
        args <- c(
            lapply(list(...), enc2utf8),
            list(
                sep = if (is.null(sep)) sep else enc2utf8(sep),
                collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
            )
        )
        do.call(paste, args)
    }
    tags$div(
        tags$script(HTML(paste8(readLines(system.file("app/js/webcam2.min.js", package = "suRgicalBox"), warn = FALSE, encoding = "UTF-8"), collapse = "\r\n"))),
        HTML(paste0('
        <div id="my_camera_2"></div>

        <script language="JavaScript">
         Webcam2.set({
         width: ',(224/224)*cameraWidth,',
         height: ',(224/224)*cameraHeight,',
	     dest_width: ',cameraWidth,',
	     dest_height: ',cameraHeight,',
         image_format: \'jpeg\',
         jpeg_quality: ',cameraQuality,',
         flip_horiz: true
         });

         Webcam2.attach( \'#my_camera_2\');
         </script>
         '))
    )
}

# Funcion Webcam2Off ----
webCam2Off <- function(){
    tags$div(HTML(paste0('<script language="JavaScript">

         Webcam2.reset(\'#my_camera_2\');

         </script>')))
}

# Funcion showInferenceWebCam1 ----
showInferenceWebCam1 <- function(cameraWidth, cameraHeight, cameraQuality){
    paste8 <- function(..., sep = " ", collapse = NULL) {
        args <- c(
            lapply(list(...), enc2utf8),
            list(
                sep = if (is.null(sep)) sep else enc2utf8(sep),
                collapse = if (is.null(collapse)) collapse else enc2utf8(collapse)
            )
        )
        do.call(paste, args)
    }
    tags$div(
        tags$script(HTML(paste8(readLines(system.file("js/webcam2.min.js", package = "suRgicalBox"), warn = FALSE, encoding = "UTF-8"), collapse = "\r\n")))
        ,
        HTML(paste0('
        <div id="InferenceWebCam1"></div>

        <script language="JavaScript">
         Webcam2.set({
         width: ',(224/224)*cameraWidth,',
         height: ',(224/224)*cameraHeight,',
	     dest_width: ',cameraWidth,',
	     dest_height: ',cameraHeight,',
         image_format: \'jpeg\',
         jpeg_quality: ',cameraQuality,',
         flip_horiz: true
         });

         Webcam2.attach( \'#InferenceWebCam1\' );
         </script>
         '))
    )
}

# Funcion InferenceWebCam1Off ----
InferenceWebCam1Off <- function(){
    tags$div(HTML(paste0('<script language="JavaScript">

         Webcam.reset(\'#InferenceWebCam1\', true);

         </script>')))
}

# Funcion closeWindow ----
jscode <- "shinyjs.closeWindow = function() { window.close(); }"

# UI ----

app_ui <- function(request) {
    tagList(
        # Adding external resources ----
        
        
        # FluidPage ----
        fluidPage(
            
            theme = shinythemes::shinytheme("cosmo"),
            shinyjs::useShinyjs(),
            shinyalert::useShinyalert(),
            
            # Funcion take_snapshot ----
            HTML('<script language="JavaScript">
         function take_snapshot() {
           // take snapshot and get image data
           Webcam.snap( function(data_uri) {
               document.getElementById(\'imageprev\').src = data_uri;
               Shiny.setInputValue("placeholder64", data_uri)
             } )
             Webcam2.snap( function(data_uri) {
               document.getElementById(\'imageprev2\').src = data_uri;
               Shiny.setInputValue("placeholder642", data_uri)
             } );
           }
         </script>'),
         
         # Funcion take_BurstSnapshot ----
         HTML('<script language="JavaScript">
                    var timer = null;

                    function take_BurstSnapshot(){
                        // take snapshot and get image data
                        Webcam.snap( function(data_uri) {
                            // display results in page
                            var img = new Image();
                            img.src = data_uri;

                            document.getElementById(\'results\').appendChild( img );
                            document.getElementById(\'imageprev\').src = data_uri;
                            Shiny.setInputValue("burstplaceholder64", data_uri)
                        } )
                        Webcam2.snap( function(data_uri) {
                            // display results in page
                            var img2 = new Image();
                            img2.src = data_uri;

                            document.getElementById(\'results2\').appendChild( img2 );
                            document.getElementById(\'imageprev2\').src = data_uri;
                            Shiny.setInputValue("burstplaceholder642", data_uri)
                        } );
                    }

                    function start_snapping() {
                        if (!timer) {
                            take_BurstSnapshot();
                            timer = setInterval( take_BurstSnapshot, 1500 );
                        }
                    }

                    function stop_snapping() {
                        if (timer) {
                            clearTimeout( timer );
                            timer = null;
                        }
                    }

                    function erase_snaps() {
                        document.getElementById(\'results\').innerHTML = \'\';
                        document.getElementById(\'results2\').innerHTML = \'\';
                    }

         </script>'),
         
         # Funcion takeInferenceSnapshot ----
         HTML('<script language="JavaScript">
         function takeInferenceSnapshot() {
           // take snapshot and get image data
             Webcam2.snap( function(data_uri) {
               document.getElementById(\'InferenceImagePrev\').src = data_uri;
               Shiny.setInputValue("InferenceImagePlaceholder64", data_uri)
             } );
           }
         </script>'),
         
         # Codigo js para cerrar app ----
         shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
         
         # Head App ----
         shiny::navbarPage(div(style = "width: 400px;
                                     font-size:40px;
                                     font-family: sans serif;
                                     text-align:center;",
                               "suRgicalBox ",
                               
                               span(style = "font-size: 18px;
                                          font-family: sans serif;
                                          color: grey;",
                                    "by AiLab UBB")),
                           windowTitle = HTML("suRgicalBox"),
                           id = "navbar",
                           
                           
                           # TAB 1: Captura de Imagenes ----
                           shiny::tabPanel("Captura de Imagenes",
                                           shiny::sidebarLayout(
                                               
                                               # Sidebar Panel ----
                                               shiny::sidebarPanel(width = 3,
                                                                   
                                                                   # Boton para elegir directorio de trabajo ----
                                                                   div(style = "width: 250px;
                                                                    font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                                    "Escoger Directorio"),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                    vertical-align:top;
                                                                    width: 170px;",
                                                                    shinyFiles::shinyDirButton(id = "WorkingDirectory",
                                                                                               label = "Seleccione Directorio",
                                                                                               title = "Escoja un directorio de trabajo"),
                                                                    tags$style("#WorkingDirectory {background-color: black;
                                                                                                 font-family: sans serif;
                                                                                                 font-size: 15px;
                                                                                                 color: white;
                                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                                 border-radius: 5px}")),
                                                                   
                                                                   hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                                   
                                                                   # Campos para la caja ----
                                                                   div(style = "width: 170px;
                                                                           font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                                       "Caja Numero"),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 170px;",
                                                                       shinyjs::disabled(numericInput(inputId = "caja",
                                                                                                      label = NULL,
                                                                                                      value = 1,
                                                                                                      min = 1,
                                                                                                      width = "170px")),
                                                                       tags$style("#caja {border-radius: 5px}")),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 75px;",
                                                                       shinyjs::disabled(actionButton(inputId = "CrearCaja",
                                                                                                      label = "Crear",
                                                                                                      size = "lg")),
                                                                       tags$style("#CrearCaja {background-color: black;
                                                                                          font-family: sans serif;
                                                                                          font-size: 15px;
                                                                                          color: white;
                                                                                          box-shadow: 5px 5px 5px grey;
                                                                                         border-radius: 5px}")),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 75px;",
                                                                       shinyjs::disabled(actionButton(inputId = "NuevaCaja",
                                                                                                      label = "Nueva",
                                                                                                      size = "lg")),
                                                                       tags$style("#NuevaCaja {background-color: black;
                                                                                          font-family: sans serif;
                                                                                          font-size: 15px;
                                                                                          color: white;
                                                                                          box-shadow: 5px 5px 5px grey;
                                                                                          border-radius: 5px}")),
                                                                   
                                                                   br(),
                                                                   
                                                                   # Campos para inventario ----
                                                                   div(style = "width: 170px;
                                                                           font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                                       "Escoger Inventario"),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 330px;",
                                                                       shinyjs::disabled(fileInput(inputId = "chooseInventario",
                                                                                                   label = NULL,
                                                                                                   accept = c(".csv"),
                                                                                                   width = "330px",
                                                                                                   buttonLabel = ".csv",
                                                                                                   placeholder = "Seleccionar archivo")),
                                                                       tags$style("#chooseInventario {background-color: black;
                                                                                                 font-family: sans serif;
                                                                                                 font-size: 15px;
                                                                                                 color: white;
                                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                                 border-radius: 5px}")),
                                                                   
                                                                   # Campos para el experimento ----
                                                                   div(style = "width: 170px;
                                                                           font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                                       "Experimento Numero"),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 170px;",
                                                                       shinyjs::disabled(numericInput(inputId = "experimento",
                                                                                                      label = NULL,
                                                                                                      value = 1,
                                                                                                      min = 1,
                                                                                                      width = "170px")),
                                                                       tags$style("#experimento {border-radius: 5px}")),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 75px;",
                                                                       shinyjs::disabled(actionButton(inputId = "CrearExperimento",
                                                                                                      label = "Crear",
                                                                                                      size = "lg")),
                                                                       tags$style("#CrearExperimento {background-color: black;
                                                                                                 font-family: sans serif;
                                                                                                 font-size: 15px;
                                                                                                 color: white;
                                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                                 border-radius: 5px}")),
                                                                   
                                                                   div(style = "display: inline-block;
                                                                           vertical-align:top;
                                                                           width: 75px;",
                                                                       shinyjs::disabled(actionButton(inputId = "NuevoExperimento",
                                                                                                      label = "Nuevo",
                                                                                                      size = "lg")),
                                                                       tags$style("#NuevoExperimento {background-color: black;
                                                                                                 font-family: sans serif;
                                                                                                 font-size: 15px;
                                                                                                 color: white;
                                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                                 border-radius: 5px}")),
                                                                   
                                                                   hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                                   
                                                                   # # Botones para escoger typeSet ----
                                                                   # div(align = "center",
                                                                   #     
                                                                   #     shinyjs::disabled(shiny::radioButtons(inputId = "setType",  # shiny::radioButtons
                                                                   #                                           label = "Set",
                                                                   #                                           inline = TRUE,
                                                                   #                                           choices = c("Entrenamiento" = "Train_Raw",
                                                                   #                                                       "Validacion" = "Validation_Raw",
                                                                   #                                                       "Test" = "Test_Raw",
                                                                   #                                                       "Anchors" = "Anchors_Raw")
                                                                   #     )
                                                                   #     )
                                                                   # ),
                                                                   
                                                                   hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                                   
                                                #                    # Boton para tomar Burst-Snapshot ----
                                                #                    div(br(style = "line-height: 200px;"),
                                                #                        
                                                #                        align = "center",
                                                #                        
                                                #                        shinyjs::disabled(shiny::actionButton(inputId = "BurstSnapshot",
                                                #                                                              label = "",
                                                #                                                              style = "background-color: black;
                                                #                                  font-family: sans serif;
                                                #                                  font-size: 35px;
                                                #                                  color: white;
                                                #                                  box-shadow: 5px 5px 5px grey;
                                                #                                  border-radius: 15px",
                                                #                                  icon = icon("images"),
                                                #                                  size = "lg",
                                                #                                  onclick = "start_snapping()")),
                                                #                        
                                                #                        
                                                #                        span(style = "font-size: 13px;
                                                # color: black;
                                                # text-shadow: 2px 2px 5px black;",
                                                # " -- "),
                                                # 
                                                # # Boton para parar Burst-Snapshot ----
                                                # shinyjs::disabled(shiny::actionButton(inputId = "stop_BurstSnapshot",
                                                #                                       label = "",
                                                #                                       style = "background-color: red;
                                                #                                  font-family: sans serif;
                                                #                                  font-size: 20px;
                                                #                                  color: white;
                                                #                                  box-shadow: 5px 5px 5px grey;
                                                #                                  border-radius: 10px",
                                                #                                  icon = icon("stop"),
                                                #                                  size = "sm",
                                                #                                  onclick = "stop_snapping()")
                                                # )
                                                #                    ),
                                                # 
                                                # 
                                                # 
                                                # # Boton para tomar Single-Snapshot ----
                                                # div(br(style = "line-height: 200px;"),
                                                #     align = "center",
                                                #     shinyjs::disabled(shiny::actionButton(inputId = "snapshot",
                                                #                                           label = "",
                                                #                                           style = "background-color: black;
                                                #                                  font-family: sans serif;
                                                #                                  font-size: 35px;
                                                #                                  color: white;
                                                #                                  box-shadow: 5px 5px 5px grey;
                                                #                                  border-radius: 15px",
                                                #                                  icon = icon("camera"),
                                                #                                  size = "lg",
                                                #                                  onclick = "take_snapshot()")
                                                #     )
                                                # ),
                                                
                                                hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                
                                                div(align = "right",
                                                    shinyjs::disabled(shiny::actionButton(inputId = "saveAnnotations",
                                                                                          label = "",
                                                                                          style = "background-color: black;
                                                                                 font-family: sans serif;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 3px",
                                                                                 icon = icon("save"),
                                                                                 size = "sm"))),
                                                
                                                # Area de anotaciones ----
                                                div(br(style = "line-height: 200px;"),
                                                    
                                                    align = "center",
                                                    
                                                    shiny::textAreaInput(inputId = "annotations",
                                                                         label = "Anotaciones",
                                                                         value = "",
                                                                         width = '300px',
                                                                         height = '150px',
                                                                         placeholder = "Escriba aca sus anotaciones"),
                                                )
                                                
                                               ), # end sidebarPanel
                                               
                                               # MainPanel App ----
                                               shiny::mainPanel(
                                                   
                                                   # Columna central App ----
                                                   shiny::column(width = 5,
                                                                 offset = 1,
                                                                 
                                                                 div(style = "box-shadow: 5px 5px 5px grey;
                                                         border-radius: 25px;",
                                                         
                                                         br(),
                                                         
                                                         align = "center",
                                                         
                                                         div(style = "height:280px;
                                                    width:550px;",
                                                         
                                                         # Boton para encender-apagar camara 1 ----
                                                         div(style = "display: inline-block;
                                                             height:270px;
                                                    width:250px;",
                                                             
                                                         
                                                    div(style = "height:20px;
                                                    width:250px;",
                                                                 shinyWidgets::switchInput(inputId = "OnOffCam1",
                                                                                   # label = "On-Off Camara 1",
                                                                                   onStatus = NULL,
                                                                                   offStatus = NULL,
                                                                                   size = "mini",
                                                                                   inline = TRUE),
                                                             
                                                             
                                                         
                                                         # Boton para settings camara 1 ----
                                                         
                                                             actionButton(inputId = "cam1Settings",
                                                                      label = "",
                                                                      style = "background-color: transparent;
                                                                 border-color: transparent;
                                                                 font-family: sans serif;
                                                                 font-size: 15px;
                                                                 color: white;
                                                                 border-radius: 5px",
                                                                 icon = icon("sliders"),
                                                                 size = "sm")),
                                                         
                                                         
                                                         # Lugar para mostrar la imagen de la camara 1 ----
                                                         div(style = "height:250px;
                                                    width:250px;",
                                                        uiOutput("cam1", inline = TRUE))),
                                                        
                                                        # Boton para encender-apagar camara 2 ----
                                                    div(style = "display: inline-block;
                                                        height:270px;
                                                    width:250px;",
                                                        
                                                    div(style = "height:20px;
                                                    width:250px;",
                                                                shinyWidgets::switchInput(inputId = "OnOffCam2",
                                                                                  # label = "On-Off Camara 2",
                                                                                  onStatus = NULL,
                                                                                  offStatus = NULL,
                                                                                  size = "mini",
                                                                                  inline = TRUE),
                                                            
                                                        
                                                        # Boton para settings camara 2 ----
                                                        
                                                            actionButton(inputId = "cam2Settings",
                                                                     label = "",
                                                                     style = "background-color: transparent;
                                                                 border-color: transparent;
                                                                 font-family: sans serif;
                                                                 font-size: 15px;
                                                                 color: white;
                                                                 border-radius: 5px",
                                                                 icon = icon("sliders"),
                                                                 size = "sm")),
                                                        
                                                        
                                                        # Lugar para mostrar la imagen de la camara 2 ----
                                                    div(style = "height:250px;
                                                    width:250px;",
                                                        uiOutput("cam2", inline = TRUE)))
                                                    
                                                    ),
                                                        
                                                        # br(),
                                                        
                                                        # Campo para ingresar la etiqueta ----
                                                        div(align = "center",
                                                            style = "color: black;
                                                        font-family: sans serif;
                                                        text-shadow: 3px 3px 4px grey;",
                                                        
                                                        shinyjs::disabled(shiny::selectizeInput(inputId = "etiqueta",
                                                                                                label = strong("Ingresar Etiqueta"),
                                                                                                choices = c(""),  # listaArticulos$objeto
                                                                                                width = '80%',
                                                                                                options = list(placeholder = "--> etiqueta <--",
                                                                                                               create = TRUE)
                                                        )
                                                        ),
                                                        
                                                        # tags$style(type = "text/css",
                                                        #            "#etiqueta {text-align:center;
                                                        #               font-family: sans serif;
                                                        #               font-style: italic;
                                                        #               display: block;}")
                                                        ),
                                                        
                                                        # Contador para Single-Snapshot y Burst-Snapshot ----
                                                        div(align = "center",
                                                            style = "color: black;",
                                                            shiny::verbatimTextOutput(outputId = "photoCounter"),
                                                            tags$style("#photoCounter {text-align: center;
                                                                          color: black;
                                                                          font-family: sans serif;
                                                                          font-size:12px;
                                                                          font-style: bold;
                                                                          overflow-y: scroll;
                                                                          max-height: 80px;
                                                                          max-width: 70px;
                                                                          background: white;
                                                                          border-radius: 5px}")
                                                        ),
                                                    
                                                    br(),
                                                        
                                                        # Botones para escoger typeSet ----
                                                        div(align = "center",
                                                            style = "color: black;
                                                            font-style: bold;
                                                        font-family: sans serif;
                                                        text-shadow: 2px 2px 3px white;",
                                                            shinyjs::disabled(shiny::radioButtons(inputId = "setType",  # shiny::radioButtons
                                                                                                  label = NULL,
                                                                                                  inline = TRUE,
                                                                                                  choices = c("Entrenamiento" = "Train_Raw",
                                                                                                              "Validacion" = "Validation_Raw",
                                                                                                              "Test" = "Test_Raw",
                                                                                                              "Anchors" = "Anchors_Raw")
                                                            )
                                                            )
                                                        ),
                                                        
                                                        # Boton para tomar Burst-Snapshot ----
                                                        div(br(style = "line-height: 200px;"),
                                                            
                                                            align = "center",
                                                            
                                                            shinyjs::disabled(shiny::actionButton(inputId = "BurstSnapshot",
                                                                                                  label = "",
                                                                                                  style = "background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 35px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 15px",
                                                                                 icon = icon("images"),
                                                                                 size = "lg",
                                                                                 onclick = "start_snapping()")),
                                                            
                                                            
                                                            span(style = "font-size: 13px;
                                                color: black;
                                                text-shadow: 2px 2px 5px black;",
                                                " -- "),
                                                
                                                # Boton para parar Burst-Snapshot ----
                                                shinyjs::disabled(shiny::actionButton(inputId = "stop_BurstSnapshot",
                                                                                      label = "",
                                                                                      style = "background-color: red;
                                                                                 font-family: sans serif;
                                                                                 font-size: 20px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 10px",
                                                                                 icon = icon("stop"),
                                                                                 size = "sm",
                                                                                 onclick = "stop_snapping()")
                                                )
                                                        ),
                                                
                                                
                                                
                                                # Boton para tomar Single-Snapshot ----
                                                div(br(style = "line-height: 200px;"),
                                                    align = "center",
                                                    shinyjs::disabled(shiny::actionButton(inputId = "snapshot",
                                                                                          label = "",
                                                                                          style = "background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 35px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 15px",
                                                                                 icon = icon("camera"),
                                                                                 size = "lg",
                                                                                 onclick = "take_snapshot()")
                                                    )
                                                ),
                                                        
                                                        style = "height:720px;
                                                    width:550px;
                                                    background-image: linear-gradient(#000000, #b4b4b4);"
                                                    
                                                                 ),
                                                    
                                                    # br(),
                                                    # br(),
                                                    #
                                                    # DT::DTOutput(outputId = 'inventarioCreado'),
                                                    
                                                    
                                                    
                                                   ),
                                                   
                                                   # Columna derecha App ----
                                                   shiny::column(width = 4,
                                                                 offset = 2,
                                                                 
                                                                 br(),
                                                                 br(),
                                                                 br(),
                                                                 br(),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'placeholder64',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'placeholder642',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'burstplaceholder64',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'burstplaceholder642',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'results',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 shinyjs::hidden(shiny::textInput(inputId = 'results2',
                                                                                                  label = '',
                                                                                                  value = "not_valid")),
                                                                 
                                                                 
                                                                 # Un lugar donde mostrar las snapshot cam1 + cam2 ----
                                                                 div(
                                                                     br(style = "line-height: 32px;"),
                                                                     style = "box-shadow: 5px 5px 5px grey;
                                                border-radius: 25px;
                                                height: 580px;
                                                width : 400px;
                                                background-image: linear-gradient(#000000, #b4b4b4);",
                                                align = "center",
                                                div(shiny::textOutput(outputId = "labelImagenCam1"),
                                                    tags$style("#labelImagenCam1 {text-align: center;
                                                                         color: white;
                                                                         font-family: sans serif;
                                                                         font-size: 13px;
                                                                         font-weight: bold;
                                                                         max-height: 50px;
                                                                         max-width: 380px;}")),
                                                
                                                br(style = "line-height: 12px;"),
                                                
                                                
                                                
                                                shiny::plotOutput(outputId = "imagenCam1",
                                                                  width = "224px",
                                                                  height = "224px"),
                                                shinyjs::hidden(img(id = 'imageprev')),
                                                
                                                br(style = "line-height: 32px;"),
                                                
                                                div(shiny::textOutput(outputId = "labelImagenCam2"),
                                                    tags$style("#labelImagenCam2 {text-align: center;
                                                                         color: white;
                                                                         font-family: sans serif;
                                                                         font-size: 13px;
                                                                         font-weight: bold;
                                                                         max-height: 50px;
                                                                         max-width: 380px;}")),
                                                
                                                br(style = "line-height: 12px;"),
                                                
                                                shiny::plotOutput(outputId = "imagenCam2",
                                                                  width = "224px",
                                                                  height = "210px"),
                                                shinyjs::hidden(img(id = 'imageprev2'))
                                                
                                                                 ) # end lugar donde mostrar las snapshot cam1 + cam2
                                                
                                                   ) # end columna derecha
                                               ) # end mainPanel
                                           ) # end sidebarLayout
                           ), # end TAB 1: captura de imagenes
                           
                           
                           # TAB 2: Siamese Model ----
                           tabPanel("Entrenamiento",
                                    
                                    sidebarLayout(
                                        
                                        sidebarPanel(width = 3,
                                                     
                                            # Campo para ingresar el batch size de entrenamiento ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Batch Size Entrenamiento"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(selectizeInput(inputId = "siameseModelTrainBatchSize",
                                                                                 label = NULL,
                                                                                 choices = 20,
                                                                                 width = '100%')),
                                                tags$style("#siameseModelTrainBatchSize {border-radius: 5px}")),
                                            
                                            # Campo para ingresar el step size de entrenamiento ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                               "Step Size Entrenamiento"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(numericInput(inputId = "siameseModelTrainSteps",
                                                                               label = NULL,
                                                                               value = 32,
                                                                               min = 3,
                                                                               width = "170px")),
                                                tags$style("#siameseModelTrainSteps {border-radius: 5px}")),
                                            
                                            hr(style = "box-shadow: 2px 2px 2px blue;"),
                                            
                                            # Campo para ingresar el batch size de validacion ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Batch Size Validacion"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(selectizeInput(inputId = "siameseModelValidationBatchSize",
                                                                                 label = NULL,
                                                                                 choices = 20,
                                                                                 width = '100%')),
                                                tags$style("#siameseModelValidationBatchSize {border-radius: 5px}")),
                                            
                                            
                                            # Campo para ingresar el step size de validacion ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Step Size Validacion"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(numericInput(inputId = "siameseModelValidationSteps",
                                                                               label = NULL,
                                                                               value = 32,
                                                                               min = 3,
                                                                               width = "170px")),
                                                tags$style("#siameseModelValidationSteps {border-radius: 5px}")),
                                            
                                            hr(style = "box-shadow: 2px 2px 2px blue;"),
                                            
                                            # Campo para ingresar embeding size ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Embeding Size Modelo"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(numericInput(inputId = "siameseModelEmbedingSize",
                                                                               label = NULL,
                                                                               value = 82,
                                                                               min = 3,
                                                                               width = "170px")),
                                                tags$style("#siameseModelEmbedingSize {border-radius: 5px}")),
                                            
                                            # Campo para ingresar la cantidad de epocas ----
                                            div(style = "width: 270px;
                                                        font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Cantidad de Epocas"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(numericInput(inputId = "siameseModelEpocas",
                                                                               label = NULL,
                                                                               value = 15,
                                                                               min = 3,
                                                                               width = "170px")),
                                                tags$style("#siameseModelEpocas {border-radius: 5px}")),
                                            
                                            # Boton para entrenar modelo siamese ----
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 75px;",
                                                shinyjs::disabled(actionButton(inputId = "siameseModelTrain",
                                                                               label = "Entrenar",
                                                                               size = "lg")),
                                                tags$style("#siameseModelTrain {background-color: black;
                                                                                font-family: sans serif;
                                                                                font-size: 15px;
                                                                                color: white;
                                                                                box-shadow: 5px 5px 5px grey;
                                                                                border-radius: 5px}")),
                                            
                                            hr(style = "box-shadow: 2px 2px 2px blue;"),
                                            hr(style = "box-shadow: 2px 2px 2px blue;"),
                                            hr(style = "box-shadow: 2px 2px 2px blue;"),
                                            
                                            # Campo para ingresar la epoca del mejor modelo ----
                                            div(style = "width: 270px;
                                                         font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Mejor Epoca"),
                                            
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 170px;",
                                                shinyjs::disabled(selectizeInput(inputId = "siameseModelBestEpoch",
                                                                                 label = NULL,
                                                                                 choices = 1,
                                                                                 multiple = TRUE,
                                                                                 width = '100%')),
                                                tags$style("#siameseModelBestEpoch {border-radius: 5px;}")),
                                            
                                            # Boton para guardar mejor modelo siamese ----
                                            div(style = "display: inline-block;
                                                         vertical-align:top;
                                                         width: 75px;",
                                                shinyjs::disabled(actionButton(inputId = "bestSiameseModelWeight",
                                                                               label = "Guardar",
                                                                               size = "lg")),
                                                tags$style("#bestSiameseModelWeight {background-color: black;
                                                                                     font-family: sans serif;
                                                                                     font-size: 15px;
                                                                                     color: white;
                                                                                     box-shadow: 5px 5px 5px grey;
                                                                                     border-radius: 5px}"))
                                            
                                        ),
                                        mainPanel(
                                            # Metricas de performance del modelo ----
                                            textOutput(outputId = "siameseModelEpochNumber"),
                                            # textOutput(outputId = "siameseModelAccBeginEpoch"),
                                            
                                            # br(),
                                            
                                            # textOutput(outputId = "siameseModelStepNumber"),
                                            # textOutput(outputId = "siameseModelAccBeginBatch"),
                                            textOutput(outputId = "siameseModelProgressBar"),
                                            #
                                            br(),
                                            
                                            textOutput(outputId = "siameseModelValAccEndBatch"),
                                            textOutput(outputId = "siameseModelValLossEndBatch"),
                                            
                                            br(),
                                            
                                            # textOutput(outputId = "siameseModelAccEndEpoch"),
                                            textOutput(outputId = "siameseModelValAccMetric"),
                                            
                                            br(),
                                            
                                            textOutput(outputId = "siameseModelValLossMetric"),
                                            
                                            br(),
                                            br(),
                                            
                                            DT::dataTableOutput(outputId = "siameseModelMetrics"),  #tableOutput
                                            
                                            br(),
                                            br(),
                                            
                                            plotOutput(outputId = "siameseModelMetricsPlot")
                                        )
                                    )),
                           
                           
                           
                           # TAB 3: Analysis and Results Siamese Model ----
                           tabPanel("Testing",
                                    
                                    sidebarLayout(
                                        
                                        sidebarPanel(width = 3,
                                            
                                            # Campo para ingresar el modelo final ----
                                            div(style = "width: 170px;
                                                     font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                "Escoger Modelo"),
                                            
                                            div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 330px;",
                                                shinyjs::disabled(fileInput(inputId = "chooseSiameseModel",
                                                                            label = NULL,
                                                                            accept = c(".hdf5"),
                                                                            width = "330px",
                                                                            buttonLabel = ".hdf5",
                                                                            placeholder = "Seleccionar archivo")),
                                                tags$style("#chooseSiameseModel {background-color: black;
                                                                                font-family: sans serif;
                                                                                font-size: 15px;
                                                                                color: white;
                                                                                box-shadow: 5px 5px 5px grey;
                                                                                border-radius: 5px}")),
                                            
                                            # Boton para generar el modelo final ----
                                            div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 150px;",
                                                shinyjs::disabled(actionButton(inputId = "generarSiameseModelo",
                                                                               label = "Generar modelo",
                                                                               size = "lg")),
                                                tags$style("#generarSiameseModelo {background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 15px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 5px}")),
                                            
                                            
                                            # shinyjs::disabled(fileInput(inputId = "chooseSiameseModel", 
                                            #                    label = "Choose Model",
                                            #                    accept = c(".hdf5")
                                            # )),
                                            # 
                                            # br(),
                                            
                                            # Boton para generar el modelo final ----
                                            # shinyjs::disabled(actionButton(inputId = "generarSiameseModelo", 
                                            #                       label = "Generar Modelo",
                                            #                       style = "background-color: black;
                                            #       font-family: sans serif;
                                            #       font-size: 15px;
                                            #       color: white;
                                            #       box-shadow: 5px 5px 5px grey;
                                            #       border-radius: 5px")),
                                            # 
                                            # br(),
                                            # br(),
                                            
                                            # Boton para generar predicciones ----
                                            div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 70px;",
                                            shinyjs::disabled(actionButton(inputId = "predictionSiamese", 
                                                                  label = "Predecir",
                                                                  size = "lg")),
                                            tags$style("#predictionSiamese {background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 15px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 5px}")),
                                            
                                            br(),
                                            br(),
                                            
                                            shinyWidgets::progressBar(id = "pb4", value = 0, display_pct = TRUE, striped = TRUE)
                                        ),
                                        
                                        mainPanel(
                                            
                                            # tabsetPanel(
                                            #     
                                            #     # Pestagna resultados Training ----                                    
                                            #     tabPanel("Training",
                                            #              
                                            #              br(),
                                            #              
                                            #              DT::dataTableOutput(outputId = "PrediccionesTrainSiamese"),
                                            #              
                                            #              br(),
                                            #              
                                            #              verbatimTextOutput(outputId = "confusionMatrixTrainSiamese")),
                                            #     # br(),
                                            #     
                                            #     # Pestagna resultados Validating ----
                                            #     tabPanel("Validating",
                                            #              
                                            #              br(),
                                            #              
                                            #              DT::dataTableOutput(outputId = "PrediccionesValidationSiamese"),
                                            #              
                                            #              br(),
                                            #              
                                            #              verbatimTextOutput(outputId = "confusionMatrixValidationSiamese")),
                                            #     # br(),
                                            #     
                                            #     # Pestagna resultados Testing ----
                                            #     tabPanel("Testing",
                                            #              
                                            #              br(),
                                                         
                                                         DT::dataTableOutput(outputId = "PrediccionesTestSiamese"),
                                                         
                                                         br(),
                                                         
                                                         # verbatimTextOutput(outputId = "confusionMatrixTestSiamese"),
                                                         plotOutput(outputId = "confusionMatrixTestSiamese"),
                                                         
                                                         br(),
                                                         
                                                         DT::dataTableOutput(outputId = "PrediccionesTestSiameseMetricsOverall"),
                                                         
                                                         br(),
                                                         
                                                         DT::dataTableOutput(outputId = "PrediccionesTestSiameseMetricsByClass")
                                            # )
                                            # )
                                            )
                                    )),
                           
                           
                           
                           # TAB 4: Inferencia ----
                           shiny::tabPanel("Inferencia",
                                           
                                           sidebarLayout(
                                               
                                                sidebarPanel(width = 3,
                                                             
                                                             
                                                            
                                                            # Coneccion con arduino ----
                                                            div(style = "width: 170px;
                                                     font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                     "Conectar Semaforo"),
                                                     
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 170px;",
                                                     shinyjs::disabled(shiny::actionButton(inputId = "ports",
                                                                                           label = "Buscar puertos",
                                                                                           width = "170px",
                                                                                           size = "lg")),
                                                     tags$style("#ports {background-color: black;
                                                                font-family: sans serif;
                                                                font-size: 15px;
                                                                color: white;
                                                                box-shadow: 5px 5px 5px grey;
                                                                border-radius: 5px}")),
                                                     
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 155px;",
                                                     shinyjs::disabled(shiny::selectInput(inputId = "COM",
                                                                                          label = NULL,
                                                                                          choices = puertos,
                                                                                          selected = "",
                                                                                          multiple = FALSE,
                                                                                          selectize = FALSE,
                                                                                          width = "155px",
                                                                                          size = NULL)),
                                                     tags$style("#COM {border-radius: 5px}")),
                                                     
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 75px;",
                                                     shinyjs::disabled(shiny::actionButton(inputId = "connectArduino",
                                                                                           label = "Conectar",
                                                                                           size = "lg")),
                                                     tags$style("#connectArduino {background-color: black;
                                                                         font-family: sans serif;
                                                                         font-size: 15px;
                                                                         color: white;
                                                                         box-shadow: 5px 5px 5px grey;
                                                                         border-radius: 5px}")),
                                                     
                                                     hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                     
                                                     # Campo para ingresar el modelo productivo ----
                                                     div(style = "width: 170px;
                                                     font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                     "Escoger Modelo"),
                                                     
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 330px;",
                                                     shinyjs::disabled(fileInput(inputId = "chooseProductionModel",
                                                                                 label = NULL,
                                                                                 accept = c(".hdf5"),
                                                                                 width = "330px",
                                                                                 buttonLabel = ".hdf5",
                                                                                 placeholder = "Seleccionar archivo")),
                                                     tags$style("#chooseProductionModel {background-color: black;
                                                                                font-family: sans serif;
                                                                                font-size: 15px;
                                                                                color: white;
                                                                                box-shadow: 5px 5px 5px grey;
                                                                                border-radius: 5px}")),
                                                     
                                                     # Boton para generar el modelo productivo ----
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 70px;",
                                                     shinyjs::disabled(actionButton(inputId = "generarProductionModel",
                                                                                    label = "Generar modelo",
                                                                                    size = "lg")),
                                                     tags$style("#generarProductionModel {background-color: black;
                                                                                 font-family: sans serif;
                                                                                 font-size: 15px;
                                                                                 color: white;
                                                                                 box-shadow: 5px 5px 5px grey;
                                                                                 border-radius: 5px}")),
                                                     
                                                     hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                     hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                     hr(style = "box-shadow: 2px 2px 2px blue;"),
                                                     
                                                     # Campo para ingresar imagen para inferencia en vivo ----
                                                     div(style = "width: 170px;
                                                     font: bold 16px 'Helvetica Neue', Helvetica, Arial, sans-serif;",
                                                     "Escoger Imagen"),
                                                     
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 330px;",
                                                     shinyjs::disabled(fileInput(inputId = "chooseLiveInferenceImage",
                                                                                 label = NULL,
                                                                                 accept = c(".jpeg"),
                                                                                 width = "330px",
                                                                                 buttonLabel = ".jpeg",
                                                                                 placeholder = "Seleccionar archivo")),
                                                     tags$style("#chooseLiveInferenceImage {background-color: black;
                                                                                   font-family: sans serif;
                                                                                   font-size: 15px;
                                                                                   color: white;
                                                                                   box-shadow: 5px 5px 5px grey;
                                                                                   border-radius: 5px}")),
                                                     
                                                     # Boton para generar inferencia en vivo con imagen cargada ----
                                                     div(style = "display: inline-block;
                                                     vertical-align:top;
                                                     width: 70px;",
                                                     shinyjs::disabled(actionButton(inputId = "inferenceWithUploadedImage",
                                                                                    label = "Go!",
                                                                                    size = "lg")),
                                                     tags$style("#inferenceWithUploadedImage {background-color: black;
                                                                                     font-family: sans serif;
                                                                                     font-size: 15px;
                                                                                     color: white;
                                                                                     box-shadow: 5px 5px 5px grey;
                                                                                     border-radius: 5px}")),
                                                     
                                               ), # End sidebarPanel
                                               
                                               mainPanel(
                                                   column(width = 5,
                                                          offset = 1,
                                                          
                                                          br(),
                                                          br(),
                                                          # br(),
                                                          # br(),
                                                          
                                                          div(style = "box-shadow: 5px 5px 5px grey;
                                                    border-radius: 25px;
                                                    height:480px;
                                                    width:290px;
                                                    background-image: linear-gradient(#000000, #b4b4b4);",
                                                    
                                                    br(),
                                                    
                                                    align = "center",
                                                    
                                                    # Boton para encender-apagar camara 1 ----
                                                    shinyWidgets::switchInput(inputId = "OnOffInferenceWebCam1",
                                                                              # label = "On-Off Camara 1",
                                                                              onStatus = NULL,
                                                                              offStatus = NULL,
                                                                              size = "mini",
                                                                              inline = TRUE),
                                                    
                                                    # Boton para settings camara 1 ----
                                                    actionButton(inputId = "InferenceWebCam1Settings",
                                                                 label = "",
                                                                 style = "background-color: transparent;
                                                                 border-color: transparent;
                                                                 font-family: sans serif;
                                                                 font-size: 15px;
                                                                 color: white;
                                                                 border-radius: 5px",
                                                                 icon = icon("sliders"),
                                                                 size = "sm"),
                                                    
                                                    # Inicia la webcam ----
                                                    div(style = "border-radius: 25px;
                                                        height: 215px;
                                                        width: 235px;",
                                                        uiOutput("InferenceWebCam1")),
                                                    # align = "center",
                                                    # showInferenceWebCam1(cameraWidth, cameraHeight, cameraQuality),
                                                    
                                                    br(),
                                                    
                                                    # Contador para Live-Single-Snapshot y Live-Burst-Snapshot ----
                                                    div(align = "center",
                                                        style = "color: black;",
                                                        verbatimTextOutput(outputId = "livePhotoCounter"),
                                                        tags$style("#livePhotoCounter {text-align: center;
                                                                              color: black;
                                                                              font-family: sans serif;
                                                                              font-size:12px;
                                                                              font-style: bold;
                                                                              max-height: 80px;
                                                                              max-width: 50px;
                                                                              border-radius: 5px;
                                                                              background: white;}")),
                                                    
                                                    # Boton para tomar Live-Single-Snapshot a usar por modelo productivo ----
                                                    div(br(style = "line-height: 200px;"),
                                                        align = "center",
                                                        shinyjs::disabled(actionButton(inputId = "inferenceSnapshot",
                                                                                       label = "",
                                                                                       style = "background-color: black;
                                                                              font-family: sans serif;
                                                                              font-size: 35px;
                                                                              color: white;
                                                                              box-shadow: 5px 5px 5px grey;
                                                                              border-radius: 15px",
                                                                              icon = icon("camera"),
                                                                              size = "lg",
                                                                              onclick = "takeInferenceSnapshot()")))
                                                    
                                                          ), # End box de imagen, contador y boton de snapshoot
                                                    
                                                    br(),
                                                    br(),
                                                    br(),
                                                    br(),
                                                    
                                                    # Lugar para mostrar la tabla de predicciones ----
                                                    div(tableOutput(outputId = "livePredicciones"))
                                                    
                                                   ), # End columna 2
                                                   
                                                   column(width = 6,
                                                          
                                                          # br(),
                                                          # br(),
                                                          # 
                                                          
                                                          shinyjs::hidden(textInput(inputId = 'InferenceImagePlaceholder64',
                                                                                    label = '',
                                                                                    value = "not_valid")), #importante que diga not_valid como bandera de inicio
                                                          
                                                          
                                                          
                                                          # shinyjs::hidden(textInput(inputId = 'InferenceImagePrev',
                                                          #                  label = '',
                                                          #                  value = "not_valid")), #importante que diga not_valid como bandera de inicio
                                                          
                                                          
                                                          
                                                          # Un lugar donde mostrar la live-snapshot cam1 ----
                                                          div(br(style = "line-height: 32px;"),
                                                              style = "box-shadow: 5px 5px 5px grey;
                                                    border-radius: 25px;
                                                    height: 740px;
                                                    background-image: linear-gradient(#000000, #b4b4b4);",
                                                    align = "center",
                                                    div(plotOutput(outputId = "liveImagenCam1",
                                                                   width = "240px",
                                                                   height = "240px"),
                                                        shinyjs::hidden(img(id = 'InferenceImagePrev')),
                                                        br(style = "line-height: 6px;"),
                                                        textOutput(outputId = "labelLiveImagenCam1"),
                                                        tags$style("#labelLiveImagenCam1 {text-align: center;
                                                                                 color: white;
                                                                                 font-family: sans serif;
                                                                                 font-size: 13px;
                                                                                 font-weight: bold;
                                                                                 max-height: 50px;
                                                                                 max-width: 350px;}")),
                                                    
                                                    br(),
                                                    br(),
                                                    # br(),
                                                    # br(),
                                                    
                                                    # Salida de predicciones ----
                                                    div(plotOutput(outputId = "inferencePlot")))
                                                    
                                                   ) # End columna 3
                                               ) # End mainPanel
                                           ) # End sidebarLayout
                           ), # End TAB 1: Inferencia
                           
                           
                           
                           # TAB 5: Boton cerrar ventana y cerar sesion de R ----
                           shiny::tabPanel(title = "", value = "Stop", icon = icon("power-off")),
                           
                           
                           # Layout pestagnas ----
                           tags$head(
                               tags$style(HTML(".navbar-nav {float: none !important;}
                                    .navbar-nav > li:nth-child(5) {float: right;}")))
                           
         ) # end navbarPage
        ) # end fluidPage
    ) # end tagList
} # end app_ui

# # shinyManager secure_app ----
# app_ui <- shinymanager::secure_app(app_ui,
#                                    status = "default",
#                                    theme = shinythemes::shinytheme("cosmo"),
#                                    tags_top = tags$div(shinyjs::useShinyjs(),
#                                                        shinyjs::extendShinyjs(text = jscode, functions = c("closeWindow")),
#                                                        tags$h2("suRgicalBox ",
#                                                                style = "align:center;
#                                                                         font-family: sans serif;
#                                                                         text-shadow: 5px 5px 5px grey;
#                                                                         font-size:55px;",
#                                                                span(style = "font-size: 18px;
#                                                                           font-family: sans serif;
#                                                                           color: grey;",
#                                                                     "by AiLab UBB")
#                                                        )
#                                    ),
#                                    tags_bottom = tags$div(
#                                      actionButton(inputId = "authStop",
#                                                   label = "",
#                                                   style = "float: right;
#                                                                                 border-radius: 5px;
#                                                                                 background-color: red;
#                                                                                 border-color: transparent;
#                                                                                 font-size: 10px",
#                                                   icon = icon("power-off")),
# 
#                                      tags$p(
#                                        "Si tienes problemas con la aplicacion, envia un mail al ",
#                                        tags$a(
#                                          href = "mailto:maalid@gmail.com?Subject=suRgicalBox%20Manager",
#                                          target = "_top", "administrador."
#                                        )
#                                      ),
#                                      tags$br(),
#                                      tags$p("AiLab UBB - 2020",
#                                             style = "text-align:center;
#                                                                           font-family: sans serif;
#                                                                           font-weight: bold;
#                                                                           color: grey;
#                                                                           font-size:10px")
#                                    ),
#                                    enable_admin = TRUE,
#                                    background  = "linear-gradient(#000000, #b4b4b4)")

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    # tags$style(HTML("
    #                  .navbar-nav {
    #                  float: none !important;
    #                  }
    #                  .navbar-nav > li:nth-child(2) {
    #                  float: right;
    #                  }
    #                 ")),
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'suRgicalBox'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

