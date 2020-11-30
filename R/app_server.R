#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom magrittr "%>%"
#' @importFrom keras "%<-%"
#' @noRd
app_server <- function(input, output, session ) {
    
    values <- reactiveValues(
        
        # numeroCaja =  NULL,
        inventario_df = NULL,
        inventario_parcial = tibble::tibble(),
        conn = NULL,
        productionModel = NULL
    )
    
    # ######## shinymanager
    # # shinyManager setLabels
    # shinymanager::set_labels(
    #     language = "en",
    #     "Please authenticate" = "",
    #     "Username:" = "nombre de usuario:",
    #     "Password:" = "clave de acceso:"
    # )
    # 
    # # check_credentials directly on sqlite db
    # res_auth <- shinymanager::secure_server(
    #     check_credentials = shinymanager::check_credentials(# credentials
    #         db = system.file("app/db/db1.sqlite", package = "snapShooteR"),
    #         # passphrase = key_get("R-shinymanager-key", "obiwankenobi")
    #         passphrase = "passphrase"
    #     )
    # )
    # 
    # observe({
    #     if (is.null(input$shinymanager_where) || (!is.null(input$shinymanager_where) && input$shinymanager_where %in% "application")) {
    #         ######## shinymanager
    
    # 1. WEBCAMS ----
    # CAPTURE WEBCAM1 SETTINGS ----
    # Observe cam1Settings ----
    observeEvent(input$cam1Settings, {
        # # # # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam1.bat")
        # webcamSettingsPath <- system.file("app/webcamSettings", package = "snapShooteR")
        # # # shell(glue::glue("C: & cd {webcamSettingsPath} & launchCam1.bat"))
        # # camNames <- system(glue::glue('{webcamSettingsPath} & chcp 65001 > nul & ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        # # cam1Name <- substr(camNames[[3]], start = 50, stop = nchar(camNames[[3]]) - 1)
        # # system(glue::glue('{webcamSettingsPath} & ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 1 -i video="{cam1Name}"'), wait = FALSE)
        # camNames <- system(paste(glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 'ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        camNames <- system(paste('ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        cam1Name <- substr(camNames[[3]], start = 50, stop = nchar(camNames[[3]]) - 1)
        # system(sprintf("%s %s %s",
        #                glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'),
        #                'chcp 65001 > nul',
        #                glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam1Name}"')),
        #        wait = FALSE,
        #        invisible = FALSE,
        #        # # intern = TRUE,
        #        # ignore.stdout = T,
        #        # show.output.on.console = F,
        #        minimized = TRUE)
        system(paste(glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam1Name}"')),
               wait = FALSE,
               invisible = FALSE,
               # # intern = TRUE,
               # ignore.stdout = T,
               # show.output.on.console = F,
               minimized = TRUE)
        # system(paste('guvcview --control_only'))
    })
    
    # CAPTURE WEBCAM2 SETTINGS ----
    # Observe cam2Settings ----
    observeEvent(input$cam2Settings, {
        # # # # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam2.bat")
        # webcamSettingsPath <- system.file("app/webcamSettings", package = "snapShooteR")
        # # # shell(glue::glue("C: & cd {webcamSettingsPath} & launchCam2.bat"))
        # # camNames <- shell(glue::glue('C: & cd {webcamSettingsPath} & chcp 65001 > nul & ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        # # cam2Name <- substr(camNames[[5]], start = 50, stop = nchar(camNames[[5]]) - 1)
        # # shell(glue::glue('C: & cd {webcamSettingsPath} & ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"'), wait = FALSE)
        # camNames <- system(paste(glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 'ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        camNames <- system(paste('ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        cam2Name <- substr(camNames[[5]], start = 50, stop = nchar(camNames[[5]]) - 1)
        # system(sprintf("%s %s %s",
        #                glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'),
        #                'chcp 65001 > nul',
        #                glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"')),
        #        wait = FALSE,
        #        invisible = FALSE,
        #        # # intern = TRUE,
        #        # ignore.stdout = T,
        #        # show.output.on.console = F,
        #        minimized = TRUE)
        system(paste(glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam2Name}"')),
               wait = FALSE,
               invisible = FALSE,
               # # intern = TRUE,
               # ignore.stdout = T,
               # show.output.on.console = F,
               minimized = TRUE)
    })
    
    # INFERENCE WEBCAM1 SETTINGS ----
    # Observe InferenceWebCam1Settings ----
    observeEvent(input$InferenceWebCam1Settings, {
        # # # # shell("C: & cd C:/Users/Admin/Downloads/webcam-settings-dialog-windows-master & launchCam1.bat")
        # webcamSettingsPath <- system.file("app/webcamSettings", package = "snapShooteR")
        # # # shell(glue::glue("C: & cd {webcamSettingsPath} & launchCam1.bat"))
        # # camNames <- system(glue::glue('{webcamSettingsPath} & chcp 65001 > nul & ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        # # cam1Name <- substr(camNames[[3]], start = 50, stop = nchar(camNames[[3]]) - 1)
        # # system(glue::glue('{webcamSettingsPath} & ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 1 -i video="{cam1Name}"'), wait = FALSE)
        # camNames <- system(paste(glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'), 'ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        camNames <- system(paste('ffmpeg -list_devices true -f dshow -i dummy -hide_banner'), intern = TRUE)
        cam3Name <- substr(camNames[[5]], start = 50, stop = nchar(camNames[[5]]) - 1)
        # system(sprintf("%s %s %s",
        #                glue::glue('"{webcamSettingsPath}/ffmpeg.exe"'),
        #                'chcp 65001 > nul',
        #                glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam1Name}"')),
        #        wait = FALSE,
        #        invisible = FALSE,
        #        # # intern = TRUE,
        #        # ignore.stdout = T,
        #        # show.output.on.console = F,
        #        minimized = TRUE)
        system(paste(glue::glue('ffmpeg -f dshow -show_video_device_dialog true -video_pin_name 2 -i video="{cam3Name}"')),
               wait = FALSE,
               invisible = FALSE,
               # # intern = TRUE,
               # ignore.stdout = T,
               # show.output.on.console = F,
               minimized = TRUE)
        # system(paste('guvcview --control_only'))
    })
    
    # ENCENDER O APAGAR WEBCAM1 ----
    # Observe OnOffCam1 ----
    observeEvent(input$OnOffCam1, {
        OnOffCam1Value <- input$OnOffCam1
        # print(OnOffCam1Value)
        if (OnOffCam1Value == "TRUE") {
            output$cam1 <- renderUI({showWebcam(cameraWidth, cameraHeight, cameraQuality)})
        } else {output$cam1 <- renderUI({webCamOff()})}
    })
    # observeEvent(input$OnCam1, {
    #   output$cam1 <- renderUI({showWebcam(cameraWidth, cameraHeight, cameraQuality)})
    # })
    # observeEvent(input$OffCam1, {
    #   output$cam1 <- renderUI({webCamOff()})
    # })
    
    # ENCENDER O APAGAR WEBCAM2 ----
    # Observe OnOffCam2 ----
    observeEvent(input$OnOffCam2, {
        OnOffCam2Value <- input$OnOffCam2
        # print(OnOffCam2Value)
        if (OnOffCam2Value == "TRUE") {
            output$cam2 <- renderUI({showWebcam2(cameraWidth, cameraHeight, cameraQuality)})
        } else {output$cam2 <- renderUI({webCam2Off()})}
    })
    # observeEvent(input$OnCam2, {
    #   output$cam2 <- renderUI({showWebcam2(cameraWidth, cameraHeight, cameraQuality)})
    # })
    # observeEvent(input$OffCam2, {
    #   output$cam2 <- renderUI({webCam2Off()})
    # })
    
    
    # ENCENDER O APAGAR INFERENCE WEBCAM1 ----
    # Observe OnOffInferenceWebCam1 ----
    observeEvent(input$OnOffInferenceWebCam1, {
        OnOffInferenceWebCam1Value <- input$OnOffInferenceWebCam1
        # print(OnOffInferenceWebCam1Value)
        if (OnOffInferenceWebCam1Value == "TRUE") {
            output$InferenceWebCam1 <- renderUI({showInferenceWebCam1(cameraWidth, cameraHeight, cameraQuality)})
        } else {output$InferenceWebCam1 <- renderUI({InferenceWebCam1Off()})}
    })
    
    # 2. FOLDERS ----
    # FOLDER DIRECTORIO DE TRABAJO ----
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "WorkingDirectory", roots = volumes, session = session, restrictions = system.file(package = "base"))
    
    # Observe WorkingDirectory ----
    observeEvent(input$WorkingDirectory, {
        
        # shinyjs::enable(id = "experimento")
        # shinyjs::enable(id = "CrearExperimento")
        #
        # shinyjs::disable(id = "WorkingDirectory")
        
        # workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        # setwd(workingFolderName)
        # cat("\ninput$WorkingDirectory value:\n\n")
        print(shinyFiles::parseDirPath(volumes, input$WorkingDirectory))
        
        shinyjs::enable(id = "caja")
        shinyjs::enable(id = "CrearCaja")
    })
    
    # CREAR FOLDERS ASOCIADOS A UNA CAJA ----
    # Observe CrearCaja ----
    observeEvent(input$CrearCaja, {
        
        shinyjs::disable(id = "WorkingDirectory")
        
        # shinyjs::enable(id = "NuevaCaja")
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        
        numeroCaja <- input$caja # numeroCaja <<- input$caja
        cajaFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}")
        
        inventarioFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario")
        
        if (!dir.exists(cajaFolderPath)) {
            dir.create(file.path(cajaFolderPath))
        }
        
        if (!dir.exists(inventarioFolderPath)) {
            dir.create(file.path(inventarioFolderPath))
        }
        
        
        
        # puertos <- NULL
        # conn <- NULL
        # inventario_df <- NULL
        # inventario_parcial <<- tibble::tibble()
        
        shinyjs::disable(id = "caja")
        shinyjs::disable(id = "CrearCaja")
        shinyjs::enable(id = "NuevaCaja")
        
        shinyjs::enable(id = "chooseInventario")
        # shinyjs::enable(id = "crearInventario")
        
        shinyjs::enable(id = "experimento")
        shinyjs::enable(id = "CrearExperimento")
    })
    
    # CREAR FOLDERS ASOCIADOS A UN EXPERIMENTO ----
    # Observe CrearExperimento ----
    observeEvent(input$CrearExperimento, {
        
        # shinyjs::enable(id = "NuevoExperimento")
        # shinyjs::enable(id = "setType")
        # 
        # shinyjs::enable(id = "saveAnnotations")
        # 
        # shinyjs::enable(id = "etiqueta")
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        
        numeroCaja <- input$caja
        cajaFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}")
        
        numeroExperimento <- input$experimento
        experimentFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}")
        
        documentosFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos")
        
        imagenesRawFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw")
        trainRawFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/Train_Raw")
        validationRawFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/Validation_Raw")
        testRawFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/Test_Raw")
        anchorsRawFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/Anchors_Raw")
        
        imagenesFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes")
        trainFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Train")
        validationFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Validation")
        testFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Test")
        anchorsFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Anchors")
        
        imagenesInferenciaFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Inferencia")
        
        modelosFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Modelos")
        
        resultadosTrainingFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Training")
        
        resultadosInferenciaFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia")
        
        if (!dir.exists(experimentFolderPath)) {
            dir.create(file.path(experimentFolderPath))
        }
        
        if (!dir.exists(documentosFolderPath)) {
            dir.create(file.path(documentosFolderPath))
        }
        
        if (!dir.exists(imagenesRawFolderPath)) {
            dir.create(file.path(imagenesRawFolderPath))
        }
        
        if (!dir.exists(trainRawFolderPath)) {
            dir.create(file.path(trainRawFolderPath))
        }
        
        if (!dir.exists(validationRawFolderPath)) {
            dir.create(file.path(validationRawFolderPath))
        }
        
        if (!dir.exists(testRawFolderPath)) {
            dir.create(file.path(testRawFolderPath))
        }
        
        if (!dir.exists(anchorsRawFolderPath)) {
            dir.create(file.path(anchorsRawFolderPath))
        }
        
        if (!dir.exists(imagenesFolderPath)) {
            dir.create(file.path(imagenesFolderPath))
        }
        
        if (!dir.exists(trainFolderPath)) {
            dir.create(file.path(trainFolderPath))
        }
        
        if (!dir.exists(validationFolderPath)) {
            dir.create(file.path(validationFolderPath))
        }
        
        if (!dir.exists(testFolderPath)) {
            dir.create(file.path(testFolderPath))
        }
        
        if (!dir.exists(anchorsFolderPath)) {
            dir.create(file.path(anchorsFolderPath))
        }
        
        if (!dir.exists(imagenesInferenciaFolderPath)) {
            dir.create(file.path(imagenesInferenciaFolderPath))
        }
        
        if (!dir.exists(modelosFolderPath)) {
            dir.create(file.path(modelosFolderPath))
        }
        
        if (!dir.exists(resultadosTrainingFolderPath)) {
            dir.create(file.path(resultadosTrainingFolderPath))
        }
        
        if (!dir.exists(resultadosInferenciaFolderPath)) {
            dir.create(file.path(resultadosInferenciaFolderPath))
        }
        
        inFileInventario <- input$chooseInventario
        if (!is.null(inFileInventario)) { #input$chooseInventario
            
            # inFileInventario <- input$chooseInventario
            print(input$chooseInventario$name)
            
            values$inventario_df <- readr::read_csv(file = inFileInventario$datapath) # inventario_df <<- readr::read_csv(file = inFileInventario$datapath)
            
            if (!file.exists(input$chooseInventario$name)) {
                readr::write_csv(values$inventario_df, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Caja{numeroCaja}.csv"))
            }
            
            print(values$inventario_df)
            # shinyjs::disable(id = "WorkingDirectory")
        
            shinyjs::enable(id = "NuevoExperimento")
            
            shinyjs::disable(id = "caja")
            shinyjs::disable(id = "CrearCaja")
            
            shinyjs::disable(id = "chooseInventario")
        
            shinyjs::disable(id = "experimento")
            shinyjs::disable(id = "CrearExperimento")
            
            shinyjs::enable(id = "setType")
            
            shinyjs::enable(id = "saveAnnotations")
            
            shinyjs::enable(id = "etiqueta")
            
            shinyjs::enable(id = "siameseModelTrainBatchSize")
            shinyjs::enable(id = "siameseModelTrainSteps")
            shinyjs::enable(id = "siameseModelValidationBatchSize")
            shinyjs::enable(id = "siameseModelValidationSteps")
            shinyjs::enable(id = "siameseModelEmbedingSize")
            shinyjs::enable(id = "siameseModelEpocas")
            shinyjs::enable(id = "siameseModelTrain")
        
            shinyjs::enable(id = "ports")
            
            updateSelectizeInput(session, inputId = "etiqueta", choices = values$inventario_df$Instrumento)
        
        } else {
            shinyalert::shinyalert(title = "Falta Inventario", text = "Debe ingresar un inventario antes de continuar", type = "warning")
            # inventario_df <<- readr::read_csv(file = glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Caja{numeroCaja}.csv"))
        }
        
        shiny::updateSelectInput(session, 
                                 inputId = "siameseModelTrainBatchSize", 
                                 label = "Train Batch Size", 
                                 choices = {train_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Train/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                              tibble::as_tibble() %>% 
                                              dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                                 number_train_images <- nrow(train_images)
                                 train_batch_size_df <- tibble::tibble(training_batch_size = rep(1:number_train_images), div = number_train_images/training_batch_size, resto = div %% 1)
                                 train_batch_size <- train_batch_size_df %>% dplyr::filter(resto == 0) 
                                 train_batch_size$training_batch_size},
                                 selected = 20)
        
        shiny::updateSelectInput(session, 
                                 inputId = "siameseModelValidationBatchSize", 
                                 label = "Validation Batch Size", 
                                 choices = {validation_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Validation/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                              tibble::as_tibble() %>% 
                                              dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                                 number_validation_images <- nrow(validation_images)
                                 validation_batch_size_df <- tibble::tibble(validating_batch_size = rep(1:number_validation_images), div = number_validation_images/validating_batch_size, resto = div %% 1)
                                 validation_batch_size <- validation_batch_size_df %>% dplyr::filter(resto == 0) 
                                 validation_batch_size$validating_batch_size},
                                 selected = 20)
        
        
    })
    
    # 4. NUEVA CAJA & EXPERIMENTO ----
    # CREAR NUEVA CAJA ----
    observeEvent(input$NuevaCaja, {
        
        shinyjs::enable(id = "WorkingDirectory")
        
        shinyjs::enable(id = "caja")
        shinyjs::enable(id = "CrearCaja")
        
        shinyjs::disable(id = "chooseInventario")
        
        shinyjs::disable(id = "experimento")
        shinyjs::disable(id = "CrearExperimento")
        
        shinyjs::disable(id = "NuevoExperimento")
        shinyjs::disable(id = "setType")
        
        shinyjs::disable(id = "saveAnnotations")
        
        shinyjs::disable(id = "BurstSnapshot")
        shinyjs::disable(id = "stop_BurstSnapshot")
        shinyjs::disable(id = "snapshot")
        
        shinyjs::disable(id = "etiqueta")
        
        shinyjs::disable(id = "siameseModelTrainBatchSize")
        shinyjs::disable(id = "siameseModelTrainSteps")
        shinyjs::disable(id = "siameseModelValidationBatchSize")
        shinyjs::disable(id = "siameseModelValidationSteps")
        shinyjs::disable(id = "siameseModelEmbedingSize")
        shinyjs::disable(id = "siameseModelEpocas")
        shinyjs::disable(id = "siameseModelTrain")
        shinyjs::disable(id = "siameseModelBestEpoch")
        shinyjs::disable(id = "bestSiameseModelWeight")
        
        # Resetear contador
        counter(1)
        
        numeroCaja <- input$caja + 1
        values$inventario_parcial <-  tibble::tibble()
        
        shiny::updateSelectInput(session, inputId = "caja", selected = numeroCaja)
        shinyjs::reset(id = "chooseInventario")
        # restoreInput(id = "chooseInventario", default = NULL)
        shiny::updateSelectInput(session, inputId = "experimento", selected = 1)
        shiny::updateSelectInput(session, inputId = "setType", selected = "Train")
        shiny::updateSelectInput(session, inputId = "etiqueta", selected = "--> etiqueta <--")
        shiny::updateTextAreaInput(session, inputId = "annotations", label = "Anotaciones", value = "", placeholder = "Escriba aca sus anotaciones")
        
        shiny::updateSelectInput(session, inputId = "siameseModelTrainBatchSize", label = "Train Batch Size", choices = 0, selected = 20)
        shiny::updateSelectInput(session, inputId = "siameseModelTrainSteps",  selected = 32)
        shiny::updateSelectInput(session, inputId = "siameseModelValidationBatchSize", label = "Validation Batch Size", choices = 0, selected = 20)
        shiny::updateSelectInput(session, inputId = "siameseModelValidationSteps", selected = 32)
        shiny::updateSelectInput(session, inputId = "siameseModelEmbedingSize", selected = 82)
        shiny::updateSelectInput(session, inputId = "siameseModelEpocas", selected = 15)
        shiny::updateSelectInput(session, inputId = "siameseModelBestEpoch", selected = 1)
        
        output$siameseModelEpochNumber <- NULL
        output$siameseModelStepNumber <- NULL
        output$siameseModelValAccEndBatch <- NULL
        output$siameseModelValLossEndBatch <- NULL
        output$siameseModelValAccMetric <- NULL
        output$siameseModelValLossMetric <- NULL
        output$siameseModelMetrics <- NULL
        output$siameseModelMetricsPlot <- NULL
        
    })
    
    # CREAR NUEVO EXPERIMENTO ----
    observeEvent(input$NuevoExperimento, {
        
        shinyjs::enable(id = "WorkingDirectory")
        
        shinyjs::enable(id = "chooseInventario")
        
        shinyjs::enable(id = "experimento")
        shinyjs::enable(id = "CrearExperimento")
        
        shinyjs::disable(id = "NuevoExperimento")
        shinyjs::disable(id = "setType")
        
        shinyjs::disable(id = "saveAnnotations")
        
        shinyjs::disable(id = "BurstSnapshot")
        shinyjs::disable(id = "stop_BurstSnapshot")
        shinyjs::disable(id = "snapshot")
        
        shinyjs::disable(id = "etiqueta")
        
        shinyjs::disable(id = "siameseModelTrainBatchSize")
        shinyjs::disable(id = "siameseModelTrainSteps")
        shinyjs::disable(id = "siameseModelValidationBatchSize")
        shinyjs::disable(id = "siameseModelValidationSteps")
        shinyjs::disable(id = "siameseModelEmbedingSize")
        shinyjs::disable(id = "siameseModelEpocas")
        shinyjs::disable(id = "siameseModelTrain")
        shinyjs::disable(id = "siameseModelBestEpoch")
        shinyjs::disable(id = "bestSiameseModelWeight")
        
        # Resetear contador
        counter(1)
        
        numeroExperimento <- input$experimento + 1
        values$inventario_parcial <-  tibble::tibble()
        
        shiny::updateSelectInput(session, inputId = "experimento", selected = numeroExperimento)
        shiny::updateSelectInput(session, inputId = "setType", selected = "Train")
        shiny::updateSelectInput(session, inputId = "etiqueta", selected = "--> etiqueta <--")
        shiny::updateTextAreaInput(session, inputId = "annotations", label = "Anotaciones", value = "", placeholder = "Escriba aca sus anotaciones")
        
        shiny::updateSelectInput(session, inputId = "siameseModelTrainBatchSize", label = "Train Batch Size", choices = 0, selected = 20)
        shiny::updateSelectInput(session, inputId = "siameseModelTrainSteps",  selected = 32)
        shiny::updateSelectInput(session, inputId = "siameseModelValidationBatchSize", label = "Validation Batch Size", choices = 0, selected = 20)
        shiny::updateSelectInput(session, inputId = "siameseModelValidationSteps", selected = 32)
        shiny::updateSelectInput(session, inputId = "siameseModelEmbedingSize", selected = 82)
        shiny::updateSelectInput(session, inputId = "siameseModelEpocas", selected = 15)
        shiny::updateSelectInput(session, inputId = "siameseModelBestEpoch", selected = 1)
        
        output$siameseModelEpochNumber <- NULL
        output$siameseModelStepNumber <- NULL
        output$siameseModelValAccEndBatch <- NULL
        output$siameseModelValLossEndBatch <- NULL
        output$siameseModelValAccMetric <- NULL
        output$siameseModelValLossMetric <- NULL
        output$siameseModelMetrics <- NULL
        output$siameseModelMetricsPlot <- NULL
        
    })
    
    # 5. ACTUALIZACION BOTONES ----
    # HABILITAR O DESHABILITAR BOTONES ----
    observe({
        
        OnOffCam1Value <- input$OnOffCam1
        OnOffCam2Value <- input$OnOffCam2
        
        # shinyjs::toggleState(id = "BurstSnapshot",
        #             condition = nchar(input$etiqueta) > 0)
        
        # shinyjs::toggleState(id = "snapshot",
        #             condition = nchar(input$etiqueta) > 0)
        
        shinyjs::toggleState(id = "BurstSnapshot",
                             condition = {nchar(input$etiqueta) > 0 & OnOffCam1Value == "TRUE" & OnOffCam2Value == "TRUE"} )
        
        shinyjs::toggleState(id = "snapshot",
                             condition = {nchar(input$etiqueta) > 0 & OnOffCam1Value == "TRUE" & OnOffCam2Value == "TRUE"} )
        
        shinyjs::toggleState(id = "connectArduino",
                             condition = !is.null(input$COM))
        
        shinyjs::toggleState(id = "generarProductionModel",
                             condition = !is.null(input$chooseProductionModel))
        
        shinyjs::toggleState(id = "inferenceWithUploadedImage",
                             condition = !is.null(input$chooseLiveInferenceImage))
        
    })
    
    # HABILITAR O DESHABILITAR BOTONES APRETAR BOTON stop_BurstSnapshot ----
    observeEvent(input$stop_BurstSnapshot, {
        
        shinyjs::disable(id = "stop_BurstSnapshot")
        
        shinyjs::enable(id = "NuevoExperimento")
        shinyjs::enable(id = "setType")
        
        shinyjs::enable(id = "BurstSnapshot")
        shinyjs::enable(id = "snapshot")
        
        # shinyjs::enable(id = "OnOffCam1")
        # shinyjs::enable(id = "OnOffCam2")
        shinyjs::enable(id = "cam1Settings")
        shinyjs::enable(id = "cam2Settings")
        
        shinyjs::enable(id = "etiqueta")
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        
        numeroCaja <- input$caja
        
        numeroExperimento <- input$experimento
        
        shiny::updateSelectInput(session, 
                                 inputId = "siameseModelTrainBatchSize", 
                                 label = "Train Batch Size", 
                                 choices = {train_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Train/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                     tibble::as_tibble() %>% 
                                     dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                                 number_train_images <- nrow(train_images)
                                 train_batch_size_df <- tibble::tibble(training_batch_size = rep(1:number_train_images), div = number_train_images/training_batch_size, resto = div %% 1)
                                 train_batch_size <- train_batch_size_df %>% dplyr::filter(resto == 0) 
                                 train_batch_size$training_batch_size},
                                 selected = 20)
        
        shiny::updateSelectInput(session, 
                                 inputId = "siameseModelValidationBatchSize", 
                                 label = "Validation Batch Size", 
                                 choices = {validation_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Validation/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                     tibble::as_tibble() %>% 
                                     dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                                 number_validation_images <- nrow(validation_images)
                                 validation_batch_size_df <- tibble::tibble(validating_batch_size = rep(1:number_validation_images), div = number_validation_images/validating_batch_size, resto = div %% 1)
                                 validation_batch_size <- validation_batch_size_df %>% dplyr::filter(resto == 0) 
                                 validation_batch_size$validating_batch_size},
                                 selected = 20)
        
        # numeroExperimento <- input$experimento
        
    })
    
    # SETEAR EN "train" EL TIPO DE SET DE IMAGENES CUANDO LA ETIQUETA CAMBIE ----
    observeEvent(input$etiqueta, {
        
        # Resetear contador
        counter(1)
        
        shiny::updateSelectInput(session, inputId = "setType", selected = "Train_Raw") # updateSelectInput
    })
    
    # 6. CONTADORES ----
    # CREAR E INICIALIZAR CONTADOR DE SNAPSHOTS ----
    counter <- reactiveVal(1)
    output$photoCounter <- renderText({counter()})
    
    liveCounter <- reactiveVal(1)
    output$livePhotoCounter <- renderText({liveCounter()})
    
    # 7. CAPTURA IMAGENES ----
    # CAPTURAR IMAGENES EN MODO SINGLE-SNAPSHOT ----
    observeEvent(input$snapshot, {
        
        if (!identical(input$placeholder64,"not_valid")) {
            
            shinyjs::disable(id = "WorkingDirectory")
            
            shinyjs::disable(id = "NuevoExperimento")
            shinyjs::disable(id = "setType")
            
            shinyjs::disable(id = "BurstSnapshot")
            shinyjs::disable(id = "stop_BurstSnapshot")
            shinyjs::disable(id = "snapshot")
            
            # Limpiar etiqueta
            photoLabel <- stringr::str_remove_all(input$etiqueta,"[^[:alnum:]]")
            # photoLabel <- stringr::str_remove_all(photoLabel,'[á é í ó ú ä ë ï ö ü Á É Í Ó Ú Ä Ë Ï Ö Ü]')
            photoLabel <- stringr::str_remove_all(photoLabel,' ')
            # Limpiar código base64
            inconn <- stringr::str_remove(input$placeholder64,'data:image/jpeg;base64,')
            
            inconn2 <- stringr::str_remove(input$placeholder642,'data:image/jpeg;base64,')
            
            # Crear archivo donde guardar imagen
            workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
            numeroCaja <- input$caja
            numeroExperimento <- input$experimento
            
            setRawFolderName <- as.character(input$setType)
            imagesRawPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/{setRawFolderName}")
            
            if (setRawFolderName == "Train_Raw" | setRawFolderName == "Validation_Raw") {
                classRawFolderName <- glue::glue("{imagesRawPath}/{photoLabel}/")
                if (!dir.exists(classRawFolderName)) {
                    dir.create(file.path(classRawFolderName))
                }
                
                saveImagesRawPath <- classRawFolderName #glue::glue("{imagesRawPath}/{photoLabel}/")
            }
            else {
                saveImagesRawPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/{setRawFolderName}/")
            }
            
            fileName_cam1 <- sprintf("%s_%s_%s_%s_%s",
                                     photoLabel,
                                     counter(),
                                     "Raw",
                                     "Cam1",
                                     format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss"))
            
            output$labelImagenCam1 <- renderText({
                fileName_cam1
            })
            
            fileName_cam2 <- sprintf("%s_%s_%s_%s_%s",
                                     photoLabel,
                                     counter(),
                                     "Raw",
                                     "Cam2",
                                     format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss"))
            
            output$labelImagenCam2 <- renderText({
                fileName_cam2
            })
            
            outconn <- file(description = paste0(saveImagesRawPath, fileName_cam1, ".jpeg"),
                            open = "wb")
            # Guardar imagen a disco duro
            base64enc::base64decode(what = inconn, output = outconn)
            close(outconn)
            
            outconn2 <- file(description = paste0(saveImagesRawPath, fileName_cam2, ".jpeg"),
                             open = "wb")
            # Guardar imagen a disco duro
            base64enc::base64decode(what = inconn2, output = outconn2)
            close(outconn2)
            
            
            
            ima <- magick::image_read(glue::glue("{saveImagesRawPath}/{fileName_cam1}.jpeg")) %>% 
                magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>% 
                magick::image_edge() %>%
                magick::image_negate() %>%
                magick::image_resize(geometry = magick::geometry_size_pixels(width = 337, height = 337, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                magick::image_data("gray") %>%
                as.integer() %>% 
                sketcher::sketch(style = 2,
                                 lineweight = 4,
                                 smooth = 3,
                                 gain = 0.1,
                                 contrast = 5,
                                 shadow = 19) %>%
                magick::image_read()
            
            ima2 <- magick::image_read(glue::glue("{saveImagesRawPath}/{fileName_cam2}.jpeg")) %>% 
                magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>% 
                magick::image_edge() %>%
                magick::image_negate() %>%
                magick::image_resize(geometry = magick::geometry_size_pixels(width = 337, height = 337, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                magick::image_data("gray") %>%
                as.integer() %>% 
                sketcher::sketch(style = 2,
                                 lineweight = 4,
                                 smooth = 3,
                                 gain = 0.1,
                                 contrast = 5,
                                 shadow = 19) %>%
                magick::image_read()
            
            
            if (setRawFolderName == "Train_Raw") {
                setFolderName <- "Train"
            } else if (setRawFolderName == "Validation_Raw") {
                setFolderName <- "Validation"
            } else if (setRawFolderName == "Test_Raw") {
                setFolderName <- "Test"
            } else if (setRawFolderName == "Anchors_Raw") {
                setFolderName <- "Anchors"
            }
            
            if (setFolderName == "Train" | setFolderName == "Validation") {
                classFolderName <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/{setFolderName}/{photoLabel}")
                if (!dir.exists(classFolderName)) {
                    dir.create(file.path(classFolderName))
                }
                
                saveImagesPath <- classFolderName #glue::glue("{imagesRawPath}/{photoLabel}/")
            }
            else {
                saveImagesPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/{setFolderName}")
            }
            
            ima_name <- gsub("_Raw", glue::glue("_Brightness100"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_name}.jpeg"), format = "jpeg")
            ima2_name <- gsub("_Raw", glue::glue("_Brightness100"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_name}.jpeg"), format = "jpeg")
            
            ## 2 Imagen Original Floped
            ima_floped_name <- gsub("_Raw", glue::glue("_Brightness100Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_name}.jpeg"), format = "jpeg")
            ima2_floped_name <- gsub("_Raw", glue::glue("_Brightness100Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_name}.jpeg"), format = "jpeg")
            
            ## 3 Imagen original fliped
            ima_fliped_name <- gsub("_Raw", glue::glue("_Brightness100Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_name}.jpeg"), format = "jpeg")
            ima2_fliped_name <- gsub("_Raw", glue::glue("_Brightness100Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_name}.jpeg"), format = "jpeg")
            
            ## 4 Imagen original floiped
            ima_floiped_name <- gsub("_Raw", glue::glue("_Brightness100Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_name}.jpeg"), format = "jpeg")
            ima2_floiped_name <- gsub("_Raw", glue::glue("_Brightness100Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_name}.jpeg"), format = "jpeg")
            
            ## 5 Imagen original brightnessed low
            ima_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ## 6 Imagen original brightnessed 120
            ima_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 7 Imagen original floped brightnessed low
            ima_floped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_floped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 8 Imagen original floped brightnessed high
            ima_floped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_floped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 9 Imagen original fliped brightnessed low
            ima_fliped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_fliped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 10 Imagen original fliped brightnessed high
            ima_fliped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_fliped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 11 Imagen original floiped brightnessed low
            ima_floiped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_floiped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 12 Imagen original floiped brightnessed high
            ima_floiped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_floiped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            
            output$imagenCam1 <- renderImage({
                cam1Filename <- glue::glue("{saveImagesRawPath}/{fileName_cam1}.jpeg")
                list(src = cam1Filename,
                     width = 224,
                     height = 210)
            }, deleteFile = FALSE)
            
            output$imagenCam2 <- renderImage({
                cam2Filename <- glue::glue("{saveImagesRawPath}/{fileName_cam2}.jpeg")
                list(src = cam2Filename,
                     width = 224,
                     height = 210)
            }, deleteFile = FALSE)
            
            # Aumentar contador
            nwCnt <- counter() + 1
            counter(nwCnt)
            
            shiny::updateSelectInput(session, 
                              inputId = "siameseModelTrainBatchSize", 
                              label = "Train Batch Size", 
                              choices = {train_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Train/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                  tibble::as_tibble() %>% 
                                  dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                              number_train_images <- nrow(train_images)
                              train_batch_size_df <- tibble::tibble(training_batch_size = rep(1:number_train_images), div = number_train_images/training_batch_size, resto = div %% 1)
                              train_batch_size <- train_batch_size_df %>% dplyr::filter(resto == 0) 
                              train_batch_size$training_batch_size},
                              selected = 20)
            
            shiny::updateSelectInput(session, 
                              inputId = "siameseModelValidationBatchSize", 
                              label = "Validation Batch Size", 
                              choices = {validation_images <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Validation/"), pattern = ".jpeg", recursive = TRUE) %>% 
                                  tibble::as_tibble() %>% 
                                  dplyr::filter(stringr::str_detect(value, "Brightness") & stringr::str_detect(value, ".jpeg"))
                              number_validation_images <- nrow(validation_images)
                              validation_batch_size_df <- tibble::tibble(validating_batch_size = rep(1:number_validation_images), div = number_validation_images/validating_batch_size, resto = div %% 1)
                              validation_batch_size <- validation_batch_size_df %>% dplyr::filter(resto == 0) 
                              validation_batch_size$validating_batch_size},
                              selected = 20)
            
            shinyjs::enable(id = "NuevoExperimento")
            shinyjs::enable(id = "setType")
            
            shinyjs::enable(id = "BurstSnapshot")
            
            shinyjs::enable(id = "snapshot")
            
        }
    })
    
    # CAPTURAR IMAGENES EN MODO BURST-SNAPSHOT (USING webcam.min.js + Rvision pkg) ----
    observeEvent(input$burstplaceholder64, {
        
        if (!identical(input$burstplaceholder64,"not_valid")) {
            
            shinyjs::enable(id = "stop_BurstSnapshot")
            
            shinyjs::disable(id = "WorkingDirectory")
            
            shinyjs::disable(id = "NuevoExperimento")
            shinyjs::disable(id = "setType")
            
            shinyjs::disable(id = "BurstSnapshot")
            shinyjs::disable(id = "snapshot")
            
            # shinyjs::disable(id = "OnOffCam1")
            # shinyjs::disable(id = "OnOffCam2")
            shinyjs::disable(id = "cam1Settings")
            shinyjs::disable(id = "cam2Settings")
            
            shinyjs::disable(id = "etiqueta")
            
            # Limpiar etiqueta
            photoLabel <- stringr::str_remove_all(input$etiqueta,"[^[:alnum:]]")
            # photoLabel <- stringr::str_remove_all(photoLabel,'[á é í ó ú ä ë ï ö ü Á É Í Ó Ú Ä Ë Ï Ö Ü]')
            photoLabel <- stringr::str_remove_all(photoLabel,' ')
            
            # Limpiar código base64
            inconn <- stringr::str_remove(input$burstplaceholder64,'data:image/jpeg;base64,')
            
            inconn2 <- stringr::str_remove(input$burstplaceholder642,'data:image/jpeg;base64,')
            
            # Crear archivo donde guardar imagen
            workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
            numeroCaja <- input$caja
            numeroExperimento <- input$experimento
            
            setRawFolderName <- as.character(input$setType)
            imagesRawPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/{setRawFolderName}")
            
            if (setRawFolderName == "Train_Raw" | setRawFolderName == "Validation_Raw") {
                classRawFolderName <- glue::glue("{imagesRawPath}/{photoLabel}/")
                if (!dir.exists(classRawFolderName)) {
                    dir.create(file.path(classRawFolderName))
                }
                
                saveImagesRawPath <- classRawFolderName #glue::glue("{imagesRawPath}/{photoLabel}/")
            }
            else {
                saveImagesRawPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Raw/{setRawFolderName}/")
            }
            
            fileName_cam1 <- sprintf("%s_%s_%s_%s_%s",
                                     photoLabel,
                                     counter(),
                                     "Raw",
                                     "Cam1",
                                     format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss"))
            
            output$labelImagenCam1 <- renderText({
                fileName_cam1
            })
            
            fileName_cam2 <- sprintf("%s_%s_%s_%s_%s",
                                     photoLabel,
                                     counter(),
                                     "Raw",
                                     "Cam2",
                                     format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss"))
            
            output$labelImagenCam2 <- renderText({
                fileName_cam2
            })
            
            outconn <- file(description = paste0(saveImagesRawPath, fileName_cam1, ".jpeg"),
                            open = "wb")
            
            # Guardar imagen a disco duro
            base64enc::base64decode(what = inconn, output = outconn)
            close(outconn)
            
            outconn2 <- file(description = paste0(saveImagesRawPath, fileName_cam2, ".jpeg"),
                             open = "wb")
            # Guardar imagen a disco duro
            base64enc::base64decode(what = inconn2, output = outconn2)
            close(outconn2)
            
            
            ima <- magick::image_read(glue::glue("{saveImagesRawPath}/{fileName_cam1}.jpeg")) %>% 
                magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>% 
                magick::image_edge() %>%
                magick::image_negate() %>%
                magick::image_resize(geometry = magick::geometry_size_pixels(width = 337, height = 337, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                magick::image_data("gray") %>%
                as.integer() %>% 
                sketcher::sketch(style = 2,
                                 lineweight = 4,
                                 smooth = 3,
                                 gain = 0.1,
                                 contrast = 5,
                                 shadow = 19) %>%
                magick::image_read()
            
            ima2 <- magick::image_read(glue::glue("{saveImagesRawPath}/{fileName_cam2}.jpeg")) %>% 
                magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>% 
                magick::image_edge() %>%
                magick::image_negate() %>%
                magick::image_resize(geometry = magick::geometry_size_pixels(width = 337, height = 337, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                magick::image_data("gray") %>%
                as.integer() %>% 
                sketcher::sketch(style = 2,
                                 lineweight = 4,
                                 smooth = 3,
                                 gain = 0.1,
                                 contrast = 5,
                                 shadow = 19) %>%
                magick::image_read()
            
            if (setRawFolderName == "Train_Raw") {
                setFolderName <- "Train"
            } else if (setRawFolderName == "Validation_Raw") {
                setFolderName <- "Validation"
            } else if (setRawFolderName == "Test_Raw") {
                setFolderName <- "Test"
            } else if (setRawFolderName == "Anchors_Raw") {
                setFolderName <- "Anchors"
            }
            
            if (setFolderName == "Train" | setFolderName == "Validation") {
                classFolderName <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/{setFolderName}/{photoLabel}")
                if (!dir.exists(classFolderName)) {
                    dir.create(file.path(classFolderName))
                }
                
                saveImagesPath <- classFolderName #glue::glue("{imagesRawPath}/{photoLabel}/")
            }
            else {
                saveImagesPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/{setFolderName}")
            }
            
            ima_name <- gsub("_Raw", glue::glue("_Brightness100"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_name}.jpeg"), format = "jpeg")
            ima2_name <- gsub("_Raw", glue::glue("_Brightness100"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_name}.jpeg"), format = "jpeg")
            
            ## 2 Imagen Original Floped
            ima_floped_name <- gsub("_Raw", glue::glue("_Brightness100Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_name}.jpeg"), format = "jpeg")
            ima2_floped_name <- gsub("_Raw", glue::glue("_Brightness100Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_name}.jpeg"), format = "jpeg")
            
            ## 3 Imagen original fliped
            ima_fliped_name <- gsub("_Raw", glue::glue("_Brightness100Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_name}.jpeg"), format = "jpeg")
            ima2_fliped_name <- gsub("_Raw", glue::glue("_Brightness100Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_name}.jpeg"), format = "jpeg")
            
            ## 4 Imagen original floiped
            ima_floiped_name <- gsub("_Raw", glue::glue("_Brightness100Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_name}.jpeg"), format = "jpeg")
            ima2_floiped_name <- gsub("_Raw", glue::glue("_Brightness100Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_flip() %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_name}.jpeg"), format = "jpeg")
            
            ## 5 Imagen original brightnessed low
            ima_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ## 6 Imagen original brightnessed 120
            ima_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 7 Imagen original floped brightnessed low
            ima_floped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_floped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 8 Imagen original floped brightnessed high
            ima_floped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_floped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 9 Imagen original fliped brightnessed low
            ima_fliped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_fliped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 10 Imagen original fliped brightnessed high
            ima_fliped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Fliped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_fliped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_fliped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Fliped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_fliped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            ### 11 Imagen original floiped brightnessed low
            ima_floiped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_brightnessLow_name}.jpeg"), format = "jpeg")
            ima2_floiped_brightnessLow_name <- gsub("_Raw", glue::glue("_Brightness{brightness_low}Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_low}"), saturation = glue::glue("{saturation_low}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_brightnessLow_name}.jpeg"), format = "jpeg")
            
            ### 12 Imagen original floiped brightnessed high
            ima_floiped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floiped"), glue::glue("{fileName_cam1}"))
            ima %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima_floiped_brightnessHigh_name}.jpeg"), format = "jpeg")
            ima2_floiped_brightnessHigh_name <- gsub("_Raw", glue::glue("_Brightness{brightness_high}Floiped"), glue::glue("{fileName_cam2}"))
            ima2 %>% 
                magick::image_flop() %>% 
                magick::image_flip() %>%
                magick::image_modulate(brightness = glue::glue("{brightness_high}"), saturation = glue::glue("{saturation_high}")) %>% 
                magick::image_write(path = glue::glue("{saveImagesPath}/{ima2_floiped_brightnessHigh_name}.jpeg"), format = "jpeg")
            
            
            output$imagenCam1 <- renderImage({
                cam1Filename <- glue::glue("{saveImagesRawPath}/{fileName_cam1}.jpeg")
                list(src = cam1Filename,
                     width = 224,
                     height = 210)
            }, deleteFile = FALSE)
            
            output$imagenCam2 <- renderImage({
                cam2Filename <- glue::glue("{saveImagesRawPath}/{fileName_cam2}.jpeg")
                list(src = cam2Filename,
                     width = 224,
                     height = 210)
            }, deleteFile = FALSE)
            
            # Aumentar contador
            nwCnt <- counter() + 1
            counter(nwCnt)
            
            
            
        }
    })
    
    # 8. ANOTACIONES ----
    # GUARDAR EN ARCHIVO DE TEXTO LAS ANOTACIONES ----
    observeEvent(input$saveAnnotations, {
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        numeroCaja <- input$caja
        numeroExperimento <- input$experimento
        
        documentosFolderPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos")
        
        anotacionesFilePath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Anotaciones_Caja{numeroCaja}_Experimento{numeroExperimento}")
        
        if (!dir.exists(documentosFolderPath)) {
            dir.create(file.path(documentosFolderPath))
        }
        
        myAnnotations <- input$annotations
        
        utils::write.table(myAnnotations,
                           file = glue::glue("{anotacionesFilePath}.txt"),
                           sep = "",
                           col.names = FALSE,
                           row.names = FALSE,
                           quote = FALSE)
        
    })
    
    
    
    
    # 9. TRAIN SIAMESE_MODEL ----
    observeEvent(input$siameseModelTrain, {
        
        output$featureModelMetrics <- NULL
        output$featureModelMetricsPlot <- NULL
        
        output$classificationModelMetrics <- NULL
        output$classificationModelMetricsPlot <- NULL
        
        output$siameseModelMetrics <- NULL
        output$siameseModelMetricsPlot <- NULL
        
        shinyjs::disable(id = "experimento")
        shinyjs::disable(id = "CrearExperimento")
        shinyjs::disable(id = "NuevoExperimento")
        
        shinyjs::disable(id = "setType")
        
        shinyjs::disable(id = "BurstSnapshot")
        shinyjs::disable(id = "stop_BurstSnapshot")
        shinyjs::disable(id = "snapshot")
        
        shinyjs::disable(id = "etiqueta")
        
        shinyjs::disable(id = "siameseModelTrainBatchSize")
        shinyjs::disable(id = "siameseModelTrainSteps")
        shinyjs::disable(id = "siameseModelValidationBatchSize")
        shinyjs::disable(id = "siameseModelValidationSteps")
        shinyjs::disable(id = "siameseModelEmbedingSize")
        shinyjs::disable(id = "siameseModelEpocas")
        shinyjs::disable(id = "siameseModelTrain")
        shinyjs::disable(id = "siameseModelBestEpoch")
        shinyjs::disable(id = "bestSiameseModelWeight")
        
        
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        numeroCaja <- input$caja
        numeroExperimento <- input$experimento
        experimentFolderName <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}")
        classes <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/Train/"))
        number_classes <- length(classes)
        # train_dir <- glue("Experimento{numeroExperimento}/Imagenes/Train/")
        # validation_dir <- glue("Experimento{numeroExperimento}/Imagenes/Validation/")
        modelosPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Modelos/")
        
        if (!dir.exists(modelosPath)) {
            dir.create(file.path(modelosPath))
        }
        
        siameseModel_epocas <- input$siameseModelEpocas
        siameseModel_trainBatchSize <- input$siameseModelTrainBatchSize %>% as.numeric()
        siameseModel_validationBatchSize <- input$siameseModelValidationBatchSize %>% as.numeric()
        siameseModel_embedingSize <- input$siameseModelEmbedingSize %>% as.numeric()
        siameseModel_trainSteps <- input$siameseModelTrainSteps
        siameseModel_validationSteps <- input$siameseModelValidationSteps
        
        siameseModel_weights_filepath <- file.path(modelosPath, 
                                                   sprintf("%s_%s_%s_%s%s", 
                                                           "SModel_Weights",
                                                           glue::glue("ES{siameseModel_embedingSize}"),
                                                           "E{epoch:02d}",
                                                           glue::glue("Caja{numeroCaja}_Experimento{numeroExperimento}"),
                                                           ".hdf5"))
        
        generateDataframes <- function(experiment_path, set_path) {
            
            lista_imagenes <- list.files(path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes/{set_path}"), 
                                         pattern = glue::glue("*._Cam1_*"), #"*._cam1_\\d+\\_\\d+\\_\\d+\\_\\d+\\_\\d+\\_\\d+\\.jpeg"
                                         full.names = TRUE, 
                                         recursive = T) %>% 
                tibble::as_tibble() %>% 
                dplyr::rename(Path = value) %>% 
                dplyr::mutate(Directorio = glue::glue("{workingFolderName}"),
                              Caja = glue::glue("{numeroCaja}"),
                              Experimento = glue::glue("{numeroExperimento}"),
                              Set = glue::glue("{set_path}"),
                              Imagen = gsub(glue::glue(".*/|.jpeg.*"), "", Path)) %>% 
                tidyr::separate(col = Imagen,
                                into = c("Instrumento", "Contador", "Transformacion", "Camara", "Fecha", "Hora"),
                                sep = "_",
                                remove = FALSE) %>% 
                dplyr::select(Directorio, Caja, Experimento, Set, Instrumento, Contador, Transformacion, Camara, Fecha, Hora, Imagen, Path) %>% 
                dplyr::group_split(Instrumento)
        }                              
        
        trainDataFramesList <- generateDataframes(experiment_path = experimentFolderName, 
                                                  set_path = "Train")
        readr::write_csv(trainDataFramesList %>% dplyr::bind_rows(),
                  path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Train_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
        
        valDataFramesList <- generateDataframes(experiment_path = experimentFolderName, 
                                                set_path = "Validation")
        readr::write_csv(valDataFramesList %>% dplyr::bind_rows(),
                  path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Validation_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
        
        testDataFramesList <- generateDataframes(experiment_path = experimentFolderName, 
                                                 set_path = "Test")
        readr::write_csv(testDataFramesList %>% dplyr::bind_rows(),
                  path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Test_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
        
        anchorDataFramesList <- generateDataframes(experiment_path = experimentFolderName, 
                                                 set_path = "Anchors")
        readr::write_csv(anchorDataFramesList %>% dplyr::bind_rows(),
                  path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Anchor_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
        
        preprocess_input <- function(x){
            
            x <- magick::image_read(x/255) %>%
                #     image_edge() %>% 
                #     image_negate() %>% 
                magick::image_data('rgb') %>%
                as.integer() %>%
                keras::image_to_array() %>%
                reticulate::array_reshape(c(imageWidth, imageHeight, 3)) #c(224, 224, 3)
            
            # x <- image_read(x/255) %>%
            #     image_edge() %>% 
            #     image_negate() %>% 
            #     image_data('gray') %>%  
            #     as.integer() %>%
            #     sketch(style = 1,
            #            lineweight = 2, 
            #            smooth = 2,
            #            gain = 0.01,
            #            contrast =  2,
            #            shadow = 19) %>% 
            #     image_read() %>% 
            #     image_data("rgb") %>% 
            #     as.integer() %>% 
            #     image_to_array() %>%
            #     array_reshape(c(224, 224, 3))
            
            return(x/255)
        }
        
        trainDatagen2 <- keras::image_data_generator(preprocessing_function = preprocess_input)
        
        
        
        trainGeneratorList2 <- list()
        for (i in 1:number_classes) {
            trainGenerator2 <- keras::flow_images_from_dataframe(dataframe = trainDataFramesList[[i]],
                                                          x_col = "Path",
                                                          y_col = "Instrumento",
                                                          class_mode = "other",
                                                          target_size = c(imageWidth, imageHeight), #c(224, 224)
                                                          generator = trainDatagen2,
                                                          shuffle = TRUE,
                                                          batch_size = 1)
            
            trainGeneratorList2[i] <- list(trainGenerator2)
            
        }
        # str(trainGeneratorList2) 
        
        # plot(as.raster(generator_next(trainGeneratorList2[[4]])[[1]][1,,,]))
        # generator_next(trainGeneratorList2[[4]])[[2]]
        
        valGeneratorList2 <- list()
        for (i in 1:number_classes) {
            valGenerator2 <- keras::flow_images_from_dataframe(dataframe = valDataFramesList[[i]],
                                                        x_col = "Path",
                                                        y_col = "Instrumento",
                                                        class_mode = "other",
                                                        target_size = c(imageWidth, imageHeight), #c(224, 224)
                                                        generator = trainDatagen2,
                                                        shuffle = TRUE,
                                                        batch_size = 1)
            
            valGeneratorList2[i] <- list(valGenerator2)
            
        }
        # str(valGeneratorList2) 
        
        # plot(as.raster(generator_next(valGeneratorList2[[3]])[[1]][1,,,]))
        # generator_next(valGeneratorList2[[3]])[[2]]
        
        
        
        join_generator <- function(generator_list, batch) { 
            function() { 
                batch_left <- NULL 
                batch_right <- NULL 
                similarity <- NULL 
                for (i in seq_len(batch)) {  
                    # front half 
                    if (i <= ceiling(batch/2)) { # It's suggest to use balance of positive and negative data set, so I divide half is 1(same) and another is 0(differnet).
                        grp_same <- sample(seq_len(number_classes), 1) 
                        batch_left <- abind::abind(batch_left, keras::generator_next(generator_list[[grp_same]])[[1]], along = 1) 
                        batch_right <- abind::abind(batch_right, keras::generator_next(generator_list[[grp_same]])[[1]], along = 1) 
                        similarity <- c(similarity, 1) # 1 : from the same number
                        
                        # after half     
                    } else {  
                        grp_diff <- sort(sample(seq_len(number_classes), 2)) 
                        batch_left <- abind::abind(batch_left, keras::generator_next(generator_list[[grp_diff[1]]])[[1]], along = 1) 
                        batch_right <- abind::abind(batch_right, keras::generator_next(generator_list[[grp_diff[2]]])[[1]], along = 1) 
                        similarity <- c(similarity, 0) # 0 : from the differnet number
                    } 
                } 
                return(list(list(batch_left, batch_right), similarity)) 
            } 
        }
        
        
        
        trainJoinGenerator2 <- join_generator(generator_list = trainGeneratorList2, batch = siameseModel_trainBatchSize)
        # str(trainJoinGenerator2)
        valJoinGenerator2 <- join_generator(generator_list = valGeneratorList2, batch = siameseModel_validationBatchSize)
        # str(valJoinGenerator2)
        
        
        trainImagesBatch2 <- trainJoinGenerator2()
        # par(mfrow = c(3, 1), mai = rep_len(0.01, 4))
        # for (i in 1:siameseModel_trainBatchSize) {
        #     # plot(as.raster(trainImagesBatch[[1]][[1]][i,,,]))
        #     # plot(as.raster(trainImagesBatch[[1]][[2]][i,,,]))
        #     plot(as.raster(abind(trainImagesBatch2[[1]][[1]][i,,,],
        #                          trainImagesBatch2[[1]][[2]][i,,,],
        #                          along = 2)))
        # }
        
        valImagesBatch2 <- valJoinGenerator2()
        # par(mfrow = c(3, 1), mai = rep_len(0.01, 4))
        # for (i in 1:siameseModel_validationBatchSize) {
        #     # plot(as.raster(valImagesBatch[[1]][[1]][i,,,]))
        #     # plot(as.raster(valImagesBatch[[1]][[2]][i,,,]))
        #     plot(as.raster(abind(valImagesBatch2[[1]][[1]][i,,,],
        #                          valImagesBatch2[[1]][[2]][i,,,],
        #                          along = 2)))
        # }
        
        
        saveWeightsCheckPointsCallback <- keras::callback_model_checkpoint(filepath = siameseModel_weights_filepath,
                                                                    monitor = "val_loss",
                                                                    save_weights_only = TRUE,
                                                                    mode = "min",
                                                                    verbose = 1)
        
        siameseModel_epochNumber_callback <- keras::callback_lambda(
            
            on_epoch_begin = function(epoch, logs) {
                shinyjs::html("siameseModelEpochNumber",
                              sprintf("%s %1.f/%1.f %s", "<b>Epoca</b>", epoch + 1, siameseModel_epocas, "<br>")
                              # add = TRUE
                )
            },
            
            
            on_batch_end = function(batch, logs) {
                
                batch_steps <- siameseModel_trainSteps
                
                
                shinyjs::html("siameseModelValAccEndBatch",
                              sprintf("%s %.4f", "<b>Batch Accuracy:</b> ", logs$python_function)
                              # add = TRUE
                )
                
                shinyjs::html("siameseModelValLossEndBatch",
                              sprintf("%s %.4f", "<b>Batch Loss:</b> ", logs$loss)
                              # add = TRUE
                )
                
                shinyjs::html("siameseModelProgressBar",
                              sprintf("%02.f/%02.f - %s%s%s%s%s%s%s%s%s %s",
                                      batch + 1, 
                                      batch_steps,
                                      "<font color='grey'>0%[</font>",
                                      "<font color='blue'>",
                                      paste(replicate(40*((batch + 1)/batch_steps),
                                                      "="), collapse = ""),
                                      ">",
                                      "</font>",
                                      "<font color='white'>",
                                      paste(replicate(40*(1 - ((batch + 1)/batch_steps)),
                                                      "_"), collapse = ""),
                                      "</font>",
                                      "<font color='grey'>]100%</font>",
                                      "<br>")
                              # add = TRUE
                )
                
            },
            
            on_epoch_end = function(epoch, logs) {
                
                shinyjs::html("siameseModelValAccMetric",
                              sprintf("%s %02.f %s %.4f %s",
                                      "<b>Epoch</b>",
                                      epoch + 1,
                                      "<b>Validation Accuracy:</b> ",
                                      logs$val_python_function,
                                      "<br>"),
                              add = TRUE
                )
                
                shinyjs::html("siameseModelValLossMetric",
                              sprintf("%s %02.f %s %.4f %s",
                                      "<b>Epoch</b>",
                                      epoch + 1,
                                      "<b>Validation Loss:</b> ",
                                      logs$val_loss,
                                      "<br>"), 
                              add = TRUE
                )
            }
        )
        
        create_model <- function(embedingSize) {
            
            left_input_tensor <- keras::layer_input(shape = c(imageWidth, imageHeight, 3), name = "left_input_tensor") #c(224, 224, 3)
            right_input_tensor <- keras::layer_input(shape = c(imageWidth, imageHeight, 3), name = "right_input_tensor")#c(224, 224, 3)
            
            mob <- keras::application_mobilenet(input_shape = c(imageWidth, imageHeight, 3), include_top = FALSE,  #c(224, 224, 3)
                                         pooling = "avg")
            
            conv_base <- keras::keras_model_sequential() %>%
                mob() %>%
                keras::layer_flatten(name = "layerFlatten") %>% 
                keras::layer_dense(units = 256, activation = "relu", name = "layerDense256") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.4) %>%
                keras::layer_dense(units = 128, activation = "relu", name = "layerDense128") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.3) %>%
                # layer_dense(units = 64, activation = "relu") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.2) %>%
                keras::layer_dense(units = embedingSize,  
                            activation = "relu", name = "layerDenseEmbedingSize") 
            
            left_output_tensor <- left_input_tensor  %>%
                conv_base
            
            right_output_tensor <- right_input_tensor %>%
                conv_base
            
            euclidean_distance <- function(vects) {
                c(x,y) %<-% vects
                sum_square <- keras::k_sum(keras::k_square(x - y), axis = as.integer(0), keepdims = TRUE)
                return(keras::k_sqrt(sum_square))
            }
            # euclidean_distance <- function(listxy) {
            #     x <- listxy$x
            #     y <- listxy$y
            #     sum_square <- k_sum(k_square(x - y), axis=1, keepdims=TRUE)
            #     return(k_sqrt(k_maximum(sum_square, k_epsilon())))
            # }
            
            # eucl_dist_output_shape <- function(shapes) {
            #     c(shapes1, shapes2) %<-% shapes
            #     return(list(shape1[[0]][0], 1))
            # }
            # eucl_dist_output_shape <- function(listxy) {
            #     layer1 <- listxy$x
            #     layer2 <- listxy$y
            #     return(c(layer1$shape[[2]], 1))
            # }
            
            euclidean_layer <- keras::layer_lambda(object = list(left_output_tensor, right_output_tensor), # To build self define layer, you must use layer_lamda
                                            f = euclidean_distance,
                                            # output_shape = eucl_dist_output_shape,
                                            name = "layerEuclidean", dtype = "float32") # original sin dtype = "float32" 
            
            model <- keras::keras_model(list(left_input_tensor, right_input_tensor), euclidean_layer)
            
            ContrastiveLoss <- function(y_true, y_pred) { 
                margin <- 1.2
                square_pred <- keras::k_square(y_pred)
                margin_square <- keras::k_square(keras::k_maximum(margin - y_pred, 0))
                return(keras::k_mean(y_true * square_pred + (1 - y_true) * margin_square))
            }
            
            my_accuracy <- function(y_true, y_pred) {
                return(keras::k_mean(keras::k_equal(y_true, keras::k_cast(y_pred < 0.5, dtype = "float32"))))
            }
            # my_accuracy <- function(y_true, y_pred) {
            #     return(k_mean(k_equal(y_true, as.numeric(y_pred < 0.5))))
            # }
            
            model %>%
                keras::compile(loss = ContrastiveLoss,     
                        optimizer = keras::optimizer_adam(lr = 1e-4),     
                        metrics = my_accuracy)
            
            model
        }
        
        model <- create_model(embedingSize = siameseModel_embedingSize)
        
        model %>% keras::fit_generator(trainJoinGenerator2,
                                steps_per_epoch = siameseModel_trainSteps,                
                                epochs = siameseModel_epocas,                           
                                validation_data = valJoinGenerator2,
                                validation_steps = siameseModel_validationSteps,
                                callbacks = list(saveWeightsCheckPointsCallback,
                                                 keras::callback_reduce_lr_on_plateau(factor = 0.05),
                                                 keras::callback_csv_logger(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Training/Metricas_SModel_ES{siameseModel_embedingSize}_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"), separator = ",", append = FALSE),
                                                 siameseModel_epochNumber_callback))
        
        
        
        
        
        
        siameseModel_metricas <- readr::read_csv(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Training/Metricas_SModel_ES{siameseModel_embedingSize}_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
        
        
        shinyjs::html("siameseModelEpochNumber", paste(""))
        shinyjs::html("siameseModelProgressBar", paste(""))
        shinyjs::html("siameseModelValAccEndBatch", paste(""))
        shinyjs::html("siameseModelValLossEndBatch", paste(""))
        shinyjs::html("siameseModelValAccMetric", paste(""))
        shinyjs::html("siameseModelValLossMetric", paste(""))
        
        
        output$siameseModelMetrics <- DT::renderDataTable({
            
            names(siameseModel_metricas) <- c("epoch", "contrastive_loss", "lr", "acc", "val_contrastive_loss", "val_acc")
            
            DT::datatable(siameseModel_metricas %>% 
                              dplyr::mutate_if(is.numeric, round, 4) %>% 
                              dplyr::select(-epoch),
                          options = list(lengthMenu = list(c(5, 15, -1), c("5", "15", "All")),
                                         pageLength = 5))
        })
        
        output$siameseModelMetricsPlot <- shiny::renderPlot({
            
            names(siameseModel_metricas) <- c("epoch", "loss", "lr", "acc", "val_loss", "val_acc")
            plt <- WVPlots::plot_Keras_fit_trajectory(
                siameseModel_metricas,
                title = "Performance del Siamese Model por epoca, dataset, y metrica",
                loss_pretty_name = glue::glue("contrastive_loss"),
                perf_pretty_name = "acc",
                perfname = "acc",
                fliploss = FALSE, 
                pick_metric = "acc")
            
            suppressWarnings(print(plt))
            
            ggplot2::ggsave(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Training/Metricas_SModel_ES{siameseModel_embedingSize}_Caja{numeroCaja}_Experimento{numeroExperimento}.png"))
        })
        
        
        shinyjs::enable(id = "NuevoExperimento")
        
        shinyjs::enable(id = "setType")
        
        shinyjs::enable(id = "etiqueta")
        
        shinyjs::enable(id = "siameseModelTrainBatchSize")
        shinyjs::enable(id = "siameseModelTrainSteps")
        shinyjs::enable(id = "siameseModelValidationBatchSize")
        shinyjs::enable(id = "siameseModelValidationSteps")
        shinyjs::enable(id = "siameseModelEmbedingSize")
        shinyjs::enable(id = "siameseModelEpocas")
        shinyjs::enable(id = "siameseModelTrain")
        shinyjs::enable(id = "siameseModelBestEpoch")
        shinyjs::enable(id = "bestSiameseModelWeight")
        
        shiny::updateSelectInput(session, 
                          inputId = "siameseModelBestEpoch",
                          choices = {1:input$siameseModelEpocas},
                          selected = NULL)
    })
    
    # 10. CHOOSE BEST SIAMESE_MODEL WEIGHTS EPOCHS AND DELETE THE OTHERS  ----     
    observeEvent(input$bestSiameseModelWeight, {
        
        shinyjs::disable(id = "experimento")
        shinyjs::disable(id = "CrearExperimento")
        shinyjs::disable(id = "NuevoExperimento")
        
        shinyjs::disable(id = "setType")
        
        shinyjs::disable(id = "BurstSnapshot")
        shinyjs::disable(id = "stop_BurstSnapshot")
        shinyjs::disable(id = "snapshot")
        
        shinyjs::disable(id = "etiqueta")
        
        shinyjs::disable(id = "siameseModelTrainBatchSize")
        shinyjs::disable(id = "siameseModelTrainSteps")
        shinyjs::disable(id = "siameseModelValidationBatchSize")
        shinyjs::disable(id = "siameseModelValidationSteps")
        shinyjs::disable(id = "siameseModelEmbedingSize")
        shinyjs::disable(id = "siameseModelEpocas")
        shinyjs::disable(id = "siameseModelTrain")
        shinyjs::disable(id = "siameseModelBestEpoch")
        shinyjs::disable(id = "bestSiameseModelWeight")
        
        shinyjs::disable(id = "chooseModel")
        shinyjs::disable(id = "generarModelo")
        shinyjs::disable(id = "prediction")
        
        workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
        numeroCaja <- input$caja
        numeroExperimento <- input$experimento
        siameseModel_embedingSize <- input$siameseModelEmbedingSize %>% as.numeric()
        siameseModel_best_epoch <- input$siameseModelBestEpoch %>% as.numeric() %>% sprintf("%02d", .)
        # experimentFolderName <- glue("Experimento{numeroExperimento}")
        
        for (i in 1:length(siameseModel_best_epoch)) {
            
            saveDate <- format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss")
            siameseModel_best_weigths <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Modelos/SModel_Weights_ES{siameseModel_embedingSize}_E{siameseModel_best_epoch[i]}_Caja{numeroCaja}_Experimento{numeroExperimento}.hdf5")
            
            file.rename(from = siameseModel_best_weigths, to = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Modelos/SModel_ES{siameseModel_embedingSize}_E{siameseModel_best_epoch[i]}_Caja{numeroCaja}_Experimento{numeroExperimento}_{saveDate}.hdf5"))
            
        }
        
        filesToRemove <- list.files(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Modelos/"), 
                                    pattern = glue::glue("SModel_Weights_ES{siameseModel_embedingSize}_"), 
                                    full.names = TRUE)
        file.remove(filesToRemove)
        
        shinyjs::enable(id = "NuevoExperimento")
        
        shinyjs::enable(id = "setType")
        
        shinyjs::enable(id = "etiqueta")
        
        shinyjs::enable(id = "siameseModelTrainBatchSize")
        shinyjs::enable(id = "siameseModelTrainSteps")
        shinyjs::enable(id = "siameseModelValidationBatchSize")
        shinyjs::enable(id = "siameseModelValidationSteps")
        shinyjs::enable(id = "siameseModelEmbedingSize")
        shinyjs::enable(id = "siameseModelEpocas")
        shinyjs::enable(id = "siameseModelTrain")
        shinyjs::enable(id = "siameseModelBestEpoch")
        shinyjs::enable(id = "bestSiameseModelWeight")
        
        shinyjs::enable(id = "chooseModel")
        # enable(id = "generarModelo")
        # enable(id = "prediction")
        
        shinyjs::enable(id = "chooseSiameseModel")
        # enable(id = "generarSiameseModelo")
        # enable(id = "predictionSiamese")
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 15. CONECTAR ARDUINO ----
    # Observe ports ----
    observeEvent(input$ports, {
        
        shinyjs::enable(id = "COM")
        
        puertos <- serial::listPorts() %>% tibble::enframe(name = NULL)
        # print(puertos)
        
        shiny::updateSelectInput(session,
                                 inputId = "COM",
                                 label = NULL,
                                 choices = puertos$value,
                                 selected = NULL)
        
    })
    
    # Observe connectArduino ----
    observeEvent(input$connectArduino, {
        
        com <- input$COM
        
        values$conn <- serial::serialConnection("arduino",
                                          port = glue::glue("{com}"),
                                          mode = "9600,n,8,1")
        
        open(values$conn)
        
        # output$connectionStatus <- shiny::renderPrint({summary(conn)})
        
        # print(summary(conn))
        
        shinyjs::enable(id = "chooseProductionModel")
    })
    
    # 16. INFERENCE SIAMESE MODEL ----
    # Observe generarProductionModel ----
    observeEvent(input$generarProductionModel, {
        
        shinyjs::disable(id = "experimento")
        shinyjs::disable(id = "CrearExperimento")
        shinyjs::disable(id = "NuevoExperimento")
        
        shinyjs::disable(id = "chooseProductionModel")
        shinyjs::disable(id = "generarProductionModel")
        shinyjs::disable(id = "inferenceSnapshot")
        
        inFileProductionModel <- input$chooseProductionModel
        
        if (inFileProductionModel$name %>% stringr::str_remove(".*EmbSize") %>% stringr::str_remove("_.*") == "siameseModel") {
            siameseModel_EmbedingSize <- input$siameseModelEmbedingSize %>% as.numeric()
        } else {
            siameseModel_EmbedingSize <- inFileProductionModel$name %>% stringr::str_remove(".*ES") %>% stringr::str_remove("_.*") %>% as.numeric()
        }
        
        # Crear Modelo productivo ----
        create_production_model <- function(embedingSize = siameseModel_EmbedingSize) {
            
            left_input_tensor <- keras::layer_input(shape = c(imageWidth, imageHeight, 3), name = "left_input_tensor")
            right_input_tensor <- keras::layer_input(shape = c(imageWidth, imageHeight, 3), name = "right_input_tensor")
            
            mob <- keras::application_mobilenet(input_shape = c(imageWidth, imageHeight, 3), include_top = FALSE,
                                                pooling = "avg")
            
            conv_base <- keras::keras_model_sequential() %>%
                mob() %>%
                keras::layer_flatten(name = "layerFlatten") %>%
                keras::layer_dense(units = 256, activation = "relu", name = "layerDense256") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.4) %>%
                keras::layer_dense(units = 128, activation = "relu", name = "layerDense128") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.3) %>%
                # layer_dense(units = 64, activation = "relu") %>%
                # layer_batch_normalization() %>%
                # layer_dropout(rate = 0.2) %>%
                keras::layer_dense(units = embedingSize,
                                   activation = "relu", name = "layerDenseEmbedingSize")
            
            left_output_tensor <- left_input_tensor  %>%
                conv_base
            
            right_output_tensor <- right_input_tensor %>%
                conv_base
            
            euclidean_distance <- function(vects) {
                c(x,y) %<-% vects
                sum_square <- keras::k_sum(keras::k_square(x - y), axis = as.integer(0), keepdims = TRUE)
                return(keras::k_sqrt(sum_square))
            }
            
            euclidean_layer <- keras::layer_lambda(object = list(left_output_tensor, right_output_tensor), # To build self define layer, you must use layer_lamda
                                                   f = euclidean_distance,
                                                   name = "layerEuclidean")
            
            model <- keras::keras_model(list(left_input_tensor, right_input_tensor), euclidean_layer)
            
            model
        }
        
        values$productionModel <- create_production_model()  # productionModel <<- create_production_model()
        
        values$productionModel %>% keras::load_model_weights_hdf5(filepath = inFileProductionModel$datapath,
                                                           # by_name = TRUE
        )
        
        shinyjs::enable(id = "NuevoExperimento")
        
        shinyjs::enable(id = "chooseProductionModel")
        shinyjs::enable(id = "inferenceSnapshot")
        shinyjs::enable(id = "chooseLiveInferenceImage")
    })
    
    
    # observeEvent(input$ports, {
    #
    #     puertos <- serial::listPorts() %>% tibble::enframe(name = NULL)
    #     # print(puertos)
    #
    #     shiny::updateSelectInput(session,
    #                              inputId = "COM",
    #                              label = NULL,
    #                              choices = puertos$value,
    #                              selected = NULL)
    #
    # })
    
    # Observe inferenceSnapshot ----
    observeEvent(input$inferenceSnapshot, {
        # Ejecutar siempre que no sea la primera vez con un código no válido
        if (!identical(input$InferenceImagePlaceholder64,"not_valid")) {
            
            shinyjs::disable(id = "experimento")
            shinyjs::disable(id = "CrearExperimento")
            shinyjs::disable(id = "NuevoExperimento")
            
            shinyjs::disable(id = "chooseProductionModel")
            shinyjs::disable(id = "generarProductionModel")
            shinyjs::disable(id = "inferenceSnapshot")
            
            # !Completitud caja: NO, SI ----
            if (any(values$inventario_df$Cantidad_actual > 0) == FALSE) {
                
                # NO: Caja completa, detener ----
                senial <- "1"
                msg <- "rojo, caja completa. Cambie inventario y/o numero de caja"
                shinyalert::shinyalert("Caja completa! Para continuar, escoja otro inventario y/o otra caja.", type = "error")
                senial2 <- "5"
                msg2 <- "-"
                print(senial)
                print(class(senial))
                print(values$inventario_df)
                print(msg)
                print(msg2)
                
            } else {
                
                # SI: Caja incompleta, continuar ----
                inFileProductionModelName <- input$chooseProductionModel$name %>% gsub(".hdf5", "", .)
                print(inFileProductionModelName)
                
                inconn <- stringr::str_remove(input$InferenceImagePlaceholder64,'data:image/jpeg;base64,')
                
                # Crear archivo donde guardar imagen
                workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
                
                numeroCaja <- input$caja
                numeroExperimento <- input$experimento
                
                saveImagesPath <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Inferencia")
                
                fileName_cam1 <- sprintf("%s_%s_%s_%s",
                                         liveCounter(),
                                         "Raw",
                                         "Cam1",
                                         format(Sys.time(), "%d-%m-%Y_%Hh%Mm%Ss"))  # chartr(" :-", "___", format(Sys.time(), "%F %X")))
                
                output$liveImagenCam1 <- renderImage({
                    imageName <- glue::glue("{saveImagesPath}/{fileName_cam1}.jpeg")
                    list(src = imageName,
                         width = 240,
                         height = 240)
                }, deleteFile = FALSE)
                
                output$labelLiveImagenCam1 <- renderText({
                    fileName_cam1
                })
                
                outconn <- file(description = paste0(saveImagesPath, "/", fileName_cam1, ".jpeg"),
                                open = "wb")
                
                # Guardar imagen a disco duro
                base64enc::base64decode(what = inconn, output = outconn)
                close(outconn)
                
                # Aumentar contador
                nwLiveCnt <- liveCounter() + 1
                liveCounter(nwLiveCnt)
                
                ima_path <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Imagenes_Inferencia/{fileName_cam1}.jpeg")
                print(ima_path)
                
                # set <- "live"
                
                anchors_path <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Anchor_Caja{numeroCaja}_Experimento{numeroExperimento}.csv")
                print(anchors_path)
                
                anchorsDataFramesList <- readr::read_csv(anchors_path) %>%
                    dplyr::filter(Set == "Anchors") %>%
                    dplyr::filter(Transformacion == "Brightness100") %>%
                    dplyr::select(Instrumento, Path)
                
                # Funcion de Disimilaridad inferenceSnapshot ----
                inference_Dissimilarity <- function(dataSet, imageToInfere) {
                    
                    set <- dataSet
                    # testDataFrames_tbl <- imageToInfere
                    anchorsDataFrames_tbl <- anchorsDataFramesList %>% dplyr::bind_rows()
                    diss <- tibble::tibble()
                    imageLeft <- tibble::tibble()
                    imageRight <- tibble::tibble()
                    imageLeftTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                    imageRightTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                    labelLeft <- tibble::tibble()
                    labelRight <- tibble::tibble()
                    dissTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                    labelLeftTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                    labelRightTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                    
                    
                    img_left <- imageToInfere %>% gsub(".jpeg", "", .)
                    label_img_left <- gsub(".*/|_Raw.*", "", fileName_cam1)
                    
                    imgInputLeft <- magick::image_read(imageToInfere) %>%
                        magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>%
                        magick::image_edge() %>%
                        magick::image_negate() %>%
                        magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth - 3, height = imageHeight - 3, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                        magick::image_data("gray") %>%
                        as.integer() %>%
                        sketcher::sketch(style = 2,
                                         lineweight = 4,
                                         smooth = 3,
                                         gain = 0.1,
                                         contrast = 5,
                                         shadow = 19) %>%
                        magick::image_read() %>%
                        magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth, height = imageHeight, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                        magick::image_data('rgb') %>%
                        as.integer() %>%
                        keras::image_to_array() %>%
                        reticulate::array_reshape(c(1, imageWidth, imageHeight, 3))
                    imgInputLeft <- imgInputLeft/255
                    
                    
                    for (j in 1:as.integer(nrow(anchorsDataFrames_tbl))) {
                        
                        
                        filter_idx_right <- anchorsDataFrames_tbl[j,]
                        img_right <- filter_idx_right$Path
                        label_img_right <- filter_idx_right$Instrumento
                        
                        imgInputRight <- magick::image_read(img_right) %>%
                            # image_edge() %>%
                            # image_negate() %>%
                            magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth, height = imageHeight, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                            magick::image_data('rgb') %>%
                            as.integer() %>%
                            keras::image_to_array() %>%
                            reticulate::array_reshape(c(1, imageWidth, imageHeight, 3))
                        imgInputRight <- imgInputRight/255
                        
                        dissTemp[j,] <- values$productionModel %>% keras::predict_on_batch(list(imgInputLeft, imgInputRight))
                        print(dissTemp[j,])
                        
                        imageLeftTemp[j,] <- gsub(".*/|_Raw.*", "", img_left)
                        imageRightTemp[j,] <- gsub(".*/|_Brightness.*", "", img_right)
                        
                        labelLeftTemp[j,] <- paste0(label_img_left)
                        labelRightTemp[j,] <- paste0(label_img_right)
                        
                        
                    }
                    
                    imageLeft <- dplyr::bind_rows(imageLeft, imageLeftTemp)
                    imageRight <- dplyr::bind_rows(imageRight, imageRightTemp)
                    
                    diss <- dplyr::bind_rows(diss, dissTemp)
                    
                    labelLeft <- dplyr::bind_rows(labelLeft, labelLeftTemp)
                    labelRight <- dplyr::bind_rows(labelRight, labelRightTemp)
                    
                    
                    
                    dissList <- dplyr::bind_cols(imageLeft, imageRight, labelLeft, diss, labelRight) %>%
                        # rename(imagen = V1, anchor = V11, clase_real = V12, dist = V13, clase_referencia = V14) %>%
                        dplyr::rename(imagen = V1...1, anchor = V1...2, clase_real = V1...3, dist = V1...4, clase_referencia = V1...5) %>%
                        dplyr::mutate(clase_real = clase_real %>% forcats::as_factor(),
                                      clase_referencia = clase_referencia %>% forcats::as_factor()) %>%
                        dplyr::group_split(clase_real)
                    
                    # liveTime <- chartr(" :-", "___", format(Sys.time(), "%F %X"))
                    for (i in 1:length(dissList)) {
                        
                        
                        ggplot2::ggplot(data = dissList[[i]] %>% dplyr::select(dist, clase_referencia) %>% reshape2::melt("clase_referencia"),
                                        ggplot2::aes(x = clase_referencia, y = value, group = clase_referencia)) +
                            ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
                            ggplot2::labs(title = "Inference Distance",
                                          subtitle = glue::glue("{fileName_cam1}"),
                                          caption = glue::glue("Experimento{numeroExperimento} - {inFileProductionModelName}"),
                                          y = "distancia") +
                            ggplot2::geom_hline(yintercept = 1, color = "red") +
                            ggplot2::coord_flip() +
                            ggplot2::coord_polar(theta = "x") +
                            ggplot2::theme_bw() +
                            ggplot2::xlab(NULL)
                        
                        ggplot2::ggsave(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png"))
                        
                        
                    }
                    
                    dissList
                }
                
                
                liveDissimilarityList <- inference_Dissimilarity(dataSet = "Live", imageToInfere = ima_path)
                
                readr::write_csv(liveDissimilarityList %>% dplyr::bind_rows(),
                                 path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"))
                
                output$inferencePlot <- renderImage({
                    inferencePlotName <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png")
                    list(src = inferencePlotName,
                         width = 400,
                         height = 350)
                }, deleteFile = FALSE)
                
                output$livePredicciones <- renderTable(
                    inferencePrediction %>%
                        dplyr::select(clase_predicha, dmin, accion) %>%
                        dplyr::mutate_if(is.numeric, round, 6)
                )
                
                # Logica Inferencia InferenceSnapshot ----
                df_1 <- readr::read_csv(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"))
                ima <- gsub(".*/|_Raw.*", "", fileName_cam1)
                t1 <- 0.5
                t2 <- 1.0
                
                # Imagen individual
                df1 <- df_1 %>%
                    # filter(imagen == ima) %>%
                    dplyr::select(imagen, anchor, clase_referencia, dist)
                
                df2 <- df1 %>%
                    dplyr::group_by(clase_referencia) %>%
                    dplyr::mutate(dAmin = min(dist),
                                  DAmin = abs(dist[1] - dist[2])) %>%
                    dplyr::summarise(id = which.min(dist),
                                     imagen = imagen[id],
                                     anchor = anchor[id],
                                     min_dist = dist[id],
                                     clase_referencia = clase_referencia[id],
                                     dAmin = dAmin[id],
                                     DAmin = DAmin[id]) %>%
                    dplyr::select(id, imagen, clase_referencia, dAmin, DAmin)
                
                df3 <- df2 %>%
                    dplyr::group_by(imagen) %>%
                    dplyr::mutate(dmin = min(dAmin),
                                  Dmin = min(DAmin)) %>%
                    dplyr::filter(dAmin <= t1) %>%
                    dplyr::summarise(id_k = which.min(dAmin),
                                     id_l = which.min(DAmin),
                                     imagen = imagen[id_k],
                                     dmin = dmin[id_k],
                                     Dmin = Dmin[id_l],
                                     clase_predicha_k = clase_referencia[id_k],
                                     clase_predicha_l = clase_referencia[id_l],
                                     clase_predicha = ifelse(clase_predicha_k == clase_predicha_l, clase_referencia[id_k],
                                                             ifelse(clase_predicha_k != clase_predicha_l & Dmin >= 0.2, clase_referencia[id_k], clase_referencia[id_l])))
                if (nrow(df3) == 0) {
                    inferencePrediction <- tibble::tibble(imagen = ima,
                                                          id_k = "-",
                                                          id_l = "-",
                                                          dmin = "-",
                                                          Dmin = "-",
                                                          clase_predicha_k = "-",
                                                          clase_predicha_l = "-",
                                                          clase_predicha = "objeto no pertenece a la caja",
                                                          accion = "rojo",
                                                          bit = 1) %>%
                        dplyr::select(imagen, id_k, id_l, dmin, Dmin, clase_predicha_k, clase_predicha_l, clase_predicha, accion, bit)
                    
                } else {
                    inferencePrediction <- df3 %>%
                        dplyr::mutate(accion = ifelse(dmin > t2, "rojo",
                                                      ifelse(dmin > t1 & dmin <= t2, "amarillo",
                                                             ifelse(clase_predicha == clase_predicha_k & dmin <= t1, "verde", "amarillo")))) %>%
                        dplyr::mutate(bit = ifelse(accion == "verde", 2,
                                                   ifelse(accion == "amarillo", 3, 1))) %>%
                        dplyr::select(imagen, id_k, id_l, dmin, Dmin, clase_predicha_k, clase_predicha_l, clase_predicha, accion)
                }
                
                
                readr::write_csv(inferencePrediction,
                                 path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferencePrediction_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"),
                                 append = FALSE,
                                 col_names = TRUE)
                
                accion <- inferencePrediction$accion
                print(accion)
                
                # Inferencia: NO, DUDA, SI ----
                if (accion != "verde") {
                    
                    # DUDA: Amarillo, actualizar a mano inventario ----
                    bit <- inferencePrediction$bit
                    print(bit)
                    
                    if (bit == 3) {
                        
                        senial <- "3"
                        msg <- "amarillo, por favor revisar instrumento a mano"
                        shinyalert::shinyalert("Por favor revisar instrumento a mano",
                                               type = "input",
                                               inputType = "text",
                                               inputPlaceholder = "ingrese instrumento",
                                               callbackR = function(x) { instrumentos <- values$inventario_df$Instrumento
                                               if (x %in% instrumentos) {
                                                   values$inventario_df <- values$inventario_df %>%  # inventario_df <<- inventario_df %>%
                                                       dplyr::mutate(Contador = dplyr::case_when(Instrumento == x ~ Contador + 1,
                                                                                                 TRUE ~ Contador),
                                                                     Cantidad_actual = dplyr::case_when(Instrumento == x ~ Cantidad - Contador,
                                                                                                        TRUE ~ Cantidad_actual))
                                                   
                                                   y <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                                                       Cantidad = NA,
                                                                       Contador = NA,
                                                                       Cantidad_actual = NA,
                                                                       Ingreso_manual = x,
                                                                       Imagen = fileName_cam1,
                                                                       Signal1 = senial,
                                                                       Accion1 = msg,
                                                                       Signal2 = senial2,
                                                                       Accion2 = msg2)
                                                   
                                                   values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(y)  # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(y)
                                                   
                                                   inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial)
                                                   readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                                                   
                                                   print(values$inventario_df)
                                               } else {
                                                   values$inventario_df <- values$inventario_df # inventario_df <<- inventario_df
                                                   
                                                   y <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                                                       Cantidad = NA,
                                                                       Contador = NA,
                                                                       Cantidad_actual = NA,
                                                                       Ingreso_manual = x,
                                                                       Imagen = fileName_cam1,
                                                                       Signal1 = senial,
                                                                       Accion1 = msg,
                                                                       Signal2 = senial2,
                                                                       Accion2 = msg2)
                                                   
                                                   values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(y)  # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(y)
                                                   
                                                   inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial)
                                                   readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                                                   
                                                   print(values$inventario_df)
                                               }
                                               })
                        senial2 <- "5"
                        msg2 <- "-"
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        print(msg2)
                        
                    } else {
                        
                        # NO: Rojo, objeto no pertenece a la caja ----
                        senial <- "1"
                        msg <- "rojo, instrumento no pertenece a la caja"
                        shinyalert::shinyalert("Instrumento no pertenece a la caja", type = "error")
                        senial2 <- "5"
                        msg2 <- "-"
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        print(msg2)
                        
                    }
                    
                    x <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                        Cantidad = NA,
                                        Contador = NA,
                                        Cantidad_actual = NA,
                                        Imagen = fileName_cam1,
                                        Signal1 = senial,
                                        Accion1 = msg,
                                        Signal2 = senial2,
                                        Accion2 = msg2)
                    
                    values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(x) # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(x)
                    
                    inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial)
                    readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                    
                    
                } else {
                    
                    # SI: Pertenencia al inventario: NO, SI ----
                    clase_predicha <- inferencePrediction$clase_predicha
                    instrumentos <- values$inventario_df$Instrumento
                    
                    # NO: Objeto no esta en inventario ----
                    if (!(clase_predicha %in% instrumentos)) {
                        
                        senial <- "1"
                        msg <- "rojo, instrumento no pertenece al inventario"
                        shinyalert::shinyalert("Instrumento no pertenece al inventario", type = "error")
                        senial2 <- "5"
                        msg2 <- "-"
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        print(msg2)
                        
                    } else {
                        
                        # SI: !Completitud inventario: NO, SI ----
                        values$inventario_df <- values$inventario_df %>%  # inventario_df <<- inventario_df %>%
                            dplyr::mutate(Contador = dplyr::case_when(Instrumento == clase_predicha ~ Contador + 1,
                                                                      TRUE ~ Contador),
                                          Cantidad_actual = dplyr::case_when(Instrumento == clase_predicha ~ Cantidad - Contador,
                                                                             TRUE ~ Cantidad_actual))
                        print(values$inventario_df)
                        
                        cantidad_actual_k <- values$inventario_df %>%
                            dplyr::filter(Instrumento == clase_predicha)
                        
                        cantidad_instrumentos_restantes <- values$inventario_df %>%
                            dplyr::filter(Cantidad_actual >= 0) %>%
                            dplyr::select(Cantidad_actual) %>%
                            sum()
                        
                        if (cantidad_actual_k$Cantidad_actual >= 0 & cantidad_instrumentos_restantes > 0) {
                            
                            senial <- "2"
                            msg <- "verde, poner instrumento en la caja"
                            shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                            senial2 <- "5"
                            msg2 <- "-"
                            print(senial)
                            print(class(senial))
                            print(values$inventario_df)
                            print(msg)
                            print(msg2)
                            
                        } else if (cantidad_actual_k$Cantidad_actual < 0) {
                            
                            senial <- "1"
                            msg <- "rojo, instrumento completo en inventario"
                            shinyalert::shinyalert("Instrumento completo en inventario!", type = "error")
                            senial2 <- "5"
                            msg2 <- "-"
                            print(senial)
                            print(class(senial))
                            print(values$inventario_df)
                            print(msg)
                            print(msg2)
                            
                        } else if (cantidad_actual_k$Cantidad_actual == 0 & cantidad_instrumentos_restantes == 0) {
                            
                            senial <- "2"
                            msg <- "verde, poner instrumento en la caja"
                            shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                            print(senial)
                            print(class(senial))
                            print(values$inventario_df)
                            print(msg)
                            
                            senial2 <- "4"
                            msg2 <- "blanco, caja completa"
                            shinyalert::shinyalert("La caja ha sido completada!", type = "success")
                            print(senial2)
                            print(class(senial2))
                            print(values$inventario_df)
                            print(msg2)
                            
                        }
                    }
                    
                    x <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                        Cantidad = NA,
                                        Contador = NA,
                                        Cantidad_actual = NA,
                                        Imagen = fileName_cam1,
                                        Signal1 = senial,
                                        Accion1 = msg,
                                        Signal2 = senial2,
                                        Accion2 = msg2)
                    
                    values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(x)  # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(x)
                    
                    inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial)
                    readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                    
                }
                
            } # End if !Completitud caja
            
            serial::write.serialConnection(values$conn, senial)
            Sys.sleep(2)
            serial::write.serialConnection(values$conn, senial2)
            
            shinyjs::enable(id = "NuevoExperimento")
            
            shinyjs::enable(id = "chooseProductionModel")
            shinyjs::enable(id = "inferenceSnapshot")
            
            # if (accion != "verde") {
            #     bit <- inferencePrediction$bit
            #     print(bit)
            #     if (bit == 3) {
            #         senial <- "3"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(glue("enviando {senial} -> amarillo al arduino, por favor revisar instrumento a mano"))
            #     }
            #     else {
            #         senial <- "1"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(glue("enviando {senial} -> rojo al arduino, instrumento no corresponde a la caja"))
            #     }
            # } else {
            #     clase_predicha <- inferencePrediction$clase_predicha
            #     inventario_df <<- inventario_df %>%
            #         mutate(Contador = case_when(Instrumento == clase_predicha ~ Contador + 1,
            #                                     TRUE ~ Contador),
            #                Cantidad_actual = case_when(Instrumento == clase_predicha ~ Cantidad - Contador,
            #                                            TRUE ~ Cantidad_actual))
            #     print(inventario_df)
            #
            #     cantidad_actual_k <- inventario_df %>%
            #         filter(Instrumento == clase_predicha)
            #
            #     cantidad_instrumentos_restantes <- inventario_df %>%
            #         filter(Cantidad_actual >= 0) %>%
            #         select(Cantidad_actual) %>%
            #         sum()
            #
            #     if (cantidad_actual_k$Cantidad_actual >= 0 & cantidad_instrumentos_restantes > 0) {
            #
            #         senial <- "2"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(glue("enviando {senial} -> verde al arduino, poner instrumento {clase_predicha} en la caja"))
            #         #
            #         # com <- input$COM %>% as.character()
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(15)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     } else if (cantidad_actual_k$Cantidad_actual < 0) {
            #
            #         senial <- "1"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(glue("enviando {senial} -> rojo al arduino, instrumento {clase_predicha} completo en inventario"))
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     } else if (cantidad_actual_k$Cantidad_actual == 0 & cantidad_instrumentos_restantes == 0) {
            #
            #         senial <- "2"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(glue("enviando {senial} -> verde al arduino, poner instrumento {clase_predicha} en la caja"))
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #         #
            #         # Sys.sleep(5)
            #
            #         senial_2 <- "4"
            #         print(senial_2)
            #         print(class(senial_2))
            #         print(inventario_df)
            #         print(glue("enviando {senial_2} -> blanco al arduino, caja completa"))
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial_2)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     }
            # }
            #
            # x <- tibble(Instrumento = inferencePrediction$clase_predicha,
            #             Cantidad = NA,
            #             Contador = NA,
            #             Cantidad_actual = NA,
            #             Imagen = fileName_cam1)
            #
            # inventarioActual <- inventario_df %>% bind_rows(x)
            # write_csv(inventarioActual, glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}.csv"))
            #
            # # dato <- 1 %>% as.character()
            # # print(dato)
            # # print(class(dato))
            # # print(inventario_df)
            # # print(glue("enviando {dato} -> verde al arduino, poner instrumento {clase_predicha} en la caja"))
            # #
            # com <- "COM3"
            # print(com)
            # print(class(com))
            # # conn <- serial::serialConnection("arduino",
            # #                                  port = "COM3",
            # #                                  mode = "9600,n,8,1")
            # # open(conn)
            # summary(conn)
            #
            # # senial <- inferencePrediction$bit
            # # print(dato)
            #
            # serial::write.serialConnection(conn, senial)
            #
            # # Sys.sleep(15)
            #
            # # close(conn)
            # # summary(conn)
            #
            # # output$inferencePlot <- renderImage({
            # #     inferencePlotName <- glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png")
            # #     list(src = inferencePlotName,
            # #          width = 400,
            # #          height = 350)
            # # }, deleteFile = FALSE)
            # #
            # # output$livePredicciones <- renderTable(
            # #     inferencePrediction %>%
            # #         select(clase_predicha, dmin, accion) %>%
            # #         mutate_if(is.numeric, round, 6)
            # # )
            
            shinyjs::enable(id = "NuevoExperimento")
            
            shinyjs::enable(id = "chooseProductionModel")
            shinyjs::enable(id = "inferenceSnapshot")
            
        }
    })
    
    # Observe inferenceWithUploadedImage ----
    observeEvent(input$inferenceWithUploadedImage, {
        
        shinyjs::disable(id = "experimento")
        shinyjs::disable(id = "CrearExperimento")
        shinyjs::disable(id = "NuevoExperimento")
        
        shinyjs::disable(id = "chooseProductionModel")
        shinyjs::disable(id = "generarProductionModel")
        shinyjs::disable(id = "inferenceSnapshot")
        
        
        # !Completitud caja: NO, SI ----
        if (any(values$inventario_df$Cantidad_actual > 0) == FALSE) {
            
            # NO: Caja completa, detener ----
            senial <- "1"
            msg <- "rojo, caja completa. Cambie inventario y/o numero de caja"
            shinyalert::shinyalert("Caja completa! Para continuar, escoja otro inventario y/o otra caja.", type = "error")
            senial2 <- "5"
            msg2 <- "-"
            print(senial)
            print(class(senial))
            print(values$inventario_df)
            print(msg)
            print(msg2)
            
        } else {
            
            # SI: Caja incompleta, continuar ----
            inFileProductionModelName <- input$chooseProductionModel$name %>% gsub(".hdf5", "", .)
            print(inFileProductionModelName)
            
            fileName_cam1 <- input$chooseLiveInferenceImage$name %>% gsub(".jpeg", "", .)
            print(fileName_cam1)
            print(input$chooseLiveInferenceImage$datapath)
            
            # Crear archivo donde guardar imagen
            workingFolderName <- shinyFiles::parseDirPath(volumes, input$WorkingDirectory)
            
            numeroCaja <- input$caja
            numeroExperimento <- input$experimento
            
            output$liveImagenCam1 <- renderImage({
                imageName <- input$chooseLiveInferenceImage$datapath # glue("{saveImagesPath}/{fileName_cam1}.jpeg")
                list(src = imageName,
                     width = 240,
                     height = 240)
            }, deleteFile = FALSE)
            
            output$labelLiveImagenCam1 <- renderText({
                fileName_cam1
            })
            
            # Aumentar contador
            nwLiveCnt <- liveCounter() + 1
            liveCounter(nwLiveCnt)
            
            ima_path <- input$chooseLiveInferenceImage$datapath # glue("{saveImagesPath}/{fileName_cam1}.jpeg")
            print(ima_path)
            
            # set <- "Live"
            
            anchors_path <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Documentos/Reporte_Preprocesamiento_Anchor_Caja{numeroCaja}_Experimento{numeroExperimento}.csv")
            print(anchors_path)
            
            anchorsDataFramesList <- readr::read_csv(anchors_path) %>%
                dplyr::filter(Set == "Anchors") %>%
                dplyr::filter(Transformacion == "Brightness100") %>%
                dplyr::select(Instrumento, Path)
            # %>%
            #     mutate(Path = gsub(".*/Caja", "", Path))
            print(anchorsDataFramesList$Instrumento)
            print(anchorsDataFramesList$Path)
            
            # Funcion de Disimilaridad inferenceWithUploadedImage ----
            inference_Dissimilarity <- function(dataSet, imageToInfere) {
                
                set <- dataSet
                # testDataFrames_tbl <- imageToInfere
                anchorsDataFrames_tbl <- anchorsDataFramesList %>% dplyr::bind_rows()
                diss <- tibble::tibble()
                imageLeft <- tibble::tibble()
                imageRight <- tibble::tibble()
                imageLeftTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                imageRightTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                labelLeft <- tibble::tibble()
                labelRight <- tibble::tibble()
                dissTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                labelLeftTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                labelRightTemp <- matrix(ncol = 1, nrow = nrow(anchorsDataFrames_tbl)) %>% tibble::as_tibble()
                
                
                img_left <- fileName_cam1 %>% gsub(".jpeg", "", .)
                label_img_left <- gsub(".*/|_.*", "", fileName_cam1)
                
                imgInputLeft <- magick::image_read(imageToInfere) %>%
                    magick::image_crop(magick::geometry_area(x_off = 2, y_off = 2), repage = FALSE) %>%
                    magick::image_edge() %>%
                    magick::image_negate() %>%
                    magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth - 3, height = imageHeight - 3, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                    magick::image_data("gray") %>%
                    as.integer() %>%
                    sketcher::sketch(style = 2,
                                     lineweight = 4,
                                     smooth = 3,
                                     gain = 0.1,
                                     contrast = 5,
                                     shadow = 19) %>%
                    magick::image_read() %>%
                    magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth, height = imageHeight, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                    magick::image_data('rgb') %>%
                    as.integer() %>%
                    keras::image_to_array() %>%
                    reticulate::array_reshape(c(1, imageWidth, imageHeight, 3))
                imgInputLeft <- imgInputLeft/255
                
                
                for (j in 1:as.integer(nrow(anchorsDataFrames_tbl))) {
                    
                    
                    filter_idx_right <- anchorsDataFrames_tbl[j,]
                    img_right <- filter_idx_right$Path
                    label_img_right <- filter_idx_right$Instrumento
                    
                    imgInputRight <- magick::image_read(img_right) %>%
                        # image_edge() %>%
                        # image_negate() %>%
                        magick::image_resize(geometry = magick::geometry_size_pixels(width = imageWidth, height = imageHeight, preserve_aspect = F)) %>% #opcional, usado para experimento_70
                        magick::image_data('rgb') %>%
                        as.integer() %>%
                        keras::image_to_array() %>%
                        reticulate::array_reshape(c(1, imageWidth, imageHeight, 3))
                    imgInputRight <- imgInputRight/255
                    
                    dissTemp[j,] <- values$productionModel %>% keras::predict_on_batch(list(imgInputLeft, imgInputRight))
                    print(dissTemp[j,])
                    
                    imageLeftTemp[j,] <- gsub(".*/|_Raw.*", "", img_left)
                    imageRightTemp[j,] <- gsub(".*/|_Brightness.*", "", img_right)
                    
                    labelLeftTemp[j,] <- paste0(label_img_left)
                    labelRightTemp[j,] <- paste0(label_img_right)
                    
                    
                }
                
                imageLeft <- dplyr::bind_rows(imageLeft, imageLeftTemp)
                imageRight <- dplyr::bind_rows(imageRight, imageRightTemp)
                
                diss <- dplyr::bind_rows(diss, dissTemp)
                
                labelLeft <- dplyr::bind_rows(labelLeft, labelLeftTemp)
                labelRight <- dplyr::bind_rows(labelRight, labelRightTemp)
                
                
                
                dissList <- dplyr::bind_cols(imageLeft, imageRight, labelLeft, diss, labelRight) %>%
                    # rename(imagen = V1, anchor = V11, clase_real = V12, dist = V13, clase_referencia = V14) %>%
                    dplyr::rename(imagen = V1...1, anchor = V1...2, clase_real = V1...3, dist = V1...4, clase_referencia = V1...5) %>%
                    dplyr::mutate(clase_real = clase_real %>% forcats::as_factor(),
                                  clase_referencia = clase_referencia %>% forcats::as_factor()) %>%
                    dplyr::group_split(clase_real)
                
                # liveTime <- chartr(" :-", "___", format(Sys.time(), "%F %X"))
                for (i in 1:length(dissList)) {
                    
                    
                    ggplot2::ggplot(data = dissList[[i]] %>% dplyr::select(dist, clase_referencia) %>% reshape2::melt("clase_referencia"),
                                    ggplot2::aes(x = clase_referencia, y = value, group = clase_referencia)) +
                        ggplot2::geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
                        ggplot2::labs(title = "Inference Distance",
                                      subtitle = glue::glue("{fileName_cam1}"),
                                      caption = glue::glue("Experimento{numeroExperimento} - {inFileProductionModelName}"),
                                      y = "distancia") +
                        ggplot2::geom_hline(yintercept = 1, color = "red") +
                        ggplot2::coord_flip() +
                        ggplot2::coord_polar(theta = "x") +
                        ggplot2::theme_bw() +
                        ggplot2::xlab(NULL)
                    
                    ggplot2::ggsave(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png"))
                    
                    
                }
                
                dissList
            }
            
            
            liveDissimilarityList <- inference_Dissimilarity(dataSet = "Live", imageToInfere = ima_path)
            
            readr::write_csv(liveDissimilarityList %>% dplyr::bind_rows(),
                             path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"))
            
            output$inferencePlot <- renderImage({
                inferencePlotName <- glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png")
                list(src = inferencePlotName,
                     width = 400,
                     height = 350)
            }, deleteFile = FALSE)
            
            output$livePredicciones <- renderTable(
                inferencePrediction %>%
                    dplyr::select(clase_predicha, dmin, accion) %>%
                    dplyr::mutate_if(is.numeric, round, 6)
            )
            
            # Logica Inferencia inferenceWithUploadedImage ----
            df_1 <- readr::read_csv(glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"))
            ima <- gsub(".*/|_Raw.*", "", fileName_cam1)
            t1 <- 0.5
            t2 <- 1.0
            
            # Imagen individual
            df1 <- df_1 %>%
                # filter(imagen == ima) %>%
                dplyr::select(imagen, anchor, clase_referencia, dist)
            
            df2 <- df1 %>%
                dplyr::group_by(clase_referencia) %>%
                dplyr::mutate(dAmin = min(dist),
                              DAmin = abs(dist[1] - dist[2])) %>%
                dplyr::summarise(id = which.min(dist),
                                 imagen = imagen[id],
                                 anchor = anchor[id],
                                 min_dist = dist[id],
                                 clase_referencia = clase_referencia[id],
                                 dAmin = dAmin[id],
                                 DAmin = DAmin[id]) %>%
                dplyr::select(id, imagen, clase_referencia, dAmin, DAmin)
            
            df3 <- df2 %>%
                dplyr::group_by(imagen) %>%
                dplyr::mutate(dmin = min(dAmin),
                              Dmin = min(DAmin)) %>%
                dplyr::filter(dAmin <= t1) %>%
                dplyr::summarise(id_k = which.min(dAmin),
                                 id_l = which.min(DAmin),
                                 imagen = imagen[id_k],
                                 dmin = dmin[id_k],
                                 Dmin = Dmin[id_l],
                                 clase_predicha_k = clase_referencia[id_k],
                                 clase_predicha_l = clase_referencia[id_l],
                                 clase_predicha = ifelse(clase_predicha_k == clase_predicha_l, clase_referencia[id_k],
                                                         ifelse(clase_predicha_k != clase_predicha_l & Dmin >= 0.2, clase_referencia[id_k], clase_referencia[id_l])))
            if (nrow(df3) == 0) {
                inferencePrediction <- tibble::tibble(imagen = ima,
                                                      id_k = "-",
                                                      id_l = "-",
                                                      dmin = "-",
                                                      Dmin = "-",
                                                      clase_predicha_k = "-",
                                                      clase_predicha_l = "-",
                                                      clase_predicha = "objeto no pertenece a la caja",
                                                      accion = "rojo",
                                                      bit = 1) %>%
                    dplyr::select(imagen, id_k, id_l, dmin, Dmin, clase_predicha_k, clase_predicha_l, clase_predicha, accion, bit)
                
            } else {
                inferencePrediction <- df3 %>%
                    dplyr::mutate(accion = ifelse(dmin > t2, "rojo",
                                                  ifelse(dmin > t1 & dmin <= t2, "amarillo",
                                                         ifelse(clase_predicha == clase_predicha_k & dmin <= t1, "verde", "amarillo")))) %>%
                    dplyr::mutate(bit = ifelse(accion == "verde", 2,
                                               ifelse(accion == "amarillo", 3, 1))) %>%
                    dplyr::select(imagen, id_k, id_l, dmin, Dmin, clase_predicha_k, clase_predicha_l, clase_predicha, accion, bit)
            }
            
            
            readr::write_csv(inferencePrediction,
                             path = glue::glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferencePrediction_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.csv"),
                             append = FALSE,
                             col_names = TRUE)
            
            
            # inventario_df <- tibble(Instrumento = c("BC607R",
            #                                         "BH645R",
            #                                         "BJ057R",
            #                                         "BJ60",
            #                                         "M1333516",
            #                                         "M1341416",
            #                                         "M2039020",
            #                                         "PAB31",
            #                                         "PAB31B",
            #                                         "PABSSBB",
            #                                         "PinzaNormalGrande",
            #                                         "PinzaNormalPequegna",
            #                                         "PinzaQuirurgicaGrande",
            #                                         "PinzaQuirurgicaPequegna",
            #                                         "RU125015",
            #                                         "TijeraCurva",
            #                                         "Z12260216"),
            #                         Cantidad = c(1, 2, 2, 3, 1, 1, 1, 2, 3, 1, 2, 1, 1, 2, 2, 1, 1),
            #                         Contador = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
            #                         Cantidad_actual = Cantidad - Contador)
            # inventario_df
            
            # inferencia_df <- read_csv("Documents/Onedrive/SwpDocs/AI_UBB/InferenceR01/experimento_0/checkpoints/livePrediction_siameseModel_experimento50000_EmbSize82_epoca_18-2020_09_20_13_19_10_5_Brightness100_cam1_2020_11_12_17_18_38.csv")
            # inferencia_df <- inferencia_df %>% mutate(bit = ifelse(accion == "rojo", 2,
            #                                                        ifelse(accion == "amarillo", 1, 0)))
            # inferencia_df
            
            accion <- inferencePrediction$accion
            print(accion)
            
            # Inferencia: NO, DUDA, SI ----
            if (accion != "verde") {
                
                # DUDA: Amarillo, actualizar a mano inventario ----
                bit <- inferencePrediction$bit
                print(bit)
                
                if (bit == 3) {
                    
                    senial <- "3"
                    msg <- "amarillo, por favor revisar instrumento a mano"
                    shinyalert::shinyalert("Por favor revisar instrumento a mano",
                                           type = "input",
                                           inputType = "text",
                                           inputPlaceholder = "ingrese instrumento",
                                           callbackR = function(x) { instrumentos <- values$inventario_df$Instrumento
                                           if (x %in% instrumentos) {
                                               values$inventario_df <- values$inventario_df %>%  # inventario_df <<- inventario_df %>%
                                                   dplyr::mutate(Contador = dplyr::case_when(Instrumento == x ~ Contador + 1,
                                                                                             TRUE ~ Contador),
                                                                 Cantidad_actual = dplyr::case_when(Instrumento == x ~ Cantidad - Contador,
                                                                                                    TRUE ~ Cantidad_actual))
                                               
                                               y <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                                                   Cantidad = NA,
                                                                   Contador = NA,
                                                                   Cantidad_actual = NA,
                                                                   Ingreso_manual = x,
                                                                   Imagen = fileName_cam1,
                                                                   Signal1 = senial,
                                                                   Accion1 = msg,
                                                                   Signal2 = senial2,
                                                                   Accion2 = msg2)
                                               
                                               values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(y)  # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(y)
                                               
                                               inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial)  # inventario_parcial
                                               readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                                               
                                               print(values$inventario_df)
                                               
                                               cantidad_actual_k <- values$inventario_df %>%
                                                   dplyr::filter(Instrumento == x)
                                               
                                               cantidad_instrumentos_restantes <- values$inventario_df %>%
                                                   dplyr::filter(Cantidad_actual >= 0) %>%
                                                   dplyr::select(Cantidad_actual) %>%
                                                   sum()
                                               
                                               if (cantidad_actual_k$Cantidad_actual >= 0 & cantidad_instrumentos_restantes > 0) {
                                                   
                                                   
                                                   shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                                                   
                                                   print(values$inventario_df)
                                                   
                                                   
                                               } else if (cantidad_actual_k$Cantidad_actual < 0) {
                                                   
                                                   
                                                   shinyalert::shinyalert("Instrumento completo en inventario!", type = "error")
                                                   
                                                   print(values$inventario_df)
                                                   
                                                   
                                               } else if (cantidad_actual_k$Cantidad_actual == 0 & cantidad_instrumentos_restantes == 0) {
                                                   
                                                   
                                                   shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                                                   
                                                   print(values$inventario_df)
                                                   
                                                   
                                                   
                                                   shinyalert::shinyalert("La caja ha sido completada!", type = "success")
                                                   
                                                   print(values$inventario_df)
                                                   
                                                   
                                               }
                                               
                                           } else {
                                               values$inventario_df <- values$inventario_df   # inventario_df <<- inventario_df
                                               
                                               y <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                                                   Cantidad = NA,
                                                                   Contador = NA,
                                                                   Cantidad_actual = NA,
                                                                   Ingreso_manual = x,
                                                                   Imagen = fileName_cam1,
                                                                   Signal1 = senial,
                                                                   Accion1 = msg,
                                                                   Signal2 = senial2,
                                                                   Accion2 = msg2)
                                               
                                               values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(y) # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(y)
                                               
                                               inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial) # inventario_parcial
                                               readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                                               
                                               print(values$inventario_df)
                                               
                                               shinyalert::shinyalert("Instrumento no pertenece a la caja", type = "error")
                                           }
                                           })
                    senial2 <- "5"
                    msg2 <- "-"
                    print(senial)
                    print(class(senial))
                    print(values$inventario_df)
                    print(msg)
                    print(msg2)
                    
                } else {
                    
                    # NO: Rojo, objeto no pertenece a la caja ----
                    senial <- "1"
                    msg <- "rojo, instrumento no pertenece a la caja"
                    shinyalert::shinyalert("Instrumento no pertenece a la caja", type = "error")
                    senial2 <- "5"
                    msg2 <- "-"
                    print(senial)
                    print(class(senial))
                    print(values$inventario_df)
                    print(msg)
                    print(msg2)
                    
                    x <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                        Cantidad = NA,
                                        Contador = NA,
                                        Cantidad_actual = NA,
                                        Ingreso_manual = NA,
                                        Imagen = fileName_cam1,
                                        Signal1 = senial,
                                        Accion1 = msg,
                                        Signal2 = senial2,
                                        Accion2 = msg2)
                    
                    values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(x)  # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(x)
                    
                    inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial) # inventario_parcial
                    readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                    
                }
                
                # x <- tibble(Instrumento = inferencePrediction$clase_predicha,
                #             Cantidad = NA,
                #             Contador = NA,
                #             Cantidad_actual = NA,
                #             Imagen = fileName_cam1,
                #             Signal1 = senial,
                #             Accion1 = msg,
                #             Signal2 = senial2,
                #             Accion2 = msg2)
                #
                # inventario_parcial <<- inventario_parcial %>% bind_rows(x)
                #
                # inventarioActual <- inventario_df %>% bind_rows(inventario_parcial)
                # write_csv(inventarioActual, glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}.csv"))
                
                
            } else {
                
                # SI: Pertenencia al inventario: NO, SI ----
                clase_predicha <- inferencePrediction$clase_predicha
                instrumentos <- values$inventario_df$Instrumento
                
                # NO: Objeto no esta en inventario ----
                if (!(clase_predicha %in% instrumentos)) {
                    
                    senial <- "1"
                    msg <- "rojo, instrumento no pertenece al inventario"
                    shinyalert::shinyalert("Instrumento no pertenece al inventario", type = "error")
                    senial2 <- "5"
                    msg2 <- "-"
                    print(senial)
                    print(class(senial))
                    print(values$inventario_df)
                    print(msg)
                    print(msg2)
                    
                } else {
                    
                    # SI: !Completitud inventario: NO, SI ----
                    values$inventario_df <- values$inventario_df %>%  # inventario_df <<- inventario_df %>%
                        dplyr::mutate(Contador = dplyr::case_when(Instrumento == clase_predicha ~ Contador + 1,
                                                                  TRUE ~ Contador),
                                      Cantidad_actual = dplyr::case_when(Instrumento == clase_predicha ~ Cantidad - Contador,
                                                                         TRUE ~ Cantidad_actual))
                    print(values$inventario_df)
                    
                    cantidad_actual_k <- values$inventario_df %>%
                        dplyr::filter(Instrumento == clase_predicha)
                    
                    cantidad_instrumentos_restantes <- values$inventario_df %>%
                        dplyr::filter(Cantidad_actual >= 0) %>%
                        dplyr::select(Cantidad_actual) %>%
                        sum()
                    
                    if (cantidad_actual_k$Cantidad_actual >= 0 & cantidad_instrumentos_restantes > 0) {
                        
                        senial <- "2"
                        msg <- "verde, poner instrumento en la caja"
                        shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                        senial2 <- "5"
                        msg2 <- "-"
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        print(msg2)
                        
                    } else if (cantidad_actual_k$Cantidad_actual < 0) {
                        
                        senial <- "1"
                        msg <- "rojo, instrumento completo en inventario"
                        shinyalert::shinyalert("Instrumento completo en inventario!", type = "error")
                        senial2 <- "5"
                        msg2 <- "-"
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        print(msg2)
                        
                    } else if (cantidad_actual_k$Cantidad_actual == 0 & cantidad_instrumentos_restantes == 0) {
                        
                        senial <- "2"
                        msg <- "verde, poner instrumento en la caja"
                        shinyalert::shinyalert("Ponga instrumento en la caja!", type = "success")
                        print(senial)
                        print(class(senial))
                        print(values$inventario_df)
                        print(msg)
                        
                        senial2 <- "4"
                        msg2 <- "blanco, caja completa"
                        shinyalert::shinyalert("La caja ha sido completada!", type = "success")
                        print(senial2)
                        print(class(senial2))
                        print(values$inventario_df)
                        print(msg2)
                        
                    }
                }
                
                x <- tibble::tibble(Instrumento = inferencePrediction$clase_predicha,
                                    Cantidad = NA,
                                    Contador = NA,
                                    Cantidad_actual = NA,
                                    Ingreso_manual = NA,
                                    Imagen = fileName_cam1,
                                    Signal1 = senial,
                                    Accion1 = msg,
                                    Signal2 = senial2,
                                    Accion2 = msg2)
                
                values$inventario_parcial <- values$inventario_parcial %>% dplyr::bind_rows(x) # inventario_parcial <<- inventario_parcial %>% dplyr::bind_rows(x)
                
                inventarioActual <- values$inventario_df %>% dplyr::bind_rows(values$inventario_parcial) # inventario_parcial
                readr::write_csv(inventarioActual, glue::glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}_Experimento{numeroExperimento}.csv"))
                
                
            }
            
            #     clase_predicha <- inferencePrediction$clase_predicha
            #     inventario_df <<- inventario_df %>%
            #         mutate(Contador = case_when(Instrumento == clase_predicha ~ Contador + 1,
            #                                     TRUE ~ Contador),
            #                Cantidad_actual = case_when(Instrumento == clase_predicha ~ Cantidad - Contador,
            #                                            TRUE ~ Cantidad_actual))
            #     print(inventario_df)
            #
            #     cantidad_actual_k <- inventario_df %>%
            #         filter(Instrumento == clase_predicha)
            #
            #     if (any(inventario_df$Cantidad_actual >= 0) == TRUE ) {
            #
            #     cantidad_instrumentos_restantes <- inventario_df %>%
            #         filter(Cantidad_actual >= 0) %>%
            #         select(Cantidad_actual) %>%
            #         sum()
            #
            #     } else {
            #
            #         showNotification("Caja completa! Para continuar escoja otra caja")
            #         cantidad_instrumentos_restantes <- 0
            #
            #     }
            #
            #     if (cantidad_actual_k$Cantidad_actual >= 0 & cantidad_instrumentos_restantes > 0) {
            #
            #         senial <- "2"
            #         msg <- "verde, poner instrumento en la caja"
            #         senial2 <- "5"
            #         msg2 <- "-"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(msg)
            #         print(msg2)
            #         #
            #         # com <- input$COM %>% as.character()
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(15)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     } else if (cantidad_actual_k$Cantidad_actual < 0) {
            #
            #         senial <- "1"
            #         msg <- "rojo, instrumento completo en inventario"
            #         senial2 <- "5"
            #         msg2 <- "-"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(msg)
            #         print(msg2)
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     } else if (cantidad_actual_k$Cantidad_actual == 0 & cantidad_instrumentos_restantes == 0) {
            #
            #         senial <- "2"
            #         msg <- "verde, poner instrumento en la caja"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(msg)
            #
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #         #
            #         # Sys.sleep(5)
            #
            #         senial2 <- "4"
            #         msg2 <- "blanco, caja completa"
            #         print(senial2)
            #         print(class(senial2))
            #         print(inventario_df)
            #         print(msg2)
            #
            #         # com <- input$COM
            #         # print(com)
            #         # conn <- serial::serialConnection("arduino",
            #         #                                  port = glue::glue("{com}"),
            #         #                                  mode = "9600,n,8,1")
            #         # open(conn)
            #         # summary(conn)
            #         #
            #         # # senial <- inferencePrediction$bit
            #         # # print(senial)
            #         #
            #         # serial::write.serialConnection(conn, senial_2)
            #         #
            #         # Sys.sleep(5)
            #         #
            #         # close(conn)
            #         # summary(conn)
            #
            #     } else if (cantidad_instrumentos_restantes == 0) {
            #
            #         senial <- "4"
            #         msg <- "blanco, caja completa"
            #         senial2 <- "5"
            #         msg2 <- "-"
            #         print(senial)
            #         print(class(senial))
            #         print(inventario_df)
            #         print(msg)
            #         print(msg2)
            #
            #     }
            # }
            #
            # x <- tibble(Instrumento = inferencePrediction$clase_predicha,
            #             Cantidad = NA,
            #             Contador = NA,
            #             Cantidad_actual = NA,
            #             Imagen = fileName_cam1,
            #             Signal1 = senial,
            #             Accion1 = msg,
            #             Signal2 = senial2,
            #             Accion2 = msg2)
            #
            # inventario_parcial <<- inventario_parcial %>% bind_rows(x)
            #
            # inventarioActual <- inventario_df %>% bind_rows(inventario_parcial)
            # write_csv(inventarioActual, glue("{workingFolderName}/Caja{numeroCaja}/Inventario/Inventario_Actual_Caja{numeroCaja}.csv"))
            
        } # End if !Completitud caja
        
        # # dato <- 1 %>% as.character()
        # # print(dato)
        # # print(class(dato))
        # # print(inventario_df)
        # # print(glue("enviando {dato} -> verde al arduino, poner instrumento {clase_predicha} en la caja"))
        # #
        # com <- "COM3"
        # print(com)
        # print(class(com))
        # # conn <- serial::serialConnection("arduino",
        # #                                  port = "COM3",
        # #                                  mode = "9600,n,8,1")
        # # open(conn)
        # summary(conn)
        
        # senial <- inferencePrediction$bit
        # print(dato)
        
        serial::write.serialConnection(values$conn, senial)
        Sys.sleep(2)
        serial::write.serialConnection(values$conn, senial2)
        
        # Sys.sleep(15)
        
        # close(conn)
        # summary(conn)
        
        
        
        # com <- input$COM
        # conn <- serial::serialConnection("arduino",
        #                                   port = glue::glue("{com}"),
        #                                   mode = "9600,n,8,1")
        # open(conn)
        #
        # dato <- inferencePrediction$bit
        # print(dato)
        #
        # serial::write.serialConnection(conn, dato)
        #
        # close(conn)
        
        # output$inferencePlot <- renderImage({
        #     inferencePlotName <- glue("{workingFolderName}/Caja{numeroCaja}/Experimento{numeroExperimento}/Resultados_Inferencia/inferenceDistance_{fileName_cam1}_Experimento{numeroExperimento}_{inFileProductionModelName}.png")
        #     list(src = inferencePlotName,
        #          width = 400,
        #          height = 350)
        # }, deleteFile = FALSE)
        #
        # output$livePredicciones <- renderTable(
        #     inferencePrediction %>%
        #                       select(clase_predicha, dmin, accion) %>%
        #                       mutate_if(is.numeric, round, 6)
        # )
        
        
        
        
        shinyjs::enable(id = "NuevoExperimento")
        
        shinyjs::enable(id = "chooseProductionModel")
        shinyjs::enable(id = "inferenceSnapshot")
        
        
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # 20. CERRAR APP ----
    observe({
        if (input$navbar == "Stop") {
            # if (!is.null(conn)) {
            #     close(conn)
            # } else {
            #     conn <- NULL
            # 
            # }
# 
#             close(conn)
            
            shinyjs::js$closeWindow()
            stopApp()
        }
    })
    
    #          ######## shinymanager
    #     }
    # })
    # 
    # observe({
    #     # if (!is.null(input$authStop) && input$authStop == 1) {
    #     if (req(input$authStop) == 1) {
    #         if (!is.null(conn)) {
    #             close(conn)
    #         }
    #         shinyjs::js$closeWindow()
    #         stopApp()
    #     }
    # })
    # ######## shinymanager
    
}