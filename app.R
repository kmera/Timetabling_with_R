library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(openxlsx)
library(stringr)
library(DT)
library(data.table)
library(ggplot2)
library(shinyjs)
library(shinyauthr)
library(shinydashboard)
library(shinyalert)
library(forcats)
library(sodium)
library(tibble)
library(rdrop2)

user_base <- tibble(
  user = c("admin"),
  password = c("Clave2020"), 
  password_hash = sapply(c("Clave2020"), sodium::password_store), 
  permissions = c("admin"),
  name = c("User One")
)

# Load data
professors <- read.xlsx("data/Profesores_Vigentes_2020.xlsx", sheet = 1)

nrc <- read.xlsx("data/NRC_2020.xlsx")

paralelo <- c(NA, "P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8", "P9", "P10")

w.days <- factor(c(NA, "Lunes", "Martes", "Miércoles", "Jueves", "Viernes"))
w.days <- forcats::fct_explicit_na(w.days, na_level = "NA")

ini.hour <- c(NA, paste0(rep(7:20), ":00"))
fin.hour <- c(NA, paste0(rep(8:21), ":00"))

actividades_prof <- read.xlsx("data/Profesores_Vigentes_2020.xlsx", sheet = 2)
names(actividades_prof) <- c("code", "actividad", "horas")
actividades_prof <- actividades_prof[-c(11, 31, 36), ]
horas_actividades <- c(NA, rep(1:20))

ui <- dashboardPage(
  
  dashboardHeader(title = "Facultad de Psicología - PUCE", 
                  titleWidth = "35%",
                  tags$li(class = "dropdown", style = "padding: 8px;",
                          shinyauthr::logoutUI("logout"))),
  
  dashboardSidebar(
    collapsed = TRUE, 
    div(textOutput("welcome"), style = "padding: 20px")
  ),
  
  dashboardBody(
    shinyjs::useShinyjs(),
    tags$head(tags$style(".table{margin: 0 auto;}"),
              tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                          type="text/javascript"),
              includeScript("returnClick.js")
    ),
    
    shinyauthr::loginUI("login"),
    useShinyalert(),
    uiOutput("sidebar"),
    uiOutput("bodies"),
    HTML('<div data-iframe-height></div>')
  )
)

server <- function(input, output) { 

  outputDir <- "puce"
  
  saveData <- function(data) {
    fileName <- "programacion.xlsx"
    filePath <- file.path(tempdir(), fileName)
    write.xlsx(data, filePath, row.names = FALSE)
    drop_upload(filePath, path = outputDir)
  }
  
  #loadData <- function() {
    fileInfo <- drop_dir(outputDir)
    filePath <- fileInfo$path_display
    drop_download(filePath, overwrite = T)
    this_table <- read.xlsx("programacion.xlsx")
    #return(this_table)
  #}
  
  #this_table <- read.xlsx("data/new_programacion.xlsx")
  #loadData()
  
  credentials <- callModule(shinyauthr::login, "login", 
                            data = user_base,
                            user_col = user,
                            pwd_col = password_hash,
                            sodium_hashed = TRUE,
                            log_out = reactive(logout_init()))
  
  logout_init <- callModule(shinyauthr::logout, "logout", reactive(credentials()$user_auth))
  
  observe({
    if(credentials()$user_auth) {
      shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
    } else {
      shinyjs::addClass(selector = "body", class = "sidebar-collapse")
    }
  })
  
  user_info <- reactive({credentials()$info})
  
  output$sidebar <- renderUI({
    if(!is.null(user_info()$permissions) && (user_info()$permissions == "admin")) {
      dashboardSidebar(
        sidebarMenu(
          menuItem("Información", tabName = "info", icon = icon("info-circle")),
          menuItem("Configuración", tabName = "config", icon = icon("cogs")),
          menuItem("Tabla General", tabName = "general", icon = icon("dashboard")),
          menuItem("Duplicados", tabName = "duplicated", icon = icon("copy")),
          menuItem("Reportes Gráficos", tabName = "report", icon = icon("chart-bar")),
          menuItem("Reporte Actividades", tabName = "report2", icon = icon("table"))
        )
      )
  }
  })
  
  output$bodies <- renderUI({
    if(!is.null(user_info()$permissions) && (user_info()$permissions == "admin")) {
    
      tabItems(
        tabItem(tabName = "info", 
                fluidPage(
                  div(style = 'overflow-x:hidden',
                      fluidRow(
                        HTML('<h3><p align=center>Esta aplicación permite generar la programación 
                                          de asignaturas para cada profesor de la Facultad de Psicología de la PUCE.</p>'),
                        hr(),
                        HTML('<center><img src = "logo_FP-PUCE.jpg" width = "250"></center>'),
                        hr(),
                        HTML('<center><img src = "logo-PUCE.jpg" width = "250"></center>'),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        hr(),
                        HTML('<h5><p align=center>
                             Aplicación diseñada por:
                             <br><br>
                             Klever Mera
                             <br><br>
                             klever.mera@gmail.com</h5>')
                      )
                  )
                )
        ),
        
        tabItem(tabName = "config",
                HTML(
                  "<h5>
                            Aqui se ingresan todos los parámetros de configuración de los profesores. 
                            <br><br>
                            Cuando requiera empezar desde cero, presione el botón 'Limpiar Tabla' para 
                            asegurarse que no exista ninguna configuración anterior.
                            <br><br>
                            No se olivide presionar el botón 'Guardar Todo' cuando haya terminado y
                            revisado la configuración de cada profesor.</h5>"),
                br(),
                fluidRow(
                  column(4,
                         actionButton(inputId = "del",
                                      label = "Limpiar Tabla",
                                      icon = icon("trash")),
                  ),
              
                  # column(4,
                  #        #Upload file1
                  #        fileInput("file1", "",
                  #                  buttonLabel = "Seleccione el archivo",
                  #                  accept = c(".xlsx"))
                  #        )
                ),
                
                br(),
                fluidRow(
                  column(4,
                         
                         # Select the professor name
                         selectInput(inputId = "prof_name",
                                     label = strong("Seleccione el nombre del profesor:"),
                                     choices = unique(c(NA, sort(professors$prof, na.last = F)))),
                         
                         # Select the career name
                         selectInput(inputId = "career",
                                     label = strong("Seleccione la carrera:"),
                                     choices = unique(c(NA, sort(nrc$Carrera, na.last = F)))),
                        
                         # Select the level
                         output$Level <- renderUI({
                           selectInput(inputId = "level",
                                       label = strong("Seleccione el nivel:"),
                                       choices = unique(c(NA, sort(nrc[(nrc$Carrera == input$career), 2], 
                                                                   na.last = F))))
                         }),
                         
                         # Select the classroom
                         selectInput(inputId = "classroom",
                                     label = strong("Seleccione el paralelo:"),
                                     choices = paralelo),
                         
                         # Select the subject
                         output$Subject <- renderUI({
                           selectInput(inputId = "subject",
                                       label = strong("Seleccione la asignatura:"),
                                       choices = unique(c(NA, sort(nrc[(nrc$Carrera == input$career) &
                                                                         (nrc$Nivel == input$level), 3], 
                                                                   na.last = F))))
                         }),
                         
                         # Select the NRC
                         # textInput(inputId = "nrc_code",
                         #           label = strong("Ingrese el Código Asignatura:"),
                         #           value = ""),
                         
                         # Select the day
                         selectInput(inputId = "wday",
                                     label = strong("Seleccione el día: "),
                                     choices = w.days),
                         
                         # Select the start hour
                         selectInput(inputId = "hour_ini",
                                     label = strong("Seleccione la hora de inicio de la clase: "),
                                     choices = ini.hour),
                         
                         # Select the finish hour
                         selectInput(inputId = "hour_fin",
                                     label = strong("Seleccione la hora de fin de la clase: "),
                                     choices = fin.hour),
                  ),
                  
                  column(4,
                         
                         # Activities
                         selectInput(inputId = "activities_prof",
                                     label = strong("Seleccione la actividad del profesor: "),
                                     choices = unique(c(NA, sort(actividades_prof$actividad, na.last = F)))),
                         
                         # Hour Activities
                         selectInput(inputId = "activities_hour",
                                     label = strong("Seleccione las horas de la actividad: "),
                                     choices = unique(sort(horas_actividades, na.last = F))),
                         hr(),
                         hr(),
                         HTML('<center><img src = "webhomes-brain.png" width = "150"></center>')
                  ),
                  
                  column(4,
                         box(title = h4("Configuración Docencia"), 
                             solidHeader = TRUE,
                             status = "info",
                             width = '10', 
                            textOutput(outputId = "name"),
                            textOutput(outputId = "career"),
                            textOutput(outputId = "level"),
                            textOutput(outputId = "classroom"),
                            textOutput(outputId = "subject"),
                            #textOutput(outputId = "nrc_code"),
                            textOutput(outputId = "work_day"),
                            textOutput(outputId = "hour.ini"),
                            textOutput(outputId = "hour.fin"),
                            textOutput(outputId = "credits")),
                            #textOutput(outputId = "credits_prof")),
                            hr(),
                            box(title = h4("Configuración Actividades"), 
                                solidHeader = TRUE,
                                status = "info",
                                width = '10',
                                textOutput(outputId = "activities"),
                                textOutput(outputId = "hour_activities")),
                                #textOutput(outputId = "tot_activities")),
                                hr(),
                                box(title = h4("Totales por Profesor"), 
                                    solidHeader = TRUE,
                                    status = "success",
                                    width = '10',
                                    textOutput(outputId = "name2"),
                                    textOutput(outputId = "credits_prof"),
                                    textOutput(outputId = "tot_activities"),
                                    textOutput(outputId = "tot_creds")),
                                    hr(),
                  )
                ),
                
                br(),
                fluidRow(
                  column(4
                  ),
                  
                  column(4,
                         # Save button
                         actionButton(inputId = "save",
                                      label = "Guardar Todo",
                                      icon = icon("cog")),
                  ),
                  
                  column(4
                  )
                )
        ),
        
        tabItem(tabName = "general",
                br(),
                div(DTOutput(outputId = "tabla"), 
                    style = "font-size: 80%"),
                ##
                tags$script("$(document).on('click', '#tabla button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random()) });"),
                ##
                br(),
                downloadButton(outputId = "download",
                               label = "Descargar"),
                actionButton(inputId = "Del_row_head",
                             label = "Eliminar",
                             icon = icon("trash")),
                # actionButton(inputId = "mod_row_head",
                #              label = "Editar",
                #              icon = icon("edit"))
                br(),
                br(),
                br(),
                # div(DTOutput(outputId = "tabla_prof"), 
                #     style = "font-size: 80%"),
        ),
        
        tabItem(tabName = "duplicated",
                br(),
                div(DTOutput(outputId = "duplicados"), 
                    style = "font-size: 80%")
        ),
        
        tabItem(tabName = "report",
                br(),
                plotOutput("plot1"),
                hr(),
                plotOutput("plot2"),
                br(),
                plotOutput("plot3")
        ),
        
        tabItem(tabName = "report2",
                br(),
                div(DTOutput(outputId = "varias"), 
                    style = "font-size: 80%")
                )
      )
  }
  })
  
  output$name <- renderText({
    paste0("Nombre: ", input$prof_name)
  })
  
  output$name2 <- renderText({
    paste0("Nombre: ", input$prof_name)
  })
  
  output$career <- renderText({
    paste0("Carrera: ", input$career)
  })
  
  output$level <- renderText({
    paste0("Nivel: ", input$level)
  })
  
  output$classroom <- renderText({
    paste0("Paralelo: ", input$classroom)
  })
  
  output$subject <- renderText({
    paste0("Asignatura: ", input$subject)
  })
  
  # output$nrc_code <- renderText({
  #   paste0("Código Asignatura: ", input$nrc_code)
  # })
  
  output$work_day <- renderText({
    paste0("Día de clase: ", input$wday)
  })
  
  output$hour.ini <- renderText({
    f1 <- str_extract(input$hour_ini, "[0-9]+")
    paste0("Hora inicio: ", f1, ":00")
  })
  
  output$hour.fin <- renderText({
    f1 <- str_extract(input$hour_ini, "[0-9]+")
    f2 <- str_extract(input$hour_fin, "[0-9]+")
    validate(need(as.numeric(f1) < as.numeric(f2), 
                  "Error: La hora de fin debe ser mayor que la de inicio."))
    paste0("Hora fin: ", f2, ":00")
  })
  
  cred <- reactive({
    f1 <- str_extract(input$hour_ini, "[0-9]+")
    f2 <- str_extract(input$hour_fin, "[0-9]+")
    df2 <- sum(as.numeric(f2), -as.numeric(f1), na.rm = T)
    return(df2)
  })
  
  cred_prof <- reactive({
    df3 <- cred()*2
    return(df3)
  })
  
  cred_prof_tot <- reactive({
    df4 <- sum(cred_prof(), as.numeric(input$activities_hour), na.rm = T)
    return(df4)
  })
  
  activ_tot <- reactive({
    if(!is.na(as.numeric(input$activities_hour))) {
      df5 <- as.numeric(input$activities_hour)
      return(df5)
    } else
      0
  })
  
  output$credits <- renderText({
    paste0("Créditos: ", as.character(cred()))
  })
  
  output$activities <- renderText({
    paste0("Actividad: ", input$activities_prof)
  })
  
  output$hour_activities <- renderText({
    paste0("Horas Actividad: ", input$activities_hour)
  })
  
  this_table <- reactiveVal(this_table)
  
  observeEvent(input$save, {
    t = rbind(this_table(), data.frame(Nombre = input$prof_name,
                                       Modalidad = professors[(professors$prof == input$prof_name), 3],
                                       Carrera = input$career,
                                       Nivel = input$level,
                                       Paralelo = input$classroom,
                                       Asignatura = input$subject,
                                       Cod_Asig = nrc[(nrc$Carrera == input$career &
                                                           nrc$Nivel == input$level &
                                                           nrc$Asignatura == input$subject), 4],
                                       Dia = input$wday,
                                       Hora_Inicio = input$hour_ini,
                                       Hora_Fin = input$hour_fin,
                                       Creditos_Docencia = cred(),
                                       Total_Docencia = cred_prof(),
                                       Otras_Actividades = input$activities_prof,
                                       Horas_Otras_Actividades = activ_tot(),
                                       Total_Horas_Docente = cred_prof_tot()))
    this_table(t)
    
    #total_cred()$Total <= 40 | teaching_cred()$Docencia <= 20
    if(total_cred()[(total_cred()$Nombre == input$prof_name), 2] > 40 | 
       teaching_cred()[(teaching_cred()$Nombre == input$prof_name), 2] > 40) {
      shinyjs::disable("save")
      #mensaje
      shinyalert(title = "Los creditos de docencia deben estar entre 8 y 32.
                 Tampoco puede asignar a un profesor mas de 40 horas totales.
                 Por favor elimine la última configuración", 
                 type = "error")
      
    } else shinyalert(title = "Configuración guardada", type = "success")
    
    #write.xlsx(this_table(), file = "data/new_programacion.xlsx", row.names = FALSE)
    saveData(this_table())
  })
  
  # Tabla General
  output$tabla <- renderDT({
    this_table() 
    
  }, options = list(scrollX = T,
                    selection = "single",
                    escape = F))
  
  # Tabla Duplicados
  dupli <- reactive({
    table3 <- this_table() %>% subset(Dia != "NA") %>% 
      group_by(Nombre, Dia, Hora_Inicio, Hora_Fin) %>% 
      filter(n() > 1) %>% 
      select(Nombre, Asignatura, Dia, Hora_Inicio, Hora_Fin)
    return(table3)
  })
  
  output$duplicados <- renderDT({
    dupli()
  })
  
  # Tabla Actividades
  output$varias <- renderDT({
    this_table() %>% 
      group_by(Otras_Actividades) %>% 
      summarize(Total_Actividades = sum(Horas_Otras_Actividades, na.rm = T)) %>% 
      arrange(desc(Total_Actividades)) %>% 
      select(Otras_Actividades, Total_Actividades)
    
  }, options = list(scrollX = T,
                    selection = "single",
                    escape = F))
  
  # Tabla Prof
  output$tabla_prof <- renderDT({
    table2 <- this_table() %>%
      group_by(Nombre) %>%
      summarize(Docencia = sum(as.numeric(Total_Docencia), na.rm = T),
                Otras_Actividades = sum(as.numeric(Horas_Otras_Actividades), na.rm = T),
                Total = sum(as.numeric(Total_Horas_Docente), na.rm = T))
  }, options = list(scrollX = T,
                    selection = "single",
                    escape = F))
  
  teaching_cred <- reactive({
    df3 <- this_table() %>% group_by(Nombre) %>%
      summarize(Docencia = sum(as.numeric(Total_Docencia), na.rm = T)) %>% 
      select(Nombre, Docencia)
    return(df3)
  })
  
  output$credits_prof <- renderText({
    #ifelse(teaching_cred()[, 2] >= 6 && teaching_cred()[, 2] <= 20,
           paste0("Total Créditos Docencia: ", 
                  as.character(teaching_cred()[(teaching_cred()$Nombre == input$prof_name), 2]))
     #      paste0("Los creditos de docencia deben estar entre 6 y 20, usted ha asignado: ", 
      #            as.character(teaching_cred()[(teaching_cred()$Nombre == input$prof_name), 2]), ". Por favor corregir"))
  })
  
  total_cred <- reactive({
    df4 <- this_table() %>% group_by(Nombre) %>%
      summarize(Total = sum(as.numeric(Total_Horas_Docente), na.rm = T)) %>% 
      select(Nombre, Total)
    return(df4)
  })
  
  output$tot_creds <- renderText({
    #if_else(total_cred()$Total[] <= 40,
           paste0("Total Horas Docente: ", 
                 as.character(total_cred()[(total_cred()$Nombre == input$prof_name), 2]))
     #      paste0("No puede asignar a un profesor mas de 40 creditos, usted ha asignado: ", 
      #            as.character(total_cred()[(total_cred()$Nombre == input$prof_name), 2]), ". Por favor corregir"))
  })
  
  total_activ <- reactive({
    df5 <- this_table() %>% group_by(Nombre) %>%
      summarize(Total_Activities = sum(as.numeric(Horas_Otras_Actividades), 
                                       na.rm = T)) %>% 
      select(Nombre, Total_Activities)
    return(df5)
  })
  
  output$tot_activities <- renderText({
    paste0("Total Actividades: ", 
           as.character(total_activ()[(total_activ()$Nombre == input$prof_name), 2]))
  })
  
  # Limpiar tabla
  observeEvent(input$del, {
    t = this_table()[0, ]
    this_table(t)
  })
  
  # Download this_table()
  output$download <- downloadHandler(
    filename = function() {
      paste("programacion", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(this_table(), file, row.names = FALSE)
      # write.xlsx(this_table(), file = "data/programacion.xlsx", 
      #            row.names = FALSE)
    })
  
  # Delete row
  observeEvent(input$Del_row_head,{
    showModal(
      if(length(input$tabla_rows_selected) >= 1) {
        modalDialog(
          title = "Advertencia",
          paste("Está seguro(a) de que quiere eliminar la(s) fila(s)",
                length(input$tabla_rows_selected), "?"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("ok", "Si")),
          easyClose = TRUE)
      }else{
        modalDialog(
          title = "Advertencia",
          footer = modalButton("OK"),
          paste("Por favor seleccione la(s) fila(s) que quiere eliminar"), 
          easyClose = TRUE
        )
      }
      
    )
  })
  
  observeEvent(input$ok, {
    t <- this_table()[-input$tabla_rows_selected, ]
    this_table(t)
    #write.xlsx(this_table(), file = "data/new_programacion.xlsx", row.names = FALSE)
    saveData(this_table())
    removeModal()
    shinyjs::enable("save")
    
    # if(total_cred()[(total_cred()$Nombre == input$prof_name), 2] <= 40 | 
    #    teaching_cred()[(teaching_cred()$Nombre == input$prof_name), 2] <= 32) {
    #   shinyjs::enable("save")
    #   #mensaje
    # }
  })
  
  ####
  # Edit row
  # observeEvent(input$mod_row_head,{
  #   showModal(
  #     if(length(input$tabla_rows_selected) >= 1) {
  #       modalDialog(
  #         fluidPage(
  #           h3(strong("Modificación"),align ="center"),
  #           hr(),
  #           dataTableOutput("row_modif"),
  #           actionButton("save_changes","Save Changes"),
  #           tags$script(HTML("$(document).on('click', '#save_changes', function () {
  #                            var list_value=[]
  #                            for (i = 0; i < $( '.new_input' ).length; i++)
  #                            {
  #                            list_value.push($( '.new_input' )[i].value)
  #                            }
  #                            Shiny.onInputChange('newValue', list_value) });")) ), size="l" )
  #     }else{
  #       modalDialog(
  #         title = "Advertencia",
  #         footer = modalButton("OK"),
  #         paste("Por favor seleccione la fila que quiere editar"),
  #         easyClose = TRUE
  #       )
  #     }
  #     
  #   )
  # })
  # 
  # # modify part
  # output$row_modif <- renderDataTable({
  #   selected_row = input$tabla_rows_selected
  #   old_row = this_table()[selected_row, ]
  #   row_change = list()
  #   for (i in colnames(old_row))
  #   {
  #     if (is.numeric(this_table()[[i]]))
  #     {
  #       row_change[[i]] <- paste0('<input class = "new_input" value = ', '"',
  #                                 old_row[[i]],'"',
  #                                 ' type = "number" id = new_', i, ' ><br>')
  #     } 
  #     else if( is.Date(this_table()[[i]])){
  #       row_change[[i]] <- paste0('<input class = "new_input" value = ', '"',
  #                                 old_row[[i]],'"',
  #                                 ' type = "date" id = new_  ', i, '  ><br>') 
  #     }
  #     else 
  #       row_change[[i]] <- paste0('<input class = "new_input" value = ', '"',
  #                                 old_row[[i]],'"',
  #                                 ' type = "textarea" id = new_', i, '><br>')
  #   }
  #   row_change = as.data.table(row_change)
  #   setnames(row_change,colnames(old_row))
  #   DT = row_change
  #   DT 
  # }, escape = F,options = list(dom = 't', ordering = F, scrollX = TRUE),
  # selection = "none")
  # 
  # # This is to replace the modified row to existing row
  # observeEvent(input$newValue,
  #              {
  #                newValue = lapply(input$newValue, function(col) {
  #                  if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
  #                    as.numeric(as.character(col))
  #                  } else {
  #                    col
  #                  }
  #                })
  #                df7 <- data.frame(lapply(newValue, function(x) t(data.frame(x))))
  #                colnames(df7) <- colnames(this_table())
  #                #selected_row = input$tabla_rows_selected
  #                #old_row = this_table()[selected_row, ]
  #                #replace(this_table(), this_table()[input$tabla_rows_selected], df7)
  #                this_table()[input$tabla_rows_selected] <- df7
  #                # removeModal()
  #              }
  # )
  
  ####
  # Reports
  by_mode <- reactive({
    p1 <- this_table() %>% group_by(Modalidad) %>%
      summarize(total1 = sum(as.numeric(Total_Horas_Docente), na.rm = T))
    return(p1)
  })
  
  output$plot1 <- renderPlot({
    by_mode() %>% ggplot(aes(Modalidad, total1, fill = Modalidad)) +
      geom_col() +
      xlab("Modalidad") +
      ylab("Total de Horas Docentes") +
      ggtitle("Total de Horas segun la Modalidad") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  by_day <- reactive({
    p2 <- this_table() %>% subset(Dia != "NA") %>% 
      group_by(Dia) %>%
      summarize(total2 = sum(as.numeric(Total_Horas_Docente), na.rm = T))
    return(p2)
  })
  
  output$plot2 <- renderPlot({
    by_day() %>% ggplot(aes(Dia, total2, fill = Dia)) +
      geom_col() +
      xlab("Dia de la Semana") +
      ylab("Total de Horas Docentes") +
      ggtitle("Total de Horas segun el Dia") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  by_activity <- reactive({
    p3 <- this_table() %>% group_by(Otras_Actividades) %>%
      summarize(TotalActiv = sum(as.numeric(Horas_Otras_Actividades), na.rm = T)) %>% 
      mutate(Otras_Actividades = forcats::fct_reorder(Otras_Actividades, TotalActiv)) %>% 
      arrange(desc(TotalActiv)) %>% 
      head(10)
    return(p3)
  })
  
  output$plot3 <- renderPlot({
    by_activity() %>% ggplot(aes(Otras_Actividades, TotalActiv)) +
      geom_col() +
      coord_flip() +
      xlab("Actividades") +
      ylab("Total de Horas Otras Actividades") +
      ggtitle("Total de Horas por Actividad") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
