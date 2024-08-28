# header ----
source("header.R")

# functions ----
source("functions.R")

# global options ----
options(DT.options = list(language = list(url = "German.json")))

# ui ----
ui <- fluidPage(
  
  ## window title ----
  title = "visioPA",
  
  ## includes ----
  useShinyjs(),
  includeCSS("www/layout.css"),
  includeScript("scripts/jquery.mousewheel.min.js"),
  
  ## header ----
  fluidRow(
    ### app title ----
    column(
      9, 
      h2(
        HTML("<b>visioPA - Zeitstrahl der onkologischen Patientenakte</b>"),
        style = "margin-top: 30px;"
      ),
      h5(HTML("<i class='fa fa-code-branch'></i> Version: 0.1"))
    ),
    ### KR logo ----
    column(
      3,
      h5(
        div(
          img(
            src = "logo1.png",
            width = "200",
            height = "80"
          ),
          style = "float: right; margin-top: 10px;"
          )
        )
      )
    ),
  hr(),
  ## inputs ----
  fluidRow(
    ### button window resizing ----
    column(
      1, offset = 2, 
      uiOutput("fit_button")
      ),
    ### patient (pid) ----
    column(
      2, offset = 1,
      selectizeInput(
        "pid",
        "1. Patient/in auswählen:",
        choices = c("..." = ".", sample_data %>% distinct(PID) %>% pull() %>% sort())
        )
      ),
    ### tumor (ktid) ----
    column(
      2,
      selectizeInput(
        "ktid",
        "2. Tumor auswählen:",
        choices = c("..." = ".")
      ) 
    ),
  ### help button ----
  conditionalPanel(
    condition = "input.pid != '.' && input.ktid != '.'",
    column(
      1, 
      HTML("") %>% 
        helper(
          type = "markdown",
          title = "Bedienungsanleitung",
          content = "user_manual",
          size = "l",
          style = "font-size: 32px; color: #00A1AF; margin-right: 32px; margin-top: 50px;"
          )
      )
    )
  ),
  ## outputs ----
  fluidRow(
    ### patient infos ----
    column(
      6, offset = 4, 
      uiOutput("pid_infos")
      )
    ),
  hr(),
  br(),
  ### timevis plot ----
  conditionalPanel(
    condition = "input.ktid != '.'",
    timevisOutput("timeline", width = "100%")
  )
)

# server ----
server <- function(input, output, session) {
  
  session$onSessionEnded(function() { stopApp() })
  
  ## observers ----
  
  ### init shinyhelper package ----
  observe_helpers()
  
  ### mousewheel zoom ----
  onevent("mousewheel", "timeline", {
    
    # render zoom reset button
    output$fit_button <- renderUI({
      actionButton(
        "fit", 
        "Zoom zurücksetzen",
        icon = icon("window-restore", verify_fa = FALSE),
        style = "margin-top: 55px; border-color: #00A1AF; text-align: center;"
        )
    })
    
  })
  
  ### click on fit button ----
  observeEvent(input$fit, {
    # resize window
    fitWindow("timeline")
    
    # hide button
    hide("fit")
  })
  
  ### selection of PID ----
  observeEvent(input$pid, {
    freezeReactiveValue(input, "ktid")
    # update ktid selector 
    updateSelectInput(
      session,
      "ktid",
      choices = c(
        "..." = ".",
        sample_data %>%
          filter(PID == input$pid) %>%
          distinct(KTID) %>%
          pull() %>% sort()
      ),
      selected = "."
    )
    
    # hide zoom reset when switching patients
    hide("fit")
  })
  
  ### selection of PID and KTID ----
  observe({
    req(input$pid != ".")
    req(input$ktid != ".")
    
    #### Infos wrt PID from table PATIENT ----
    output$pid_infos <- renderUI({
      
      # need error handling for renderUI
      if (input$pid != "." & input$ktid != ".") {
        lookup_id_in_reg_table("PATIENT", "PID", input$pid) %>%
          select(PID, GEBURTSDATUM,GESCHLECHT) %>%
          bind_cols(
            lookup_id_in_reg_table("REG_DIAGNOSE", "KTID", input$ktid) %>%
              select(DIAGNOSEDATUM, ICD_CODE)
            ) %>%
          mutate(
            alter_bei_diagnose = round(interval(GEBURTSDATUM, DIAGNOSEDATUM)/years(1), digits = 0) ,
            info = str_c(
              "<b>Geburtsdatum:</b> ", day(GEBURTSDATUM), ".",month(GEBURTSDATUM),".", year(GEBURTSDATUM), 
              " &nbsp&nbsp&nbsp <b>Geschlecht:</b> ", GESCHLECHT,
              " &nbsp&nbsp&nbsp <b>Alter bei Diagnose:</b> ", alter_bei_diagnose,
              " &nbsp&nbsp&nbsp <b>Diagnose:</b> ", ICD_CODE
            )
          ) %>%
          pull(info) %>% HTML()
      } else {
          ""
      }

    })
    
    ### reset selection in timeline ----
    setSelection("timeline", NULL)
 
  })
  
  ### show modal with datatable upon selection of timevis event ----
  observeEvent(input$timeline_selected, {
    
      showModal(
        modalDialog(
          DTOutput("details"),
          fade = FALSE,
          size = "l",
          footer = NULL,
          easyClose = TRUE
        )
      )
    
    # flush modal when closed
    outputOptions(output,  "details", suspendWhenHidden = FALSE)
  })
  
  ## reactives ----
  
  ### reactive data selected pid+ktid ----
  data <- reactive({
    
    req(input$pid != ".")
    req(input$ktid != ".")
    
    # filter according to selectors
    # distinct controls what's captured inside one timevis element
    sample_data %>%
      filter(PID == input$pid) %>%
      filter(KTID == input$ktid) %>%
      distinct(PID, KTID, Datum, MELDEANLASS_TYP, .keep_all = T) %>%
      # make columns for timevis
      transmute(
        content = MELDEANLASS_TYP,
        start = Datum,
        end = Datum2,
        id = row_number(),
        PID = PID, 
        KTID = KTID)
  })
  
  ### data for datatable after selection of timevis element ----
  dt_data <- reactive({

    req(input$timeline_selected)
    # handling of warning when switching pid/ktid during active selection
    req(!is.na(data()[input$timeline_selected,]$content))
    
    data()[input$timeline_selected,] %>%
      rename(Datum = start, MELDEANLASS_TYP = content) %>%
      {
        if(.$MELDEANLASS_TYP == "Diagnose") { 
          # Diagnose
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_DIAGNOSE", id_type = "KTID", ids = input$ktid),
            reg_table_date = "DIAGNOSEDATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "Fernmetastasen") {
          # Fernmetastasen
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_FERNMETASTASEN", "KTID", input$ktid),
            reg_table_date = "DIAGNOSEDATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "Histologie") {
          # Histologie
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_HISTOLOGIE", "KTID", input$ktid),
            reg_table_date = "HISTOLOGIEDATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "OP") {
          # OP
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_OPERATION", "KTID", input$ktid),
            reg_table_date = "OP_DATUM", join_column = c("KTID")
            ) %>%
          # OP Komplikation
          left_join(
            .,
            lookup_id_in_reg_table("REG_OPERATION_KOMPLIKATIONEN", "RID_OP", .$RID_OP),
            by = c("KTID", "RID_OP")
            ) %>%
          # OP Codes
          left_join(
            .,
            lookup_id_in_reg_table("REG_OPERATION_CODES", "RID_OP", .$RID_OP),
            by = c("KTID", "RID_OP")
            )
          } else if (.$MELDEANLASS_TYP == "Pathologie") {
          # Pathologie
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_PATHOLOGIE", "KTID", input$ktid),
            reg_table_date = "BEFUNDDATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "Systemtherapie") {
          # Systemtherapie
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_SYSTEMTHERAPIE", "KTID", input$ktid),
            reg_table_date = "BEGINN_DATUM", join_column = c("KTID")
           ) %>%
          # Substanz
          left_join(
            .,
            lookup_id_in_reg_table("REG_SYSTEMTHERAPIE_SUBSTANZ", "RID_SY", .$RID_SY),
            by = c("KTID", "RID_SY")
            ) %>%
          # Nebenwirkungen
          left_join(
            .,
            lookup_id_in_reg_table("REG_NEBENWIRKUNGEN", "RID_SY", .$RID_SY),
            by = c("KTID", "RID_SY")
            )
          } else if (.$MELDEANLASS_TYP == "Strahlentherapie") {
          # Strahlentherapie
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_STRAHLENTHERAPIE", "KTID", input$ktid),
            reg_table_date = "BEGINN_DATUM", join_column = c("KTID")
            ) %>%
          # Zielgebiet
          left_join(
            .,
            lookup_id_in_reg_table("REG_STRAHLENTHERAPIE_ZIELGEBIET", "RID_ST", .$RID_ST),
            by = c("KTID", "RID_ST")
            ) %>%
          # Nebenwirkungen
          left_join(
            .,
            lookup_id_in_reg_table("REG_NEBENWIRKUNGEN", "RID_ST", .$RID_ST),
            by = c("KTID", "RID_ST")
            )
          } else if (.$MELDEANLASS_TYP == "Tumorkonferenz") {
          # Tumorkonferenz
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_TUMORKONFERENZ", "KTID", input$ktid),
            reg_table_date = "DATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "TNM") {
          # TNM
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_TNM", "KTID", input$ktid),
            reg_table_date = "DATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "Verlauf") {
          # Verlauf
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_VERLAUF", "KTID", input$ktid),
            reg_table_date = "UNTERSUCHUNGSDATUM", join_column = c("KTID")
            )
          } else if (.$MELDEANLASS_TYP == "Leichenschauschein") {
          # Leichenschauschein
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_LSS", "PID", input$pid),
            reg_table_date = "STERBEDATUM", join_column = c("PID")
            )
          } else if (.$MELDEANLASS_TYP == "Todesursache") {
          # Todesursache
          # FIXME PID is not unique in this table, may need filter for removing dups
          join_reg_table_upon_trigger(
            trigger_data = ., 
            reg_table_filtered = lookup_id_in_reg_table("REG_TODESURSACHE", "PID", input$pid),
            reg_table_date = "DIAGNOSEDATUM", join_column = c("PID")
            )
          } else {
            .
          }
        } %>%
      remove_empty(., which = "cols") %>%
      # switch display depending on number of rows
      {
      if(nrow(.) == 1) {
        pivot_longer(
          ., everything(),
          names_to = "Feld", values_to = "Wert",
          values_transform = list(Wert = as.character)
          )
        } else {
          .
        }
      }
  })
  
  ## plots ----
  
  ### timevis plot ----
  output$timeline <- renderTimevis({
    timevis(
      data(), showZoom = T
      # options = list(
      #   #rollingMode = list(follow =T, offset = 0.5)
      #   )
      )
  })
  
  ## tables ----
  
  ### datatable with REG_* content ----
  output$details <- DT::renderDataTable({
    
    datatable(
      dt_data(), 
      rownames = F,
      options = list(
        scrollX = TRUE,
        processing = F,
        lengthMenu = list(c(10, -1), c("10", "Alle")),
        pageLength = 10,
        searchHighlight = TRUE
        )
      )
  })
}

shinyApp(ui = ui, server = server)
