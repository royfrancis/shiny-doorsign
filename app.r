## doorsign
## R shinyapp to generate door signs
## 2024 Roy Mathew Francis

library(shiny)
library(bslib)
library(bsicons)
library(quarto)

source("functions.r")

# UI ---------------------------------------------------------------------------

ui <- page_fluid(
  title = "NBIS Doorsign",
  theme = bs_theme(preset = "zephyr", primary = "#A7C947"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  lang = "en",
  card(
    full_screen = TRUE,
    card_header(
      class = "app-card-header",
      tags$div(
        class = "app-header",
        span(tags$img(src = "nbis.png", style = "height:18px;"), style = "vertical-align:top;display:inline-block;"),
        span(tags$h5("•", style = "margin:0px;margin-left:6px;margin-right:6px;"), style = "vertical-align:top;display:inline-block;"),
        span(tags$h5("Doorsign", style = "margin:0px;"), style = "vertical-align:middle;display:inline-block;")
      )
    ),
    layout_sidebar(
      sidebar = sidebar(
        width = "320px",
        sliderInput("in_tracks", "Number of persons", min = 1, max = 5, step = 1, value = 1),
        uiOutput("tracks"),
        div(
          accordion(
            open = FALSE,
            accordion_panel("Settings",
              icon = bsicons::bs_icon("gear-fill"),
              layout_columns(
                tooltip(
                  numericInput("in_height", "Image height", min = 1, max = 10, step = 0.5, value = 6),
                  "Height of profile image. Value between 1 and 10 cm.",
                  placement = "right"
                ),
                tooltip(
                  numericInput("in_size", "Font size", min = 5, max = 20, step = 1, value = 16),
                  "Base font size. Value between 5 and 20 pt.",
                  placement = "right"
                ),
                col_width = c(6, 6)
              ),
              layout_columns(
                tooltip(
                  numericInput("in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 2),
                  "Value between 0 and 3 cm.",
                  placement = "right"
                ),
                tooltip(
                  numericInput("in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0.6),
                  "Value between 0 and 3 cm.",
                  placement = "right"
                ),
                col_width = c(6, 6)
              ),
              tooltip(
                sliderInput("in_tracking", "Tracking", min = 0, max = 0.2, step = 0.01, value = 0.02),
                "Character spacing. Value between 0 and 0.2 pt.",
                placement = "right"
              )
            )
          )
        ),
        actionButton("btn_update", "Update", class = "btn-large"),
        layout_columns(
          style = "margin-top:5px;",
          tooltip(actionButton("btn_reset", "Reset", class = "btn-warning"), "Reset inputs. To reset images, refresh page", placement = "bottom"),
          tooltip(downloadButton("btn_download", "Download"), "Download as PDF", placement = "bottom"),
          col_widths = c(4, 8)
        )
      ),
      uiOutput("out_pdf", width = "100%", height = "100%")
      # verbatimTextOutput("out_text")
    ),
    card_footer(
      class = "app-footer",
      div(
        class = "help-note",
        paste0(format(Sys.time(), "%Y"), " Roy Francis • Version: ", fn_version()),
        HTML("• <a href='https://github.com/royfrancis/shiny-doorsign' target='_blank'><i class='fab fa-github'></i></a> <U+2022> <a href='mailto:roy.francis@nbis.se' target='_blank'><i class='fa fa-envelope'></i></a>")
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  ## create temporary directory
  temp_dir <- tempdir(check = TRUE)
  temp_id <- paste(sample(letters, 10), collapse = "")
  temp_dir_active <- file.path(temp_dir, temp_id)
  cat(paste0("Working directory: ", temp_dir_active, "\n"))
  store <- reactiveValues(wd = temp_dir_active, id = temp_id, bg_path = NULL)
  if (!dir.exists(temp_dir_active)) dir.create(temp_dir_active)
  copy_dirs(temp_dir_active)
  addResourcePath(temp_id, temp_dir_active)

  ## UI: tracks ----------------------------------------------------------------
  ## conditional ui for tracks

  output$tracks <- renderUI({
    tracks <- as.integer(input$in_tracks)
    sample_data <- sample_data_1
    if (tracks > 1) sample_data <- sample_data_5

    accordion(
      open = FALSE,
      lapply(1:tracks, function(i) {
        accordion_panel(paste("Person", i),
          icon = bsicons::bs_icon("person-circle"),
          div(
            class = "info-item",
            tooltip(
              textInput(paste0("in_name_", i), "Name", value = sample_data[[i]][["name"]], placeholder = paste("Enter name of person", i)),
              paste("Enter name of person", i),
              placement = "right",
            ),
            tooltip(
              textAreaInput(paste0("in_content_", i), "Content", value = sample_data[[i]][["content"]], placeholder = paste("Enter info for person", i), height = "100px"),
              paste("Enter info for person", i),
              placement = "right",
            ),
            tooltip(
              fileInput(paste0("in_image_", i), "Profile image", multiple = FALSE),
              paste("Upload profile image for person", i, ". Better to use an image with square aspect ratio"),
              placement = "right",
            )
          )
        )
      })
    )
  })

  ## FN: fn_vars ------------------------------------------------------------
  ## function to get ui input params

  fn_vars <- reactive({
    # validate(need(input$tracks, 'Number of persons required.'))
    tracks <- as.integer(input$in_tracks)

    l <- setNames(
      lapply(1:tracks, function(i) {
        # handling profile images
        eval(parse(text = paste0("cimg <- input$in_image_", i)))
        if (is.null(cimg)) {
          img_path <- "www/profile.png"
        } else {
          validate(fn_validate_im(cimg))
          ext <- tools::file_ext(cimg$datapath)
          img_name <- paste0("profile-", i, ".", ext)
          img_path <- file.path(store$wd, img_name)
          if (file.exists(img_path)) file.remove(img_path)
          file.copy(cimg$datapath, img_path)
          img_path <- img_name
        }

        # create list with person metadata
        setNames(
          list(
            eval(parse(text = paste0("input$in_name_", i))),
            eval(parse(text = paste0("input$in_content_", i))),
            img_path
          ), c("name", "content", "profile")
        )
      }), paste0("person-", 1:tracks)
    )



    l["profile-height"] <- paste0(ifelse(is.null(input$in_height), 6, {
      validate(fn_validate_range(input$in_height, 1, 10, label = "Image height"))
      input$in_height
    }), "cm")

    l["font-size"] <- paste0(ifelse(is.null(input$in_size), 16, {
      validate(fn_validate_range(input$in_size, 5, 20, label = "Font size"))
      input$in_size
    }), "pt")

    l["gap-above-profile"] <- paste0(ifelse(is.null(input$in_gap_above), 2, {
      validate(fn_validate_range(input$in_gap_above, 0, 3, label = "Upper gap"))
      input$in_gap_above
    }), "cm")

    l["gap-below-profile"] <- paste0(ifelse(is.null(input$in_gap_below), 0.6, {
      validate(fn_validate_range(input$in_gap_below, 0, 3, label = "Lower gap"))
      input$in_gap_below
    }), "cm")

    l["tracking"] <- paste0(ifelse(is.null(input$in_tracking), 0.4, {
      validate(fn_validate_range(input$in_tracking, -1.0, 1.0, label = "Tracking"))
      input$in_tracking
    }), "em")

    l["persons"] <- tracks

    return(l)
  })

  ## ER: Update button binding -------------------------------------------------

  evr_update <- eventReactive(input$btn_update, {
    return(fn_vars())
  })

  ## FN: fn_vars ------------------------------------------------------------
  ## function to get ui input params

  fn_build <- reactive({
    vars <- evr_update()
    # vars <- fn_vars()

    file_name <- switch(vars$persons,
      "one.qmd",
      "two.qmd",
      "three.qmd",
      "four.qmd",
      "five.qmd"
    )

    progress_plot <- shiny::Progress$new()
    progress_plot$set(message = "Creating PDF ...", value = 0.1)

    output_file <- fname()
    ppath <- store$wd
    if (file.exists(file.path(ppath, output_file))) file.remove(file.path(ppath, output_file))
    quarto::quarto_render(input = file.path(ppath, file_name), metadata = vars)
    file.rename(file.path(ppath, sub(".qmd", ".pdf", file_name, fixed = TRUE)), file.path(ppath, output_file))

    progress_plot$set(message = "Completed", value = 1)
    progress_plot$close()
  })

  ## UI: out_pdf -------------------------------------------------------------
  ## output iframe

  output$out_pdf <- renderUI({
    if (input$btn_update == 0) {
      return(div(p("Click 'Update' to generate preview.")))
    } else {
      fn_build()
      output_file <- fname()
      return(tags$iframe(src = file.path(store$id, output_file), height = "100%", width = "100%"))
    }
  })

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename = fname(),
    content = function(file) {
      fn_build()
      # cpath <- file.path(store$wd, "preview", fname())
      file.copy(file.path(store$wd, fname()), file, overwrite = T)
      unlink(fname())
    }
  )

  ## OBS: set default inputs ------------------------------------------------------------
  ## observer for updating default inputs

  observe({
    if (input$in_tracks == 1) {
      updateNumericInput(session, "in_height", "Image height", min = 1, max = 10, step = 0.5, value = 6)
      updateNumericInput(session, "in_size", "Font size", min = 5, max = 20, step = 1, value = 16)
      updateNumericInput(session, "in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 2)
      updateNumericInput(session, "in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0.6)
      # lapply(1:input$in_tracks, function(i) {
      #   uupdateTextInput(paste0("in_name_",i), "Name", value = "John Doe", placeholder = paste("Enter name of person",i)),
      #   updateTextAreaInput(paste0("in_content_",i), "Content", value = txt_content, placeholder = paste("Enter info for person",i), height = "150px"),
      # })
    }

    if (input$in_tracks == 2) {
      updateNumericInput(session, "in_height", "Image height", min = 1, max = 10, step = 0.5, value = 4)
      updateNumericInput(session, "in_size", "Font size", min = 5, max = 20, step = 1, value = 14)
      updateNumericInput(session, "in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 0.5)
      updateNumericInput(session, "in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0.1)
    }

    if (input$in_tracks == 3) {
      updateNumericInput(session, "in_height", "Image height", min = 1, max = 10, step = 0.5, value = 3)
      updateNumericInput(session, "in_size", "Font size", min = 5, max = 20, step = 1, value = 12)
      updateNumericInput(session, "in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 1)
      updateNumericInput(session, "in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0.1)
    }

    if (input$in_tracks == 4) {
      updateNumericInput(session, "in_height", "Image height", min = 1, max = 10, step = 0.5, value = 3)
      updateNumericInput(session, "in_size", "Font size", min = 5, max = 20, step = 1, value = 12)
      updateNumericInput(session, "in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 1)
      updateNumericInput(session, "in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0.1)
    }

    if (input$in_tracks == 5) {
      updateNumericInput(session, "in_height", "Image height", min = 1, max = 10, step = 0.5, value = 2.6)
      updateNumericInput(session, "in_size", "Font size", min = 5, max = 20, step = 1, value = 11)
      updateNumericInput(session, "in_gap_above", "Upper gap", min = 0, max = 3, step = 0.1, value = 0.5)
      updateNumericInput(session, "in_gap_below", "Lower gap", min = 0, max = 3, step = 0.1, value = 0)
    }
  })

  ## OBS: btn_reset ------------------------------------------------------------
  ## observer for reset

  observeEvent(input$btn_reset, {
    updateSliderInput(session, "in_tracks", "Number of persons", min = 1, max = 5, value = 1, step = 1)
    updateSliderInput(session, "in_tracking", "Tracking", min = 0, max = 0.2, step = 0.01, value = 0.02)
  })

  ## OSE -----------------------------------------------------------------------
  ## delete user directory when session ends

  session$onSessionEnded(function() {
    cat(paste0("Removing working directory: ", isolate(store$wd), " ...\n"))
    if (dir.exists(isolate(store$wd))) {
      unlink(isolate(store$wd), recursive = TRUE)
    }
  })

  ## OUT: out_text ------------------------------------------------------------
  ## debug output

  # output$out_text <- renderPrint({

  #   x <- evr_update()
  #   print(str(x))

  # })
}

shinyApp(ui = ui, server = server)
