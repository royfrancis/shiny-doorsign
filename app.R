## doorsign
## R shinyapp to generate door signs
## 2024 Roy Mathew Francis

library(shiny)
library(bslib)
library(bsicons)

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- page_fluid(style="margin-top:1em;",
  title = "NBIS Doorsign",
  theme = bs_theme(preset = "zephyr", primary = "#A7C947"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  card(
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
        width="300px",
        sliderInput("in_tracks", "Number of persons", min = 1, max = 5, value = 1, step = 1),
        uiOutput("tracks"),
        tooltip(actionButton("btn_update", "Update", class = "btn-large"), "Preview changes", placement = "top"),
        layout_columns(
          style = "margin-top:5px;",
          tooltip(actionButton("btn_reset", "Reset", class = "btn-warning"), "Reset all inputs", placement = "bottom"),
          tooltip(downloadButton("btn_download", "Download"), "Download as PDF", placement = "bottom"),
          col_widths = c(4, 8)
        )
      ),
      div(
        class = "img-output",
        imageOutput("out_plot", width = "auto", height = "auto")
      ),
      verbatimTextOutput("out_text")
    ),
    card_footer(
      class = "app-footer",
      div(
        class = "help-note",
        paste0(format(Sys.time(), "%Y"), " Roy Francis • Version: ", fn_version()),
        HTML("• <a href='https://github.com/royfrancis/shiny-doorsign' target='_blank'><i class='fab fa-github'></i></a> • <a href='mailto:roy.francis@nbis.se' target='_blank'><i class='fa fa-envelope'></i></a>")
      )
    )
  )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  ## get temporary directory
  store <- reactiveValues(epath = tempdir())

  ## UI: tracks ----------------------------------------------------------------
  ## conditional ui for tracks

  output$tracks <- renderUI({
    tracks <- as.integer(input$in_tracks)

    accordion(
      lapply(1:tracks, function(i) {
        accordion_panel(paste("Person",i), icon = bsicons::bs_icon("person-circle"),
          div(class="info-item",
            tooltip(
              textAreaInput(paste0("in_content_",i), "Content", value = txt_content, placeholder = paste("Enter info for person",i), height = "150px"),
              paste("Enter info for person",i), placement = "right",
            ),
            tooltip(
              fileInput(paste0("in_image_",i), "Profile image", multiple = FALSE),
              paste("Upload profile image for person",i), placement = "right",
            ),
            tooltip(
              numericInput(paste0("in_size_",i), "Image height", min = 0.10, max = 0.80, value = 0.40, step = 0.01),
              paste("Width of profile image for person",i,". Value between 0.1 and 0.8."), placement = "right"
            )
          )
        )
      })
    )

  })

  ## FN: fn_params ------------------------------------------------------------
  ## function to get ui input params

  fn_params <- reactive({

    tracks <- as.integer(input$in_tracks)

    l <- setNames(
      lapply(1:tracks, function(i){

        eval(parse(text = paste0("img <- input$in_image_",i)))
        if(is.null(img)){
          img_path <- NULL
        } else {
         eval(parse(text = paste0("img_path <- list(path = input$in_image_",i,"$datapath)")))
        }

        setNames(
          list(
            img_path,
            eval(parse(text = paste0("input$in_size_",i))),
            eval(parse(text = paste0("input$in_content_",i)))
          ), c("image", "size", "content")
        )

      }), paste0("person-",1:tracks)
    )

    return(l)
  })

  ## ER: Update button binding -------------------------------------------------

  evr_update <- eventReactive(input$btn_update, {
    return(fn_params())
  })

    # validation ---------------------------------------------------------------

    # fn_val <- function(x) {
    #   dp <- deparse(substitute(x))
    #   dp <- switch(dp,
    #     "l1x" = "Label 1 Hor pos",
    #     "l2x" = "Label 2 Hor pos",
    #     "l3x" = "Label 3 Hor pos",
    #     "l1y" = "Label 1 Ver pos",
    #     "l2y" = "Label 2 Ver pos",
    #     "l3y" = "Label 3 Ver pos",
    #     "lls" = "Logo left scale",
    #     "lrs" = "Logo right scale"
    #   )
    #   if (x < 0 | x > 1) paste0("Input '", dp, "' must be between 0 and 1.")
    # }

    # dfr ----------------------------------------------------------------------

  #   dfr <- data.frame(
  #     label = c(text_name, text_title, text_dept),
  #     type = c("name", "title", "dept")
  #   )
  #   dfr1 <- data.frame(label = trimws(unlist(strsplit(text_email, ","))))
  #   dfr1$type <- "email"
  #   dfr2 <- data.frame(label = unlist(strsplit(text_phone, ",")))
  #   dfr2$type <- "phone"
  #   dfr <- rbind(dfr, dfr1, dfr2)

  #   logo_right <- png::readPNG("www/scilifelab.png")
  #   logo_left <- png::readPNG("www/nbis.png")

  #   fn_validate_im <- function(x) {
  #     if (!is.null(x)) {
  #       y <- tolower(sub("^.+[.]", "", basename(x$datapath)))
  #       if (!y %in% c("jpg", "jpeg", "png", "gif")) {
  #         return("Image must be one of JPG/JPEG, PNG or GIF formats.")
  #       }
  #       if ((x$size / 1024 / 1024) > 2) {
  #         return("Image must be less than 2 MB in size.")
  #       }
  #     }
  #   }

  #   validate(fn_validate_im(input$in_im_profile))

  #   if (is.null(input$in_im_profile)) {
  #     im_profile <- png::readPNG("www/profile.png")
  #   } else {
  #     # read image, convert to png, square aspect, export as png
  #     im_final <- fn_circ_image(im = magick::image_read(input$in_im_profile$datapath), nudge_x = nudge_x, nudge_y = nudge_y)

  #     magick::image_write(im_final, path = file.path(store$epath, "image.png"), format = "png")
  #     im_profile <- png::readPNG(file.path(store$epath, "image.png"))
  #   }

  #   f <- "Merriweather"

  #   return(list(
  #     dfr = dfr, logo_right = logo_right, logo_left = logo_left, im_profile = im_profile,
  #     pos_y_text = pos_y_text, im_profile_offset_y = im_profile_offset_y, im_profile_width = im_profile_width
  #   ))
  # })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  # output$out_plot <- renderImage(
  #   {
  #     p <- fn_params()
  #     progress1 <- shiny::Progress$new()
  #     progress1$set(message = "Generating figure...", value = 40)

  #     plot_doorsign(
  #       dfr = p$dfr, im_profile = p$im_profile, logo_left = p$logo_left, logo_right = p$logo_right,
  #       pos_y_text = p$pos_y_text, im_profile_offset_y = p$im_profile_offset_y, im_profile_width = p$im_profile_width,
  #       path_export = store$epath, format_export = "png"
  #     )
  #     progress1$set(message = "Completed.", value = 100)
  #     progress1$close()

  #     scaling <- 2.8
  #     return(list(
  #       src = file.path(store$epath, "door-sign.png"), contentType = "image/png",
  #       width = round(297 * scaling, 0),
  #       height = round(210 * scaling, 0),
  #       alt = "doorsign"
  #     ))
  #   },
  #   deleteFile = TRUE
  # )

  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  # fn_download <- function() {
  #   p <- fn_params()
  #   plot_doorsign(
  #     dfr = p$dfr, im_profile = p$im_profile, logo_left = p$logo_left, logo_right = p$logo_right,
  #     pos_y_text = p$pos_y_text, im_profile_offset_y = p$im_profile_offset_y, im_profile_width = p$im_profile_width,
  #     path_export = store$epath, format_export = "pdf"
  #   )
  # }

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  # output$btn_download <- downloadHandler(
  #   filename = "door-sign.pdf",
  #   content = function(file) {
  #     progress <- shiny::Progress$new()
  #     progress$set(message = "Generating PDFs...", value = 45)
  #     fn_download()

  #     progress$set(message = "Downloading file...", value = 90)
  #     file.copy(file.path(store$epath, "door-sign.pdf"), file, overwrite = T)

  #     progress$set(message = "Completed.", value = 100)
  #     progress$close()
  #   }
  # )

  ## OUT: out_text ------------------------------------------------------------
  ## debug output

  output$out_text <- renderPrint({

    x <- evr_update()
    # x <- fn_params()
    print(str(x))

  })

}

shinyApp(ui = ui, server = server)
