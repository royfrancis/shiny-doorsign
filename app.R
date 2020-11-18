## doorsign
## R shinyapp to generate door signs
## 2020 Roy Mathew Francis

source("functions.R")

# UI ---------------------------------------------------------------------------

ui <- fluidPage(theme=shinytheme("flatly"),
                tags$head(tags$link(rel="stylesheet",type="text/css",href="styles.css")),
                fixedRow(
                  column(12,style="margin:15px;",
                         fluidRow(style="margin-bottom:10px;",
                                  span(tags$img(src='nbis.png',style="height:18px;"),style="vertical-align:top;display:inline-block;"),
                                  span(tags$h4("•",style="margin:0px;margin-left:6px;margin-right:6px;"),style="vertical-align:top;display:inline-block;"),
                                  span(tags$h4(strong("Door Sign"),style="margin:0px;"),style="vertical-align:middle;display:inline-block;")
                         ),
                         fixedRow(
                           column(3,style="max-width:300px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
                                  fluidRow(
                                    column(12,style="margin-bottom:10px;",
                                           textInput("in_text_name",NULL,value="John Doe",placeholder="Name"),
                                           textInput("in_text_title",NULL,value="Bioinformatician; NBIS",placeholder="Job title"),
                                           textInput("in_text_dept",NULL,value="Dept. of Cell and Molecular Biology (ICM)",placeholder="Department"),
                                           textInput("in_text_email",NULL,value="john.doe@nbis.se,john.doe@scilifelab.se",placeholder="Email(s)"),
                                           textInput("in_text_phone",NULL,value="0730000000",placeholder="Phone"),
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Enter multiple email/phone using comma (,).</div>')
                                    )
                                  ),
                                  sliderInput("in_pos_y_text","Text vertical position",min=0.10,max=0.90,value=0.56,step=0.01),
                                  fluidRow(
                                    column(12,style="margin-top:15px;",
                                           fileInput("in_im_profile","Profile image",multiple=FALSE)
                                    )
                                  ),
                                  fluidRow(style="margin-bottom:10px;margin-top:10px;",
                                           column(6,style=list("padding-right: 3px;"),
                                                  numericInput("in_im_profile_width","Image size",min=0.10,max=0.80,value=0.40,step=0.01)
                                           ),
                                           column(6,style=list("padding-left: 3px;"),
                                                  numericInput("in_im_profile_offset_y","Image position",min=0.01,max=0.40,value=0.20,step=0.01)
                                           )
                                  ),
                                  fluidRow(style="margin-bottom:15px;margin-top:5px;",
                                           column(6,style=list("padding-right: 3px;"),
                                                  numericInput("in_nudge_x","Image nudge x",min=0,max=50,value=0,step=1)
                                           ),
                                           column(6,style=list("padding-left: 3px;"),
                                                  numericInput("in_nudge_y","Image nudge y",min=0,max=50,value=0,step=1)
                                           )
                                  ),
                                  div(style="margin-top:25px;margin-bottom:20px;",downloadButton("btn_download","Download")),
                                  div(style="font-size:0.8em;",paste0(format(Sys.time(),'%Y'),' • Roy Francis • Version: ',fn_version()))
                           ),
                           column(6,style="max-width:450px;min-width:400px;padding-top:15px;padding-bottom:15px;border-radius:4px;",
                                  sliderInput("in_scale","Image preview scale",min=1,max=6,step=0.2,value=3,width="100%"),
                                  fluidRow(
                                    column(12,style="padding-top:5px;padding-bottom:10px",
                                           HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Scale controls preview below and does not affect download.</div>')
                                    )
                                  ),
                                  div(class="img-output",
                                      imageOutput("out_plot",width="auto",height="auto")
                                  )
                                  #verbatimTextOutput("out_display")
                           )
                         )
                  )
                )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {

  ## get temporary directory
  store <- reactiveValues(epath=tempdir())
  
  ## UI: ui_settings -----------------------------------------------------------
  ## ui to display settings
  output$ui_settings <- renderUI({
    validate(fn_validate(input$in_settings))

    if(input$in_settings) {
      column(3,style="max-width:300px;border-radius:4px;background:#ebedef;",
        h4("Settings"),
        div(
          tags$b("Label 1"),
          fluidRow(
            column(4,style=list("padding-right: 3px;"),
                   numericInput("in_label1_size",label="Size",value=8,min=0,max=20,step=0.5)
            ),
            column(4,style=list("padding-right: 3px; padding-left: 3px;"),
                   numericInput("in_label1_x",label="Hor pos",value=0.5,min=0,max=1,step=0.02)
            ),
            column(4,style=list("padding-left: 3px;"),
                   numericInput("in_label1_y",label="Ver pos",value=0.54,min=0,max=1,step=0.02)
            )
          )
        )
      )
    }

  })

  ## FN: fn_params ------------------------------------------------------------
  ## function to get plot params

  fn_params <- reactive({

    #validate(fn_validate(input$in_text_name))
    #validate(fn_validate(input$in_text_title))
    #validate(fn_validate(input$in_text_dept))
    #validate(fn_validate(input$in_text_email))
    #validate(fn_validate(input$in_text_phone))
    #validate(fn_validate(input$in_image_profile))
    
    # if values are available, use them, else use defaults
    if(is.null(input$in_text_name)){text_name <- "John Doe"}else{text_name <- input$in_text_name}
    if(is.null(input$in_text_title)){text_title <- "Bioinformatician; NBIS"}else{text_title <- input$in_text_title}
    if(is.null(input$in_text_dept)){text_dept<- "Dept. of Cell and Molecular Biology (ICM)"}else{text_dept <- input$in_text_dept}
    if(is.null(input$in_text_email)){text_email <- "john.doe@nbis.se"}else{text_email <- input$in_text_email}
    if(is.null(input$in_text_phone)){text_phone <- "0730000000"}else{text_phone <- input$in_text_phone}
    
    if(is.null(input$in_pos_x)){pos_x <- 0.12}else{pos_x <- input$in_pos_x}
    if(is.null(input$in_pos_y_text)){pos_y_text <- 0.56}else{pos_y_text <- input$in_pos_y_text}
    if(is.null(input$in_line_spacing)){line_spacing <- 0.042}else{line_spacing <- input$in_line_spacing}
    if(is.null(input$in_im_profile_width)){im_profile_width <- 0.4}else{im_profile_width <- input$in_im_profile_width}
    if(is.null(input$in_im_profile_offset_y)){im_profile_offset_y <- 0.20}else{im_profile_offset_y <- input$in_im_profile_offset_y}

    if(is.null(input$in_nudge_x)){nudge_x <- 0}else{nudge_x <- input$in_nudge_x}
    if(is.null(input$in_nudge_y)){nudge_y <- 0}else{nudge_y <- input$in_nudge_y}
    
    # validation ---------------------------------------------------------------
    
    fn_val <- function(x) {
      dp <- deparse(substitute(x))
      dp <- switch(dp,"l1x"="Label 1 Hor pos","l2x"="Label 2 Hor pos",
                   "l3x"="Label 3 Hor pos","l1y"="Label 1 Ver pos",
                   "l2y"="Label 2 Ver pos","l3y"="Label 3 Ver pos",
                   "lls"="Logo left scale","lrs"="Logo right scale")
      if(x<0 | x>1) paste0("Input '",dp,"' must be between 0 and 1.")
    }
    #validate(fn_val(l1x))
    
    # dfr ----------------------------------------------------------------------
    
    dfr <- data.frame(label=c(text_name,text_title,text_dept),
                      type=c("name","title","dept"))
    dfr1 <- data.frame(label=trimws(unlist(strsplit(text_email,","))))
    dfr1$type <- "email"
    dfr2 <- data.frame(label=unlist(strsplit(text_phone,",")))
    dfr2$type <- "phone"
    dfr <- rbind(dfr,dfr1,dfr2)
    
    logo_right <- png::readPNG("www/scilifelab.png")
    logo_left <- png::readPNG("www/nbis.png")

    fn_validate_im <- function(x){
      if(!is.null(x)){
        y <- tolower(sub("^.+[.]","",basename(x$datapath)))
        if(!y %in% c("jpg","png","jpeg","gif")) return("Image must be one of JPG/JPEG, PNG or GIF formats.")
        if((x$size/1024/1024)>2) return("Image must be less than 2 MB in size.")
      }
    }
    
    validate(fn_validate_im(input$in_im_profile))
    
    if(is.null(input$in_im_profile)) {
      im_profile <- png::readPNG("www/profile.png")
    }else{
      # read image, convert to png, square aspect, export as png
      im_final <- fn_circ_image(im=magick::image_read(input$in_im_profile$datapath),nudge_x=nudge_x,nudge_y=nudge_y)
      
      magick::image_write(im_final,path=file.path(store$epath,"image.png"),format="png")
      im_profile <- png::readPNG(file.path(store$epath,"image.png"))
    }
    
    f <- "Merriweather"

    return(list(dfr=dfr,logo_right=logo_right,logo_left=logo_left,im_profile=im_profile,
                pos_y_text=pos_y_text,im_profile_offset_y=im_profile_offset_y,im_profile_width=im_profile_width))
  })

  ## OUT: out_plot ------------------------------------------------------------
  ## plots figure

  output$out_plot <- renderImage({
    progress1 <- shiny::Progress$new()
    progress1$set(message="Capturing settings...", value=10)
    p <- fn_params()
    
    progress1$set(message="Generating figure...", value=40)
    
    plot_doorsign(dfr=p$dfr,im_profile=p$im_profile,logo_left=p$logo_left,logo_right=p$logo_right,
                  pos_y_text=p$pos_y_text,im_profile_offset_y=p$im_profile_offset_y,im_profile_width=p$im_profile_width,
		  path_export=store$epath,format_export="png")
    progress1$set(message="Completed.", value=100)
    progress1$close()
    return(list(src=file.path(store$epath,"door-sign.png"),contentType="image/png",
                width=round(297*input$in_scale,0),
                height=round(210*input$in_scale,0),
                alt="doorsign"))
  },deleteFile=TRUE)
  
  ## OUT: out_display -------------------------------------------------------
  ## prints general variables for debugging

  output$out_display <- renderPrint({
    cat(paste0(
      "Input type: ",input$in_input,"\n",
      "Settings: ",input$in_settings,"\n",
      "Label 1 size: ",input$in_label1_size,"\n",
      "Label 2 size: ",input$in_label2_size,"\n",
      "Label 3 size: ",input$in_label3_size,"\n",
      "Label 1 x: ",input$in_label1_x,"\n",
      "Label 2 x: ",input$in_label2_x,"\n",
      "Label 3 x: ",input$in_label3_x,"\n",
      "Label 1 y: ",input$in_label1_y,"\n",
      "Label 2 y: ",input$in_label2_y,"\n",
      "Label 3 y: ",input$in_label3_y,"\n",
      "Logo left: ",input$in_logo_left,"\n",
      "Logo left offset: ",input$in_logo_left_offset,"\n",
      "Logo left scale: ",input$in_logo_left_scale,"\n",
      "Logo right: ",input$in_logo_right,"\n",
      "Logo right offset: ",input$in_logo_right_offset,"\n",
      "Logo right scale: ",input$in_logo_right_scale,"\n",
      "Font: ",input$in_family,"\n"
    ))
  })

  ## FN: fn_temp ---------------------------------------------------------------
  ## function to create filename and temporary dir
  
  # fn_temp <- reactive({
  #   
  #   req(fn_input())
  #   
  #   npages <- ceiling(nrow(fn_input())/8)
  #   if(npages>1) {fname <- "nametag_1.zip"}else{fname <- "nametag_1.png"}
  #   
  #   store$npages <- npages
  #   store$path <- tempdir()
  #   store$fname <- paste0(store$path,"/",fname)
  # })
  
  ## FN: fn_download -----------------------------------------------------------
  ## function to download a zipped file with images

  fn_download <- function(){
    p <- fn_params()
    plot_doorsign(dfr=p$dfr,im_profile=p$im_profile,logo_left=p$logo_left,logo_right=p$logo_right,
                  pos_y_text=p$pos_y_text,im_profile_offset_y=p$im_profile_offset_y,im_profile_width=p$im_profile_width,
		  path_export=store$epath,format_export="pdf")
  }

  ## DHL: btn_download ---------------------------------------------------------
  ## download handler for downloading zipped file

  output$btn_download <- downloadHandler(
    filename="door-sign.pdf",
    content=function(file) {
      progress <- shiny::Progress$new()
      progress$set(message="Generating PDFs...", value=45)
      fn_download()
      
      progress$set(message="Downloading file...", value=90)
      file.copy(file.path(store$epath,"door-sign.pdf"),file,overwrite=T)
      
      progress$set(message="Completed.", value=100)
      progress$close()
    }
  )
}

shinyApp(ui=ui, server=server)
