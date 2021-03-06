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
                           column(3,style="max-width:330px;background:#ebedef;padding-top:15px;padding-bottom:15px;border-radius:4px;",
                                  fluidRow(
                                    column(12,style="margin-bottom:10px;",
                                           textInput("in_text_name",NULL,value="John Doe",placeholder="Name"),
                                           textInput("in_text_title",NULL,value="Bioinformatician; NBIS",placeholder="Job title"),
                                           textInput("in_text_dept",NULL,value="Dept. of Cell and Molecular Biology (ICM)",placeholder="Department"),
                                           textInput("in_text_email",NULL,value="john.doe@nbis.se,john.doe@scilifelab.se",placeholder="Email(s)"),
                                           shinyBS::bsTooltip(id="in_text_email",title="Enter multiple emails using comma (,).",placement="top",trigger="hover"),
                                           textInput("in_text_phone",NULL,value="0730000000",placeholder="Phone"),
                                           shinyBS::bsTooltip(id="in_text_phone",title="Enter multiple phone numbers using comma (,).",placement="top",trigger="hover")
                                           #HTML('<div class="help-note"><i class="fas fa-info-circle"></i>  Enter multiple email/phone using comma (,).</div>')
                                    )
                                  ),
                                  sliderInput("in_pos_y_text","Text vertical position",min=0.10,max=0.90,value=0.56,step=0.01),
                                  fluidRow(
                                    column(12,style="margin-top:15px;",
                                           fileInput("in_im_profile","Profile image",multiple=FALSE)
                                    )
                                  ),
                                  fluidRow(
                                    column(6,class="no-pad-right",
                                           numericInput("in_im_profile_width","Image size",min=0.10,max=0.80,value=0.40,step=0.01),
                                           shinyBS::bsTooltip(id="in_im_profile_width",title="Width of profile image. Value between 0.1 and 0.8.",placement="top",trigger="hover")
                                    ),
                                    column(6,class="no-pad-left",
                                           numericInput("in_im_profile_offset_y","Image position",min=0.01,max=0.40,value=0.20,step=0.01),
                                           shinyBS::bsTooltip(id="in_im_profile_offset_y",title="Distance of profile image from top edge. Value between 0.01 and 0.2.",placement="top",trigger="hover")
                                    )
                                  ),
                                  fluidRow(style="margin-bottom:15px;margin-top:5px;",
                                           column(6,class="no-pad-right",
                                                  numericInput("in_nudge_x","Image nudge x",min=-100,max=100,value=0,step=1),
                                                  shinyBS::bsTooltip(id="in_nudge_x",title="Horizontal adjustment for profile image inside circular mask. Relevant for landscape images. Value between -100 and 100.",placement="top",trigger="hover")
                                                  
                                           ),
                                           column(6,class="no-pad-left",
                                                  numericInput("in_nudge_y","Image nudge y",min=-100,max=100,value=0,step=1),
                                                  shinyBS::bsTooltip(id="in_nudge_y",title="Vertical adjustment for profile image inside circular mask. Relevant for portrait images. Value between -100 and 100.",placement="top",trigger="hover")
                                           )
                                  ),
                                  div(style="margin-top:25px;margin-bottom:25px;",
                                      downloadButton("btn_download","Download"),
                                      shinyBS::bsTooltip(id="btn_download",title="Print on A4 paper at 105% scaling and fold in half.",placement="right",trigger="hover")
                                  ),
                                  div(style="font-size:0.8em;",paste0(format(Sys.time(),'%Y'),' • Roy Francis • Version: ',fn_version()))
                           ),
                           column(6,
                                  div(class="img-output",
                                      imageOutput("out_plot",width="auto",height="auto")
                                  )
                           )
                         )
                  )
                )
)

# SERVER -----------------------------------------------------------------------

server <- function(input, output, session) {
  
  ## get temporary directory
  store <- reactiveValues(epath=tempdir())
  
  ## FN: fn_params ------------------------------------------------------------
  ## function to get plot params
  
  fn_params <- reactive({
    
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
        if(!y %in% c("jpg","jpeg","png","gif")) return("Image must be one of JPG/JPEG, PNG or GIF formats.")
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
    
    scaling <- 2.8
    return(list(src=file.path(store$epath,"door-sign.png"),contentType="image/png",
                width=round(297*scaling,0),
                height=round(210*scaling,0),
                alt="doorsign"))
  },deleteFile=TRUE)
  
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
