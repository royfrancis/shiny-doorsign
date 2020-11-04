# doorsign
# functions

library(Cairo)
library(curl)
library(ggplot2)
# sudo apt install libmagick++-dev
library(magick)
library(png)
library(shiny)
library(shinyBS)
library(shinythemes)
library(showtext)

if(!"gfont" %in% sysfonts::font_families()) sysfonts::font_add_google("Merriweather","gfont")
showtext_opts(dpi=300)

# fn_version
fn_version <- function() {
  return("v1.0.0")
}

# validation
fn_validate <- function(input,message1,message2,message3)
{

  if(missing(message1)) message1 <- "Input is missing."
  gcheck <- length(grep("Argument \\'\\w+\\' missing",message1))
  if(gcheck == 1)
  {
    m1 <- sub("Argument ","",message1)
    m1 <- sub(" missing.","",m1)
  }

  if (all(is.null(input))) {
    if(missing(message1)) message1 <- "Input is missing."
    print(message1)
  } else if (is.numeric(input) | is.list(input)) {
    if(all(is.na(input)))
    {
      if(missing(message2))
      {
        if(gcheck==1) message2 <- paste0("Argument ",m1," is NA.",sep="")
        if(gcheck!=1) message2 <- "Input is NA."
      }
      print(message2)
    }
  } else if (is.character(input)) {
    if(all(nchar(input) == 0))
    {
      if(missing(message3))
      {
        if(gcheck==1) message3 <- paste0("Argument ",m1," is empty.",sep="")
        if(gcheck!=1) message3 <- "Input is empty."
      }
      print(message3)
    }
  } else {
    NULL
  }
}

# img_dims_left ------------------------------------------------------------

#' @title img_dims_left
#' @description Computes image dimensions to place on a plot
#' @param img An image object from readPNG()
#' @param img_width Final image width in plot units
#' @param img_pos_x Image x start position
#' @param img_pos_y Image y end position
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#' 
img_dims_left <- function(img,img_width,img_pos_x,img_pos_y,canvas_height,canvas_width) {
  
  w_scaler <- canvas_width/canvas_height
  img_height <- ((img_width*nrow(img))/ncol(img))*w_scaler
  img_x1 <- img_pos_x
  img_x2 <- img_x1+img_width
  img_y2 <- 1-(img_pos_y+(img_pos_y*w_scaler))
  img_y1 <- round(img_y2-img_height,3)
  
  return(list(xmin=img_x1,xmax=img_x2,ymin=img_y1,ymax=img_y2))
}

# img_dims_right ------------------------------------------------------------

#' @title img_dims_right
#' @description Computes image dimensions to place on a plot
#' @param img An image object from readPNG()
#' @param img_width Final image width in plot units
#' @param img_pos_x Image x start position
#' @param img_pos_y Image y end position
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#' 
img_dims_right <- function(img,img_width,img_pos_x,img_pos_y,canvas_height,canvas_width) {
  
  w_scaler <- canvas_width/canvas_height
  img_height <- ((img_width*nrow(img))/ncol(img))*w_scaler
  img_x2 <- 1-img_pos_x
  img_x1 <- img_x2-img_width
  img_y2 <- 1-(img_pos_y+(img_pos_y*w_scaler))
  img_y1 <- round(img_y2-img_height,3)
  
  return(list(xmin=img_x1,xmax=img_x2,ymin=img_y1,ymax=img_y2))
}

# plot_doorsign ------------------------------------------------------------

#' @title plot_doorsign
#' @description Creates an A4 sized image with door sign information
#' @param dfr Data.frame with columns label and type [data.frame]
#' @param pos_x X-axis alignment [numeric]
#' @param pos_y_text Y-axis position where text starts [numeric]
#' @param line_spacing Spacing between text [numeric]
#' @param canvas_height Height of output image in mm [numeric]
#' @param canvas_width Width of output image in mm [numeric]
#' @param img_profile A png array denoting profile image [array]
#' @param img_profile_width Final width of profile image in plot units [numeric]
#' @param img_profile_pos_x X-axis start position of profile image [numeric]
#' @param img_profile_pos_y Y-axis position of profile image [numeric]
#' @param logo_left Logo for top left. Output from readPNG() [array]
#' @param logo_left_width Width of left logo in plot units [numeric]
#' @param logo_left_pos_x X-position for left logo [numeric]
#' @param logo_left_pos_y Y-position for left logo [numeric]
#' @param logo_right Logo for top right. Output from readPNG() [array]
#' @param logo_right_width Width of right logo in plot units [numeric] 
#' @param logo_left_pos_x X-position for right logo [numeric]
#' @param logo_left_pos_y Y-position for right logo [numeric]
#' @param path_export Export path [character]
#' @param format_export Export filetype, png or pdf [character]
#' 
plot_doorsign <- function(dfr,pos_x=0.12,pos_y_text=0.56,line_spacing=0.042,canvas_height=210,canvas_width=148.5,
                          img_profile=NULL,img_profile_width=0.4,img_profile_pos_x=NULL,img_profile_pos_y=0.12,
                          logo_left=NULL,logo_left_width=0.14,logo_left_pos_x=0.06,logo_left_pos_y=0.03,
                          logo_right=NULL,logo_right_width=0.26,logo_right_pos_x=0.06,logo_right_pos_y=0.03,
                          logo_email=NULL,logo_phone=NULL,logo_comm_width=0.05,logo_comm_pos_x=NULL,path_export=".",
                          format_export="png",debug=FALSE) {
  
  if(is.null(img_profile_pos_x)) img_profile_pos_x <- pos_x
  if(is.null(logo_comm_pos_x)) logo_comm_pos_x <- pos_x
  dfr$x <- pos_x
  dfr$y <- 0
  
  # make y positions for text
  for(i in 1:nrow(dfr)) {
    if(dfr$type[i]=="name") sp <- pos_y_text
    if(dfr$type[i]=="title") sp <- dfr$y[i-1]+(line_spacing*1.2)
    if(dfr$type[i]=="dept") sp <- dfr$y[i-1]+(line_spacing*1)
    if(dfr$type[i]=="email" | dfr$type[i]=="phone") sp <- dfr$y[i-1]+line_spacing
    dfr$y[i] <- sp
  }
  
  # create plot, add text
  p <- ggplot()+
    geom_text(data=subset(dfr,dfr$type=="name"),aes(x=x,y=1-y,label=label),hjust=0,size=7.5,fontface="bold",family="gfont")
  p <- p+geom_text(data=subset(dfr,dfr$type=="title"),aes(x=x,y=1-y,label=label),hjust=0,size=7,family="gfont")
  p <- p+geom_text(data=subset(dfr,dfr$type=="dept"),aes(x=x,y=1-y,label=label),hjust=0,size=5.5,family="gfont")
  p <- p+geom_text(data=subset(dfr,dfr$type=="email"),aes(x=x,y=1-y,label=label),hjust=0,size=5,family="gfont")
  p <- p+geom_text(data=subset(dfr,dfr$type=="phone"),aes(x=x,y=1-y,label=label),hjust=0,size=5,family="gfont")
  
  p <- p+
    coord_cartesian(xlim=c(0,2),ylim=c(0,1))+
    scale_x_continuous(expand=c(0,0))+
    scale_y_continuous(expand=c(0,0))
  
  # add rectangle motif left
  if(!debug) p <- p+annotate("rect",xmin=0,xmax=0.075,ymin=min(1-dfr$y)-0.02,ymax=max(1-dfr$y)+0.02,colour="#A7C947",fill="#A7C947")
  if(debug) p <- p+annotate("rect",xmin=0,xmax=0.075,ymin=min(1-dfr$y)-0.02,ymax=max(1-dfr$y)+0.02,fill="grey80",colour="grey80",alpha=0.5)
  
  # add right logo
  if(!is.null(logo_right)) {
    dims_logo_right <- img_dims_right(logo_right,logo_right_width,logo_right_pos_x,logo_right_pos_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(logo_right,xmin=dims_logo_right$xmin,xmax=dims_logo_right$xmax,
                                          ymin=dims_logo_right$ymin,ymax=dims_logo_right$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_logo_right$xmin,xmax=dims_logo_right$xmax,
                              ymin=dims_logo_right$ymin,ymax=dims_logo_right$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_logo_right$xmin+((dims_logo_right$xmax-dims_logo_right$xmin)/2),
                 y=dims_logo_right$ymin+((dims_logo_right$ymax-dims_logo_right$ymin)/2),
                 label=paste0("raw: x",logo_right_pos_x,", y",logo_right_pos_y,"\n",
                              "final: xmin",dims_logo_right$xmin,", ymin",dims_logo_right$ymin),
                 size=3)
  }
  
  # add left logo
  if(!is.null(logo_left)) {
    dims_logo_left <- img_dims_left(logo_left,logo_left_width,logo_left_pos_x,logo_left_pos_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(logo_left,xmin=dims_logo_left$xmin,xmax=dims_logo_left$xmax,
                                          ymin=dims_logo_left$ymin,ymax=dims_logo_left$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_logo_left$xmin,xmax=dims_logo_left$xmax,
                              ymin=dims_logo_left$ymin,ymax=dims_logo_left$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_logo_left$xmin+((dims_logo_left$xmax-dims_logo_left$xmin)/2),
                 y=dims_logo_left$ymin+((dims_logo_left$ymax-dims_logo_left$ymin)/2),
                 label=paste0("raw: x",logo_left_pos_x,", y",logo_left_pos_y,"\n",
                              "final: xmin",dims_logo_left$xmin,", ymin",dims_logo_left$ymin),
                 size=3)
  }
  
  # add profile image
  if(!is.null(img_profile)) {
    
    dims_img_profile <- img_dims_left(img_profile,img_profile_width,img_profile_pos_x,img_profile_pos_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(img_profile,xmin=dims_img_profile$xmin,xmax=dims_img_profile$xmax,
                                          ymin=dims_img_profile$ymin,ymax=dims_img_profile$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_img_profile$xmin,xmax=dims_img_profile$xmax,
                              ymin=dims_img_profile$ymin,ymax=dims_img_profile$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_img_profile$xmin+((dims_img_profile$xmax-dims_img_profile$xmin)/2),
                 y=dims_img_profile$ymin+((dims_img_profile$ymax-dims_img_profile$ymin)/2),
                 label=paste0("raw: x",img_profile_pos_x,", y",img_profile_pos_y,"\n",
                              "final: xmin",dims_img_profile$xmin,", ymin",dims_img_profile$ymin),
                 size=3)
  }
  
  # add email icon
  if(!is.null(logo_email)) {
    dfr_email <- subset(dfr,dfr$type=="email")
    for(i in 1:nrow(dfr_email)) {
      dims_logo_email <- img_dims_left(logo_email,logo_comm_width,logo_comm_pos_x,1-(dfr_email$y[i]),canvas_height,canvas_width)
      if(!debug) p <- p + annotation_raster(logo_email,xmin=dims_logo_email$xmin,xmax=dims_logo_email$xmax,
                                            ymin=dims_logo_email$ymin,ymax=dims_logo_email$ymax)
      if(debug) p <- p+annotate("rect",xmin=dims_logo_email$xmin,xmax=dims_logo_email$xmax,
                                ymin=dims_logo_email$ymin,ymax=dims_logo_email$ymax,
                                fill="white",colour="grey80")+
          annotate("text",x=dims_logo_email$xmin+((dims_logo_email$xmax-dims_logo_email$xmin)/2),
                   y=dims_logo_email$ymin+((dims_logo_email$ymax-dims_logo_email$ymin)/2),
                   label=paste0("raw: x=",logo_email_pos_x,", y=",dfr_email$y[i]),
                   size=3)
    }
  }
  
  # add phone icon
  if(!is.null(logo_phone)) {
    dfr_phone <- subset(dfr,dfr$type=="phone")
    for(i in 1:nrow(dfr_phone)) {
      dims_logo_phone <- img_dims_left(logo_phone,logo_comm_width,logo_comm_pos_x,1-dfr_phone$y[i],canvas_height,canvas_width)
      if(!debug) p <- p + annotation_raster(logo_phone,xmin=dims_logo_phone$xmin,xmax=dims_logo_phone$xmax,
                                            ymin=dims_logo_phone$ymin,ymax=dims_logo_phone$ymax)
      if(debug) p <- p+annotate("rect",xmin=dims_logo_phone$xmin,xmax=dims_logo_phone$xmax,
                                ymin=dims_logo_phone$ymin,ymax=dims_logo_phone$ymax,
                                fill="white",colour="grey80")+
          annotate("text",x=dims_logo_phone$xmin+((dims_logo_phone$xmax-dims_logo_phone$xmin)/2),
                   y=dims_logo_phone$ymin+((dims_logo_phone$ymax-dims_logo_phone$ymin)/2),
                   label=paste0("raw: x=",logo_comm_pos_x,", y=",dfr_phone$y[i]),
                   size=3)
    }
  }
  
  # add page divider
  p <- p+annotate("segment",x=1,xend=1,y=0,yend=1,linetype=3,colour="grey70")
  
  p <- p+
    labs(x="",y="")+
    theme(legend.position="none",
          axis.text=element_blank(),
          axis.title=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0,"lines"),
          strip.background=element_blank(),
          strip.text=element_blank(),
          axis.ticks=element_blank(),
          panel.background=element_blank(),
          plot.background=element_blank(),
          plot.margin=margin(0,0,0,0),
          axis.ticks.length=unit(0,"pt"))
  
  height <- 210
  width <- 297
  
  if(format_export=="pdf") {
    grDevices::cairo_pdf(file=file.path(path_export,"door-sign.pdf"),height=round(height/25.4,2),width=round(width/25.4,2))
    showtext::showtext_begin()
    print(p)
    showtext::showtext_end()
    dev.off()
  }
  
  if(format_export=="png") {
    grDevices::png(file=file.path(path_export,"door-sign.png"),width=width,height=height,units="mm",res=300,type="cairo")
    showtext::showtext_begin()
    print(p)
    showtext::showtext_end()
    dev.off()
  }
  
}
