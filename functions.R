# doorsign
# functions

library(Cairo)
library(curl)
library(ggplot2)
# sudo apt install libmagick++-dev
library(magick)
library(png)
library(shiny)
library(shinythemes)
library(showtext)

if(!"gfont" %in% sysfonts::font_families()) sysfonts::font_add_google("Merriweather","gfont")
showtext_opts(dpi=300)

# fn_version
fn_version <- function() {
  return("v1.1.0")
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

# fn_circ_image ------------------------------------------------------------

#' @title fn_circ_image
#' #' @description Converts input image into circular masked square aspect ratio
#' @param im An image from readPNG()
#' @param nudge_x
#' @param nudge_y
#' @return Returns a magick image object
#' 
fn_circ_image <- function(im,nudge_x,nudge_y) {
  
  im_orig <- magick::image_convert(im,format="png")
  im_orig_info_height <- magick::image_info(im_orig)$height
  im_orig_info_width <- magick::image_info(im_orig)$width
  gv <- ifelse(im_orig_info_height > im_orig_info_width,"North","Center")
  
  min_len <- min(im_orig_info_height,im_orig_info_width)
  im_crop <- magick::image_crop(im_orig, geometry=paste0(min_len,"x",min_len,"+",nudge_x,"+",nudge_y),gravity=gv,repage=TRUE)
  
  if(!exists("mask_scale")) mask_scale <- magick::image_read("www/mask.png")
  mask_scale_crop <- magick::image_scale(mask_scale,image_info(im_crop)$height)
  
  #mask_scale_crop <- magick::image_draw(image_blank(min_len,min_len))
  #symbols(min_len/2,min_len/2,circles=(min_len/2)-3,bg='black',inches=FALSE,add=TRUE)
  #dev.off()
  
  im_final <- image_composite(im_crop,mask_scale_crop,operator="Blend")
  
  return(im_final)
}

# im_dims_right ------------------------------------------------------------

#' @title im_dims_right
#' @description Computes image dimensions for a right aligned image
#' @param im An image object from readPNG()
#' @param im_width Final image width in plot units
#' @param im_offset_x Image distance from right x edge
#' @param im_offset_y Image distance from top y edge
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#'
im_dims_right <- function(im,im_width,im_offset_x,im_offset_y,canvas_height,canvas_width) {
  
  w_scaler <- canvas_width/canvas_height
  im_height <- ((im_width*nrow(im))/ncol(im))*w_scaler
  im_x2 <- 1-im_offset_x
  im_x1 <- im_x2-im_width
  im_y2 <- 1-im_offset_y
  im_y1 <- round(im_y2-im_height,3)
  
  return(list(xmin=im_x1,xmax=im_x2,ymin=im_y1,ymax=im_y2))
}

# im_dims_left ------------------------------------------------------------

#' @title im_dims_left
#' @description Computes image dimensions for a left aligned image
#' @param im An image object from readPNG()
#' @param im_width Final image width in plot units
#' @param im_offset_x Image distance from left x edge
#' @param im_offset_y Image distance from top y edge
#' @param canvas_height Canvas height in any units
#' @param canvas_width Canvas width in any units
#' 
im_dims_left <- function(im,im_width,im_offset_x,im_offset_y,canvas_height,canvas_width) {
  
  w_scaler <- canvas_width/canvas_height
  im_height <- ((im_width*nrow(im))/ncol(im))*w_scaler
  im_x1 <- im_offset_x
  im_x2 <- im_x1+im_width
  im_y2 <- 1-im_offset_y
  im_y1 <- round(im_y2-im_height,3)
  
  return(list(xmin=im_x1,xmax=im_x2,ymin=im_y1,ymax=im_y2))
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
#' @param im_profile A png array denoting profile image [array]
#' @param im_profile_width Final width of profile image in plot units [numeric]
#' @param im_profile_offset_x X-axis start position of profile image [numeric]
#' @param im_profile_offset_y Y-axis position of profile image [numeric]
#' @param logo_left Logo for top left. Output from readPNG() [array]
#' @param logo_left_width Width of left logo in plot units [numeric]
#' @param logo_left_offset_x X-position for left logo [numeric]
#' @param logo_left_offset_y Y-position for left logo [numeric]
#' @param logo_right Logo for top right. Output from readPNG() [array]
#' @param logo_right_width Width of right logo in plot units [numeric] 
#' @param logo_left_offset_x X-position for right logo [numeric]
#' @param logo_left_offset_y Y-position for right logo [numeric]
#' @param path_export Export path [character]
#' @param format_export Export filetype, png or pdf [character]
#' 
plot_doorsign <- function(dfr,pos_x=0.12,pos_y_text=0.56,line_spacing=0.042,canvas_height=210,canvas_width=148.5,
                          im_profile=NULL,im_profile_width=0.4,im_profile_offset_x=NULL,im_profile_offset_y=0.2,
                          logo_left=NULL,logo_left_width=0.14,logo_left_offset_x=0.06,logo_left_offset_y=0.03,
                          logo_right=NULL,logo_right_width=0.26,logo_right_offset_x=0.06,logo_right_offset_y=0.03,
                          logo_email=NULL,logo_phone=NULL,logo_comm_width=0.05,logo_comm_offset_x=NULL,path_export=".",
                          format_export="png",debug=FALSE) {
  
  if(is.null(im_profile_offset_x)) im_profile_offset_x <- 0.075
  if(is.null(logo_comm_offset_x)) logo_comm_offset_x <- pos_x
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
    dims_logo_right <- im_dims_right(logo_right,logo_right_width,logo_right_offset_x,logo_right_offset_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(logo_right,xmin=dims_logo_right$xmin,xmax=dims_logo_right$xmax,
                                          ymin=dims_logo_right$ymin,ymax=dims_logo_right$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_logo_right$xmin,xmax=dims_logo_right$xmax,
                              ymin=dims_logo_right$ymin,ymax=dims_logo_right$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_logo_right$xmin+((dims_logo_right$xmax-dims_logo_right$xmin)/2),
                 y=dims_logo_right$ymin+((dims_logo_right$ymax-dims_logo_right$ymin)/2),
                 label=paste0("raw: x",logo_right_offset_x,", y",logo_right_offset_y,"\n",
                              "final: xmin",dims_logo_right$xmin,", ymin",dims_logo_right$ymin),
                 size=3)
  }
  
  # add left logo
  if(!is.null(logo_left)) {
    dims_logo_left <- im_dims_left(logo_left,logo_left_width,logo_left_offset_x,logo_left_offset_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(logo_left,xmin=dims_logo_left$xmin,xmax=dims_logo_left$xmax,
                                          ymin=dims_logo_left$ymin,ymax=dims_logo_left$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_logo_left$xmin,xmax=dims_logo_left$xmax,
                              ymin=dims_logo_left$ymin,ymax=dims_logo_left$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_logo_left$xmin+((dims_logo_left$xmax-dims_logo_left$xmin)/2),
                 y=dims_logo_left$ymin+((dims_logo_left$ymax-dims_logo_left$ymin)/2),
                 label=paste0("raw: x",logo_left_offset_x,", y",logo_left_offset_y,"\n",
                              "final: xmin",dims_logo_left$xmin,", ymin",dims_logo_left$ymin),
                 size=3)
  }
  
  # add profile image
  if(!is.null(im_profile)) {
    
    dims_im_profile <- im_dims_left(im_profile,im_profile_width,im_profile_offset_x,im_profile_offset_y,canvas_height,canvas_width)
    if(!debug) p <- p + annotation_raster(im_profile,xmin=dims_im_profile$xmin,xmax=dims_im_profile$xmax,
                                          ymin=dims_im_profile$ymin,ymax=dims_im_profile$ymax)
    if(debug) p <- p+annotate("rect",xmin=dims_im_profile$xmin,xmax=dims_im_profile$xmax,
                              ymin=dims_im_profile$ymin,ymax=dims_im_profile$ymax,
                              fill="white",colour="grey80")+
        annotate("text",x=dims_im_profile$xmin+((dims_im_profile$xmax-dims_im_profile$xmin)/2),
                 y=dims_im_profile$ymin+((dims_im_profile$ymax-dims_im_profile$ymin)/2),
                 label=paste0("raw: x",im_profile_offset_x,", y",im_profile_offset_y,"\n",
                              "final: xmin",dims_im_profile$xmin,", ymin",dims_im_profile$ymin),
                 size=3)
  }
  
  # add email icon
  if(!is.null(logo_email)) {
    dfr_email <- subset(dfr,dfr$type=="email")
    for(i in 1:nrow(dfr_email)) {
      dims_logo_email <- im_dims_left(logo_email,logo_comm_width,logo_comm_offset_x,1-(dfr_email$y[i]),canvas_height,canvas_width)
      if(!debug) p <- p + annotation_raster(logo_email,xmin=dims_logo_email$xmin,xmax=dims_logo_email$xmax,
                                            ymin=dims_logo_email$ymin,ymax=dims_logo_email$ymax)
      if(debug) p <- p+annotate("rect",xmin=dims_logo_email$xmin,xmax=dims_logo_email$xmax,
                                ymin=dims_logo_email$ymin,ymax=dims_logo_email$ymax,
                                fill="white",colour="grey80")+
          annotate("text",x=dims_logo_email$xmin+((dims_logo_email$xmax-dims_logo_email$xmin)/2),
                   y=dims_logo_email$ymin+((dims_logo_email$ymax-dims_logo_email$ymin)/2),
                   label=paste0("raw: x=",logo_email_offset_x,", y=",dfr_email$y[i]),
                   size=3)
    }
  }
  
  # add phone icon
  if(!is.null(logo_phone)) {
    dfr_phone <- subset(dfr,dfr$type=="phone")
    for(i in 1:nrow(dfr_phone)) {
      dims_logo_phone <- im_dims_left(logo_phone,logo_comm_width,logo_comm_offset_x,1-dfr_phone$y[i],canvas_height,canvas_width)
      if(!debug) p <- p + annotation_raster(logo_phone,xmin=dims_logo_phone$xmin,xmax=dims_logo_phone$xmax,
                                            ymin=dims_logo_phone$ymin,ymax=dims_logo_phone$ymax)
      if(debug) p <- p+annotate("rect",xmin=dims_logo_phone$xmin,xmax=dims_logo_phone$xmax,
                                ymin=dims_logo_phone$ymin,ymax=dims_logo_phone$ymax,
                                fill="white",colour="grey80")+
          annotate("text",x=dims_logo_phone$xmin+((dims_logo_phone$xmax-dims_logo_phone$xmin)/2),
                   y=dims_logo_phone$ymin+((dims_logo_phone$ymax-dims_logo_phone$ymin)/2),
                   label=paste0("raw: x=",logo_comm_offset_x,", y=",dfr_phone$y[i]),
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

# create circular mask
#png(filename="mask.png",height=30,width=30,units="mm",res=300,type="cairo-png")
#par(mar=c(0,0,0,0))
#plot(x=1,y=1,type="n",xlim=c(1,ii3$width),ylim=c(1,ii3$height),bty="n",axes=FALSE,xlab="",ylab="")
#polygon(x = radius * cos(theta) + center_x, y = radius * sin(theta) + center_y,col = "black")
#dev.off()
