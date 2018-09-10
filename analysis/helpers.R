# function to decode polylines returned from Google Maps Directions API, returns lat and long of given leg
library(hrbrthemes)
import_roboto_condensed()

decodeLine <- function(encoded){
  require(bitops)
  
  vlen <- nchar(encoded)
  vindex <- 0
  varray <- NULL
  vlat <- 0
  vlng <- 0
  
  while(vindex < vlen){
    vb <- NULL
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen){
        vindex <- vindex + 1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63  
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlat <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlat <- vlat + dlat
    
    vshift <- 0
    vresult <- 0
    repeat{
      if(vindex + 1 <= vlen) {
        vindex <- vindex+1
        vb <- as.integer(charToRaw(substr(encoded, vindex, vindex))) - 63        
      }
      
      vresult <- bitOr(vresult, bitShiftL(bitAnd(vb, 31), vshift))
      vshift <- vshift + 5
      if(vb < 32) break
    }
    
    dlng <- ifelse(
      bitAnd(vresult, 1)
      , -(bitShiftR(vresult, 1)+1)
      , bitShiftR(vresult, 1)
    )
    vlng <- vlng + dlng
    
    varray <- rbind(varray, c(vlat * 1e-5, vlng * 1e-5))
  }
  coords <- data.frame(varray)
  names(coords) <- c("lat", "lon")
  coords
}


# theme for static heat map of bike trips
theme_dark_map <- function(..., base_size = 12){
  
  theme_bw(..., base_size) + 
    theme(text = element_text( color = "#ffffff"),
          rect = element_rect(fill = "#000000", color = "#000000"),
          plot.background = element_rect(fill = "#000000", color = "#000000"),
          panel.background = element_rect(fill = "#000000", color = "#000000"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(face = "bold", hjust = 0.5),
          plot.subtitle = element_text( hjust = 0.5))
}


theme_ds <- function(base_size = 12){
  theme_bw(base_size) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, face = "bold"),
        axis.title = element_text(face = "bold"),
        strip.background = element_rect(fill = "white"),
        strip.text = element_text(face = "bold"),
        axis.text = element_text(face = "bold"))
}


theme_dark_ds <- function(base_size = 12){
  theme_bw(base_size) + 
  theme(  text = element_text(face = "bold", family = "Roboto Condensed", colour = "grey"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, face = "bold"),
         #axis.text.x = element_text(angle = 45, hjust = 1),  
         axis.text = element_text(colour = "grey", face = "bold"),
         axis.ticks = element_line(colour = "grey"),
         panel.border = element_rect(colour = "white"),
         plot.background = element_rect(fill = "#252525", colour = "#252525"),
         panel.background = element_rect(fill = "#252525", colour = "#252525"),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         legend.background = element_rect(fill = "#252525"),
         legend.key = element_rect(colour = "grey", fill = "#252525"),
         strip.background = element_rect(fill = "#252525", colour = "white"),
         strip.text = element_text(colour = "grey", face = "bold")
  ) 
}

theme_colbar <- function(location = "bottom"){
  theme(legend.position = location,
    legend.direction = "horizontal", 
    legend.key.height = unit(2, units = "mm"),
    legend.key.width = unit(10, units = "mm")) 
}
base_commarea_map <- function(x, palette = "viridis"){
  ggplot(x) +
    geom_polygon(data = broom::tidy(areas), aes(x = long, y = lat, group = group), colour = "#252525") + 
    geom_polygon(aes(x = long, y = lat, fill = avg, group = group)) +
    geom_path(aes(x = long, 
                  y = lat, 
                  group = group), 
              color = "black", size = 0.1) +
    scale_fill_viridis(option = palette, direction = 1,
                       guide = guide_colorbar(
                         direction = "horizontal",
                         barheight = unit(2, units = "mm"),
                         barwidth = unit(30, units = "mm"),
                         draw.ulim = F,
                         title.position = "top",
                         title.hjust = 0.5,
                         label.hjust = 0.5)) +
    # pretty_breaks
    # scale_fill_manual(
    #   values = rev(magma(8)[2:6]),
    #   breaks = rev(brks_scale),
    #   name = "Avg Trips",
    #   drop = FALSE,
    #   labels = rev(brks_scale),
    #   guide = guide_legend(
    #     direction = "horizontal",
    #     keyheight = unit(2, units = "mm"),
    #     keywidth = unit(70/6, units = "mm"),
  #     title.position = "top",
  #     title.hjust = 0.5,
  #     label.hjust = 1,
  #     nrow = 1,
  #     byrow = T,
  #     reverse = T,
  #     label.position = "bottom"
  #   )) +
  # theme_dark_ds() + 
  theme_dark_map() + 
    theme(legend.position = "top") +
    coord_equal()
}
