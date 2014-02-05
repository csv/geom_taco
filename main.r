exports <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity",
na.rm = FALSE, ...) {
  GeomTaco$new(mapping = mapping, data = data, stat = stat, position = position, 
  na.rm = na.rm, ...)
}
      
GeomTaco <- proto(Geom, {
  objname <- "taco"

  draw_groups <- function(., ...) .$draw(...)
  draw <- function(., data, scales, coordinates, na.rm = FALSE, ...) {    
    data <- remove_missing(data, na.rm, 
      c("x", "label","meat","salsa","lime","radish"), name = "geom_taco")
    if (empty(data)) return(zeroGrob())
    
    with(coord_transform(coordinates, data, scales), 
      ggname(.$my_name(), pointsGrob(x, y, size=unit(size, "mm"), pch=shape, 
      gp=gpar(col=alpha(colour, alpha), fill = alpha(fill, alpha), fontsize = size * .pt)))
    )
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    
    with(data,
      pointsGrob(0.5, 0.5, size=unit(size, "mm"), pch=shape, 
      gp=gpar(
        col=alpha(colour, alpha), 
        fill=alpha(fill, alpha), 
        fontsize = size * .pt)
      )
    )
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("x", "y")
  default_aes <- function(.) aes(shape=16, colour="black", size=2, fill = NA, alpha = NA)
  
})
