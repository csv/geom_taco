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
      c("label","fill","salsa","lime","radish","guacamole","cilantro"), name = "geom_taco")
    if (empty(data)) return(zeroGrob())
    print(data)   
  }

  draw_legend <- function(., data, ...) {
    data <- aesdefaults(data, .$default_aes(), list(...))
    print(data)   
  }

  default_stat <- function(.) StatIdentity
  required_aes <- c("label")
  default_aes <- function(.) aes(fill = 'carne asada',
    salsa = FALSE, lime = FALSE, radish = FALSE, guacamole = FALSE, cilantro = FALSE)
})
