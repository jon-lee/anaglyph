anaglyph.plot <- function(x,y,z,left="red",right="cyan",depth="med",style="pop-out",type="p",...) {

  if (depth=="low") {
    scale <- 0.1
  } else if (depth=="med") {
    scale <- 0.2
  } else if (depth=="high") {
    scale <- 0.5
  }
  
  if (style=="pop-in") {
    shift <- 1
  } else if (style=="none") {
    shift <- 0.5
  } else if (style=="pop-out") {
    shift <- 0
  }
    
  # convert z-values to adjustment (between 0 and 1)
  adjust <- ((z - min(z))/(max(z)-min(z))-shift)*scale
  
  xy1 <- xy.coords(x+adjust/2,y)
  xy2 <- xy.coords(x-adjust/2,y)
  
  plot(xy1, col=right,...)
  plot.xy(xy2, col=left, type=type,...)
}
