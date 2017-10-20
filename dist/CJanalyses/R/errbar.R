errbar <- function( x, y, height, width, lty = 1, col = "black",
                    orientation = "v", ... )
{
  switch( orientation, 
            "v" = { xA <- x
                    xB <- x
                    yA <- y + height
                    yB <- y - height
                  },
            "h" = { xA <- x + height
                    xB <- x - height
                    yA <- y
                    yB <- y
                  },
          stop( "orientation can strictly take the values \"v\" (default) or \"h\"" ) )
  
  arrows( x0 = x, y0 = y, x1 = xA, y1 = yA, angle = 90, length = width,
          lty = lty, col = col, ... )
  
  arrows( x0 = x, y0 = y, x1 = xB, y1 = yB , angle = 90, length = width,
          lty = lty, col = col, ... )
}
