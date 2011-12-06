   #  A class to represent the functions as being specific to a JavaScript Canvas device
setClass("RJavaScriptCanvasMethods",
          representation(result = "FunctionOrNULL"),
          contains = "RDevDescMethods")

setClass("JavaScriptCanvasDevice", contains = "DevDescPtr")

setClass("JavaScriptCanvasPlot",
          representation(canvasId = "character",
                         dims = "numeric"),
          contains = "character") # should we have this extend "character" or contain a code = "character"????

setGeneric("getCode",
            function(from, ...)
               standardGeneric("getCode"))

setMethod("getCode", "JavaScriptCanvasDevice",
           function(from, ...)
              from$deviceSpecific$state@result)  #XXX for now return the function

setAs("JavaScriptCanvasDevice", "RJavaScriptCanvasMethods",
           function(from)
              from$deviceSpecific$state) 


jsCanvas =
  # The function that creates the graphics device, assembling the
  #  graphical functions that share a common state where the code is written.
  #
  #  file can be a file name or a connection.
  #  It can also be an expression in which case we will return the generated code.
  #  ... is currently for wrapup, but we may want to use if for additional parameter settings.
  #
function(file = character(), dim = c(1000, 800), col = "black", fill = "transparent", ps = 10, wrapup = writeCode,
         canvasId  = "canvas", multiCanvas = FALSE, ...,
          runPlotCommand = !missing(file) && !is.character(substitute(file)))  # need runPlotCommand to be a lot smarter.
                                                                               # perhaps a new function jsPlot() to indicate a plot command.
                                                                               # can't easily differentiate between paste(file, ".txt")
                                                                               # and plot(1:10).  But we can look at file after we have opened the device.
{
  cmd = substitute(file)

  if(!is.matrix(dim))
    dim = matrix(dim, , 2, dimnames = list(canvasId, c('width', 'height')))
  
  result = character()
  pages = list()
  commands = character()
  add = function(x, restore = FALSE)
    commands <<- c(commands, if(restore) "ctx.save();", x, if(restore) "ctx.restore();")

  endPage = function()
    {
      if(length(commands)) {
         plot = new("JavaScriptCanvasPlot", commands)
         pageNum = length(pages) + 1L
         if(multiCanvas) {
           plot@canvasId = if(length(canvasId) == 1)
                               paste(canvasId, pageNum, sep = "")
                           else
                               canvasId[pageNum]
         } else {
           plot@canvasId = canvasId
         }

         plot@dims = as.numeric(dim[ (pageNum - 1L) %% nrow(dim) + 1L , ])
         pages[[ pageNum ]] <<- plot
         commands <<- character()
      }
    }


  funs = as(dummyDevice(), "RJavaScriptCanvasMethods")

#
# circle, line, rect, 
# text
# polygon, polyline
# strWidth,
# metricInfo, strWidthUTF8,

# size
# newPage


    # No implementations for
  funs@mode = NULL
  funs@activate = NULL
  funs@deactivate = NULL
  funs@deactivate = NULL
  funs@locator = NULL
  funs@onExit = NULL

  
  funs@line = function(x1, y1, x2, y2, context, dev) {
    add(c("// line",
          "ctx.beginPath();",
          setContext(context),
          sprintf("ctx.moveTo(%s, %s);", as.integer(x1), as.integer(y1)),
          sprintf("ctx.lineTo(%s, %s);", as.integer(x2), as.integer(y2)),
          "ctx.stroke();"))
  }

  funs@rect = function(x1, y1, x2, y2, context, dev) {
    op = if(!isTransparent(context$fill))  "fillRect" else "strokeRect"

    add(c("// rect",
          setContext(context),
          sprintf("ctx.%s(%d, %d, %d, %d);",
                     op,
                     as.integer(min(x1, x2)), as.integer(min(y1, y2)),
                     abs(as.integer(x2 - x1)), abs(as.integer(y2 - y1)))))
  }

  funs@circle = function(x1, y1, r, context, dev) {

    add(c("// circle",
          "ctx.beginPath()",
          setContext(context),
          sprintf("ctx.arc(%d, %d, %d, 0, 2 * Math.PI, true);", as.integer(x1), as.integer(y1), as.integer(r)),
          if(isTransparent(context$fill)) "ctx.fill()" else "ctx.stroke()"))
  }

  funs@text = function(x1, y1, txt, rot, hadj, context, dev) {

    add(c("// text",
                                        #        'ctx.fillStyle = "Black";', # fillText()
           setContext(context),
           #  sprintf("ctx.rotate(%f)", rot),
           sprintf("drawText(ctx, '%s', %d, %d, %d, '%s', %f, %f);",
                    "sans", as.integer(max(10, context$ps) * context$cex),
                     as.integer(x1), as.integer(y1), txt, rot, hadj)), restore = FALSE)
  }

  funs@strWidth = function(str, gcontext, dev) {
     nchar(str) * max(10, gcontext$ps) * gcontext$cex
  }

  funs@newPage = function(gcontext, dev) {
     endPage()
  }


  funs@close = function(dev) {
      endPage()
      result <<- wrapup(pages, if(runPlotCommand) character() else file, ...)
 }


  funs@polyline =
      #
      # XXX Connecting all the successive points. i.e. Polygon.
      #
  function(n, x, y, gcontext, dev) {

    x = x[1:n]
    y = y[1:n]
    add(c(paste("// polyline", n),
        "ctx.beginPath();",
        setContext(gcontext),
            # move to the first point and then connect all the subsequent ones.
        sprintf("ctx.moveTo(%s, %s);", as.integer(x[1]), as.integer(y[1])),
        sprintf("ctx.lineTo(%s, %s);", as.integer(x[-1]), as.integer(y[-1])),
        "ctx.stroke();"))
  }

  funs@polygon =
    function(n, x, y, gcontext, dev)
      {
        x = x[1:n]
        y = y[1:n]
        
        add(c(paste("// polygon", n),
              "ctx.beginPath();",
              setContext(gcontext),
              sprintf("ctx.moveTo(%s, %s);", as.integer(x[1]), as.integer(y[1])),
              sprintf("ctx.lineTo(%s, %s);", as.integer(x[-1]), as.integer(y[-1])),
              "ctx.closePath()",
              if(isTransparent(gcontext$fill)) "ctx.stroke();" else "ctx.fill();"))
      }



  funs@initDevice = function(dev) {

       # The all important parameter to set ipr to get the plot region with adequate margins
    dev$ipr = rep(1/72.27, 2)
    dev$cra = rep(c(6, 13)/12) * 10
    dev$startps = 10
    dev$canClip = TRUE
    dev$canChangeGamma = TRUE
    dev$startgamma = 1
    dev$startcol = as("black", "RGBInt")
  }
  funs@result = function() result

  dev = graphicsDevice(funs, dim, col, fill, ps)

  dev$ipr = rep(1/72.27, 2)
  dev$cra = rep(c(6, 13)/12) * 10


  if(runPlotCommand) {
     eval(cmd, sys.parent())
     dev.off()
#     endPage()
     result
  } else
     new("JavaScriptCanvasDevice", dev)
}

