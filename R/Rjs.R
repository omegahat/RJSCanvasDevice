# See https://developer.mozilla.org/en/HTML/Canvas


# polyline.
# Fonts
# Fix the point size in the gcontext being 0.
# Explore other ways of rendering text.
# Polygon is very slow
#



asJSFunction =
  #
  # This takes the javascript code and puts it into a function
  # adding extra material to check that the JavaScript canvas is supported
  # and obtaining the context from the HTML canvas element.
  #
  #  code - the character vector giving the Javascript code for drawing the plot.
  #
  #  name - the name of the JavaScript function to be defined.
  #
  #  addContext - logical indicating whether to add extra code to determine
  #               if the JavaScript engine has support for the canvas
  #               and for obtaining the context object used for drawing.
  #
  #  canvasId - the value of the id attribute identifying the canvas element in the
  #             HTML document
  #
function(code, name = "rdraw", addContext = TRUE, canvasId = "device")
{
  paste(c(paste("function", name, "(canvas)"), "{", 
         if(addContext)
           paste("    ", 
                 c(sprintf("if(canvas == null) canvas = document.getElementById('%s');", canvasId),
                   "if(! canvas.getContext) return;",
                    "var ctx = canvas.getContext('2d');",
                    "ctx.clearRect(0, 0, canvas.width, canvas.height);",
                    "CanvasTextFunctions.enable(ctx);")),
         paste("     ", as(code, "character")), "}"), collapse = "\n")
}


setContext =
  # 
  #  set the current values for the JavaScript context based on the
  #  values from R's graphic context.
  #
function(context, restore = FALSE)
{
   col = as(context$col, "RGB")
   fill = as(context$fill, "RGB")   

   c(if(restore)
       "ctx.save()",
     if(!isTransparent(col))
         sprintf("ctx.strokeStyle = '%s';", col),
     if(!isTransparent(fill))
         sprintf("ctx.fillStyle = '%s';", fill),
     sprintf("ctx.lineWidth = '%.3f';", as.numeric(context$lwd)),
     sprintf("ctx.lineJoin = '%s';", jsLineJoin(context$ljoin)),
     sprintf("ctx.lineCap = '%s';", jsLineCap(context$lend)),
     if(restore)
       "ctx.restore()"
    )
}

jsLineJoin =
    # map the R value for a R_GE_linejoin value to a string that
    # we use in JavaScript for the lineJoin field of the JS context.
function(x)
{
      if(length(names(x)) == 0)
        x = as(x, 'R_GE_linejoin')

      tolower(gsub("(GE_|_JOIN)", "", names(x)))
}

jsLineCap =
    # map the R value for a R_GE_lineend value to a string that
    # we use in JavaScript for the lineCap field of the JS context.  
function(x)
{
     if(length(names(x)) == 0)
       x = as(x, 'R_GE_lineend')  

     tolower(gsub("(GE_|_CAP)", "", names(x)))
}


writeCode =
  #
  # Write the list of plots to a file/connection.
  #  pages is a list and each element is a character vector giving the
  #  JavaScript code for that plot.
  # It is now a JavaScriptCanvasPlot so it is a character vector of JS code, but
  #  also has  @dims and @canvasId slots.
  
  #  We call asJSFunction() for each of these, adding the computations to
  #  get the context from the specified <canvas> element(s) in the HTML document
  #  If there are multiple plots, the caller should supply either a vector of canvas identifiers
  #  or we will use canvas1, canvas2, canvas3, ...
  #
function(pages, file = character(), canvasId = 'canvas', funName = "rdraw", multiCanvas = FALSE,
         jsURLs = JSCode(),
         inlineJS = TRUE)
{
      if(length(pages) == 0) {
        warning("no content produced for plot")
        return(character())
      }

      if(length(file) == 0) 
          con = textConnection("foo", "w", local = TRUE)


# Need to open file first if that is what we are using.
# Perhaps leave this to HTML.
#      if(length(jsURLs))
#           addJavaScripts(jsURLs, inlineJS, con)
      
      ans =
        if(length(pages) == 1) {
          if(length(file)) con = file 
          cat(asJSFunction(pages[[1]], funName, canvasId = pages[[1]]@canvasId), "\n\n", sep = "\n", file = con)
          funName
        } else {
          if(length(file) > 0) {
            if(!inherits(file, "connection")) {
              con = file(file, "w")
              on.exit(close(con))
            } else
              con = file
          }
 
          if(multiCanvas && length(canvasId) < length(pages))
             canvasId = c(canvasId, paste('canvas', seq(length(canvasId) + 1, length(pages)), sep = ""))
        
          funNames = paste(funName, seq(along = pages), sep = "")
          mapply(function(code, id)
                      cat(asJSFunction(code, name = id, canvasId = code@canvasId),
                          "\n\n", sep = "\n", file = con),
                   pages, funNames)
             funNames
      }

      if(inlineJS && length(jsURLs)) {
        sapply(jsURLs,
                function(u) {
                   cat(paste("// ", u, "\n"),
                        readLines(u),
                       paste("// end of ", u, "\n"),
                      file = con, sep = "\n")
                 })
      }

      if(length(file))
        ans
      else
        textConnectionValue(con)

       
}



###############################################

#XXX
#  Add where to get the additional code, e.g. the canvastext.js
#   the additional functions such as clear()

# Why do we not get a call to rect at the beginning of each plot to clear the panel.



