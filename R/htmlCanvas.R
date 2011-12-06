###############  HTML Section ##################

htmlCanvas =
  #
  # The idea here is to create an HTML document with the code inside it.
  # We can do this if we get the code back which we can do by passing the plotting expressions to
  # this function. But we also have to handle the asynchronous case where we call dev.off()
  # and it writes the code.  To do this, we specify a different value of wrapup for the
  # jsCanvas() function.
  #
function(file, dim = c(1000, 800), template = system.file("template.html", package = "RJSCanvasDevice"), canvasId = "canvas", ...)
{
  if(missing(dim) || length(dim) == 0)  {
     dim = readCanvasDim(template)
     if(missing(canvasId))
       canvasId = rownames(dim)
  }

  jsCanvas(file, dim, wrapup = htmlWrapup, template = template, runPlotCommand = FALSE, canvasId = canvasId, ...)
}

readCanvasDim =
  #
  # Reads an HTML document and gets the dimensions of the different canvas objects
  # and returns a matrix with the rownames giving the name/id of the canvas.
  #
function(filename)
{
   doc = htmlParse(filename)
   nodes = getNodeSet(doc, "//canvas")
   if(length(nodes)) {
     ans = structure(sapply(nodes, function(x) as.numeric(xmlAttrs(x)[c("width", "height")])),
                      dimnames = list(c("width", "height"), sapply(nodes, xmlGetAttr, "id")))
#    if(ncol(ans) == 1)
#      as(ans, "numeric")
#    else
       t(ans)
   } else
     c(width = 1000, height = 800)
}

htmlWrapup =
  #
  #
  # XXX Fix up the width and height of the canvas elements. 
  #
  # Get the canvas dimensions from the HTML document.
  # If multiCanvas = TRUE, create new canvases in the HTML if there is only one.
  # Have writeCode return the name of the canvas as the names of the function names vector returned.
  #
  # [Done] Allow rewriting of the jsURLs prefixes. Provide a new function and allow the caller to specify
  #  a base URL and we tag the relevant file names on to that. See JSCode()
  #
  
function(pages, file, template = "template.html", dim = c(),
         canvasId = character(),
         multiCanvas = FALSE,
         jsURLs = JSCode(),
         inlineJS = TRUE, ...)
{

    # Now get the JavaScript code as one big block
    # We arrange for writeCode() to write to a text connection.
  con = textConnection(NULL, "w", local = TRUE)
  funNames = writeCode(pages, con, canvasId = canvasId, multiCanvas = multiCanvas, inlineJS = FALSE, ...)
  jsCode = textConnectionValue(con) 

 
    # Read the template
  doc = htmlParse(template)

    #####################################
   # Fix up the template to have the right number of canvas with the right names and with the right
   # dimensions.
   # XXX needs work. Use the info in each pages[[i]]@dims, @canvasId
    # get the id's of the canvases
  fixHTMLDoc(doc, pages, multiCanvas)

    ################################################
       # Now insert the JS code in the head.

  head = getNodeSet(doc, "//head")[[1]]  

  if(length(jsURLs)) 
    sapply(jsURLs, function(x) addJSCode(head, if(inlineJS) I(x) else x, x))

  addJSCode(head, jsCode)


    # Fix up empty <script> nodes as Firefox doesn't like <script ... />
    # but wants <script....></script>
  emptyScripts = getNodeSet(head, ".//script[string(.) = '']")
  sapply(emptyScripts, function(x) newXMLTextNode( " " , parent = x))
  
    # Add the onload to call the functions.
  body = getNodeSet(doc, "//body")[[1]]
  cmd = paste(if(multiCanvas) funNames else funNames[1], "()", collapse = "; ", sep = "")
  xmlAttrs(body) = c("onload" = cmd)
  

  saveXML(doc, file)
}

makeCanvasNode =
function(id, width = 1000, height = 800, parent = NULL)
{
  if(length(width) == 2 && missing(height))
    height = width[2]
  
  newXMLNode("canvas", attrs = c(id = id, width = width[1], height = height[1]),
              "\nIf you can see this, your browser doesn't support the JavaScript canvas\n",
             parent = parent)
}

JSCode =
  #
  # Gives the names of the JS files we need to make this code run.
  #  If given baseURL, we use that as the base URL. This allows us to
  # refer to the files at URLs where we _will_ find them when we render the
  # JavaScript.
  #
function(baseURL = NA)
{  
  files = list.files(system.file("JavaScript", package = "RJSCanvasDevice"), pattern = "\\.js$", full.names = TRUE)

  if(length(baseURL) && !is.na(baseURL)) {
    
    sprintf("%s/%s", gsub("/$", "", baseURL), basename(files))
  } else
    files
}


fixHTMLDoc =
  #
  # This ensures that we have the right names for the canvases
  # and they have the right dimensions corresponding to the plot.
function(doc, plots, multiCanvas = FALSE, removeUnusedCanvas = TRUE)
{
  canvas = getNodeSet(doc, "//canvas")
  canvasId = sapply(canvas, xmlGetAttr, "id")

  body = getNodeSet(doc, "//body")[[1]]

  id = sapply(plots, slot, "canvasId")
  isMissing = !(id %in% canvasId)      # & !duplicated(id)
  if(any(isMissing)) 
     mapply(makeCanvasNode, id[isMissing], lapply(plots[isMissing], slot, "dims"), MoreArgs = list(parent = body))

  
    # Next, fix up the dimensions of each of the canvases.
    # We can skip over the isMissing ones.
  if(any(!isMissing)) {
    i = !isMissing & !duplicated(id)
    mapply(function(node, dims) {
              xmlAttrs(node) = c(width = dims[1], height = dims[2])
           }, canvas[i], lapply(plots[i], slot, "dims"))
  }
  if(removeUnusedCanvas) {
     notUsed = !(canvasId %in% id)
     if(any(notUsed))
       removeNodes(canvas[notUsed]) # , TRUE)     
  }

  TRUE
}  

