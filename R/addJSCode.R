# See addECMAScript
setGeneric("addJSCode",
            function(to, code, src = character(), attrs = c(type = "application/x-javascript"), ...)
              standardGeneric("addJSCode"))


setMethod("addJSCode", c("HTMLInternalDocument"),
           function(to, code, src = character(), attrs = c(type = "application/x-javascript"), ...) {
              head = getNodeSet(to, "//head")
              if(length(head) == 0)
                 stop("No <head> node")
              addJSCode(head[[1]], code, src, attrs, ...)
           })

setOldClass("AsIs")

setMethod("addJSCode", c("XMLInternalNode", "AsIs"),
           function(to, code, src = character(), attrs = c(type = "application/x-javascript"), ...) {
             attrs["src"] = code
             newXMLNode("script", attrs = attrs, parent = to)             
           })

setMethod("addJSCode", c("XMLInternalNode", "character"),
           function(to, code, src = character(), attrs = c(type = "application/x-javascript"), ...) {
    # Don't use CDATA nodes. HTML browser's don't like them.
    # So put in a comment.
            txt = c("", code, "") # puts newlines into the comment.
            if(length(src))
              attrs["originalURL"] = src

            newXMLNode("script", attrs = attrs,
                        newXMLCommentNode(paste(txt, collapse = "\n")),
                        parent = to)          
           })
