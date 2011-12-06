library(RJSCanvasDevice)

dev = jsCanvas("plot1_10.js")
  plot(1:10, main = "Some text", ylab = "Y label")
  text(1, 5, "Hi there")
  text(1:10, 1:10, 1:10)
  abline(v = seq(.5, by = 1, length = 10), lty = 2, col = "green")
  abline(h = seq(.5, by = 1, length = 10), lwd = 3, col = "blue")
dev.off()

jsCanvas('usaCounties.js')
 library(maps)
 map('county')
dev.off()

jsCanvas('parLayout.js')
 par(mfrow = c(2, 2))
 x = rnorm(100)
 plot(1:10)
 hist(x)
 plot(x, x)
 curve(sin(x), 0, 2*pi)
dev.off()



jsCanvas('pairs.js')
 pairs(mtcars[, 1:5])
dev.off()


library(lattice)
jsCanvas('xyplot.js')
  xyplot(mpg ~ wt | cyl, mtcars)
dev.off()

x = jsCanvas(expression({plot(1:10); symbols()))


# A connection.
mycon = file("bob1", "w")
 jsCanvas(mycon)
 plot(1:10)
 dev.off()
close(mycon)


 # Result is the JavaScript code.
x = jsCanvas(expression(symbols(rev(1:10), 1:10, circles = rep(1, 10), inches = FALSE, col = "red")))


  #
htmlCanvas("bob.html")
plot(1:10)
dev.off()

 
  #
htmlCanvas("bob.html")
plot(1:10, pch = 21)
dev.off()


htmlCanvas("circle.html")
 symbols(rev(1:10), 1:10, circles = rep(1, 10), inches = FALSE, col = "red")
dev.off()  


htmlCanvas("rect.html")
   plot(1:10)
   rect(2, 2, 4, 4, border = "blue")
   rect(5, 5, 7, 8, border = "red", col = "green")  
dev.off()  


if(require(hexbin)) {
 htmlCanvas("hexbin.html")
     x <- rnorm(10000)
     y <- rnorm(10000)
     plot(hexbin(x, y + x*(x+1)/4)) #          main = "(X, X(X+1)/4 + Y)  where X,Y ~ rnorm(10000)")
 dev.off()
}


library(lattice)
 htmlCanvas("lattice.html")
   xyplot(mpg ~ wt | cyl, mtcars)
 dev.off()
