roctopus <- function(x, FUN, formatter = hmr::.default.formatter, ...) {
  r <- hmr(x, formatter = identity, map = function(m) {
    wd <- getwd() ## why?
    c <- .ro.new()
    url <-attr(c, "url")
    tryCatch(RSclient::RS.eval(c, bquote(iotools:::.ro.chunk(.(wd), .(FUN), .(m), .(formatter))), wait=FALSE, lazy=FALSE),
	     error=function(...) NULL)
    tryCatch(RSclient::RS.close(c), error=function(...) NULL)
    url
  }, wait = TRUE, reducers=0, ...)
  f <- open(r)
  on.exit(close(f))
  x <- readLines(f)
  ## FIXME: rm r
  lapply(x, worker)
}
