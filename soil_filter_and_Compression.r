fitsoilwater <-
function (theta, x, xlab = NULL, ylab = NULL, ...) 
{
    if (!requireNamespace("rpanel", quietly = TRUE)) 
        stop("package rpanel is required")
    if (!inherits(c(theta, x), "numeric")) 
        stop("non-numeric arguments!")
    if (length(theta) != length(x)) 
        stop("incompatible dimensions!")
    dat <- data.frame(theta, x)
    if (is.null(xlab)) 
        xlab = "Matric potential"
    if (is.null(ylab)) 
        ylab = "Soil water content"
    f.graph <- function() {
        plot(theta ~ x, data = dat, las = 1, xlab = xlab, ylab = ylab, 
            main = "Soil Water Retention Curve", ...)
    }
    f.graph()
    theta_R <- theta_S <- alpha <- n <- NULL
    f.panel <- function(pan) {
        f.graph()
        with(pan, curve(soilwater(x, theta_R, theta_S, alpha, 
            n), add = TRUE, col = "red"))
        return(pan)
    }
    f.fit <- function(pan) {
        start <- with(pan, pan[c("theta_R", "theta_S", "alpha", 
            "n")])
        fit <- try(with(pan, nls(theta ~ soilwater(x, theta_R, 
            theta_S, alpha, n), data = dat, start = start)))
        if (inherits(fit, "try-error")) {
            rpanel::rp.messagebox("No convergence... try other initial values.", 
                title = "Warning!")
        }
        else {
            f.graph()
            est <- coef(fit)
            curve(soilwater(x, est[1], est[2], est[3], est[4]), 
                add = TRUE, col = "blue")
            print(summary(fit))
            print(Rsq(fit))
        }
        return(pan)
    }
    panel <- rpanel::rp.control("Interactive fit")
    rpanel::rp.slider(panel, variable = theta_R, from = 0, to = max(theta), 
        resolution = 0.01, initval = 0.8 * min(theta), 
        title = "theta_R", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = theta_R, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = theta_S, from = 0, to = max(theta), 
        resolution = 0.01, initval = 0.8 * max(theta), 
        title = "theta_S", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = theta_S, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = alpha, from = 0, to = 2, resolution = 0.01, 
        initval = 0.01, title = "alpha", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = alpha, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = n, from = 0, to = 15, resolution = 0.01, 
        initval = 2, title = "n", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = n, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.button(panel, title = "NLS estimates", action = f.fit,
        foreground = "white", background = "navy")
    rpanel::rp.button(panel, title = "__________________ Quit __________________", 
        action = function(pan) return(pan), quitbutton = TRUE, 
        foreground = "red")
}

fitsoilwater3 <-
function (theta, x, xlab = NULL, ylab = NULL, ...) 
{
    if (!requireNamespace("rpanel", quietly = TRUE)) 
        stop("package rpanel is required")
    if (!inherits(c(theta, x), "numeric")) 
        stop("non-numeric arguments!")
    if (length(theta) != length(x)) 
        stop("incompatible dimensions!")
    dat <- data.frame(theta, x)
    if (is.null(xlab)) 
        xlab = "Matric potential"
    if (is.null(ylab)) 
        ylab = "Soil water content"
    f.graph <- function() {
        plot(theta ~ x, data = dat, las = 1, xlab = xlab, ylab = ylab, 
            main = "Soil Water Retention Curve", ...)
    }
    f.graph()
    theta_R <- a1 <- p1 <- a2 <- p2 <- NULL
    f.panel <- function(pan) {
        f.graph()
        with(pan, curve(soilwater3(x, theta_R, a1, p1, a2, p2), 
            add = TRUE, col = "red"))
        return(pan)
    }
    f.fit <- function(pan) {
        start <- with(pan, pan[c("theta_R", "a1", "p1", "a2", 
            "p2")])
        fit <- try(with(pan, nls(theta ~ soilwater3(x, theta_R, 
            a1, p1, a2, p2), data = dat, start = start)))
        if (inherits(fit, "try-error")) {
            rpanel::rp.messagebox("No convergence... try other initial values.", 
                title = "Warning!")
        }
        else {
            f.graph()
            est <- coef(fit)
            curve(soilwater3(x, est[1], est[2], est[3], est[4], 
                est[5]), add = TRUE, col = "blue")
            print(summary(fit))
            print(Rsq(fit))
        }
        return(pan)
    }
    panel <- rpanel::rp.control("Interactive fit")
    ran.t <- 2 * range(theta)
    rpanel::rp.slider(panel, variable = theta_R, from = 0, to = max(theta), 
        resolution = 0.01, initval = 0.8 * min(theta), title = "theta_R", 
        action = f.panel)
    rpanel::rp.doublebutton(panel, variable = theta_R, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = a1, from = -0.5, to = 10, resolution = 0.01, 
        initval = 0.07, title = "a1", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = a1, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = p1, from = 0, to = 15000, resolution = 5, 
        initval = 3670, title = "p1", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = p1, step = 1, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = a2, from = 0, to = 10, resolution = 0.01, 
        initval = 0.32, title = "a2", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = a2, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = p2, from = 0, to = 1500, resolution = 5, 
        initval = 70, title = "p2", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = p2, step = 1, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.button(panel, title = "NLS estimates", action = f.fit, 
        foreground = "white", background = "navy")
    rpanel::rp.button(panel, title = "__________________ Quit __________________", 
        action = function(pan) return(pan), quitbutton = TRUE, 
        foreground = "red")
}




fitsoilwater5 <-
function (theta, x, theta_S, xlab = NULL, ylab = NULL, ...) 
{
    if (!requireNamespace("rpanel", quietly = TRUE)) 
        stop("package rpanel is required")
    if (!inherits(c(theta, x), c("numeric", "integer"))) 
        stop("non-numeric arguments!")
    if (length(theta) != length(x)) 
        stop("incompatible dimensions!")
    stopifnot(theta_S >= 0)
    dat <- data.frame(theta, x)
    if (is.null(ylab)) 
        ylab = "Soil water content"
    if (is.null(xlab)) 
        xlab = "Matric potential"
    f.graph <- function() {
        plot(theta ~ x, data = dat, las = 1, xlab = xlab, ylab = ylab, 
            main = "Soil Water Retention Curve", ...)
    }
    f.graph()
    theta_R <- alpha <- n <- b0 <- b1 <- b2 <- NULL
    f.panel <- function(pan) {
        f.graph()
        with(pan, curve(soilwater5(x, theta_R, theta_S = theta_S, alpha, 
            n, m = 1 - 1/n, b0, b1, b2), add = TRUE, col = "red"))
        return(pan)
    }
    f.fit <- function(pan) {
        start <- with(pan, pan[c("theta_R", "alpha", 
            "n", "b0", "b1", "b2")])
        fit <- try(with(pan, nls(theta ~ soilwater5(x, theta_R, 
            theta_S = theta_S, alpha, n, m = 1 - 1/n, b0, b1, b2), 
            data = dat, start = start)))
        if (inherits(fit, "try-error")) {
            rpanel::rp.messagebox("No convergence... try other initial values.", 
                title = "Warning!")
        }
        else {
            f.graph()
            est <- coef(fit)
            with(dat, lines(x, soilwater5(x, theta_R = est[1], 
               theta_S = theta_S, alpha = est[2], n = est[3], b0 = est[4],
               b1 = est[5], b2 = est[6]), col = "blue"))
            print(summary(fit))
            print(Rsq(fit))
        }
        return(pan)
    }
    panel <- rpanel::rp.control("Interactive fit")
    rpanel::rp.slider(panel, variable = theta_R, from = 0, to = max(theta)*1.5, 
        resolution = 0.01, initval = 0.2, title = "theta_R", 
        action = f.panel)
    rpanel::rp.doublebutton(panel, variable = theta_R, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = alpha, from = 0, to = 2, resolution = 0.01, 
        initval = 0.05, title = "alpha", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = alpha, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = n, from = 0, to = 30, resolution = 0.01, 
        initval = 10, title = "n", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = n, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = b0, from = -2, to = 2, resolution = 0.01, 
        initval = 0.1, title = "b0", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = b0, step = 0.01, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = b1, from = -0.5, to = 0.5, resolution = 1e-04, 
        initval = -0.017, title = "b1", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = b1, step = 1e-04, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.slider(panel, variable = b2, from = -1, to = 1, resolution = 1e-05, 
        initval = 1e-04, title = "b2", action = f.panel)
    rpanel::rp.doublebutton(panel, variable = b2, step = 1e-05, title = "", 
        action = f.panel, showvalue = TRUE, foreground = "blue")
    rpanel::rp.button(panel, title = "NLS estimates", action = f.fit, 
        foreground = "white", background = "navy")
    rpanel::rp.button(panel, title = "__________________ Quit __________________", 
        action = function(pan) return(pan), quitbutton = TRUE, 
        foreground = "red")
}





compressive_properties4 <- function(matric.suction, soil = c("PloughLayer", "PloughPan")) { 

    x <- log10(matric.suction)

    N1 <- function(x) 1.4612 + 0.5080*x - 0.0723*x^2 
    CI1 <- function(x) 0.0233 + 0.0728*x - 0.0110*x^2
    k1 <- function (x) 0.0221*exp(x^-0.4530)
   
    N2 <- function(x) 1.2726 + 0.4223*x - 0.0691*x^2 
    CI2 <- function(x) 0.0112 + 0.0578*x - 0.0105*x^2
    k2 <- function (x) 0.0252*exp(x^-0.5414)
    
    N <- c()
    CI <- c()
    k <- c()

    if (soil=="PloughLayer") {N <- N1(x=x)}
    if (soil=="PloughLayer") {CI <- CI1(x=x)}
    if (soil=="PloughLayer") {k <- k1(x=x)}

    if (soil=="PloughPan") {N <- N2(x=x)}
    if (soil=="PloughPan") {CI <- CI2(x=x)}
    if (soil=="PloughPan") {k <- k2(x=x)}


    out <- data.frame(N=N,lambda=CI, k=k)
    return(out)
}



compressive_properties5 <- function(water.content, soil = c("SandyLoam", "ClayLoam")) {
  w <- water.content
  N <- c()
  CI <- c()
  k <- c()
  if (soil=="SandyLoam") { N <- 2.430 - 0.0055*(w-11.2)^2}
  if (soil=="SandyLoam") { CI <- (N-1.572)/17}
  if (soil=="SandyLoam") { k <- CI*(0.119-(0.082*w/17))}

  if (soil=="ClayLoam") { N <- 2.813 - 0.0128*(w-17.4)^2}
  if (soil=="ClayLoam") { CI <- (N-1.557)/26}
  if (soil=="ClayLoam") { k <- CI*(0.119-(0.082*w/26))}

  out <- data.frame(N=N,lambda=CI, kappa=k)
  return(out)
}