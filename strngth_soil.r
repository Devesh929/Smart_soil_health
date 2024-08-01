server_particle <- function(input, output) {
  
  # my function 
  part.sedimen <- function(d, h, g=9.81, v, Pd, Wd) {
    
    v <- v*10^-3
    d <- d*10^-6
    h <- h/100
    out0 <- (h*18*v)/( (d^2)*(Pd-Wd)*g)
    out1 <- out0/60
    out2 <- out1/60
    out3 <- out2/24
    rest <- data.frame(out0,out1,out2,out3)
    return(rest)
  }

  
  
 output$plot1 <- renderPlot({
   
    y <- part.sedimen(d=input$d, h=input$h, g=9.81, 
                      v=input$v, Pd=input$Pd, Wd=input$Wd)$out2
    x <- input$d
    lab <- c(1000,100,50,10,2,1)
    
    m <- function (x) 9.592e-01 + 2.041e-02*x
    tam <- m(input$d)

    plot(x=x,y=y,log="x",xaxt='n', pch=19,cex=tam, 
        col="brown", xlim=c(1,1000), ylim=c(0,30),xlab="",ylab="")
    axis(1,at=lab, labels=lab)
    mtext(expression("Soil particle diameter (d)"~(mu*m)),1,line=2.5)
    mtext("Sedimentation time (t) (hour)",2,line=2.5)
    text("soil particle",x=x,y=y+2, cex=0.9)
    abline(v=c(2,50), col=2)
    axis(3,at=c(1.2,8,300), labels=c("clay","silt","sand"))
  })
 
 

 output$values <- renderTable({
   
   y <- part.sedimen(d=input$d, h=input$h, g=9.81, 
                     v=input$v, Pd=input$Pd, Wd=input$Wd)
   x <- input$d
   
   data <- data.frame(Diameter=x,Seconds=y$out0,Minutes=y$out1,Hours=y$out2,Days=y$out3)
   
 })
 
 
}


# ==============================================
ui_particle <- fluidPage(
  # App title ----
  titlePanel("Sedimentation time of soil particle (Stokes' law)"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("d", HTML(paste0(" Soil particle diameter (&mu;m)")),
                  min = 2, max = 100,
                  step = 1, value=100, dragRange=TRUE),
  
      sliderInput("v", HTML(paste0("Fluid viscosity 10",tags$sup("-3"), "(kg m",tags$sup("-1")," s",tags$sup("-1"),")")),
                  min = 0.5, max = 1.5,
                  value = 1, step = 0.1),
      

      sliderInput("h", 'Fall height of soil particle (cm)',
                  min = 5, max = 25,
                  value = 20, step = 5),
      
      sliderInput("Pd", HTML(paste0("Particle density (kg m",tags$sup("-3"),")")),
                  min = 2400, max = 3000,
                  value = 2650, step = 50),
      
      sliderInput("Wd", HTML(paste0("Fluid density (kg m",tags$sup("-3"),")")),
                  min = 950, max = 1050,
                  value = 1000, step = 10)
      
    ),   
    # Main panel for displaying outputs ----
    mainPanel(      
      plotOutput('plot1'),
      tableOutput("values"))
  ),
  verticalLayout(
    column(12,wellPanel(
      tags$h4("by Renato P. de Lima")
    )))
)

particle.sedimentation_App <- function() {
  shinyApp(ui_particle, server_particle)
  }
  
  
  
  
  
  
  soilStrength <-  
function(clay.content, matric.suction=NULL, water.content=NULL)
{
   if (!is.null(matric.suction) || !is.null(water.content)) {
      if (is.null(clay.content)) 
            warning("To estimate soil strength, please inform water.content or matric.suction")
      if (is.numeric(matric.suction) & is.numeric(water.content))
            warning("To estimate soil strength, please inform only one of them: water.content or matric.suction")
   }

  # inverted water retention curve 
  vanG.matric <- function (theta, thetaR, thetaS, alpha, n) {
      S <- (theta-thetaR)/(thetaS-thetaR)
      f <- n/(1-n)
      h <- (1/alpha)*((S^f)-1)^(1/n)
      out <- data.frame(theta, h)
      return(out)
  }

  # parameters of the soil retention curve 
  thetaS <- c(0.47,0.54,0.57,0.64,0.66)
  thetaR <- c(0.049356,0.08689,0.10696,0.125941,0.139358)
  alpha <- c(0.79,0.72,1.66,2.04,2.27)
  n <- c(1.72,1.56,1.52,1.47,1.38)
  m <- c(0.42,0.36,0.34,0.33,0.28)

  # precompression stress estimation
     pre.cons.water <- function(clay.content, water.content) {
         mh <- c()
         for (j in 1:length(clay.content)) {
            if (clay.content[j] <= 20) {
               mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[1], thetaS=thetaS[1], alpha=alpha[1], n=n[1])$h
            }
            else if (clay.content[j] > 20 & clay.content[j] <= 31) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[2], thetaS=thetaS[2], alpha=alpha[2], n=n[2])$h
            }
            else if (clay.content[j] > 31 & clay.content[j] <= 37) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[3], thetaS=thetaS[3], alpha=alpha[3], n=n[3])$h
            }
            else if (clay.content[j] > 37 & clay.content[j] <= 52) {
                  mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[4], thetaS=thetaS[4], alpha=alpha[4], n=n[4])$h
            }
            else {
               mh[j] <- vanG.matric(theta=water.content[j], thetaR=thetaR[5], thetaS=thetaS[5], alpha=alpha[5], n=n[5])$h
            }
         }
         return(round(mh, 2))
      }
       
     if (length(matric.suction) > 0) {
         matric.suction <- matric.suction
     }
     else {
            matric.suction <- pre.cons.water(clay.content = clay.content,water.content = water.content)
     }

     pcs <- c()
     for (j in 1:length(clay.content)) {
        if (clay.content[j] < 20) {
            pcs[j] <- round(129 * matric.suction[j]^(0.15),0)
        }
        else if (clay.content[j] >= 20 & clay.content[j] <= 31) {
            pcs[j] <- round(123.3 * matric.suction[j]^(0.13),0)
        }
        else if (clay.content[j] > 31 & clay.content[j] <= 37) {
            pcs[j] <- round(85 * matric.suction[j]^(0.17),0)
        }
        else if (clay.content[j] > 37 & clay.content[j] <= 52) {
            pcs[j] <- round(70.1 * matric.suction[j]^(0.16),0)
        }
        else if (clay.content[j] > 52) {
            pcs[j] <- round(62.7 * matric.suction[j]^(0.15),0)
        }
     }

     pcs05 <- pcs*0.5
     pcs11 <- pcs*1.1
     soil.strength <- data.frame(pcs,pcs05,pcs11)
        colnames(soil.strength) <- c("Pc","LL.Pc","UL.Pc")
    return(soil.strength)
}



soilStrength3 <- function(bulk.density, water.content, 
                          texture=c("VeryFine","Fine","MediumFine","Medium","Coarse")) { 

 w <- water.content
 BD <- bulk.density
 PC <- c()
 if (texture == "VeryFine") {PC <- 7.71 + 112.21*BD - 2.81*w } # VeryFine
 if (texture == "Fine") {PC <- 4.19 + 202.54*BD - 10.92*w } # Fine
 if (texture == "MediumFine") {PC <- -223 + 347*BD - 7.93*w } # MediumFine
 if (texture == "Medium") {PC <- -136 + 155*BD } # Medium
 if (texture == "Coarse") {PC <- -220 + 191*BD + 2.77*w } # Coarse

 for (j in 1: length(PC)) { 
 if (PC[j] < 0) {PC[j] <- 0}
 }
 
 return(PC)
}
