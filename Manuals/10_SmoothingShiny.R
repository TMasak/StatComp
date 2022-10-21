#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MASS)
library(locpol)
library(ggplot2)
library(ggpubr)

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Local Polynomial Fitting of Motorcycle Accident Data"),
    
    # Sidebars
    sidebarLayout(
        sidebarPanel(
            sliderInput("x0",
                        "Evaluation point:",
                        min = 10,
                        max = 50,
                        value = 20),
            sliderInput("h",
                        "Bandwidth:",
                        min = 3,
                        max = 10,
                        value = 5),
            sliderInput("p",
                        "Order Polynomial:",
                        min = 0,
                        max = 3,
                        value = 2),
            checkboxInput("toggle_kernel",
                          "Weighting Kernel",
                          value=T),
            checkboxInput("toggle_fullfit",
                          "Full Fitted Local Polynomial",
                          value=F),
            checkboxInput("toggle_x0_fit",
                          "Polynomial at the Evaluation Point",
                          value=F),
            checkboxInput("toggle_point",
                          "Fitted Value",
                          value=F),
        ),
        
        # Resulting Plot
        mainPanel(
            plotOutput("distPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    
    output$distPlot <- renderPlot({
        data(mcycle)
        x <- mcycle$times
        y <- mcycle$accel
        # options that will surely vary
        p <- input$p   # order for the local polynomial 
        x0 <- input$x0
        # options that might vary
        kern_name <- "epanechnik" # which kernel should be used
        grid_size <- 200     # on how many points should we evaluate the local fit
        binwidth <- diff(seq(min(x),max(x),length=200))[1]
        # weights
        epanechnik <- function(x) 3/4*(1-x^2)*I(-1 <= x)*I(x <= 1)
        epan_eval <- epanechnik(seq(-1,1,length=100))
        kernel_data <- data.frame(times = input$h*seq(-1,1,length=100) + x0,
                                  y = epan_eval/sum(epan_eval)*100/input$h/2)
        loc_data <- mcycle[abs(mcycle$times-x0) <= input$h,]
        w <- epanechnik((x0 - loc_data$times)/input$h)
        loc_data$w <- w
        # full fit and local fit at y0
        if(input$toggle_fullfit){
            if(p > 0){
                local_fit <- locpol(accel~times, deg=p, data=mcycle, xevalLen=grid_size,
                                    kernel=EpaK, bw=input$h)
            }else{
                # manual calculation of the local constant fit
                local_fit <- rep(0,grid_size)
                fit_grid <- seq(min(mcycle$times),max(mcycle$times),length = grid_size)
                for(i in 1:grid_size){
                    x0i <- fit_grid[i]
                    w <- epanechnik((mcycle$times-x0i)/input$h)
                    local_fit[i] <- weighted.mean(mcycle$accel,w)
                }
                local_fit <- list(lpFit = data.frame(times = fit_grid, accel = local_fit))
            }
        }
        if(p > 0){
            y0 <- locpol(accel~times, deg=p, data=mcycle, xeval=x0, kernel=EpaK, bw=input$h)
            y0 <- y0$lpFit$accel
        }else{
            # manual calculation of the local constant fit 
            w <- epanechnik((mcycle$times-x0)/input$h)
            y0 <- weighted.mean(mcycle$accel,w)
        }
        
        p1 <- ggplot() +
            geom_vline(xintercept = x0, linetype=2)
        if(input$toggle_fullfit){
            p1 <- p1+ geom_line(data = local_fit$lpFit, mapping = aes(x = times, y = accel),
                                col = 4, size = 1.5)
        }
        p1 <- p1 + scale_y_continuous(limits = c(min(mcycle$accel), max(mcycle$accel))) +
            scale_x_continuous(limits = c(min(mcycle$times), max(mcycle$times))) +
            geom_point(data = mcycle, mapping = aes(x = times, y = accel), alpha=0.2) +
            geom_point(data = loc_data, mapping = aes(x = times, y = accel, size = w*6))
        if(input$toggle_x0_fit){
            if(p > 0){
                p1 <- p1 + stat_smooth(data = loc_data, mapping = aes(x = times, y = accel, weight = w),
                                       method = "lm", formula = y ~ poly(x, p), se=F, fullrange = T)
            }else{
                p1 <- p1 + stat_smooth(data = loc_data, mapping = aes(x = times, y = accel, weight = w),
                                       method = "lm", formula = y ~ 1, se=F, fullrange = T)
            }
        }
        if(input$toggle_point){
            p1 <- p1 + geom_point(data = data.frame(x = x0, y = y0), mapping = aes(x = x, y = y),
                                  col = 2,shape = 4, size = 4, stroke = 2)
        }
        
        p2 <- ggplot() + scale_x_continuous(limits = c(min(mcycle$times), max(mcycle$times)))+
            scale_y_continuous(labels = function(x) sprintf("%.2f", x)) +
            geom_line(data = kernel_data, mapping = aes(x = times, y = y)) + 
            geom_vline(xintercept = x0, linetype=2) 
        
        if(input$toggle_kernel){
            ggarrange(p1, p2, ncol=1, nrow=2, heights = c(4,1))
        }else{
            p1
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
