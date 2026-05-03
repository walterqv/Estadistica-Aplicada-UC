library(shiny)

# =========================
# UI
# =========================
ui <- fluidPage(
  titlePanel("Cadena de Markov - Sistema Climático"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Distribución inicial"),
      sliderInput("piS", "Soleado", min = 0, max = 1, value = 0.33),
      sliderInput("piL", "Lluvioso", min = 0, max = 1, value = 0.33),
      sliderInput("piN", "Nublado", min = 0, max = 1, value = 0.34),
      
      h4("Pasos de simulación"),
      sliderInput("steps", "Número de pasos", min = 5, max = 100, value = 30),
      
      actionButton("run", "Simular")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Evolución", plotOutput("evolutionPlot")),
        tabPanel("Distribución estacionaria", verbatimTextOutput("stationary")),
        tabPanel("Trayectoria", plotOutput("trajectoryPlot"))
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output) {
  
  states <- c("Soleado", "Lluvioso", "Nublado")
  
  # Matriz de transición fija
  P <- matrix(c(
    0.6, 0.2, 0.2,
    0.3, 0.5, 0.2,
    0.4, 0.3, 0.3
  ), nrow = 3, byrow = TRUE)
  
  observeEvent(input$run, {
    
    # Normalizar distribución inicial
    pi0 <- c(input$piS, input$piL, input$piN)
    pi0 <- pi0 / sum(pi0)
    
    # Evolución
    n_steps <- input$steps
    dist <- matrix(0, nrow = n_steps, ncol = 3)
    dist[1, ] <- pi0
    
    for (t in 2:n_steps) {
      dist[t, ] <- dist[t-1, ] %*% P
    }
    
    colnames(dist) <- states
    
    # =====================
    # Gráfico evolución
    # =====================
    output$evolutionPlot <- renderPlot({
      matplot(dist, type = "l", lty = 1, lwd = 2,
              xlab = "Tiempo",
              ylab = "Probabilidad",
              main = "Evolución de la distribución")
      legend("right", legend = states, lty = 1, lwd = 2)
    })
    
    # =====================
    # Distribución estacionaria
    # =====================
    output$stationary <- renderPrint({
      eig <- eigen(t(P))
      pi_star <- Re(eig$vectors[,1])
      pi_star <- pi_star / sum(pi_star)
      
      names(pi_star) <- states
      print(round(pi_star, 4))
    })
    
    # =====================
    # Simulación trayectoria
    # =====================
    output$trajectoryPlot <- renderPlot({
      set.seed(123)
      
      n_sim <- input$steps
      current <- sample(1:3, 1, prob = pi0)
      traj <- numeric(n_sim)
      traj[1] <- current
      
      for (t in 2:n_sim) {
        current <- sample(1:3, 1, prob = P[current, ])
        traj[t] <- current
      }
      
      plot(traj, type = "s", yaxt = "n",
           xlab = "Tiempo",
           ylab = "Estado",
           main = "Trayectoria simulada")
      
      axis(2, at = 1:3, labels = states)
    })
    
  })
}

# =========================
# RUN APP
# =========================
shinyApp(ui = ui, server = server)