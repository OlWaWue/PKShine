
library(shiny)
library(ggplot2)
library(mrgsolve)

model <- mread_cache("pk1", modlib())


ui <- fluidPage(
   
   # Application title
   titlePanel("PK Shine - Eine PK-Simulation für Würzburger Studierende"),
   

   sidebarLayout(
      sidebarPanel(
        inputPanel("Substanzeigenschaften",
          textInput("DRUG",
                       "Name der Substanz:",
                       value = "Examplocin"),
           numericInput("VD",
                       "Verteilungsvolumen [L]:",
                       min = 1,
                       max = 100000,
                       value = 30),
           numericInput("CL",
                        "Clearance [L/h]:",
                        min = 1,
                        max = 100000,
                        value = 5),
          conditionalPanel(condition = "input.ORAL == '1' ",
             numericInput("KA",
                          "Absorptionsrate [1/h]:",
                          min = 1,
                          max = 100000,
                          value = 0.5),
            numericInput("F_ORAL",
                         "systemisch verfügbarer Anteil",
                         min=0,
                         max=1,
                         value=0.95)
            )
        ),
        inputPanel("Dosierungsstrategie",
          checkboxInput("ORAL",
                        "orale Verabreichung?"),
          conditionalPanel(condition = "input.ORAL == '0'",
            checkboxInput("BOLUS",
                          "Bolusinjektion?")
          ),
          checkboxInput("LOAD","Loading dose?"),
          conditionalPanel(condition = "input.LOAD == '1'",
            numericInput("LAMT", "Loading dose [mg]:",
               value = 1000)
 
          ),
           numericInput("AMT",
                        "Dosis [mg]:",
                        min = 1,
                        max = 100000,
                        value = 500),
           
           numericInput("II",
                        "Dosierungsintervall [h]:",
                        min = 1,
                        max = 100000,
                        value = 12),
          numericInput("ADDL",
                       "Anzahl an weiteren Dosen:",
                       min = 1,
                       max = 30,
                       value = 0),
          conditionalPanel(condition = "input.ORAL == '0' & input.BOLUS == '0'",
            numericInput("RATE",
                         "Infusionsrate [mg/h]:",
                         min = 1,
                         max = 30,
                         value = 100)
          )
        ),
        checkboxInput("THER","Therapeutischer Bereich?"),
        conditionalPanel(condition = "input.THER == '1'",
          inputPanel("Therapeutischer Bereich",
                     numericInput("MAX", "Obere Grenze [mg/L]:",
                                  value = 10),
                     numericInput("MIN", "Untere Grenze [mg/L]:",
                                  value = 5)
          )
        ),
        checkboxInput("LOG", "Y-Achse logarithmisch skalieren?")
      ),

      
      mainPanel(
         plotOutput("distPlot", height = "600px")
      )
   )
)


server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     
     vd <- input$VD
     cl <- input$CL
     amt <- input$AMT
     ka <- input$KA
     drug <- input$DRUG
     ii <- input$II
     addl <- input$ADDL
     rate <- input$RATE 
     
     time <- addl * ii + ii
     
     cmt = 2
     
     subtitle <- paste("i.v. mit einer Infusionsrate von", rate, "mg/h", addl+1,"mal, alle", ii, "Stunden")
     if(addl==0){
       subtitle <- paste("i.v. mit einer Infusionsrate von", rate, "mg/h")
       time <- 24
     }
     
     if(input$ORAL){
      cmt <- 1
      rate <- 0
      amt <- amt * input$F_ORAL
      subtitle <- paste("oral", addl+1,"mal, alle", ii, "Stunden")
      if(addl==0){
        subtitle <- "oral"
        time <- 24
      }
     }
     
     if(input$BOLUS){
       rate <- 0
       subtitle <- paste("i.v. als Bolusinjektion", addl+1,"mal, alle", ii, "Stunden")
       if(addl==0){
         subtitle <- "i.v. als Bolusinjektion"
         time <- 24
       }

     }
     
     evnt <- ev(amt = amt, cmt=cmt, rate=rate, ii=ii, addl=addl)
     
     if(input$LOAD){
       ld <- ev(time=0, amt = input$LAMT, cmt=cmt, rate=0)
       evnt <- ev(time=ii, amt = amt, cmt=cmt, rate=rate, ii=ii, addl=addl)
       evnt <- ld+evnt
       time <- time + ii
       subtitle <- paste(subtitle, "nach einer Loading Dose von", input$LAMT, "mg")
       if(input$BOLUS){
         subtitle <- paste(subtitle, "i.v. als Bolus")
       } else if (input$ORAL){
         subtitle <- paste(subtitle, "oral")
       } else {
         subtitle <- paste(subtitle, "i.v. als Bolus")
       }
     }
     
     out <- model %>% ev(evnt) %>%param (KA=ka, V=vd, CL=cl) %>% mrgsim(end = time, delta = 0.1)
     
     
     data <- as.data.frame(out)
     
     
     pl<- ggplot(data, aes(x=time, y=CP)) + geom_line(size=2) + theme_bw() + 
           xlab("Zeit [h]") + ylab("Plasmakonzentration [mg/L]") + ggtitle( paste(input$AMT, "mg", drug), subtitle = subtitle) + 
           theme(text = element_text(size=18)) + 
           scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
     
     if(input$LOG){
       pl <- pl + scale_y_log10(expand = c(0, 0))
     }
     
     if(input$THER){
       pl <- pl + geom_ribbon(aes(ymin=input$MIN, ymax=input$MAX), fill="blue", alpha=0.15) +
         geom_hline(aes(yintercept=input$MAX), linetype="dashed", size = 1.2, colour="red") +
         geom_hline(aes(yintercept=input$MIN), linetype="dashed", size = 1.2, colour="green") 
     }
     
     print(pl)
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

