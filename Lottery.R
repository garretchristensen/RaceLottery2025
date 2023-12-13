
library(dplyr)
library(tibble)
library(readxl)
library(markdown)
library(shiny)
library(shinyBS)

temp<-read.csv("./2024 HiLo lottery data_FINALnoemail.csv", stringsAsFactors = FALSE) #LOAD THE DATA
df<-as_tibble(temp)

df$fullname<-paste(df$First_Name, df$Last_Name, sep=" ", collapse = NULL)
head(df)
#DETERMINE TICKETS FROM THE DATA
df$Applications<-df$Previous_Applications

#k is defined according to the following rule:
# k=0 if finishes==0
#k=0.5 if finishes==1
#k=1 if finishes==2
#k=1.5 if finishes==3
#k=1 if finishes>=4
df$k <- ifelse(df$Previous_Finishes==0 , 0,
               ifelse(df$Previous_Finishes==1,  0.5,
                      ifelse(df$Previous_Finishes==2, 1, 
                             ifelse(df$Previous_Finishes==3, 1.5,
                                    ifelse(df$Previous_Finishes>=4, 0.5, 0)))))


#Shifts max out at 30 (10 each race)
df$v<-pmin(df$Volunteer_Shifts, 30)
df$t<-pmin(df$Extra_Trailwork, 10)

#Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
df$tickets <-2^(df$k+df$Applications+1) + 2*log(df$v+df$t+1)

#NUMBER OF MEN AND WOMEN APPLICANTS AND PICKS
n_women_app=nrow(women<-df[which(df$Gender=="F"),])
n_women_pick <- 68
n_men_pick <- 75
n_women_wait_pick<-75
n_men_wait_pick<-75

shinyApp(
  ui<-fluidPage(
    htmltools::includeMarkdown("./markdown/headline.md"),
    bsCollapse(id = "collapse", multiple=TRUE,
               bsCollapsePanel("Lottery Design",
                           htmltools::includeMarkdown("./markdown/background.md"),
                          style = "info"
               ),
               bsCollapsePanel("What are my odds?",
                          htmltools::includeMarkdown("./markdown/justtellmetheodds.md"),
                          fluidRow(
                          column(6, sliderInput("apps", label = h5("Previous applications"), min=0, max=5, value = 0)),
                          column(6, sliderInput("finishes", label = h5("Previous Finishes"), min=0, max=5, value = 0)),
                          column(6, sliderInput("volunteer", label = h5("Volunteer Shifts"), min=0, max=30, value = 0)),
                          column(6, sliderInput("trailwork", label = h5("Extra Trailwork"), min=0, max=10, value = 0)),
                         "Your tickets in the lottery:", textOutput("tickets")
                         ),
                         style = "success"
               ),
               
               bsCollapsePanel("Set the Seed", 
                               fluidRow(#htmltools::includeMarkdown("./markdown/implementingthelottery.md"),  
                                 column(6, numericInput("num", label = h1("Enter the seed"), value = NA)),
                                 column(6, numericInput("num2", label = h1("Confirm the seed"), value = NA)),
                               ),
                               style="info" 
               ),
               
               bsCollapsePanel("Results",
                               fluidRow(
                               column(6, "These are the women selected in the lottery:", 
                               verbatimTextOutput("valueW"),
                               "These are the waitlisted women:",
                               verbatimTextOutput("valueWLW")),
                               column(6, "These are the men selected in the lottery:", 
                               verbatimTextOutput("valueM"),
                               "These are the waitlisted men:",
                               verbatimTextOutput("valueWLM")),
                               ),
                               style="success")
               
               
              
               
    ) #bsCollapse
  ), #fluidPage
  
  server<- function(input, output) {
  
    output$tickets <- renderText({
    #Do THE VARIABLE TRANSFORMATIONS FOR PEOPLE's ENTERED VALUES
    k_sim <- ifelse(input$finishes==0 , 0,
                   ifelse(input$finishes==1,  0.5,
                          ifelse(input$finishes==2, 1,
                                 ifelse(input$finishes==3, 1.5,
                                        ifelse(input$finishes>=4, 0.5, 0)))))


    #Shifts max out at 30 (10 each race)
    v_sim<-pmin(input$volunteer, 30)
    t_sim<-pmin(input$trailwork, 10)

    #Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
    2^(k_sim+input$apps+1) + 2*log(v_sim+t_sim+1)

    }) #END RENDER TEXT
    
    
    
    #WOMEN WINNERS, REACTIVE, THEN RENDER
    w_winners <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
          set.seed(input$num) #SET THE SEED WITH DICE
          #SPLIT THE DATA INTO MENS AND WOMENS
          women<-df[which(df$Gender=="F"),]
          #DRAW THE WOMEN
          women_winners<-sample_n(women, n_women_pick, replace = FALSE, weight=women$tickets)
          w_output<-subset(women_winners, select=c("fullname", "City", "State"))
          #Add ID column data$ID <- seq.int(nrow(data))
          w_output$Num<-seq.int(nrow(w_output))
          #CHANGE THE VARIABLE NAME
          names(w_output)[1]<-"Selected_Women"
          #PRINT THE DATA TO SEND TO REACTIVE
          as.data.frame(w_output)
      } # CLOSE IF
    }) #CLOSE REACTIVE
    output$valueW <- renderPrint({
      w_winners()
    }) #END RENDER PRINT
    
    #MEN WINNERS, REACTIVE, THEN RENDER
    m_winners <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
        set.seed(input$num) #SET THE SEED WITH DICE!
        #SPLIT THE DATA INTO MENS AND WOMENS
        men<-df[which(df$Gender=="M"),]
        #DRAW THE MEN
        men_winners<-sample_n(men, n_men_pick, replace = FALSE, weight=men$tickets)
        m_output<-subset(men_winners, select=c("fullname", "City", "State"))
        #Add ID column data$ID <- seq.int(nrow(data))
        m_output$Num<-seq.int(nrow(m_output))
        #CHANGE THE VARIABLE NAME
        names(m_output)[1]<-"Selected_Men"
        #PRINT THE DATA TO SEND TO REACTIVE
        as.data.frame(m_output)
      } # CLOSE IF
    }) #CLOSE REACTIVE
    output$valueM <- renderPrint({
      m_winners()
    }) #END RENDER PRINT
    
  #WAITLIST
    waitlistW <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
        set.seed(input$num) #SET THE SEED WITH DICE!
        #REDO THE LOTTERIES (ARE THEY THE SAME?)
        women_winners<-sample_n(women, n_women_pick, replace = FALSE, weight=women$tickets)
        women_waitlist_pool<-anti_join(women, women_winners)
        #PICK THE WAITLISTERS
        women_waiters <- sample_n(women_waitlist_pool, n_women_wait_pick, replace = FALSE, weight=women_waitlist_pool$tickets)
        w_output_wait<-subset(women_waiters, select=c("fullname", "City", "State"))
        w_output_wait$Num<-seq.int(nrow(w_output_wait))
        names(w_output_wait)[1]<-"Waitlisted_Women"
        #Send the winners' names to be output
        names(w_output_wait)[1]<-"Waitlisted_Name"
        as.data.frame(w_output_wait)
      } # CLOSE IF
    }) #CLOSE REACTIVE
    output$valueWLW <- renderPrint({
    waitlistW()
    }) #END RENDER PRINT
      
    waitlistM <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
        set.seed(input$num) #SET THE SEED WITH DICE!
        #REDO THE LOTTERIES (ARE THEY THE SAME?)
        men_winners<-sample_n(men, n_men_pick, replace = FALSE, weight=men$tickets)
        men_waitlist_pool<-anti_join(men, men_winners)
        #PICK THE WAITLISTERS
        men_waiters <- sample_n(men_waitlist_pool, n_men_wait_pick, replace = FALSE, weight=men_waitlist_pool$tickets)
        m_output_wait<-subset(men_waiters, select=c("fullname", "City", "State"))
        m_output_wait$Num<-seq.int(nrow(m_output_wait))
        names(m_output_wait)[1]<-"Waitlisted_Men"
        #Send the winners' names to be output
        names(m_output_wait)[1]<-"Waitlisted_Name"
        as.data.frame(m_output_wait)
      } # CLOSE IF
    }) #CLOSE REACTIVE
    output$valueWLM <- renderPrint({
      waitlistM()
    }) #END RENDER PRINT
    
    
  }, #CLOSE SERVER
  options = list(height = 900) 
)




