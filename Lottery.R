
library(dplyr)
library(tibble)
library(readxl)
library(markdown)
library(shiny)
library(shinyBS)
library(DT)

temp<-read.csv("./2024 High Lonesome FINAL lottery data_noemail.csv", stringsAsFactors = FALSE) #LOAD THE DATA
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
n_women_app=nrow(women<-df[which(df$Gender=="Female"),])
women<-df[which(df$Gender=="Female"),]
men<-df[which(df$Gender=="Male"),]
n_women_pick <- 85
n_men_pick <- 77
n_women_wait_pick<-75
n_men_wait_pick<-75

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

# WOMEN ODDS
#the number of women with a given number of tickets
applicants <- pull((women %>% count(tickets))[,2], n)

#those ticket numbers
tickets_per_applicant <- sort(women$tickets[!duplicated(women$tickets)])

#the total tickets from that 'category'
original_tickets <- applicants * tickets_per_applicant
ticket_counts <- original_tickets

for (i in 1:n_women_pick) {
  #odds of picking that category
  prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
  #expected reduction in tickets by picking a person from that category
  exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
  #reduce the tickets remaining
  ticket_counts <- ticket_counts - exp_ticket_reduction
}
#tickets pulled from a category
tickets_taken <- original_tickets - ticket_counts
#odds from that category
odds_of_selection <- tickets_taken / original_tickets
#people from that category
num_people_taken <- odds_of_selection * applicants
w_odds <- cbind(tickets_per_applicant, odds_of_selection, applicants, num_people_taken)
format(w_odds, nsmall=2)

# MEN ODDS
#the number of men with a given number of tickets
applicants <- pull((men %>% count(tickets))[,2], n)

#those ticket numbers
tickets_per_applicant <- sort(men$tickets[!duplicated(men$tickets)])

#the total tickets from that 'category'
original_tickets <- applicants * tickets_per_applicant
ticket_counts <- original_tickets

for (i in 1:n_men_pick) {
  #odds of picking that category
  prob_of_selecting_category <- ticket_counts / sum(ticket_counts)
  #expected reduction in tickets by picking a person from that category
  exp_ticket_reduction <- prob_of_selecting_category * tickets_per_applicant
  #reduce the tickets remaining
  ticket_counts <- ticket_counts - exp_ticket_reduction
}
#tickets pulled from a category
tickets_taken <- original_tickets - ticket_counts
#odds from that category
odds_of_selection <- tickets_taken / original_tickets
#people from that category
num_people_taken <- odds_of_selection * applicants
m_odds <- cbind(tickets_per_applicant, odds_of_selection, applicants, num_people_taken)

women<- as.data.frame(women)
men<- as.data.frame(men)

shinyApp(
  ui<-fluidPage(
    htmltools::includeMarkdown("./markdown/headline.md"),
    bsCollapse(id = "collapse", multiple=TRUE,
               bsCollapsePanel("Lottery Design",
                           htmltools::includeMarkdown("./markdown/background.md"),
                          style = "info"
               ),
               bsCollapsePanel("How many tickets do I have?",
                          htmltools::includeMarkdown("./markdown/howmanytickets.md"),
                          fluidRow(
                          column(6, sliderInput("apps", label = h5("Previous applications"), min=0, max=5, value = 0)),
                          column(6, sliderInput("finishes", label = h5("Previous Finishes"), min=0, max=5, value = 0)),
                          column(6, sliderInput("volunteer", label = h5("Volunteer Shifts"), min=0, max=30, value = 0)),
                          column(6, sliderInput("trailwork", label = h5("Extra Trailwork"), min=0, max=10, value = 0)),
                         "Your tickets in the lottery:", textOutput("tickets")
                         ),
                         style = "success"
               ),
               
               bsCollapsePanel("What are my odds?",
                        htmltools::includeMarkdown("./markdown/justtellmetheodds.md"),                 
                        "These are the odds for women with the given number of tickets:",
                        DT::dataTableOutput("oddsW"),
                        "These are the odds for men with the given number of tickets:",
                        DT::dataTableOutput("oddsM"),
                        style = "info"
                ),
               bsCollapsePanel("Set the Seed", 
                               fluidRow(#htmltools::includeMarkdown("./markdown/implementingthelottery.md"),  
                                 column(6, numericInput("num", label = h1("Enter the seed"), value = NA)),
                                 column(6, numericInput("num2", label = h1("Confirm the seed"), value = NA)),
                               ),
                               style="success" 
               ),
               
               bsCollapsePanel("Results: Winners",
                               fluidRow(
                               column(6, "These are the women selected in the lottery:", 
                               DT::dataTableOutput("valueW"),
                               downloadButton("downloadW", "Download Women")),
                               column(6, "These are the men selected in the lottery:", 
                               DT::dataTableOutput("valueM"),
                               downloadButton("downloadM", "Download Men")),
                              ),
                               style="info"),
               bsCollapsePanel("Results: Waitlist",
                               fluidRow(
                                 column(6,"These are the waitlisted women:",
                                        dataTableOutput("valueWLW"),
                                        downloadButton("downloadWLW", "Download WL Women")),
                                 column(6, "These are the waitlisted men:",
                                        dataTableOutput("valueWLM"),
                                        downloadButton("downloadWLM", "Download WL Men"),),
                               ),
                               style="success"
                               )
               
              
               
    ), #bsCollapse
    
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
    
    #ODDS
    output$oddsW <- DT::renderDataTable({
      datatable(w_odds) %>% formatPercentage(c("odds_of_selection"), digits=2) %>% formatRound(c("tickets_per_applicant", "num_people_taken"), digits=3)
    })
    output$oddsM <- DT::renderDataTable({
      datatable(m_odds) %>% formatPercentage(c("odds_of_selection"), digits=2) %>% formatRound(c("tickets_per_applicant", "num_people_taken"), digits=3)
    })

        
    #WOMEN WINNERS, REACTIVE, THEN RENDER
    w_winners <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
          set.seed(input$num) #SET THE SEED WITH DICE
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
    
    output$valueW <- DT::renderDataTable(
      w_winners(), options = list(pageLength = 5)
    ) #END RENDER PRINT
    
    output$downloadW <-downloadHandler(
        filename = function(){"WomenWinners.csv"}, 
         content = function(fname){
         write.csv(w_winners(), fname)
         }
    )
    
    #MEN WINNERS, REACTIVE, THEN RENDER
    m_winners <- reactive({
      req(input$num, input$num2)
      if (input$num == input$num2) {
        set.seed(input$num) #SET THE SEED WITH DICE!
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
    output$valueM <- DT::renderDataTable(
      m_winners(), options = list(pageLength = 5)
    ) #END RENDER PRINT
    output$downloadM <-downloadHandler(
      filename = function(){"MenWinners.csv"}, 
      content = function(fname){
        write.csv(m_winners(), fname)
      }
    )
    
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
    output$valueWLW <- renderDataTable({
    waitlistW()
    }) #END RENDER PRINT
    output$downloadWLW <-downloadHandler(
      filename = function(){"WomenWaitlist.csv"}, 
      content = function(fname){
        write.csv(waitlistW(), fname)
      }
    )
      
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
    output$valueWLM <- renderDataTable({
      waitlistM()
    }) #END RENDER PRINT
    output$downloadWLM <-downloadHandler(
      filename = function(){"MenWaitlist.csv"}, 
      content = function(fname){
        write.csv(waitlistM(), fname)
      }
    )
    
  }, #CLOSE SERVER
  options = list(height = 900) 
)




