#rm(list=ls(all=TRUE))

#RUN THIS CODE AFTER THE SEED IS KNOWN


set.seed(-123456789) #PUT IN THE REAL SEED!


library(dplyr)
library(tibble)
library(readxl)
temp<-read.csv("./2023 HiLo lottery data_FINAL.csv", stringsAsFactors = FALSE) #LOAD THE DATA
df<-as_tibble(temp)

df$fullname<-paste(df$First_Name, df$Last_Name, sep=" ", collapse = NULL)
head(df)
n_men_app=nrow(men<-df[which(df$Gender=="M"),])
n_women_app=nrow(women<-df[which(df$Gender=="F"),])


n_women_pick <- 68
n_men_pick <- 63


df$Applications<-df$Previous_Applications

df$k <- ifelse(df$Previous_Finishes==0 , 0,
               ifelse(df$Previous_Finishes==1,  0.5,
                      ifelse(df$Previous_Finishes==2, 1, 
                             ifelse(df$Previous_Finishes==3, 1.5,
                                    ifelse(df$Previous_Finishes>=4, 0.5, 0)))))


#Shifts max out at 30
df$n<-pmin(df$Volunteer_Shifts, 30)
df$t<-pmin(df$Extra_Trailwork, 10)

#Tickets=2^(n+k+1)+2ln(v+t+1) where n, k, v, and t are defined as follows:
df$tickets <-2^(df$k+df$Applications+1) + 2*log(df$n+df$t+1)

#SPLIT THE DATA INTO MENS AND WOMENS
men<-df[which(df$Gender=="M"),]
women<-df[which(df$Gender=="F"),]

####################################################

#PRINT THE ODDS
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
w_odds
############################
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
m_odds

#PRINT A CSV OF THE ODDS
men_blurb<-"This is the mens odds. Find your number of tickets to find the odds. \n The columns are your tickets (points), the odds of a man with that number of tickets getting in, the number of men with that exact tickets/point-score to apply, and the expected number of men with those tickets to get in. \n \n"
women_blurb<-"\n This is the womens odds. Find your number of tickets to find the odds. \n The columns are your tickets (points), the odds of a woman with that number of tickets getting in, the number of women with that exact tickets/point-score to apply, and the expected number of women with those tickets to get in. \n \n"
sink("odds.txt")
cat(men_blurb)
round(m_odds, digits = 4)
cat(women_blurb)
round(w_odds, digits = 4)
sink()



##############################################################
#DRAW THE LOTTERY

#dplyr function sample_n will work with weights, normalize automatically
#syntax:sample_n(tbl, size, replace = FALSE, weight = NULL, .env = NULL, ...)
#Run the separate lotteries
women_winners<-sample_n(women, n_women_pick, replace = FALSE, weight=women$tickets)
men_winners<-sample_n(men, n_men_pick, replace = FALSE, weight=men$tickets)



#########################################################
#waitlist
#########################################################

women_waitlist_pool<-anti_join(women, women_winners)
n_women_waitlist_pool<-nrow(women_waitlist_pool)

men_waitlist_pool<-anti_join(men, men_winners)
n_men_waitlist_pool<-nrow(men_waitlist_pool)

#SIMPLER THIS YEAR, JUST ENTER THE NUMBERS FOR THE WL, 8 and 7
n_women_wait_pick<-75
n_men_wait_pick<-75

#PICK THE WAITLISTERS
#WOMEN MIGHT NOT HAVE ANY

women_waiters <- sample_n(women_waitlist_pool, n_women_wait_pick, replace = FALSE, weight=women_waitlist_pool$tickets)
w_output_wait<-subset(women_waiters, select=c("fullname"))
w_output_wait_priv<-subset(women_waiters, select=c("fullname", "Email_Address"))
w_output_wait$Num<-seq.int(nrow(w_output_wait))
#w_output_wait<-w_output_wait[,c(5,1,2,3,4)]
names(w_output_wait)[1]<-"Waitlisted_Women"


#ASSUME MEN WILL HAVE ENOUGH FOR A FULL WAITLIST
men_waiters <- sample_n(men_waitlist_pool, n_men_wait_pick, replace = FALSE, weight=men_waitlist_pool$tickets)

#I can't figure out how to label tables, so just make the table itself
#look sort of well-labeled
#subset
#dfnew5 <- subset(diamonds, select=c("color", "carat", "price"))
m_output_wait<-subset(men_waiters, select=c("fullname"))
m_output_wait_priv<-subset(men_waiters, select=c("fullname", "Email_Address"))
#Add ID column data$ID <- seq.int(nrow(data))
m_output_wait$Num<-seq.int(nrow(m_output_wait))
#rearrange columns df2[,c(1,3,2,4)
#m_output_wait<-m_output_wait[,c(5,1,2,3,4)]
#rename: names(data)[3]<-"new_name"
names(m_output_wait)[1]<-"Waitlisted_Men"
   


##########################################
#Don't Zipper the waitlists in 2022
########################################
#make column names identical so columns line up
names(m_output_wait)[1]<-"Waitlisted_Name"
names(w_output_wait)[1]<-"Waitlisted_Name"
#bind women first for the waitlist for 2021

temp <- bind_rows(w_output_wait, m_output_wait)

#sort and relabel
#temp <- arrange(temp, Num)
#temp$GenderNum <-temp$Num
#temp$Num <-seq.int(nrow(temp))



################################
#ADD EMAILS TO THE OUTPUT FOR CALEB ONLY
################################
private_winners <- bind_rows(women_winners, men_winners)
write.csv(private_winners, ".\\HL2023Winners.csv")

private_pools <-bind_rows(women_waiters, men_waiters)
private_waiters <- left_join(temp, private_pools, by=c("Waitlisted_Name"="fullname"))
write.csv(private_waiters, ".\\HL2023Waitlist.csv")
