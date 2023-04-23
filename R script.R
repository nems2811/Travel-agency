abd<-read.csv("C:/Users/manik/Desktop/QMB_Class/Project/Abandoned.csv")
res<-read.csv("C:/Users/manik/Desktop/QMB_Class/Project/Reservation.csv")

abd[abd==""] <- NA
res[res==""]<- NA

known_states <- abd[complete.cases(abd['Address']),]
table(known_states$Test_Control)
View(known_states)

test_group<-subset(abd,abd$Test_Control=="test")
View(test_group)
control_group<-subset(abd,abd$Test_Control=="control")
View(control_group)

match_email=test_group$Email[complete.cases(test_group$Email)] %in% res$Email[complete.cases(res$Email)]
match_incoming=test_group$Incoming_Phone[complete.cases(test_group$Incoming_Phone)] %in% res$Incoming_Phone[complete.cases(res$Incoming_Phone)]
match_contact=test_group$Contact_Phone[complete.cases(test_group$Contact_Phone)] %in% res$Contact_Phone[complete.cases(res$Contact_Phone)]
match_incoming_contact= test_group$Incoming_Phone[complete.cases(test_group$Incoming_Phone)] %in% res$Contact_Phone[complete.cases(res$Contact_Phone)] 
match_contact_incoming= test_group$Contact_Phone[complete.cases(test_group$Contact_Phone)] %in% res$Incoming_Phone[complete.cases(res$Incoming_Phone)]


test_group$match_email <-0
test_group$match_email[complete.cases(test_group$Email)] <- 1* match_email
sum(test_group$match_email)

test_group$match_incoming <- 0
test_group$match_incoming[complete.cases(test_group$Incoming_Phone)] <- 1* match_incoming
sum(test_group$match_incoming)

test_group$match_contact <- 0
test_group$match_contact[complete.cases(test_group$Contact_Phone)] <- 1* match_contact
sum(test_group$match_contact)

test_group$match_incoming_contact <- 0
test_group$match_incoming_contact[complete.cases(test_group$Incoming_Phone)] <- 1* match_incoming_contact
sum(test_group$match_incoming_contact)

test_group$match_contact_incoming <- 0
test_group$match_contact_incoming[complete.cases(test_group$Contact_Phone)] <- 1* match_contact_incoming
sum(test_group$match_contact_incoming)


test_group$pur<-0
test_group$pur <- 1*(test_group$match_contact | test_group$match_contact_incoming | test_group$match_email | test_group$match_incoming_contact | test_group$match_incoming)
sum(test_group$pur)
View(test_group)

match_email1=control_group$Email[complete.cases(control_group$Email)] %in% res$Email[complete.cases(res$Email)]
table(match_email1)
match_incoming1=control_group$Incoming_Phone[complete.cases(control_group$Incoming_Phone)] %in% res$Incoming_Phone[complete.cases(res$Incoming_Phone)]
match_contact1=control_group$Contact_Phone[complete.cases(control_group$Contact_Phone)] %in% res$Contact_Phone[complete.cases(res$Contact_Phone)]
match_incoming_contact1= control_group$Incoming_Phone[complete.cases(control_group$Incoming_Phone)] %in% res$Contact_Phone[complete.cases(res$Contact_Phone)] 
match_contact_incoming1= control_group$Contact_Phone[complete.cases(control_group$Contact_Phone)] %in% res$Incoming_Phone[complete.cases(res$Incoming_Phone)]

control_group$match_email <-0
control_group$match_email[complete.cases(control_group$Email)] <- 1* match_email1
sum(control_group$match_email)

control_group$match_incoming <- 0
control_group$match_incoming[complete.cases(control_group$Incoming_Phone)] <- 1* match_incoming1
sum(control_group$match_incoming)

control_group$match_contact <- 0
control_group$match_contact[complete.cases(control_group$Contact_Phone)] <- 1* match_contact1
sum(control_group$match_contact)

control_group$match_incoming_contact <- 0
control_group$match_incoming_contact[complete.cases(control_group$Incoming_Phone)] <- 1* match_incoming_contact1
sum(control_group$match_incoming_contact)

control_group$match_contact_incoming <- 0
control_group$match_contact_incoming[complete.cases(control_group$Contact_Phone)] <- 1* match_contact_incoming1
sum(control_group$match_contact_incoming)

control_group$pur<-0
control_group$pur <- 1*(control_group$match_contact | control_group$match_contact_incoming | control_group$match_email | control_group$match_incoming_contact | control_group$match_incoming)
sum(control_group$pur)

View(control_group)

state1 = subset(test_group,test_group$Address=="KS")
nrow(state1)
sum(state1$pur)
View(state1c)

state1c = subset(control_group,control_group$Address=="KS")
nrow(state1c)
sum(state1c$pur)

state2 = subset(test_group,test_group$Address=="TX")
nrow(state2)
sum(state2$pur)


state2c = subset(control_group,control_group$Address=="TX")
nrow(state2c)
sum(state2c$pur)


state3 = subset(test_group,test_group$Address=="FL")
nrow(state3)
sum(state3$pur)

state3c = subset(control_group,control_group$Address=="FL")
nrow(state3c)
sum(state3c$pur)

state4 = subset(test_group,test_group$Address=="AZ")
nrow(state4)
sum(state4$pur)

state4c = subset(control_group,control_group$Address=="AZ")
nrow(state4c)
sum(state4c$pur)

state5 = subset(test_group,test_group$Address=="UT")
nrow(state5)
sum(state5$pur)

state5c = subset(control_group,control_group$Address=="UT")
nrow(state5c)
sum(state5c$pur)

install.packages("writexl")
library("writexl")

new_df<-subset(abd,abd$pur==1)

new_df$D_Email<- ifelse(new_df$Email != 'NA',1,0)
new_df["D_Email"][is.na(new_df["D_Email"])] <- 0

new_df$D_State<- ifelse(new_df$Address != 'NA',1,0)
new_df["D_State"][is.na(new_df["D_State"])] <- 0

new_file<- data.frame(
  Customer_ID=new_df$Caller_ID,
  Test_Variable =new_df$Test_Control,
  Outcome = new_df$pur,
  D_Email = new_df$D_Email,
  D_State = new_df$D_State
)

write_xlsx(new_file,"C:/Users/manik/Desktop/QMB_Class/Project/Matched.xlsx")

View(new_file)
View(abd)

linear_model<-lm(abd$pur~abd$Test_Control, data = abd)
summary(linear_model)
abc<-lm(pur ~ Test_Control, data = abd)
summary(abc)

model<- lm(abd$pur ~ abd$Test_Control + abd$email+ abd$state, data = abd)
summary(model)

multi_linear_model <- lm(pur~Test_Control * email +Test_Control * state, data=abd)
summary(multi_linear_model)


anova_out=aov(abd$pur ~ abd$Test_Control, data = abd)
summary(anova_out)


library(stargazer)
stargazer(linear_model,model,multi_linear_model,type = "html",out="C:/Users/manik/Desktop/QMB_Class/Project/midterm.htm")

View(abd)


write_xlsx(abd,"C:/Users/manik/Desktop/QMB_Class/Project/Cleaned_Data.xlsx")
