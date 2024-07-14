#Import the data
library(readxl)
Incident <- read_excel("Incident.xlsx")
Medication <- read_excel("Medication.xlsx")
Patient <- read_excel("Patient.xlsx")
Scene <- read_excel("Scene.xlsx")

#Filter data for graphs
install.packages("tidyverse")
library(dplyr) #load tidyverse.dplyr

#Join data tables by Primary Key (Fact_Incident_PK)
df <- Incident %>% left_join(Medication) %>% left_join(Patient) %>% left_join(Scene)
df <- df %>% arrange(desc(Fact_Incident_PK)) #optional sorting

#Isolate Narcan counts in one table
narcan <- df %>% filter(Medication_Given_Description == 'Naloxone (Narcan)')

#Extract Month from Incident_Date_time col
library(lubridate)
Narcan_plot <- narcan %>% select(Fact_Incident_PK, Incident_Date_Time, Scene_Incident_County_Name, Patient_Gender) %>% mutate(Month = month(Incident_Date_Time))

#Remove unnecessary counties
Narcan_plot2 <- filter(Narcan_plot, Scene_Incident_County_Name == "Benton" | Scene_Incident_County_Name == "Pulaski" | Scene_Incident_County_Name == "Washington")

#Remove duplicate values based on incident number. Different dosages of Narcan were given to same patient at same time.
#I removed duplicates to try to make my Count Y axis resemble the example graph. The exact count values were not matched.
#This could be due to inaccuracies in data collection IE Gender 
Narcan_plot2 <- Narcan_plot2 %>% distinct()

#Extract values to be plotted
plot1 <- data.frame(table(Narcan_plot2$Month, Narcan_plot2$Patient_Gender=='Male'))

#Rename Variables of df
plot1 <- plot1 %>% rename(Month=Var1, Gender=Var2 )
library(stringr)
plot1$Gender <- str_replace_all(plot1$Gender, c("TRUE" = "Male", "FALSE" = "Female"))  

#Load graph package and plot graphs
library(ggplot2) 
p1=ggplot(plot1,aes(y=Freq,x=Month,color=Gender))+geom_line(aes(group=Gender))+geom_point((aes(group=Gender)))+
  ggtitle("2022 Benton, Pulaski, and Washington Naloxone(Narcan) Counts")
print(p1)

#Extract values to be plotted
plot2 <- Narcan_plot2 %>% group_by(Month, Scene_Incident_County_Name) %>% summarise(counts =n())

#Plot 2
p2= ggplot(plot2,aes(y=counts,x=Month,color=Scene_Incident_County_Name))+geom_point(aes(group=Scene_Incident_County_Name))+geom_smooth(aes(group=Scene_Incident_County_Name))+
  scale_x_discrete(limits=c('1','2','3','4','5','6','7','8','9','10','11','12'))+
  ggtitle("2022 Naloxone(Narcan) Counts by County")
print(p2)

#Plot 3
plot3 <- Narcan_plot2 %>% group_by(Month, Scene_Incident_County_Name, Patient_Gender) %>% summarise(counts =n())
plot3 <-plot3 %>% filter(Patient_Gender=='Male' | Patient_Gender=='Female')

#Graph
p3= ggplot(plot3,aes(y=counts,x=Month,color=Scene_Incident_County_Name,linetype=Patient_Gender))+geom_smooth(se=FALSE,aes(group=interaction(Patient_Gender,Scene_Incident_County_Name)))+
  scale_x_discrete(limits=c('1','2','3','4','5','6','7','8','9','10','11','12'))+
  ggtitle("2022 Naloxone(Narcan) Counts by County by Gender")
print(p3)