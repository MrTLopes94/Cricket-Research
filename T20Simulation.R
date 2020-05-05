library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

##%######################################################%##
#                                                          #
####        Import | Slice | Export all matches         ####
#                                                          #
##%######################################################%##


Data <- readr::read_delim("/Users/DrGoble/Documents/R Studio/ipl_csv_male/335982.csv", # Imports a single CSV file  that you want to analyse **REMEMBER TO CHANGE PATH**
                          delim = ";",
                          col_names = FALSE) 
View(Data)
DataSlICE <- Data[-c(1:20),] # Removes the first 20 rows
View(DataSlICE)

write.xlsx(DataSlICE, '/Users/DrGoble/Documents/R Studio/BatSlice.xlsx', # Exports the new to your desired location **REMEMBER TO SELECT PATH**
           col.names = FALSE, row.names = FALSE, append = FALSE)

##%######################################################%##
#                                                          #
####                    Import Match                    ####
#                                                          #
##%######################################################%##

library(readxl)
A335982 <- read_excel("~/Documents/R Studio/IPL Data/A335982.xlsx") # Imports a single EXCEL file  that you want to analyse **REMEMBER TO CHANGE PATH**

View(A335982)

##%######################################################%##
#                                                          #
####                Initail partnership                 ####
#                                                          #
##%######################################################%##

OnStrike <- filter(A335982, Striker == "BB McCullum") # Defines the batter on strike and only selects data when they are on strike **REMEMBER TO CHANGE BATTER NAME**
OffStrike <- filter(A335982, Non_Striker == "BB McCullum") # Defines the NON-STRIKE batter and only selects data when they are on strike **REMEMBER TO CHANGE BATTER NAME**
Partnerships <- bind_rows(OnStrike, OffStrike) # Combines the strike and non-strike data into one sheet- Defines the total partnership of the batter in question

View(OnStrike) 
View(OffStrike)
View(Partnerships)

##%######################################################%##
#                                                          #
####                  Innings OnStrike                  ####
#                                                          #
##%######################################################%##

OnStrikeFil <- filter(OnStrike, Runs == 0, Extras == 1,2,3,4,5,6,7) # filters the onstrike data to exclude extras
FOO<- nrow(OnStrikeFil)
View(FOO)

OnStrikeTRuns <-tally(OnStrike, Runs) # calculates Total Runs scored
OnStrikeBalls <- tally(OnStrike, Innings) - FOO # calculates Total Balls faced
OnStrikeSR <- ((OnStrikeTRuns/OnStrikeBalls)*100) # calculates Strike Rate
OnStrikeSingles <-tally(OnStrike, Runs == 1) # calculates Total singles run
OnStrikeTwos <- tally(OnStrike, Runs == 2) # calculates Total twos run
OnStrikeThrees <- tally(OnStrike, Runs == 3) # calculates Total threes run
OnStrikeFours <- tally(OnStrike, Runs == 4) # calculates Total fours hit
OnStrikeSixes <- tally(OnStrike, Runs == 6) # calculates Total sixes hit
OnStrikeExtras <- tally(OnStrike, Extras) # calculates Total total extras in his innings

# Binds all data claculated above into one row
OnStrikeSummary <- bind_cols(OnStrikeTRuns, OnStrikeBalls, OnStrikeSR, OnStrikeSingles, OnStrikeTwos, OnStrikeThrees, OnStrikeFours, OnStrikeSixes,OnStrikeExtras)
names(OnStrikeSummary)<- c("S_TRuns", "S_Balls", "S_StrikeRate", "S_1's","S_2's", "S_3's", "S_4's", "S_6's", "S_TExtras")
View(OnStrikeSummary)


# THE NEXT SECTION DOES EXACTLY THE SAME AS ABOVE HOWEVER FOR WHEN THE BATTER IS NOT ONSTRIKE - I.E. RUNS FOR HIS PARTNER/PARNERS --> FROM THIS WE CAN LEARN ABOUT THE PARTNERSHIPS 

##%######################################################%##
#                                                          #
####                 Innings OffStrike                  ####
#                                                          #
##%######################################################%##

OffStrikeFil <- filter(OffStrike, Runs == 0, Extras == 1,2,3,4,5,6,7)
F00<- nrow(OffStrikeFil)
View(FOO)

OffStrikeTRuns <-tally(OffStrike, Runs)
OffStrikeBalls <- tally(OffStrike, Innings) - FOO
OffStrikeSR <- ((OffStrikeTRuns/OffStrikeBalls)*100)
OffStrikeSingles <-tally(OffStrike, Runs == 1)
OffStrikeTwos <- tally(OffStrike, Runs == 2)
OffStrikeThrees <- tally(OffStrike, Runs == 3)
OffStrikeFours <- tally(OffStrike, Runs == 4)
OffStrikeSixes <- tally(OffStrike, Runs == 6)
OffStrikeExtras <- tally(OffStrike, Extras)

OffStrikeSummary <- bind_cols(OffStrikeTRuns, OffStrikeBalls, OffStrikeSR, OffStrikeSingles, OffStrikeTwos, OffStrikeThrees, OffStrikeFours, OffStrikeSixes,OffStrikeExtras)
names(OffStrikeSummary)<- c("N_S_TRuns", "N_S_Balls", "N_S_StrikeRate", "N_S_1's","N_S_2's", "N_S_3's", "N_S_4's", "N_S_6's", "N_S_TExtras")
View(OffStrikeSummary)

##%######################################################%##
#                                                          #
####                Partnership Summary                 ####
#                                                          #
##%######################################################%##

# combines the on strike and off strike data to form the partnership data

PartnershipSummary <- bind_cols(OnStrikeSummary, OffStrikeSummary)
View(PartnershipSummary)

# THE NEXT SECTIONS DRAWS HISTOGRAMS FOR THE RUN ACCUMULATION STATS

##%######################################################%##
#                                                          #
####                  Run Accumulation                  ####
#                                                          #
##%######################################################%##

OnStrike1<- mutate(OnStrike, Ball_Num = seq.int(nrow(OnStrike))) #creates new coloumn with ball number for the on strike data 
OffStrike1<- mutate(OffStrike, Ball_Num = seq.int(nrow(OffStrike))) #creates new coloumn with ball number for the off strike data 
Partnerships1 <- mutate(Partnerships, Ball_Num = seq.int(nrow(Partnerships))) #creates new coloumn with ball number for the parnership


ggplot(data=OnStrike1, aes(x=Ball_Num, y=Runs)) + #creates plot when Batter is OnStrike  
    geom_bar(stat="identity", width=0.5, color="blue", fill="white")

ggplot(data=OffStrike1, aes(x=Ball_Num, y=Runs)) + #creates plot when Batter is OffStrike  
    geom_bar(stat="identity", width=0.5, color="red", fill="white")

ggplot(data=Partnerships1, aes(x=Ball_Num, y=Runs)) + #creates combined plot of Strike and Non Strike (I.E. THE PARTNERSHIPS CREATED WHILE THE BATTER IS AT THE CREASE)
    geom_bar(stat="identity", width=0.5, color="green", fill="white")
