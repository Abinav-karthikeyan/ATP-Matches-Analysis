library(readxl)
library(writexl)
library(tibble)
library(ggplot2)
library(dplyr)
library(leaflet)
library(tidygeocoder)
library(plotly)
library(ggcorrplot)
library(stringr)
library(tidyr)
library(ggpubr)
library(BSDA)
library(distributions3)
library(plotly)
setwd("C://Users//abhin//OneDrive//Documents//Course Content//PDA//Group Collab")

atp_data0 = read.csv("atp_matches_2022.csv")
atp_data1 = read.csv("atp_matches_2021.csv")
atp_data2= read.csv("atp_matches_2023.csv")

nrow(atp_data)

atp_data = rbind(atp_data0, atp_data1, atp_data2)

#Writing to Directory 





for(i in 1:ncol(atp_data))
{
  if(is.character(atp_data[,i]))
    atp_data[,i] = as.factor(atp_data[,i])
}

write_xlsx(atp_data , "Tennis_Dataset.xlsx")



# Compute a new data frame for win loss ratio

wins = atp_data%>% count(winner_name)
colnames(wins) = c("Player", "Number_of_wins")

loss = atp_data%>% count(loser_name)
colnames(loss) = c("Player", "Number_of_defeats")

win_loss_stats = merge(wins, loss , by = "Player" )

colnames(win_loss_stats) = c("Name" , "Wins" , "Loss")


win_loss_stats$Winning_percetage = round(win_loss_stats$Wins / (win_loss_stats$Wins + win_loss_stats$Loss),2)

hands = atp_data%>% group_by(winner_name)%>%count(winner_hand)

hands = hands %>% distinct()

hands= hands[!duplicated(hands),]

colnames(hands) = c("Name", "Hand" , "n")

win_loss_stats = merge(win_loss_stats,hands , by ="Name")

win_loss_stats = win_loss_stats[,!duplicated(as.list(win_loss_stats))]


write_xlsx(win_loss_stats, "ATP_Data//win_loss.xlsx")

# For mapping tourneys


locs = atp_data%>% group_by(tourney_level)%>%
  filter(tourney_level %in% c("A" , "M" , "G" ))%>% distinct(tourney_name)

locs$tourney_name = as.character(locs$tourney_name)

for(i in 1:nrow(locs))
{
  pattern = c("Open" , "Cup" , "Finals" , "Masters")
  
    
    replaced = gsub("Open", "", locs$tourney_name[i])
    replaced =  gsub("Cup", "", replaced)
    replaced =  gsub("Finals", "", replaced)
    replaced =  gsub("Masters", "", replaced)
    replaced = gsub("\\d+" , "" ,replaced)
    
    
    
    locs$tourney_name[i] = replaced
    
  
 
  
}

locs = locs%>%distinct(tourney_name)

locs= as.tibble(locs)

# Geocoding tourneys

lat_longs <- locs[-1,] %>%
  geocode(tourney_name, method = 'osm', lat = latitude , long = longitude)


lat_longs_new = merge(lat_longs , locs[-1,] , by = "tourney_name")
lat_longs_new[80,] = c("US_Open",40.75037647534364, -73.84560994976644, "GrandSlams")
lat_longs_new[62,] = c("Roland Garros",48.845941989847056, 2.253943385891643, "GrandSlams")
lat_longs_new[70,] = c("Sofia",42.69868284365271, 23.32669057472257,"Tour Level")

levels(lat_longs_new$tourney_level) = c("Tour Level" , "Davis" , "Yearend-finals" , "GrandSlams" , "Masters1000")

lat_longs_new$latitude = round(as.numeric(lat_longs_new$latitude),4)
lat_longs_new$longitude = round(as.numeric(lat_longs_new$longitude),4)

qpal <- colorFactor(c("navy", "red","green"), domain =
                      c("Tour Level", "GrandSlams" , "Masters1000"))

leaflet(lat_longs_new) %>% addTiles() %>% addCircleMarkers(radius = 3,
 color = ~qpal(tourney_level), popup =lat_longs_new$tourney_name)%>%
  addLegend(color = c("navy", "red","green") 
            ,labels =c("GrandSlams" , "Masters1000","Tour Level"))%>%
  addProviderTiles(providers$OPNVKarte)


# Build a correlation heat map b/w total number of games in the match, number of aces, number of df_s, number of breaks, match length, winner seed

atp_data$total_dfs = atp_data$w_df + atp_data$l_df
atp_data$total_aces = atp_data$w_ace + atp_data$l_ace
atp_data$total_breaks = atp_data$w_bpFaced - atp_data$w_bpSaved + atp_data$l_bpFaced - atp_data$l_bpSaved

fig <- plot_ly(z = volcano, type = "heatmap")

fig



cor(atp_data$total_dfs,atp_data$total_aces,atp_data$total_breaks)

corr.plot <- ggcorrplot(
  corr, hc.order = TRUE, type = "lower", outline.col = "white",
  p.mat = p.mat
)

ggplot(atp_data)+geom_point(mapping = aes(x = atp_data$total_dfs , y=atp_data$total_breaks))

View(n_incomplete)

which.max(table(n_incomplete$loser_name))

colSums(n_incomplete$loser_name)


# Analysing Player Retirements from this point:

top_retirements = n_incomplete%>% group_by(loser_name)%>% tally()%>%arrange(desc(n))

write_xlsx(top_retirements , "Retirement_ATP.xlsx")

sinner_ret = n_incomplete[n_incomplete$loser_name %in% c("Jannik Sinner","Grigor Dimitrov" , 
                                                         "Matteo Berrettini","Benoit Paire","Gael Monfils","Jack Draper"
                                                         ,"Tallon Griekspoor"),]
View(sinner_ret)

n_incomplete = atp_data[grep('[a-zA-Z]' , atp_data$score),]

n_incomplete%>% group_by(surface)%>% tally()


wc = atp_data[atp_data$winner_entry == "WC",]%>% group_by(atp_data$winner_name)

nrow(wc)

atp_data%>% group_by(winner_name) %>% tally()

last_tie = atp_data[grep("7-6",atp_data$score), ]

final_matches = atp_data[atp_data$round =="F", ]

davis = atp_data[atp_data$tourney_level == "D",]

View(atp_data)

write_xlsx(final_matches,"List_of_Tournament_Finals.xlsx" )
View(final_matches)
View(last_tie)

grass = atp_data[ atp_data$surface== "Grass",]

View(grass)

# Two way Anova, interaction and individual effects between surface of the game and 

atp_data_temp = atp_data%>% mutate(tourney_level = ifelse(tourney_name == "Halle", "M" , tourney_level))
atp_data_temp$avgminsperrally = atp_data$minutes / ( atp_data$w_svpt + atp_data$l_svpt)
atp_data_temp = na.omit(atp_data_temp)

surface_sample = atp_data_temp[,c(2,3,5,50)]%>% filter(tourney_level %in% c("A" , "M" , "G" ))%>% group_by(tourney_name)%>%sample_n(3, replace = TRUE)
surface_sample = unique(surface_sample)

ggqqplot(surface_sample$avgminsperrally) + ggtitle("Q-Q plot for samples of Minutes_per_rally")

shapiro.test(surface_sample$avgminsperrally)

colnames(surface_sample) = c("Tournament" , "Surface" , "Level" , "MinsperRally")

surface_sample$Level = as.factor(surface_sample$Level)

levels(surface_sample$Level) = c("Tour Level","GrandSlam" ,"Masters")

aov = aov(MinsperRally ~ Surface + Level +Surface*Level , data = surface_sample)

summary(aov)

View(surface_sample)

geom_boxplot(surface_sample, aes (surface_sample$avgminsperrally))

ggboxplot( surface_sample,x = "Level", y = "MinsperRally", color = "Surface",
          palette = c("orange", "green" , "blue"))

write_xlsx(surface_sample , "Two_Way_Anova.xlsx")


surface_wide = pivot_wider(surface_sample[,c(-1)] , names_from = tourney_level , values_from = avgminsperrally, values_fn = mean)



# Transformation for Ace Data

ace_data = atp_data[]

View(atp_data)

atp_data%>%group_by(winner_name)%>%group_by(loser_name)

ace_data = read_xlsx("Ace_data.xlsx")

head(ace_data)



for(i in 1:nrow(ace_data))
{
  
  if(is.na(ace_data$Height[i]))
  {
    rows = ace_data%>%filter(`Player Nationality` == ace_data[i,]$`Player Nationality`)
    ace_data$Height[i] = mean(na.omit(rows)$Height)
  }
  
}

ace_data$Height = round(ace_data$Height,0)
ace_data = na.omit(ace_data)
write_xlsx(ace_data , "Ace_data.xlsx")

# Extracting data for Top 10 Defeats

topplayers = c("Carlos Alcaraz",
            "Novak Djokovic",
            "Casper Ruud",
            "Daniil Medvedev",
            "Andrey Rublev",
            "Stefanos Tsitsipas",
            "Holger Rune",
            "Alexander Zverev",
            "Taylor Fritz","Jannik Sinner")


topdefeats = atp_data[atp_data$loser_name %in% topplayers ,]

write_xlsx(topdefeats , "Top_10_Defeats.xlsx")
View(topdefeats)


topdefeatsfinal = topdefeats[topdefeats$round == "F", ]
topdefeatsround = setdiff(topdefeats ,topdefeatsfinal)

topdefeatsfinal$pressurepointrate = (topdefeatsfinal$l_df) / topdefeatsfinal$l_svpt
topdefeatsround$pressurepointrate = (topdefeatsround$l_df) / topdefeatsround$l_svpt
topdefeatsfinal = topdefeatsfinal%>%filter(!is.na(pressurepointrate))
topdefeatsround = topdefeatsround%>%filter(!is.na(pressurepointrate))


sample_round =  round(sample(topdefeatsround$pressurepointrate,65,replace = FALSE),3)*100
shapiro.test(sample_round)

sample_finals = round(topdefeatsfinal$pressurepointrate,3)*100

shapiro.test(sample_finals)

ggqqplot(sample_round)+ ggtitle("Q-Q plot for Double fault rate in Round matches")
ggqqplot(sample_finals) + ggtitle("Q-Q plot for Double fault rate in Finals")

den = sqrt((var(sample_round) / length(sample_round))+ (var(sample_finals) / length(sample_finals)))

z.test(x = sample_round , y = sample_finals , sigma.x = sqrt(var(sample_round)),sigma.y = sqrt(var(sample_finals)) )

df_percent = data.frame("Rounds" = sample_round, "Finals" = c(sample_finals, rep(NA, length(sample_round) - length(sample_finals))))
write_xlsx(df_percent,"2_Way_Z_data.xlsx")


#Area plot

matches = atp_data%>%count(tourney_date)

matches$tourney_date = as.Date(matches$tourney_date,format= '%Y%m%d')

matches$tourney_date = as.Date.character(matches$tourney_date, format = '%Y%m%d')

head(matches)

plot_ly(matches , x=~tourney_date , y = ~n , type = 'scatter' , mode = 'point'
        , fill = 'tozeroy' , fillcolor = 'Pink')%>%layout(xaxis = list(title = "Date" , dtick = "M2", tickformat="%b<br>%Y")
                                                           ,yaxis = list(title = 'Number of Matches Played'))
# Regression Eqn



l1 = c(2,3,4)
l2=c(9,10)

rep(l1,l2)