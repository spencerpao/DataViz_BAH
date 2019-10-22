# Before importing, I deleted the Region Summary Data (at the very end of the document)
# I kept lines 1-53
# Importing library to read excel doc
library(readxl)
data <- read_excel("Data Viz Challenge/Viz1/2017 Fuel and Energy_DataVizChallenge1.xlsx", col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"),
                   skip = 1)

# Scenario 1
# Game plan: Summarize how much energy is being used by region. 
# Legends will contain information about which region represents which states..
# X Axis - > Regions
# Y Axis -> Percentage of corresponding Energy in relation to corresponding gas.

totals <- as.data.frame(t(cbind(colSums(data[,3:9])))) #(Sum of columns)
# Percentages table
Percentages <- as.data.frame(matrix(data = NA, nrow = dim(data)[1], ncol = dim(totals)[2]))

# Calculating the Percentages of Energy Used by the corresponding total energy (by column)
# Iterating through Columns
for (f in 1:dim(Percentages)[2]) {
  # Iterating through rows
  for (s in 1:dim(Percentages)[1]) {
    Percentages[s,f] = data[s,f+2] / as.numeric(totals[f])
  }
}
# Now, I have a dataframe of Percentages that each single state makes up to the total amount
names(Percentages) <- names(data)[3:9]
Percentages <- cbind(data$Region,Percentages)
names(Percentages)[1] <- cbind("Region")

# Time to aggregate sums
Sum_Ag_Percentages <- Percentages %>%
  group_by(Region) %>%
  summarise_all(funs(sum))

# Taking out the total Column
Sum_Ag_Percentages <- Sum_Ag_Percentages[,1:7]
# Reordering the Sum_AG_Percentages table so that it is easier on the eyes...
Sum_Ag_Percentages <- rbind(Sum_Ag_Percentages %>% filter(Region == "I"),
                            Sum_Ag_Percentages %>% filter(Region == "II"),
                            Sum_Ag_Percentages %>% filter(Region == "III"),
                            Sum_Ag_Percentages %>% filter(Region == "IV"),
                            Sum_Ag_Percentages %>% filter(Region == "V"),
                            Sum_Ag_Percentages %>% filter(Region == "VI"),
                            Sum_Ag_Percentages %>% filter(Region == "VII"),
                            Sum_Ag_Percentages %>% filter(Region == "VIII"),
                            Sum_Ag_Percentages %>% filter(Region == "IX"),
                            Sum_Ag_Percentages %>% filter(Region == "X"))
# Dataframe to plot...
plot_AG <- data.frame()
for (n in 2:dim(Sum_Ag_Percentages)[2]) {
  # Going to combine 
  Nt <- as.data.frame(rep(names(Sum_Ag_Percentages)[n], 10))
  v <- cbind(Sum_Ag_Percentages[,1], Sum_Ag_Percentages[,n],Nt)
  names(v) <- cbind("Region", "Gas", "Type_of_Gas")
  plot_AG <- rbind(plot_AG, v)
}

library(dplyr)
library(ggplot2)
library(tidyr)
library(grid)
library(gtable)
library(forcats)
# Creating the Names for each region...
# will use when associating regions with states.
for (i in 1:length(unique(data$Region))) {
  assign(paste("Names_",unique(data$Region)[i], sep = "" ), cbind(data %>% filter(Region == unique(data$Region)[i]) %>% select(State)))
}

plot_AG$Region <- fct_relevel(plot_AG$Region, levels=c("I","II","III","IV","V","VI","VII","VIII","IX","X"))

# Now plotting.. bar plots
p = ggplot(plot_AG, aes(fill=Type_of_Gas, x = Region, y = Gas)) + 
  geom_bar(position = "dodge", color = "black",stat = "identity") + 
  ggtitle("U.S. Public Transit Fuel & Energy Consumption by Region (2017) \n By Spencer Pao")  + 
  ylab("Fuel Used (%)") +
  labs(fill = "Types of Fuel",
       caption = "Region I: CT, MA ME, NH, RI, VT \n Region II: NJ, NY, PR \n Region III: DC, DE, MD, PA, VA, WV \n Region IV: AL, FL, GA, KY, MS, NC, SC, TN \n Region V: IL, IN, MI, MN, OH, WI \n Region VI: AR, LA, NM, OK, TX \n Region VII: IA, KS, MO, NE \n Region VIII: CO, MT, ND, SD, UT \n Region IX: AZ, CA, HI, NV \n Region X: AK, ID, OR, WA") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)) +
  scale_fill_manual(values=c("#3333cc", "#ff9933", "#cc0000", "#66ffff", "#cc0099", "#009900"))
p
