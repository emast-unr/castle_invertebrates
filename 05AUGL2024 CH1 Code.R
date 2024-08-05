setwd("C:/Users/eclai/OneDrive/Desktop/School/Master's Degree/Thesis/Chapter 1 Macroinvertebrate Biodiversity/ecosphere_code")
library(openxlsx)
library(dplyr)
library(hillR)
library(ggplot2)
library(ggpubr)
library(viridis)
library(plotly)
library(corrr)
library(plyr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(vegan)
library(car)
library(emmeans)
library(RVAideMemoire)
library(gridExtra)
library(tidyverse)
library(ecodist)
library(rstatix)
library(grid)
library(tidyverse)



#import excel sheets
counts<-read.xlsx("Castle Benthic Raw Data Abbreviated.xlsx",'FamilyCountTotal')
colnames(counts) <- counts[1,]
counts <- counts[-1, ] 
counts <- counts %>%
  select(1:38) %>%    
  na.omit() %>%       
  mutate_at(1:30, ~ as.numeric(.))  # Convert columns 1 to 30 to numeric

abundance.data<-read.xlsx("Castle Benthic Raw Data Abbreviated.xlsx",'FamilyCountTotal')
colnames(abundance.data) <-abundance.data[1,]
abundance.data <-abundance.data[-1, ]
long.term.ice<-read.csv("ice-out-date.csv")
  
#Calculate Ice out mean and standard deviation
# Calculate the mean of the Julian.Date column
mean_julian_date <- mean(long.term.ice$Julian.Date, na.rm = TRUE)

# Calculate the standard deviation of the Julian.Date column
sd_julian_date <- sd(long.term.ice$Julian.Date, na.rm = TRUE)

# Calculate the cutoff values for one standard deviation
lower_cutoff <- mean_julian_date - sd_julian_date
upper_cutoff <- mean_julian_date + sd_julian_date

# Print the cutoff values
cat("One standard deviation range:\n")
cat("Lower cutoff:", lower_cutoff, "\n")
cat("Upper cutoff:", upper_cutoff, "\n")


###Turn count data into hill numbers and create new dataframe 
hill_richness<- hillR::hill_taxa(counts[,1:30], q = 0) #Calculate Eveness
hill_shannon<- hillR::hill_taxa(counts[,1:30], q = 1) #Calculates shannon 
hill_simpsons<- hillR::hill_taxa(counts[,1:30], q = 2) #calculates simpsons

diversity.df<- as.data.frame(cbind(year=as.numeric(counts$year),
                                   site=counts$Site,zone=counts$Zone,month_class=counts$month_class,
                                   Month=counts$month, hill_richness, hill_shannon, hill_simpsons))


# Convert columns to numeric
diversity.df$hill_richness <- as.numeric(diversity.df$hill_richness)
diversity.df$hill_shannon <- as.numeric(diversity.df$hill_shannon)
diversity.df$hill_simpsons <- as.numeric(diversity.df$hill_simpsons)

# Filter rows where hill_richness is greater than 0
diversity.df <- filter(diversity.df, hill_richness > 0)

# Merge data frames by "year"
diversity.df <- merge(diversity.df, long.term.ice, by = "year", all = TRUE)

# Select specific columns for diversity.zone.df
diversity.zone.df <- diversity.df[, c("year", "Month", "zone", "hill_richness", "hill_shannon", "hill_simpsons")]

########################
###  Visualize data  ###
########################

#Ice Data
#We Julian Date: Mean:133, Median:135,Q1:121,Q3:145
long.term.ice$Classification <- factor(long.term.ice$Classification, levels = c("Early", "Average", "Late"))
long.term.ice$Julian.Date<-as.numeric(long.term.ice$Julian.Date)
long.term.ice$year<-as.numeric(long.term.ice$year)
summary(long.term.ice)
long.term.ice.plot <- ggplot(long.term.ice, aes(x = year, y = Julian.Date, color = Classification)) +
  geom_point(size = 3.5) +
  geom_point(shape = 1, size = 3.5, color = "black") +
  scale_color_manual(values = c("Average" = "#117733", "Early" = "#DDCC77", "Late" = "#CC6677")) +
  geom_hline(yintercept = 133, linetype = 2, color = "black", linewidth = 1) + # Increase the line thickness
  annotate("text", x = 2020, y = 136, label = "Mean (133)", size = 10 / .pt) + # 10-point text
  scale_x_continuous(breaks = seq(1960, 2022, by = 10)) +
  scale_y_continuous(breaks = seq(50, 200, by = 10)) +
  ylab("Ice Out (Julian Date)") +
  xlab("Year") +
  theme(legend.position = c(0.15, 0.15)) +
  annotate('rect', xmin = 2008, xmax = 2023, ymin = -Inf, ymax = Inf, alpha = .3, fill = 'grey') +
  theme(panel.background = element_rect(fill = 'white')) +
  theme(panel.border = element_rect(fill = 'transparent', color = 'black', linewidth = 2)) +
  guides(color = guide_legend(override.aes = list(size = 5))) + # Adjust the legend size
  theme(legend.background = element_rect(fill = 'white', color = 'black')) + # Add black border to the legend
  theme(text = element_text(size = 10)) # Ensure all text is 10 points
long.term.ice.plot

# Save the plot with a width of 8.5 cm and dpi of 600
ggsave("long_term_ice_plot.png", plot = long.term.ice.plot, width = 18, units = "cm", dpi = 600)


###Diversity by zone
diversity.df$zone <- factor(diversity.df$zone, levels = c("Littoral", "Sublittoral", "Profundal"))
diversity.df<-na.omit(diversity.df)

# Richness Zone plot
richness.plot <- diversity.df %>%
  ggplot(aes(x = zone, y = hill_richness, fill = zone)) +
  geom_boxplot(color = "black",outlier.shape=NA) +
  ylab("Richness (q=0)") +
  scale_y_continuous(limits = c(0, 13)) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

# Shannon Zone plot
shannon.plot <- diversity.df %>%
  ggplot(aes(x = zone, y = hill_shannon, fill = zone)) +
  geom_boxplot(color = "black",outlier.shape=NA) +
  ylab("Shannon (q=1)") +
  scale_y_continuous(limits = c(0, 13)) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        axis.text.x = element_blank(),
        axis.title.x=element_blank(),
        legend.position = "none")

# Simpsons Zone plot
simpsons.plot <- diversity.df %>%
  ggplot(aes(x = zone, y = hill_simpsons, fill = zone)) +
  geom_boxplot(color = "black",outlier.shape=NA) +
  ylab("Simpsons (q=2)") +
  xlab("Zone")+
  scale_y_continuous(limits = c(0, 13)) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220"))+
  theme(panel.background = element_rect(fill = "white", color = "black"),
        legend.position = "none")

# Combine zone plots
combined_plots_zone <- ggarrange(richness.plot,shannon.plot, simpsons.plot, ncol = 1, nrow = 3)

# Print the combined plots
print(combined_plots_zone)


###Diversity by ice and zone

diversity.df$Classification <- factor(diversity.df$Classification, levels = c("Early", "Average", "Late"))
diversity.df$zone <- factor(diversity.df$zone, levels = c("Littoral","Sublittoral","Profundal"))
create_boxplot <- function(data, y_var, title) {
  ggplot(data, aes(x = Classification, y = !!sym(y_var), fill = zone)) +
    geom_boxplot(color = "black", outlier.shape = NA) +
    labs(y = title) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 13)) +
    scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220"), name = "Zone") +  # Color scheme
    guides(fill = guide_legend(title = "Zone")) +  # Add fill legend
    theme(
      legend.position = "none",  # Remove legends
      panel.background = element_rect(fill = "white", color = "black"),  # White background with black outline
      panel.grid.major = element_blank(),
      panel.grid.major.y = element_blank(),  # Remove major y-axis grid lines
      panel.grid.minor.y = element_blank(),  # Remove minor y-axis grid lines
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),  # Remove x-axis label
      axis.line = element_line(color = "black"),
      axis.text.y = element_text(color = "black")  # Add y-axis labels
    )
}

# Create boxplots
richness.plot <- create_boxplot(diversity.df, "hill_richness", "Richness(q=0)")
shannon.plot <- create_boxplot(diversity.df, "hill_shannon", "Shannon (q=1)")
simpsons.plot <- create_boxplot(diversity.df, "hill_simpsons", "Simpsons (q=2)")

# Combine plots with legends at the top
combined_plot_ice <- grid.arrange(richness.plot, 
                              shannon.plot, 
                              simpsons.plot,
                              ncol = 1, 
                              heights = c(1, 1, 1))

# Print the combined plot
print(combined_plot_ice)


####Stacked relative abundance barcharts
#zone
relative_ab_data<-read.xlsx("Castle Benthic Raw Data Abbreviated.xlsx",'relative_abundance')
#zone class
relative_zone<-relative_ab_data%>%
  select(c(39:42))%>%
  na.omit()

# Define the desired order of x-axis categories
desired_order <- c("Littoral", "Sublittoral", "Profundal")

# Reorder the data frame based on the desired order of x-axis categories
relative_zone$zone <- factor(relative_zone$zone, levels = desired_order)

# Order 'family_zone' within each 'zone' level based on 'relative_ab_zone'
relative_zone <- relative_zone %>%
  group_by(zone) %>%
  mutate(rank = rank(desc(relative_ab_zone))) %>%
  ungroup() %>%
  arrange(zone, rank) %>%
  select(-rank)

stacked.zone<-ggplot(relative_zone,aes(x=zone, y=relative_ab_zone,fill = reorder(family_zone, -relative_ab_zone)))+
  geom_bar(stat="identity",position="stack",color="black")+
  geom_text(aes(label = ifelse(relative_ab_zone < 1, "", as.character(family_zone))), 
            position = position_stack(vjust = 0.5), 
            color = "black") +
  labs(x = "Zone",
       y = "Relative Abundance (%)",
       fill = "Family")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "black"),  # Set background color to white with black border
        panel.grid.major.y = element_blank(),  # Set color and width of major gridlines on y-axis
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))+  # Set axis line color to black
  scale_y_continuous(expand = expansion(mult = c(0.005, 0.005)))

stacked.zone
ggsave("zone_class_relative_abundance.png", device='png', dpi=600)






##Figure combinng zone diveristy and relative abundance
combined_plot_zone_div_abund <- grid.arrange(combined_plots_zone,stacked.zone,ncol=2,nrow=1)
ggsave("comined_zone_figure.png",plot=combined_plot_zone_div_abund, device='png', width = 18, units = "cm", dpi = 600)




#ice relative abundance stacked barcharts
relative_ab_data<-read.xlsx("Castle Benthic Raw Data Abbreviated.xlsx",'relative_abundance')
#ice class
relative_ice<-relative_ab_data%>%
  select(c(43:46))%>%
  na.omit()

# Define the desired order of x-axis categories
desired_order <- c("Early", "Average", "Late")

# Reorder the data frame based on the desired order of x-axis categories
relative_ice$ice <- factor(relative_ice$ice, levels = desired_order)

# Order 'family_ice' within each 'ice' level based on 'relative_ab_ice'
relative_ice <- relative_ice %>%
  group_by(ice) %>%
  mutate(rank = rank(desc(relative_ab_ice))) %>%
  ungroup() %>%
  arrange(ice, rank) %>%
  select(-rank)

stacked.ice<-ggplot(relative_ice,aes(x=ice, y=relative_ab_ice,fill = reorder(family_ice, -relative_ab_ice)))+
  geom_bar(stat="identity",position="stack",color="black")+
  geom_text(aes(label = ifelse(relative_ab_ice < 1, "", as.character(family_ice))), 
            position = position_stack(vjust = 0.5), 
            color = "black") +
  labs(x = "Ice Classification",
       y = "Relative Abundance (%)",
       fill = "Family")+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white", color = "black"),  # Set background color to white with black border
        panel.grid.major.y = element_blank(),  # Set color and width of major gridlines on y-axis
        panel.grid.minor = element_blank(),  # Remove minor grid lines
        axis.line = element_line(color = "black"))+  # Set axis line color to black
  scale_y_continuous(expand = expansion(mult = c(0.005, 0.005)))

stacked.ice

combined_plot_ice_div_abund <- grid.arrange(combined_plot_ice,stacked.ice,ncol=2,nrow=1)
ggsave("combined_ice_figure.png",plot=combined_plot_ice_div_abund, device='png', width = 18, units = "cm", dpi = 600)


################################
###   NMDS Analysis Family   ###
################################
  
###NMDS FAMILY abundance for whole lake (compare zones)
nmds.data.all<-counts
nmds.data.all<-filter(nmds.data.all,inventory_sum>0)
nmds.data<-subset(nmds.data.all,select=c(1:30))
#hellinger transforming to use with double zero problem
nmds.data<-decostand(nmds.data,method="hellinger")
set.seed(123)
#Bray-Curtis takes into account species presence/absence and abundance
nmds<-metaMDS(nmds.data,distance="bray",k=2,trymax=100)
#Stress 0.16 likely fine for interpretation
nmds
#shepards test/goodness of fit
goodness(nmds)#test statistics for goodness of fit for each point
stressplot(nmds)


#plot with more variables
data.scores<-as.data.frame(scores(nmds)$sites)
data.scores$zone<-nmds.data.all$Zone
data.scores$ice_class<-nmds.data.all$ice_class
data.scores$lake_site<-nmds.data.all$Site
data.scores$month<-nmds.data.all$month_class
species.scores<-as.data.frame(scores(nmds, "species"))
species.scores$species<-rownames(species.scores)
data.scores$zone <- factor(data.scores$zone, levels = c("Littoral", "Sublittoral", "Profundal"))
data.scores$ice_class <- factor(data.scores$ice_class, levels = c("Early", "Average", "Late"))
data.scores$month <- factor(data.scores$month, levels = c("June", "July", "Aug/Sept"))
# Define the mapping of site names to codes
site_code <- c("Alder" = "L1", "Dock" = "L2", "Spring" = "L3", "T1" = "L4", "T3" = "S1", "T4" = "P1", "T5" = "P2")

# Add a new column site_codes from site names
data.scores <- mutate(data.scores, site_code = site_code[lake_site])

plot1 <- ggplot() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = "black"), size = 0.4) + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 3, color = "black") + 
  stat_ellipse(data = data.scores, aes(x = NMDS1, y = NMDS2, color = zone)) + 
  theme_bw() +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(x = "NMDS1", colour = "Zone", y = "NMDS2") +
  scale_color_manual(values = c("Littoral" = "#40B0A6", "Sublittoral" = "#FFC20A", "Profundal" = "#DC3220")) +
  annotate("text", x = -Inf, y = Inf, label = "a)", hjust = -0.5, vjust = 1, size = 6)

plot2 <- ggplot() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = "black"), size = 0.4) + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 3, color = "black") + 
  stat_ellipse(data = data.scores, aes(x = NMDS1, y = NMDS2, color = ice_class)) +
  theme_bw() +
  scale_color_manual(values = c("Early" = "#DDCC77", "Average" = "#117733", "Late" = "#CC6677")) +
  labs(x = "NMDS1", colour = "Ice Out Classification", y = "NMDS2") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -Inf, y = Inf, label = "b)", hjust = -0.5, vjust = 1, size = 6)

plot3 <- ggplot() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = "black"), size = 0.4) + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 3, color = "black") + 
  stat_ellipse(data = data.scores, aes(x = NMDS1, y = NMDS2, color = month)) +
  theme_bw() +
  scale_color_manual(values = c("June" = "#785ef0", "July" = "#009e73", "Aug/Sept" = "#fe6100")) +
  labs(x = "NMDS1", colour = "Month", y = "NMDS2") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -Inf, y = Inf, label = "c)", hjust = -0.5, vjust = 1, size = 6)

plot4 <- ggplot() +
  geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, colour = "black"), size = 0.4) + 
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 1, size = 3, color = "black") + 
  stat_ellipse(data = data.scores, aes(x = NMDS1, y = NMDS2, color = site_code)) +
  theme_bw() +
  scale_color_manual(values = c("L1" = "#332288", "L2" = "#117733", "L3" = "#44aa99","L4"="#88CCEE","S1"="#ddcc77","P1"="#CC6677","P2"="#aa4499")) +
  labs(x = "NMDS1", colour = "Site Code", y = "NMDS2") +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.spacing.y = unit(0, "mm"), 
        panel.border = element_rect(colour = "black", fill = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  annotate("text", x = -Inf, y = Inf, label = "d)", hjust = -0.5, vjust = 1, size = 6)

# Arrange plots in a 2x2 grid and save
combined_plot <- grid.arrange(plot1, plot2, plot3, plot4, ncol = 2, nrow = 2)

# Save the combined plot
ggsave("combined_plot.png", combined_plot, width = 18, height = 13, units = "cm",dpi=600)

#ANOSIM for zone
nmds.data<-as.matrix(nmds.data)
anosim.family.zone<-anosim(nmds.data,nmds.data.all$Zone,distance="bray",permutations = 9999)
anosim.family.zone
#ANOSIM for ice out
anosim.family.ice<-anosim(nmds.data,nmds.data.all$ice_class,distance="bray",permutations = 9999)
anosim.family.ice
#ANOSIM for month
anosim.family.month<-anosim(nmds.data,nmds.data.all$month_class,distance="bray",permutations = 9999)
anosim.family.month
#ANOSIM for site (significant)
anosim.family.site<-anosim(nmds.data,nmds.data.all$Site,distance="bray",permutations = 9999)
anosim.family.site



#The next section is broken down by zone to see if groupings are significant depending on the zone.Results show no community differences even when broken down by zone
#Littoral NMDS
nmds.data.littoral.all<-filter(nmds.data.all, Zone=="Littoral")
nmds.data.littoral.all<-filter(nmds.data.littoral.all,inventory_sum>0)
nmds.data.littoral<-subset(nmds.data.littoral.all,select=c(1:30))


#hellinger transform
nmds.data.littoral<-decostand(nmds.data.littoral,method="hellinger")
set.seed(123)
#Bray-Curtis takes into account species presence/absence and abundance
nmds.littoral<-metaMDS(nmds.data.littoral,distance="bray",k=2,trymax=100)
#Stress 0.16 likely fine for interpretation
nmds.littoral
#shepards test/goodness of fit
goodness(nmds.littoral)#test statistics for goodness of fit for each point
stressplot(nmds.littoral)

## No significance ANOSIM for ice out (R=-0.004603, significance= 0.54)
anosim.littoral.family.ice<-anosim(nmds.data.littoral,nmds.data.littoral.all$ice_class,distance="bray",permutations = 9999)
anosim.littoral.family.ice
#difference based on site R=0.3605, Significance+ 1e-04
anosim.littoral.family.site<-anosim(nmds.data.littoral,nmds.data.littoral.all$Site,distance="bray",permutations = 9999)
anosim.littoral.family.site
#Anosim based on month, not significat R=-0.002388, significance=0.5013
anosim.littoral.family.month<-anosim(nmds.data.littoral,nmds.data.littoral.all$month_class,distance="bray",permutations = 9999)
anosim.littoral.family.month


#Sublittoral NMDS
nmds.data.sublittoral.all<-filter(nmds.data.all, Zone=="Sublittoral")
nmds.data.sublittoral.all<-filter(nmds.data.sublittoral.all,inventory_sum>0)
nmds.data.sublittoral<-subset(nmds.data.sublittoral.all,select=c(1:30))

#hellinger transform
nmds.data.sublittoral<-decostand(nmds.data.sublittoral,method="hellinger")
set.seed(123)
#Bray-Curtis takes into account species presence/absence and abundance
nmds.sublittoral<-metaMDS(nmds.data.sublittoral,distance="bray",k=2,trymax=100)
#Stress 0.14 likely fine for interpretation
nmds.sublittoral
#shepards test/goodness of fit
goodness(nmds.sublittoral)#test statistics for goodness of fit for each point
stressplot(nmds.sublittoral)

## No significance ANOSIM for ice out
anosim.sublittoral.family.ice<-anosim(nmds.data.sublittoral,nmds.data.sublittoral.all$ice_class,distance="bray",permutations = 9999)
##Anosim showing no significant difference in family based on ice (R=-0.04294, significance=0.8717)
anosim.sublittoral.family.ice
#month
anosim.sublittoral.family.month<-anosim(nmds.data.sublittoral,nmds.data.sublittoral.all$month_class,distance="bray",permutations = 9999)
##Anosim showing no significant difference in family based on ice (R=-0.04294, significance=0.8717)
anosim.sublittoral.family.month

#Profundal NMDS
nmds.data.profundal.all<-filter(nmds.data.all, Zone=="Profundal")
nmds.data.profundal.all<-filter(nmds.data.profundal.all,inventory_sum>0)
nmds.data.profundal<-subset(nmds.data.profundal.all,select=c(1:30))

#hellinger transform
nmds.data.profundal<-decostand(nmds.data.profundal,method="hellinger")
set.seed(123)
#Bray-Curtis takes into account species presence/absence and abundance
nmds.profundal<-metaMDS(nmds.data.profundal,distance="bray",k=2,trymax=100)
#Stress 0.14 likely fine for interpretation
nmds.profundal
#shepards test/goodness of fit
goodness(nmds.profundal)#test statistics for goodness of fit for each point
stressplot(nmds.profundal)

# ANOSIM
#ice
anosim.profundal.family.ice<-anosim(nmds.data.profundal,nmds.data.profundal.all$ice_class,distance="bray",permutations = 9999)
anosim.profundal.family.ice
#month
anosim.profundal.family.month<-anosim(nmds.data.profundal,nmds.data.profundal.all$month_class,distance="bray",permutations = 9999)
anosim.profundal.family.month
#site
anosim.profundal.family.site<-anosim(nmds.data.profundal,nmds.data.profundal.all$Site,distance="bray",permutations = 9999)
anosim.profundal.family.site

#####################
###  PERMANOVAs   ###
#####################

#assess normality of biodiveristy data
littoral.diversity <- diversity.df %>%
  filter(zone == "Littoral") %>%
  mutate(hill_richness = as.numeric(hill_richness))
#littoral
hist(littoral.diversity$hill_richness)
qqnorm(littoral.diversity$hill_richness)
qqline(littoral.diversity$hill_richness, col = "red")  # Add a reference line
#The null hypothesis of the test is that the data is normally distributed. If the p-value is less than your chosen significance level (e.g., 0.05), you would reject the null hypothesis and conclude that the data is not normally distributed.
shapiro.test(littoral.diversity$hill_richness)
#littoral shannon
hist(littoral.diversity$hill_shannon)
qqnorm(littoral.diversity$hill_shannon)
qqline(littoral.diversity$hill_shannon, col = "red")  # Add a reference line
shapiro.test(littoral.diversity$hill_shannon)
#littoral simpsons
hist(littoral.diversity$hill_simpsons)
qqnorm(littoral.diversity$hill_simpsons)
qqline(littoral.diversity$hill_simpsons, col = "red")  # Add a reference line
shapiro.test(littoral.diversity$hill_simpsons)


#sublittoral
sublittoral.diversity <- diversity.df %>%
  filter(zone == "Sublittoral") %>%
  mutate(hill_richness = as.numeric(hill_richness))
#sublittoral
hist(sublittoral.diversity$hill_richness)
qqnorm(sublittoral.diversity$hill_richness)
qqline(sublittoral.diversity$hill_richness, col = "red")  # Add a reference line
#The null hypothesis of the test is that the data is normally distributed. If the p-value is less than your chosen significance level (e.g., 0.05), you would reject the null hypothesis and conclude that the data is not normally distributed.
shapiro.test(sublittoral.diversity$hill_richness)
#sublittoral shannon
hist(sublittoral.diversity$hill_shannon)
qqnorm(sublittoral.diversity$hill_shannon)
qqline(sublittoral.diversity$hill_shannon, col = "red")  # Add a reference line
shapiro.test(sublittoral.diversity$hill_shannon)
#sublittoral simpsons
hist(sublittoral.diversity$hill_simpsons)
qqnorm(sublittoral.diversity$hill_simpsons)
qqline(sublittoral.diversity$hill_simpsons, col = "red")  # Add a reference line
shapiro.test(sublittoral.diversity$hill_simpsons)


#profundal
profundal.diversity <- diversity.df %>%
  filter(zone == "Profundal") %>%
  mutate(hill_richness = as.numeric(hill_richness))
#profundal
hist(profundal.diversity$hill_richness)
qqnorm(profundal.diversity$hill_richness)
qqline(profundal.diversity$hill_richness, col = "red")  # Add a reference line
#The null hypothesis of the test is that the data is normally distributed. If the p-value is less than your chosen significance level (e.g., 0.05), you would reject the null hypothesis and conclude that the data is not normally distributed.
shapiro.test(profundal.diversity$hill_richness)
#profundal shannon
hist(profundal.diversity$hill_shannon)
qqnorm(profundal.diversity$hill_shannon)
qqline(profundal.diversity$hill_shannon, col = "red")  # Add a reference line
shapiro.test(profundal.diversity$hill_shannon)
#profundal simpsons
hist(profundal.diversity$hill_simpsons)
qqnorm(profundal.diversity$hill_simpsons)
qqline(profundal.diversity$hill_simpsons, col = "red")  # Add a reference line
shapiro.test(profundal.diversity$hill_simpsons)

#Can't meet normality assumptions, so use permanova as non-parametric test
#profundal
perm.profundal<- profundal.diversity[complete.cases(profundal.diversity$hill_richness), ]
adonis2(perm.profundal$hill_richness~perm.profundal$Classification)
perm.profundal<- profundal.diversity[complete.cases(profundal.diversity$hill_shannon), ]
adonis2(perm.profundal$hill_shannon~perm.profundal$Classification)
perm.profundal<- profundal.diversity[complete.cases(profundal.diversity$hill_simpsons), ]
adonis2(perm.profundal$hill_simpsons~perm.profundal$Classification)

#sublittoral
#Richness is different
perm.sublittoral<- sublittoral.diversity[complete.cases(sublittoral.diversity$hill_richness), ]
adonis2(perm.sublittoral$hill_richness~perm.sublittoral$Classification)
perm.sublittoral<- sublittoral.diversity[complete.cases(sublittoral.diversity$hill_shannon), ]
adonis2(perm.sublittoral$hill_shannon~perm.sublittoral$Classification)
perm.sublittoral<- sublittoral.diversity[complete.cases(sublittoral.diversity$hill_simpsons), ]
adonis2(perm.sublittoral$hill_simpsons~perm.sublittoral$Classification)

#littoral
###Seing richness difference in littoral
perm.littoral<- littoral.diversity[complete.cases(littoral.diversity$hill_richness), ]
adonis2(perm.littoral$hill_richness~perm.littoral$Classification)
perm.littoral<- littoral.diversity[complete.cases(littoral.diversity$hill_shannon), ]
adonis2(perm.littoral$hill_shannon~perm.littoral$Classification)
perm.littoral<- littoral.diversity[complete.cases(littoral.diversity$hill_simpsons), ]
adonis2(perm.littoral$hill_simpsons~perm.littoral$Classification)

#####Are zone richness different? YES according to PERMANOVA
diversity.df<-na.omit(diversity.df)
adonis2(diversity.df$hill_richness~diversity.df$zone)
#####Are zone shannon different? YES according to PERMANOVA
adonis2(diversity.df$hill_shannon~diversity.df$zone)
#####Are zone simpsons different? YES according to PERMANOVA
adonis2(diversity.df$hill_simpsons~diversity.df$zone)

##Pairwise comparison
#All significantly different
bray.rich<-bcdist(diversity.df[,c("hill_richness")])
bray.shan<-bcdist(diversity.df[,c("hill_shannon")])
bray.simp<-bcdist(diversity.df[,c("hill_simpsons")])
#significantly differences between early and late ice out
bray.lit.rich<-bcdist(perm.littoral[,c("hill_richness")])
bray.sublit.rich<-bcdist(perm.sublittoral[,c("hill_richness")])


#Bonferroni Correction: This method is simple and conservative. It involves dividing the significance level (α) by the number of comparisons (n). For example, if you are performing 10 comparisons and want an overall α level of 0.05, you would test each individual comparison at α = 0.005.
pairwise.perm.manova(bray.rich,diversity.df$zone,p.method="bonferroni",nperm=999)
pairwise.perm.manova(bray.shan,diversity.df$zone,p.method="bonferroni",nperm=999)
pairwise.perm.manova(bray.simp,diversity.df$zone,p.method="bonferroni",nperm=999)
pairwise.perm.manova(bray.lit.rich,perm.littoral$Classification,p.method="bonferroni",nperm=999)
pairwise.perm.manova(bray.sublit.rich,perm.sublittoral$Classification,p.method="bonferroni",nperm=999)
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6099145/#:~:text=The%20simplest%20way%20to%20adjust,length%20of%20the%20vector%20P_values).

########################################
###       Identify unique taxa       ###
########################################

#Ice littoral 
# Step 1: Subset the data for the littoral zone
littoral_data <- counts[counts$Zone == "Littoral", ]

# Step 2: Check ice_class values
unique_ice_classes <- unique(littoral_data$ice_class)
print(unique_ice_classes)

# Step 3: Separate the data into early, average, and late ice-out years
early_ice_class <- littoral_data[littoral_data$ice_class == "Early", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(early_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
early_ice_class <- early_ice_class[, !(names(early_ice_class) %in% zero_columns)]

# Step 3: Separate the data into average, average, and late ice-out years
average_ice_class <- littoral_data[littoral_data$ice_class == "Average", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(average_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
average_ice_class <- average_ice_class[, !(names(average_ice_class) %in% zero_columns)]

# Step 3: Separate the data into late, average, and late ice-out years
late_ice_class <- littoral_data[littoral_data$ice_class == "Late", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(late_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
late_ice_class <- late_ice_class[, !(names(late_ice_class) %in% zero_columns)]


# Extract species names from column names
early_names <- unique(unlist(strsplit(colnames(early_ice_class), "_")))
average_names <- unique(unlist(strsplit(colnames(average_ice_class), "_")))
late_names <- unique(unlist(strsplit(colnames(late_ice_class), "_")))


# Identify species present in early ice class but not in late ice class
early_not_in_late <- setdiff(early_names, late_names)
# Print the species names
print(early_not_in_late)

late_not_in_early<- setdiff(late_names, early_names)
print(late_not_in_early)

#Ice sublittoral 
# Step 1: Subset the data for the sublittoral zone
sublittoral_data <- counts[counts$Zone == "Sublittoral", ]

# Step 2: Check ice_class values
unique_ice_classes <- unique(sublittoral_data$ice_class)
print(unique_ice_classes)

# Step 3: Separate the data into early, average, and late ice-out years
early_ice_class <- sublittoral_data[sublittoral_data$ice_class == "Early", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(early_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
early_ice_class <- early_ice_class[, !(names(early_ice_class) %in% zero_columns)]

# Step 3: Separate the data into average, average, and late ice-out years
average_ice_class <- sublittoral_data[sublittoral_data$ice_class == "Average", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(average_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
average_ice_class <- average_ice_class[, !(names(average_ice_class) %in% zero_columns)]

# Step 3: Separate the data into late, average, and late ice-out years
late_ice_class <- sublittoral_data[sublittoral_data$ice_class == "Late", ]%>%
  select(c(1:30))
# Calculate the sum of each column
column_sums <- colSums(late_ice_class)

# Identify columns where the sum is zero
zero_columns <- names(column_sums[column_sums == 0])

# Remove columns with all zero values
late_ice_class <- late_ice_class[, !(names(late_ice_class) %in% zero_columns)]


# Extract species names from column names
early_names <- unique(unlist(strsplit(colnames(early_ice_class), "_")))
average_names <- unique(unlist(strsplit(colnames(average_ice_class), "_")))
late_names <- unique(unlist(strsplit(colnames(late_ice_class), "_")))


# Identify species present in early ice class but not in late ice class
early_not_in_average <- setdiff(early_names, average_names)
# Print the species names
print(early_not_in_average)

early_not_in_late<- setdiff(early_names, late_names)
print(early_not_in_late)


##compare zones
# Assuming your count data is stored in a dataframe called 'counts'
# and you have a column 'Zone' indicating the zone (e.g., littoral, sublittoral, profundal)

# Step 1: Subset the data for each zone
littoral_data <- counts[counts$Zone == "Littoral", ]
sublittoral_data <- counts[counts$Zone == "Sublittoral", ]
profundal_data <- counts[counts$Zone == "Profundal", ]

# Step 2: Select relevant columns (assuming the taxa columns are from 1 to 30)
littoral_taxa <- littoral_data %>%
  select(c(1:30))
sublittoral_taxa <- sublittoral_data %>%
  select(c(1:30))
profundal_taxa <- profundal_data %>%
  select(c(1:30))

# Step 3: Calculate the sum of each column for each zone
littoral_column_sums <- colSums(littoral_taxa)
sublittoral_column_sums <- colSums(sublittoral_taxa)
profundal_column_sums <- colSums(profundal_taxa)

# Identify columns where the sum is non-zero in each zone
littoral_non_zero_columns <- names(littoral_column_sums[littoral_column_sums > 0])
sublittoral_non_zero_columns <- names(sublittoral_column_sums[sublittoral_column_sums > 0])
profundal_non_zero_columns <- names(profundal_column_sums[profundal_column_sums > 0])

# Step 4: Identify taxa present in one zone but not in the others
littoral_not_in_sublittoral <- setdiff(littoral_non_zero_columns, sublittoral_non_zero_columns)
littoral_not_in_profundal <- setdiff(littoral_non_zero_columns, profundal_non_zero_columns)

sublittoral_not_in_littoral <- setdiff(sublittoral_non_zero_columns, littoral_non_zero_columns)
sublittoral_not_in_profundal <- setdiff(sublittoral_non_zero_columns, profundal_non_zero_columns)

profundal_not_in_littoral <- setdiff(profundal_non_zero_columns, littoral_non_zero_columns)
profundal_not_in_sublittoral <- setdiff(profundal_non_zero_columns, sublittoral_non_zero_columns)

# Step 5: Print the taxa names
print("Taxa present in Littoral but not in Sublittoral:")
print(littoral_not_in_sublittoral)

print("Taxa present in Littoral but not in Profundal:")
print(littoral_not_in_profundal)

print("Taxa present in Sublittoral but not in Littoral:")
print(sublittoral_not_in_littoral)

print("Taxa present in Sublittoral but not in Profundal:")
print(sublittoral_not_in_profundal)

print("Taxa present in Profundal but not in Littoral:")
print(profundal_not_in_littoral)

print("Taxa present in Profundal but not in Sublittoral:")
print(profundal_not_in_sublittoral)



#################################
### Productivity Calculations ###
#################################

invertbiomass <- read.xlsx("Castle Benthic Raw Data Abbreviated.xlsx",'updated_BiomassR')
iceouts <- data.frame(seq(2008,2023,1))
iceouts$iceoff <- c(140,141,176,178,121,135,57,51,112,147,101,153,116,114,104,167)
names(iceouts) <- c("year", "iceoff")
#classify using one standard deviation from the mean
iceouts <- iceouts %>%
  mutate(classification = case_when(
    iceoff < 108 ~ "Early",
    iceoff >= 109 & iceoff <= 158 ~ "Average",
    iceoff > 158 ~ "Late"
  ))


invertbiomass$Year <- as.numeric(invertbiomass$Year)
invertbiomass$Month <- as.factor(invertbiomass$Month)
invertbiomass$Mass <- as.numeric(invertbiomass$Mass)
invertbiomass$Site <- as.factor(invertbiomass$Site)
invertbiomass$count <- rep(1,24704)
BiomassSummary <- ddply(invertbiomass, c("Year","Month","Site", "Order", "Family"), summarise,
                        N = sum(!is.na(count)),
                        avgMass = mean(Mass),
                        stdev = sd(Mass),
                        se = stdev/sqrt(N),
                        total = sum(Mass),
                        maxMass = max(Mass))
#Correcting for surface area differences
bucket <- .045238
eckman <- .023104*3
warnings()
#estimating Mass per M2
BiomassSummary$MassM2 <- ifelse(BiomassSummary$Site == "Spring", BiomassSummary$total/bucket,
                                ifelse(BiomassSummary$Site == "Dock",BiomassSummary$total/bucket,BiomassSummary$total/eckman))
#estimating the taxon specific P/B ration using Banse and Mosher 1980#
#a and b are Banse and Mosher variables
a = -.19 
b = -0.37                                       
Cpercent = .45
BiomassSummary$PB <- 10^(a+(b*(log10(BiomassSummary$avgMass)))) 
BiomassSummary$TaxaProd_mgC_M2 <-(BiomassSummary$PB*BiomassSummary$MassM2)*.45 #converting dry mass to Carbon using 45% from VZ and Chandra 2006

#Assigning zones 
BiomassSummary$zone <- ifelse(BiomassSummary$Site == "Spring", "Littoral",
                              ifelse(BiomassSummary$Site == "Alder", "Littoral",
                                     ifelse(BiomassSummary$Site == "Dock", "Littoral",
                                            ifelse(BiomassSummary$Site == "T1", "Littoral",
                                                   ifelse(BiomassSummary$Site == "T3", "Sublittoral",
                                                          ifelse(BiomassSummary$Site == "T4", "Profundal",
                                                                 ifelse(BiomassSummary$Site == "T5", "Profundal","Littoral")))))))
unique(BiomassSummary$zone)

BiomassSummary$ice_type <- ifelse(BiomassSummary$Year == 2008, "Average", 
                                  ifelse(BiomassSummary$Year == 2009, "Average",
                                         ifelse(BiomassSummary$Year == 2010, "Late",
                                                ifelse(BiomassSummary$Year == 2011, "Late",
                                                       ifelse(BiomassSummary$Year == 2012, "Average",
                                                              ifelse(BiomassSummary$Year == 2013, "Average",
                                                                     ifelse(BiomassSummary$Year == 2014, "Early",
                                                                            ifelse(BiomassSummary$Year == 2015, "Early", 
                                                                                   ifelse(BiomassSummary$Year == 2016, "Average",
                                                                                          ifelse(BiomassSummary$Year == 2017,"Average",
                                                                                                 ifelse(BiomassSummary$Year == 2018,"Early",
                                                                                                        ifelse(BiomassSummary$Year == 2019,"Average",
                                                                                                               ifelse(BiomassSummary$Year == 2020,"Average",
                                                                                                                      ifelse(BiomassSummary$Year == 2021,"Average",
                                                                                                                             ifelse(BiomassSummary$Year == 2022,"Early",
                                                                                                                                    ifelse(BiomassSummary$Year == 2023, "Late",""))))))))))))))))
SiteProd <- ddply(BiomassSummary, c("Year","Month","Site", "ice_type"), summarise,
                  total_mgC_M2 = sum(TaxaProd_mgC_M2,na.rm=TRUE))
SiteProd$Month<- factor(SiteProd$Month, levels = c("June", "July", "Aug/Sept"))


#Assigning zones 
SiteProd$zone <- ifelse(SiteProd$Site == "Spring", "Littoral",
                        ifelse(SiteProd$Site == "Alder", "Littoral",
                               ifelse(SiteProd$Site == "Dock", "Littoral",
                                      ifelse(SiteProd$Site == "T1", "Littoral",
                                             ifelse(SiteProd$Site == "T3", "Sublittoral",
                                                    ifelse(SiteProd$Site == "T4", "Profundal",
                                                           ifelse(SiteProd$Site == "T5", "Profundal","Littoral")))))))
#Calculate means (different group sizes)
zone_prod_weighted <- SiteProd %>%
  group_by(Year, Month, ice_type,zone) %>%
  mutate(avg_mgC_M2 = mean(total_mgC_M2, na.rm = TRUE))%>%
  distinct(avg_mgC_M2,.keep_all=FALSE)%>%
  ungroup() %>%
  mutate(zone = factor(zone, levels = c("Littoral", "Sublittoral", "Profundal")))

zone_prod_weighted_plot <- zone_prod_weighted %>%
  ggplot(aes(x = zone, y = avg_mgC_M2, fill = zone)) +
  geom_boxplot(color = "black", outlier.shape = 21) +  # Changed outlier shape to 21
  labs(x = "Zone",
       y = "Mean mgC/M2") +
  scale_y_continuous(limits = c(0, 3750)) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220")) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.25, 0.91),  # Adjust legend position as needed
        legend.box.background = element_rect(color = "black", linewidth = 1),
        legend.title = element_blank(),
        legend.key = element_rect(fill = NA, color = NA))  # Remove individual key boxes

zone_prod_weighted_plot
ggsave("zone_prod.png", zone_prod_weighted_plot, width = 8.5,height= 18, units = "cm",dpi=600)


####Zone productivity by ice type
zone_prod_weighted$ice_type<- factor(zone_prod_weighted$ice_type, levels = c("Early", "Average", "Late"))
zone_ice_prod <- zone_prod_weighted %>%
  ggplot(aes(x = ice_type, y = avg_mgC_M2, fill = zone)) +
  geom_boxplot(color = "black", outlier.shape = "circle") +
  labs(x = "Ice ice_type",
       y = "Mean mgC/M2") +
  scale_y_continuous(limits = c(0, 3750)) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220")) +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        legend.position = c(0.85,0.85),
        legend.background = element_rect(fill = "white", color = "black"),  # Black box around legend
        legend.title = element_blank(),  # Remove legend title
        legend.key = element_rect(color = "black"))

zone_ice_prod


##Month
june_prod <- filter(zone_prod_weighted, Month == "June")
july_prod <- filter(zone_prod_weighted, Month == "July")
aug.sept_prod <- filter(zone_prod_weighted, Month == ("Aug/Sept"))

# Find the range of your data to set appropriate limits
max_value <- max(c(june_prod$avg_mgC_M2, july_prod$avg_mgC_M2, aug.sept_prod$avg_mgC_M2), na.rm = TRUE)

# Set y-axis limits to cover the range of your data
y_limits <- c(0, max_value)

# Create ggplot objects for each month with adjusted y-axis limits
june_prod_plot <- june_prod %>%
  ggplot(aes(x = ice_type, y = avg_mgC_M2, fill = zone)) +
  geom_boxplot(color = "black", outlier.shape = "circle") +
  labs(x = NULL,  # Remove x-axis label for all except the bottom one
       y = NULL) +  # Remove y-axis label
  scale_y_continuous(limits = y_limits) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none") +
  annotation_custom(grob = textGrob("June", x = unit(0.05, "npc"), y = unit(0.95, "npc"), just = "left", gp = gpar(fontsize = 12, fontface = "bold")))

july_prod_plot <- july_prod %>%
  ggplot(aes(x = ice_type, y = avg_mgC_M2, fill = zone)) +
  geom_boxplot(color = "black", outlier.shape = "circle") +
  labs(x = NULL,  # Remove x-axis label for all except the bottom one
       y = NULL) +  # Remove y-axis label
  scale_y_continuous(limits = y_limits) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none") +
  annotation_custom(grob = textGrob("July", x = unit(0.05, "npc"), y = unit(0.95, "npc"), just = "left", gp = gpar(fontsize = 12, fontface = "bold")))

aug.sept_prod_plot <- aug.sept_prod %>%
  ggplot(aes(x = ice_type, y = avg_mgC_M2, fill = zone)) +
  geom_boxplot(color = "black", outlier.shape = "circle") +
  labs(x = "Ice ice_type",  # Keep x-axis label only for the bottom plot
       y = NULL) +  # Remove y-axis label
  scale_y_continuous(limits = y_limits) +
  scale_fill_manual(values = c("#40B0A6", "#FFC20A", "#DC3220")) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white", color = "black"),
        panel.grid = element_blank(),  # Remove gridlines
        legend.position = "none") +
  annotation_custom(grob = textGrob("Aug/Sept", x = unit(0.05, "npc"), y = unit(0.95, "npc"), just = "left", gp = gpar(fontsize = 12, fontface = "bold")))

# Create the y-axis label
y_label <- textGrob("Mean mgC/M2", rot = 90, gp = gpar(fontsize = 14))

# Arrange plots in a 3x1 layout with the common y-axis label
productivity_month_plot<-grid.arrange(arrangeGrob(june_prod_plot, july_prod_plot, aug.sept_prod_plot, ncol = 1), left = y_label)
ggsave("productivity_month.png", productivity_month_plot, width = 18, height = 16, units = "cm",dpi=600)

lit <- subset(zone_prod_weighted, zone == "Littoral")
lit$Month <- factor(lit$Month, levels = c("June", "July", "Aug/Sept"))
lit$ice_type <- factor(lit$ice_type, levels = c("Early", "Average", "Late"))
sublit <- subset(zone_prod_weighted, zone == "Sublittoral")
sublit$Month <- factor(sublit$Month, levels = c("June", "July", "Aug/Sept"))
sublit$ice_type <- factor(sublit$ice_type, levels = c("Early", "Average", "Late"))
profun <- subset(zone_prod_weighted, zone == "Profundal")
profun$Month <- factor(profun$Month, levels = c("June", "July", "Aug/Sept"))
profun$ice_type <- factor(profun$ice_type, levels = c("Early", "Average", "Late"))




hist(zone_prod_weighted$avg_mgC_M2)
shapiro.test(zone_prod_weighted$avg_mgC_M2)

#Permanova for zone
zone_prod_weighted<-na.omit(zone_prod_weighted)
adonis2(zone_prod_weighted$avg_mgC_M2~zone_prod_weighted$zone)
bray<-bcdist(zone_prod_weighted[,c("avg_mgC_M2")])
pairwise.perm.manova(bray,zone_prod_weighted$zone,p.method="hochberg",nperm=999)

#not significant in whole lake all months
adonis2(zone_prod_weighted$avg_mgC_M2~zone_prod_weighted$ice_type)

#monthy by zone
july_lit<-filter(july_prod, zone=="Littoral")
adonis2(july_lit$avg_mgC_M2~july_lit$ice_type)
june_lit<-filter(june_prod, zone=="Littoral")
adonis2(june_lit$avg_mgC_M2~june_lit$ice_type)
aug.sept_lit<-filter(aug.sept_prod, zone=="Littoral" )
adonis2(aug.sept_lit$avg_mgC_M2~aug.sept_lit$ice_type)

july_sublit<-filter(july_prod, zone=="Sublittoral")
adonis2(july_sublit$avg_mgC_M2~july_sublit$ice_type)
june_sublit<-filter(june_prod, zone=="Sublittoral")
adonis2(june_sublit$avg_mgC_M2~june_sublit$ice_type)
aug.sept_sublit<-filter(aug.sept_prod, zone=="Sublittoral")
#Significant
adonis2(aug.sept_sublit$avg_mgC_M2~aug.sept_sublit$ice_type)
##Error 
bray<-bcdist(aug.sept_sublit[,c("avg_mgC_M2")])
#because of too low of samples 13 total,8 average, 4 early, 1 late
pairwise.perm.manova(bray,aug.sept_sublit$ice_type,p.method="hochberg")

july_profun<-filter(july_prod, zone=="Profundal")
adonis2(july_profun$avg_mgC_M2~july_profun$ice_type)
june_profun<-filter(june_prod, zone=="Profundal")
adonis2(june_profun$avg_mgC_M2~june_profun$ice_type)
aug.sept_profun<-filter(aug.sept_prod, zone=="Profundal")
adonis2(aug.sept_profun$avg_mgC_M2~aug.sept_profun$ice_type)



