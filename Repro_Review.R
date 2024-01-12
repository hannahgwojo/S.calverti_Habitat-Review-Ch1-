# Set working directory 
setwd("D:/McIntyre_Lab/Thesis/Data/ReproReview")

#Library the ggplot2 package
library(ggplot2)
library(dplyr)

# Make a data frame of the different habitat characteristics:
hab.char <- c("Seepage", "Steephead", "Sphagnum moss", "Sand bottom","Undercut banks",
             "Small stream", "Forested", "Sandhill")

# Make a data frame for the frequency of occurrence for each characteristic
freq <- c(36, 12, 24, 12, 12, 36, 52, 76)

# Make a data frame for the combined habitat characteristics and frequency of each characteristic:
hab.freq <- data.frame(hab.char, freq)

# Add a column that specifies the scale of each habitat characteristic
hab.freq$Scale <- c("Local", "Local", "Local", "Local", "Local", "Local", 
                    "Landscape", "Landscape")

# Add a column to data frame with the number of sources that cite each term
hab.freq$Number <- c("N=9", "N=3", "N=6", "N=3", "N=3", "N=9", "N=13", "N=19")


#Make a bar graph showing the frequency of each characteristic
hab.plot <- ggplot(data = hab.freq) +
  geom_bar(aes(x = factor(hab.char, level = c("Seepage", "Steephead",           #Add levels to order the bars by scale
                                        "Sphagnum moss", "Sand bottom",
                                        "Undercut banks", "Small stream", 
                                        "Forested","Sandhill")),
              y = freq, fill = Scale), stat = "identity", color= "black") +     #Color the bars by scale and outline them in black
  geom_text(aes(x= hab.char, y= freq, label=Number), vjust= -0.25, size = 5) +  #Add the number of sources for each term to the top of the bars and make the font size larger
  expand_limits(x=0, y=c(0,100)) +                                              #Expands the y-axis to 100 as max value
  scale_y_continuous(expand = c(0,0)) + scale_x_discrete(expand = c(0,0)) +     #Brings the bars to rest on the x-axis
  xlab("Habitat/Landscape Characteristic") +                                    #Label the x-axis
  ylab("Frequency of occurrence (%)") +                                         #Label the y-axis
  scale_fill_manual(name = "Scale", labels = c("Landscape", "Local"),
                    values = cpalette) +                                                         #Create your own color palette for the bars
  theme_classic() +                                                             #Preset theme that looks clean
  theme(legend.spacing.y = unit(0.05, 'cm'),                                    #Creates some vertical spacing between the legend keys
        axis.text = element_text(color = "black", size = 15),                   #Makes the axes black
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))                                                                            

hab.plot                 


# Create a different color palette for the bars (you have to run this before you run the graph code)
stacked.color <- c("#000000", "#FFFFFF", "#999999") #It will use the colors in the order they are listed in the object
cpalette <- c("#003300","#99FF99")
#Copied code to switch the bar labels between sample size and percent easily:
# ("36%", "12%", "24%", "12%", "12%", "36%", "52%", "76%")
# ("N=9", "N=3", "N=6", "N=3", "N=3", "N=9", "N=13", "N=19")

###########################################################################
##### Make a stacked bar graph with the breakdown of each source type #####
###########################################################################

#Read in the data for the stacked bar graph
stacked.data <- read.csv("stacked_data.csv")

#Make a stacked bar graph to show distribution of source type
stacked.bar <- ggplot(stacked.data, aes(x = factor(term, level = c("Seepage", 
                                                  "Steephead", "Sphagnum", 
                                                  "Sand", "Underbank",
                                                  "Small stream","Forested",
                                                  "Sandhill")),
                                        y = termtotal, fill = type)) +
  geom_bar(position = 'stack', stat = 'identity', color = "black") +
  geom_text(aes(x = term,label = termtotal, y = termtotal), size = 5, 
            hjust = 0.5, vjust = 1.5, position = "stack") +
  scale_fill_manual(name = "Source Type", 
                      labels = c("Citizen Science", "Published Sources", "Survey"),
                    values = stacked.color) +
  scale_x_discrete(name = "Habitat/Landscape Characteristic", 
                   labels = c("Seepage", "Steephead", "Sphagnum moss", 
                              "Sand bottom", "Undercut banks", "Small stream",
                              "Forested", "Sandhill"), expand = c(0,0)) +
  expand_limits(x=0, y=c(0,20)) +
  scale_y_continuous(name = " Number of sources", expand = c(0,0)) +
  theme_classic() +                                                             
  theme(legend.spacing.y = unit(0.05, 'cm'),                                    
        axis.text = element_text(color = "black", size = 15),                   
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))  
  
  

stacked.bar



#"Forested", "Sand bottom", "Sandhill", "Seepage", "Small",
#"Sphagnum moss", "Steephead", "Undercut banks"
