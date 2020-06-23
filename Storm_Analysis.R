library(data.table)
library(dplyr)
library(ggplot2)
library(R.utils)
library(xtable)
library(ggpubr)
library(RColorBrewer)

#Create a sub folder (IF NOT EXISTS) to store the data
sub_dir <- "project_data"
output_dir <- file.path(getwd(), sub_dir)
file_data <- file.path(getwd(), sub_dir, "StormData.csv")

# Only download if the file doesn't exists
if (!file.exists(file_data)){
    # Check if the folder exists
    if (!dir.exists(output_dir)){
        dir.create(output_dir)
    }
    url_target <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
    bzip_file <- file.path( output_dir, "raw_data.bz2")
    download.file(url_target, bzip_file)
    
    bunzip2(filename=bzip_file,destname=file_data, overwrite=TRUE, remove=FALSE)
}

# Read only the fields that will be needed
fields_needed <- c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm_data <- fread( file_data, select = fields_needed )

# Fix the column with Proprerty Damage, according to its "K" = 1000, "M" = 1000000, "B" = 1000000000. Keep the same if criteria
storm_data[PROPDMGEXP %in% c("K","k"), PROPDMG:= PROPDMG * 1000 ] # K = Thousands . Multiply by 1000
storm_data[PROPDMGEXP %in% c("M","m"), PROPDMG:= PROPDMG * 1000000 ] # M = Millions. Multiply by 1000000
storm_data[PROPDMGEXP %in% c("B","b"), PROPDMG:= PROPDMG * 1000000000 ] # B = Billions. Multiply by 1000000000

# Other cases we should keep the same value 

#--------------------------------------------------------
#Which types of events are most harmful to population health?
#------------------------------------------------------
overall_fatalities <- sum(storm_data$FATALITIES)    
overall_injuries <- sum(storm_data$INJURIES)    

harm_per_event <-   storm_data %>%  
                    group_by(EVTYPE)  %>% 
                    summarise(Total_Fatalities = sum(FATALITIES, na.rm=TRUE), Total_Injuries = sum(INJURIES, na.rm=TRUE))

top_injuries <-  as.data.table(top_n(harm_per_event, n=10, Total_Injuries)) 
top_fatalities <-  as.data.table(top_n(harm_per_event, n=10, Total_Fatalities)) 

top_injuries[,contribuition:=round((Total_Injuries/overall_injuries)*100,1)]
top_fatalities[,contribuition:=round((Total_Fatalities/overall_fatalities)*100,1)]

events <- union( top_injuries$EVTYPE, top_fatalities$EVTYPE)
color_events <- hcl.colors(length(events), palette = "Set3" )
names(color_events)  <- events

max_fatalities = max(top_fatalities$Total_Fatalities)
max_injuries = max(top_injuries$Total_Injuries)


g1 <- top_fatalities %>%  
    ggplot(aes(y = Total_Fatalities, x = reorder(x = EVTYPE, X = Total_Fatalities), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = FALSE)   + 
    ylim(0,max_fatalities*1.2) +
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Fatalities+max_fatalities*0.15), 
              colour="black") +
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Fatalities by Event",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal()

g2 <- top_injuries %>%  
    ggplot(aes(y = Total_Injuries, x = reorder(x = EVTYPE, X = Total_Injuries), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = FALSE )   + 
    ylim(0,max_injuries*1.2) +
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Injuries+max_injuries*0.15), 
                colour="black") +
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Injuries by Event",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal()

g3 <- as.data.table(union(top_injuries, top_fatalities)) %>% 
    ggplot(aes(y = Total_Injuries, x = reorder(x = EVTYPE, X = Total_Injuries), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = TRUE )   + 
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    theme_minimal() + theme(legend.position = "bottom")


ggarrange(g1, g2, common.legend = TRUE, legend.grob=get_legend(g3), legend="bottom")


overall_damage <- sum(storm_data$PROPDMG) /1000000000

top_damage <-   as.data.table( storm_data %>%
    group_by(EVTYPE) %>%
    summarise(Total_Damage = sum(PROPDMG)/1000000000)  %>%
    top_n( n=10, Total_Damage )   %>%
    mutate(Total_Damage=round(Total_Damage,1)) )

top_damage[,contribuition:=round((Total_Damage/overall_damage)*100,1)]

events <- union(events, setdiff(top_damage$EVTYPE, events )  )  
color_events <- hcl.colors(length(events), palette = "Set3" )
names(color_events)  <- events

max_damage = max(top_damage$Total_Damage)

g4 <- top_damage %>%  
    ggplot(aes(y = Total_Damage, x = reorder(x = EVTYPE, X = Total_Damage), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = FALSE)   + 
    ylim(0,max_damage*1.2) +
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Damage+max_damage*0.15), 
              colour="black") +
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Damage by Event (Billions)",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal()

g4
