---
output:
  html_document:
    keep_md: yes
---

Storm Analysis - Analysis of impact of different weather events in the USA based on the NOAA Storm Database
------------------------------------------------------------------------------------------------------------
#### By Eduardo Godoy 



### Assignment  
The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, figures, or other summaries. You may use any R package you want to support your analysis.  
  
### Synopsis  
Storms and other severe weather events can cause both public health and economic problems for communities and municipalities. Many severe events can result in fatalities, injuries, and property damage, and preventing such outcomes to the extent possible is a key concern.  
  
This project involves exploring the U.S. National Oceanic and Atmospheric Administration's (NOAA) storm database. This database tracks characteristics of major storms and weather events in the United States, including when and where they occur, as well as estimates of any fatalities, injuries, and property damage.  

### Data  
The data for this assignment come in the form of a comma-separated-value file compressed via the bzip2 algorithm to reduce its size. You can download the file from the course web site:[Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) [47Mb]  
  
There is also some documentation of the database available. Here you will find how some of the variables are constructed/defined.  

* [National Weather Service Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)    
* [National Climatic Data Center Storm Events FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf)  
  
The events in the database start in the year 1950 and end in November 2011. In the earlier years of the database there are generally fewer events recorded, most likely due to a lack of good records. More recent years should be considered   more complete.  
&nbsp;
  
### Data Processing  
  
#### Load the libraries  
Load all the necessaries libraries for script's execution  
  

```r
library(data.table)
library(dplyr)
library(ggplot2)
library(R.utils)
library(xtable)
library(ggpubr)
library(RColorBrewer)
```
&nbsp;
  
#### Data download
Download the data in a local folder to load it later  
  

Setup variables

```r
sub_dir <- "project_data"
output_dir <- file.path(getwd(), sub_dir)
file_data <- file.path(getwd(), sub_dir, "StormData.csv")
```
  
Only download if the file doesn't exists

```r
if (!file.exists(file_data)){
    # Check if the folder exists
    if (!dir.exists(output_dir)){
        dir.create(output_dir) # Create the folder
    }
    url_target <- "https://d396qusza40orc.cloudfront.net/repdata/data/StormData.csv.bz2"
    bzip_file <- file.path( output_dir, "raw_data.bz2")
    download.file(url_target, bzip_file)
    
    bunzip2(filename=bzip_file,destname=file_data, overwrite=TRUE, remove=FALSE)
}
```
&nbsp;
  
#### Load and Data Preparation

Read only the fields that will be needed

```r
fields_needed <- c("BGN_DATE", "EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
storm_data <- fread( file_data, select = fields_needed )
```
  
Fix the field with Proprerty Damage, according to its scale defined in the field PROPDMGEXP:  
* "K" = 1.000  
* "M" = 1.000.000  
* "B" = 1.000.000.000  
  
If the field is null the script won't change the Property Damage  

```r
storm_data[PROPDMGEXP %in% c("K","k"), PROPDMG:= PROPDMG * 1000 ] # K = Thousands . Multiply by 1.000
storm_data[PROPDMGEXP %in% c("M","m"), PROPDMG:= PROPDMG * 1000000 ] # M = Millions. Multiply by 1.000.000
storm_data[PROPDMGEXP %in% c("B","b"), PROPDMG:= PROPDMG * 1000000000 ] # B = Billions. Multiply by 1.000.000.000
```
&nbsp;
  
### Which types of events are most harmful to population health?
The script will generate two plots in order to answer this question:  
* The top 10 most harmful types of events in terms of deaths  
* The top 10 most harmful types of events in terms of injuries  
&nbsp;  
  
#### Data preparation 
Save the total fatalities and injuries. We'll need it later to calculation the contribuition of each type to the total.
  

```r
overall_fatalities <- sum(storm_data$FATALITIES)    
overall_injuries <- sum(storm_data$INJURIES)  
```

Calculate in a different data table the total injuries and deaths per event type  

```r
harm_per_event <-   storm_data %>%  
                    group_by(EVTYPE)  %>% 
                    summarise(Total_Fatalities = sum(FATALITIES, na.rm=TRUE), 
                              Total_Injuries = sum(INJURIES, na.rm=TRUE))
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

We need to create one top 10 data table for injuries and other for deaths. The top 10 can be diferent 

```r
top_injuries <-  as.data.table(top_n(harm_per_event, n=10, Total_Injuries)) 
top_fatalities <-  as.data.table(top_n(harm_per_event, n=10, Total_Fatalities)) 
```

Calculate the Top 10 distribuiton according to the total overall

```r
top_injuries[,contribuition:=round((Total_Injuries/overall_injuries)*100,1)]
top_fatalities[,contribuition:=round((Total_Fatalities/overall_fatalities)*100,1)]
```

Even though we'll have two different plots, we need to keep the same colors for the event types.   

```r
events <- union( top_injuries$EVTYPE, top_fatalities$EVTYPE) # The result will be a unique list of type of events
color_events <- hcl.colors(length(events), palette = "Set3" ) # Get the total colors need from the palette
names(color_events)  <- events # Relate the colors to the event types
```
&nbsp;  
  
#### Results  
  
We need the max fatalities and injures to calculate the contribuition label position  
Put 20% extra space in the plot size (y limit)
This max will also help in the calculation of the label position:  

```r
max_fatalities = max(top_fatalities$Total_Fatalities)
max_injuries = max(top_injuries$Total_Injuries)
```
  
Generate the total contribuition to be used later  

```r
tot_contribuition_fatalities <- paste(sum(top_fatalities$contribuition), "%")
tot_contribuition_injuries <- paste(sum(top_injuries$contribuition), "%")
```
  
Generate the first plog: Fatalities per Event Type

```r
g1 <- top_fatalities %>%  
    ggplot(aes(y = Total_Fatalities, 
               x = reorder(x = EVTYPE, X = Total_Fatalities), 
               fill=EVTYPE)) +
    geom_bar(stat = "identity", 
             show.legend = FALSE) + 
    ylim(0,max_fatalities*1.2) + # Create an extra space for the contribuition label
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Fatalities+max_fatalities*0.15), # Put the label after the y (15% of the max)
                  size = 3) +
    coord_flip() + # Rotate the plot
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Fatalities by Event",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(size = 7),
          axis.text.y = element_text(size=8))
```
  
Generate the secondt plog: Injuries per Event Type

```r
g2 <- top_injuries %>%  
    ggplot(aes(y = Total_Injuries,
               x = reorder(x = EVTYPE, X = Total_Injuries), 
               fill=EVTYPE)) +
    geom_bar(stat = "identity", 
             show.legend = FALSE ) + 
    ylim(0,max_injuries*1.2) + # Create an extra space for the contribuition label
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Injuries+max_injuries*0.15), # Put the label after the y (15% of the max),
                  size = 3 ) +
    coord_flip() + # Rotate the plot
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Injuries by Event",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(size = 7),
          axis.text.y = element_text(size=8))
```
  
We need to create a third plot just to get the legend combined in the fatalities and deaths

```r
g3 <- as.data.table(union(top_injuries, top_fatalities)) %>% 
    ggplot(aes(y = Total_Injuries, x = reorder(x = EVTYPE, X = Total_Injuries), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = TRUE )   + 
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    theme_minimal() + 
    theme(legend.position = "bottom",
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 6) )
```
  
Print the final plot

```r
ggarrange(g1, g2, 
          common.legend = TRUE, 
          legend.grob=get_legend(g3), 
          legend="bottom",
          widths = c(1, 1))
```

<img src="Storm_Analysis_files/figure-html/unnamed-chunk-16-1.png" width="100%" />
&nbsp;  
   
#### Conclusion   
The top 10 results are the major issues for the public health:    
* The top 10 fatalities are responsable per 79.8 % of the total fatalities  
* The top 10 injuries are responsable per 89.4 % of the total fatalities  
  
&nbsp;  
   
### Which types of events have the greatest economic consequences?
The script will generate a plot with the top 10 worst event type in order to answer this question:  
&nbsp;  
  
#### Data preparation 
Calculate the overall_damage (in Billions)

```r
overall_damage <- sum(storm_data$PROPDMG) /1000000000
```
  
Generate the top 10 worst type

```r
top_damage <-   as.data.table( storm_data %>%
    group_by(EVTYPE) %>%
    summarise(Total_Damage = sum(PROPDMG)/1000000000)  %>%
    top_n( n=10, Total_Damage )   %>%
    mutate(Total_Damage=round(Total_Damage,1)) )
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```
  
Calculate the contribuition against the overall

```r
top_damage[,contribuition:=round((Total_Damage/overall_damage)*100,1)]
```
    
The plot needs to use the same legend before (same colors for the same types) and add the new types

```r
events <- union(events, setdiff(top_damage$EVTYPE, events )  )  
color_events <- hcl.colors(length(events), palette = "Set3" )
names(color_events)  <- events
```

We need the max damage to calculate the contribuition label position  
Put 20% extra space in the plot size (y limit)
This max will also help in the calculation of the label position:  


```r
max_damage = max(top_damage$Total_Damage)
```
&nbsp;  
  
#### Results  
  

```r
top_damage %>%  
    ggplot(aes(y = Total_Damage, x = reorder(x = EVTYPE, X = Total_Damage), fill=EVTYPE))+
    geom_bar(stat = "identity", show.legend = FALSE)   + 
    ylim(0,max_damage*1.2) +
    geom_text(aes(label=paste(contribuition,"%"), 
                  x=EVTYPE, 
                  y=Total_Damage+max_damage*0.15), 
              size=3) +
    coord_flip() +
    scale_fill_manual("Legend", values = color_events) +
    labs(
        title = "Total Damage by Event (Billions)",
        caption = "*Percentage against overall",
        x = NULL,
        y = NULL
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(size = 7),
          axis.text.y = element_text(size=8))
```

![](Storm_Analysis_files/figure-html/unnamed-chunk-22-1.png)<!-- -->
  
#### Conclusion  
The order of the worst types of event in economical terms is different of the harm impact 



