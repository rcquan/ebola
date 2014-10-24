# server.R

library(RCurl)
library(ggplot2)
library(stringr)
library(reshape2)
library(magrittr)
library(dplyr)
library(scales)
library(shiny)
library(foreign)
library(RColorBrewer)

url <- "https://raw.githubusercontent.com/cmrivers/ebola/master/country_timeseries.csv"

data <- getURL(url, ssl.verifypeer = FALSE)
df <- read.csv(textConnection(data))
#Drop the Date col
df1_noDate <- df[, !names(df) %in% c("Date")]
#Build a series from 0...latest day in data set
day <- c(0:max(df1_noDate$Day))
#We'll add updates on each day we have data for each country here
df3_merge <- data.frame(day)
#For each country:
for(country in 2:ncol(df1_noDate)){
  df_temp <- df1_noDate[, c(1, country)] #Day,(Cases|Deaths)_Country
  #Data set is snapshots at day of reporting, with NAs representing "no change"/"no new data"
  #so ignore those with NAs.
  df_temp <- na.omit(df_temp)

  #Rescale all series so day 0 == first reported case/death
  df_temp$day.adj <- df_temp$Day - min(df_temp$Day)

  df3_merge <- merge(x = df3_merge, y = df_temp[, names(df_temp) != "Day"],
                     by.x = "day", by.y = "day.adj", all.x = TRUE)
}

row.names(df3_merge) <- df3_merge$day
df3_merge <- df3_merge[, names(df3_merge) != "day"]

df4 <- as.data.frame(t(as.matrix(df3_merge)))

vars <- colsplit(row.names(df4), "_", c("type", "place"))
df4 <- cbind(vars, df4)
row.names(df4) <- NULL

df5_melt <- melt(df4)
names(df5_melt) <- c("type", "place", "day", "count")
df5_melt$type[df5_melt$type == "Case"] <- "Cases"


all <- unique(df5_melt$place)
c_colors <- brewer.pal(length(all), 'Set1')
names(c_colors) <- all

theme_set(theme_minimal())

shinyServer(function(input, output) {

  data_plot <- reactive({
    df_plot <- df5_melt[!is.na(df5_melt$count), ]
	selection <- input$countries
	if("All" %in% input$countries || length(input$countries) == 0 ){
		selection <- all
	}
   df_plot %>% 
	 filter(place %in% selection) %>%
     mutate(count = as.numeric(count), day=as.numeric(day))
  })
  
  output$countriesList <- renderUI({
    checkboxGroupInput("countries",
                       label = h3("Countries to display"),
                       choices = c(all, "All"),
                       selected = "All")
  })



  plot <- reactive({
    g <- ggplot(data = data_plot(),
                aes(x = day, y = count,
                    group = place, color = place)) +
                        geom_point() + geom_line()+
                            facet_grid(~ type) +
                                scale_x_continuous(name="Days after first report") +
                                    scale_y_continuous(name="Counts") +
                                        scale_colour_manual(name="Country", values=c_colors) +
                                          ggtitle("Number of observations for days after first report")

    if(!input$log){
      return(g)
    } else{
      h <- g + scale_y_continuous(trans=log10_trans()) +
          scale_y_log10(name="Counts") +
              ggtitle("Number of observations for days after first report (log10 scale)")
      return(h)
    }
  })

  output$plot <- renderPlot({
    print(plot())
  })
})
