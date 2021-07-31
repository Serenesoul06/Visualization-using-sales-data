library(dplyr)
library(ggplot2)
library(plotly)
library(plotrix)

getwd()
setwd("D:/GITHUB/R/Visualization using sales data")
Sales=read.csv("Sales data.csv")
View(Sales)

# 1.	Compare Sales by region for 2016 with 2015 using bar chart

Sales_by_region=group_by(Sales, Region) %>%
  summarise(SALES2015=sum(Sales2015), SALES2016=sum(Sales2016))

fig=plot_ly(Sales_by_region, x=~Region, y=~SALES2015, type='bar',name="Sales 2015",
            text =~SALES2015, textposition = 'inside',
            hovertemplate=paste('<i><b>Sales 2015<b></i>: $%{y:.2f}',
                                '<br><b>Region</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  add_trace(y=~SALES2016, name="Sales 2016",
            text =~SALES2016, textposition = 'inside',
            hovertemplate=paste('<i><b>Sales 2016<b></i>: $%{y:.2f}',
                                '<br><b>Region</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  layout(title="Sales by Region for 2016 v/s 2015",
         yaxis=list(title="Sales"),
         xaxis=list(title="Region"),
         barmode="group",
         showlegend = T, 
         legend= list(x=100, y=0.5))

fig

# 2.	Pie charts for sales for each region in 2016 

Regionwise_sales_2016=group_by(Sales, Region) %>%
  summarise(SALES2016=sum(Sales2016))

piechart <- plot_ly(type='pie', labels=Regionwise_sales_2016$Region, values=Regionwise_sales_2016$SALES2016, 
               textinfo='label+percent',
               insidetextorientation='h') %>% #'h' for horizontal and 'radial' for round text rotation
  layout(title="Pie chart of Regionwise Sales in 2016")        
piechart

#3D pie chart

lbls=paste(Regionwise_sales_2016$Region, "\n",round(((Regionwise_sales_2016$SALES2016)/sum(Regionwise_sales_2016$SALES2016))*100,digits=2), "%")
#paste function is used to concatenate vectors as strings.
pie3D(Regionwise_sales_2016$SALES2016, labels=lbls, col=c("aquamarine","sky blue","pink"), explode=0.2, 
      main="Pie chart of Regionwise Sales in 2016")

# 3.Compare sales of 2015 and 2016 with Region and Tiers.

Region_and_tierwise_sales=group_by(Sales,Region,Tier) %>%
  summarise(SALES2015=sum(Sales2015), SALES2016=sum(Sales2016))

Region=rep(Region_and_tierwise_sales$Region,2)
Tier=rep(Region_and_tierwise_sales$Tier,2)
Sales=c(Region_and_tierwise_sales$SALES2015, Region_and_tierwise_sales$SALES2016)
Year=c(rep("2015",12), rep("2016",12))
g3=data.frame(Region, Tier, Sales, Year)
View(g3)
g4=ggplot(g3, aes(Tier,Sales))
options(scipen=999)
g5=g4+geom_bar(stat = "identity", aes(fill = Year), position = "dodge")+
  facet_wrap(~Region, ncol = 3)+ggtitle("Regionwise & Tierwise Sales in 2015 v/s 2016")
g6=ggplotly(g5)
g6


# 4.In East region, which state registered a decline in 2016 as compared to 2015? 
Sales=read.csv("Sales data.csv")
East_region_sales=Sales[Sales$Region=="East",]

Sales_by_state=group_by(East_region_sales, State) %>%
  summarise(SALES2015=sum(Sales2015), SALES2016=sum(Sales2016))

fig1=plot_ly(Sales_by_state, x=~State, y=~SALES2015, type='bar',name="Sales 2015",
            hovertemplate=paste('<i><b>Sales 2015<b></i>: $%{y:.2f}',
                                '<br><b>State</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  add_trace(y=~SALES2016, name="Sales 2016",
            hovertemplate=paste('<i><b>Sales 2016<b></i>: $%{y:.2f}',
                                '<br><b>State</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  layout(title="East region Sales by State for 2016 v/s 2015",
         yaxis=list(title="Sales"),
         xaxis=list(title="State"),
         barmode="group",
         showlegend = T, 
         legend= list(title=list(text='<b> Year </b>'),x=100, y=0.5),
         annotations=list(x=1, y=-0.1,
                          text="Comment: In East region, NY registered a decline in Sales in 2016",
                          font = list(size = 12),
                          showarrow = FALSE,
                          xref = 'paper', 
                          yref = 'paper', 
                          xanchor='right', yanchor='auto',
                          xshift=0, yshift=0)
  )

fig1

# 5.In all the High tier, which Division saw a decline in number of units sold in 2016
# compared to 2015?  

High_tier_units=Sales[Sales$Tier=="High",]

Units_by_division=group_by(High_tier_units, Division) %>%
  summarise(Units2015=sum(Units2015), Units2016=sum(Units2016))

fig2=plot_ly(Units_by_division, x=~Division, y=~Units2015, type='bar',name="Units 2015",
             hovertemplate=paste('<i><b>Units 2015<b></i>: $%{y:.2f}',
                                 '<br><b>Division</b>: %{x}<br>',
                                 "<extra></extra>")) %>%
  add_trace(y=~Units2016, name="Units 2016",
            hovertemplate=paste('<i><b>Units 2016<b></i>: $%{y:.2f}',
                                '<br><b>Division</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  layout(title="High tier Units by Division for 2016 v/s 2015",
         yaxis=list(title="Units"),
         xaxis=list(title="Division"),
         barmode="group",
         showlegend = T, 
         legend= list(title=list(text='<b> Year </b>'),x=100, y=0.5)
  )

fig2

# 6.Create a new column Qtr 
# .	Jan - Mar : Q1
# .	Apr - Jun : Q2
# .	Jul - Sep : Q3
# .	Oct - Dec : Q4

Sales$QTR=ifelse(Sales$Month %in% c("Jan","Feb","Mar"),"Q1", 
               ifelse(Sales$Month %in% c("Apr","May","Jun"),"Q2",
                      ifelse(Sales$Month %in% c("Jul","Aug","Sep"),"Q3",
                             ifelse(Sales$Month %in% c("Oct","Nov","Dec"),"Q4","NULL"))))
View(Sales)

# 7.Compare Qtr wise sales in 2015 and 2016 in a bar plot  

Qtrwise_sales=group_by(Sales, QTR) %>% 
  dplyr::summarise(SALES2015=sum(Sales2015), SALES2016=sum(Sales2016))
  
fig7=plot_ly(Qtrwise_sales, x=~QTR, y=~SALES2015, type='bar',name="Sales 2015",
             hovertemplate=paste('<i><b>Sales 2015<b></i>: $%{y:.2f}',
                                 '<br><b>Quarter</b>: %{x}<br>',
                                 "<extra></extra>")) %>%
  add_trace(y=~SALES2016, name="Sales 2016",
            hovertemplate=paste('<i><b>Sales 2016<b></i>: $%{y:.2f}',
                                '<br><b>Quarter</b>: %{x}<br>',
                                "<extra></extra>")) %>%
  layout(title="Quarter wise Sales for 2016 v/s 2015",
         yaxis=list(title="Sales"),
         xaxis=list(title="Quarter"),
         barmode="group",
         showlegend = T, 
         legend= list(title=list(text='<b> Year </b>'),x=100, y=0.5)
  )

fig7

# 8.Determine the composition of Qtr wise sales in 2015 with regards to all the Tiers in a pie chart.
# (Draw 4 pie charts representing a Quarter for each Tier)

Tier_and_qtrwise_sales=group_by(Sales,Tier, QTR) %>% 
  dplyr::summarise(SALES2015=sum(Sales2015))

Q1=Tier_and_qtrwise_sales[Tier_and_qtrwise_sales$QTR=="Q1",]
Q2=Tier_and_qtrwise_sales[Tier_and_qtrwise_sales$QTR=="Q2",]
Q3=Tier_and_qtrwise_sales[Tier_and_qtrwise_sales$QTR=="Q3",]
Q4=Tier_and_qtrwise_sales[Tier_and_qtrwise_sales$QTR=="Q4",]
pied <- plot_ly() %>%
  add_pie(data=Q1,labels=Q1$Tier, values=Q1$SALES2015,
                textinfo='label+percent',
                insidetextorientation='h',
                domain = list(row = 0, column = 0),
          name="QTR 1") %>%
  add_pie(data=Q2,labels=Q2$Tier, values=Q2$SALES2015,
          textinfo='label+percent',
          insidetextorientation='h',
          domain = list(row = 0, column = 1),
          name="QTR 2") %>%
  add_pie(data=Q3,labels=Q3$Tier, values=Q3$SALES2015,
          textinfo='label+percent',
          insidetextorientation='h',
          domain = list(row = 1, column = 0),
          name="QTR 3") %>%
  add_pie(data=Q4,labels=Q4$Tier, values=Q4$SALES2015,
          textinfo='label+percent',
          insidetextorientation='h',
          domain = list(row = 1, column = 1),
          name="QTR 4") %>%
  layout(title="Pie chart of Tier and Quarter-wise Sales in 2015",
         grid=list(rows=2, columns=2)) 
pied
