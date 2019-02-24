viz<-df_cdc_1
library(plotly)
library(tmap)
library(tmaptools)
packageVersion('plotly')
coords<-read_csv("zip.csv")

## Basic barcharts and histograms (Dependent variable)
p <- plot_ly(x = df_cdc_1$ment_cont,
             type = "histogram")

p

### By state (prevelance of mental illness)
p1d<-df_cdc_2%>%
  filter(mental_14=="1")%>%
  select("state","mental_14")%>%
  group_by(state)%>%
  summarise(no_rows=length(state))

p2d<-df_cdc_2%>%
  filter(mental_14=="0")%>%
  select("state","mental_14")%>%
  group_by(state)%>%
  summarise(no_rows=length(state))

p3d<-merge(p1d,p2d,by="state")%>%
  mutate(prev=no_rows.x/no_rows.y)%>%
  filter(prev>0.13)

p1 <- plot_ly(
  x = p3d$state,
  y = p3d$prev,
  name = "Percentage prevalance of mental illnesses by US States & Territories",
  type = "bar"
)

p1

## Basic chloropleths
p3<-merge(p1d,p2d,by="state")%>%
  mutate(prev=no_rows.x/no_rows.y)
names(p3)[names(p3)=="state"]<-'State'

mergep3<-merge(p3,coords,by="State")

## GIS america shape file 
# give state boundaries a white border

# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(df, locationmode = 'USA-states') %>%
  add_trace(
    z = ~prev, text = ~hover, locations=~State,
    color = ~prev, colors = 'Red')%>%
  colorbar(title = "% proportion of survey respondents with mental issues") %>%
  layout('No of mental breakdowns in the last 30 days<br>(Hoover for breakdown)',
    geo = g
  

    