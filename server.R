library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
zipdata <- allzips
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
zipdata <- zipdata[order(zipdata$centile),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  
  output$perpetratorvsvictimAgevsweapon <- renderPlot({
    ggplot(ndf.by.weapon.geom.points,aes(x=victim.age,y=perpetrator.age))+
      geom_point(stat="identity", col="steelblue",size=0.5)+
      facet_wrap(~ sex.weapon.used)+
      geom_smooth(method="glm", size=0.6, col="darkred")+
      homi.theme+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))+
      theme(strip.text = element_text(size=8,color ="darkblue"))+
      labs(x="Victim Age",
           y="Perpetrator Age")
  })
  
  output$victimRacevsperpetrator <- renderPlot({
    gg.by.race <- ggplot(top4.by.race,aes(x=who.killed.who.race,y=total.by.race))+
      geom_bar(stat="identity",fill="darkred",width=0.5)+
      homi.theme+
      labs(x="Race",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    
    grid.arrange(gg.by.race,
                 table.by.race,ncol=2)
    
  })
  
  output$femalevsmalevsweapon <- renderPlot({
    by.ndf.by.weapon.used$sex.weapon.used <- fct_inorder(by.ndf.by.weapon.used$sex.weapon.used )
    
    plot.gender.weapon.used.T20 <- ggplot(by.ndf.by.weapon.used[1:20,],aes(x = sex.weapon.used,y=freq.by.weapon.used))+
      geom_bar(stat="identity",fill="darkred",width=0.5)+
      homi.theme+
      labs(x="Gender and Weapon Used",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    plot.gender.weapon.used.T20
    
  })
  
  output$weaponUsed <- renderPlot({
    Plot.weapon.used <- ggplot(by.weapon,aes(x = weapon,y=freq.by.weapon))+
      geom_bar(stat = "identity",fill="red",width = 0.5)+
      homi.theme+
      labs(x="Weapon",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    Plot.weapon.used
    
  })
  
  output$femalevsmalekilledby <- renderPlot({
    plot.Weapon.vs.gender <- ggplot(by.weapon.sex,aes(x = weapon,y=freq.by.weapon))+
      geom_bar(stat = "identity",fill="red",width=0.5)+
      facet_wrap(~ victim.sex)+
      homi.theme+
      labs(x="Weapon",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    plot.Weapon.vs.gender    
  })
  
  output$whokilledwho <- renderPlot({
    by.family$who.killed.who<- fct_inorder(by.family$who.killed.who)  
    
    plot.by.family <- ggplot(by.family,aes(x=who.killed.who, y=total.number.re ))+
      geom_bar(stat="identity", fill="darkred",width = 0.5)+
      homi.theme+
      labs(x="Family relationship",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    
    table.by.family <- tableGrob(by.family, rows=NULL,theme = tt.homi.f)
    
    grid.arrange(plot.by.family,
                 table.by.family,ncol=2)
    
  })
  
  output$friendsvssex <- renderPlot({
    ggplot(friend.crime, aes(x= who.killed.who.Friend.sex))+
      geom_bar(alpha=0.8,fill="steelblue", width=0.3)+
      homi.theme+
      labs(x="Who Killed Who?",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 12, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
  })
  
  output$employeevsemployer <- renderPlot({
    ggplot(employee.employer.crime ,aes(x=relationship,fill = relationship))+
      geom_bar(alpha=0.4,col="gold", width=0.4)+
      homi.theme+
      labs(x= "Relationship", y= "Number of incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
  })
  
  output$exhusbandvsexwife <- renderPlot({
    ex.h.vs.ex.w <- ggplot(ex.husband.ex.wife.crime ,aes(x=relationship,fill = relationship))+
      geom_bar(alpha=0.4,col="gold", width=0.4)+
      homi.theme+
      ggtitle("Victims \n Ex-Husband VS Ex-Wife")+labs(x= "Relationship",
                                                       y= "Number of incidents")+theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
                                                                                       axis.title.y=element_text(size=10),
                                                                                       axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    ex.h.vs.ex.w.age <- ggplot(ex.husband.ex.wife.crime, aes(x= older.or.younger))+geom_bar(alpha=0.7,fill="gold3", width=0.3)+
      homi.theme+
      ggtitle("Victims \n Ex-Husband VS Ex-Wife VS Age")+
      labs(x="Older and Yonger",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    grid.arrange(ex.h.vs.ex.w,
                 ex.h.vs.ex.w.age,ncol=2)
  })
  
  output$gfvsbf <- renderPlot({
    plot.gf.bf.vic<-ggplot(girl.boy.crime ,aes(x=victim.sex,fill=victim.sex))+
      geom_bar(alpha=0.4,col="gold", width=0.4)+
      homi.theme+
      ggtitle("Victims \n Girlfriend VS Boyfriend")+labs(x= "Gender",
                                                         y= "Number of incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    plot.gf.bf.pre<-ggplot(girl.boy.crime ,aes(x=perpetrator.sex, fill=perpetrator.sex))+
      geom_bar(alpha=0.4,col="gold", width = 0.4)+
      homi.theme+
      ggtitle("Perpetrators \n Girlfriend VS Boyfriend")+labs(x= "Gender",
                                                              y= "Number of incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    grid.arrange(plot.gf.bf.vic,
                 plot.gf.bf.pre,
                 ncol=2)
    
  })
  
  output$incidents <- renderPlot({
    plot.homic.years<-ggplot(data = by.year,
                             aes(x=as.numeric(year),
                                 y=freq.year))+
      geom_line(size=2,col="yellow3")+
      homi.theme+
      ggtitle("Number of incidents occurred per Year")+
      labs(x="Year",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    plot.homic.years.points<-ggplot(data = by.year,
                                    aes(x=as.factor(year),
                                        y=freq.year))+
      geom_point(size=1,col="blue")+
      homi.theme+
      ggtitle("Number of Incidents occurred per Year")+
      labs(x="Year",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    
    by.state$state <- fct_inorder(by.state$state)
    plot.by.state <- ggplot(data = by.state,
                            aes(x=as.factor(state),
                                y=freq.by.state))+
      geom_bar(stat= "identity", fill="darkred", width=0.5 )+
      homi.theme+
      ggtitle("Number of Incidents occurred per Year")+
      labs(x="State",
           y="Number of Incidents")+
      theme(axis.text.x=element_text(size= 6, angle=90,hjust = 0.5),
            axis.title.y=element_text(size=10),
            axis.title.x=element_text(size=10))+
      theme(plot.title = element_text(size = 10))
    
    grid.arrange(arrangeGrob(plot.homic.years,plot.homic.years.points,ncol=2),
                 plot.by.state)
  })
  
  output$incidentprediction <- renderPlot({
    new_data2 <- read.csv("data/database4.csv")
    
    
    summarise(new_data2)
    print(new_data2)
    
    
    
    library(mosaic)
    N =nrow(new_data2)
    plot(freq.year~year,data = new_data2)
    
    #creatye lenier model
    lm1 = lm(freq.year~year,data = new_data2)
    summary(lm1)
    #boot1 is run for like 1000 times
    boot1 = do(1000)*lm(freq.year~year,data = resample(new_data2))
    hist(boot1$year)
    sd(boot1$year)
    
    trainset = sample(1:N,20,replace = FALSE)
    new_data2.train = new_data2[trainset,]
    new_data2.test = new_data2[-trainset,]
    
    
    plot(freq.year~year,data = new_data2.train)
    lm1 = lm(freq.year~year,data = new_data2.train)
    summary(lm1)
    
    pred.test = predict(lm1, newdata = new_data2.test)
    #all models fall in same line because it is is a lenier regression
    plot(pred.test~year,data = new_data2.test)
    
    
    #next prediction
    pred2.test = predict(lm1, newdata = new_data2.test, interval = 'prediction', level = 0.95)
    head(pred2.test)
    head(pred.test)
    pred2.test[,2]
    
    
    plot(pred2.test[,1] ~year, data = new_data2.test,pch=19,ylim = range(pred2.test))
    points(pred2.test[,2] ~year, data = new_data2.test, pch=4)
    points(pred2.test[,3] ~year, data = new_data2.test,  pch=4)
    points(year ~ year, data = new_data2.test,  pch=19)
    points(freq.year ~ year, data = new_data2.test,  pch=19,col="red")
  })
  
  

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$color
    sizeBy <- input$size
    
    if (colorBy == "superzip") {
      # Color and palette are treated specially in the "superzip" case, because
      # the values are categorical instead of continuous.
      colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
      pal <- colorFactor("viridis", colorData)
    } else {
      colorData <- zipdata[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "superzip") {
      # Radius is treated specially in the "superzip" case.
      radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
    }
    
    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData))
  })
  

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Incident Count:", as.integer(selectedZip$centile*10)),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedZip$city.x, selectedZip$state.x
      ))), tags$br()
      
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })


  ## Data Explorer ###########################################

  output$homicideData <- DT::renderDataTable({
    
    DT::datatable(homi.r %>% 
                    ungroup() %>%
                    select(-record.id))
  })
}
