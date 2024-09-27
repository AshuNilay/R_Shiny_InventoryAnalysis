server <- function(input, output, session) {
  
###################################################################################
  output$selected_var <- renderText({ 
    paste("Table of Inventory aging more then 2 years and inwarded in", input$y2, "month") })
  
  observe({ 
    val1 <- Masterlist2[which(Masterlist2$Month == input$y1),2]
    val2 <- Masterlist2[which(Masterlist2$Month == input$y1),3]
    val3 <- Masterlist2[which(Masterlist2$Month == input$y1),4]
    val4 <- Masterlist2[which(Masterlist2$Month == input$y1),5]              
    val5 <- Masterlist2[which(Masterlist2$Month == input$y1),6]
    val6 <- Masterlist2[which(Masterlist2$Month == input$y1),7]
    val7 <- Masterlist2[which(Masterlist2$Month == input$y1),8]
    val8 <- Masterlist2[which(Masterlist2$Month == input$y1),9]
    
    output$Box1 <- renderValueBox({
      valueBox(paste0(val1)," Cr.", subtitle = h4("Total Inventory Value"), icon = icon("fas fa-rupee-sign"), color = "orange")
    })
    output$Box2 <- renderValueBox({
      valueBox(paste0(val2)," Cr.",subtitle = h4("Inventory Value(0-30 Days)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box3 <- renderValueBox({
      valueBox(paste0(val3)," Cr.",subtitle = h4("Inventory Value(30-60 Days)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box4 <- renderValueBox({
      valueBox(paste0(val4)," Cr.",subtitle = h4("Inventory Value(60-90 Days)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box5 <- renderValueBox({
      valueBox(paste0(val5)," Cr.",subtitle = h4("Inventory Value(90-180 Days)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box6 <- renderValueBox({
      valueBox(paste0(val6)," Cr.",subtitle = h4("Inventory Value(180-365 Days)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box7 <-renderValueBox({
      valueBox(paste0(val7)," Cr.",subtitle = h4("Inventory Value(1-2 Years)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    output$Box8 <- renderValueBox({
      valueBox(paste0(val8)," Cr.",subtitle = h4("Inventory Value(More than 2 Years)"), icon = icon("fas fa-rupee-sign"), color = "aqua")
    })
    
  })

###########################################
#scale_fill_manual(c("seagreen","aquamarine4","red","blue","yellow","black","orange"))


output$plot_dash1_1 <- renderPlot({
  ggplot(list1_Pune,aes(x=factor(Month,levels =c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")), y = Total_Value)) + geom_bar(stat = "identity", position = "dodge", fill = "steelblue", width = 0.3) +
    labs(x= "Month - FY (2018-19) ", y= "Total_value (Cr Rupees)") + theme_classic() + geom_text(aes(label=Total_Value), vjust=-0.3, size=4.5) 

})
output$plot_dash1_2 <- renderPlot({
  ggplot(list2_Pune,aes(x=factor(Month,levels =c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")), y = Total_Value,fill = as.factor(`Val. Class`))) + geom_bar(stat = "identity", position = "dodge") +
  labs(x= "Month - FY (2018-19) ", y= "Total_value (Cr Rupees)", fill = "Value Class") + theme_classic() +ylim(0,700) + geom_text(aes(label= Total_Value),position = position_dodge(0.9),vjust = -0.4,size = 2.5)+
  #scale_fill_brewer(palette="Set1") +  scale_color_manual(labels = c("Bought Out", "Imports","WIP-Inhouse","Masop Parts","In-Transit","IFLT","SFLT"))
  scale_fill_manual(values=c("yellow4", "#E69F00", "#56B4E9","brown1","darkolivegreen4","burlywood3","green"), 
                    name="Valuation Class",
                    breaks=c(110,120,230,240,250,530,550),
                    labels=c("Bought Out", "Imports","WIP-Inhouse","Masop Parts","In-Transit","IFLT","SFLT"))
})
output$plot_dash1_3 <- renderPlot({
  ggplot(list2_Pune,aes(x=factor(Month,levels =c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")), y = Total_Stock,fill = as.factor(`Val. Class`))) + geom_bar(stat = "identity", position = "dodge") +
  labs(x= "Month - FY (2018-19) ", y= "Total_Stock(Lakhs Units)", fill = "Value Class") + theme_classic() + geom_text(aes(label= Total_Stock),position = position_dodge(0.9),vjust = -0.4,size = 2.5)+ 
    scale_fill_manual(values=c("yellow4", "#E69F00", "#56B4E9","brown1","darkolivegreen4","burlywood3","green"), 
                      name="Valuation Class",
                      breaks=c(110,120,230,240,250,530,550),
                      labels=c("Bought Out", "Imports","WIP-Inhouse","Masop Parts","In-Transit","IFLT","SFLT"))

  })
output$plot_dash1_4 <- renderPlot({
ggplot(list2_Pune,aes(x=factor(Month,levels =c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")), y = Total_Parts,fill = as.factor(`Val. Class`))) + geom_bar(stat = "identity", position = "dodge") +
  labs(x= "Month - FY (2018-19) ", y= "Total_Unique_Parts(Units)", fill = "Value Class") + theme_classic() + geom_text(aes(label= Total_Parts),position = position_dodge(0.9),vjust = -0.4,size = 2.5)+ 
  scale_fill_manual(values=c("yellow4", "#E69F00", "#56B4E9","brown1","darkolivegreen4","burlywood3","green"), 
                      name="Valuation Class",
                      breaks=c(110,120,230,240,250,530,550),
                      labels=c("Bought Out", "Imports","WIP-Inhouse","Masop Parts","In-Transit","IFLT","SFLT"))
  })

###################################################################################################
df1<- reactive({
  g %>% filter(Month == input$y1)
}) 

output$plot_dash4_3_1 <- renderPlot({
ggplot(df1(),aes(x= factor(Value_Class), y = Total_Value))+geom_bar(stat = "identity",position = "dodge", fill="lightgreen") +
    labs(x = "Value_classification", y = "Total_Value") + theme_classic() + geom_text(aes(label=Total_Value), vjust=-0.3, size=4.5)+ 
    scale_x_discrete(limits=c("110","120","230",240,250,530,550),labels=c("Bought Out", "Imports","WIP-Inhouse","Masop Parts","In-Transit","IFLT","SFLT"))
})

#################################################################################################3
output$plot_dash4_3_2 <- renderPlotly({
  
  plot_ly(df1(), labels = ~Value_Class, values = ~Total_Value, type = 'pie') %>%
    add_trace(y = ~110, name = 'Bought Out') %>%
    add_trace(y = ~120, name = 'Imports') %>%
    add_trace(y = ~230, name = 'WIP') %>%
    add_trace(y = ~240, name = 'Massop Parts') %>%
    add_trace(y = ~250, name = 'In-Transit') %>%
    add_trace(y = ~530, name = 'IFLT') %>%
    add_trace(y = ~550, name = 'SFLT')
})

#####################################################################
Inv_Value <- reactive({
  Masterlist2 %>% gather(Inventory.Age,Inventory.Value,3:9) %>% filter(Month == input$y1) 
})


output$plot_dash4_4_1 <- renderPlot({
 ggplot(data=Inv_Value(), aes(x=Inventory.Age, y=Inventory.Value,level = Inventory.Age)) +
    geom_bar(position = "dodge",stat="identity",fill = "purple")+labs(x = "Inventory Aging", y = "Inventory Value") +
    geom_text(aes(label=round(Inventory.Value,digits = 2)), vjust=-0.3, size=4.5) +theme_classic() +
    scale_x_discrete(limits=c("Days30","Days30_60","Days60_90","Days90_180","Days180_365","Year1_2","Year2More"))
})
##########################################################################################
output$plot_dash4_4_2 <- renderPlotly({
  
  plot_ly(Inv_Value(), labels = ~Inventory.Age, values = ~Inventory.Value, type = 'pie') 
  
})

##################################################

output$plot5 <- renderPlot({
ggplot(df1(),aes(x= factor(Value_Class, levels =c(110,120,230,240,250,530,550)), y = Total_Value))+geom_bar(stat = "identity",position = "dodge", fill="lightgreen") +
    labs(x = "Value_classification", y = "Total_Value") + theme_classic() + geom_text(aes(label=Total_Value), vjust=-0.3, size=4.5)
  + scale_x_discrete(limits=c(110,120,230,240,250,530,550))})

###########################
Inv_Class0 <- reactive({
  Inv_Class %>% filter(Inv_Class$Month == input$y2)
})

Inv_Class1 <- reactive({ if (input$y3 == "All Classes")
{Inv_Class0()}
  else {Inv_Class0() %>% filter(`Val. Class`== input$y3)}
  
  })

output$table1 <- DT::renderDataTable(Inv_Class1(),server = FALSE,extensions = c('Buttons') ,class = 'cell-border stripe',
                                     rownames = FALSE, caption = 'All values are in INR Cr.',colnames = c('Ageing(0-30 Days)' = 11, 'Ageing(>2 Years)' = 12),
                                     style = 'bootstrap',options = list(dom = 'Bfrtip',buttons = c('copy', 'pdf', 'print'),orderClasses = TRUE
))

output$D_Data <- downloadHandler(
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(Inv_Class, file)
  })


#############################HighLights################################33
dfg <- reactive({
  Master %>% filter(Master$Month==input$y4)
})

#output$html1 <- renderUI({
 # HTML(paste0(h4("1. The dataset has ", nrow(dfg()),"oservations.<br>
  #            2. The dataset has ", max(Oct$Rate),"observations.")))
# })

output$table4 <- renderTable({g})


################################################Data Accuracy##############################################
output$table_2 <- DT::renderDataTable({
  DT::datatable(Oct_Rate,extensions = c('Buttons'), options = list(dom = 'Bfrtip',lengthMenu = c(5,10), pageLength = 5, buttons = c('copy', 'pdf', 'print')))})
  
output$n <- renderUI({
  HTML(paste0(" # There are total ", " <b>",  nrow(dfg()),"</b>", "Records with Zero Rate having Stock value more then zero <br>", 
              " # Total Value of such records is ", " <b>", sum(dfg()$`Total Value`),"Rs.", "</b>"))})

##########################
output$n1 <- renderUI({
  HTML(paste0(" # There are total ", " <b>",  length(which(Mis_Mar$mis_Value != Mis_Mar$`Total Value`)),"</b>", "Records with Zero Rate having Stock value more then zero <br>", 
              " # Total Value of such records is ", " <b>", sum(Mis_Mar$`Total Value`),"Rs.", "</b>",
              " # There are", length(which(Mis_Mar$mis_Value > 1)), "Records", "for which difference is more than 1 Rs." ,
              " # Total value of mismatch is ", sum(Mis_Mar$mis_Value), "Rs.",
              " # Maximum Value of Mismatch is", max(Mis_Mar$mis_Value), "Rs," ))
       })

###@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@2
Miss_value1 <- reactive({ Miss_value %>% filter(Miss_value$Month == input$y4)})

output$table_5 <- DT::renderDataTable({
  DT::datatable(Miss_value1(),extensions = c('Buttons'), options = list(dom = 'Bfrtip',lengthMenu = c(5,10), pageLength = 10, buttons = c('copy', 'pdf', 'print')))})


#################################################
output$q1 <- renderUI({
  HTML(paste0(" # Inventory of INR 52K was last consumed before 2002; still showing in System.<br>",
                "# Inventory of INR 4.82 Lacs was last consumed in  2003-2005.<br>",
                " # Inventory of INR 20.46 Lacs was last consumed in  2006-2008.<br>",
                " # Inventory of INR 4.82 Cr was last consumed in  2009-2014). "))})

output$q2 <- renderUI({
  HTML(paste0("# MASOP parts inventory value is of INR 4.38 Cr (More than 1 year).<br>",
              "# MASOP parts inventory value is of INR 1.10 Cr (6 Months to 1 year)" ))})

##################################################################################################################333

output$plot5 <- renderPlot({
  ggplotly(ggplot(df1, aes(x=Product_Segment, y=Inventory_Cr)) +
  geom_bar(position = "dodge",stat="identity",fill = "purple")+
  geom_text(aes(label=Inventory_Cr), vjust=-0.3, size=4.5) +theme_classic()) })

output$plot6 <- renderPlot({
  ggplotly(ggplot(g,aes(x= factor(Value_Class), y = Total_Value))+geom_bar(stat = "identity",position = "dodge", fill="lightgreen") +
    labs(x = "Value_classification", y = "Total_Value") + theme_classic() + geom_text(aes(label=Total_Value), vjust=-0.3, size=4.5))
})


output$table_4 <- DT::renderDataTable({
  DT::datatable(Master_Rate,extensions = c('Buttons'), options = list(dom = 'Bfrtip',lengthMenu = c(5,10), pageLength = 10, buttons = c('copy', 'pdf', 'print')))})

output$table_3 <- DT::renderDataTable({
  DT::datatable(Master_Rate,extensions = c('Buttons'), options = list(dom = 'Bfrtip',lengthMenu = c(5,10), pageLength = 10, buttons = c('copy', 'pdf', 'print')))})
##########################################################Master Data #######################33333
output$plot_DA1_31<- renderPlot({
  ggplot(data=Inv_Value(), aes(x=Inventory.Age, y=Inventory.Value,level = Inventory.Age)) +
    geom_bar(position = "dodge",stat="identity",fill = "purple")+labs(x = "Inventory Aging", y = "Inventory Value") +
    geom_text(aes(label=round(Inventory.Value,digits = 2)), vjust=-0.3, size=4.5) +theme_classic() +
    scale_x_discrete(limits=c("Days30","Days30_60","Days60_90","Days90_180","Days180_365","Year1_2","Year2More"))
})
output$plot_DA1_32<- renderPlot({
  ggplot(data=Inv_Value(), aes(x=Inventory.Age, y=Inventory.Value,level = Inventory.Age)) +
    geom_bar(position = "dodge",stat="identity",fill = "purple")+labs(x = "Inventory Aging", y = "Inventory Value") +
    geom_text(aes(label=round(Inventory.Value,digits = 2)), vjust=-0.3, size=4.5) +theme_classic() +
    scale_x_discrete(limits=c("Days30","Days30_60","Days60_90","Days90_180","Days180_365","Year1_2","Year2More"))
})



}



