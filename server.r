shinyServer(function(input, output, session) {
  
  output$plot <- renderPlot({
    if (input$selection=="hiv")  {
      title2 <- paste("What they tweeted on" , input$selection  )
      labelX <- paste( input$selection )
      graph2 <- ggplot(total.nHIV,aes(tweetMY,hivFund.count,fill=hivFund))+
        geom_bar(stat="identity",position="dodge") + ylab("Mentioned") + xlab("Month of Tweet") +  ggtitle(title2) + 
        theme(plot.title = element_text(lineheight=2.8, face="bold") )  + theme_bw() + guides(fill=guide_legend(title="Key Words")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
        print(graph2) 
      
    }
    
    else if (input$selection=="malaria")  {
      title2 <- paste("What they tweeted on" , input$selection  )
      labelX <- paste( input$selection )
      graph2 <- ggplot(total.nmalaria,aes(tweetMY,malFund.count,fill=malFund))+
        geom_bar(stat="identity",position="dodge") + ylab("Mentioned") + xlab("Month of Tweet") +  ggtitle(title2) + 
        theme(plot.title = element_text(lineheight=2.8, face="bold") )  + theme_bw() + guides(fill=guide_legend(title="Key Words")) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      print(graph2) 
      
    }
    
    
    else if (input$selection=="pneumonia")  {
      title2 <- paste("What they tweeted on" , input$selection  )
      labelX <- paste( input$selection )
      graph2 <- ggplot(total.npneumonia,aes(tweetMY,pneumFund.count,fill=pneumFund))+
        geom_bar(stat="identity",position="dodge") + ylab("Mentioned") + xlab("Month of Tweet") +  ggtitle(title2) + 
        theme(plot.title = element_text(lineheight=2.8, face="bold") )  + theme_bw() + guides(fill=guide_legend(title="Key Words")) + 
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
      print(graph2) 
      
    }
    
    
  })
  
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$wordPlot <- renderPlot({
    if (input$selection=="hiv")  {
      
      wordcloud_rep(tweetHIV, scale=c(2,0.5), min.freq = 300 ,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
     
    }
    
    else if (input$selection=="malaria")  {
      
     wordcloud_rep(tweetmalaria, scale=c(2,0.5), min.freq = 300 ,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
   
    }
    
    else if (input$selection=="pneumonia")  {
     
       wordcloud_rep(tweetpneumonia, scale=c(2,0.5), min.freq =300 ,max.words=100,
                random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
       
    }
    
  })
    

  output$table <- renderDataTable({
    cars
  }, options=list(pageLength=10))
})

