zoogvisLineChart<-function(z, ...){
  
  suppressPackageStartupMessages(require(googleVis, quietly = TRUE))
  suppressPackageStartupMessages(require(dplyr, quietly = TRUE))
  op <- options(gvis.plot.tag=NULL)
  
  if(is.zoo(z)){
    df<-data.frame(t=index(z), coredata(z))
    tagnames<-names(df)
    n<-NCOL(df)-1
    #names(df)<-c('t', paste0('v', 1:n))

    h1<-gvisLineChart(xvar='t', yvar=tagnames[2:(n+1)], data = df, chartid = 'ZooSeries',
                      options = list(
                        crosshair ="{trigger:'both', orientation:'vertical'}",
                        explorer = "{axis:'horizontal', actions:['dragToZoom', 'rightClickToReset'], maxZoomIn: .01 }",
                        hAxis = "{title:'Fecha'}",
                        focusTarget = 'category', 
                        legend = "{position: 'top', textStyle: {color: 'black', fontSize: 15}, maxLines: 10}",
                        series="[{targetAxisIndex:0, color:'blue', lineWidth: '1'}, 
                        {targetAxisIndex:1, color:'red', lineWidth: '1'},
                        {targetAxisIndex:0, color:'blue', lineWidth: '0.5'}, 
                        {targetAxisIndex:1, color:'red', lineWidth: '0.5'}]",
                        vAxes="[{title:'',
                        titleTextStyle: {color: 'blue'},
                        textStyle:{color: 'blue'},
                        textPosition: 'out',
                        direction: '1'}, 
                        {title:'',
                        titleTextStyle: {color: 'red'},  
                        textStyle:{color: 'red'},
                        textPosition: 'out',
                        direction: '1'}]",
                        width=1200, 
                        height=600,
                        title = 'PI Time series',
                        titleTextStyle = "{position: 'in', textStyle: {color: 'black', fontSize: 18}, maxLines: 10}"
                      )
                      )
    
    h1$html$footer<-NULL
    
    h1

  }else{print('not a zoo object!!!')}
  
}
