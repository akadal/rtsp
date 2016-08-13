library("rjson")
require("GA")

rtsp <- function(mypoints,calctype="meter",startpoint=NULL,endpoint=NULL){
  
  # including start point
  if (!is.null(startpoint)){
    mypoints <- c(startpoint,mypoints)
  }
  
  # including end point
  if (!is.null(endpoint)){
    mypoints <- c(mypoints,endpoint)
  }
  
  # getting matrix from web service
  if (calctype=="time"){
    mymatrix <- createMapMatrix(mypoints,type = "time")
  }else{
    mymatrix <- createMapMatrix(mypoints)
  }
  
  print("Calculating route...")
  # GA is working..
  myga <- ga(type = "permutation",
             fitness = rtspfitnessfunction, 
             min=1, 
             max=length(mypoints),
             maxiter=1000,
             mymatrix=mymatrix,
             startpoint=startpoint,
             endpoint=endpoint,
             mypoints=mypoints,
             monitor=FALSE)
  
  mysolution <- myga@solution[1,]
  
  print("Done. Best solution:")
  # print best solution
  for(i in mysolution) {
    print(mypoints[i])
  } 
  
  # return best solution
  return (mysolution)
} 


createMapMatrix <- function(mypoints,calctype="meter",mode="driving") {
  
  meterinfo <- c()
  timeinfo <- c()
  
  print("getting matrix data from web service")
  
  for (i in 1:length(mypoints)){
    for (j in 1:length(mypoints)){
      if (i==j) {
        meterinfo  <- c(meterinfo,0)
        timeinfo <- c(timeinfo,0)
      } else {
        url_base <- paste("https://maps.googleapis.com/maps/api/distancematrix/json?origins=",
                          mypoints[i],
                          "&destinations=",
                          mypoints[j],
                          "&mode=",
                          mode,
                          "&language=en-En",
                          sep="")
        
        
        #print (url_base)
        json_file <- url_base
        json_data <- fromJSON(paste(readLines(json_file), collapse=""))
        
        
        m  <- json_data$rows[[1]]$elements[[1]]$distance$value
        #print(m)
        sn <- json_data$rows[[1]]$elements[[1]]$duration$value
        
        if (is.null(m)) {
          meterinfo  <- c(meterinfo,NULL)
        } else {
          meterinfo  <- c(meterinfo,m)
        }
        
        
        if (is.null(sn)) {
          timeinfo  <- c(timeinfo,NULL)
        } else {
          timeinfo  <- c(timeinfo,sn)
        }
        
        
      }
    }
  }
  
  print("data is received.")
  
  if (calctype=="time") {
    return(matrix(timeinfo,length(mypoints),length(mypoints)) )
  } else {
    return(matrix(meterinfo,length(mypoints),length(mypoints)) )
  }
}


rtspfitnessfunction <- function (x,mymatrix,startpoint,endpoint,mypoints) {
  
  # calculating cost
  total <- 0
  for (ss in 1:(length(x)-1)) {
    total <- total + mymatrix[x[ss],x[(ss+1)]]
  }
  
  # hard constraint
  if (!is.null(startpoint) && (mypoints[x[1]]!=startpoint)) {
    total <- total + 99999999 # longer than the longest distance between any two places on eart!
  }
  
  # hard constraint
  if(!is.null(endpoint) && (mypoints[x[length(x)]]!=endpoint) ){
    #print (total)
    total <- total + 99999999 # longer than the longest distance between any two places on eart!
  } 
  
  return(-total)
}

