# rtsp - an R Library: Real Travelling Salesman Problem

# Usage example:

source("rtsp.R")


mypoints <- c("İstanbul", "Ankara", "İzmir")


rtsp(mypoints)

------------------------

mypoints <- c("Bodrum", "Antalya", "Çeşme")

rtsp(mypoints, startpoint="İstanbul", endpoint="İstanbul")

