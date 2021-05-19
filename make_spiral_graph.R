# The spiral graph function was broken in a casnet update; this is the previous
# version that works as before

make_spiral_graph_matti <- function(g,
                              type = "Archimedean",
                              arcs = 6,
                              a = 1,
                              b = NULL,
                              rev= FALSE,
                              curvature = -0.6,
                              angle = 90,
                              markTimeBy = NULL,
                              labelSize = 3,
                              alphaV = 1,
                              alphaE = .6,
                              showArrows = FALSE,
                              title = "",
                              subtitle = "",
                              showEpochLegend = TRUE,
                              markEpochsBy = NULL,
                              epochColours = NULL,
                              epochLabel = "Epoch",
                              showSizeLegend = FALSE,
                              sizeLabel = "Size",
                              scaleVertexSize = c(1,6),
                              vertexBorderColour = "black",
                              scaleEdgeSize = 1/5,
                              edgeColourLabel = "Weight",
                              showEdgeColourLegend = FALSE,
                              edgeColourByEpoch = TRUE,
                              defaultEdgeColour = "grey70",
                              doPlot = TRUE){
  
  g$layout <- layout_as_spiral(g, type = type, arcs = arcs, a = a, b = b, rev = rev)
  
  if(is.null(markTimeBy)){
    if(!is.null(markEpochsBy)){
      grIDs <- ts_changeindex(markEpochsBy)
      tbreaks <- unique(sort(c(grIDs$xmax,grIDs$xmax)))
      tlabels <- ceiling(tbreaks)
      #markTimeBy <- TRUE
    } else {
      x <- 1:igraph::vcount(g)
      v <- seq(1,igraph::vcount(g),by=igraph::vcount(g)/arcs)
      tbreaks <- c(1,which(diff(findInterval(x, v))!=0),igraph::vcount(g))
      #tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
      if(max(tbreaks)!=igraph::vcount(g)){
        tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
        tlabels <- paste(tbreaks)
      }
      if(min(tbreaks)>1){
        tbreaks<- c(1,tbreaks)
        tlabels <- paste(tbreaks)
      }
    }
  } else {
    if(is.numeric(markTimeBy)){
      if(all(markTimeBy%in%1:igraph::vcount(g))){
        tbreaks <- unique(markTimeBy)
      }
      if(!is.null(names(markTimeBy))){
        tlabels <- unique(names(markTimeBy))
      } else {
        tlabels <- paste(tbreaks)
      }
    } else {
      markTimeBy <- TRUE
    }
  }
  
  if(is.logical(markTimeBy)){
    if(markTimeBy){
      if(type == "Euler"){
        tbreaks <- which(diff(as.numeric(cut(g$layout[,2], breaks = arcs,include.lowest = TRUE)))!=0)
        tbreaks[1] <- 1
        tbreaks[which.max(tbreaks)] <- igraph::vcount(g)
      } else {
        tbreaks <- which(diff(c(g$layout[1,1],g$layout[,2],g$layout[1,2])>=g$layout[1,2])!=0)
      }
      if(max(tbreaks)>igraph::vcount(g)){tbreaks[which.max(tbreaks)]<-igraph::vcount(g)}
      if(max(tbreaks)<igraph::vcount(g)){tbreaks<- c(tbreaks,igraph::vcount(g))}
      if(min(tbreaks)>1){tbreaks<- c(1,tbreaks)}
      tlabels <- paste(tbreaks)
    }
  }
  
  if(max(tbreaks)!=igraph::vcount(g)){
    tbreaks[which.max(tbreaks)]<-igraph::vcount(g)
    #tlabels <- paste(tbreaks)
    tlabels <- tlabels[tbreaks]
  }
  
  if(min(tbreaks)>1){
    tbreaks<- c(1,tbreaks)
    #tlabels <- paste(tbreaks)
    tlabels <- tlabels[tbreaks]
  }
  
  if(length(which(diff(tbreaks)==1))>0){
    tbreaks <- tbreaks[-(which(diff(tbreaks)==1)+1)]
    #tlabels <- paste(tbreaks)
    tlabels <- tlabels[tbreaks]
  }
  
  if(is.null(markEpochsBy)){
    markEpochsBy <- character(igraph::vcount(g))
    for(i in 1:(length(tbreaks)-1)){
      markEpochsBy[tbreaks[i]:tbreaks[i+1]] <- rep(paste0(tbreaks[i],"-",tbreaks[i+1]),length(tbreaks[i]:tbreaks[i+1]))
    }
    if(!is.null(epochColours)){
      if(length(unique(markEpochsBy))>length(unique(epochColours))){
        warning("Number of unique epochs is unequal to number of unique colours!\nUsing default colour scheme.")
        epochColours <- NULL
      }
    }
  }
  
  g <- plotNET_groupColour(g,
                           groups = markEpochsBy,
                           colourV = TRUE,
                           colourE = edgeColourByEpoch,
                           # alphaV = alphaV,
                           # aplhaE = alphaE,
                           groupColours = epochColours,
                           defaultEdgeColour = defaultEdgeColour,
                           doPlot = FALSE)
  
  size <- 1
  if(!is.null(V(g)$size)){
    size <- V(g)$size
  }
  
  gNodes        <- as.data.frame(g$layout)
  gNodes$ID     <- as.numeric(V(g))
  gNodes$colour <- V(g)$colour
  gNodes$labels <- factor(V(g)$groupnum, levels = unique(V(g)$groupnum), labels = unique(V(g)$group))
  gNodes$size   <- size
  gNodes$alpha  <- V(g)$alpha
  
  width <- 1
  if(!is.null(E(g)$width)){
    width <- E(g)$width
  }
  gEdges        <- igraph::get.data.frame(g)
  gEdges$from.x <- gNodes$V1[match(gEdges$from, gNodes$ID)]
  gEdges$from.y <- gNodes$V2[match(gEdges$from, gNodes$ID)]
  gEdges$to.x   <- gNodes$V1[match(gEdges$to, gNodes$ID)]
  gEdges$to.y   <- gNodes$V2[match(gEdges$to, gNodes$ID)]
  gEdges$width  <- width
  gEdges$colorVar <- as.numeric_discrete(gEdges$color)
  
  if(is.null(vertexBorderColour))(
    vBc <- gNodes$colour
  ) else {
    if(length(vertexBorderColour)==1|length(vertexBorderColour)==NROW(gNodes)){
      vBc <- vertexBorderColour
    } else {
      warning("Invalid value(s) for vertexBorderColour, using default.")
      vertexBorderColour <- "black"
    }
  }
  
  # Fix same coords
  sameID <- which(gEdges$from.x==gEdges$to.x)
  if(length(sameID)>0){
    gNodes$V2[gEdges$from[sameID]] <- gNodes$V2[gEdges$from[sameID]]+mean(diff(gNodes$V2))/2
    gEdges$to.x[sameID] <- gEdges$to.x[sameID]+mean(diff(gEdges$to.x))/2
    gEdges$to.y[sameID] <- gEdges$to.y[sameID]+mean(diff(gEdges$to.y))/2
  }
  
  
  gEdges$curvature <- curvature
  if(type=="Euler"){
    if(curvature<0){
      gEdges$curvature[gEdges$from.y<.5|gEdges$from.x>.5] <- -1*curvature
    } else {
      gEdges$curvature[gEdges$from.y>.5|gEdges$from.x<.5] <- -1*curvature
    }
  }
  
  ar <- NULL
  if(igraph::is_directed(g)){
    if(showArrows){
      ar <- ggplot2::arrow(type = "closed", ends = "last", length = ggplot2::unit(mean(gNodes$size, na.rm = TRUE)*scaleVertexSize[2], "mm"))
    }
  }
  
  gg <- ggplot2::ggplot(gNodes, ggplot2::aes_(x = ~V1, y = ~V2)) +
    ggplot2::geom_curve(data = gEdges, aes_(x = ~from.x,
                                            xend = ~to.x,
                                            y = ~from.y,
                                            yend = ~to.y,
                                            colour = ~colorVar),
                        curvature = curvature,
                        arrow = ar,
                        angle = angle,
                        size = gEdges$width * scaleEdgeSize,
                        alpha = alphaE) +
    geom_point(aes(fill = labels, size = size), pch=21, colour = vBc, alpha = alphaV) +
    ggtitle(label = title, subtitle = subtitle) +
    scale_fill_manual(epochLabel, values = unique(gNodes$colour)) +
    scale_size(sizeLabel, range = scaleVertexSize) +
    scale_color_gradientn(edgeColourLabel, colours = unique(gEdges$color))
  
  if(showEpochLegend){
    gg <- gg + guides(fill = guide_legend(title.position = "top",
                                          byrow = TRUE,
                                          nrow=2,
                                          override.aes = list(size=5, order = 0)))
  } else {
    gg <- gg + guides(fill = "none")
  }
  
  if(showSizeLegend){
    gg <- gg + guides(size = guide_legend(title.position = "top",
                                          byrow = TRUE,
                                          nrow=2,
                                          override.aes = list(legend.key.size = unit(1.2,"lines"), order = 1)))
  } else {
    gg <- gg + guides(size = "none")
  }
  
  if(showEdgeColourLegend){
    gg <- gg + guides(colour = guide_legend(title.position = "top",
                                            byrow = TRUE,
                                            nrow=2,
                                            override.aes = list(size=5, order = 3)))
  } else {
    gg <- gg + guides(colour = "none")
  }
  
  if(!is.null(markTimeBy)){
    gg <- gg + annotate("label", x=gNodes$V1[tbreaks], y=gNodes$V2[tbreaks], label = tlabels, size=labelSize)
  }
  
  gg <- gg +
    coord_fixed() +
    theme_void() +
    theme(legend.title = element_text(face="bold"),
          legend.position =  "top",
          legend.margin = margin(t = 0,r = 1,l = 1,0))
  
  if(doPlot){
    print(gg)
  }
  
  return(invisible(gg))
}