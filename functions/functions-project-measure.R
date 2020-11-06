prepareMeasureData = function(measure)
 {
  measure.short = measure[,c(2,3,6,13,19,20,23,30)];
  measure.df = subsetDataFrame(measure.short, "my.gender", "==", "m")
  measure.df = subsetDataFrame(measure.short, "age", ">=", 18)
  measure.df = subsetDataFrame(measure.short, "age", "<=", 28)
  mylogic = measure.df$arm.reach > measure.df$height
  measure.df = measure.df[mylogic,]
  measure.ndf = measure.df[, c(1,2,3,5,6,7,4,8)];


  measure.ndf = na.omit(measure.ndf);
  set.seed(123)
  rows=sample(1:(nrow(measure.ndf)),50, replace = TRUE, prob = NULL)
  measure.fdf = measure.ndf[rows,]

  measure.fdf;
}

getProportionHandWidth = function(measure)
{
  measure.fdf = measure
  measure.height.proportion = measure.fdf$height/measure.fdf$hand.width
  #View(measure.height.proportion)
  measure.armspan.proportion = measure.fdf$arm.span/measure.fdf$hand.width
  #View(measure.armspan.proportion)
  measure.armreach.proportion = measure.fdf$arm.reach/measure.fdf$hand.width
  #View(measure.armreach.proportion)
  measure.proportion1 = cbind(measure.height.proportion, measure.armspan.proportion, measure.armreach.proportion)
  measure.data1 = measure.df[,c(1,6)]
  measure.proportion = cbind(measure.data1,measure.proportion1)
  measure.proportion
}

prepareDataNBA = function(nbadata)
{
  #converting to numerica
  nbadata_handlength = as.numeric(nbadata$HAND.LENGTH..INCHES.)
  nbadata_handwidth = as.numeric(nbadata$HAND.WIDTH..INCHES.)
  
  #create a data frame 
  nbadata_ = data.frame(nbadata_handlength, nbadata_handwidth)
  
  #converting from feet and inches to inches
  for( i in 1:nrow(nbadata)){
    for(j in 1:4)
    {
      height = as.character(nbadata[i,c(6,7,8,10)][j]);
      tmp = strsplit(height,"'",fixed=TRUE)[[1]];
      feet = as.numeric(tmp[1]);
      inches = as.numeric(tmp[2]);
      final = 12 * as.numeric(tmp[1]) + as.numeric(tmp[2]);
      nbadata[i,c(6,7,8,10)][j] = final;
    }
  }
  #removing columns I dont need and renaming data 
  nbadata = nbadata[, c(1,4,5,7,8,10)]
  colnames(nbadata)[1] = "players"
  colnames(nbadata)[2] = "hand.length"
  colnames(nbadata)[3] = "hand.width"
  colnames(nbadata)[4] = "height"
  colnames(nbadata)[5] = "standing.reach"
  colnames(nbadata)[6] = "wingspan"
  nbadata$rookie.year=2019;
  set.seed(123);
  nbadata = na.omit(nbadata);
  rows=sample(1:(nrow(nbadata)),50)
  nba.fdf = nbadata[rows,]
  ncol = 2:7
  for(i in ncol){
    nba.fdf[,i] = as.numeric(nba.fdf[,i])
  }
  nba.fdf 
}
