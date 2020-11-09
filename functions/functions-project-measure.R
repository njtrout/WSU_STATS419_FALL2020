prepareMeasureData = function(measure)
{
  measure.short = measure[,c(2,3,6,13,19,20,23,30)];
  measure.short$gender = as.character(measure.short$my.gender)
  measure.df = subsetDataFrame(measure.short, "gender", "==", "m")
  measure.df = subsetDataFrame(measure.df, "age", ">=", 18)
  measure.df = subsetDataFrame(measure.df, "age", "<=", 28)
  
  proportion = measure.df$height/measure.df$hand.width
  mylogic = proportion<10
  mylogic[is.na(mylogic)] = TRUE
  measure.ndf = measure.df[mylogic,]
  
  mylogic = measure.ndf$arm.reach > measure.ndf$height
  mylogic[is.na(mylogic)] = TRUE
  measure.ndf = measure.ndf[mylogic,]
  #should I leave in players?
  #measure.fdf = measure.ndf[, c(1,2,3,5,6,7,4,8)];
  measure.fdf = measure.ndf[, c(2,3,7,5,6)];
  colnames(measure.fdf)[1] = "height"
  colnames(measure.fdf)[2] = "wingspan"
  colnames(measure.fdf)[3] = "standing.reach"
  colnames(measure.fdf)[4] = "hand.length"
  colnames(measure.fdf)[5] = "hand.width"
  
  set.seed(123)
  
  #rows=sample(1:(nrow(measure.ndf)),50, replace = FALSE, prob = NULL)
  #measure.fdf = measure.ndf[rows,]
  
  measure.fdf;
  
}
#when pushed to git last measure was passed in 
getProportionHandWidthMeasure = function(measure.df)
{
  
  measure.fdf = measure.df
  measure.height.proportion = measure.fdf$height/measure.fdf$hand.width
  #View(measure.height.proportion)
  measure.armspan.proportion = measure.fdf$arm.span/measure.fdf$hand.width
  #View(measure.armspan.proportion)
  measure.armreach.proportion = measure.fdf$arm.reach/measure.fdf$hand.width
  #View(measure.armreach.proportion)
  measure.proportion1 = cbind(measure.height.proportion, measure.armspan.proportion, measure.armreach.proportion)
  measure.data1 = measure.df[,c(1,5)]
  measure.proportion = cbind(measure.data1,measure.proportion1)
  measure.proportion$hand.width.proportion = 1
  measure.proportion$arm.reach = NULL
  measure.proportion
}

prepareDataNBA = function(nbadata)
{
  
  #nbadata = read.csv("C:/Users/Nic Trout/Documents/C/WSU_STATS419_FALL2020/project-measure_nic/nbadata.txt", sep ="\t", header = TRUE);
  #converting to numerica
  nbadata_handlength = as.numeric(nbadata$HAND.LENGTH..INCHES.)
  nbadata_handwidth = as.numeric(nbadata$HAND.WIDTH..INCHES.)
  
  #create a data frame 
  nbadata_ = data.frame(nbadata_handlength, nbadata_handwidth)
  nbanew = nbadata
  #converting from feet and inches to inches
  for( i in 1:nrow(nbadata)){
    for(j in 1:4)
    {
      height = as.character(nbadata[i,c(6,7,8,10)][j]);
      tmp = strsplit(height,"'",fixed=TRUE)[[1]];
      feet = as.numeric(tmp[1]);
      inches = as.numeric(tmp[2]);
      final = 12 * as.numeric(tmp[1]) + as.numeric(tmp[2]);
      nbanew[i,c(6,7,8,10)][j] = final;
    }
  }
  #removing columns I dont need and renaming data 
  nbadata = nbanew
  nbadata = nbadata[, c(7,10,8,4,5,1)]
  #nbadata = nbadata[, c(1,2,4,5,7,8,10)]
  #colnames(nbadata)[2] = "POS
  colnames(nbadata)[1] = "height"
  colnames(nbadata)[2] = "wingspan"
  colnames(nbadata)[3] = "standing.reach"
  colnames(nbadata)[4] = "hand.length"
  colnames(nbadata)[5] = "hand.width"
  colnames(nbadata)[6] = "players"
  nbanew = nbadata
  nbadata$rookie.year=2019;
  set.seed(123);
  nbadata = na.omit(nbadata);
  rows=sample(1:(nrow(nbadata)),50)
  nba.fdf = nbadata[rows,]
  ncol = 1:5
  for(i in ncol){
    nba.fdf[,i] = as.numeric(nba.fdf[,i])
  }
  nba.fdf 
}

getProportionHandWidthNBA = function(nba.df)
{
  nba.df.height.proportion = nba.df$height/nba.df$hand.width
  
  nba.df.armspan.proportion = nba.df$wingspan/nba.df$hand.width
  
  nba.df.armreach.proportion = nba.df$standing.reach/nba.df$hand.width
  
  nba.df.proportion1 = cbind(nba.df.height.proportion, nba.df.armspan.proportion, nba.df.armreach.proportion)
  nba.df.data1 = nba.df[,c(6,1,2,3,4,5)]
  nba.df.proportion = cbind(nba.df.data1,nba.df.proportion1)
  nba.df.proportion$hand.width.proportion = 1
  nba.df.proportion
  nba.df.proportion[,c(1,7:10)]
  
}

plotcorrS = function(x.m = x.m,
                     x.n = x.n,
                     y.m = y.m,
                     y.n = y.n,
                     xl = "x",
                     yl = "y",
                     main.m = "MEASURE",
                     main.n = "NBA",
                     dotcolor.n = "blue",
                     linecolor.n = "red",
                     dotcolor.m = "#5e6a71",
                     linecolor.m = "#981e32"){
  
  minx = min(x.m,x.n, na.rm = TRUE);
  maxx = max(x.m,x.n, na.rm = TRUE);
  
  miny = min(y.m,y.n, na.rm = TRUE);
  maxy = max(y.m,y.n, na.rm = TRUE);
  
  plot(x.m, y.m, main = main.m, xlab = xl, ylab = yl, bty = "n", col = dotcolor.m, xlim = c(minx,maxx), ylim = c(miny,maxy));
  reg.m = lm(y.m~x.m)
  #print(summary(reg.m));
  abline(reg.m, col= linecolor.m)
  
  plot(x.n, y.n, main = main.n, xlab = xl, ylab = yl, bty = "n", col = dotcolor.n, xlim = c(minx,maxx), ylim = c(miny,maxy));
  reg.n = lm(y.n~x.n)
  #print(summary(reg.n));
  abline(reg.n, col= linecolor.n)
  
  #https://stats.stackexchange.com/questions/93540/testing-equality-of-coefficients-from-two-different-regressions
  
  nba.beta = reg.n$coefficients[2]
  nba.se = summary(reg.n)$coefficients[2,2]
  
  measure.beta = reg.m$coefficients[2]
  measure.se = summary(reg.m)$coefficients[2,2]
  
  z = (nba.beta - measure.beta)/(sqrt(nba.se^2+measure.se^2))
  z
  
}