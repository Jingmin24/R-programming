install.packages("corrplot")
library(corrplot)
loans_numeric<-sapply(clean.df,is.numeric)##only to numeric variables!
correlation<-cor(clean.df[,loans_numeric])
correlation
corrplot(correlation,type = "upper"
         ,tl.pos = "d",tl.col = "black",tl.cex = 0.6)
corrplot(correlation,add = TRUE,type = "lower",
         method = "number",diag=FALSE
         ,tl.pos = "n",
         cl.pos = "n",cl.ratio= 0.2,addCoefasPercent = TRUE)
