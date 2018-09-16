NT <- function()
{
  mrenv <- new.env()
  
  gnewtable <- function (items, multiple = FALSE, chosencol = 1, icon.FUN = NULL, 
                         filter.column = NULL, filter.labels = NULL, filter.FUN = NULL, 
                         handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit()) 
  {
    if (!missing(items)) {
      if (is.vector(items)) 
        items <- data.frame(.= items, stringsAsFactors = FALSE)
      if (is.matrix(items)) 
        items <- data.frame(items, stringsAsFactors = FALSE)
    }
    widget <- .gtable(toolkit, items = items, multiple = multiple, 
                      chosencol = chosencol, icon.FUN = icon.FUN, filter.column = filter.column, 
                      filter.labels = filter.labels, filter.FUN = filter.FUN, 
                      handler = handler, action = action, container = container, 
                      ...)
    obj <- new("gTable", widget = widget, toolkit = toolkit)
    return(obj)
  }
  
  Main_Win <- gwindow(title="Normality Test for Continuous Variables",visible=TRUE,width=1000,height=600,expand=TRUE)
  lyt <- glayout(container=Main_Win,spacing=16)
  importData <- gbutton("Input Datasets",container=lyt)
  saveResult <- gbutton("Save Result",container=lyt)
  exit <- gbutton("Exit",container=lyt)
  runMethod <- gbutton("Run Method",container=lyt)
  generatePlot <- gbutton("Generate Plot",container=lyt)
  methodLabel <- glabel("Select Methods:")
  plotLabel <- glabel("Select Plots:")
  methodCom <- gcombobox(c("Kolmogorov-Smirnor (KS)","Lilliefors (Lillie)","Cramer-von Mises (CVM)",
                           "Anderson-Darling (AD)","Shapiro-Wilk (Shapiro)","Pearson chi-square (Pearson)",
                           "Jarque-Bera (JB)","D'Agostino (Dago)","Shapiro-Francia(SF)"),selected=1,editable=TRUE,container=lyt)
  plotCom <- gcombobox(c("Histogram","Box diagram","Q-Q diagram"),selected=1,editable=TRUE,container=lyt)
  
  
  lyt[1,1] <- importData
  lyt[4,1] <- methodLabel
  lyt[5,1] <- methodCom
  lyt[6,1] <- runMethod
  lyt[7,1] <- saveResult
  lyt[10,1] <- plotLabel
  lyt[11,1] <- plotCom
  lyt[12,1] <- generatePlot
  lyt[15,1] <- exit
  
  nb1 <- gnotebook(tab.pos=3,closebuttons=TRUE,dontCloseThese=TRUE,container=lyt,expand=TRUE)
  
  tb <- gnewtable("     
                  1. This program is an R package for normality test on continuous variables.
                  
                  2. It is developed by RenWenlong, Xiaojing, ZhangYawen and LiangZhikai.
                  
                  Version 1.0",multiple=TRUE,container=nb1,expand=TRUE,label="About the program")
  font(tb)<-c(size="x-large")
  lyt[1:16,2,expand=TRUE] <- nb1
  
  
  
  addHandlerClicked(importData,handler=function(h,...){
    testdata <- gfile(text="Select a file...",type="open",
                      filter=list("All files"=list(patterns=c("*")),
                                  "CSV files"=list(patterns=c("*.csv"))))
    
    if(is.na(testdata))
    {
      gmessage("Please input correct format data !","Warning",icon="warning")
      return
    }else{
      mrenv$RawData <- as.matrix(read.csv(testdata,header=FALSE))
      showRawData <- as.data.frame(mrenv$RawData)
      Input_dfe <- gdfedit(showRawData,container=nb1,expand=TRUE,label="Input Dataset")
    }
  })
  
  addHandlerClicked(runMethod,handler=function(h,...){
    if(svalue(methodCom)=="Kolmogorov-Smirnor (KS)")
    {
      FuncSub1()
    }else if(svalue(methodCom)=="Lilliefors (Lillie)")
    {
      FuncSub2()
    }else if(svalue(methodCom)=="Cramer-von Mises (CVM)")
    {
      FuncSub3()
    }else if(svalue(methodCom)=="Anderson-Darling (AD)")
    {
      FuncSub4()
    }else if(svalue(methodCom)=="Shapiro-Wilk (Shapiro)")
    {
      FuncSub5()
    }else if(svalue(methodCom)=="Pearson chi-square (Pearson)")
    {
      FuncSub6()
    }else if(svalue(methodCom)=="Jarque-Bera (JB)")
    {
      FuncSub7()
    }else if(svalue(methodCom)=="D'Agostino (Dago)")
    {
      FuncSub8()
    }else if(svalue(methodCom)=="Shapiro-Francia(SF)")
    {
      FuncSub9()
    }
  })
  
  ######################Methods#######################
  FuncSub1 <- function()
  {
    #############Kolmogorov-Smirnor (KS)################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_ks <- ks.test(jitter(x),"pnorm",mean(x),sd(x))
      save_ks <- matrix(NA,3,2)
      save_ks[1,1] <- "One-sample Kolmogorov-Smirnov normality test"
      save_ks[2,1] <- c("D:")
      save_ks[3,1] <- c("P-value:")
      save_ks[1,2] <- " "
      save_ks[2,2] <- as.numeric(res_ks[[1]])
      save_ks[3,2] <- as.numeric(res_ks[[2]])
      sub1_def <- gdfedit(save_ks,container=nb1,expand=TRUE,label="Kolmogorov-Smirnor (KS)")
      mrenv$ks <- save_ks
    }
  }
  
  FuncSub2 <- function()
  {
    #############Lilliefors (Lillie)####################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_lf <- lillie.test(x)
      save_lf <- matrix(NA,3,2)
      save_lf[1,1] <- "Lilliefors normality test"
      save_lf[2,1] <- c("D:")
      save_lf[3,1] <- c("P-value:")
      save_lf[1,2] <- " "
      save_lf[2,2] <- as.numeric(res_lf[[1]])
      save_lf[3,2] <- as.numeric(res_lf[[2]])
      sub2_def <- gdfedit(save_lf,container=nb1,expand=TRUE,label="Lilliefors (Lillie)")
      mrenv$lf <- save_lf
    }
  }
  
  FuncSub3 <- function()
  {
    #############Cramer-von Mises (CVM)#################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_cvm <-  cvm.test(x)
      save_cvm <- matrix(NA,3,2)
      save_cvm[1,1] <- "Cramer-von Mises normality test"
      save_cvm[2,1] <- c("W:")
      save_cvm[3,1] <- c("P-value:")
      save_cvm[1,2] <- " "
      save_cvm[2,2] <- as.numeric(res_cvm[[1]])
      save_cvm[3,2] <- as.numeric(res_cvm[[2]])
      sub3_def <- gdfedit(save_cvm,container=nb1,expand=TRUE,label="Cramer-von Mises (CVM)")
      mrenv$cvm <- save_cvm
    }
  }
  
  FuncSub4 <- function()
  {
    #############Anderson-Darling (AD)##################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_ad <- ad.test(x)
      save_ad <- matrix(NA,3,2)
      save_ad[1,1] <- "Anderson-Darling normality test"
      save_ad[2,1] <- c("A:")
      save_ad[3,1] <- c("P-value:")
      save_ad[1,2] <- " "
      save_ad[2,2] <- as.numeric(res_ad[[1]])
      save_ad[3,2] <- as.numeric(res_ad[[2]])
      sub4_def <- gdfedit(save_ad,container=nb1,expand=TRUE,label="Anderson-Darling (AD)")
      mrenv$ad <- save_ad 
    }
  }
  
  FuncSub5 <- function()
  {
    #############Shapiro-Wilk (Shapiro)#################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_sw <- shapiro.test(x)
      save_sw <- matrix(NA,3,2)
      save_sw[1,1] <- "Shapiro-Wilk normality test"
      save_sw[2,1] <- c("W:")
      save_sw[3,1] <- c("P-value:")
      save_sw[1,2] <- " "
      save_sw[2,2] <- as.numeric(res_sw[[1]])
      save_sw[3,2] <- as.numeric(res_sw[[2]])
      sub5_def <- gdfedit(save_sw,container=nb1,expand=TRUE,label="Shapiro-Wilk (Shapiro)")
      mrenv$sw <- save_sw
    }
  }
  
  FuncSub6 <- function()
  {
    #############Pearson chi-square (Pearson)###########
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_chi <- pearson.test(x)
      save_chi <- matrix(NA,3,2)
      save_chi[1,1] <- "Pearson chi-square normality test"
      save_chi[2,1] <- c("P:")
      save_chi[3,1] <- c("P-value:")
      save_chi[1,2] <- " "
      save_chi[2,2] <- as.numeric(res_chi[[1]])
      save_chi[3,2] <- as.numeric(res_chi[[2]])
      sub6_def <- gdfedit(save_chi,container=nb1,expand=TRUE,label="Pearson chi-square (Pearson)")
      mrenv$chi <- save_chi 
    }
  }
  
  FuncSub7 <- function()
  {
    #############Jarque-Bera (JB)#######################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_jb <- jarque.bera.test(x)
      save_jb <- matrix(NA,3,2)
      save_jb[1,1] <- "Jarque Bera normality test"
      save_jb[2,1] <- c("X-squared:")
      save_jb[3,1] <- c("P-value:")
      save_jb[1,2] <- " "
      save_jb[2,2] <- as.numeric(res_jb[[1]])
      save_jb[3,2] <- as.numeric(res_jb[[3]])
      sub7_def <- gdfedit(save_jb,container=nb1,expand=TRUE,label="Jarque-Bera (JB)")
      mrenv$jb <- save_jb 
    }
  }
  
  FuncSub8 <- function()
  {
    #############D!/Agostino (Dago)######################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_dago <- dagoTest(x)
      save_dago <- matrix(NA,9,2)
      save_dago[1,1] <- "D'Agostino normality test"
      save_dago[1,2] <- " "
      save_dago[2,1] <- c("STATISTIC:")
      save_dago[2,2] <- " "
      save_dago[3,1] <- c("Chi2 | Omnibus:")
      save_dago[3,2] <- as.numeric(res_dago@test[[1]][1])
      save_dago[4,1] <- c("Z3  | Skewness:")
      save_dago[4,2] <- as.numeric(res_dago@test[[1]][2])
      save_dago[5,1] <- c("Z4  | Kurtosis:")
      save_dago[5,2] <- as.numeric(res_dago@test[[1]][3])
      save_dago[6,1] <- c("P Value:")
      save_dago[6,2] <- " "
      save_dago[7,1] <- c("Omnibus  Test:")
      save_dago[7,2] <- as.numeric(res_dago@test[[3]][1])
      save_dago[8,1] <- c("Skewness Test:")
      save_dago[8,2] <- as.numeric(res_dago@test[[3]][2])
      save_dago[9,1] <- c("Kurtosis Test:")
      save_dago[9,2] <- as.numeric(res_dago@test[[3]][3])
      sub8_def <- gdfedit(save_dago,container=nb1,expand=TRUE,label="D!/Agostino (Dago)")
      mrenv$dago <- save_dago
    }
  }
  
  FuncSub9 <- function()
  {
    #############Shapiro-Francia(SF)####################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      mm <- dim(rawdata)[1]
      nn <- dim(rawdata)[2]
      x <- matrix(rawdata,mm*nn,1)
      res_sf <- sf.test(x)
      save_sf <- matrix(NA,3,2)
      save_sf[1,1] <- "Shapiro-Francia normality test"
      save_sf[2,1] <- c("W:")
      save_sf[3,1] <- c("P-value:")
      save_sf[1,2] <- " "
      save_sf[2,2] <- as.numeric(res_sf[[1]])
      save_sf[3,2] <- as.numeric(res_sf[[2]])
      sub9_def <- gdfedit(save_sf,container=nb1,expand=TRUE,label="Shapiro-Francia(SF))")
      mrenv$sf <- save_sf
    }
  }
  
  addHandlerClicked(saveResult,handler=function(h,...){
    if(svalue(methodCom)=="Kolmogorov-Smirnor (KS)")
    {
      FuncSave1 <- mrenv$ks
      if(exists("FuncSave1")==FALSE||is.null(FuncSave1)==TRUE)
      {
        gmessage("There is something wrong in the output of Kolmogorov-Smirnor (KS) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave1,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Lilliefors (Lillie)")
    {
      FuncSave2 <- mrenv$lf
      if(exists("FuncSave2")==FALSE||is.null(FuncSave2)==TRUE)
      {
        gmessage("There is something wrong in the output of Lilliefors (Lillie) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave2,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Cramer-von Mises (CVM)")
    {
      FuncSave3 <- mrenv$cvm
      if(exists("FuncSave3")==FALSE||is.null(FuncSave3)==TRUE)
      {
        gmessage("There is something wrong in the output of Cramer-von Mises (CVM) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave3,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Anderson-Darling (AD)")
    {
      FuncSave4 <- mrenv$ad
      if(exists("FuncSave4")==FALSE||is.null(FuncSave4)==TRUE)
      {
        gmessage("There is something wrong in the output of Anderson-Darling (AD) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave4,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Shapiro-Wilk (Shapiro)")
    {
      FuncSave5 <- mrenv$sw
      if(exists("FuncSave5")==FALSE||is.null(FuncSave5)==TRUE)
      {
        gmessage("There is something wrong in the output of Shapiro-Wilk (Shapiro) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave5,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Pearson chi-square (Pearson)")
    {
      FuncSave6 <- mrenv$chi
      if(exists("FuncSave6")==FALSE||is.null(FuncSave6)==TRUE)
      {
        gmessage("There is something wrong in the output of Pearson chi-square (Pearson) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave6,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Jarque-Bera (JB)")
    {
      FuncSave7 <- mrenv$jb
      if(exists("FuncSave7")==FALSE||is.null(FuncSave7)==TRUE)
      {
        gmessage("There is something wrong in the output of Jarque-Bera (JB) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave7,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="D'Agostino (Dago)")
    {
      FuncSave8 <- mrenv$dago
      if(exists("FuncSave8")==FALSE||is.null(FuncSave8)==TRUE)
      {
        gmessage("There is something wrong in the output of D'Agostino (Dago) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave8,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
    if(svalue(methodCom)=="Shapiro-Francia(SF)")
    {
      FuncSave9 <- mrenv$sf
      if(exists("FuncSave9")==FALSE||is.null(FuncSave9)==TRUE)
      {
        gmessage("There is something wrong in the output of Shapiro-Francia(SF) results!","Info",icon="info")
        return
      }else{
        outputPath <- gfile(text="Save a file...",type="save",filter=list("All files"=list(patterns=c("*")),"CSV files"=list(patterns=c("*.csv"))))
        write.table(FuncSave9,outputPath,sep = ",",row.names=FALSE,col.names = FALSE)
      }
    }
    
  })
  
  
  #######################Plots########################
  addHandlerClicked(generatePlot,handler=function(h,...){
    if(svalue(plotCom)=="Histogram")
    {
      FuncPlot1()
    }else if(svalue(plotCom)=="Box diagram")
    {
      FuncPlot2()
    }else if(svalue(plotCom)=="Q-Q diagram")
    {
      FuncPlot3()
    }
  })
  
  FuncPlot1 <- function()
  {
    ##################histogram#########################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      win1 <- gwindow("Histogram",width=600,height=600)
      ggwin1 <- ggraphics(container=win1)
      addHandlerChanged(ggwin1, handler=function(h,...) {
        hist(rawdata)
      })
    }
  }
  
  FuncPlot2 <- function()
  {
    ##################boxplot##############################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      win1 <- gwindow("Box diagram",width=600,height=600)
      ggwin1 <- ggraphics(container=win1)
      addHandlerChanged(ggwin1, handler=function(h,...) {
        boxplot(rawdata)
      })
    }
  }
  
  FuncPlot3 <- function()
  {
    ##################qqnorm###########################
    rawdata <- mrenv$RawData
    if(is.null(rawdata)==TRUE)
    {
      gmessage("Please input correct format data firstly !","Warning",icon="warning")
      return
    }else{
      win1 <- gwindow("Q-Q diagram",width=600,height=600)
      ggwin1 <- ggraphics(container=win1)
      addHandlerChanged(ggwin1, handler=function(h,...) {
        qqnorm(rawdata)
      })
    }
  }

  addHandlerClicked(exit,handler=function(h,...){
    gconfirm("Yes or no?",handler=function(h,...){dispose(Main_Win)})
  })
}



