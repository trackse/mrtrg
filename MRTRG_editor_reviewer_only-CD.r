#to editor and reviewer only to replicated fully

#disk="//tsclient/P/"
disk="P://"
##################引用包##################
if(TRUE){
  
  
  if(TRUE){
    #library(tidyr)
    library(rms)
    library(Hmisc)
    library(lattice)
    library(survival)
    library(Formula)
    library(ggplot2)
    library(foreign)
    library(glmnet)
    # library(h2o) ;library(data.table);library(bit64)
    # 
    # library(foreign)
    # library(tidyverse)
    # library(survival)
    # library(mlr)
    # library(gridExtra)
    # library(ggfortify)
    # library(CoxBoost)
    # library(randomForestSRC)
    # 
    # # library(lattice)
    # # library(ggalluvial)
    # # library(tidyr)
    # library(arules)
    # library(arulesViz)
    # library(tmle)
    
   
  }
  
}

#快速运行
if(TRUE){
  
  load("input.RData")  #load data, also uploaded necessary part of them
  
}


#######################core code#####################################



#PFS
if(TRUE){
  
  
  #some test
  if(TRUE){
    if(TRUE){
      cbdata_10=ebvdb3m[which(ebvdb3m$sumall2f==0),]
      cbdata_10$codex=10
      cbdata_11=ebvdb3m[which(ebvdb3m$sumall2f==1),]
      cbdata_11$codex=11
      
      cbdata_20=ebvdb3m[which(ebvdb3m$EBV3m3f==0),]
      cbdata_20$codex=20
      cbdata_21=ebvdb3m[which(ebvdb3m$EBV3m3f==1),]
      cbdata_21$codex=21
      cbdata_22=ebvdb3m[which(ebvdb3m$EBV3m3f==2),]
      cbdata_22$codex=22
      
      cbdata_30=ebvdb3m[which(ebvdb3m$test2ebv==0),]
      cbdata_30$codex=30
      cbdata_31=ebvdb3m[which(ebvdb3m$test2ebv==1),]
      cbdata_31$codex=31
      cbdata_32=ebvdb3m[which(ebvdb3m$test2ebv==2),]
      cbdata_32$codex=32
      
     
      cbdata=rbind(cbdata_10,cbdata_11,cbdata_20,cbdata_21,cbdata_22,cbdata_30,cbdata_31,cbdata_32)
      cbdata$codex=as.factor(cbdata$codex)
      
      
      paintKP("codex","progress","progresstime",data=cbdata)
      
      cbdata$newcode=as.factor(ifelse(cbdata$codex==22,3,
                                      ifelse(cbdata$codex %in% c(21,11,32),2,
                                             ifelse(cbdata$codex %in% c(31,20,10),1,0)
                                      )
      ));table(cbdata$newcode)
      
      paintKP("newcode","progress","progresstime",data=cbdata)
      
      
    }
    
    
    
  }
  
 
  #our result
  if(TRUE){
  
    ebvdb3m$sumall_3best=as.factor(ifelse( ebvdb3m$sumall<=2,0,ifelse( ebvdb3m$sumall<=9,1,2)));table(ebvdb3m$sumall_3best)
    paintKP("sumall_3best","progress","progresstime",data=ebvdb3m)
    
    
    
   
    ebvdb3m$EBV3m_3best=as.factor(ifelse( ebvdb3m$EBV3m<=0,0,ifelse( ebvdb3m$EBV3m<=2.040,1,2)));table(ebvdb3m$EBV3m_3best)
    
    paintKP("EBV3m_3best","progress","progresstime",data=ebvdb3m)
    
    
    
  
    ebvdb3m$ebv_3best=as.factor(ifelse( ebvdb3m$x29x<1,0,ifelse( ebvdb3m$x29x<26,1,2)));table(ebvdb3m$ebv_3best)
   
    paintKP("ebv_3best","progress","progresstime",data=ebvdb3m)
    
   
    
  }
  
  #temp finally
  if(FALSE){
    if(TRUE){
      cbdata_10=ebvdb3m[which(ebvdb3m$sumall_3best==0),]
      cbdata_10$codex=10
      cbdata_11=ebvdb3m[which(ebvdb3m$sumall_3best==1),]
      cbdata_11$codex=11
      cbdata_12=ebvdb3m[which(ebvdb3m$sumall_3best==2),]
      cbdata_12$codex=12
      
      cbdata_20=ebvdb3m[which(ebvdb3m$EBV3m_3best==0),]
      cbdata_20$codex=20
      cbdata_21=ebvdb3m[which(ebvdb3m$EBV3m_3best==1),]
      cbdata_21$codex=21
      cbdata_22=ebvdb3m[which(ebvdb3m$EBV3m_3best==2),]
      cbdata_22$codex=22
      
      cbdata_30=ebvdb3m[which(ebvdb3m$ebv_3best==0),]
      cbdata_30$codex=30
      cbdata_31=ebvdb3m[which(ebvdb3m$ebv_3best==1),]
      cbdata_31$codex=31
      cbdata_32=ebvdb3m[which(ebvdb3m$ebv_3best==2),]
      cbdata_32$codex=32
      
      # cbdata_30=ebvdb3m[which(ebvdb3m$x29x2f==0),]
      # cbdata_30$codex=30
      # cbdata_31=ebvdb3m[which(ebvdb3m$x29x2f==1),]
      # cbdata_31$codex=31
      # cbdata=rbind(cbdata_10,cbdata_11,cbdata_20,cbdata_21,cbdata_22,cbdata_30,cbdata_31)
      cbdata=rbind(cbdata_10,cbdata_11,cbdata_12,cbdata_20,cbdata_21,cbdata_22,cbdata_30,cbdata_31,cbdata_32)
      cbdata$codex=as.factor(cbdata$codex)
      
      
      paintKP("codex","progress","progresstime",data=cbdata,cexnrisk = 0.3)
      # table()
      
      if(FALSE){
        splots=list()
        splots[[1]]=myggplot(thiscolum="codex","progress","progresstime",legend_title = paste0(c("PFS:","combine"),collapse = ""),
                             legend_labs = c("trg<=2","trg<=9","trg>9","postEBV=0","postEBV<=2.04","postEBV>2.04","preEBV<1","preEBV<26","preEBV>=26"),thisdata=cbdata,thisbreak = 12,sentp = "AUTO")
        # splots[[2]]=myggplot(thiscolum="EBV3m_3best","progress","progresstime",legend_title = paste0(c("PFS:","postEBV"),collapse = ""),legend_labs = c("0","<=2.04",">2.04"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
        # splots[[3]]=myggplot(thiscolum="ebv_3best","progress","progresstime",legend_title = paste0(c("PFS:","preEBV"),collapse = ""),legend_labs = c("<1","<26",">=26"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
        # # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","N1:pure>=6mm"),collapse = ""),legend_labs = c("none","IC"),thisdata=N01_data[N01_data$RLN_divide_6mm==2,],thisbreak = 12,sentp = "AUTO",ifadjusted=c("Ttwo","x10x","test2ebv"))
        
        # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
        
        # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
        
        res <- arrange_ggsurvplots(splots,ncol = 1, nrow = 1, print = FALSE)
        
        # pdf( paste0( c("P:/npc3/乙肝对鼻咽癌的研究_诱导化疗/pdf/乙肝进行诱导化疗有害","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
        # res
        # dev.off()
        
        ggsave( paste0( c("ouput.pdf"),collapse = ""), plot=res,width =20, height = 30, units = "cm") # 1row 2 col
        
      }
      
      
      
      if(TRUE){
        cbdata$newcode=as.factor(ifelse(cbdata$codex==22,3,
                                        ifelse(cbdata$codex %in% c(12,21),2,
                                               ifelse(cbdata$codex %in% c(32,11,31,10,20),1,0)
                                        )
        ));table(cbdata$newcode)
        
        paintKP("newcode","progress","progresstime",data=cbdata)
        
        
        
        if(FALSE){
          splots=list()
          splots[[1]]=myggplot(thiscolum="newcode","progress","progresstime",legend_title = paste0(c("PFS:","combine"),collapse = ""),
                               legend_labs = c("preEBV=0","other","trg>9 | preEBV<=2.04","postEBV>2.04"),thisdata=cbdata,thisbreak = 12,sentp = "AUTO")
          # splots[[2]]=myggplot(thiscolum="EBV3m_3best","progress","progresstime",legend_title = paste0(c("PFS:","postEBV"),collapse = ""),legend_labs = c("0","<=2.04",">2.04"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # splots[[3]]=myggplot(thiscolum="ebv_3best","progress","progresstime",legend_title = paste0(c("PFS:","preEBV"),collapse = ""),legend_labs = c("<1","<26",">=26"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","N1:pure>=6mm"),collapse = ""),legend_labs = c("none","IC"),thisdata=N01_data[N01_data$RLN_divide_6mm==2,],thisbreak = 12,sentp = "AUTO",ifadjusted=c("Ttwo","x10x","test2ebv"))
          
          # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
          
          # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
          
          res <- arrange_ggsurvplots(splots,ncol = 1, nrow = 1, print = FALSE)
          
          # pdf( paste0( c("P:/npc3/乙肝对鼻咽癌的研究_诱导化疗/pdf/乙肝进行诱导化疗有害","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
          # res
          # dev.off()
          
          ggsave( paste0( c("input",".pdf"),collapse = ""), plot=res,width =15, height = 15, units = "cm") # 1row 2 col
          
        }
        
        
        
        ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,3,
                                            ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1,2,
                                                   ifelse(ebvdb3m$ebv_3best==0,0,1)
                                            )
        ))
        
        #os best
        ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,3,
                                            ifelse(ebvdb3m$EBV3m_3best==1,2,
                                                   ifelse(ebvdb3m$ebv_3best==0,0,1)
                                            )
        ))
        
        
        
        # ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
        #                                     ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1 | ebvdb3m$ebv_3best==2,1,0)
        # ));table(ebvdb3m$changeCode)
        
        #64 276  18  29 
        paintKP("changeCode","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode)
        paintKP("changeCode","dead","deadtime",data=ebvdb3m);table(ebvdb3m$changeCode)
        coxP("changeCode+ajcc",data=ebvdb3m)
        paintKP("changeCode","moveornot","movetime",data=ebvdb3m);table(ebvdb3m$changeCode)
        paintKP("changeCode","relapse","relapsetime",data=ebvdb3m);table(ebvdb3m$changeCode)
        # paintKP("changeCode","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode)
        
        #330  28  29
        paintKP("EBV3m_3best","progress","progresstime",data=ebvdb3m);table(ebvdb3m$EBV3m_3best)
        
        table(ebvdb3m[,c("changeCode","EBV3m_3best")])
        
        
        ebvdb3m$changeCode_killsum=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
                                                    ifelse(ebvdb3m$EBV3m_3best==1 | ebvdb3m$ebv_3best==2,1,0)
        ));table(ebvdb3m$changeCode_killsum)
        #245 113  29
        paintKP("changeCode_killsum","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode_killsum)
        
        
        
        ebvdb3m$changeCode_killebv=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
                                                    ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1,1,0)
        ));table(ebvdb3m$changeCode_killebv)
        # 314  44  29
        paintKP("changeCode_killebv","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode_killebv)
        
        
        
        
        
      }
      
      
      
      
    }
    
    
    
  }
  
  # finally
  if(TRUE){
    if(TRUE){
      
      #原始过程
      if(TRUE){
        cbdata_10=ebvdb3m[which(ebvdb3m$sumall_3best==0),]
        cbdata_10$codex=20
        cbdata_11=ebvdb3m[which(ebvdb3m$sumall_3best==1),]
        cbdata_11$codex=21
        cbdata_12=ebvdb3m[which(ebvdb3m$sumall_3best==2),]
        cbdata_12$codex=22
        
        cbdata_20=ebvdb3m[which(ebvdb3m$EBV3m_3best==0),]
        cbdata_20$codex=30
        cbdata_21=ebvdb3m[which(ebvdb3m$EBV3m_3best==1),]
        cbdata_21$codex=31
        cbdata_22=ebvdb3m[which(ebvdb3m$EBV3m_3best==2),]
        cbdata_22$codex=32
        
        cbdata_30=ebvdb3m[which(ebvdb3m$ebv_3best==0),]
        cbdata_30$codex=10
        cbdata_31=ebvdb3m[which(ebvdb3m$ebv_3best==1),]
        cbdata_31$codex=11
        cbdata_32=ebvdb3m[which(ebvdb3m$ebv_3best==2),]
        cbdata_32$codex=12
        
        # cbdata_30=ebvdb3m[which(ebvdb3m$x29x2f==0),]
        # cbdata_30$codex=30
        # cbdata_31=ebvdb3m[which(ebvdb3m$x29x2f==1),]
        # cbdata_31$codex=31
        # cbdata=rbind(cbdata_10,cbdata_11,cbdata_20,cbdata_21,cbdata_22,cbdata_30,cbdata_31)
        cbdata=rbind(cbdata_10,cbdata_11,cbdata_12,cbdata_20,cbdata_21,cbdata_22,cbdata_30,cbdata_31,cbdata_32)
        cbdata$codex=as.factor(cbdata$codex)
        
        
        paintKP("codex","progress","progresstime",data=cbdata,cexnrisk = 0.3)
        
        
        # table()
        
        if(FALSE){
          splots=list()
          splots[[1]]=myggplot(thiscolum="codex","progress","progresstime",legend_title = paste0(c("PFS:","combine"),collapse = ""),
                               legend_labs = c("preEBV<1","preEBV<26","preEBV>=26","trg<=2","trg<=9","trg>9","postEBV=0","postEBV<=2.04","postEBV>2.04"),thisdata=cbdata,thisbreak = 12,sentp = "AUTO")
          # splots[[2]]=myggplot(thiscolum="EBV3m_3best","progress","progresstime",legend_title = paste0(c("PFS:","postEBV"),collapse = ""),legend_labs = c("0","<=2.04",">2.04"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # splots[[3]]=myggplot(thiscolum="ebv_3best","progress","progresstime",legend_title = paste0(c("PFS:","preEBV"),collapse = ""),legend_labs = c("<1","<26",">=26"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","N1:pure>=6mm"),collapse = ""),legend_labs = c("none","IC"),thisdata=N01_data[N01_data$RLN_divide_6mm==2,],thisbreak = 12,sentp = "AUTO",ifadjusted=c("Ttwo","x10x","test2ebv"))
          
          # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
          
          # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
          
          res <- arrange_ggsurvplots(splots,ncol = 1, nrow = 1, print = FALSE)
          
          # pdf( paste0( c("P:/npc3/乙肝对鼻咽癌的研究_诱导化疗/pdf/乙肝进行诱导化疗有害","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
          # res
          # dev.off()
          
          ggsave( paste0( c("input,".pdf"),collapse = ""), plot=res,width =20, height = 30, units = "cm") # 1row 2 col
          
          coxP_all("codex",data=cbdata)
          
        }
        
      }
      
      
     
      if(FALSE){
        cbdata$newcode=as.factor(ifelse(cbdata$codex==32,3,
                                        ifelse(cbdata$codex %in% c(22,31),2,
                                               ifelse(cbdata$codex %in% c(12,21,11,20,30),1,0)
                                        )
        ));table(cbdata$newcode)
        
        paintKP("newcode","progress","progresstime",data=cbdata)
        
        
        
        if(FALSE){
          splots=list()
          splots[[1]]=myggplot(thiscolum="newcode","progress","progresstime",legend_title = paste0(c("PFS:","combine"),collapse = ""),
                               legend_labs = c("preEBV=0","other","trg>9 | 0<preEBV<=2.04","postEBV>2.04"),thisdata=cbdata,thisbreak = 12,sentp = "AUTO")
          # splots[[2]]=myggplot(thiscolum="EBV3m_3best","progress","progresstime",legend_title = paste0(c("PFS:","postEBV"),collapse = ""),legend_labs = c("0","<=2.04",">2.04"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # splots[[3]]=myggplot(thiscolum="ebv_3best","progress","progresstime",legend_title = paste0(c("PFS:","preEBV"),collapse = ""),legend_labs = c("<1","<26",">=26"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO")
          # # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","N1:pure>=6mm"),collapse = ""),legend_labs = c("none","IC"),thisdata=N01_data[N01_data$RLN_divide_6mm==2,],thisbreak = 12,sentp = "AUTO",ifadjusted=c("Ttwo","x10x","test2ebv"))
          
          # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
          
          # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
          
          res <- arrange_ggsurvplots(splots,ncol = 1, nrow = 1, print = FALSE)
          
          # pdf( paste0( c("input","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
          # res
          # dev.off()
          
          ggsave( paste0( c("input",".pdf"),collapse = ""), plot=res,width =15, height = 15, units = "cm") # 1row 2 col
          
          coxP_all("newcode",data=cbdata)
          
        }
        
        
        
        ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,3,
                                            ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1,2,
                                                   ifelse(ebvdb3m$ebv_3best==0,0,1)
                                            )
        ))
        
        #os best
        ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,3,
                                            ifelse(ebvdb3m$EBV3m_3best==1,2,
                                                   ifelse(ebvdb3m$ebv_3best==0,0,1)
                                            )
        ))
        
        
        
        # ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
        #                                     ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1 | ebvdb3m$ebv_3best==2,1,0)
        # ));table(ebvdb3m$changeCode)
        
        #64 276  18  29 
        paintKP("changeCode","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode)
        paintKP("changeCode","dead","deadtime",data=ebvdb3m);table(ebvdb3m$changeCode)
        coxP("changeCode+ajcc",data=ebvdb3m)
        paintKP("changeCode","moveornot","movetime",data=ebvdb3m);table(ebvdb3m$changeCode)
        paintKP("changeCode","relapse","relapsetime",data=ebvdb3m);table(ebvdb3m$changeCode)
        # paintKP("changeCode","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode)
        
        #330  28  29
        paintKP("EBV3m_3best","progress","progresstime",data=ebvdb3m);table(ebvdb3m$EBV3m_3best)
        
        table(ebvdb3m[,c("changeCode","EBV3m_3best")])
        
       
        ebvdb3m$changeCode_killsum=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
                                                    ifelse(ebvdb3m$EBV3m_3best==1 | ebvdb3m$ebv_3best==2,1,0)
        ));table(ebvdb3m$changeCode_killsum)
        #245 113  29
        paintKP("changeCode_killsum","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode_killsum)
        
        
      
        ebvdb3m$changeCode_killebv=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,2,
                                                    ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1,1,0)
        ));table(ebvdb3m$changeCode_killebv)
        # 314  44  29
        paintKP("changeCode_killebv","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode_killebv)
        
        
        
        
        
      }
      
      
      
      
      
    }
    
    
    
  }
  
  
  #OS\DMFS
  if(TRUE){
    
    # 61 253  44  29 
    ebvdb3m$changeCode=as.factor(ifelse(ebvdb3m$EBV3m_3best==2,3,
                                        ifelse(ebvdb3m$sumall_3best==2 | ebvdb3m$EBV3m_3best==1,2, #穷举这里的切割值
                                               ifelse(ebvdb3m$ebv_3best!=0,1,0)##或直接深度将这个组再细分
                                               # ifelse(ebvdb3m$test2ebv!=0 ,1,0)
                                        )
    ));table(ebvdb3m$changeCode)
    
   
    
    coxP_all("changeCode",data=ebvdb3m)
    coxP_all("changeCode+hualiao",data=ebvdb3m)
    paintKP("changeCode","progress","progresstime",data=ebvdb3m);table(ebvdb3m$changeCode)
    paintKP("changeCode","dead","deadtime",data=ebvdb3m);table(ebvdb3m$changeCode)
    paintKP("changeCode","moveornot","movetime",data=ebvdb3m);table(ebvdb3m$changeCode)
    paintKP("changeCode","relapse","relapsetime",data=ebvdb3m);table(ebvdb3m$changeCode)
    
    
    if(FALSE){
      splots=list()
      splots[[1]]=myggplot(thiscolum="changeCode","dead","deadtime",legend_title = paste0(c("OS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[2]]=myggplot(thiscolum="changeCode","relapse","relapsetime",legend_title = paste0(c("RFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[3]]=myggplot(thiscolum="changeCode","moveornot","movetime",legend_title = paste0(c("DMFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[4]]=myggplot(thiscolum="changeCode","progress","progresstime",legend_title = paste0(c("PFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=ebvdb3m,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      
      # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
      
      # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
      
      res <- arrange_ggsurvplots(splots,ncol = 2, nrow = 2, print = FALSE)
      
      # pdf( paste0( c("input","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
      # res
      # dev.off()
      
      ggsave( paste0( c("input","final",".pdf"),collapse = ""), plot=res,width =30, height = 30, units = "cm") # 1row 2 col
      
      coxP_all("changeCode",data=ebvdb3m)
      
    }
    
    
    
    # ebvdb3m[which(ebvdb3m$changeCode),]
    
  }
  
  
  #对照组：final:nomogram二分类,对照决策树算法的4分类
  #predict_ronghe,divide_ronghe,dtree4f
  if(TRUE){
    # ：final:nomogram二分类,
    if(TRUE){
      thisvariable=c("EBV3m_3best","x11x","Ntwo")
      # mypath="output" #手动创建文件夹
      
      
      
      myobjectstr=paste0(thisvariable,collapse = "+")
      myformular=as.formula(paste0(c("msurv~",myobjectstr),collapse = ""))
      myobject=coxP(myobjectstr,ebvdb3m,"progresstime","progress",ireturn = "object",ifprintobj = FALSE)
      
      #设定要存放在什么变量中：：这里存放在predict_base
      ebvdb3m$predict_ronghe=predict(myobject,newdata=ebvdb3m);ebvdb3m$predict_EBV3m_sex_N=ebvdb3m$predict_ronghe
      
      
      ebvdb3m$divide_ronghe=as.factor(ifelse(ebvdb3m$predict_ronghe<0,0,1));ebvdb3m$EBV3m_sex_N_risk=ebvdb3m$divide_ronghe
    }
    
    #dtree4f
    if(TRUE){
      ebvdb3m$dtree4f=as.factor(ifelse(ebvdb3m$EBV3m>=0.43,
                                       # ifelse(totalData$x29x<1335.5,2,3)
                                       ifelse(ebvdb3m$x29x<27.45,2,3),
                                       ifelse(ebvdb3m$sumall<5.5,0,1)
      ));table(ebvdb3m$dtree4f)
    }
    
    
  }
  
}

##9分可以区分post-EBV 为0
if(FALSE){
  # trydata_yes=totalData
  # HBsAg_yes="BMI"
  
  summary(ebvdb3m$sumall)
  
  ebvdb3m$sumall9cut=as.factor(ifelse(ebvdb3m$sumall<=9,0,1));table(ebvdb3m$sumall9cut)
  ebvdb3m$postEBV_0f=as.factor(ifelse(ebvdb3m$EBV3m<=0,0,1));table(ebvdb3m$postEBV_0f)
  
  ebvdb3m$sumall3f
  
  splots=list()
  splots[[1]]=myggplot(thiscolum="sumall3f","dead","deadtime",legend_title = paste0(c("OS:","sumall9cut"),collapse = ""),legend_labs = c("<=2","<=9","9"),thisdata=ebvdb3m,ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  splots[[2]]=myggplot(thiscolum="sumall3f","relapse","relapsetime",legend_title = paste0(c("LRFS:","sumall9cut"),collapse = ""),legend_labs = c("<=2","<=9","9"),thisdata=ebvdb3m,ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  splots[[3]]=myggplot(thiscolum="sumall3f","moveornot","movetime",legend_title = paste0(c("DMFS:","sumall9cut"),collapse = ""),legend_labs = c("<=2","<=9","9"),thisdata=ebvdb3m,ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  splots[[4]]=myggplot(thiscolum="sumall3f","progress","progresstime",legend_title = paste0(c("PFS:","sumall9cut"),collapse = ""),legend_labs = c("<=2","<=9","9"),thisdata=ebvdb3m[which(ebvdb3m$EBV3m==0),],ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  
  myggplot(thiscolum="sumall3f","progress","progresstime",legend_title = paste0(c("PFS:","sumall9cut"),collapse = ""),legend_labs = c("<=2","<=9","9"),thisdata=ebvdb3m[which(ebvdb3m$EBV3m==0),],ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  
  myggplot(thiscolum="sumall9cut","progress","progresstime",legend_title = paste0(c("PFS:","sumall9cut"),collapse = ""),legend_labs = c("<=9","9"),thisdata=ebvdb3m[which(ebvdb3m$EBV3m==0),],ifadjusted = c("x2x","x6x","x10x","test2ebv"),sentp = "AUTO")
  
  # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
  
  res <- arrange_ggsurvplots(splots,ncol = 2, nrow = 2, print = FALSE)
  
  # pdf( paste0( c("input","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
  # res
  # dev.off()
  
  ggsave( paste0( c("input",".pdf"),collapse = ""), plot=res,width = 30, height = 30, units = "cm") # 1row 2 col
  
}


#table 1 & 单因素分析整合输出版:找出混杂因素
if(TRUE){
  
  
  # View(thisdata[which(is.na(thisdata$deadtime) ),tailcols])
  
  if(TRUE){
    # 3m
    table1headcols=c("x10x","x11x","who","test2ebv","ebv_3best","x2x","Ttwo","x6x","Ntwo","ajcc","ajcctwo","hualiao","辅助化疗","EBV3m_3best","sumall_3best","dead","moveornot","relapse","progress")
    # table1factors=c(table1headcols,c(setdiff(thistryname,c("咽后IMRT3mNG","咽颈3mNG均有阳性") ) ))
    table1factors=table1headcols
    # #6m
    # table1headcols=c("x10x","x11x","who","test1ebv","x32x_3切割","乳酸脱氢酶.LDHUL._切割","x2x","Ttwo","x6x","Ntwo","ajcc","ajcctwo","hualiao","dead","moveornot","relapse","progress")
    # table1factors=c(table1headcols,c("EBV6m2f",setdiff(need6torun,c("咽后IMRT6mNG","咽颈6mNG均有阳性") ) ) )
    # 
    
    # names(thisdata)
    # # summary(read6mEbv[,"咽后T2WI低信号"])
    # 
    # summary(thisdata$x11x)
    # trainData=rbind(trainData,validData)
    # trainData=rbind(trainData,validData,testData)
    # testData=validData
    
    #合并
    if(FALSE){
      mycolsna=intersect(names(trainData),names(testData))
      myuniqueid=unique( tempnamco$x1x );length(myuniqueid)
      otherids=setdiff(myuniqueid,trainData$x1x)
      tempnamco=rbind(trainData[which(trainData$x1x %in% myuniqueid),mycolsna],testData[which(testData$x1x %in% otherids),mycolsna])
      huasi_data=tempnamco
      trainData=huasi_data
      
      length( intersect(thisdata$x1x,validData$x1x)   ) 
    }
    
    
    #训练与验证一致时，卡方检验只取一列，代表只是想用来生成分布
    
    #二分类
    if(TRUE){
      trainData=ebvdb3m[which(ebvdb3m$divide_ronghe==0),]
      testData=ebvdb3m[which(ebvdb3m$divide_ronghe==1),]
    }
    
    
    #TRG法
    if(TRUE){
      
      trainData=ebvdb3m[which(ebvdb3m$sumall_3best==1),]
      testData=ebvdb3m[which(ebvdb3m$sumall_3best!=1),]
    }
    
    
    #dtree4f
    if(TRUE){
      table(ebvdb3m$dtree4f)
      trainData=ebvdb3m[which(ebvdb3m$dtree4f==2),]
      testData=ebvdb3m[which(ebvdb3m$dtree4f==3),]
    }
    
    
    #result
    if(TRUE){
      trainData=ebvdb3m[which(ebvdb3m$changeCode==2),]
      testData=ebvdb3m[which(ebvdb3m$changeCode==3),]
    }
    
    
    
    
    
    
    # IQRornot=TRUE #连续变量时，是否使用median (IQR)生成分布，FALSE时median (mean~max),默认TRUE
    IQRornot=TRUE
    
    #注，不进行稀有变量检查，因此卡方检验遇到稀有变量，进入下面的卡方补刀
  }
  
  
  
  #table 1卡方表格---支持连续变量版：chiTables纯结果，kafang#制作表格
  if(TRUE){
    # trainData=trainData[which(trainData$x36x==0),]
    # names(trainData)
    # 
    # # twoData[,table1factors]
    # str(complete_data[,c("who")])
    # checkSame("who",trainData,testData,ifreturn = "db",ifchangename = TRUE);
    # table(newTotalData[,c("ajcc","who")])
    # table(complete_data[,c("ajcc","who")])
    # t.test(trainData[,cname],testData[,cname])
    
    thispeidui=c()
    chiTables=c()
    # thisiii=0
    for(cname in table1factors){
      print(cname)
      if(cname=="1"){
        
      }else{
        
        if(is.factor(trainData[,cname])){
          thisrow=checkSame(cname,trainData,testData,ifreturn = "db",ifchangename = TRUE);
          # checkSame(cname,trainData,testData);
          chiTables=rbind(chiTables,c(cname,changeNames(cname),checkSame(cname,trainData,testData) ))
          
        }else{
          
          #连续变量时，是否使用median (IQR)生成分布，FALSE时median (mean~max)
          if(IQRornot){
            train_su=summary(trainData[,cname])
            train_su_str=paste0(c(round(train_su[3],1),"(",round(train_su[2],1),"~",round(train_su[5],1),")" ),collapse = "")
            test_su=summary(testData[,cname])
            test_su_str=paste0(c(round(test_su[3],1),"(",round(test_su[2],1),"~",round(test_su[5],1),")" ),collapse = "")
          }else{
            train_su=summary(trainData[,cname])
            train_su_str=paste0(c(round(train_su[3],1),"(",round(train_su[1],1),"~",round(train_su[6],1),")" ),collapse = "")
            test_su=summary(testData[,cname])
            test_su_str=paste0(c(round(test_su[3],1),"(",round(test_su[1],1),"~",round(test_su[6],1),")" ),collapse = "")
          }
          
          
          thispsustr=t.test(trainData[,cname],testData[,cname])
          
          chiTables=rbind(chiTables,c(cname,changeNames(cname),thispsustr$p.value))
          
          thisrow=cbind(cname,nrow(trainData),train_su_str,nrow(testData),test_su_str,train_su_str,test_su_str,round( thispsustr$p.value,3 ) )
          thisrow=as.data.frame(thisrow)
          colnames(thisrow)=c("variables","leftnum","v3","rightnum","v5","v6","v7","pvalue")
          
        }
        # print(thisrow)
        thispeidui=rbind(thispeidui,thisrow)
        
        # thisiii= thisiii+1
        # if(thisiii==2){
        #   break;
        # }
        
        
        
        # str(thisrow)
      }
      
    }
    thispeidui=as.data.frame(thispeidui) #完整变格
    
    
    thispeidui$leftnum=as.integer(as.character(thispeidui$leftnum))
    thispeidui$rightnum=as.integer(as.character(thispeidui$rightnum))
    thispeidui$totalnum=thispeidui$leftnum+thispeidui$rightnum
    thispeidui$totalper=ifelse(is.na(thispeidui$totalnum),NA,round(thispeidui$totalnum/(nrow(trainData)+nrow(testData))*100,1) )
    thispeidui$total=apply( thispeidui[,c("totalnum","totalper")],1,FUN=function(x){
      if(is.na(x[1])){
        return(x[1])
      }
      
      
      return( paste0(c(x[1],"(",x[2],"%)"),collapse = "") )
    })
    
    thistable1=thispeidui[,c("variables","total","v6","v7","pvalue")]
    thistable1$total=ifelse( is.na(thistable1$total),"", thistable1$total)
    
    kafang=thistable1 #不同的输出要注意改这个
    
    # View(thistable1[,c(1,3)])
    # kafang=thistable1 #制作表格
    # chiTables=as.data.frame(thistable1) #但纯得到P值
    # chiTables$V3=as.numeric(thistable1$V3)
    
    
    #连续变量
    if(FALSE){
      
      if(TRUE){
        
        thistable1=table1detail("who",trainData,"dead","deadtime")
        thistable1=table1detail("who",validData,"dead","deadtime")
        # thistable1=table1detail("who",testData,"dead","deadtime")
        thistable1=table1detail("who",complete_data[which(complete_data$ajcc %in% c(3,4)),],"dead","deadtime")
        
        # 补刀WHO
        table(trainData[,"who"])
        table(complete_data[,"who"])
        fisher.test(t(cbind(c(1257,63),c(386,0))))
        # fisher.test(t(cbind(c(351,969),c(94,292))))
        
        cname="x10x"
        cname="淋巴细胞绝对值.10E9L."
        cname="deadtime"
        
        continue_vars=summary(complete_data[,cname])
        print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
        # 
        continue_vars=summary(trainData[,cname])
        print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
        
        continue_vars=summary(testData[,cname])
        print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
        
        t.test(trainData[,cname],testData[,cname])
        
        coxP_all(cname,trainData)
        coxP_all("x11x",trainData)
        
        t.test(trainData[,cname],testData[,cname])
        
        # t.test(trainData[,cname],testData[,cname])
      }
      
    }
    
    #补刀，C反应蛋白
    if(FALSE){
      
      train_su=summary(trainData[,"x32x"])
      train_su_str=paste0(c(round(train_su[3],1),"(",round(train_su[2],1),"~",round(train_su[5],1),")" ),collapse = "")
      
      table(trainData[,"x32x_3切割"])
      table(trainData[,"x32x_ROCcut"])
      
      
      coxP("x32x",trainData,"deadtime","dead")
      coxP(paste0(c("x32x","x2x","x6x","test1ebv","x10x"),collapse = "+"),trainData,"deadtime","dead")
      
      coxP("x32x_3切割",trainData,"deadtime","dead")
      coxP(paste0(c("x32x_3切割","x2x","x6x","test1ebv","x10x"),collapse = "+"),trainData,"deadtime","dead")
      
      coxP("x32x_ROCcut",trainData,"deadtime","dead")
      coxP(paste0(c("x32x_ROCcut","x2x","x6x","test1ebv","x10x"),collapse = "+"),trainData,"deadtime","dead")
    }
    
  }
  
  #3m单因素分析，
  if(TRUE){
    #"ALLLNnecrosisnumber","age","LNsize",
    
    # table1factors=predictors_reads
    
    # table1factors=c("x11x","who","test2ebv","x2x","x6x","ajcc","hualiao","necrosis","necrosis3F","ENS","Retronecrosis","UpperLNNecrosis","LowerLNNecrosis","CLNnecrosis","Necrosisside","NC_detail","max60over")
    # table2factors=setdiff(table1factors,c("x10x"))
    thistable1=table1detail(table1factors,ebvdb3m,"dead","deadtime")
    thistable2=table1detail(table1factors,ebvdb3m,"moveornot","movetime")
    thistable3=table1detail(table1factors,ebvdb3m,"relapse","relapsetime")
    thistable4=table1detail(table1factors,ebvdb3m,"progress","progresstime")
    
    thistable=cbind(thistable1[,c(1,2,3,6)],thistable2[,c(3,6)],thistable3[,c(3,6)],thistable4[,c(3,6)])
    # View(thistable)
    
    # 补刀
    if(FALSE){
      
      # table(trainData[,"who"])
      # table(complete_data[,"who"])
      # fisher.test(t(cbind(c(1257,63),c(386,0))))
      # 
      # fisher.test(t(cbind(c(1222,1,97),c(322,1,63))))
      # 
      # fisher.test(t(cbind(c(351,969),c(94,292))))
      
      cname="x10x"
      cname="ALLLNnecrosisnumber"
      # cname="x504x"
      cname="deadtime"
      
      continue_vars=summary(huasi_data[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      # 
      continue_vars=summary(trainData[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      
      continue_vars=summary(testData[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      
      t.test(trainData[,cname],testData[,cname])
      
      coxP_all(cname,trainData)
      coxP_all("x11x",trainData)
      
      t.test(trainData[,cname],testData[,cname])
      
      # t.test(trainData[,cname],testData[,cname])
      
      # mycolsna=intersect(names(trainData),names(testData))
      # myuniqueid=unique( tempnamco$x1x )
      # tempnamco=rbind(trainData[which(trainData$x1x %in% myuniqueid),mycolsna],testData[which(testData$x1x %in% myuniqueid),mycolsna])
      # huasi_data=tempnamco
      
      fit <- prodlim(Surv(time=deadtime,event=dead) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(huasi_data[,"dead"])
      
      fit <- prodlim(Surv(time=movetime,event=moveornot) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(huasi_data[,"moveornot"])
      
      
      fit <- prodlim(Surv(time=relapsetime,event=relapse) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(huasi_data[,"relapse"])
      
      fit <- prodlim(Surv(time=progresstime,event=progress) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(huasi_data[,"progress"])
      
      
      summary(huasi_data[,"deadtime"])
      
    }
  }
  
  #6m单因素分析，
  if(FALSE){  ##用完注销
    #"ALLLNnecrosisnumber","age","LNsize",
    
    # table1factors=predictors_reads
    
    # table1factors=c("x11x","who","test2ebv","x2x","x6x","ajcc","hualiao","necrosis","necrosis3F","ENS","Retronecrosis","UpperLNNecrosis","LowerLNNecrosis","CLNnecrosis","Necrosisside","NC_detail","max60over")
    # table2factors=setdiff(table1factors,c("x10x"))
    thistable1=table1detail(table1factors,validData,"dead","deadtime")
    thistable2=table1detail(table1factors,validData,"moveornot","movetime")
    thistable3=table1detail(table1factors,validData,"relapse","relapsetime")
    thistable4=table1detail(table1factors,validData,"progress","progresstime")
    
    thistable=cbind(thistable1[,c(1,2,3,6)],thistable2[,c(3,6)],thistable3[,c(3,6)],thistable4[,c(3,6)])
    # View(thistable)
    
    # 补刀
    if(FALSE){
      
      # table(trainData[,"who"])
      # table(complete_data[,"who"])
      # fisher.test(t(cbind(c(1257,63),c(386,0))))
      # 
      # fisher.test(t(cbind(c(1222,1,97),c(322,1,63))))
      # 
      # fisher.test(t(cbind(c(351,969),c(94,292))))
      
      cname="x10x"
      cname="ALLLNnecrosisnumber"
      # cname="x504x"
      cname="deadtime"
      
      continue_vars=summary(huasi_data[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      # 
      continue_vars=summary(trainData[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      
      continue_vars=summary(testData[,cname])
      print(paste0(c(continue_vars[3],"(",continue_vars[2],"~",continue_vars[5],")" ),collapse = ""))
      
      t.test(trainData[,cname],testData[,cname])
      
      coxP_all(cname,trainData)
      coxP_all("x11x",trainData)
      
      t.test(trainData[,cname],testData[,cname])
      
      # t.test(trainData[,cname],testData[,cname])
      
      fit <- prodlim(Surv(time=deadtime,event=dead) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(complete_data[,"dead"])
      
      fit <- prodlim(Surv(time=movetime,event=moveornot) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(complete_data[,"moveornot"])
      
      
      fit <- prodlim(Surv(time=relapsetime,event=relapse) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(complete_data[,"relapse"])
      
      fit <- prodlim(Surv(time=progresstime,event=progress) ~ 1,data=huasi_data)
      predict(fit,times=c(60))
      table(complete_data[,"progress"])
    }
  }
  
  # 合并卡方检验及单因素分析
  if(TRUE) {
    mytable1Final=cbind(kafang,thistable[,3:10])
    View(mytable1Final)
    if(FALSE){
      write.csv(mytable1Final,"P:/npc3/曹迪治疗后EBV/temp/mytable2.csv",row.names = FALSE)
    }
    
  }
  
  
}


#多因素分析
if(TRUE){
  thisy=coxP("sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo",data=ebvdb3m,"progresstime","progress",ireturn = "summary",ifstep=FALSE);thisy
  
  thisy=coxP("EBV3m_3best+x11x+Ntwo",data=ebvdb3m,"progresstime","progress",ireturn = "summary",ifstep=FALSE);thisy
  
  thisy=coxP("changeCode+x11x+Ntwo",data=ebvdb3m,"progresstime","progress",ireturn = "summary",ifstep=FALSE);thisy
  
  thisy=coxP("sumall_3best+ebv_3best+EBV3m_3best+x10x+Ttwo+Ntwo",data=ebvdb3m,"deadtime","dead",ireturn = "summary",ifstep=FALSE);thisy
  thisy=coxP("changeCode+x10x+Ttwo+Ntwo",data=ebvdb3m,"deadtime","dead",ireturn = "summary",ifstep=FALSE);thisy
  
  thisy=coxP("sumall_3best+ebv_3best+EBV3m_3best+Ntwo",data=ebvdb3m,"movetime","moveornot",ireturn = "summary",ifstep=FALSE);thisy
  thisy=coxP("changeCode+Ntwo",data=ebvdb3m,"movetime","moveornot",ireturn = "summary",ifstep=FALSE);thisy
  
  thisy=coxP("sumall_3best+ebv_3best+EBV3m_3best+x11x",data=ebvdb3m,"relapsetime","relapse",ireturn = "summary",ifstep=FALSE);thisy
  thisy=coxP("changeCode+x11x",data=ebvdb3m,"relapsetime","relapse",ireturn = "summary",ifstep=FALSE);thisy
  
  paintKP("codex","progress","progresstime",data=cbdata,cexnrisk = 0.3)
  thisy=coxP("codex+x11x+Ntwo",data=cbdata,"progresstime","progress",ireturn = "summary",ifstep=FALSE);thisy
  
  thisyd=as.data.frame(cbind(thisy$coefficients,thisy$conf.int))
  thisyd$rangehr=apply(thisyd[,c(6,8,9)],1,FUN=function(x){
    temphr= paste0(c(
      round(as.numeric(as.character(x[1])),2),"(",
      round(as.numeric(as.character(x[2])),2),",",
      round(as.numeric(as.character(x[3])),2),")"
    ),collapse = "")
    return(temphr)
  })
  
  View(thisyd[,c(1,10,5)]) #mostly
  # View(thisyd[,c(1,10,5)])
  
  if(FALSE){
    cindexCompare2(cnameformlua="sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo",
                   baseformula="ebv_3best+EBV3m_3best+x11x+Ntwo",
                   thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    
    cindexCompare2(cnameformlua="sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo",
                   baseformula="test2ebv+EBV3m_3best+x11x+Ntwo",
                   thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    
    cindexCompare2(cnameformlua="sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo",
                   baseformula="EBV3m_3best+x11x+Ntwo",
                   thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    
    cindexCompare2(cnameformlua="changeCode+x11x+Ntwo",
                   baseformula="EBV3m_3best+x11x+Ntwo",
                   thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    # ebvdb3m$EBV3m2f
    # cindexCompare2(cnameformlua="changeCode+x11x+Ntwo",
    #                baseformula="EBV3m2f+x11x+Ntwo",
    #                thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    # 
    # cindexCompare2(cnameformlua="changeCode",
    #                baseformula="EBV3m2f",
    #                thistime="progresstime",thisevent="progress",traindb=ebvdb3m,validb=ebvdb3m,ifpaint=FALSE,ifeasy = TRUE,iifstep = FALSE,ifprintobj = TRUE)
    # 
    
    
  }
  
  
  
}


#批量生成cindex及两两间P值 
if(FALSE){
  
  # ebvdb3m$dtree4f=as.factor(ifelse(ebvdb3m$EBV3m>0.43,
  #                                    # ifelse(totalData$x29x<1335.5,2,3)
  #                                    ifelse(ebvdb3m$x29x<27.45,2,3),
  #                                    ifelse(ebvdb3m$sumall<5.5,0,1)
  # ));table(ebvdb3m$dtree4f)
  # 
  # paintKP("dtree4f","progress","progresstime",data=ebvdb3m)
  # 
  
  # View( cindexCompare(cnameformlua=paste0(c("咽后IMRT3mCNN","EBV3m3f+Ntwo+test2ebv"),collapse = "+"),
  #                     baseformula="EBV3m3f+Ntwo+test2ebv",
  #                     # baseformula="淋巴细胞绝对值_3切割+乳酸脱氢酶.LDHUL._ROCcut+pre.PNI_ROCcut+单核细胞绝对值.10E9L._ROCcut+x2x+x6x+x10x+test1ebv",
  #                     thistime="movetime",thisevent="moveornot",traindb=trainData,validb=validData,iifstep =FALSE,ifeasy = TRUE) )
  # 
  # View( cindexCompare(cnameformlua=paste0(c("咽后IMRT3mCNN","EBV3m3f"),collapse = "+"),
  #                     baseformula="咽后IMRT3mCNN+EBV3m3f",
  #                     # baseformula="淋巴细胞绝对值_3切割+乳酸脱氢酶.LDHUL._ROCcut+pre.PNI_ROCcut+单核细胞绝对值.10E9L._ROCcut+x2x+x6x+x10x+test1ebv",
  #                     thistime="progresstime",thisevent="progress",traindb=trainData,validb=validData,iifstep =FALSE,ifeasy = TRUE) )
  # 
  
  #这个好用，生成cindex表---模板
  #DMFS,最好一步步点
  if(FALSE){
    
    #设定,prepare
    if(TRUE){
      #设定结局
      thistime="progresstime"
      thisevent="progress"
      
      traindb=ebvdb3m
      validb=ebvdb3m
      
      
      #设定全部要对比的formulas
      allformulas=c("Ntwo",
                    
                    "ebv_3best",
                    "ebv_3best+x11x+Ntwo",
                    
                    "EBV3m2f",
                    "EBV3m2f+x11x+Ntwo",
                    
                    "EBV3m_3best",
                    "EBV3m_3best+x11x+Ntwo",
                    "EBV3m_3best+ebv_3best+x11x+Ntwo",
                    
                    
                    # "EBV3m2f",
                    # "EBV3m2f+x11x+Ntwo",
                    # "EBV3m2f+test2ebv+x11x+Ntwo",
                    
                    
                    "sumall_3best",
                    "sumall_3best+x11x+Ntwo",
                    "sumall_3best+ebv_3best+x11x+Ntwo",
                    
                    "sumall_3best+EBV3m_3best",
                    "sumall_3best+EBV3m_3best+x11x+Ntwo",
                    
                    
                    "sumall_3best+ebv_3best+EBV3m_3best",
                    "sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo",
                    
                    "divide_ronghe",
                    "divide_ronghe+x11x+Ntwo",
                    
                    
                    "dtree4f",
                    "dtree4f+x11x+Ntwo",
                    
                    "changeCode",
                    "changeCode+x11x+Ntwo"
                    
                    
                    
                    
                    
                    
      )
      #设定一个主要对比的东西，主表，也是用来生成一个table瞧瞧先：
      # baseformula="咽后IMRT3mCNN+EBV3m3f+Ntwo+test2ebv"
      baseformula="sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo"
      # baseformula="EBV3m2f"
      # baseformula="EBV3m3f+咽后IMRT3mCNN"
    }
    
    
    # 生成主表及cindex
    if(TRUE){
      thisrow=c()
      for(cname in allformulas){
        
        # Tthiscindex=cindexCompare(cnameformlua=paste0(c("x2x+x6x+testebv",cname),collapse = "+"),baseformula="x2x+x6x+testebv",thistime="movetime",thisevent="moveornot",traindb=trainData,validb=testData,iifstep =FALSE)
        
        Tthiscindex=cindexCompare(cnameformlua=cname,baseformula=baseformula,thistime=thistime,thisevent=thisevent,traindb=traindb,validb=validb,iifstep =FALSE)
        # View(T1cindex)
        
        A11=paste0(c( round(Tthiscindex[1,1],3),"(",round(Tthiscindex[1,2],3),"~",round(Tthiscindex[1,3],3),")"   ),collapse = "") 
        A12=round(Tthiscindex[1,11],3)
        
        A13=paste0(c( round(Tthiscindex[2,1],3),"(",round(Tthiscindex[2,2],3),"~",round(Tthiscindex[2,3],3),")"   ),collapse = "") 
        A14=round(Tthiscindex[2,11],3)
        
        thisrow=rbind(thisrow,c(cname,A11,A12,A13,A14))
      }
      thisrow=as.data.frame(thisrow)
      View(thisrow)
    }
    
    
    #做次表，所有方程两两间的P值，不一定用到
    if(TRUE){
      trainresults=c()
      testresults=c()
      for(reffername in allformulas){
        # reffername="predict_T1"
        print(reffername);
        thisrow=c()
        for(cname in allformulas){
          Tthiscindex=cindexCompare(cnameformlua=cname,baseformula=reffername,thistime=thistime,thisevent=thisevent,traindb=traindb,validb=validb,iifstep =FALSE)
          # View(T1cindex)
          
          A11=paste0(c( round(Tthiscindex[1,1],3),"(",round(Tthiscindex[1,2],3),"~",round(Tthiscindex[1,3],3),")"   ),collapse = "") 
          A12=round(Tthiscindex[1,11],3)
          
          A13=paste0(c( round(Tthiscindex[2,1],3),"(",round(Tthiscindex[2,2],3),"~",round(Tthiscindex[2,3],3),")"   ),collapse = "") 
          A14=round(Tthiscindex[2,11],3)
          
          thisrow=rbind(thisrow,c(cname,A11,A12,A13,A14))
          
          
        }
        thisrow=as.data.frame(thisrow)
        trainrow=as.character(thisrow$V3)
        testrow=as.character(thisrow$V5)
        
        trainresults=rbind(trainresults,trainrow)
        testresults=rbind(testresults,testrow)
        
        # print(c(trainrow,testrow))
        
      }
      trainresults=as.data.frame(trainresults)
      testresults=as.data.frame(testresults)
      View(trainresults)
      View(testresults)
    }
    
    
    
  }
  
  
  #PFS
  if(TRUE){
    #设定,prepare
    if(TRUE){
      #设定结局
      thistime="progresstime"
      thisevent="progress"
      
      traindb=trainData
      validb=validData
      
      
      #设定全部要对比的formulas
      allformulas=c("Ntwo",
                    "Ntwo+x11x",
                    "test2ebv",
                    "Ntwo+test2ebv+x11x",
                    "Ttwo+Ntwo+test2ebv+x11x",
                    "EBV3m3f",
                    "咽后IMRT3mCNN",
                    "咽后IMRT3mCNN+EBV3m3f",
                    "咽后IMRT3mCNN+Ntwo+x11x",
                    "EBV3m3f+Ntwo+test2ebv+x11x",
                    "咽后IMRT3mCNN+EBV3m3f+Ntwo+test2ebv+x11x")
      #设定一个主要对比的东西，主表，也是用来生成一个table瞧瞧先：
      # baseformula="咽后IMRT3mCNN+EBV3m3f+Ntwo+test2ebv"
      baseformula="咽后IMRT3mCNN+EBV3m3f"
    }
    
    
    # 生成主表及cindex
    if(TRUE){
      thisrow=c()
      for(cname in allformulas){
        
        # Tthiscindex=cindexCompare(cnameformlua=paste0(c("x2x+x6x+testebv",cname),collapse = "+"),baseformula="x2x+x6x+testebv",thistime="movetime",thisevent="moveornot",traindb=trainData,validb=testData,iifstep =FALSE)
        
        Tthiscindex=cindexCompare(cnameformlua=cname,baseformula=baseformula,thistime=thistime,thisevent=thisevent,traindb=traindb,validb=validb,iifstep =FALSE)
        # View(T1cindex)
        
        A11=paste0(c( round(Tthiscindex[1,1],3),"(",round(Tthiscindex[1,2],3),"~",round(Tthiscindex[1,3],3),")"   ),collapse = "") 
        A12=round(Tthiscindex[1,11],3)
        
        A13=paste0(c( round(Tthiscindex[2,1],3),"(",round(Tthiscindex[2,2],3),"~",round(Tthiscindex[2,3],3),")"   ),collapse = "") 
        A14=round(Tthiscindex[2,11],3)
        
        thisrow=rbind(thisrow,c(cname,A11,A12,A13,A14))
      }
      thisrow=as.data.frame(thisrow)
      View(thisrow)
    }
    
    
    #做次表，所有方程两两间的P值，不一定用到
    if(TRUE){
      trainresults=c()
      testresults=c()
      for(reffername in allformulas){
        # reffername="predict_T1"
        print(reffername);
        thisrow=c()
        for(cname in allformulas){
          Tthiscindex=cindexCompare(cnameformlua=cname,baseformula=reffername,thistime=thistime,thisevent=thisevent,traindb=traindb,validb=validb,iifstep =FALSE)
          # View(T1cindex)
          
          A11=paste0(c( round(Tthiscindex[1,1],3),"(",round(Tthiscindex[1,2],3),"~",round(Tthiscindex[1,3],3),")"   ),collapse = "") 
          A12=round(Tthiscindex[1,11],3)
          
          A13=paste0(c( round(Tthiscindex[2,1],3),"(",round(Tthiscindex[2,2],3),"~",round(Tthiscindex[2,3],3),")"   ),collapse = "") 
          A14=round(Tthiscindex[2,11],3)
          
          thisrow=rbind(thisrow,c(cname,A11,A12,A13,A14))
          
          
        }
        thisrow=as.data.frame(thisrow)
        trainrow=as.character(thisrow$V3)
        testrow=as.character(thisrow$V5)
        
        trainresults=rbind(trainresults,trainrow)
        testresults=rbind(testresults,testrow)
        
        # print(c(trainrow,testrow))
        
      }
      trainresults=as.data.frame(trainresults)
      testresults=as.data.frame(testresults)
      View(trainresults)
      View(testresults)
    }
    
    
    
  }
  
}


#画nomogram\DCA画图:prepare model
# 减少了一步；风险由divide_ronghe定义或分开：model_risk base_risk
# 降低上下文依赖性
if(TRUE){
  
  #画图--支撑材料-小图;;同时尝试一下高低风险组
  if(TRUE){
    
    #0.先运行一次
    if(TRUE){
      testData=ebvdb3m
      totalData=ebvdb3m
      trainData=ebvdb3m
      trainData$msurv=Surv(time=as.double(trainData[,"progresstime"]),event=as.double(trainData[,"progress"]))
      testData$msurv=Surv(time=as.double(testData[,"progresstime"]),event=as.double(testData[,"progress"]))
      totalData$msurv=Surv(time=as.double(totalData[,"progresstime"]),event=as.double(totalData[,"progress"]))
    }
    
    ##第一步,setting::手动点下面一个个，每次只点一个;当全部执行时，得到所有新字段
    ##设定要运行的模型
    #prepare
    if(TRUE){
      
      
      #"EBV3m_3best","x11x","Ntwo"
      if(TRUE){
        thisvariable=c("EBV3m_3best","x11x","Ntwo")
        mypath="P://npc3/曹迪MRTRG/pdf/EBV3m_sex_N/" #手动创建文件夹
        
        
        
        myobjectstr=paste0(thisvariable,collapse = "+")
        myformular=as.formula(paste0(c("msurv~",myobjectstr),collapse = ""))
        myobject=coxP(myobjectstr,trainData,"progresstime","progress",ireturn = "object",ifprintobj = FALSE)
        
        #设定要存放在什么变量中：：这里存放在predict_base
        totalData$predict_ronghe=predict(myobject,newdata=totalData);totalData$predict_EBV3m_sex_N=totalData$predict_ronghe
        trainData$predict_ronghe=predict(myobject,newdata=trainData); trainData$predict_EBV3m_sex_N=trainData$predict_ronghe
        testData$predict_ronghe=predict(myobject,newdata=testData); testData$predict_EBV3m_sex_N=testData$predict_ronghe
        
        trainData$divide_ronghe=as.factor(ifelse(trainData$predict_ronghe<0,0,1));trainData$EBV3m_sex_N_risk=trainData$divide_ronghe
        testData$divide_ronghe=as.factor(ifelse(testData$predict_ronghe<0,0,1));testData$EBV3m_sex_N_risk=testData$divide_ronghe
        totalData$divide_ronghe=as.factor(ifelse(totalData$predict_ronghe<0,0,1));totalData$EBV3m_sex_N_risk=totalData$divide_ronghe
        
        
        dd=datadist(trainData)
        #warnings()
        options(datadist="dd")
        
      }
      
      #changeCode+x11x+Ntwo
      if(TRUE){
        thisvariable=c("changeCode","x11x","Ntwo")
        mypath="P://npc3/曹迪MRTRG/pdf/changeCode_sex_N/" #手动创建文件夹
        
        
        
        myobjectstr=paste0(thisvariable,collapse = "+")
        myformular=as.formula(paste0(c("msurv~",myobjectstr),collapse = ""))
        myobject=coxP(myobjectstr,trainData,"progresstime","progress",ireturn = "object",ifprintobj = FALSE)
        
        #设定要存放在什么变量中：：这里存放在predict_base
        totalData$predict_ronghe=predict(myobject,newdata=totalData);totalData$predict_changeCode_sex_N=totalData$predict_ronghe
        trainData$predict_ronghe=predict(myobject,newdata=trainData); trainData$predict_changeCode_sex_N=trainData$predict_ronghe
        testData$predict_ronghe=predict(myobject,newdata=testData); testData$predict_changeCode_sex_N=testData$predict_ronghe
        
        trainData$divide_ronghe=as.factor(ifelse(trainData$predict_ronghe<0,0,1));trainData$changeCode_sex_N_risk=trainData$divide_ronghe
        testData$divide_ronghe=as.factor(ifelse(testData$predict_ronghe<0,0,1));testData$changeCode_sex_N_risk=testData$divide_ronghe
        totalData$divide_ronghe=as.factor(ifelse(totalData$predict_ronghe<0,0,1));totalData$changeCode_sex_N_risk=totalData$divide_ronghe
        
        
        dd=datadist(trainData)
        #warnings()
        options(datadist="dd")
        
      }
      
      #sumall_3best+ebv_3best+EBV3m_3best+x11x+Ntwo
      if(TRUE){
        thisvariable=c("sumall_3best","ebv_3best","EBV3m_3best","x11x","Ntwo")
        mypath="input" #手动创建文件夹
        
        
        
        myobjectstr=paste0(thisvariable,collapse = "+")
        myformular=as.formula(paste0(c("msurv~",myobjectstr),collapse = ""))
        myobject=coxP(myobjectstr,trainData,"progresstime","progress",ireturn = "object",ifprintobj = FALSE)
        
        #设定要存放在什么变量中：：这里存放在predict_base
        totalData$predict_ronghe=predict(myobject,newdata=totalData);totalData$predict_final=totalData$predict_ronghe
        trainData$predict_ronghe=predict(myobject,newdata=trainData); trainData$predict_final=trainData$predict_ronghe
        testData$predict_ronghe=predict(myobject,newdata=testData); testData$predict_final=testData$predict_ronghe
        
        trainData$divide_ronghe=as.factor(ifelse(trainData$predict_ronghe<0,0,1));trainData$final_risk=trainData$divide_ronghe
        testData$divide_ronghe=as.factor(ifelse(testData$predict_ronghe<0,0,1));testData$final_risk=testData$divide_ronghe
        totalData$divide_ronghe=as.factor(ifelse(totalData$predict_ronghe<0,0,1));totalData$final_risk=totalData$divide_ronghe
        
        
        dd=datadist(trainData)
        #warnings()
        options(datadist="dd")
        
      }
      
      
      
      
    }
    
    
    
    
    ##第二步
    
    if(FALSE){
      
      #nomogram
      if(TRUE){
        pdf( paste0(c(mypath,"1.nomogram_test2.pdf"),collapse = "") ,width =10 )
        # try_nomogram1=my_nomogram(myformular ,trainData=trainData,thisStatus="progress",thisparcex=1.1 ,
        #                           funat=c(1.00,0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)
        # )
        # try_nomogram1=my_nomogram35Years(myformular ,trainData=trainData,thisStatus="progress",thisparcex=1.1 ,
        #                                  funat=c(1.00,0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0)
        # )
        
        try_nomogram1=my_nomogram35Years(myformular ,trainData=trainData,thisStatus="progress",thisparcex=1.1 ,
                                         funat=c(1.00,0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.3,0.2,0.1,0),ifcutpoints = TRUE,thisTime = "progresstime"
        )
        
        
        
        # my_nomogram35Years
        plot(try_nomogram1, xfrac=.3)
        dev.off()
        
      }
      
      #cali
      if(TRUE){
        # pdf( paste0(c(mypath,"2.cali_test1000.pdf"),collapse = ""),width =10,height = 10 )
        # layout(matrix(c(1:4),2,2,byrow=T)) #2行两列
        # #记得发表的时候，将thisboot改为1000，10是用于快速生成
        # # myformular=as.formula(paste0(c("msurv~",paste0(base_variables,collapse = "+") ),collapse = ""))
        # # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=10,subtitles = FALSE,setmonths=36)
        # # myformular=as.formula(paste0(c("msurv~",paste0(c("predict_T1","predict_T1C"),collapse = "+") ),collapse = ""))
        # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=1000,subtitles = FALSE,setmonths=36,cutpoint = 3,iferror = TRUE)
        # 
        # # myformular=as.formula(paste0(c("msurv~",paste0(c("predict_T1","predict_T1C"),collapse = "+") ),collapse = ""))
        # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=1000,subtitles = FALSE,setmonths=60,cutpoint = 3,iferror = TRUE)
        # dev.off()
        
        
        # pdf( paste0(c(mypath,"2.cali_test1000.pdf"),collapse = ""),width =10,height = 10 )
        # layout(matrix(c(1:4),2,2,byrow=T)) #2行两列
        # #记得发表的时候，将thisboot改为1000，10是用于快速生成
        # # myformular=as.formula(paste0(c("msurv~",paste0(base_variables,collapse = "+") ),collapse = ""))
        # # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=10,subtitles = FALSE,setmonths=36)
        # # myformular=as.formula(paste0(c("msurv~",paste0(c("predict_T1","predict_T1C"),collapse = "+") ),collapse = ""))
        # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=1000,subtitles = FALSE,setmonths=36,cutpoint = 5,iferror = TRUE,xlim = c(0.6,1))
        # # dev.off()
        # 
        # # pdf( paste0(c(mypath,"2.cali_5years.pdf"),collapse = ""),width =10,height = 5 )
        # # layout(matrix(c(1:2),1,2,byrow=T)) #2行两列
        # # myformular=as.formula(paste0(c("msurv~",paste0(c("predict_T1","predict_T1C"),collapse = "+") ),collapse = ""))
        # my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=1000,subtitles = FALSE,setmonths=60,cutpoint = 5,iferror = TRUE,xlim = c(0.6,1))
        # 
        # 
        # dev.off()
        
        
        pdf( paste0(c(mypath,"2.cali_5years.pdf"),collapse = ""),width =10,height = 5 )
        layout(matrix(c(1:2),1,2,byrow=T)) #2行两列
        # myformular=as.formula(paste0(c("msurv~",paste0(c("predict_T1","predict_T1C"),collapse = "+") ),collapse = ""))
        my_cali_bymonth(myformular,trainData,testData,"progress",thisboot=1000,subtitles = FALSE,setmonths=60,cutpoint = 5,iferror = TRUE,xlim = c(0.5,1))
        dev.off()
        
      }
      
      #score  #要手动
      if(FALSE){
        
        
        thisA=my_ggdotchart(mypredict="predict_ronghe",trainData,thisStatus="progress",ylab = "Radscores",legend_title="Primary"
                            ,ylim=c(min( trainData$predict_ronghe),max( trainData$predict_ronghe)),fontsize = 16,sentname="metastasis")
        
        thisB= my_ggdotchart(mypredict="predict_ronghe",testData,thisStatus="progress",ylab = "Radscores",legend_title="Validation"
                             ,ylim=c(min( trainData$predict_ronghe),max( trainData$predict_ronghe)),fontsize = 16,sentname="metastasis")
        
        
        thisAB=ggarrange(thisA, thisB,labels = NULL,ncol = 2, nrow = 1)
        
        
        pdf( paste0(c(mypath,"3.scores.pdf"),collapse = ""),width =10,height=4 )
        thisAB
        # Sys.sleep(3)
        dev.off()
      }
      
      
      #KM #要手动
      if(FALSE){
        splots=list()
        splots[[1]]=myggplot(thiscolum="divide_ronghe","progress","progresstime",legend_title = paste0(c("Primary",""),collapse = ""),legend_labs = c("low", "high"),thisdata=trainData,ifadjusted =c("Ntwo","x11x"),thisbreak = 12,sentp = "AUTO")
        splots[[2]]=myggplot(thiscolum="divide_ronghe","progress","progresstime",legend_title = paste0(c("Validation",""),collapse = ""),legend_labs = c("low", "high"),thisdata=testData,ifadjusted =c("Ntwo","x11x"),thisbreak = 12,sentp = "AUTO")
        res <- arrange_ggsurvplots(splots,ncol = 2, nrow =1, print = FALSE);
        pdf( paste0(c(mypath,"4.KMs.pdf"),collapse = ""),width =10,height=5 )
        res
        dev.off()
        
        
        splots=list()
        splots[[1]]=myggplot(thiscolum="divide_ronghe","dead","deadtime",legend_title = paste0(c("OS",""),collapse = ""),legend_labs = c("low", "high"),thisdata=trainData,ifadjusted =c("x10x","x2x","Ntwo"),thisbreak = 12,sentp = "AUTO")
        splots[[2]]=myggplot(thiscolum="divide_ronghe","relapse","relapsetime",legend_title = paste0(c("LRFS",""),collapse = ""),legend_labs = c("low", "high"),thisdata=testData,ifadjusted =c("x11x"),thisbreak = 12,sentp = "AUTO")
        splots[[3]]=myggplot(thiscolum="divide_ronghe","moveornot","movetime",legend_title = paste0(c("DMFS",""),collapse = ""),legend_labs = c("low", "high"),thisdata=testData,ifadjusted =c("Ntwo"),thisbreak = 12,sentp = "AUTO")
        splots[[4]]=myggplot(thiscolum="divide_ronghe","progress","progresstime",legend_title = paste0(c("PFS",""),collapse = ""),legend_labs = c("low", "high"),thisdata=testData,ifadjusted =c("Ntwo","x11x"),thisbreak = 12,sentp = "AUTO")
        res <- arrange_ggsurvplots(splots,ncol = 2, nrow =2, print = FALSE);
        pdf( paste0(c(mypath,"4.all_KMs.pdf"),collapse = ""),width =10,height=10 )
        res
        dev.off()
        
      }
      
      
      #DCA不整合版
      if(FALSE){
        
        library(rmda)
        library(RColorBrewer )
        thisstatus="progress"
        setStatus=thisstatus
        plotData=trainData
        # plotData=testData
        
        
        #合并数据
        # myconls=intersect(trainData,testData);plotData=rbind(trainData[,myconls],testData[,myconls])
        
        
        # plotData=testData
        # plotData=testData[which( (testData$x6x==3 & testData$x2x==4)==FALSE ),] #test
        plotData[,setStatus]=as.numeric(as.character(plotData[,setStatus]))
        
        
        # myformulas=c("msurv~predict_TN","msurv~clinical")
        myformulas=c("msurv~predict_TN","msurv~base","msurv~predict_ronghe")
        
        
        setcutoff=0.5
        #几个复杂模型对比::train
        if(FALSE){
          TN= decision_curve(formula=as.formula("progress~predict_TN"),
                             data = plotData,
                             thresholds = seq(0, setcutoff, by = .01),
                             bootstraps = 10)
          
          TNEBV= decision_curve(formula=as.formula("progress~predict_TNEBV"),
                                data = plotData,
                                thresholds = seq(0, setcutoff, by = .01),
                                bootstraps = 10)
          
          TN3mEBV= decision_curve(formula=as.formula("progress~predict_TN3mEBV"),
                                  data = plotData,
                                  thresholds = seq(0,setcutoff, by = .01),
                                  bootstraps = 10)
          
          
          Nebv3mebv= decision_curve(formula=as.formula("progress~predict_Nebv3mebv"),
                                    data = plotData,
                                    thresholds = seq(0,setcutoff, by = .01),
                                    bootstraps = 10)
          
          
          
          thismodel= decision_curve(formula=as.formula("progress~predict_model"),
                                    data = plotData,
                                    thresholds = seq(0,setcutoff, by = .01),
                                    bootstraps = 10)
          
          
          pdf( paste0(c(mypath,"5.training_DCA.pdf"),collapse = ""),width =10,height=10 )
          pdf( paste0(c(mypath,"5.testing_DCA.pdf"),collapse = ""),width =10,height=10 )
          pdf( paste0(c(mypath,"5.training_plus_testing_DCA.pdf"),collapse = ""),width =10,height=10 )
          
          
          
          plot_decision_curve(x=list(TN,TNEBV,Nebv3mebv,thismodel),curve.names= c("TN","TNEBV","Nebv3mebv","model"),
                              cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                              ylim = c(-0.01, 0.15), #set ylim
                              confidence.intervals =FALSE,standardize = FALSE)
          
          
          
          plot_decision_curve(x=list(TN,TNEBV,TN3mEBV,thismodel),curve.names= c("TN","TNEBV","TN3m","thismodel"),
                              cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                              ylim = c(-0.01, 0.15), #set ylim
                              confidence.intervals =FALSE,standardize = FALSE)
          
          # plot(y=0.5,add=TRUE)
          # mtext(paste0(thisStatus,"训练-decision_curve"),3)
          dev.off()
          
          # dev.off()
        }
        
        
        
      }
      
      
      #heatmap ---work
      if(FALSE){
        
        library(corrplot)
        library(RColorBrewer)
        
        #没有基础变量的时候
        if(FALSE){
          # https://blog.csdn.net/gavin_cdc/article/details/102608232?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase
          mycorvalu=c(thisvariable,"progress","x2x","x6x","ajcc","testebv","vol","predict_ronghe")
          tempdb=apply(totalData[,mycorvalu],1,FUN=function(x){return(as.numeric(as.character(x))  )})
          # str(tempdb)
          tempdb=as.data.frame(t(tempdb))
          colnames(tempdb)=c(thisvariable,"Metastasis","T classification","N classification","Clinical stage","EBV","Volume","Radscores")
          thiscor=cor(tempdb,method = c("spearman"))
        }
        
        #没有基础变量的时候
        if(FALSE){
          
          # totalData$ajcc
          # https://blog.csdn.net/gavin_cdc/article/details/102608232?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase
          mycorvalu=c(thisvariable,"progress","vol","predict_ronghe")
          tempdb=apply(totalData[,mycorvalu],1,FUN=function(x){return(as.numeric(as.character(x))  )})
          # str(tempdb)
          tempdb=as.data.frame(t(tempdb))
          colnames(tempdb)=c("T classification","N classification","EBV",setdiff(thisvariable,c("x2x","x6x","testebv")),"Metastasis","Volume","Predicted scores")
          thiscor=cor(tempdb,method = c("spearman"))
        }
        
        #没有基础变量的时候
        if(TRUE){
          # https://blog.csdn.net/gavin_cdc/article/details/102608232?utm_medium=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase&depth_1-utm_source=distribute.pc_relevant.none-task-blog-BlogCommendFromBaidu-7.nonecase
          mycorvalu=c(thisvariable,"progress","vol","predict_ronghe")
          tempdb=apply(totalData[,mycorvalu],1,FUN=function(x){return(as.numeric(as.character(x))  )})
          # str(tempdb)
          tempdb=as.data.frame(t(tempdb))
          colnames(tempdb)=c("N classification","EBV",setdiff(thisvariable,c("x2x","x6x","testebv")),"Metastasis","Volume","Predicted scores")
          thiscor=cor(tempdb,method = c("spearman"))
        }
        
        
        write.csv(thiscor,paste0(c(mypath,"相关系数.csv"),collapse = "") );
        
        pdf( paste0(c(mypath,"5.heatmap.pdf"),collapse = ""),width =10,height=10 )
        # corrplot(thiscor,method = "number",type = "upper")
        # corrplot(thiscor, order = "hclust", addrect = 2, col = terrain.colors(100))
        # corrplot(thiscor, method = "number",order = "hclust", addrect = 2, col = rev(brewer.pal(n = 8, name = "RdBu")) )
        
        corrplot(thiscor, method = "color", col =  rev(brewer.pal(n = 8, name = "RdBu")),
                 type = "upper", 
                 # order = "original",
                 order = "hclust", 
                 number.cex = .7,
                 addCoef.col = "black", # Add coefficient of correlation
                 tl.col = "black", tl.srt = 90, # Text label color and rotation
                 # Combine with significance
                 # p.mat = p.mat, sig.level = 0.01, insig = "blank", 
                 # hide correlation coefficient on the principal diagonal
                 diag = FALSE)
        
        
        dev.off()
        
        
      }
      
      
      
      
      
      
      
    }
    
    
    
    #DCA 
    if(FALSE){
      #画出决策曲线DCA
      
      library(rmda)
      library(RColorBrewer )
      
      setStatus="progress"
      # plotData=testData
      plotData=trainData
      plotData[,setStatus]=as.numeric(as.character(plotData[,setStatus]))
      
      # predict_EBV3m_sex_N
      # predict_changeCode_sex_N
      # predict_final
      # 
      myformulas=c("msurv~predict_EBV3m_sex_N","msurv~predict_changeCode_sex_N")
      
      
      setcutoff=1.0
      #几个复杂模型对比
      if(TRUE){
        EBV3m_sex_N= decision_curve(formula=as.formula("progress~predict_EBV3m_sex_N"),
                                    data = plotData,
                                    thresholds = seq(0, setcutoff, by = .01),
                                    bootstraps = 10)
        
        changeCode_sex_N= decision_curve(formula=as.formula("progress~predict_changeCode_sex_N"),
                                         data = plotData,
                                         thresholds = seq(0, setcutoff, by = .01),
                                         bootstraps = 10)
        
        # yan_bad3m= decision_curve(formula=as.formula("progress~predict_咽后bad3m"),
        #                       data = plotData,
        #                       thresholds = seq(0,setcutoff, by = .01),
        #                       bootstraps = 10)
        
        Final= decision_curve(formula=as.formula("progress~predict_final"),
                              data = plotData,
                              thresholds = seq(0,setcutoff, by = .01),
                              bootstraps = 10)
        
        
        pdf( paste0(c(mypath,"validation_DCA2.pdf"),collapse = ""),width =10,height=10 )
        # pdf( paste0(c(mypath,"training_DCA2.pdf"),collapse = ""),width =10,height=10 )
        # paste0(rev(brewer.pal(n = 6, name = "RdBu")),"")
        plot_decision_curve(x=list(EBV3m_sex_N,Final),curve.names= c("EBV3m_sex_N","Final"),
                            cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                            ylim = c(-0.01, 0.15), #set ylim
                            # plot_decision_curve(x=list(EBV3m_sex_N,changeCode_sex_N,Final),curve.names= c("EBV3m_sex_N","changeCode_sex_N","Final"),
                            #                     cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                            #                     ylim = c(-0.01, 0.15), #set ylim
                            confidence.intervals =FALSE,standardize = FALSE)
        # plot_decision_curve(x=list(TN,TN_ebv_3mebv,yan_bad3m,Rulefit),curve.names= c("TN","TN_ebv_3mEBV","yan_bad3m","Rulefit"),
        #                     cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
        #                     ylim = c(-0.01, 0.15), #set ylim
        #                     confidence.intervals =FALSE,standardize = FALSE)
        # mtext(paste0(thisStatus,"训练-decision_curve"),3)
        
        dev.off()
        
        table(trainData[which(trainData$x6x==3 & trainData$EBV3m2f==1),"progress"])
        
        table(trainData[which(trainData$EBV3m2f==1),"progress"])
        
      }
      
      
    }
    
    ##第四步，尝试一下误导化疗::跑完上面的模型后再折腾，因为上面要保存变量
    if(FALSE){
      # table(totalData[totalData$predict_base>0,"x6x"])
      # table(totalData[totalData$predict_model>0,"x6x"])
      
      coxP_all("youdao",data=trainData[which(trainData$predict_model>0),])
      coxP_all("youdao+x2x+x6x+x10x",data=totalData[which(totalData$predict_model>0),])
      totalData$high_risk_model=as.factor(ifelse(totalData$predict_model>0,1,0))
      
      table(totalData[,c("x500x_cut","high_risk_model")])
      
      # coxP_all("youdao",data=totalData[which(totalData$high_risk_model==1 & totalData$x500x_cut==0),])
      
      splots=list()
      
      splots[[1]]=myggplot(thiscolum="youdao","dead","deadtime",legend_title = paste0(c("OS:","model_high_risk"),collapse = ""),legend_labs = c("CCRT","IC+CCRT"),thisdata=totalData[totalData$predict_model>0,],ifadjusted =c("x2x","x6x","x10x"),thisbreak = 12,sentp = "AUTO")
      splots[[2]]=myggplot(thiscolum="youdao","dead","deadtime",legend_title = paste0(c("OS:","base_high_risk"),collapse = ""),legend_labs = c("CCRT","IC+CCRT"),thisdata=totalData[totalData$predict_base>0,],ifadjusted =c("test2ebv","x2x","x6x","x10x"),thisbreak = 12,sentp = "AUTO")
      res <- arrange_ggsurvplots(splots,ncol = 2, nrow = 1, print = FALSE)
      
      # pdf( paste0( c("input","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
      # res
      # dev.off()
      
      ggsave( paste0( c("output",".pdf"),collapse = ""), plot=res,width = 30, height = 15, units = "cm") # 1row 2 col
      
    }
    
    
  }
  
  
  
}


#仅画最终的分类DCA
if(TRUE){
  ##第三步，DCA对比多个模型：：跑完上面的模型后再折腾，因为上面要保存变量
  #DCA 
  if(FALSE){
    #画出决策曲线DCA
    
    library(rmda)
    library(RColorBrewer )
    
    setStatus="progress"
    # plotData=testData
    plotData=ebvdb3m
    plotData[,setStatus]=as.numeric(as.character(plotData[,setStatus]))
    
    # predict_EBV3m_sex_N
    # predict_changeCode_sex_N
    # predict_final
    # 
    # myformulas=c("msurv~predict_EBV3m_sex_N","msurv~predict_changeCode_sex_N")
    
    
    setcutoff=1.0
    #几个复杂模型对比
    if(TRUE){
      EBV3m_3best= decision_curve(formula=as.formula("progress~EBV3m_3best"),
                                  data = plotData,
                                  thresholds = seq(0, setcutoff, by = .01),
                                  bootstraps = 10)
      
      divide_ronghe= decision_curve(formula=as.formula("progress~divide_ronghe"),
                                    data = plotData,
                                    thresholds = seq(0, setcutoff, by = .01),
                                    bootstraps = 10)
      
      dtree4f= decision_curve(formula=as.formula("progress~dtree4f"),
                              data = plotData,
                              thresholds = seq(0, setcutoff, by = .01),
                              bootstraps = 10)
      
      # yan_bad3m= decision_curve(formula=as.formula("progress~predict_咽后bad3m"),
      #                       data = plotData,
      #                       thresholds = seq(0,setcutoff, by = .01),
      #                       bootstraps = 10)
      
      Final= decision_curve(formula=as.formula("progress~changeCode"),
                            data = plotData,
                            thresholds = seq(0,setcutoff, by = .01),
                            bootstraps = 10)
      
      
      pdf( paste0(c("input","DCA4个3.pdf"),collapse = ""),width =10,height=10 )
      # pdf( paste0(c(mypath,"training_DCA2.pdf"),collapse = ""),width =10,height=10 )
      # paste0(rev(brewer.pal(n = 6, name = "RdBu")),"")
      plot_decision_curve(x=list(EBV3m_3best,divide_ronghe,dtree4f,Final),curve.names= c("EBV3m_3best","divide_ronghe","dtree4f","Final"),
                          cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                          ylim = c(-0.01, 0.15), #set ylim
                          # plot_decision_curve(x=list(EBV3m_sex_N,changeCode_sex_N,Final),curve.names= c("EBV3m_sex_N","changeCode_sex_N","Final"),
                          #                     cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
                          #                     ylim = c(-0.01, 0.15), #set ylim
                          confidence.intervals =FALSE,standardize = FALSE)
      # plot_decision_curve(x=list(TN,TN_ebv_3mebv,yan_bad3m,Rulefit),curve.names= c("TN","TN_ebv_3mEBV","yan_bad3m","Rulefit"),
      #                     cost.benefit.axis =FALSE,col = rev(brewer.pal(n = 5, name = "Set1")),
      #                     ylim = c(-0.01, 0.15), #set ylim
      #                     confidence.intervals =FALSE,standardize = FALSE)
      # mtext(paste0(thisStatus,"训练-decision_curve"),3)
      
      dev.off()
      
      table(trainData[which(trainData$x6x==3 & trainData$EBV3m2f==1),"progress"])
      
      table(trainData[which(trainData$EBV3m2f==1),"progress"])
      
    }
    
    
  }
}


#补决策树建立模型---作为对比
if(TRUE){
  #决策树求切割值----fail
  #注意了：minsplit = 1,minbucket=1,cp=0 是关键参；否则容易出不了结果；maxdept限制深度；method::anova 是类似logistic二分类，method::exp生存分析
  if(FALSE){
    library(rpart);library("visNetwork");library("sparkline")
    totalData=ebvdb3m
    totalData$msurv=Surv( time=as.double(totalData[,"progresstime"]),event=as.double(totalData[,"progress"]) )
    res <- rpart(msurv ~ x29x+EBV3m+sumall, data=totalData,method="anova" ,control =rpart.control(minsplit = 1,minbucket=1,cp=0,maxdepth = 2))
    plot(res)
    # Visualize
    visTree(res, main = "Iris classification Tree",
            width = "80%",  height = "400px")
    
    
    
    #注意了，当求生存分析的时候，使用的是exp
    totalData[,"progresstime"]=ifelse(totalData[,"progresstime"]==0,0.1,totalData[,"progress"])
    totalData$msurv=Surv( time=as.double(totalData[,"progresstime"]),event=as.double(totalData[,"progress"]) )
    res <- rpart(msurv ~ x29x+EBV3m+sumall, data=totalData,method="exp" ,control =rpart.control(minsplit = 3,minbucket=1,cp=0,maxdepth = 2))
    plot(res)
    # Visualize
    visTree(res, main = "Iris classification Tree")
    # visTree(res, main = "Iris classification Tree",
    #         width = "80%",  height = "400px")
    # 
    # totalData$dtree4f=as.factor(ifelse(totalData$EBV3m<0.43,
    #                                    ifelse(totalData$x29x<73.75,0,1),
    #                                    ifelse(totalData$x29x<1335.5,2,3)
    #                                    ));table(totalData$dtree4f)
    # # 0   1   2   3 
    # # 299  51  36   1 
    
    totalData$dtree4f=as.factor(ifelse(totalData$EBV3m>0.43,
                                       # ifelse(totalData$x29x<1335.5,2,3)
                                       ifelse(totalData$x29x<27.45,2,3),
                                       ifelse(totalData$sumall<5.5,0,1)
    ));table(totalData$dtree4f)
    
    paintKP("dtree4f","progress","progresstime",data=totalData)
    
    if(FALSE){
      splots=list()
      splots[[1]]=myggplot(thiscolum="dtree4f","dead","deadtime",legend_title = paste0(c("OS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=totalData,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[2]]=myggplot(thiscolum="dtree4f","relapse","relapsetime",legend_title = paste0(c("RFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=totalData,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[3]]=myggplot(thiscolum="dtree4f","moveornot","movetime",legend_title = paste0(c("DMFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=totalData,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      splots[[4]]=myggplot(thiscolum="dtree4f","progress","progresstime",legend_title = paste0(c("PFS:",""),collapse = ""),legend_labs = c("1","2","3","4"),thisdata=totalData,thisbreak = 12,sentp = "AUTO",ifadjusted=c("1"))
      
      # splots[[4]]=myggplot(thiscolum="youdao","progress","progresstime",legend_title = paste0(c("PFS:","lake"),collapse = ""),legend_labs = c("CCRT", "IC+CCRT"),thisdata=positiveDb,thisbreak = 12,sentp = "AUTO",ifadjusted =c("x2x","x6x","agegroup","test1ebv","x497x","x494x"))
      
      # splots[[4]]=myggplot(thiscolum="BMI_cut","progress","progresstime",legend_title = paste0(c("PFS:",HBsAg_yes),collapse = ""),legend_labs = c("BMI<26.5", "BMI>26.5"),thisdata=trydata_yes,ifadjusted =c("x10x","test2ebv","x2x","x6x"),thisbreak = 12,sentp = "AUTO")
      
      res <- arrange_ggsurvplots(splots,ncol = 2, nrow = 2, print = FALSE)
      
      # pdf( paste0( c("input","all(样式一)",".pdf"),collapse = ""),width =10,height=10 )
      # res
      # dev.off()
      
      ggsave( paste0( c("input","dtree4f",".pdf"),collapse = ""), plot=res,width =30, height = 30, units = "cm") # 1row 2 col
      
      coxP_all("dtree4f",data=totalData)
      
    }
    
  }
  
}



