
source("./data_preprocessing/readdata.R")





tsp.mainfunc <- function(iter,list_model,file_path,save_path = "./BenchMarking/modelresults/tspResult/"){
        
        TotalPath = file_path
        
        algorithm.listnames = list.files(TotalPath)
        n = length(algorithm.listnames)
        
        eParaCsv = NULL
        gompertzParaCsv = NULL
        expLinearPara = NULL
        decayParaCsv = NULL
        tsp_relationship = NULL
        count.process = 0
        dir.create(save_path)
        respath = save_path
        
        #######################
        #save modeling curve  #
        #######################
        pdfname = paste(save_path,"_",iter,"_",unlist(list_model)$name,".pdf",sep = "")
        pdf(pdfname)
        par(mfrow = c(2,2))
        
        
        for(i in 1:n){
              
                instance.filepath = paste(TotalPath,algorithm.listnames[i],"/","symmetric",'/',sep = "")
               
                instance.listnames = list.files(instance.filepath)
                m = length(instance.listnames)
                for(j in 1:m){#
                        data.filepath = paste(instance.filepath,instance.listnames[j],sep = "")
                        
                        s = getTheMatrixData_TspSuite(data.filepath)
                        s = s[order(s[,1]),]
                        
                        
                        xData = s[, 1]
                        yData = s[, 3]
                        
                        
                        #####################
                        ##algorithm instances name
                        ####################
                        res.path = paste(algorithm.listnames[i],"/symmetric/",instance.listnames[j],sep = "")
                        
                        globalmodels <- list_model
                        p = findBestFitting(s,xData,yData,globalmodels,iter)
                        p$plotFunction(s,p$par)
                        title(main = res.path)
                        grid()
                        
                        relationship = cbind(p$name,res.path,deparse.level = 0)
                        
                        tsp_relationship = rbind(tsp_relationship,relationship,deparse.level = 0)
                        temp_Para = cbind(p$name,res.path,t(p$par),p$residual,deparse.level = 0)
                        
                        if(grepl("logisticModel",p$name))
                                eParaCsv = rbind(eParaCsv,temp_Para,deparse.level = 0)
                        if(grepl("gompertzModel",p$name))
                                gompertzParaCsv = rbind(gompertzParaCsv,temp_Para,deparse.level = 0)
                        if(grepl("decay",p$name)){
                                decayParaCsv = rbind(decayParaCsv,temp_Para,deparse.level = 0)
                        }
                        if(grepl("expLinearModel",p$name))
                                expLinearPara = rbind(expLinearPara,temp_Para,deparse.level = 0)
                        
                        count.process = count.process + 1
                        
                        print(paste("Process",count.process,"continue...",sep = ""))
                        
                }
        }
        
        dev.off()
        
        ###########
        
        
        
        
        if(!is.null(tsp_relationship)){
                colnames(tsp_relationship) <- c("instance_file","model")
                csvname.rela <- paste(respath,"_relationship.csv",sep = "")
                write.csv(tsp_relationship,csvname.rela,row.names = FALSE,col.names = TRUE)
        }
        
        if(!is.null(eParaCsv)){
                if(ncol(eParaCsv) == 7){
                        colnames(eParaCsv) <- c("model","instance_file","a","b","c","d","residuals")
                }else
                        colnames(eParaCsv) <- c("model","instance_file","a","b","c","residuals")
                
                csvname.log <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                write.csv(eParaCsv,csvname.log,row.names = FALSE,col.names = TRUE)
        }
        
        
        if(!is.null(expLinearPara)){
                
                if(ncol(expLinearPara) == 7){
                        colnames(expLinearPara) <- c("model","instance_file","a","b","c","d","residuals")
                }else
                        colnames(expLinearPara) <- c("model","instance_file","a","b","c","residuals")
                csvname.linear <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                write.csv(expLinearPara,csvname.linear,row.names = FALSE,col.names = TRUE)
        }
        
        if(!is.null(decayParaCsv)){
                if(ncol(decayParaCsv) == 7){
                        colnames(decayParaCsv) <- c("model","instance_file","a","b","c","d","residuals")
                }else
                        colnames(decayParaCsv) <- c("model","instance_file","a","b","c","residuals")
                csvname.decay <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep = "")
                write.csv(decayParaCsv,csvname.decay,row.names = FALSE,col.names = TRUE)
        }
        
        
        if(!is.null(gompertzParaCsv)){
                if(ncol(gompertzParaCsv) == 7){
                        colnames(gompertzParaCsv) <- c("model","instance_file","a","b","c","d","residuals")
                }else
                        colnames(gompertzParaCsv)<- c("model","instance_file","a","b","c","residuals")
                csvname.gomp <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep="")
                write.csv(gompertzParaCsv,csvname.gomp,row.names = FALSE,col.names = TRUE )
        }
        
        
}
