#the up-to-the-minute procedure,and the best one till now/2016/10/23
#rm(list = ls())
#source("../R_Code/core_fitting_procedure.R")

source("./data_preprocessing/readdata.R")




maxsat.mainfunc <- function(iter,list_model,file_path,save_path = "./BenchMarking/OPResults/MaxsatResult/",datavolume = 1,method = 1){
        
        TotalPath = file_path
        respath = save_path
        relationship = NULL
        eParaCsv = NULL
        decayParaCsv = NULL
        expLinearPara = NULL
        gompertzParaCsv = NULL
        firstFileList = list.files(TotalPath)
        firstFileList = firstFileList[-grep("\\.",firstFileList)]
        dir.create(save_path)
        
        #setwd(TotalPath)
        #par(mfrow = c(2,2))
        pdfname = paste(save_path,"_",iter,"_",unlist(list_model)$name,".pdf",sep = "")
        pdf(pdfname)
        par(mfrow = c(2,2))
        
        count.process = 0
        for(jFirstLayer in 1:length(firstFileList)){
                firstFilePath = paste(TotalPath,firstFileList[jFirstLayer],"/",sep = "")
                algorithm_name = paste(firstFileList[jFirstLayer],"/",sep = "")
                filename = paste(save_path ,firstFileList[jFirstLayer],sep = "")
                if(!file.exists(filename)){
                        dir.create(filename)
                }
                a = list.files(firstFilePath)
                for(i in 1:length(a)){
                        secondFilename = paste(filename,"/",a[i],sep = "")
                        if(!file.exists(secondFilename)){
                                dir.create(secondFilename)
                        }
                        if(grepl("\\.",a[i]) == TRUE) i= i+1
                        tryCatch({
                                instance_alogorithm_name = paste(algorithm_name,a[i],"/",sep = "")
                                pathpath = paste(firstFilePath,a[i],"/",sep = "")
                                s = getMatrixData(pathpath)
                                s = s[order(s[, 1]), ]
                        },error = function(e){
                                
                        })
                        
                        if(method == 2){
                                sample.index = sort(sample(1:nrow(s),ceiling(nrow(s)*datavolume)))
                                
                        } else if (method == 1){
                                
                                sample.index = c(1:ceiling(nrow(s)*datavolume))
                        }
                        
                        xData = s[sample.index, 1]
                        yData = s[sample.index, 3]
                        #globalmodels <- list(decayModelpositive)
                        globalmodels <- list_model
                        #  globalmodels <- list(decayModelpositive,linearLogyLogxModel,polynomialModel,logisticModel)
                        
                        p = findBestFitting(s,xData,yData,globalmodels,iter)
                        p$plotFunction(s,p$par)
                        #print(instance_alogorithm_name)
                        title(main = instance_alogorithm_name)
                        grid()
                        ee = cbind(instance_alogorithm_name,p$name)
                        relationship = rbind(relationship,ee,deparse.level = 0)
                        temp_Para = cbind(instance_alogorithm_name,p$name,t(p$par),p$residual,deparse.level = 0)
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
        
        
        if(!is.null(relationship)){
                colnames(relationship) <- c("instance_file","model")
                csvname.rela <- paste(respath,iter,"_",unlist(list_model)$name,"_model.csv",sep="")
                write.csv(relationship,csvname.rela,row.names = FALSE,col.names = TRUE)
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

