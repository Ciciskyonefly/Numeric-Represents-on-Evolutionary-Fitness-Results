remove(list = ls())
start.time = Sys.time()
source("./GN.R")

iter <- c(10,50,100,150,200,250)
#iter = c(150)

#list_model = list(logisticModelpositive)
# list_model = list(decayModelpositive,decayModelpasitive)
# list_model = list(gompertzModelpositive,gompertzModelpositive)
# list_model = list(expLinearModelpositive,expLinearModelpasitive)

list_model <- list(logisticModelpositive,
 decayModelpositive,decayModelpasitive,
 gompertzModelpositive,gompertzModelpasitive)
#                    expLinearModelpositive,expLinearModelpasitive)

run_problem <- function(problem_id){
        if(problem_id == 1){
                source("./Saving_Code/Saving_Relationship_MaxSat_Main_Cici_Frame.R")
                cat("MaxSat Problem:\n")
                for(temp_i in 1:length(iter)){
                        cat("iter:",iter[temp_i],"\n")
                        
                        for(temp_j in 1:length(list_model)){
                                cat("model:",unlist(list_model[temp_j])$name,"\n")
                                
                                file_path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/maxSat/results/"
                                save_path = "./BenchMarking/modelresults/GN.maxsat/"
                                maxsat.mainfunc(iter[temp_i],list_model[temp_j],file_path,save_path)
                                rm(file_path)
                                
                        }
                }
        }
        
        if(problem_id == 2){
                source("./Saving_Code/Saving_Relationship_TspSuite_Main_Cici_Frame.R")
                #source("./Saving_Code/Saving_Relationship_TspSuite_Main_Cici_Frame_2016_05_20.R")
                cat("Tsp Problem:")
                for(temp_i in 1:length(iter)){
                        for(temp_j in 1:length(list_model)){
                                file_path = "D:/BenchMarking/optimizationBenchmarkingDocu-master/optimizationBenchmarkingDocu-master/examples/tspSuite/tsp/"
                                # save_path = "./BenchMarking/modelresults/tspResult/"
                                save_path = "./BenchMarking/modelresults/GN.tsp/"
                                tsp.mainfunc(iter[temp_i],list_model[temp_j],file_path,save_path = save_path)
                                rm(file_path)
                        }
                }  
                
        }
        
}


#run_problem(1)
run_problem(2)

end.time = Sys.time()
print(end.time - start.time)
