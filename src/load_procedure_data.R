# load procedure data ----------
load_procedure_data <- function(exclude_ids, cohort = "20220202") {

  if (cohort == "20220202") {
    proc_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220202/Procedures_2022-02-02.txt"
  }
    
  if (cohort == "20220101") {
    proc_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20220101/Procedures_2022-01-02.txt"
  }
  
  if (cohort == "20211001") {
    proc_path <- "/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_5883/20211001/Procedures_2021-10-01.txt"
  }
  
  proc_tested <- fread(proc_path)
  setnames(proc_tested, c("Encrypted_PatientID"), c("id"))
  
  # proc_unmatched <- fread("/net/junglebook/michiganmedicine/data/Data_Pulls_from_Data_Office/HPI_4705_RandomControls/HPI_4705_unmatched_ctrls_Procedures.txt")
  # setnames(proc_unmatched, c("Deid_ID"), c("id"))
  
  out <- proc_tested[, !c("MGI_DeID_PatientId")]
  # out <- rbindlist(list(proc_tested[, !c("MGI_DeID_PatientId")], proc_unmatched[id %notin% exclude_ids]))
  
  return(out)
  
}
