# Loading and cleaning Virginia eviction data from Legal Services Corporation
# Authors: Michele Claibourn
# Last revised: 2022-10-26

# create cases2022, hearings2022, plaintiffs2022, defendants2022
# to match prior data (for now)

library(tidyverse)
library(lubridate)

# compare LSC files to virginiacourtdata files
# LSC files
lsc_case <- read_csv("civilcases/2022-10-11/rva_case_information.csv")
lsc_def <- read_csv("civilcases/2022-10-11/rva_defendant_information.csv")
lsc_pla <- read_csv("civilcases/2022-10-11/rva_plaintiff_information.csv")
lsc_hear <- read_csv("civilcases/2022-10-11/rva_hearing_information.csv")
lsc_judg <- read_csv("civilcases/2022-10-11/rva_judgment_information.csv")
lsc_rep <- read_csv("civilcases/2022-10-11/rva_reports.csv")
lsc_serv <- read_csv("civilcases/2022-10-11/rva_service_process.csv")
lsc_garn <- read_csv("civilcases/2022-10-11/rva_garnishment_information.csv")

names(lsc_def)
# rename to: (no def_id, add counter), Name, DBATA, Address, Judgment, Attorney, case_id
names(lsc_def) <- c("case_id", "Name", "Attorney", "DBATA", "Address", "Judgment")
def <- lsc_def %>% 
  mutate(id = row_number()) %>% 
  select(id, Name, DBATA, Address, Judgment, Attorney, case_id)
write_csv(def, "civilcases/DistrictCivil_2022/Defendants.csv")

names(lsc_pla)
# rename to: (no pla_id, add counter), Name, DBATA, Address, Judgment, Attorney, case_id 
names(lsc_pla) <- c("case_id", "Name", "Attorney", "DBATA", "Address", "Judgment")
pla <- lsc_pla %>% 
  mutate(id = row_number()) %>% 
  select(id, Name, DBATA, Address, Judgment, Attorney, case_id)
write_csv(pla, "civilcases/DistrictCivil_2022/Plaintiffs.csv")

names(lsc_hear)
# rename to: (no hear_id, add counter), Date, Result, case_id (remove time, hearing_type, courtroom)
names(lsc_hear) <- c("case_id", "Date", "Time", "Result", "HearingType", "Courtroom")
hear <- lsc_hear %>% 
  mutate(id = row_number(),
         Date = as.Date(Date, "%m/%d/%Y")) %>% 
  select(id, Date, Result, case_id, HearingType, Courtroom)
write_csv(hear, "civilcases/DistrictCivil_2022/Hearings.csv")

names(lsc_case)
# c2dp_case_id, county, fips, case_number, filed_date, case_type, debt_type
# to
# id, ..., fips, (add details_fetched_for_hearing_date), CaseNumber, FiledDate, CaseType, DebtType
names(lsc_case) <- c("id", "county", "fips", "CaseNumber", "FiledDate", "CaseType", "DebtType")
case <- lsc_case %>% 
  mutate(details_fetched_for_hearing_date = "none",
         FiledDate = as.Date(FiledDate, "%m/%d/%Y")) %>% 
  select(id, fips, details_fetched_for_hearing_date, CaseNumber, FiledDate, CaseType,
         DebtType, county)

# add
#  Judgment, Costs, AttorneyFees, PrincipalAmount, OtherAmount, InterestAward,
#  Possession, WritIssuedDate, HomesteadExemptionWaived, IsJudgmentSatisfied, 
#  DateSatisfactionFiled, OtherAwarded, FurtherCaseInformation, Garnishee, Address,
#  GarnisheeAnswer, AnswerDate, NumberofChecksReceived, AppealDate, AppealedBy,
#  collected, WritofEvictionIssuedDate, WritofFieriFaciasIssuedDate

# combine case, judgment, garnishee
names(lsc_judg)
# caseid, judgment, homestead_exemption_waived, is_judgment_satisfied,
#  date_satisfaction_filed, further_case_information, costs, attorney_fees,
#  principal_amount, interest_award, possession, writ_of_eviction_issued_date,
#  other_amount, other_awarded
names(lsc_judg) <- c("id", "Judgment", "HomesteadExemptionWaived", "IsJudgmentSatisfied",
                     "DateSatisfactionFiled", "FurtherCaseInformation", "Costs",
                     "AttorneyFees", "PrincipalAmount", "InterestAward", "Possession",
                     "WritofEvictionIssuedDate", "OtherAmount", "OtherAwarded")
judg <- lsc_judg %>% 
  select(id, Judgment, Costs, AttorneyFees, PrincipalAmount, OtherAmount, 
         InterestAward, Possession, HomesteadExemptionWaived, IsJudgmentSatisfied,
         DateSatisfactionFiled, OtherAwarded, FurtherCaseInformation)

names(lsc_garn)
# caseid, garnishee, address, number_of_checks_received, garnishee_answer, answer_date
names(lsc_garn) <- c("id", "Garnishee", "Address", "NumberofChecksReceived",
                     "GarnisheeAnswer", "AnswerDate")
garn <- lsc_garn %>% 
  select(id, Garnishee, Address, GarnisheeAnswer, AnswerDate, NumberofChecksReceived)

case <- case %>% 
  left_join(judg) %>% 
  left_join(garn)

names(case)
# new case still missing some info
# WritIssuedDate, AppealDate, AppealedBy, collected, WritofEvictionIssuedDate, WritofFieriFaciasIssuedDate
case <- case %>% 
  mutate(WritIssuedDate = NA_character_, AppealDate = NA_character_,
         AppealedBy = NA_character_, collected = NA_character_,
         WritofEvictionIssuedDate = NA_character_, WritofFieriFaciasIssuedDate = NA_character_) %>% 
  select(id, fips, details_fetched_for_hearing_date, CaseNumber, FiledDate,
         CaseType, DebtType, Judgment, Costs, AttorneyFees, PrincipalAmount,
         OtherAmount, InterestAward, Possession, WritIssuedDate, 
         HomesteadExemptionWaived, IsJudgmentSatisfied, DateSatisfactionFiled,
         OtherAwarded, FurtherCaseInformation, Garnishee, Address, GarnisheeAnswer,
         AnswerDate, NumberofChecksReceived, AppealDate, AppealedBy, collected,
         WritofEvictionIssuedDate, WritofFieriFaciasIssuedDate)

write_csv(case, "civilcases/DistrictCivil_2022/Cases.csv")

