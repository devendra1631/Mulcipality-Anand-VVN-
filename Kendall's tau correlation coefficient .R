############################################TAX######################################################
library(corrplot)

# Create a matrix with the data you provided
data_matrix1 <- matrix(c(1,8,6,7,3,9,5,10,4,2,
9,10,7,8,5,2,3,6,4,1,
10,1,2,5,4,8,7,9,6,3,
10,1,2,6,5,8,7,9,4,3,
10,2,3,5,4,6,7,1,9,8,
2,9,6,7,4,8,3,10,1,5,
2,1,5,7,9,4,8,10,6,3,
8.5,5,8.5,8.5,8.5,2,1,6,3,4,
1,3,5,2,7,8,9,10,6,4,
6,10,9,7,8,1,5,4,3,2,
1,4,5,2,7,3,6,9,8,10,
4,3,9,7,10,8,5,6,2,1
), 
              nrow = 12, ncol = 10, byrow = TRUE, 
              dimnames = list(c("Property Tax", "Water Tax", "Sewerage Tax", "Conservancy (Sanitation) Tax", "Lighting Tax", "Education Tax", "Vehicle Tax", "Tax on Animals", "Professional Tax", "Advertisement Tax", "Octroi & Toll", "Others Taxes"),
                              c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021")))

# Compute Kendall's tau correlation matrix
kt_matrix1<- cor(mat, method = "kendall")

# Create a heatmap of the correlation matrix
corrplot(kt_matrix1, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

# Extract the correlation matrix
cor_mat


















####################################################Revenue_Exp#########################################################

# Load required packages
library(corrplot)

# Create matrix of data
data_matrix2 <- matrix(c(3,4,2,10,7,5,6,9,8,1
,10,5,3,8,9,4,6,1,7,2
,7,9,6,10,8,2,5,3,4,1
,1,5,4,9,9,2,9,6,7,3
,7,5,9,10,2,4,3,6,8,1
,1,2,6,7,10,5,8,4,9,3
,2,3,1,5,7,4,6,9,10,8
,1,2,3,4,6,5,7,8,10,9
,6,1,4,9,5,8,10,7,3,2,
2,1,3,5,4,7,6,8,9,10,
7,	8,	6,	9,	10,	2,	3,	5,	4,	1,
7,	6,	5,	1,	2,	3,	9,	4,	9,	9,
4,	3,	2,	6,	1,	5,	8	,10	,9	,7
,3,	5,	1,	2,	4,	9.5,	8,	9.5,	6	,7
,6,	8,	3,	10,	1,	5,	9,	2	,7,	4,
1,	4,	2,	8,	7,	6,	3,	10,	9,	5,
9,	9,	9,	6,	4,	1,	5,	2,	3,	7,
5,	1,	3,	2,	6	,8	,10,	9,	7,	4,
5,	3,	2	,10,	8,	6,	4	,7,	9,	1,
6,	7,4,	3,	2,	8,	5,	10,	9,	1,
5,	7,	6,	10,	8,	9,	4,	2,	3,	1,
4	,1,	2,	6,	3,	7,	8,	5,	9,	10,
3,	9.5,	9.5,	2,	1,	5,	8,	4,	7,	6,
7,	2,	6,	5,	4,	10,	3,	1,	9,	8,
2,	4,	1,	8,	6,	5,	7,9,	10,	3,
1,	7,	10,	3,	5,	9,	4,	2,	6,	8,
8,	7,	9,	2,	6,	5,	4,	3,	1,	10,
7,	9,	3,	8,	4,	6,	10,	1,	5,	2,
1,	2,	3,	7,	7,	7,	7,	7,	7,	7,
8,	8,	3,	8,	1,	4,	5,	2,	8,	8),
                       nrow = 30, ncol = 10, byrow = TRUE,
                       dimnames = list(c("Salaries_Wages_and_Bonus", "Benefits_and_Allowances", 
                                         "Employee_Provident_Fund", "Election", "Rent_Rates_and_Taxes",
                                         "Office_Maintenance", "Communication_Expenses", 
                                         "Books_and_Periodicals", "Printing_and_Stationery", 
                                         "Travelling_Conveyance", "Insurance", "Fees", "Legal_Expenses", 
                                         "Professional_and_other_Fees", "Council_meeting_Honorarium_sitting_fees", 
                                         "Advertisement_and_Publicity", "Private_security_guard", "Bank_charges", 
                                         "Power_Fuel", "Bulk_Purchases", "Hire_Charges_(e-tendering)", 
                                         "Repairs_and_maintenance_Infrastructure_Assets", "Repairs_maintenance_Civic_Amenities", 
                                         "Other_operating_maintenance_expenses", "Electricity", "Sanitation_and_Waste_Management", 
                                         "Grants", "Contributions", "Subsidies", "Miscellaneous_expenses_(Violet)"), 
                                       c("RANK12", "RANK13", "RANK14", "RANK15", "RANK16", "RANK17", "RANK18", "RANK19", "RANK20", "RANK21")))




# Compute Kendall's tau matrix
kt_matrix2 <- cor(data_matrix2, method = "kendall")



# Create heatmap of Kendall's tau matrix
corrplot(kt_matrix2, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")

































####################################################Capital_Exp#########################################################
# Load required packages
library(corrplot)
library(gplots)

data <- as.matrix(read.table(text = "capiat_revenue RANK12 RANK13 RANK14 RANK15 RANK16 RANK17 RANK18 RANK19 RANK20 RANK21
Office_Buildings_and_Quarters 8 8 8 8 1 8 5 4 3 2
Civic_amenities_and_Service_Centers 5 6 8 2 1 4 9 7 10 3
Roads_Bridges 3 2 1 6 10 8 5 4 7 9
Sewerage_and_Drainage 4 3 7 10 1 5 8 6 9 2
Waterways 5 10 6 9 8 1 3 4 2 7
Public_Lighting 9 2 5 4 1 6 7 10 8 3
Hospital 1 7 5 8 6 9 10 4 3 2
Vehicles 7 6 4 10 8 3 5 1 9 2
Furniture_Fixtures_Fittings_and_Electrical_Appliances 8.5 8.5 8.5 2 1 6 5 3 4 8.5
Other_Equipment_Computer_Machinery 5 8.5 8.5 8.5 3 8.5 4 2 1 6
Fire_Services 6 7 9 10 4 8 5 3 1 2
Cemetery 4 8 6 10 9 5 7 3 2 1
Garden 10 7 5 4 8 2 1 3 6 9
Deposits 2 5 1 6 8 10 4 9 7 3
Advances 7 3 5 8 2 4 6 9.5 9.5 1
loans 2 1 9 7 4 9 3 9 6 5
Intrest 7 7 2 3 7 7 7 7 7 1
Other(Bill_deduction) 3 6 8 4 1 7 9 5 2 10", header = TRUE))[, -1]

data <- apply(data, 2, as.numeric)

kendall_matrix3 <- cor(data, method = "kendall")
# Create heatmap of Kendall's tau matrix
corrplot(kendall_matrix3, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")












####################################################Non_Tax#########################################################
library(corrplot)
data4 <- as.matrix(read.table(text ="Non_Tax RANK12 RANK13 RANK14 RANK15 RANK16 RANK17 RANK18 RANK19 RANK20 RANK21

Empanelment_and_Registration_Charges 	10	8	9	6	5	1	3	2	4	7
Licensing_Fees 	10	2	9	5	7	4	8	6	1	3
Fees_for_Grant_of_Permit 	1	3	7	4	5	8	10	9	6	2
Fees_for_Certificate_or_Extract 	8	6	10	3	7	4	9	5	2	1
Development_Charges 	7	4	8	6	10	2	1	5	9	3
Regularization_Fees 	8	3	2	4	10	9	1	5	6	7
Penalties_and_Fines 	10	8	4	2	3	6	5	1	7	9
Other_Fees 	3	10	1	7	8	9	6	2	4	5
User_Charges 	6	7	10	8	5	3	1	2	9	4
Entry_Fees 	2	3	5	8	9	10	6	7	4	1
Service_Administrative_Charges 	9	6	7	2	5	3	4	1	8	10
Other_Charges 	2	3	5	7	1	8	6	4	9	10
Fees_Remission_and_Refund 	9	3	10	6	7	4	8	5	2	1
Sale_of_stores_&_scrap 	5	7	1	3	9	10	8	2	4	6
Rent_from_Civic_Amenities 	4	2	6	5	9	8	10	7	1	3
Rent_from_Guest_Houses 	3	1	4	2	10	5	8	6	9	7
Other_rents 	6	4	7	8	2	3	10	9	5	1
Interest 	3	7	5	2	6	1	4	9	10	8
Miscellaneous_Income 	4	9	8	3	5	7	10	6	1	2", header = TRUE))[, -1]

data4 <- apply(data4, 2, as.numeric)

kendall_matrix4 <- cor(data4, method = "kendall")
# Create heatmap of Kendall's tau matrix
corrplot(kendall_matrix4, method = "color", type = "upper", tl.col = "black", addCoef.col = "black")







