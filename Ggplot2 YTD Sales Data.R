## ------------------------------------------------
## Read xlsx sales YTD data and creat long format
## -----------------------------
# Read-in the data from Excel files
library(readxl)
ytd <- read_excel("/Users/Danny/Downloads/YTD Sales Performance  as of Jan-May 2016.xls", sheet = "OSS")
detach(package:readxl)

# Do data frame preparation
names(ytd) <- make.names(names(ytd))  # Rename with function make.names
ytd <- ytd[!is.na(ytd$Agent_Code), ]  # Remove last total row which agent code = NA
ytd$ClosedDate <- as.Date(ytd$ClosedDate, origin = "1899-12-30")  # Convert ClosedDate from numeric to Date object 

# Convert data from complex wide format to long format

library(dplyr)
library(tidyr)

salesYTD <- ytd %>%
  select(Agent_Code, starts_with("New")) %>% # Select Agent_code and column start with "New"
  gather(Month_Product, Approved, starts_with("New")) %>%  # Convert wide format to long format all column start with "New"
  separate(Month_Product, c("New", "Product", "Month")) %>%  # Separate column Month_Product to 3 column new month and product 
  select(-New) %>%  # Remove column New which not used
  # create YYYYMM column form 
  # Abbriviate month (Jan, Feb..) convert to number by match widh month.abb
  # Add leading Zero with formatC flag = "0"
  # Paste with YYYY = 2016
  mutate(YYYYMM = paste0("2016", formatC(match(Month, month.abb), width = 2 , format = "d", flag = "0"))) %>%
  filter(match(Month, month.abb) <= 5)  # Last month update is May
## ------------------------------------------------

# Adding useful information

agentHead <- ytd %>%
  select(AMSup.NAME, TLCode, Agent_Code, Source_Code, TL_CODE_TYPE, Status, OpenDate, ClosedDate)

sales <- salesYTD %>%
  left_join(agentHead, by = "Agent_Code")

rm(agentHead)
rm(salesYTD)

library(ggplot2)

# 1. qplot Basic
qplot(data = sales, x = YYYYMM, y = Approved, color = Product)  # Vary color
qplot(data = sales, x = YYYYMM, y = Approved, size = Product)  # Vary size
qplot(data = sales, x = YYYYMM, y = Approved, shape = Product)  # Vary shape

# 2. qplot with facet
# qplot facet_grid (Row faceted varible) ~ (Column faceted varible)
# . = not defined facet varible
# facet_wrap(~ Column faceted varible), rearrange to multiple line
qplot(data = sales, x = YYYYMM, y = Approved) + facet_grid(. ~ Product)  # Column facet by Product
qplot(data = sales, x = YYYYMM, y = Approved) + facet_grid(Product ~ .)  # Row facet by Product
qplot(data = sales, x = YYYYMM, y = Approved) + facet_grid(Product ~ TL_CODE_TYPE)  # Facet row by Product, column by TL
qplot(data = sales, x = YYYYMM, y = Approved) + facet_wrap(~ TL_CODE_TYPE)  # Column facet rearrange to muliple row for better viewing
qplot(data = sales, x = YYYYMM, y = Approved) + facet_wrap(~ Source_Code)  # Column facet rearrage multiple line

# 3. Add geom jitter
qplot(data = sales, x = YYYYMM, y = Approved)
qplot(data = sales, x = YYYYMM, y = Approved, geom = "jitter")
qplot(data = sales, x = YYYYMM, y = Approved, geom = "jitter", color = Product)
qplot(data = sales, x = YYYYMM, y = Approved, geom = "jitter", color = Product, alpha = .5)

# 4. Re-order the column
qplot(data = sales, x = Source_Code, y = Approved)  # Default column ordered in alphabetical order
aggregate(by = sales["Source_Code"], x = sales["Approved"], FUN = sum)  # Check sum of Approved by Source Code
qplot(data = sales, x = reorder(Source_Code, Approved, FUN = sum), y = Approved)  # Order column Source_Code by sum of Approved
qplot(data = sales, x = TL_CODE_TYPE, y = Approved)  # Defaul ordered
qplot(data = sales, x = reorder(TL_CODE_TYPE, Approved, FUN = sum), y = Approved)  # Order column TL_CODE_TYPE by sum of Approved
qplot(data = sales, x = factor(TL_CODE_TYPE, levels = c("ITL", "CTL", "OTL")), y = Approved)  # Order column TL_CODE_TYPE by specified levels

# 5. Mixed geom
qplot(data = sales, x = Source_Code, y = Approved, geom = c("jitter", "boxplot"))

# 6. histrogram and barchart
qplot(data = sales, Approved)
qplot(data = sales, Approved, binwidth = 1)
qplot(data = sales, Approved, binwidth = 5)

resolution(sales$Approved)  # Smallest binwidth

last_plot() + xlim(0, 5)  # Zoom in with xlim
last_plot() + coord_cartesian(xlim = c(0, 5))  # Zoomin with coord_cartesian
last_plot() + xlim(0, 10)
last_plot() + coord_cartesian(xlim = c(0, 10))

# 7. adding aesthetics

qplot(data = sales, Approved, binwidth = 1, fill = TL_CODE_TYPE) + xlim(0, 10) + ylim(0, 2500)
qplot(data = sales, Approved, binwidth = 1, fill = Source_Code) + xlim(0, 10) + ylim(0, 2500)
qplot(data = sales, Approved, binwidth = 1, fill = Source_Code) + xlim(0, 10) + ylim(0, 2500) + facet_wrap(~ TL_CODE_TYPE)
