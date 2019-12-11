# Nexus v0.4.3
Nexus is a user friendly, open-source software package written in the R programming language that provides a seamless integration of Caisis and Diamonds clinical databases at the Fred Hutch.  It is intended for data scientists to reduce the effort required to prepare data for statistical and exploratory analyses.  It requires credentials which can be obtained for investigators of IRB approved protocols.

### Features of Nexus
* Seamlessly integrate Caisis and Diamonds databases
* Import data tables directly into R environment without having to perform a separate SQL query
* Clean fields with mixed numeric and categorical data types
* Estimate overall survival from the time of diagnosis to death or last encounter
* Estimate treatment response from the duration between therapies
* Parse complex karyotype into a structured data table
* Parse percent plasma cells from pathology text reports
* Quickly filter laboratory data using a separate date range for each patient
* Reformat laboratory data tables with laboratory value or date as column or row
* Plot any laboratory value across any cohort of patients using a consistent time scale
* Compute number of encounters or observations across first and last time point within any data table

### Nexus imports the follow data tabes in to R

#### Caisis database
* Disease status
* Encounters
* Bone marrow biosy and flow cytometry reports
* FISH and cytogenetics reports
* Medical therapy history
* Radiation therapy history
* Radiology reports

#### Diamonds database
* Fred Hutch protocol consents
* Laboratory data
* ICD codes

#### Computed tables
* Overall survival
* Duration of between treatments and line of therapy
* Duration and number of encounters between any time points per patient
* Parsed karyotype strings
* Parsed percentage of plasma cells

#### Additional tables coming soon
* Vital signs
* Gateway and NW Biotrust consents
* PIRO protocols
* Laboratory orders
* Diagnoses
* Appointments
* Freezer Pro specimens

### Installation instructions for MacOS

#### Setup DNS server using unixODBC and freeTDS to be used by odbc

###### Install brew if you do not have it already
```
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

###### Install unixodbc using brew
```
brew install unixodbc
```

###### Install freetds using using brew making sure to specify that it has to be used with unixodbc
```
brew install freetds --with-unixodbc
```

###### Add the following configuration settings to your freetds.conf file (**the version number in the path may be different for your installation**)
```
nano /usr/local/Cellar/freetds/1.00.80/etc/freetds.conf

[CONGO-H]
	host = 140.107.116.197
	instance = H
	port = 51000
	tds version = 7.0
```

###### Add the following configuration settings to your odbc.ini file (**the version number in the path may be different for your installation**)	
```
nano /usr/local/Cellar/unixodbc/2.3.5_1/etc/odbc.ini

[CONGO-H]
	DRIVER = FreeTDS
	Description = ODBC INI FILE
	ServerName = CONGO-H
	Instance = H
```

###### Add the following configuration settings to your odbcinst.ini file (**the version number in the paths may be different for your installation**)
```
nano /usr/local/Cellar/unixodbc/2.3.5_1/etc/odbcinst.ini

[FreeTDS]
	Description = FreeTDS
	Driver = /usr/local/Cellar/freetds/1.00.80/lib/libtdsodbc.so
	Setup = /usr/local/Cellar/freetds/1.00.80/lib/libtdsodbc.so
	UsageCount = 1
```

###### Test the DNS connection in the command line
```
tsql -S CONGO-H -U 'fhcrc\username'
```

###### Create symbolic links to the above edited files to your home directory
```
ln -sF /usr/local/Cellar/freetds/1.00.80/etc/freetds.conf ~/.freetds.conf
ln -sF /usr/local/Cellar/unixodbc/2.3.5_1/etc/odbc.ini ~/.odbc.ini
ln -sF /usr/local/Cellar/unixodbc/2.3.5_1/etc/odbcinst.ini ~/.odbcinst.ini
```

###### Add an envronmental variable to your Rprofile pointing to the directory with the odbc.ini and odbcinst.ini files (**repeat this step every time R is updated**)
```
nano /Library/Frameworks/R.framework/Versions/Current/Resources/library/base/R/Rprofile

Sys.setenv(ODBCSYSINI="/usr/local/Cellar/unixodbc/2.3.5_1/etc/")
```

###### Install odbc in R
```
install.packages("odbc")
```

###### Test DNS setup in R (if you do not see your driver listed, then something went wrong)
```
library(odbc)
odbc::odbcListDrivers()
```

#### Download Nexus package from GitHub in R
```
install.packages("devtools")
devtools::install_github("davidcoffey/Nexus")
```

### Basic examples

#### Open connection to DNS server in R
```
library(DBI)
library(odbc)
library(Nexus)
congo <- DBI::dbConnect(odbc::odbc(), "CONGO-H", uid = "fhcrc\\username", pwd = rstudioapi::askForPassword("Database password"))
```

#### Extracting data
```
observations = c("HCT", "WBC", "PLT", "INTRPE", "BJPTCA", "BJPT", "KFLC", "LFLC", "MC1QN", "MC2QN")
labs = extractLabs(connection = congo, labs = observations, format = "raw")

DxCodes = c("C90.00", "C90.01", "C90.03")
diagnoses = extractDiagnoses(connection = congo, diagnoses = DxCodes, format = "raw")

demographics = extractDemographics(connection = congo)
status = extractDiseaseStatus(connection = congo)
pathology = extractPathology(connection = congo)
cytogenetics = extractCytogenetics(connection = congo)
protocols = extractProtocols(connection = congo)
radiology = extractRadiology(connection = congo)
radiationTherapy = extractRadiationTherapy(connection = congo)
medicalTherapy = extractMedicalTherapy(connection = congo)
```

#### Plot selected labs
```
plotLabs(data = labs, chart = "line", lab = "LFLC", interactive = TRUE)
```

#### Merge data tables
```
merged = Reduce(function(x, y) merge(x, y, all = FALSE), list(labs, status, diagnoses))
```

### Useful links
* [Setting up ODBC Drivers](http://db.rstudio.com/drivers)
* [Microsoft SQL Connection Settings](http://db.rstudio.com/microsoft-sql-server/)
* [ODBC R package installation](https://github.com/rstats-db/odbc)
