# Diamonds v0.1.4
This R package is designed to make extracting, manipulating, and visualizing data from the Diamonds database easier.

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

###### Use a text editor like nano to edit the freetds.conf configuration file
```
nano /usr/local/Cellar/freetds/1.00.9/etc/freetds.conf

	[CONGO-H]
		host = 140.107.116.197
		instance = H
		port = 51000
		tds version = 7.0
```

###### Use a text editor like nano to edit the odbc.ini file	
```
nano /usr/local/Cellar/unixodbc/2.3.4/etc/odbc.ini

	[CONGO-H]
		DRIVER = FreeTDS
		Description = ODBC INI FILE
		ServerName = CONGO-H
		Instance = H
```
###### Use a text editor like nano to edit the odbcinst.ini file
```
nano /usr/local/Cellar/unixodbc/2.3.4/etc/odbcinst.ini

	[FreeTDS]
		Description = FreeTDS
		Driver = /usr/local/Cellar/freetds/1.00.9/lib/libtdsodbc.so
		Setup= /usr/local/Cellar/freetds/1.00.9/lib/libtdsodbc.so
		UsageCount = 1
```

###### Test DNS connection
```
tsql -S CONGO-H -U 'fhcrc\username'
```

###### Create symbolic links to the above edited files to your home directory
```
ln -sF /usr/local/Cellar/freetds/1.00.9/etc/freetds.conf ~/.freetds.conf
ln -sF /usr/local/Cellar/unixodbc/2.3.4/etc/odbc.ini ~/.odbc.ini
ln -sF /usr/local/Cellar/unixodbc/2.3.4/etc/odbcinst.ini ~/.odbcinst.ini
```

###### Use a text editor like nano to edit your Rprofile file (**this and the following step will need to be repeated every time R is updated**)
```
nano /Library/Frameworks/R.framework/Versions/3.4/Resources/library/base/R/Rprofile

	Sys.setenv(ODBC_INCLUDE="/usr/local/include")
	Sys.setenv(ODBC_LIBS="/usr/local/lib")
```

###### Install odbc in R
```
install.packages("odbc")
```

###### Test DNS setup in R
```
library(odbc)
odbc::odbcListDrivers()
```

###### Connect to DNS server in R
```
library(DBI)
channel <- DBI::dbConnect(odbc::odbc(), "CONGO-H", uid = "fhcrc\\username", pwd = rstudioapi::askForPassword("Database password"))
```

#### Download Diamonds package from GitHub in R
```
install.packages("devtools")
devtools::install_github("davidcoffey/Diamonds")
```

### Useful links
* [Setting up ODBC Drivers](http://db.rstudio.com/drivers)
* [Microsoft SQL Connection Settings](http://db.rstudio.com/microsoft-sql-server/)
* [ODBC R package installation](https://github.com/rstats-db/odbc)
