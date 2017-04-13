# Diamonds
This R package is designed to make extracting, manipulating, and visualizing data from the Diamonds database easier.

### Installation instructions for MacOS

#### Setup DNS server using unixODBC and freeTDS to be used by RODBC

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

	[NILE-H]
		host = 140.107.116.188
		instance = H
		port = 52000
		tds version = 7.0
```

###### Use a text editor like nano to edit the odbc.ini file	
```
nano /usr/local/Cellar/unixodbc/2.3.4/etc/odbc.ini

	[NILE-H]
		DRIVER = FreeTDS
		Description = ODBC INI FILE
		ServerName = NILE-H
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
tsql -S NILE-H -U 'fhcrc\username'
```

###### Create symbolic links to the above edited files to your home directory
```
ln -sF /usr/local/Cellar/freetds/1.00.9/etc/freetds.conf ~/.freetds.conf
ln -sF /usr/local/Cellar/unixodbc/2.3.4/etc/odbc.ini ~/.odbc.ini
ln -sF /usr/local/Cellar/unixodbc/2.3.4/etc/odbcinst.ini ~/.odbcinst.ini
```

###### Use a text editor like nano to edit your Rprofile file (**this and the following step will need to be repeated every time R is updated**)
```
nano /Library/Frameworks/R.framework/Versions/3.3/Resources/library/base/R/Rprofile

	Sys.setenv(ODBC_INCLUDE="/usr/local/include")
	Sys.setenv(ODBC_LIBS="/usr/local/lib")
```

###### Downlaod [RODBC v1.3.-12](https://cran.r-project.org/src/contrib/Archive/RODBC/) (newer versions don’t work) and install in R
```
install.packages("~/Downloads/RODBC_1.3-12.tar", repos=NULL, type="source")
```

###### Test DNS setup in R
```
library(RODBC)
odbcDataSources()
```

###### Connect to DNS server in R
```
channel <- odbcConnect("NILE-H", uid = "fhcrc\\username", pwd = "**********")
```

#### Download Diamonds package from GitHub in R
```
install.packages("devtools")
devtools::install_github("davidcoffey/Diamonds")
```

### Useful links
* [Setup ODBC for R on OS X](http://hiltmon.com/blog/2013/09/18/setup-odbc-for-r-on-os-x/)
* [How to install RODBC on Mac OS X Yosemite with unixodbc and freetds](http://hiltmon.com/blog/2013/09/18/setup-odbc-for-r-on-os-x/)
