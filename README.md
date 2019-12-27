# Eddie ice phenology module
## Bill Perry wlperry@ilstu.edu   
   
This is the r version of the Project Eddie Ice Phenology module.    
   
### The project as all I have developed are divided into a common structure.    
*Base Project name*    
    data - the dat for the project    
    documents - general documents for the module - word and excel things
    figures - all figures are saved here    
    output - if output data is generated its stored here    
    scripts - folder for all of the scripts    
    .Rproj - file to be run to load the project in R Studio     

### The data for this project is: `lake_ice_phenology_module-.xls`    
The file is structured into several tabs in the workbook with each lake name:    
     Baikal   
     Cazenovia       
     Mendota   
     Monona   
     Sunapee   
     Oneida    
     Wingra     
     
### Within each tabe there are 6 columns of data
      lake           - name of the lake	   
      start_year     - year of ice data	   
      ice_on_date    - date of ice on - month/day/year   
      ice_off_date	 - date of ice off - month/day/year   
      ice_on_julian	 - ice on day of year
      ice_off_julian - ice off day of year    	   
      
### Scripts files to use are 
     regression for ice off phenology.R - this is a basic R script    
     Regressions for ice of phelology NOTEBOOK.Rmd - notebook format