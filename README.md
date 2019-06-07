# GIPL_BMI_Fortran
This is to develop a BMI for GIPL with FORTRAN.

### 1. Compile with cmake ###

> `mkdir _build && cd _build`  
> `cmake .. -DCMAKE_INSTALL_PREFIX=[install_path]`  
> `make install`  
> `source ../scripts/update_rpaths.sh`  
> `ctest`
> 
> `cmake .. -DCMAKE_INSTALL_PREFIX=~/Documents/test`