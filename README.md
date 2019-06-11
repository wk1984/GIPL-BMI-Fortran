# GIPL_BMI_Fortran
This is to develop a BMI for GIPL with FORTRAN.

### 1. Compile with cmake ###

> `mkdir _build && cd _build`  
> `cmake .. -DCMAKE_INSTALL_PREFIX=[install_path]`  
> `make install`  
> `source ../scripts/update_rpaths.sh`  
> `ctest`
> 

### 2. Current input variables [will be added in future]

| Standard Name                             | Description                                                                                                                       | Unit     | Type    |
|-------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|----------|---------|
| land\_surface\_air\_\_temperature             | SURFACE TEMPERATURE FIELDS (TEMPERATURES FROM THE START TO THE END OF THE INTERVAL)                                               | deg.C    | Real    |
| snowpack\_\_depth                   | SNOW DEPTH                | cm     | Real |
| snow\_\_thermal\_conductivity                   | SNOW THERMAL CONDUCTIVITY                | W m-1 K-1    | Real |
| soil\_water\_\_volume\_fraction                   | Volumetric Water Content                | m3 m-3    | Real |
| soil\_unfrozen\_water\_\_a                  | Soil Unfrozen Water Parameter 'a'                | -    | Real |
| soil\_unfrozen\_water\_\_b                  | Soil Unfrozen Water Parameter 'b'                | -    | Real |

### 3. Current output variables [will be added in future]

| Standard Name                             | Description                                                                                                                       | Unit     | Type    |
|-------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|----------|---------|
| soil\_\_temperature             | SOIL TEMPERATURE at all Nodes (including snow layers when there are snow covered)                                              | deg.C    | Real    |
| model\_soil\_layer\_\_count             | number of all nodes (including snow layers when there are snow covered)                                              | -    | integer    |

