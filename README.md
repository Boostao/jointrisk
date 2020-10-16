# jointrisk

Updates

# 1.1.5

Change `update_polygons` return form to include message.

# 1.1.4

sort risks before creating polygons

# 1.1.3

sf performance upgrade, batching unions.

# 1.1.2

sf performance upgrade

# 1.1.1

Split buff_and_remove_streets into an API version and a batch optimized version.

# 1.1.0

Cut off streets for polygons using HERE Navstreets dataset.

# 1.0.2

Condition modified for UMESSUPE to 
  !.subset2(dt, "UMESSUPE") %in% c("ME")
instead of 
  .subset2(dt, "UMESSUPE") %in% c("PI", "*", NA, "")
so everything that is not ME default to square feet conversion

# 1.0.1

Change the border used to limit search space when obtaining joint risk (now uses the pockets geometries instead of the individual risk geometries).

# 1.0.0

Initial app version

Package pour l'identification des risques conjoints au commercial.
