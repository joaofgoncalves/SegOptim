g.region raster=UAV_Mindelo_RGB_3cm_FID_1.1@PERMANENT


      i.segment --verbose --overwrite group=UAV_Mindelo_RGB_3cm_FID_1@PERMANENT output=GRASS_RG_Segm_6c1c6f621e2c threshold=0.45 minsize=50 memory=2046 iterations=30


      r.out.gdal -c --overwrite input=GRASS_RG_Segm_6c1c6f621e2c@PERMANENT output=C:\Users\JG\AppData\Local\Temp\RtmpQXxhJ4\file6c1c41db4b19.tif format=GTiff type=UInt32


      g.remove -f -b type=raster name=GRASS_RG_Segm_6c1c6f621e2c@PERMANENT

