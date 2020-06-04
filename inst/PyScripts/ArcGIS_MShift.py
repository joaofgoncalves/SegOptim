

import sys

if len(sys.argv) < 7:

    print(
        "\n\n"
        "# ----------------------------------------------------------------------- #\n"
        "# Performing ArcGIS Mean-shift segmentation algorithm\n"
        "# ----------------------------------------------------------------------- #\n\n"
        "Arguments:\n\n"
        "arg1: Temporary workspace folder (string)\n"
        "arg2: Input raster to perform segmentation (string)\n"
        "arg3: Spectral detail (1 - 20, double)\n"
        "arg4: Spatial detail (1 - 20, long)\n"
        "arg5: Minimum segment size (in pixels; long)\n"
        "arg6: Output raster path (string)\n\n"
        "# ----------------------------------------------------------------------- #\n\n"
    )

else:

    import os
    import arcpy
    from arcpy.sa import *

    # Check out the ArcGIS Spatial Analyst extension license
    arcpy.CheckOutExtension("Spatial")
    arcpy.env.overwriteOutput = True

    # Create temporary workspace
    tmpDir = sys.argv[1]

    if not os.path.exists(tmpDir):
        os.makedirs(tmpDir)

    arcpy.env.workspace = tmpDir

    # Set local variables
    inRstPath = sys.argv[2]                     # Input raster segmentation features
    spectral_detail = float(sys.argv[3])        # Optimized parameter #1
    spatial_detail = float(sys.argv[4])         # Optimized parameter #2
    min_segment_size = int(float(sys.argv[5]))  # Optimized parameter #3
    outSegmentIdRstPath = sys.argv[6]           # Output raster

    # Execute mean-shift segmentation
    try:
        segmentRaster = SegmentMeanShift(inRstPath, spectral_detail, spatial_detail, min_segment_size)

    except arcpy.ExecuteError:
        # Remove temporary directory on error
        os.rmdir(tmpDir)
        sys.exit(1)

    else:
        try:
            # Make segment index raster
            segmentRstIdx = ComputeSegmentAttributes(segmentRaster)
            segmentRstIdx.save(outSegmentIdRstPath)

        except arcpy.ExecuteError:
            # Remove temporary directory and files on error
            #import shutil
            # shutil.rmtree(tmpDir)
            sys.exit(1)

        else:
            arcpy.ClearWorkspaceCache_management()
            sys.exit(0)
