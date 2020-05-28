
import sys

if len(sys.argv) < 7:

    print(
        "\n\n"
        "# ----------------------------------------------------------------------- #\n"
        "# Performing RSGISLib Shepherd segmentation algorithm\n"
        "# ----------------------------------------------------------------------- #\n\n"
        "Arguments:\n\n"
        "arg1: Input image file path (string)\n"
        "arg2: Temporary directory where output files will be placed (string)\n"
        "arg3: Number of clusters for the k-means algorithm (int)\n"
        "arg4: Minimum size (in pixels; int)\n"
        "arg5: Spectral threshold used to merge objects (float)\n"
        "arg6: Output filename (placed on temporary directory in arg2; string)\n\n"
        "# ----------------------------------------------------------------------- #\n\n"
    )

else:

    import os
    import string
    import random
    import rsgislib
    from rsgislib import segmentation
    from rsgislib import imagecalc

    def rand_str(size=10, chars=string.ascii_uppercase + string.digits):
        return ''.join(random.choice(chars) for x in range(size))

    # ------------------------------------------------------------------------------------------------------------------
    # Perform k-means clustering (no sub-sampling performed)
    # ------------------------------------------------------------------------------------------------------------------

    inputImage = sys.argv[1]
    tmpPath = sys.argv[2] + '/'

    outputMatrixFile = tmpPath + "rsgislib_shep_kmeanscenters_" + rand_str()
    numClust = int(float(sys.argv[3]))
    maxIter = 250
    degChange = 0.0025
    subSample = 1 # this means no sub-sampling
    ignoreZeros = True
    initMethod = rsgislib.INITCLUSTER_DIAGONAL_FULL_ATTACH

    try:
        imagecalc.kMeansClustering(inputImage, outputMatrixFile, numClust, maxIter, subSample, ignoreZeros,
                                   degChange, initMethod)
    except:
        print("An error occurred while performing kMeansClustering!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # ------------------------------------------------------------------------------------------------------------------
    # Apply k-means clustering to image
    # ------------------------------------------------------------------------------------------------------------------

    outputImage_1 = tmpPath + "rsgislib_shep_clust_" + rand_str() + ".tif"
    outputImageAux_1 = outputImage_1 + ".aux.xml"
    clustercenters = outputMatrixFile + str(".gmtxt")
    gdalformat = 'GTiff'

    try:
        segmentation.labelPixelsFromClusterCentres(inputImage, outputImage_1, clustercenters, ignoreZeros, gdalformat)
    except:
        print("An error occurred while performing labelPixelsFromClusterCentres!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # ------------------------------------------------------------------------------------------------------------------
    # Eliminate single pixels
    # ------------------------------------------------------------------------------------------------------------------

    outputImage_2 = tmpPath + "rsgislib_shep_clust_nsp_" + rand_str() + ".tif"
    tempFile = tmpPath + "tmp_clust_nsp_" + rand_str() + ".tif"

    try:
        segmentation.eliminateSinglePixels(inputImage, outputImage_1, outputImage_2, tempFile, gdalformat,
                                           True, ignoreZeros)
    except:
        print("An error occurred while performing eliminateSinglePixels!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # ------------------------------------------------------------------------------------------------------------------
    # Perform clumping
    # ------------------------------------------------------------------------------------------------------------------

    outputImage_3 = tmpPath + "rsgislib_shep_clump_" + rand_str() + ".tif"

    try:
        segmentation.clump(outputImage_2, outputImage_3, gdalformat, True, 0)
    except:
        print("An error occurred while performing clump!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # ------------------------------------------------------------------------------------------------------------------
    # Eliminate small segments/clumps
    # ------------------------------------------------------------------------------------------------------------------

    outputImage_4 = tmpPath + "rsgislib_shep_clump_clean_" + rand_str() + ".tif"
    minSize = int(float(sys.argv[4]))
    spectralThresh = float(sys.argv[5])

    try:
        segmentation.rmSmallClumpsStepwise(inputImage, outputImage_3, outputImage_4, gdalformat, False, "", False, True,
                                           minSize, spectralThresh)
    except:
        print("An error occurred while performing rmSmallClumpsStepwise!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # ------------------------------------------------------------------------------------------------------------------
    # Relabel clumps
    # ------------------------------------------------------------------------------------------------------------------

    outputImage_5 = sys.argv[6]

    try:
        segmentation.relabelClumps(outputImage_4, outputImage_5, gdalformat, True)
    except:
        print("An error occurred while performing relabelClumps!")
        print(sys.exc_info()[0])
        sys.exit(1)

    # Remove temporary files
    try:

        print("\nDeleting temporary files...")
        os.remove(outputImage_1)
        os.remove(outputImageAux_1)
        os.remove(outputImage_2)
        os.remove(outputImage_3)
        os.remove(outputImage_4)
        os.remove(clustercenters)
        print("done.\n\n")

        sys.exit(0)

    except IOError as e:
        print("An error occurred while removing temporary files: " + e)
        print(sys.exc_info()[0])
