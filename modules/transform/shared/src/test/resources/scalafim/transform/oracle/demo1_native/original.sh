#!/bin/bash
set -euo pipefail
export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1 OMP_NUM_THREADS=1
cd /work
antsRegistration --version
antsApplyTransformsToPoints -d 3 -p 1 -i template.mha -o original_t1.mha -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5
antsApplyTransformsToPoints -d 3 -p 1 -i original_t1.mha -o original_bold_sequential.mha -t scanner_to_t1.txt
antsApplyTransformsToPoints -d 3 -p 1 -i template.mha -o original_bold.mha -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 -t scanner_to_t1.txt
antsApplyTransformsToPoints -d 3 -p 1 -i inverse.mha -o original_inverse.mha -t /input/sub-01_from-MNI152NLin6Asym_to-T1w_mode-image_xfm.h5
antsApplyTransformsToPoints -d 3 -p 1 -i original_t1.mha -o original_roundtrip.mha -t /input/sub-01_from-MNI152NLin6Asym_to-T1w_mode-image_xfm.h5
antsApplyTransforms -d 3 --float 0 -u double -i boldref.nii.gz -r target.nii.gz -o original_bold_warp.nii.gz -n Linear -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 -t scanner_to_t1.txt
antsApplyTransforms -d 3 --float 0 -u double -i source_coord_0.nii.gz -r target.nii.gz -o original_coord_0.nii.gz -n Linear -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 -t scanner_to_t1.txt
antsApplyTransforms -d 3 --float 0 -u double -i source_coord_1.nii.gz -r target.nii.gz -o original_coord_1.nii.gz -n Linear -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 -t scanner_to_t1.txt
antsApplyTransforms -d 3 --float 0 -u double -i source_coord_2.nii.gz -r target.nii.gz -o original_coord_2.nii.gz -n Linear -t /input/sub-01_from-T1w_to-MNI152NLin6Asym_mode-image_xfm.h5 -t scanner_to_t1.txt
