#!/bin/bash
set -euo pipefail
export ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=1 OMP_NUM_THREADS=1
cd /work
antsApplyTransformsToPoints -d 3 -p 1 -i template.mha -o crop_t1.mha -t t1_to_template.h5
antsApplyTransformsToPoints -d 3 -p 1 -i template.mha -o crop_bold.mha -t t1_to_template.h5 -t scanner_to_t1.txt
antsApplyTransformsToPoints -d 3 -p 1 -i inverse.mha -o crop_inverse.mha -t template_to_t1.h5
antsApplyTransformsToPoints -d 3 -p 1 -i original_t1.mha -o crop_roundtrip.mha -t template_to_t1.h5
antsApplyTransforms -d 3 --float 0 -u double -i boldref.nii.gz -r target.nii.gz -o crop_bold_warp.nii.gz -n Linear -t t1_to_template.h5 -t scanner_to_t1.txt
