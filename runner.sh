#! /bin/bash

# Run docker yap image with sample file
docker run -v `pwd`/src/:/src/ -it yap_image_ub1804 yap -L /src/plqc.yap
