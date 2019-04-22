#! /bin/bash

# build the documentation
docker run  \
       -v `pwd`/src/:/code/  \
       -v `pwd`/docs/:/docs/  \
       -v `pwd`/docs/config/:/config/  \
       -it doxy-yap_image_ub1804  \
       doxygen-yap  /config/doxy_config_file.cfg

# Run docker yap image with library main file
docker run  \
       -v `pwd`/src/:/src/  \
       -it yap_image_ub1804  \
       yap -L /src/plqc.yap

# Run docker yap image with test file
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it yap_image_ub1804  \
       yap -L /test/context_test.yap
