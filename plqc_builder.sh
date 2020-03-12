#! /bin/bash

# build the documentation
#echo skip || \
docker run  \
       -v `pwd`/src/:/code/  \
       -v `pwd`/docs/:/docs/  \
       -v `pwd`/docs/config/:/config/  \
       -it doxy-yap_image_ub1804  \
       doxygen-yap  /config/doxy_config_file.cfg


# Run docker swipl image with shared helpers plunit test file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it swipl  \
       swipl -t "consult('/test/plqc_common.plt'), show_coverage(run_tests)."

# Run docker swipl image with generic records plunit test file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it swipl  \
       swipl -t "consult('/test/generic_records.plt'), show_coverage(run_tests)."

# Run docker swipl image with context plunit test file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it swipl  \
       swipl -t "consult('/test/context.plt'), show_coverage(run_tests)."

# Run docker swipl image with result plunit test file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it swipl  \
       swipl -t "consult('/test/result.plt'), show_coverage(run_tests)."
       #swipl -t "load_test_files([]), run_tests." -s /src/result.pl

# Run docker yap image with library main file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -it yap_image_ub1804  \
       yap -L /src/plqc.yap

# Run docker yap image with main file test file
# echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it yap_image_ub1804  \
       yap -L /test/plqc_test.yap

# Run docker yap image with testcase run file
#echo skip || \
docker run  \
       -v `pwd`/src/:/src/  \
       -v `pwd`/test/:/test/  \
       -it yap_image_ub1804  \
       yap -L /test/plqc_run_test.yap
