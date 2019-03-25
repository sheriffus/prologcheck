# prologcheck 2.0
`prologcheck 2.0` - Reimplementation of the prologcheck tool (Cláudio Amaral, Mário Florido, Vítor Santos Costa:
PrologCheck - Property-Based Testing in Prolog. FLOPS 2014: 1-17)


# Documentation

For documentation, we can use a container of the `docker-doxygen-yap` docker image
to process the configuration and build html, xml and latex sources.

The configuration file was generated with the following command

$ docker run -v `pwd`/code/:/code/  \
             -v `pwd`/docs/:/docs/  \
             -v `pwd`/docs/config/:/config/ \
             -it doxy-yap_image_ub1804 \
             doxygen-yap -g /config/doxy_config_file.cfg

You con diff the automatically generated config file if you are curious about what
changes were made, but it is mostly the default values.

The documentation is built with the command

$ docker run -v `pwd`/src/:/code/  \
             -v `pwd`/docs/:/docs/  \
             -v `pwd`/docs/config/:/config/ \
             -it doxy-yap_image_ub1804 \
             doxygen-yap  /config/doxy_config_file.cfg


# References
```bibtex
@inproceedings{DBLP:conf/flops/AmaralFC14,
  author    = {Cl{\'{a}}udio Amaral and
               M{\'{a}}rio Florido and
               V{\'{\i}}tor Santos Costa},
  title     = {PrologCheck - Property-Based Testing in Prolog},
  booktitle = {Functional and Logic Programming - 12th International Symposium, {FLOPS}
               2014, Kanazawa, Japan, June 4-6, 2014. Proceedings},
  pages     = {1--17},
  year      = {2014},
  crossref  = {DBLP:conf/flops/2014},
  url       = {https://doi.org/10.1007/978-3-319-07151-0\_1},
  doi       = {10.1007/978-3-319-07151-0\_1},
  timestamp = {Thu, 15 Jun 2017 21:39:44 +0200},
  biburl    = {https://dblp.org/rec/bib/conf/flops/AmaralFC14},
  bibsource = {dblp computer science bibliography, https://dblp.org}
}
```
