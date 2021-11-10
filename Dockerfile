############################################################
# Dockerfile to build Owl docker image
# Based on owlbarn/owl master branch
# By Liang Wang <liang.wang@cl.cam.ac.uk>
############################################################

FROM ocaml/opam:ubuntu-20.04-ocaml-4.10
USER opam

##################### PREREQUISITES ########################

RUN sudo apt-get -y update
RUN sudo apt-get -y install m4 wget unzip aspcud libshp-dev libplplot-dev gfortran
RUN sudo apt-get -y install pkg-config git camlp4-extra

RUN opam install -y dune 
RUN opam install -y ctypes 
RUN opam install -y stdio sexplib configurator 

#FIXME: no need to list these packages 
RUN opam install -y cmdliner fmt astring topkg 
RUN opam install -y uutf

RUN opam install -y alcotest 
RUN opam install -y ocaml-compiler-libs
RUN opam install -y plplot

RUN opam install -y lwt
RUN opam install -y lwt_log camomile react 
RUN opam install -y zed mew_vi
RUN opam install -y lambda-term
RUN opam install -y utop

#TODO: use fixed version of opam packages

#################### INSTALL OPENBLAS ######################

#ENV OPENBLASPATH /home/opam/OpenBLAS
#RUN cd /home/opam && git clone https://github.com/xianyi/OpenBLAS.git
#RUN cd $OPENBLASPATH && sudo make && sudo make install && sudo make clean

RUN sudo apt-get -y install apt-utils libopenblas-dev liblapacke-dev

################# INSTALL EIGEN LIBRARY ####################

RUN opam source --dev-repo eigen
RUN cd eigen && git checkout 0.1.1 && \
    echo 'version: "0.1.1"' >> eigen.opam && opam install -w .

#################### SET UP ENV VARS #######################
RUN opam install -y camlzip
RUN opam install -y npy

ENV PATH /home/opam/.opam/4.10/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:$PATH
ENV CAML_LD_LIBRARY_PATH /home/opam/.opam/4.10/lib/stublibs
# ENV LD_LIBRARY_PATH /usr/lib/:/usr/local/lib:/home/opam/.opam/4.10.0/lib/:/home/opam/.opam/4.10.0/lib/stublibs/:/usr/lib/x86_64-linux-gnu/:/opt/OpenBLAS/lib
ENV LD_LIBRARY_PATH /usr/lib/:/usr/local/lib:/home/opam/.opam/4.10/lib/:/home/opam/.opam/4.10/lib/stublibs/:/usr/lib/x86_64-linux-gnu/

####################   INSTALL OWL  #######################

ENV OWLPATH /home/opam/owl
RUN cd /home/opam && wget https://github.com/owlbarn/owl/archive/refs/tags/1.0.0.tar.gz \
    && tar xvf 1.0.0.tar.gz && mv owl-1.0.0 owl
RUN sed -i -- 's/\"-llapacke\" :: ls/ls/g' $OWLPATH/src/owl/config/configure.ml
RUN cd $OWLPATH && make && make install


############## SET UP DEFAULT CONTAINER VARS ##############

RUN echo "#require \"owl-top\";; open Owl;;" >> /home/opam/.ocamlinit \
    && bash -c 'echo -e "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> /home/opam/.profile' \
    && opam config env >> /home/opam/.bashrc \
    && bash -c "source /home/opam/.bashrc" 

ENV CODEPATH /home/opam/code/

ADD . $CODEPATH
WORKDIR $CODEPATH
ENTRYPOINT /bin/bash