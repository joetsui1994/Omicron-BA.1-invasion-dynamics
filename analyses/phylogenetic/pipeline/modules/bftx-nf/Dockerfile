FROM openjdk:8 AS beast
WORKDIR /tmp
RUN apt-get update && apt-get install -y \
	ant
	# build-essential \
	# autoconf \
	# automake \
	# libtool \
	# subversion 

# Clean-up
RUN apt-get -y autoremove
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* 

ENV JAVA_TOOL_OPTIONS -Dfile.encoding=UTF8
ENV ROOT_HOME /root

# To make copying over easier in the end
RUN mkdir ${ROOT_HOME}/beast_builds/
RUN mkdir -p ${ROOT_HOME}/libs

# Clone and install Beast from sources
WORKDIR ${ROOT_HOME}
RUN git clone --branch BigFastTreeModel https://beast-dev@github.com/beast-dev/beast-mcmc.git \
	&& cd ${ROOT_HOME}/beast-mcmc \
	&& git checkout d1a5 \
	&& ant linux \
	&& mkdir -p /usr/local \
	&& mv ${ROOT_HOME}/beast-mcmc/release/Linux/BEASTv1* ${ROOT_HOME}/beast_builds/BEAST \
	&& ant -f build_beastgen.xml package \
	&& mv ${ROOT_HOME}/beast-mcmc/release_beastgen/BEASTGen*/ ${ROOT_HOME}/beast_builds/ \
	&& ant dist \
	&& mv ${ROOT_HOME}/beast-mcmc/build ${ROOT_HOME}/beast_builds/BEAST/

# Rust and fertree / TODO remove rust at the end
FROM rust:1.51 as rust
WORKDIR /root
RUN git clone https://github.com/jtmccr1/fertree.git
WORKDIR /root/fertree
RUN git checkout 5c7947d
RUN cargo install --path .



COPY --from=beast ${ROOT_HOME}/beast_builds/* /usr/local/
COPY --from=beast ${ROOT_HOME}/libs/lib/* /usr/local/lib/
COPY --from=beast ${ROOT_HOME}/libs/include/* /usr/local/include/
COPY --from=beast /usr/local/openjdk-8 /usr/local/openjdk-8

# RUN docker pull evolbioinfo/treetime:v0.8.5
