FROM relateiq/oracle-java8:latest

ENV SBT_VERSION 0.13.9

RUN \
  wget https://dl.bintray.com/sbt/debian/sbt-$SBT_VERSION.deb && \
  dpkg -i sbt-$SBT_VERSION.deb && \
  rm sbt-$SBT_VERSION.deb && \
  apt-get update && \
  apt-get install sbt

RUN sbt -v

VOLUME /src /bin/soccer-rest/src
VOLUME build.sbt /bin/soccer-rest/build.sbt

WORKDIR /bin/soccer-rest