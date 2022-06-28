FROM ubuntu
COPY .actrc /root
RUN apt-get update && apt-get install --yes curl
RUN curl https://raw.githubusercontent.com/nektos/act/master/install.sh | bash