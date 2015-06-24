FROM ubuntu:14.10

RUN apt-get update && apt-get install -y \
    git \
    wget \
    curl \
    python \
    build-essential \
    ruby-dev

RUN gem install compass

RUN curl -sL https://deb.nodesource.com/setup | sudo bash -
RUN apt-get install -y nodejs

# Install Stack
RUN wget -q -O- http://download.fpcomplete.com/ubuntu/fpco.key | sudo apt-key add -
RUN echo 'deb http://download.fpcomplete.com/ubuntu/utopic stable main'|sudo tee /etc/apt/sources.list.d/fpco.list
RUN sudo apt-get update
RUN sudo apt-get install stack -y

# Get code
RUN git clone https://github.com/yanhick/conchord-ui.git /conchord
WORKDIR /conchord

# Build client
RUN npm install
RUN ./node_modules/bower/bin/bower install --allow-root
RUN ./node_modules/.bin/gulp dev

# Build server
RUN stack setup
RUN stack build

ENV PORT 80
EXPOSE 80

# Copy static files and server binary to opt
RUN mv dist/ /opt
RUN find . -name conchord-ui -type f | grep dist | xargs -I % cp % /opt/dist

WORKDIR /opt

# Start server
CMD /opt/dist/conchord-ui

# Clean up
RUN apt-get remove -y \
    git \
    wget \
    curl \
    python \
    ruby-dev \
    build-essential

RUN rm -rf /conchord
