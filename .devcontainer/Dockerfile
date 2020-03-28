# Use the official image as a parent image
FROM node:12

ENV CONTAINERHOME /home/node
RUN apt update; apt install git; apt clean;

RUN npm install -g purescript@0.13.6 --unsafe-perm; 
RUN npm install -g spago@0.14.0 parcel-bundler@1.12.4 sass@1.25.0 --unsafe-perm
# yarn is already installed in node image (yarn@1.22.0)

USER node
# Inform Docker that the container is listening on the specified port at runtime.
EXPOSE 1234

WORKDIR $CONTAINERHOME
# Run the specified command within the container.
CMD [ "bash", "" ]