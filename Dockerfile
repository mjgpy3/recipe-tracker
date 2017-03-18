FROM node:6.10-slim

RUN mkdir /app
WORKDIR /app

RUN npm install -g elm@0.18.0

EXPOSE 3000

COPY ./src /app

RUN npm i

WORKDIR /app/ui
RUN yes | elm make /app/ui/Main.elm --output /app/ui/elm.js

WORKDIR /app

ENTRYPOINT ["node", "/app/server.js"]
