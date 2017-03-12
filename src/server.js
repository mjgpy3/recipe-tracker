'use strict';

const Hapi = require('hapi');
const uuid = require('node-uuid');
const request = require('request-promise');
const R = require('ramda');
const fs = require('fs');

const esUrl = 'http://localhost:2113/';
const streamUrl = streamName => `${esUrl}streams/${streamName}`;
const projectionStateUrl = projectionName => `${esUrl}projection/${projectionName}/state`;
const partitionedProjectionStateUrl =
  (partition, projectionName) => `${projectionStateUrl(projectionName)}?partition=${partition}`;

const allProjections = () =>
  request({
    url: `${esUrl}projections/any`,
    method: 'GET',
    json: true,
    headers: {
      Accept: 'application/json'
    }
  });

const createProjection = name =>
  allProjections()
    .then(
      projections => {
        if (!projections.projections.find(proj => proj.name === name)) {
          request({
            url: `${esUrl}projections/continuous?name=${name}&type=js&enabled=true&emit=false&trackemittedstreams=false`,
            method: 'POST',
            body: fs
              .readFileSync(`projections/${name}.js`)
              .toString('utf8'),
            headers: {
              Authorization: 'Basic YWRtaW46Y2hhbmdlaXQ=',
              'Content-Type': 'text/plain'
            }
          })
          .then(
            _ => console.log(`Created "${name}" projection`),
            err => console.log(`Failed to create "${name}", error:`, err)
          )
        } else {
          console.log(`Projection "${name}" already exists - skipping`);
        }
      }
    )
    .then(
      _ => 42,
      err => console.log('Error: ', err)
    );

const enableByCategoryProjection = () =>
  request({
    url: `${esUrl}projection/$by_category/command/enable`,
    method: 'POST',
    json: true,
    body: {},
    headers: {
      Authorization: 'Basic YWRtaW46Y2hhbmdlaXQ='
    }
  })
  .then(
    _ => console.log('Enabled "$by_category" projection'),
    err => console.log(`Failed to enable "$by_category", error: ${err}`)
  );

const bootstrap = () =>
  Promise.all(
    [
      enableByCategoryProjection()
    , createProjection('all-recipe-names')
    , createProjection('all-recipe-summaries')
    , createProjection('recipe')
    ]
  );

bootstrap();

const produce = (event, stream) =>
  request({
    url: streamUrl(stream),
    method: 'POST',
    json: true,
    body: [event],
    headers: {
      'Content-Type': 'application/vnd.eventstore.events+json'
    }
  });

const server = new Hapi.Server();
server.connection({ port: 3000, host: 'localhost', routes: { cors: true } });

server.route({
  method: 'POST',
  path: '/recipe/{name}/cooked',
  handler: (req, reply) => {
    const event = {
      eventId: uuid.v4(),
      eventType: 'RecipeCooked',
      data: {}
    };
    console.log(event);
    produce(event, `Recipe-${req.params.name}`)
      .then(
        _ => {
          console.log('Cooked');
          reply({ message: 'Cooked' });
        },
        err => {
          console.log('Error:', err);
          reply({ message: 'An error occured' }).code(500);
        }
      );
    ;
  }
});

server.route({
  method: 'POST',
  path: '/recipe/{name}/follow',
  handler: (req, reply) => {
    const event = {
      eventId: uuid.v4(),
      eventType: 'RecipeFollowingStarted',
      data: {}
    };
    console.log(event);
    produce(event, `Recipe-${req.params.name}`)
      .then(
        _ => {
          console.log('Follow started');
          reply({ message: 'Follow started' });
        },
        err => {
          console.log('Error:', err);
          reply({ message: 'An error occured' }).code(500);
        }
      );
    ;
  }
});

const recipeNames = () =>
  request({
    method: 'GET',
    uri: projectionStateUrl('all-recipe-names'),
    json: true
  })
  .then(R.defaultTo([]));

const recipeSummaries = () =>
  request({
    method: 'GET',
    uri: projectionStateUrl('all-recipe-summaries'),
    json: true
  })
  .then(R.defaultTo([]));

server.route({
  method: 'POST',
  path: '/recipe',
  handler: (req, reply) => {
    const recipeName = req.payload.recipe.name;

    recipeNames()
      .then(
        R.ifElse(
          R.contains(recipeName),
          _ => reply({ message: `A reecipe named "${recipeName}" already exists.` }).code(400),
          _ => {
            const event = {
              eventId: uuid.v4(),
              eventType: 'RecipeAdded',
              data: req.payload
            };
            console.log(event);
            produce(event, `Recipe-${event.data.recipe.name}`)
              .then(
                _ => {
                  console.log('Added');
                  reply({ message: 'Added' });
                },
                err => {
                  console.log('Error:', err);
                  reply({ message: 'An error occured' }).code(500);
                }
              );
          }
        )
      );
  }
});

server.route({
  method: 'GET',
  path: '/recipes',
  handler: (_, reply) => {
    recipeSummaries()
      .then(
        reply,
        err => {
          console.log('Error:', err);
          if (err.statusCode === 404) {
            reply([]);
          }
          else {
            reply({ message: 'An error occured' }).code(500);
          }
        }
      );
    ;
  }
});

server.route({
  method: 'GET',
  path: '/recipe/{name}',
  handler: (req, reply) => {
    request({
      method: 'GET',
      uri: partitionedProjectionStateUrl(`Recipe-${req.params.name}`, 'recipe'),
      json: true
    })
      .then(
        response => {
          reply(response.recipe);
        },
        err => {
          console.log('Error:', err);
          reply({ message: 'An error occured' }).code(500);
        }
      );
    ;
  }
});

server.register(require('inert'), (err) => {

  if (err) {
    throw err;
  }

  server.route({
    method: 'GET',
    path: '/{param*}',
    handler: {
      directory: {
        path: 'ui'
      }
    }
  });

  server.route({
    method: 'GET',
    path: '/',
    handler: (request, reply) => {
      reply.file('./ui/index.html');
    }
  });
});

server.start((err) => {
  if (err) {
    throw err;
  }
  console.log(`Server running at: ${server.info.uri}`);
});
