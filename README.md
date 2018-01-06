# Exploring Miso

My playground for testing Miso on GHC and GHCJS. It interested me because I've been using Elm
for quite a while now. Not having Elm on the server is kind of a bummer, so I went to look for alternatives.
Out came Miso!

## Installation
This repo consists of 2 different Stack projects that are "linked" together using the `build` script.

Make sure you have [Stack](https://docs.haskellstack.org/en/stable/README/) installed.

### Backend

Run the following command to setup GHC for our backend:

```
stack setup --stack-yaml backend/stack.yaml
```

### Frontend

Run the following command to setup GHCJS for our frontend

```
stack setup --stack-yaml frontend/stack.yaml
```

### Compiling

Use the build command to compile both the backend and frontend. And then we can launch the application.
The backend will serve the frontend JS.

```
./build
stack exec backend --stack-yaml backend/stack.yaml
```

And then visit [localhost:3000](http://localhost:3000) to view the application.

## Shared logic

The point of using GHCJS, of course, is to share most logic between our backend and frontend.
This logic lives inside the `common` folder.

Miso can pre-render on the server, that's why most/all view logic is in the `common` folder. We make
use of this pre-rendering. Miso can "take over" this pre-rendered HTML on the client.

## Inspiration

* [dmjio/miso examples](https://github.com/dmjio/miso)
* [FPtje/miso-isomorphic-example](https://github.com/FPtje/miso-isomorphic-example)
