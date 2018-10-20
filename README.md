## Full Fun Web with Safe Stack

All demos can be run from the command line: `fake build --target run`

### Requirements

- [dotnet core 2+](https://www.microsoft.com/net/download)
- `yarn` and `node.js`
- `fake` command line build utility
  - Installing fake using dotnet core
  - `dotnet tool install fake-cli -g`

### Common Build Issue

Sometimes when flipping between projects and running them the `obj` and `src` directories may
become stale and will need to be deleted from both the `Client` and `Server` directories.

### Slides

[https://shanecharles.github.io/full-fun-web-with-safe-stack](https://shanecharles.github.io/full-fun-web-with-safe-stack)
