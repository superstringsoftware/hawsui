# wsUI

LOCAL UI interface between Haskell and minimal browser where data is communicated via websocket protocol.

Idea is very simple:

- we translate selected javascript UI events to our haskell websocket server
- haskell gets events with listeners, which do something with the data received
- haskell sends some data back to JS listeners, which reactively update the UI

## Architecture considerations

Since we want to be haskell-centric, we want to give the means to call (more or less) arbitrary javascript from haskell via ws. So, this is RPC of sorts, or RJSFC over websocket in this case, 5-letter acronym, how cool is that? (Remote JS Function Call). If we implement this bus, it will give us sufficiently general mechanism to work with plain html / js, or React or any other library eventually.

In fact, this then can be used to create "bindings" for different js libs and call them from haskell absolutely transparently.

The only thing that needs to be implemented *from* the UI into haskell is Event translation. So, again, we can simply take a subset (or all) browser events, translate them into something haskell would understand, send them via ws, and process them in haskell. This way, browser becomes pretty dumb, even though we are sufficiently free to leave some manipulation on the client via React for instance.

### RJSFC

We need a universal interface of calling arbitrary javascript in the browser session connected to our haskell via websocket. To do this, we need:

- MainEventProcessor on js side, which will listen to ALL events, sort through them by type and call corresponding system handlers
- Function Calls event processor on js side, that plugs into MainEventProcessor and listens to function calls of a specific type -->
- to make it nicer, we can create separate Function Calls processors corresponding to different libraries - e.g., for global (window.) functions we'll have one, for React - another etc.

For a quick-n-dirty implementation, it is enough to have a "window" call processor, as well as specialized processor that finds a specific DOM Element by id and then calls whatever functions on it. It should give us enough UI manipulation to start.