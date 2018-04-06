# wsUI

LOCAL UI interface between Haskell and minimal browser where data is communicated via websocket protocol.

Idea is very simple:

- we translate selected javascript UI events to our haskell websocket server
- haskell gets events with listeners, which do something with the data received
- haskell sends some data back to JS listeners, which reactively update the UI 