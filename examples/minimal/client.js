function runMain() {
    console.log("starting up...");
    window.mainSocket = new WebSocket("ws://localhost:8080");
    window.mainSocket.onmessage = processEvents;
}

function processEvents (event) {
    console.log(event.data);
  }

function newToDo(id) {
    console.log ("New ToDo creating...");
    var element = document.getElementById(id);
    txt = element.value;
    console.log ("Requested new todo:", txt);
    
    var callData = {
        eventName: "newToDo",
        eventData: { name: txt}
    }

    window.mainSocket.send(JSON.stringify (callData) );
    
}