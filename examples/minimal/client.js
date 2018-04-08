import mainProcessor from './EventProcessors.js';

export default function runMain() {
    console.log("starting up...");
    window.mainSocket = new WebSocket("ws://localhost:8080");
    window.mainSocket.onmessage = mainProcessor.processEvents;
}


export function newToDo(id) {
    console.log ("New ToDo creating...");
    var element = document.getElementById(id);
    txt = element.value;
    console.log ("Requested new todo:", txt);
    
    var callData = {
        eventName: "newToDo",
        eventData: txt
    }

    window.mainSocket.send(JSON.stringify (callData) );
    
}

export function fileChosen(evt) {
    var files = evt.target;
    console.log(target);
}