function runMain() {
    console.log("starting up...");
    window.mainSocket = new WebSocket("ws://localhost:8080");
    window.mainSocket.onmessage = processEvents;
}

function processEvents (event) {
    console.log(event);
    var obj = JSON.parse(event.data);
    console.log(obj);
    var fn = window[obj.funName[0]];
    for (var i = 1; i<obj.funName.length; i++)
        fn = fn[obj.funName[i]]
    fn.call(null, obj.argVal);
  }

function newToDo(id) {
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