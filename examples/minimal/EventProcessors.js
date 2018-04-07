
class MainEventsProcessor {
    constructor(){
        this.globalEP = new CallEventsProcessor(window);
        this.processEvents = this.processEvents.bind(this); // I HATE javascript.
    }

    // main event processing function
    processEvents(event) {
        console.log(event);
        var obj = JSON.parse(event.data);
        console.log(obj);
        this.globalEP.callJS(obj);
    }
}


// universal class, processing function calls
// TBD: proper error handling!

// it expects the following object for it's function calls:
// {
//        funName  : [Text] - name of the function to call encoded as an array, e.g. console.log turns into ["console", "log"]
//        argArray : array of the arguments in case of the many-argument function OR
//        argVal   : single value in case of the single-argument function
//        isSingle : boolean to distinguish between previous 2 cases (YES, the burden is on HASKELL!!!)
// }
class CallEventsProcessor {
    // e.g., window for globals etc
    constructor (rootObj) {
        this.rootObj = rootObj;
    }

    // function that processes the event in the format above and calls the function
    callJS (evt) {
        var fn = this.resolveFunction(evt.funName);
        if (evt.isSingle) {
            fn.call (null, evt.argVal);
        } else {
            // TODO: change this BACK to support multi args!!!
            //fn.apply(null, evt.argArray);
            fn.call (null, evt.argVal);
        }
    }

    // get a function from encoded name array
    resolveFunction (funName) {
        var fn = this.rootObj[funName[0]];
        for (let i = 1; i<funName.length; i++)
            fn = fn[funName[i]];
        console.log(fn);
        return fn;
    }
}

export default new MainEventsProcessor();