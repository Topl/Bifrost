/*
 Source is originated from https://github.com/morungos/java-xmlhttprequest

 Articles about Nashorn:
 - https://blog.codecentric.de/en/2014/06/project-nashorn-javascript-jvm-polyglott/

 How it work:
  in https://github.com/morungos/java-xmlhttprequest, it uses Timer to run setTimeout and setInterval task,
  but they are run in a separate thread of the Timer creates that is different with the main JavaScript thread.

  This implementation uses ScheduledExecutorService instead of Timer so the threads for task scheduling can be
  reused instead of each JavasScript thread create a Timer thread when using Timer.

  And most important thing is this adds global.nashornEventLoop and scheduled tasks only add function callback
  object in eventLoop (ArrayQueue), and it is main JavaScript thread to run these function callback by calling
  `global.nashornEventLoop.process();` at the end of JavaScript Application. It is just like browser or NodeJS
  that event loop is called when the main stack is cleared.

  When runs on server with Promise, remember to call `nashornEventLoop.process()` when waiting for Promise by
  Thread.sleep(), and call `nashornEventLoop.reset()` if server thread (e.g. Servlet thread) decides to be
  timeout so that eventLoop will be clean for next request.
 */
(function nashornEventLoopMain(context) {
  'use strict';

  var Thread = Java.type('java.lang.Thread');
  var Phaser = Java.type('java.util.concurrent.Phaser');
  var ArrayDeque = Java.type('java.util.ArrayDeque');
  var HashMap = Java.type('java.util.HashMap');
  var TimeUnit = Java.type("java.util.concurrent.TimeUnit");
  var Runnable = Java.type('java.lang.Runnable');


  var globalTimerId;
  var timerMap;
  var eventLoop;
  var phaser = new Phaser();

  // __NASHORN_POLYFILL_TIMER__ type is ScheduledExecutorService
  var scheduler = context.__NASHORN_POLYFILL_TIMER__;

  resetEventLoop();

   console.log('main javascript thread ' + Thread.currentThread().getName());

  function resetEventLoop() {
    globalTimerId = 1;
    if (timerMap) {
      timerMap.forEach(function (key, value) {
        value.cancel(true);
      })
    }
    timerMap = new HashMap();
    eventLoop = new ArrayDeque();
  }

  function waitForMessages() {
    phaser.register();
    var wait = !(eventLoop.size() === 0);
    phaser.arriveAndDeregister();

    return wait;
  }

  function processNextMessages() {
    var remaining = 1;
    while (remaining) {
      console.log('eventLoop size ' + eventLoop.size() + 'in thread ' + Thread.currentThread().getName());
      phaser.register();
      var message = eventLoop.removeFirst();
      remaining = eventLoop.size();
      phaser.arriveAndDeregister();

      var fn = message.fn;
      var args = message.args;

      try {
        console.log('processNextMessages in thread ' + Thread.currentThread().getName());
        fn.apply(context, args);
      } catch (e) {
        console.trace(e);
        console.trace(fn);
        console.trace(args);
      }
    }
  }

  context.nashornEventLoop = {
    process: function () {
      console.log('nashornEventLoop.process is called in thread ' + Thread.currentThread().getName())
      while (waitForMessages()) {
        processNextMessages()
      }
    },
    reset: resetEventLoop
  };


  function createRunnable(fn, timerId, args, repeated) {
    return new Runnable {
      run: function () {
        try {
          var phase = phaser.register();
          eventLoop.addLast({
            fn: fn,
            args: args
          });
           console.log('TimerTask add one event, and eventLoop size ' + eventLoop.size() +
             ' in thread ' + Thread.currentThread().getName());
        } catch (e) {
          console.trace(e);
        } finally {
          if (!repeated) timerMap.remove(timerId);
          phaser.arriveAndDeregister();
        }
      }
    }
  }

  var setTimeout = function (fn, millis /* [, args...] */) {
    var args = [].slice.call(arguments, 2, arguments.length);

    var timerId = globalTimerId++;
    var runnable = createRunnable(fn, timerId, args, false);

    var task = scheduler.schedule(runnable, millis, TimeUnit.MILLISECONDS);
    timerMap.put(timerId, task);

    return timerId;
  };

  var setImmediate = function (fn /* [, args...] */) {
    var args = [].slice.call(arguments, 1, arguments.length);
    return setTimeout(fn, 0, args);
  }

  var clearImmediate = function (timerId) {
    clearTimeout(timerId);
  }

  var clearTimeout = function (timerId) {
    var task = timerMap.get(timerId);
    if (task) {
      task.cancel();
      timerMap.remove(timerId);
    }
  };

  var setInterval = function (fn, delay /* [, args...] */) {
    var args = [].slice.call(arguments, 2, arguments.length);

    var timerId = globalTimerId++;
    var runnable = createRunnable(fn, timerId, args, true);
    var task = scheduler.scheduleWithFixedDelay(runnable, delay, delay, TimeUnit.MILLISECONDS);
    timerMap.put(timerId, task);

    return timerId;
  };

  var clearInterval = function (timerId) {
    clearTimeout(timerId);
  };

  context.setTimeout = setTimeout;
  context.clearTimeout = clearTimeout;
  context.setImmediate = setImmediate;
  context.clearImmediate = clearImmediate;
  context.setInterval = setInterval;
  context.clearInterval = clearInterval;
})(typeof global !== "undefined" && global || typeof self !== "undefined" && self || this);
