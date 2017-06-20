/*
 Source is originated from https://github.com/morungos/java-xmlhttprequest

 Articles about Nashorn:
 - https://blog.codecentric.de/en/2014/06/project-nashorn-javascript-jvm-polyglott/
 */
(function nashornEventLoopMain(context) {
  'use strict';

  var System = Java.type('java.lang.System');
  var RequestBuilder = Packages.org.apache.http.client.methods.RequestBuilder;
  var FutureCallback = Packages.org.apache.http.concurrent.FutureCallback;
  var HttpAsyncClientBuilder = Packages.org.apache.http.impl.nio.client.HttpAsyncClientBuilder;
  var BasicHeader = Packages.org.apache.http.message.BasicHeader;
  var ArrayList = Java.type('java.util.ArrayList');
  var ByteArrayEntity = Packages.org.apache.http.entity.ByteArrayEntity;
  var StringEntity = Packages.org.apache.http.entity.StringEntity;
  var EntityBuilder = Packages.org.apache.http.client.entity.EntityBuilder;
  var ContentType = Packages.org.apache.http.entity.ContentType;

  var XMLHttpRequest = function () {
    var method, url, async, user, password, headers = {};

    var that = this;

    this.onreadystatechange = function () {
    };

    this.onload = function () {
    };
    this.onerror = function () {};

    this.readyState = 0;
    this.response = null;
    this.responseText = null;
    this.responseType = '';
    this.status = null;
    this.statusText = null;
    this.timeout = 0; // no timeout by default
    this.ontimeout = function () {
    };
    this.withCredentials = false;
    var requestBuilder = null;

    this.abort = function () {

    };

    this.getAllResponseHeaders = function () {

    };

    this.getResponseHeader = function (key) {

    };

    this.setRequestHeader = function (key, value) {
      headers[key] = value;
    };

    this.open = function (_method, _url, _async, _user, _password) {
      this.readyState = 1;

      method = _method;
      url = _url;

      async = _async === false ? false : true;

      user = _user || '';
      password = _password || '';

      requestBuilder = RequestBuilder.create(_method);
      requestBuilder.setUri(_url);

      // for (var prop in headers) {
      //   requestBuilder.addHeader(prop, headers[prop])
      // }

      context.setTimeout(this.onreadystatechange, 0);
    };

    this.send = function (data) {
      var that = this;

      var clientBuilder = HttpAsyncClientBuilder.create();
      var httpHeaders = new ArrayList(headers.length);

      for (var prop in headers) {
        httpHeaders.add(new BasicHeader(prop, headers[prop]));
      }

      if (window.__HTTP_SERVLET_REQUEST__) {
        var copyHeaders = ['Cookie', 'Authorization'];
        for (var i = 0; i < copyHeaders.length; i++) {
          httpHeaders.add(new BasicHeader(copyHeaders[i], window.__HTTP_SERVLET_REQUEST__.getHeader(copyHeaders[i])));
        }
      }

      clientBuilder.setDefaultHeaders(httpHeaders);

      if (data === undefined || data === null) {
        requestBuilder.setEntity(null);
      } else if (typeof data === 'string') {
        requestBuilder.setEntity(new StringEntity(data));
      } else {
        throw new Error('unsupported body data type');
      }

      var httpclient = clientBuilder.build();
      httpclient.start();

      var callback = new FutureCallback({
        completed: function (response) {
          that.readyState = 4;

          var body = org.apache.http.util.EntityUtils.toString(response.getEntity(), 'UTF-8');
          that.responseText = that.response = body;

          var finalException = null;
          if (that.responseType === 'json') {
            try {
              that.response = JSON.parse(that.response);
            } catch (e) {

              // Store the error
              finalException = e;

            }
          }

          if (finalException) {
            return;
          }

          var statusLine = response.getStatusLine();
          that.status = statusLine.getStatusCode();
          that.statusText = statusLine.getReasonPhrase();

          context.setTimeout(that.onreadystatechange, 0);
          context.setTimeout(that.onload, 0);
          httpclient.close();

        },
        cancelled: function () {
          context.System.err.println("Cancelled");
          httpclient.close();
        },
        failed: function (e) {

          that.readyState = 4;
          that.status = 0;
          that.statusText = e.getMessage();
          context.setTimeout(that.onreadystatechange, 0);
          context.setTimeout(that.onerror, 0);

          httpclient.close();
        }
      });

      httpclient.execute(requestBuilder.build(), null, callback);
    }
  }

  context.XMLHttpRequest = XMLHttpRequest;
})(typeof global !== "undefined" && global || typeof self !== "undefined" && self || this);
