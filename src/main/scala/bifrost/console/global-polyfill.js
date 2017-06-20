var global = this;
var self = this;
var window = this;
var process = {env: {}};
var console = {};
console.debug = print;
console.warn = print;
console.log = print;
console.error = print;
console.trace = print;

Object.assign = function (t) {
  for (var s, i = 1, n = arguments.length; i < n; i++) {
    s = arguments[i];
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p))
      t[p] = s[p];
  }
  return t;
};
