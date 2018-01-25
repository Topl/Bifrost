var fun = function(){
'use strict';
/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Su00E9bastien Doeraene
 */

/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */





// Get the environment info
var $env = (typeof __ScalaJSEnv === "object" && __ScalaJSEnv) ? __ScalaJSEnv : {};

// Global scope
var $g =
  (typeof $env["global"] === "object" && $env["global"])
    ? $env["global"]
    : ((typeof global === "object" && global && global["Object"] === Object) ? global : this);
$env["global"] = $g;

// Where to send exports



var $e =
  (typeof $env["exportsNamespace"] === "object" && $env["exportsNamespace"])
    ? $env["exportsNamespace"] : $g;

$env["exportsNamespace"] = $e;

// Freeze the environment info
$g["Object"]["freeze"]($env);

// Linking info - must be in sync with scala.scalajs.runtime.LinkingInfo
var $linkingInfo = {
  "envInfo": $env,
  "semantics": {




    "asInstanceOfs": 1,








    "arrayIndexOutOfBounds": 1,










    "moduleInit": 2,





    "strictFloats": false,




    "productionMode": false

  },



  "assumingES6": false,

  "linkerVersion": "0.6.18",
  "globalThis": this
};
$g["Object"]["freeze"]($linkingInfo);
$g["Object"]["freeze"]($linkingInfo["semantics"]);

// Snapshots of builtins and polyfills






var $imul = $g["Math"]["imul"] || (function(a, b) {
  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  var ah = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
});

var $fround = $g["Math"]["fround"] ||









  (function(v) {
    return +v;
  });


var $clz32 = $g["Math"]["clz32"] || (function(i) {
  // See Hacker's Delight, Section 5-3
  if (i === 0) return 32;
  var r = 1;
  if ((i & 0xffff0000) === 0) { i <<= 16; r += 16; };
  if ((i & 0xff000000) === 0) { i <<= 8; r += 8; };
  if ((i & 0xf0000000) === 0) { i <<= 4; r += 4; };
  if ((i & 0xc0000000) === 0) { i <<= 2; r += 2; };
  return r + (i >> 31);
});


// Other fields




















var $lastIDHash = 0; // last value attributed to an id hash code



var $idHashCodeMap = $g["WeakMap"] ? new $g["WeakMap"]() : null;



// Core mechanism

var $makeIsArrayOfPrimitive = function(primitiveData) {
  return function(obj, depth) {
    return !!(obj && obj.$classData &&
      (obj.$classData.arrayDepth === depth) &&
      (obj.$classData.arrayBase === primitiveData));
  }
};


var $makeAsArrayOfPrimitive = function(isInstanceOfFunction, arrayEncodedName) {
  return function(obj, depth) {
    if (isInstanceOfFunction(obj, depth) || (obj === null))
      return obj;
    else
      $throwArrayCastException(obj, arrayEncodedName, depth);
  }
};


/** Encode a property name for runtime manipulation
  *  Usage:
  *    env.propertyName({someProp:0})
  *  Returns:
  *    "someProp"
  *  Useful when the property is renamed by a global optimizer (like Closure)
  *  but we must still get hold of a string of that name for runtime
  * reflection.
  */
var $propertyName = function(obj) {
  for (var prop in obj)
    return prop;
};

// Runtime functions

var $isScalaJSObject = function(obj) {
  return !!(obj && obj.$classData);
};


var $throwClassCastException = function(instance, classFullName) {




  throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new $c_jl_ClassCastException().init___T(
      instance + " is not an instance of " + classFullName));

};

var $throwArrayCastException = function(instance, classArrayEncodedName, depth) {
  for (; depth; --depth)
    classArrayEncodedName = "[" + classArrayEncodedName;
  $throwClassCastException(instance, classArrayEncodedName);
};



var $throwArrayIndexOutOfBoundsException = function(i) {
  var msg = (i === null) ? null : ("" + i);



  throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new $c_jl_ArrayIndexOutOfBoundsException().init___T(msg));

};


var $noIsInstance = function(instance) {
  throw new $g["TypeError"](
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
};

var $makeNativeArrayWrapper = function(arrayClassData, nativeArray) {
  return new arrayClassData.constr(nativeArray);
};

var $newArrayObject = function(arrayClassData, lengths) {
  return $newArrayObjectInternal(arrayClassData, lengths, 0);
};

var $newArrayObjectInternal = function(arrayClassData, lengths, lengthIndex) {
  var result = new arrayClassData.constr(lengths[lengthIndex]);

  if (lengthIndex < lengths.length-1) {
    var subArrayClassData = arrayClassData.componentData;
    var subLengthIndex = lengthIndex+1;
    var underlying = result.u;
    for (var i = 0; i < underlying.length; i++) {
      underlying[i] = $newArrayObjectInternal(
        subArrayClassData, lengths, subLengthIndex);
    }
  }

  return result;
};

var $objectToString = function(instance) {
  if (instance === void 0)
    return "undefined";
  else
    return instance.toString();
};

var $objectGetClass = function(instance) {
  switch (typeof instance) {
    case "string":
      return $d_T.getClassOf();
    case "number": {
      var v = instance | 0;
      if (v === instance) { // is the value integral?
        if ($isByte(v))
          return $d_jl_Byte.getClassOf();
        else if ($isShort(v))
          return $d_jl_Short.getClassOf();
        else
          return $d_jl_Integer.getClassOf();
      } else {
        if ($isFloat(instance))
          return $d_jl_Float.getClassOf();
        else
          return $d_jl_Double.getClassOf();
      }
    }
    case "boolean":
      return $d_jl_Boolean.getClassOf();
    case "undefined":
      return $d_sr_BoxedUnit.getClassOf();
    default:
      if (instance === null)
        return instance.getClass__jl_Class();
      else if ($is_sjsr_RuntimeLong(instance))
        return $d_jl_Long.getClassOf();
      else if ($isScalaJSObject(instance))
        return instance.$classData.getClassOf();
      else
        return null; // Exception?
  }
};

var $objectClone = function(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.clone__O();
  else
    throw new $c_jl_CloneNotSupportedException().init___();
};

var $objectNotify = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notify__V();
};

var $objectNotifyAll = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notifyAll__V();
};

var $objectFinalize = function(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    instance.finalize__V();
  // else no-op
};

var $objectEquals = function(instance, rhs) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.equals__O__Z(rhs);
  else if (typeof instance === "number")
    return typeof rhs === "number" && $numberEquals(instance, rhs);
  else
    return instance === rhs;
};

var $numberEquals = function(lhs, rhs) {
  return (lhs === rhs) ? (
    // 0.0.equals(-0.0) must be false
    lhs !== 0 || 1/lhs === 1/rhs
  ) : (
    // are they both NaN?
    (lhs !== lhs) && (rhs !== rhs)
  );
};

var $objectHashCode = function(instance) {
  switch (typeof instance) {
    case "string":
      return $m_sjsr_RuntimeString$().hashCode__T__I(instance);
    case "number":
      return $m_sjsr_Bits$().numberHashCode__D__I(instance);
    case "boolean":
      return instance ? 1231 : 1237;
    case "undefined":
      return 0;
    default:
      if ($isScalaJSObject(instance) || instance === null)
        return instance.hashCode__I();

      else if ($idHashCodeMap === null)
        return 42;

      else
        return $systemIdentityHashCode(instance);
  }
};

var $comparableCompareTo = function(instance, rhs) {
  switch (typeof instance) {
    case "string":

      $as_T(rhs);

      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    case "number":

      $as_jl_Number(rhs);

      return $m_jl_Double$().compare__D__D__I(instance, rhs);
    case "boolean":

      $asBoolean(rhs);

      return instance - rhs; // yes, this gives the right result
    default:
      return instance.compareTo__O__I(rhs);
  }
};

var $charSequenceLength = function(instance) {
  if (typeof(instance) === "string")

    return $uI(instance["length"]);



  else
    return instance.length__I();
};

var $charSequenceCharAt = function(instance, index) {
  if (typeof(instance) === "string")

    return $uI(instance["charCodeAt"](index)) & 0xffff;



  else
    return instance.charAt__I__C(index);
};

var $charSequenceSubSequence = function(instance, start, end) {
  if (typeof(instance) === "string")

    return $as_T(instance["substring"](start, end));



  else
    return instance.subSequence__I__I__jl_CharSequence(start, end);
};

var $booleanBooleanValue = function(instance) {
  if (typeof instance === "boolean") return instance;
  else                               return instance.booleanValue__Z();
};

var $numberByteValue = function(instance) {
  if (typeof instance === "number") return (instance << 24) >> 24;
  else                              return instance.byteValue__B();
};
var $numberShortValue = function(instance) {
  if (typeof instance === "number") return (instance << 16) >> 16;
  else                              return instance.shortValue__S();
};
var $numberIntValue = function(instance) {
  if (typeof instance === "number") return instance | 0;
  else                              return instance.intValue__I();
};
var $numberLongValue = function(instance) {
  if (typeof instance === "number")
    return $m_sjsr_RuntimeLong$().fromDouble__D__sjsr_RuntimeLong(instance);
  else
    return instance.longValue__J();
};
var $numberFloatValue = function(instance) {
  if (typeof instance === "number") return $fround(instance);
  else                              return instance.floatValue__F();
};
var $numberDoubleValue = function(instance) {
  if (typeof instance === "number") return instance;
  else                              return instance.doubleValue__D();
};

var $isNaN = function(instance) {
  return instance !== instance;
};

var $isInfinite = function(instance) {
  return !$g["isFinite"](instance) && !$isNaN(instance);
};

var $doubleToInt = function(x) {
  return (x > 2147483647) ? (2147483647) : ((x < -2147483648) ? -2147483648 : (x | 0));
};

/** Instantiates a JS object with variadic arguments to the constructor. */
var $newJSObjectWithVarargs = function(ctor, args) {
  // This basically emulates the ECMAScript specification for 'new'.
  var instance = $g["Object"]["create"](ctor.prototype);
  var result = ctor["apply"](instance, args);
  switch (typeof result) {
    case "string": case "number": case "boolean": case "undefined": case "symbol":
      return instance;
    default:
      return result === null ? instance : result;
  }
};

var $resolveSuperRef = function(initialProto, propName) {
  var getPrototypeOf = $g["Object"]["getPrototypeOf"];
  var getOwnPropertyDescriptor = $g["Object"]["getOwnPropertyDescriptor"];

  var superProto = getPrototypeOf(initialProto);
  while (superProto !== null) {
    var desc = getOwnPropertyDescriptor(superProto, propName);
    if (desc !== void 0)
      return desc;
    superProto = getPrototypeOf(superProto);
  }

  return void 0;
};

var $superGet = function(initialProto, self, propName) {
  var desc = $resolveSuperRef(initialProto, propName);
  if (desc !== void 0) {
    var getter = desc["get"];
    if (getter !== void 0)
      return getter["call"](self);
    else
      return desc["value"];
  }
  return void 0;
};

var $superSet = function(initialProto, self, propName, value) {
  var desc = $resolveSuperRef(initialProto, propName);
  if (desc !== void 0) {
    var setter = desc["set"];
    if (setter !== void 0) {
      setter["call"](self, value);
      return void 0;
    }
  }
  throw new $g["TypeError"]("super has no setter '" + propName + "'.");
};







var $propertiesOf = function(obj) {
  var result = [];
  for (var prop in obj)
    result["push"](prop);
  return result;
};

var $systemArraycopy = function(src, srcPos, dest, destPos, length) {
  var srcu = src.u;
  var destu = dest.u;


  if (srcPos < 0 || destPos < 0 || length < 0 ||
      (srcPos > ((srcu.length - length) | 0)) ||
      (destPos > ((destu.length - length) | 0))) {
    $throwArrayIndexOutOfBoundsException(null);
  }


  if (srcu !== destu || destPos < srcPos || (((srcPos + length) | 0) < destPos)) {
    for (var i = 0; i < length; i = (i + 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  } else {
    for (var i = (length - 1) | 0; i >= 0; i = (i - 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  }
};

var $systemIdentityHashCode =

  ($idHashCodeMap !== null) ?

  (function(obj) {
    switch (typeof obj) {
      case "string": case "number": case "boolean": case "undefined":
        return $objectHashCode(obj);
      default:
        if (obj === null) {
          return 0;
        } else {
          var hash = $idHashCodeMap["get"](obj);
          if (hash === void 0) {
            hash = ($lastIDHash + 1) | 0;
            $lastIDHash = hash;
            $idHashCodeMap["set"](obj, hash);
          }
          return hash;
        }
    }

  }) :
  (function(obj) {
    if ($isScalaJSObject(obj)) {
      var hash = obj["$idHashCode$0"];
      if (hash !== void 0) {
        return hash;
      } else if (!$g["Object"]["isSealed"](obj)) {
        hash = ($lastIDHash + 1) | 0;
        $lastIDHash = hash;
        obj["$idHashCode$0"] = hash;
        return hash;
      } else {
        return 42;
      }
    } else if (obj === null) {
      return 0;
    } else {
      return $objectHashCode(obj);
    }

  });

// is/as for hijacked boxed classes (the non-trivial ones)

var $isByte = function(v) {
  return typeof v === "number" && (v << 24 >> 24) === v && 1/v !== 1/-0;
};

var $isShort = function(v) {
  return typeof v === "number" && (v << 16 >> 16) === v && 1/v !== 1/-0;
};

var $isInt = function(v) {
  return typeof v === "number" && (v | 0) === v && 1/v !== 1/-0;
};

var $isFloat = function(v) {



  return typeof v === "number";

};


var $asUnit = function(v) {
  if (v === void 0 || v === null)
    return v;
  else
    $throwClassCastException(v, "scala.runtime.BoxedUnit");
};

var $asBoolean = function(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Boolean");
};

var $asByte = function(v) {
  if ($isByte(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Byte");
};

var $asShort = function(v) {
  if ($isShort(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Short");
};

var $asInt = function(v) {
  if ($isInt(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Integer");
};

var $asFloat = function(v) {
  if ($isFloat(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Float");
};

var $asDouble = function(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Double");
};


// Unboxes


var $uZ = function(value) {
  return !!$asBoolean(value);
};
var $uB = function(value) {
  return $asByte(value) | 0;
};
var $uS = function(value) {
  return $asShort(value) | 0;
};
var $uI = function(value) {
  return $asInt(value) | 0;
};
var $uJ = function(value) {
  return null === value ? $m_sjsr_RuntimeLong$().Zero$1
                        : $as_sjsr_RuntimeLong(value);
};
var $uF = function(value) {
  /* Here, it is fine to use + instead of fround, because asFloat already
   * ensures that the result is either null or a float.
   */
  return +$asFloat(value);
};
var $uD = function(value) {
  return +$asDouble(value);
};






// TypeArray conversions

var $byteArray2TypedArray = function(value) { return new $g["Int8Array"](value.u); };
var $shortArray2TypedArray = function(value) { return new $g["Int16Array"](value.u); };
var $charArray2TypedArray = function(value) { return new $g["Uint16Array"](value.u); };
var $intArray2TypedArray = function(value) { return new $g["Int32Array"](value.u); };
var $floatArray2TypedArray = function(value) { return new $g["Float32Array"](value.u); };
var $doubleArray2TypedArray = function(value) { return new $g["Float64Array"](value.u); };

var $typedArray2ByteArray = function(value) {
  var arrayClassData = $d_B.getArrayOf();
  return new arrayClassData.constr(new $g["Int8Array"](value));
};
var $typedArray2ShortArray = function(value) {
  var arrayClassData = $d_S.getArrayOf();
  return new arrayClassData.constr(new $g["Int16Array"](value));
};
var $typedArray2CharArray = function(value) {
  var arrayClassData = $d_C.getArrayOf();
  return new arrayClassData.constr(new $g["Uint16Array"](value));
};
var $typedArray2IntArray = function(value) {
  var arrayClassData = $d_I.getArrayOf();
  return new arrayClassData.constr(new $g["Int32Array"](value));
};
var $typedArray2FloatArray = function(value) {
  var arrayClassData = $d_F.getArrayOf();
  return new arrayClassData.constr(new $g["Float32Array"](value));
};
var $typedArray2DoubleArray = function(value) {
  var arrayClassData = $d_D.getArrayOf();
  return new arrayClassData.constr(new $g["Float64Array"](value));
};

// TypeData class


/** @constructor */
var $TypeData = function() {




  // Runtime support
  this.constr = void 0;
  this.parentData = void 0;
  this.ancestors = null;
  this.componentData = null;
  this.arrayBase = null;
  this.arrayDepth = 0;
  this.zero = null;
  this.arrayEncodedName = "";
  this._classOf = void 0;
  this._arrayOf = void 0;
  this.isArrayOf = void 0;

  // java.lang.Class support
  this["name"] = "";
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = false;
  this["isRawJSType"] = false;
  this["isInstance"] = void 0;
};


$TypeData.prototype.initPrim = function(



    zero, arrayEncodedName, displayName) {
  // Runtime support
  this.ancestors = {};
  this.componentData = null;
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  this.isArrayOf = function(obj, depth) { return false; };

  // java.lang.Class support
  this["name"] = displayName;
  this["isPrimitive"] = true;
  this["isInstance"] = function(obj) { return false; };

  return this;
};


$TypeData.prototype.initClass = function(



    internalNameObj, isInterface, fullName,
    ancestors, isRawJSType, parentData, isInstance, isArrayOf) {
  var internalName = $propertyName(internalNameObj);

  isInstance = isInstance || function(obj) {
    return !!(obj && obj.$classData && obj.$classData.ancestors[internalName]);
  };

  isArrayOf = isArrayOf || function(obj, depth) {
    return !!(obj && obj.$classData && (obj.$classData.arrayDepth === depth)
      && obj.$classData.arrayBase.ancestors[internalName])
  };

  // Runtime support
  this.parentData = parentData;
  this.ancestors = ancestors;
  this.arrayEncodedName = "L"+fullName+";";
  this.isArrayOf = isArrayOf;

  // java.lang.Class support
  this["name"] = fullName;
  this["isInterface"] = isInterface;
  this["isRawJSType"] = !!isRawJSType;
  this["isInstance"] = isInstance;

  return this;
};


$TypeData.prototype.initArray = function(



    componentData) {
  // The constructor

  var componentZero0 = componentData.zero;

  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet, when this file is read
  var componentZero = (componentZero0 == "longZero")
    ? $m_sjsr_RuntimeLong$().Zero$1
    : componentZero0;


  /** @constructor */
  var ArrayClass = function(arg) {
    if (typeof(arg) === "number") {
      // arg is the length of the array
      this.u = new Array(arg);
      for (var i = 0; i < arg; i++)
        this.u[i] = componentZero;
    } else {
      // arg is a native array that we wrap
      this.u = arg;
    }
  }
  ArrayClass.prototype = new $h_O;
  ArrayClass.prototype.constructor = ArrayClass;


  ArrayClass.prototype.get = function(i) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
    return this.u[i];
  };
  ArrayClass.prototype.set = function(i, v) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
    this.u[i] = v;
  };


  ArrayClass.prototype.clone__O = function() {
    if (this.u instanceof Array)
      return new ArrayClass(this.u["slice"](0));
    else
      // The underlying Array is a TypedArray
      return new ArrayClass(new this.u.constructor(this.u));
  };






































  ArrayClass.prototype.$classData = this;

  // Don't generate reflective call proxies. The compiler special cases
  // reflective calls to methods on scala.Array

  // The data

  var encodedName = "[" + componentData.arrayEncodedName;
  var componentBase = componentData.arrayBase || componentData;
  var arrayDepth = componentData.arrayDepth + 1;

  var isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  // Runtime support
  this.constr = ArrayClass;
  this.parentData = $d_O;
  this.ancestors = {O: 1, jl_Cloneable: 1, Ljava_io_Serializable: 1};
  this.componentData = componentData;
  this.arrayBase = componentBase;
  this.arrayDepth = arrayDepth;
  this.zero = null;
  this.arrayEncodedName = encodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = undefined;

  // java.lang.Class support
  this["name"] = encodedName;
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = true;
  this["isInstance"] = isInstance;

  return this;
};


$TypeData.prototype.getClassOf = function() {



  if (!this._classOf)
    this._classOf = new $c_jl_Class().init___jl_ScalaJSClassData(this);
  return this._classOf;
};


$TypeData.prototype.getArrayOf = function() {



  if (!this._arrayOf)
    this._arrayOf = new $TypeData().initArray(this);
  return this._arrayOf;
};

// java.lang.Class support


$TypeData.prototype["getFakeInstance"] = function() {



  if (this === $d_T)
    return "some string";
  else if (this === $d_jl_Boolean)
    return false;
  else if (this === $d_jl_Byte ||
           this === $d_jl_Short ||
           this === $d_jl_Integer ||
           this === $d_jl_Float ||
           this === $d_jl_Double)
    return 0;
  else if (this === $d_jl_Long)
    return $m_sjsr_RuntimeLong$().Zero$1;
  else if (this === $d_sr_BoxedUnit)
    return void 0;
  else
    return {$classData: this};
};


$TypeData.prototype["getSuperclass"] = function() {



  return this.parentData ? this.parentData.getClassOf() : null;
};


$TypeData.prototype["getComponentType"] = function() {



  return this.componentData ? this.componentData.getClassOf() : null;
};


$TypeData.prototype["newArrayOfThisClass"] = function(lengths) {



  var arrayClassData = this;
  for (var i = 0; i < lengths.length; i++)
    arrayClassData = arrayClassData.getArrayOf();
  return $newArrayObject(arrayClassData, lengths);
};




// Create primitive types

var $d_V = new $TypeData().initPrim(undefined, "V", "void");
var $d_Z = new $TypeData().initPrim(false, "Z", "boolean");
var $d_C = new $TypeData().initPrim(0, "C", "char");
var $d_B = new $TypeData().initPrim(0, "B", "byte");
var $d_S = new $TypeData().initPrim(0, "S", "short");
var $d_I = new $TypeData().initPrim(0, "I", "int");
var $d_J = new $TypeData().initPrim("longZero", "J", "long");
var $d_F = new $TypeData().initPrim(0.0, "F", "float");
var $d_D = new $TypeData().initPrim(0.0, "D", "double");

// Instance tests for array of primitives

var $isArrayOf_Z = $makeIsArrayOfPrimitive($d_Z);
$d_Z.isArrayOf = $isArrayOf_Z;

var $isArrayOf_C = $makeIsArrayOfPrimitive($d_C);
$d_C.isArrayOf = $isArrayOf_C;

var $isArrayOf_B = $makeIsArrayOfPrimitive($d_B);
$d_B.isArrayOf = $isArrayOf_B;

var $isArrayOf_S = $makeIsArrayOfPrimitive($d_S);
$d_S.isArrayOf = $isArrayOf_S;

var $isArrayOf_I = $makeIsArrayOfPrimitive($d_I);
$d_I.isArrayOf = $isArrayOf_I;

var $isArrayOf_J = $makeIsArrayOfPrimitive($d_J);
$d_J.isArrayOf = $isArrayOf_J;

var $isArrayOf_F = $makeIsArrayOfPrimitive($d_F);
$d_F.isArrayOf = $isArrayOf_F;

var $isArrayOf_D = $makeIsArrayOfPrimitive($d_D);
$d_D.isArrayOf = $isArrayOf_D;


// asInstanceOfs for array of primitives
var $asArrayOf_Z = $makeAsArrayOfPrimitive($isArrayOf_Z, "Z");
var $asArrayOf_C = $makeAsArrayOfPrimitive($isArrayOf_C, "C");
var $asArrayOf_B = $makeAsArrayOfPrimitive($isArrayOf_B, "B");
var $asArrayOf_S = $makeAsArrayOfPrimitive($isArrayOf_S, "S");
var $asArrayOf_I = $makeAsArrayOfPrimitive($isArrayOf_I, "I");
var $asArrayOf_J = $makeAsArrayOfPrimitive($isArrayOf_J, "J");
var $asArrayOf_F = $makeAsArrayOfPrimitive($isArrayOf_F, "F");
var $asArrayOf_D = $makeAsArrayOfPrimitive($isArrayOf_D, "D");

/** @constructor */
function $c_O() {
  /*<skip>*/
}
/** @constructor */
function $h_O() {
  /*<skip>*/
}
$h_O.prototype = $c_O.prototype;
$c_O.prototype.init___ = (function() {
  return this
});
$c_O.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
$c_O.prototype.toString__T = (function() {
  var jsx$2 = $objectGetClass(this).getName__T();
  var i = this.hashCode__I();
  var x = $uD((i >>> 0));
  var jsx$1 = x.toString(16);
  return ((jsx$2 + "@") + $as_T(jsx$1))
});
$c_O.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this)
});
$c_O.prototype.toString = (function() {
  return this.toString__T()
});
function $is_O(obj) {
  return (obj !== null)
}
function $as_O(obj) {
  return obj
}
function $isArrayOf_O(obj, depth) {
  var data = (obj && obj.$classData);
  if ((!data)) {
    return false
  } else {
    var arrayDepth = (data.arrayDepth || 0);
    return ((!(arrayDepth < depth)) && ((arrayDepth > depth) || (!data.arrayBase.isPrimitive)))
  }
}
function $asArrayOf_O(obj, depth) {
  return (($isArrayOf_O(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Object;", depth))
}
var $d_O = new $TypeData().initClass({
  O: 0
}, false, "java.lang.Object", {
  O: 1
}, (void 0), (void 0), $is_O, $isArrayOf_O);
$c_O.prototype.$classData = $d_O;
function $f_s_math_ScalaNumericAnyConversions__unifiedPrimitiveEquals__O__Z($thiz, x) {
  if ($is_jl_Character(x)) {
    if ((x === null)) {
      var x2 = 0
    } else {
      var this$2 = $as_jl_Character(x);
      var x2 = this$2.value$1
    };
    return ($thiz.isValidChar__Z() && ($thiz.intValue__I() === x2))
  } else if ($isByte(x)) {
    var x3 = $uB(x);
    return ($thiz.isValidByte__Z() && ($thiz.byteValue__B() === x3))
  } else if ($isShort(x)) {
    var x4 = $uS(x);
    return ($thiz.isValidShort__Z() && ($thiz.shortValue__S() === x4))
  } else if ($isInt(x)) {
    var x5 = $uI(x);
    return ($thiz.isValidInt__Z() && ($thiz.intValue__I() === x5))
  } else if ($is_sjsr_RuntimeLong(x)) {
    var t = $uJ(x);
    var lo = t.lo$2;
    var hi = t.hi$2;
    var t$1 = $thiz.longValue__J();
    var lo$1 = t$1.lo$2;
    var hi$1 = t$1.hi$2;
    return ((lo$1 === lo) && (hi$1 === hi))
  } else if ($isFloat(x)) {
    var x7 = $uF(x);
    return ($thiz.floatValue__F() === x7)
  } else if (((typeof x) === "number")) {
    var x8 = $uD(x);
    return ($thiz.doubleValue__D() === x8)
  } else {
    return false
  }
}
function $f_s_math_ScalaNumericAnyConversions__unifiedPrimitiveHashcode__I($thiz) {
  var t = $thiz.longValue__J();
  var lo = t.lo$2;
  var hi = t.hi$2;
  return ((((hi === (-1)) ? (((-2147483648) ^ lo) >= 0) : (hi > (-1))) && ((hi === 0) ? (((-2147483648) ^ lo) <= (-1)) : (hi < 0))) ? lo : $m_sr_Statics$().longHash__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi)))
}
function $f_s_util_control_NoStackTrace__fillInStackTrace__jl_Throwable($thiz) {
  var this$1 = $m_s_util_control_NoStackTrace$();
  if (this$1.$$undnoSuppression$1) {
    return $c_jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call($thiz)
  } else {
    return $as_jl_Throwable($thiz)
  }
}
function $f_sci_VectorPointer__copyOf__AO__AO($thiz, a) {
  var copy = $newArrayObject($d_O.getArrayOf(), [a.u.length]);
  $systemArraycopy(a, 0, copy, 0, a.u.length);
  return copy
}
function $f_sci_VectorPointer__gotoNextBlockStart__I__I__V($thiz, index, xor) {
  if ((xor < 1024)) {
    $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
  } else if ((xor < 32768)) {
    $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1));
    $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get(0), 1))
  } else if ((xor < 1048576)) {
    $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get((31 & ((index >>> 15) | 0))), 1));
    $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get(0), 1));
    $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get(0), 1))
  } else if ((xor < 33554432)) {
    $thiz.display3$und$eq__AO__V($asArrayOf_O($thiz.display4__AO().get((31 & ((index >>> 20) | 0))), 1));
    $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get(0), 1));
    $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get(0), 1));
    $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get(0), 1))
  } else if ((xor < 1073741824)) {
    $thiz.display4$und$eq__AO__V($asArrayOf_O($thiz.display5__AO().get((31 & ((index >>> 25) | 0))), 1));
    $thiz.display3$und$eq__AO__V($asArrayOf_O($thiz.display4__AO().get(0), 1));
    $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get(0), 1));
    $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get(0), 1));
    $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get(0), 1))
  } else {
    throw new $c_jl_IllegalArgumentException().init___()
  }
}
function $f_sci_VectorPointer__getElem__I__I__O($thiz, index, xor) {
  if ((xor < 32)) {
    return $thiz.display0__AO().get((31 & index))
  } else if ((xor < 1024)) {
    return $asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1).get((31 & index))
  } else if ((xor < 32768)) {
    return $asArrayOf_O($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1).get((31 & ((index >>> 5) | 0))), 1).get((31 & index))
  } else if ((xor < 1048576)) {
    return $asArrayOf_O($asArrayOf_O($asArrayOf_O($thiz.display3__AO().get((31 & ((index >>> 15) | 0))), 1).get((31 & ((index >>> 10) | 0))), 1).get((31 & ((index >>> 5) | 0))), 1).get((31 & index))
  } else if ((xor < 33554432)) {
    return $asArrayOf_O($asArrayOf_O($asArrayOf_O($asArrayOf_O($thiz.display4__AO().get((31 & ((index >>> 20) | 0))), 1).get((31 & ((index >>> 15) | 0))), 1).get((31 & ((index >>> 10) | 0))), 1).get((31 & ((index >>> 5) | 0))), 1).get((31 & index))
  } else if ((xor < 1073741824)) {
    return $asArrayOf_O($asArrayOf_O($asArrayOf_O($asArrayOf_O($asArrayOf_O($thiz.display5__AO().get((31 & ((index >>> 25) | 0))), 1).get((31 & ((index >>> 20) | 0))), 1).get((31 & ((index >>> 15) | 0))), 1).get((31 & ((index >>> 10) | 0))), 1).get((31 & ((index >>> 5) | 0))), 1).get((31 & index))
  } else {
    throw new $c_jl_IllegalArgumentException().init___()
  }
}
function $f_sci_VectorPointer__gotoPos__I__I__V($thiz, index, xor) {
  if ((xor >= 32)) {
    if ((xor < 1024)) {
      $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
    } else if ((xor < 32768)) {
      $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1));
      $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
    } else if ((xor < 1048576)) {
      $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get((31 & ((index >>> 15) | 0))), 1));
      $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1));
      $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
    } else if ((xor < 33554432)) {
      $thiz.display3$und$eq__AO__V($asArrayOf_O($thiz.display4__AO().get((31 & ((index >>> 20) | 0))), 1));
      $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get((31 & ((index >>> 15) | 0))), 1));
      $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1));
      $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
    } else if ((xor < 1073741824)) {
      $thiz.display4$und$eq__AO__V($asArrayOf_O($thiz.display5__AO().get((31 & ((index >>> 25) | 0))), 1));
      $thiz.display3$und$eq__AO__V($asArrayOf_O($thiz.display4__AO().get((31 & ((index >>> 20) | 0))), 1));
      $thiz.display2$und$eq__AO__V($asArrayOf_O($thiz.display3__AO().get((31 & ((index >>> 15) | 0))), 1));
      $thiz.display1$und$eq__AO__V($asArrayOf_O($thiz.display2__AO().get((31 & ((index >>> 10) | 0))), 1));
      $thiz.display0$und$eq__AO__V($asArrayOf_O($thiz.display1__AO().get((31 & ((index >>> 5) | 0))), 1))
    } else {
      throw new $c_jl_IllegalArgumentException().init___()
    }
  }
}
function $f_sci_VectorPointer__stabilize__I__V($thiz, index) {
  var x1 = (((-1) + $thiz.depth__I()) | 0);
  switch (x1) {
    case 5: {
      var a = $thiz.display5__AO();
      $thiz.display5$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a));
      var a$1 = $thiz.display4__AO();
      $thiz.display4$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$1));
      var a$2 = $thiz.display3__AO();
      $thiz.display3$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$2));
      var a$3 = $thiz.display2__AO();
      $thiz.display2$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$3));
      var a$4 = $thiz.display1__AO();
      $thiz.display1$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$4));
      $thiz.display5__AO().set((31 & ((index >>> 25) | 0)), $thiz.display4__AO());
      $thiz.display4__AO().set((31 & ((index >>> 20) | 0)), $thiz.display3__AO());
      $thiz.display3__AO().set((31 & ((index >>> 15) | 0)), $thiz.display2__AO());
      $thiz.display2__AO().set((31 & ((index >>> 10) | 0)), $thiz.display1__AO());
      $thiz.display1__AO().set((31 & ((index >>> 5) | 0)), $thiz.display0__AO());
      break
    }
    case 4: {
      var a$5 = $thiz.display4__AO();
      $thiz.display4$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$5));
      var a$6 = $thiz.display3__AO();
      $thiz.display3$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$6));
      var a$7 = $thiz.display2__AO();
      $thiz.display2$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$7));
      var a$8 = $thiz.display1__AO();
      $thiz.display1$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$8));
      $thiz.display4__AO().set((31 & ((index >>> 20) | 0)), $thiz.display3__AO());
      $thiz.display3__AO().set((31 & ((index >>> 15) | 0)), $thiz.display2__AO());
      $thiz.display2__AO().set((31 & ((index >>> 10) | 0)), $thiz.display1__AO());
      $thiz.display1__AO().set((31 & ((index >>> 5) | 0)), $thiz.display0__AO());
      break
    }
    case 3: {
      var a$9 = $thiz.display3__AO();
      $thiz.display3$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$9));
      var a$10 = $thiz.display2__AO();
      $thiz.display2$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$10));
      var a$11 = $thiz.display1__AO();
      $thiz.display1$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$11));
      $thiz.display3__AO().set((31 & ((index >>> 15) | 0)), $thiz.display2__AO());
      $thiz.display2__AO().set((31 & ((index >>> 10) | 0)), $thiz.display1__AO());
      $thiz.display1__AO().set((31 & ((index >>> 5) | 0)), $thiz.display0__AO());
      break
    }
    case 2: {
      var a$12 = $thiz.display2__AO();
      $thiz.display2$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$12));
      var a$13 = $thiz.display1__AO();
      $thiz.display1$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$13));
      $thiz.display2__AO().set((31 & ((index >>> 10) | 0)), $thiz.display1__AO());
      $thiz.display1__AO().set((31 & ((index >>> 5) | 0)), $thiz.display0__AO());
      break
    }
    case 1: {
      var a$14 = $thiz.display1__AO();
      $thiz.display1$und$eq__AO__V($f_sci_VectorPointer__copyOf__AO__AO($thiz, a$14));
      $thiz.display1__AO().set((31 & ((index >>> 5) | 0)), $thiz.display0__AO());
      break
    }
    case 0: {
      break
    }
    default: {
      throw new $c_s_MatchError().init___O(x1)
    }
  }
}
function $f_sci_VectorPointer__initFrom__sci_VectorPointer__I__V($thiz, that, depth) {
  $thiz.depth$und$eq__I__V(depth);
  var x1 = (((-1) + depth) | 0);
  switch (x1) {
    case (-1): {
      break
    }
    case 0: {
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    case 1: {
      $thiz.display1$und$eq__AO__V(that.display1__AO());
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    case 2: {
      $thiz.display2$und$eq__AO__V(that.display2__AO());
      $thiz.display1$und$eq__AO__V(that.display1__AO());
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    case 3: {
      $thiz.display3$und$eq__AO__V(that.display3__AO());
      $thiz.display2$und$eq__AO__V(that.display2__AO());
      $thiz.display1$und$eq__AO__V(that.display1__AO());
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    case 4: {
      $thiz.display4$und$eq__AO__V(that.display4__AO());
      $thiz.display3$und$eq__AO__V(that.display3__AO());
      $thiz.display2$und$eq__AO__V(that.display2__AO());
      $thiz.display1$und$eq__AO__V(that.display1__AO());
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    case 5: {
      $thiz.display5$und$eq__AO__V(that.display5__AO());
      $thiz.display4$und$eq__AO__V(that.display4__AO());
      $thiz.display3$und$eq__AO__V(that.display3__AO());
      $thiz.display2$und$eq__AO__V(that.display2__AO());
      $thiz.display1$und$eq__AO__V(that.display1__AO());
      $thiz.display0$und$eq__AO__V(that.display0__AO());
      break
    }
    default: {
      throw new $c_s_MatchError().init___O(x1)
    }
  }
}
/** @constructor */
function $c_Ljava_math_BigInteger$QuotAndRem() {
  $c_O.call(this);
  this.quot$1 = null;
  this.rem$1 = null
}
$c_Ljava_math_BigInteger$QuotAndRem.prototype = new $h_O();
$c_Ljava_math_BigInteger$QuotAndRem.prototype.constructor = $c_Ljava_math_BigInteger$QuotAndRem;
/** @constructor */
function $h_Ljava_math_BigInteger$QuotAndRem() {
  /*<skip>*/
}
$h_Ljava_math_BigInteger$QuotAndRem.prototype = $c_Ljava_math_BigInteger$QuotAndRem.prototype;
$c_Ljava_math_BigInteger$QuotAndRem.prototype.toArray__ALjava_math_BigInteger = (function() {
  var xs = new $c_sjs_js_WrappedArray().init___sjs_js_Array([this.quot$1, this.rem$1]);
  var len = $uI(xs.array$6.length);
  var array = $newArrayObject($d_Ljava_math_BigInteger.getArrayOf(), [len]);
  var elem$1 = 0;
  elem$1 = 0;
  var this$4 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(xs, 0, $uI(xs.array$6.length));
  while (this$4.hasNext__Z()) {
    var arg1 = this$4.next__O();
    array.set(elem$1, arg1);
    elem$1 = ((1 + elem$1) | 0)
  };
  return array
});
$c_Ljava_math_BigInteger$QuotAndRem.prototype.init___Ljava_math_BigInteger__Ljava_math_BigInteger = (function(quot, rem) {
  this.quot$1 = quot;
  this.rem$1 = rem;
  return this
});
var $d_Ljava_math_BigInteger$QuotAndRem = new $TypeData().initClass({
  Ljava_math_BigInteger$QuotAndRem: 0
}, false, "java.math.BigInteger$QuotAndRem", {
  Ljava_math_BigInteger$QuotAndRem: 1,
  O: 1
});
$c_Ljava_math_BigInteger$QuotAndRem.prototype.$classData = $d_Ljava_math_BigInteger$QuotAndRem;
/** @constructor */
function $c_Ljava_math_BitLevel$() {
  $c_O.call(this)
}
$c_Ljava_math_BitLevel$.prototype = new $h_O();
$c_Ljava_math_BitLevel$.prototype.constructor = $c_Ljava_math_BitLevel$;
/** @constructor */
function $h_Ljava_math_BitLevel$() {
  /*<skip>*/
}
$h_Ljava_math_BitLevel$.prototype = $c_Ljava_math_BitLevel$.prototype;
$c_Ljava_math_BitLevel$.prototype.init___ = (function() {
  return this
});
$c_Ljava_math_BitLevel$.prototype.shiftLeftOneBit__AI__AI__I__V = (function(result, source, srcLen) {
  var elem$1 = 0;
  elem$1 = 0;
  var isEmpty$4 = (srcLen <= 0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + srcLen) | 0);
  if ((!isEmpty$4)) {
    var i = 0;
    while (true) {
      var v1 = i;
      var iVal = source.get(v1);
      result.set(v1, ((iVal << 1) | elem$1));
      elem$1 = ((iVal >>> 31) | 0);
      if ((i === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i = ((1 + i) | 0)
    }
  };
  if ((elem$1 !== 0)) {
    result.set(srcLen, elem$1)
  }
});
$c_Ljava_math_BitLevel$.prototype.shiftLeft__Ljava_math_BigInteger__I__Ljava_math_BigInteger = (function(source, count) {
  var intCount = (count >> 5);
  var andCount = (31 & count);
  var offset = ((andCount === 0) ? 0 : 1);
  var resLength = ((((source.numberLength$2 + intCount) | 0) + offset) | 0);
  var resDigits = $newArrayObject($d_I.getArrayOf(), [resLength]);
  this.shiftLeft__AI__AI__I__I__V(resDigits, source.digits$2, intCount, andCount);
  var result = new $c_Ljava_math_BigInteger().init___I__I__AI(source.sign$2, resLength, resDigits);
  result.cutOffLeadingZeroes__V();
  return result
});
$c_Ljava_math_BitLevel$.prototype.shiftRight__Ljava_math_BigInteger__I__Ljava_math_BigInteger = (function(source, count) {
  var intCount = (count >> 5);
  var andCount = (31 & count);
  if ((intCount >= source.numberLength$2)) {
    return ((source.sign$2 < 0) ? $m_Ljava_math_BigInteger$().MINUS$undONE$1 : $m_Ljava_math_BigInteger$().ZERO$1)
  } else {
    var resLength = ((source.numberLength$2 - intCount) | 0);
    var resDigits = $newArrayObject($d_I.getArrayOf(), [((1 + resLength) | 0)]);
    this.shiftRight__AI__I__AI__I__I__Z(resDigits, resLength, source.digits$2, intCount, andCount);
    if ((source.sign$2 < 0)) {
      var i = 0;
      while (((i < intCount) && (source.digits$2.get(i) === 0))) {
        i = ((1 + i) | 0)
      };
      var cmp = ((source.digits$2.get(i) << ((32 - andCount) | 0)) !== 0);
      if (((i < intCount) || ((andCount > 0) && cmp))) {
        i = 0;
        while (((i < resLength) && (resDigits.get(i) === (-1)))) {
          resDigits.set(i, 0);
          i = ((1 + i) | 0)
        };
        if ((i === resLength)) {
          resLength = ((1 + resLength) | 0)
        };
        var ev$6 = i;
        resDigits.set(ev$6, ((1 + resDigits.get(ev$6)) | 0))
      }
    };
    var result = new $c_Ljava_math_BigInteger().init___I__I__AI(source.sign$2, resLength, resDigits);
    result.cutOffLeadingZeroes__V();
    return result
  }
});
$c_Ljava_math_BitLevel$.prototype.shiftLeftOneBit__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(source) {
  var srcLen = source.numberLength$2;
  var resLen = ((1 + srcLen) | 0);
  var resDigits = $newArrayObject($d_I.getArrayOf(), [resLen]);
  this.shiftLeftOneBit__AI__AI__I__V(resDigits, source.digits$2, srcLen);
  var result = new $c_Ljava_math_BigInteger().init___I__I__AI(source.sign$2, resLen, resDigits);
  result.cutOffLeadingZeroes__V();
  return result
});
$c_Ljava_math_BitLevel$.prototype.bitLength__Ljava_math_BigInteger__I = (function(bi) {
  if ((bi.sign$2 === 0)) {
    return 0
  } else {
    var bLength = (bi.numberLength$2 << 5);
    var highDigit = bi.digits$2.get((((-1) + bi.numberLength$2) | 0));
    if ((bi.sign$2 < 0)) {
      var i = bi.getFirstNonzeroDigit__I();
      if ((i === (((-1) + bi.numberLength$2) | 0))) {
        highDigit = (((-1) + highDigit) | 0)
      }
    };
    bLength = ((bLength - $clz32(highDigit)) | 0);
    return bLength
  }
});
$c_Ljava_math_BitLevel$.prototype.inplaceShiftRight__Ljava_math_BigInteger__I__V = (function(bi, count) {
  var sign = bi.sign$2;
  if ((!((count === 0) || (bi.sign$2 === 0)))) {
    var intCount = (count >> 5);
    bi.numberLength$2 = ((bi.numberLength$2 - intCount) | 0);
    var shift = this.shiftRight__AI__I__AI__I__I__Z(bi.digits$2, bi.numberLength$2, bi.digits$2, intCount, (31 & count));
    if (((!shift) && (sign < 0))) {
      var i = 0;
      while (((i < bi.numberLength$2) && (bi.digits$2.get(i) === (-1)))) {
        bi.digits$2.set(i, 0);
        i = ((1 + i) | 0)
      };
      if ((i === bi.numberLength$2)) {
        bi.numberLength$2 = ((1 + bi.numberLength$2) | 0)
      };
      var ev$3 = bi.digits$2;
      var ev$4 = i;
      ev$3.set(ev$4, ((1 + ev$3.get(ev$4)) | 0))
    };
    bi.cutOffLeadingZeroes__V();
    bi.java$math$BigInteger$$firstNonzeroDigit$2 = (-2)
  }
});
$c_Ljava_math_BitLevel$.prototype.shiftRight__AI__I__AI__I__I__Z = (function(result, resultLen, source, intCount, count) {
  var i = 0;
  var allZero = true;
  while ((i < intCount)) {
    allZero = (!(!(allZero & (source.get(i) === 0))));
    i = ((1 + i) | 0)
  };
  if ((count === 0)) {
    $systemArraycopy(source, intCount, result, 0, resultLen)
  } else {
    var leftShiftCount = ((32 - count) | 0);
    allZero = (!(!(allZero & ((source.get(i) << leftShiftCount) === 0))));
    i = 0;
    while ((i < (((-1) + resultLen) | 0))) {
      result.set(i, (((source.get(((i + intCount) | 0)) >>> count) | 0) | (source.get(((1 + ((i + intCount) | 0)) | 0)) << leftShiftCount)));
      i = ((1 + i) | 0)
    };
    result.set(i, ((source.get(((i + intCount) | 0)) >>> count) | 0));
    i = ((1 + i) | 0)
  };
  return allZero
});
$c_Ljava_math_BitLevel$.prototype.shiftLeft__AI__AI__I__I__V = (function(result, source, intCount, count) {
  if ((count === 0)) {
    $systemArraycopy(source, 0, result, intCount, ((result.u.length - intCount) | 0))
  } else {
    var rightShiftCount = ((32 - count) | 0);
    result.set((((-1) + result.u.length) | 0), 0);
    var i = (((-1) + result.u.length) | 0);
    while ((i > intCount)) {
      var ev$5 = i;
      result.set(ev$5, (result.get(ev$5) | ((source.get((((-1) + ((i - intCount) | 0)) | 0)) >>> rightShiftCount) | 0)));
      result.set((((-1) + i) | 0), (source.get((((-1) + ((i - intCount) | 0)) | 0)) << count));
      i = (((-1) + i) | 0)
    }
  };
  var isEmpty$4 = (intCount <= 0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + intCount) | 0);
  if ((!isEmpty$4)) {
    var i$1 = 0;
    while (true) {
      var v1 = i$1;
      result.set(v1, 0);
      if ((i$1 === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i$1 = ((1 + i$1) | 0)
    }
  }
});
var $d_Ljava_math_BitLevel$ = new $TypeData().initClass({
  Ljava_math_BitLevel$: 0
}, false, "java.math.BitLevel$", {
  Ljava_math_BitLevel$: 1,
  O: 1
});
$c_Ljava_math_BitLevel$.prototype.$classData = $d_Ljava_math_BitLevel$;
var $n_Ljava_math_BitLevel$ = (void 0);
function $m_Ljava_math_BitLevel$() {
  if ((!$n_Ljava_math_BitLevel$)) {
    $n_Ljava_math_BitLevel$ = new $c_Ljava_math_BitLevel$().init___()
  };
  return $n_Ljava_math_BitLevel$
}
/** @constructor */
function $c_Ljava_math_Conversion$() {
  $c_O.call(this);
  this.DigitFitInInt$1 = null;
  this.BigRadices$1 = null
}
$c_Ljava_math_Conversion$.prototype = new $h_O();
$c_Ljava_math_Conversion$.prototype.constructor = $c_Ljava_math_Conversion$;
/** @constructor */
function $h_Ljava_math_Conversion$() {
  /*<skip>*/
}
$h_Ljava_math_Conversion$.prototype = $c_Ljava_math_Conversion$.prototype;
$c_Ljava_math_Conversion$.prototype.init___ = (function() {
  $n_Ljava_math_Conversion$ = this;
  var xs = new $c_sjs_js_WrappedArray().init___sjs_js_Array([(-1), (-1), 31, 19, 15, 13, 11, 11, 10, 9, 9, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5]);
  var len = $uI(xs.array$6.length);
  var array = $newArrayObject($d_I.getArrayOf(), [len]);
  var elem$1 = 0;
  elem$1 = 0;
  var this$5 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(xs, 0, $uI(xs.array$6.length));
  while (this$5.hasNext__Z()) {
    var arg1 = this$5.next__O();
    array.set(elem$1, $uI(arg1));
    elem$1 = ((1 + elem$1) | 0)
  };
  this.DigitFitInInt$1 = array;
  var xs$1 = new $c_sjs_js_WrappedArray().init___sjs_js_Array([(-2147483648), 1162261467, 1073741824, 1220703125, 362797056, 1977326743, 1073741824, 387420489, 1000000000, 214358881, 429981696, 815730721, 1475789056, 170859375, 268435456, 410338673, 612220032, 893871739, 1280000000, 1801088541, 113379904, 148035889, 191102976, 244140625, 308915776, 387420489, 481890304, 594823321, 729000000, 887503681, 1073741824, 1291467969, 1544804416, 1838265625, 60466176]);
  var len$1 = $uI(xs$1.array$6.length);
  var array$1 = $newArrayObject($d_I.getArrayOf(), [len$1]);
  var elem$1$1 = 0;
  elem$1$1 = 0;
  var this$10 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(xs$1, 0, $uI(xs$1.array$6.length));
  while (this$10.hasNext__Z()) {
    var arg1$1 = this$10.next__O();
    array$1.set(elem$1$1, $uI(arg1$1));
    elem$1$1 = ((1 + elem$1$1) | 0)
  };
  this.BigRadices$1 = array$1;
  return this
});
$c_Ljava_math_Conversion$.prototype.toDecimalScaledString__J__I__T = (function(value, scale) {
  if (((value.lo$2 === 0) && (value.hi$2 === 0))) {
    switch (scale) {
      case 0: {
        return "0";
        break
      }
      case 1: {
        return "0.0";
        break
      }
      case 2: {
        return "0.00";
        break
      }
      case 3: {
        return "0.000";
        break
      }
      case 4: {
        return "0.0000";
        break
      }
      case 5: {
        return "0.00000";
        break
      }
      case 6: {
        return "0.000000";
        break
      }
      default: {
        if ((scale === (-2147483648))) {
          var scaleVal = "2147483648"
        } else {
          var i = ((-scale) | 0);
          var scaleVal = ("" + i)
        };
        var result = ((scale < 0) ? "0E+" : "0E");
        return (result + scaleVal)
      }
    }
  } else {
    var ahi = value.hi$2;
    var negNumber = (ahi < 0);
    var elem$1 = null;
    elem$1 = "";
    var currentChar = 18;
    if (negNumber) {
      var lo = value.lo$2;
      var hi = value.hi$2;
      var lo$1 = ((-lo) | 0);
      var hi$1 = ((lo !== 0) ? (~hi) : ((-hi) | 0));
      var x_$_lo$2 = lo$1;
      var x_$_hi$2 = hi$1;
      var t = new $c_sjsr_RuntimeLong().init___I__I(x_$_lo$2, x_$_hi$2)
    } else {
      var t = value
    };
    var lo$2 = t.lo$2;
    var hi$2 = t.hi$2;
    var v_$_lo$2 = lo$2;
    var v_$_hi$2 = hi$2;
    while (true) {
      var prev_$_lo$2 = v_$_lo$2;
      var prev_$_hi$2 = v_$_hi$2;
      var this$5_$_lo$2 = v_$_lo$2;
      var this$5_$_hi$2 = v_$_hi$2;
      var this$6 = $m_sjsr_RuntimeLong$();
      var lo$3 = this$6.divideImpl__I__I__I__I__I(this$5_$_lo$2, this$5_$_hi$2, 10, 0);
      var hi$3 = this$6.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      var jsx$1_$_lo$2 = lo$3;
      var jsx$1_$_hi$2 = hi$3;
      v_$_lo$2 = jsx$1_$_lo$2;
      v_$_hi$2 = jsx$1_$_hi$2;
      currentChar = (((-1) + currentChar) | 0);
      var b_$_lo$2 = v_$_lo$2;
      var b_$_hi$2 = v_$_hi$2;
      var blo = b_$_lo$2;
      var b0 = (65535 & blo);
      var b1 = ((blo >>> 16) | 0);
      var a0b0 = $imul(10, b0);
      var a0b1 = $imul(10, b1);
      var lo$4 = ((a0b0 + (a0b1 << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$4 = (($imul(10, b_$_hi$2) + ((c1part >>> 16) | 0)) | 0);
      var alo = prev_$_lo$2;
      var ahi$1 = prev_$_hi$2;
      var lo$5 = ((alo - lo$4) | 0);
      var hi$5 = ((((-2147483648) ^ lo$5) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi$1 - hi$4) | 0)) | 0) : ((ahi$1 - hi$4) | 0));
      var lo$6 = ((48 + lo$5) | 0);
      var c = (65535 & lo$6);
      elem$1 = (("" + new $c_jl_Character().init___C(c)) + $as_T(elem$1));
      var this$11_$_lo$2 = v_$_lo$2;
      var this$11_$_hi$2 = v_$_hi$2;
      if ((!((this$11_$_lo$2 === 0) && (this$11_$_hi$2 === 0)))) {
        /*<skip>*/
      } else {
        break
      }
    };
    var exponent = (((-1) + ((((18 - currentChar) | 0) - scale) | 0)) | 0);
    if (((scale > 0) && (exponent >= (-6)))) {
      var index = ((1 + exponent) | 0);
      if ((index > 0)) {
        var thiz = $as_T(elem$1);
        var jsx$2 = $as_T(thiz.substring(0, index));
        var thiz$1 = $as_T(elem$1);
        elem$1 = ((jsx$2 + ".") + $as_T(thiz$1.substring(index)))
      } else {
        var end = ((-index) | 0);
        var isEmpty$4 = (end <= 0);
        var scala$collection$immutable$Range$$lastElement$4 = (((-1) + end) | 0);
        if ((!isEmpty$4)) {
          var i$1 = 0;
          while (true) {
            var v1 = i$1;
            elem$1 = (("" + new $c_jl_Character().init___C(48)) + $as_T(elem$1));
            if ((i$1 === scala$collection$immutable$Range$$lastElement$4)) {
              break
            };
            i$1 = ((1 + i$1) | 0)
          }
        };
        elem$1 = ("0." + $as_T(elem$1))
      }
    } else if ((scale !== 0)) {
      var result1 = ("" + exponent);
      if ((exponent > 0)) {
        result1 = (("" + new $c_jl_Character().init___C(43)) + result1)
      };
      result1 = (("" + new $c_jl_Character().init___C(69)) + result1);
      if ((((18 - currentChar) | 0) > 1)) {
        var x$1 = $as_T(elem$1);
        var c$1 = (65535 & $uI(x$1.charCodeAt(0)));
        var jsx$3 = new $c_jl_Character().init___C(c$1);
        var thiz$2 = $as_T(elem$1);
        elem$1 = (((jsx$3 + ".") + $as_T(thiz$2.substring(1))) + result1)
      } else {
        elem$1 = (("" + $as_T(elem$1)) + result1)
      }
    };
    return (negNumber ? (("" + new $c_jl_Character().init___C(45)) + $as_T(elem$1)) : $as_T(elem$1))
  }
});
$c_Ljava_math_Conversion$.prototype.dropLeadingZeros__p1__T__T = (function(s) {
  var zeroPrefixLength = 0;
  var len = $uI(s.length);
  while (true) {
    if ((zeroPrefixLength < len)) {
      var index = zeroPrefixLength;
      var jsx$1 = ((65535 & $uI(s.charCodeAt(index))) === 48)
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      zeroPrefixLength = ((1 + zeroPrefixLength) | 0)
    } else {
      break
    }
  };
  var beginIndex = zeroPrefixLength;
  return $as_T(s.substring(beginIndex))
});
$c_Ljava_math_Conversion$.prototype.toDecimalScaledString__Ljava_math_BigInteger__T = (function(bi) {
  var sign = bi.sign$2;
  var numberLength = bi.numberLength$2;
  var digits = bi.digits$2;
  if ((sign === 0)) {
    return "0"
  } else if ((numberLength === 1)) {
    var i = digits.get(0);
    var x = $uD((i >>> 0));
    var jsx$1 = x.toString(10);
    var absStr = $as_T(jsx$1);
    return ((sign < 0) ? ("-" + absStr) : absStr)
  } else {
    var result = "";
    var temp = $newArrayObject($d_I.getArrayOf(), [numberLength]);
    var tempLen = numberLength;
    $systemArraycopy(digits, 0, temp, 0, tempLen);
    do {
      var rem = 0;
      var i$1 = (((-1) + tempLen) | 0);
      while ((i$1 >= 0)) {
        var value = rem;
        var value$1 = temp.get(i$1);
        var this$8 = $m_sjsr_RuntimeLong$();
        var lo = this$8.divideUnsignedImpl__I__I__I__I__I(value$1, value, 1000000000, 0);
        temp.set(i$1, lo);
        var hi$3 = (lo >> 31);
        var b0 = (65535 & lo);
        var b1 = ((lo >>> 16) | 0);
        var a0b0 = $imul(51712, b0);
        var a1b0 = $imul(15258, b0);
        var a0b1 = $imul(51712, b1);
        var lo$1 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
        var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
        var hi$4 = (((((($imul(1000000000, hi$3) + $imul(15258, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
        var lo$2 = ((value$1 - lo$1) | 0);
        rem = lo$2;
        i$1 = (((-1) + i$1) | 0)
      };
      var this$10 = rem;
      var remStr = ("" + this$10);
      var beginIndex = $uI(remStr.length);
      var padding = $as_T("000000000".substring(beginIndex));
      result = ((padding + remStr) + result);
      while (((tempLen !== 0) && (temp.get((((-1) + tempLen) | 0)) === 0))) {
        tempLen = (((-1) + tempLen) | 0)
      }
    } while ((tempLen !== 0));
    result = this.dropLeadingZeros__p1__T__T(result);
    return ((sign < 0) ? (("" + new $c_jl_Character().init___C(45)) + result) : result)
  }
});
var $d_Ljava_math_Conversion$ = new $TypeData().initClass({
  Ljava_math_Conversion$: 0
}, false, "java.math.Conversion$", {
  Ljava_math_Conversion$: 1,
  O: 1
});
$c_Ljava_math_Conversion$.prototype.$classData = $d_Ljava_math_Conversion$;
var $n_Ljava_math_Conversion$ = (void 0);
function $m_Ljava_math_Conversion$() {
  if ((!$n_Ljava_math_Conversion$)) {
    $n_Ljava_math_Conversion$ = new $c_Ljava_math_Conversion$().init___()
  };
  return $n_Ljava_math_Conversion$
}
/** @constructor */
function $c_Ljava_math_Division$() {
  $c_O.call(this)
}
$c_Ljava_math_Division$.prototype = new $h_O();
$c_Ljava_math_Division$.prototype.constructor = $c_Ljava_math_Division$;
/** @constructor */
function $h_Ljava_math_Division$() {
  /*<skip>*/
}
$h_Ljava_math_Division$.prototype = $c_Ljava_math_Division$.prototype;
$c_Ljava_math_Division$.prototype.init___ = (function() {
  return this
});
$c_Ljava_math_Division$.prototype.divide__AI__I__AI__I__AI__I__AI = (function(quot, quotLength, a, aLength, b, bLength) {
  var normA = $newArrayObject($d_I.getArrayOf(), [((1 + aLength) | 0)]);
  var normB = $newArrayObject($d_I.getArrayOf(), [((1 + bLength) | 0)]);
  var divisorShift = $clz32(b.get((((-1) + bLength) | 0)));
  if ((divisorShift !== 0)) {
    $m_Ljava_math_BitLevel$().shiftLeft__AI__AI__I__I__V(normB, b, 0, divisorShift);
    $m_Ljava_math_BitLevel$().shiftLeft__AI__AI__I__I__V(normA, a, 0, divisorShift)
  } else {
    $systemArraycopy(a, 0, normA, 0, aLength);
    $systemArraycopy(b, 0, normB, 0, bLength)
  };
  var firstDivisorDigit = normB.get((((-1) + bLength) | 0));
  var i = (((-1) + quotLength) | 0);
  var elem$1 = 0;
  elem$1 = aLength;
  while ((i >= 0)) {
    var elem$1$1 = 0;
    elem$1$1 = 0;
    if ((normA.get(elem$1) === firstDivisorDigit)) {
      elem$1$1 = (-1)
    } else {
      var value = normA.get(elem$1);
      var value$1 = normA.get((((-1) + elem$1) | 0));
      var this$5 = $m_sjsr_RuntimeLong$();
      var lo = this$5.divideUnsignedImpl__I__I__I__I__I(value$1, value, firstDivisorDigit, 0);
      var hi$3 = this$5.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      elem$1$1 = lo;
      var a0 = (65535 & lo);
      var a1 = ((lo >>> 16) | 0);
      var b0 = (65535 & firstDivisorDigit);
      var b1 = ((firstDivisorDigit >>> 16) | 0);
      var a0b0 = $imul(a0, b0);
      var a1b0 = $imul(a1, b0);
      var a0b1 = $imul(a0, b1);
      var lo$1 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$4 = (((((($imul(hi$3, firstDivisorDigit) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
      var lo$2 = ((value$1 - lo$1) | 0);
      var elem$1$2 = 0;
      elem$1$2 = lo$2;
      if ((elem$1$1 !== 0)) {
        elem$1$1 = ((1 + elem$1$1) | 0);
        _loop: while (true) {
          elem$1$1 = (((-1) + elem$1$1) | 0);
          var value$2 = elem$1$1;
          var value$3 = normB.get((((-2) + bLength) | 0));
          var a0$1 = (65535 & value$2);
          var a1$1 = ((value$2 >>> 16) | 0);
          var b0$1 = (65535 & value$3);
          var b1$1 = ((value$3 >>> 16) | 0);
          var a0b0$1 = $imul(a0$1, b0$1);
          var a1b0$1 = $imul(a1$1, b0$1);
          var a0b1$1 = $imul(a0$1, b1$1);
          var lo$3 = ((a0b0$1 + (((a1b0$1 + a0b1$1) | 0) << 16)) | 0);
          var c1part$1 = ((((a0b0$1 >>> 16) | 0) + a0b1$1) | 0);
          var hi$8 = (((($imul(a1$1, b1$1) + ((c1part$1 >>> 16) | 0)) | 0) + (((((65535 & c1part$1) + a1b0$1) | 0) >>> 16) | 0)) | 0);
          var value$4 = elem$1$2;
          var value$5 = normA.get((((-2) + elem$1) | 0));
          var value$6 = elem$1$2;
          var lo$4 = ((value$6 + firstDivisorDigit) | 0);
          var hi$13 = ((((-2147483648) ^ lo$4) < ((-2147483648) ^ value$6)) ? 1 : 0);
          if ((hi$13 === 0)) {
            elem$1$2 = lo$4;
            var hi$14 = ((-2147483648) ^ hi$8);
            var hi$15 = ((-2147483648) ^ value$4);
            if (((hi$14 === hi$15) ? (((-2147483648) ^ lo$3) > ((-2147483648) ^ value$5)) : (hi$14 > hi$15))) {
              continue _loop
            }
          };
          break
        }
      }
    };
    if ((elem$1$1 !== 0)) {
      var borrow = $m_Ljava_math_Division$().multiplyAndSubtract__AI__I__AI__I__I__I(normA, ((elem$1 - bLength) | 0), normB, bLength, elem$1$1);
      if ((borrow !== 0)) {
        elem$1$1 = (((-1) + elem$1$1) | 0);
        var elem$1$3_$_lo$2 = 0;
        var elem$1$3_$_hi$2 = 0;
        var jsx$1_$_lo$2 = 0;
        var jsx$1_$_hi$2 = 0;
        elem$1$3_$_lo$2 = jsx$1_$_lo$2;
        elem$1$3_$_hi$2 = jsx$1_$_hi$2;
        var isEmpty$4 = (bLength <= 0);
        var scala$collection$immutable$Range$$lastElement$4 = (((-1) + bLength) | 0);
        if ((!isEmpty$4)) {
          var i$1 = 0;
          while (true) {
            var v1 = i$1;
            var this$15_$_lo$2 = elem$1$3_$_lo$2;
            var this$15_$_hi$2 = elem$1$3_$_hi$2;
            var value$7 = normA.get(((((elem$1 - bLength) | 0) + v1) | 0));
            var value$8 = normB.get(v1);
            var lo$5 = ((value$7 + value$8) | 0);
            var hi$20 = ((((-2147483648) ^ lo$5) < ((-2147483648) ^ value$7)) ? 1 : 0);
            var alo = this$15_$_lo$2;
            var ahi = this$15_$_hi$2;
            var lo$6 = ((alo + lo$5) | 0);
            var hi$21 = ((((-2147483648) ^ lo$6) < ((-2147483648) ^ alo)) ? ((1 + ((ahi + hi$20) | 0)) | 0) : ((ahi + hi$20) | 0));
            var jsx$2_$_lo$2 = lo$6;
            var jsx$2_$_hi$2 = hi$21;
            elem$1$3_$_lo$2 = jsx$2_$_lo$2;
            elem$1$3_$_hi$2 = jsx$2_$_hi$2;
            var jsx$3 = elem$1;
            var this$17_$_lo$2 = elem$1$3_$_lo$2;
            var this$17_$_hi$2 = elem$1$3_$_hi$2;
            normA.set(((((jsx$3 - bLength) | 0) + v1) | 0), this$17_$_lo$2);
            var this$18_$_lo$2 = elem$1$3_$_lo$2;
            var this$18_$_hi$2 = elem$1$3_$_hi$2;
            var lo$7 = this$18_$_hi$2;
            var jsx$4_$_lo$2 = lo$7;
            var jsx$4_$_hi$2 = 0;
            elem$1$3_$_lo$2 = jsx$4_$_lo$2;
            elem$1$3_$_hi$2 = jsx$4_$_hi$2;
            if ((i$1 === scala$collection$immutable$Range$$lastElement$4)) {
              break
            };
            i$1 = ((1 + i$1) | 0)
          }
        }
      }
    };
    if ((quot !== null)) {
      quot.set(i, elem$1$1)
    };
    elem$1 = (((-1) + elem$1) | 0);
    i = (((-1) + i) | 0)
  };
  return ((divisorShift !== 0) ? ($m_Ljava_math_BitLevel$().shiftRight__AI__I__AI__I__I__Z(normB, bLength, normA, 0, divisorShift), normB) : ($systemArraycopy(normA, 0, normB, 0, bLength), normA))
});
$c_Ljava_math_Division$.prototype.remainderArrayByInt__AI__I__I__I = (function(src, srcLength, divisor) {
  var result = 0;
  var i = (((-1) + srcLength) | 0);
  while ((i >= 0)) {
    var value = result;
    var value$1 = src.get(i);
    var this$2 = $m_sjsr_RuntimeLong$();
    var lo = this$2.remainderUnsignedImpl__I__I__I__I__I(value$1, value, divisor, 0);
    result = lo;
    i = (((-1) + i) | 0)
  };
  return result
});
$c_Ljava_math_Division$.prototype.gcdBinary__I__I__I = (function(val1, val2) {
  var op1 = val1;
  var op2 = val2;
  var i = op1;
  var lsb1 = ((i === 0) ? 32 : ((31 - $clz32((i & ((-i) | 0)))) | 0));
  var i$1 = op2;
  var lsb2 = ((i$1 === 0) ? 32 : ((31 - $clz32((i$1 & ((-i$1) | 0)))) | 0));
  var pow2Count = ((lsb1 < lsb2) ? lsb1 : lsb2);
  if ((lsb1 !== 0)) {
    op1 = ((op1 >>> lsb1) | 0)
  };
  if ((lsb2 !== 0)) {
    op2 = ((op2 >>> lsb2) | 0)
  };
  do {
    if ((op1 >= op2)) {
      op1 = ((op1 - op2) | 0);
      var jsx$1 = op1;
      var i$2 = op1;
      op1 = ((jsx$1 >>> ((i$2 === 0) ? 32 : ((31 - $clz32((i$2 & ((-i$2) | 0)))) | 0))) | 0)
    } else {
      op2 = ((op2 - op1) | 0);
      var jsx$2 = op2;
      var i$3 = op2;
      op2 = ((jsx$2 >>> ((i$3 === 0) ? 32 : ((31 - $clz32((i$3 & ((-i$3) | 0)))) | 0))) | 0)
    }
  } while ((op1 !== 0));
  return (op2 << pow2Count)
});
$c_Ljava_math_Division$.prototype.divideAndRemainderByInteger__Ljava_math_BigInteger__I__I__Ljava_math_BigInteger$QuotAndRem = (function(bi, divisor, divisorSign) {
  var valDigits = bi.digits$2;
  var valLen = bi.numberLength$2;
  var valSign = bi.sign$2;
  if ((valLen === 1)) {
    var valDigit = valDigits.get(0);
    var n = ($uD((valDigit >>> 0)) / $uD((divisor >>> 0)));
    var value = $uI((n | 0));
    var quo_$_lo$2 = value;
    var quo_$_hi$2 = 0;
    var n$1 = ($uD((valDigit >>> 0)) % $uD((divisor >>> 0)));
    var value$1 = $uI((n$1 | 0));
    var rem_$_lo$2 = value$1;
    var rem_$_hi$2 = 0;
    if ((valSign !== divisorSign)) {
      var this$13_$_lo$2 = quo_$_lo$2;
      var this$13_$_hi$2 = quo_$_hi$2;
      var lo = this$13_$_lo$2;
      var hi$2 = this$13_$_hi$2;
      var lo$1 = ((-lo) | 0);
      var hi$3 = ((lo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      var jsx$1_$_lo$2 = lo$1;
      var jsx$1_$_hi$2 = hi$3;
      quo_$_lo$2 = jsx$1_$_lo$2;
      quo_$_hi$2 = jsx$1_$_hi$2
    };
    if ((valSign < 0)) {
      var this$16_$_lo$2 = rem_$_lo$2;
      var this$16_$_hi$2 = rem_$_hi$2;
      var lo$2 = this$16_$_lo$2;
      var hi$4 = this$16_$_hi$2;
      var lo$3 = ((-lo$2) | 0);
      var hi$5 = ((lo$2 !== 0) ? (~hi$4) : ((-hi$4) | 0));
      var jsx$2_$_lo$2 = lo$3;
      var jsx$2_$_hi$2 = hi$5;
      rem_$_lo$2 = jsx$2_$_lo$2;
      rem_$_hi$2 = jsx$2_$_hi$2
    };
    return new $c_Ljava_math_BigInteger$QuotAndRem().init___Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(quo_$_lo$2, quo_$_hi$2)), $m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(rem_$_lo$2, rem_$_hi$2)))
  } else {
    var quotientSign = ((valSign === divisorSign) ? 1 : (-1));
    var quotientDigits = $newArrayObject($d_I.getArrayOf(), [valLen]);
    var div = this.divideArrayByInt__AI__AI__I__I__I(quotientDigits, valDigits, valLen, divisor);
    var remainderDigits = $m_s_Array$().apply__I__sc_Seq__AI(div, new $c_sjs_js_WrappedArray().init___sjs_js_Array([]));
    var result0 = new $c_Ljava_math_BigInteger().init___I__I__AI(quotientSign, valLen, quotientDigits);
    var result1 = new $c_Ljava_math_BigInteger().init___I__I__AI(valSign, 1, remainderDigits);
    result0.cutOffLeadingZeroes__V();
    result1.cutOffLeadingZeroes__V();
    return new $c_Ljava_math_BigInteger$QuotAndRem().init___Ljava_math_BigInteger__Ljava_math_BigInteger(result0, result1)
  }
});
$c_Ljava_math_Division$.prototype.multiplyAndSubtract__AI__I__AI__I__I__I = (function(a, start, b, bLen, c) {
  var elem$1 = 0;
  elem$1 = 0;
  var elem$1$1 = 0;
  elem$1$1 = 0;
  var isEmpty$4 = (bLen <= 0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + bLen) | 0);
  if ((!isEmpty$4)) {
    var i = 0;
    while (true) {
      var v1 = i;
      $m_Ljava_math_Multiplication$();
      var a$1 = b.get(v1);
      var c$1 = elem$1;
      var a0 = (65535 & a$1);
      var a1 = ((a$1 >>> 16) | 0);
      var b0 = (65535 & c);
      var b1 = ((c >>> 16) | 0);
      var a0b0 = $imul(a0, b0);
      var a1b0 = $imul(a1, b0);
      var a0b1 = $imul(a0, b1);
      var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$4 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
      var lo$1 = ((lo + c$1) | 0);
      var hi$6 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + hi$4) | 0) : hi$4);
      var value = a.get(((start + v1) | 0));
      var lo$2 = ((value - lo$1) | 0);
      var hi$8 = ((((-2147483648) ^ lo$2) > ((-2147483648) ^ value)) ? (-1) : 0);
      var value$1 = elem$1$1;
      var hi$9 = (value$1 >> 31);
      var lo$3 = ((lo$2 + value$1) | 0);
      var hi$10 = ((((-2147483648) ^ lo$3) < ((-2147483648) ^ lo$2)) ? ((1 + ((hi$8 + hi$9) | 0)) | 0) : ((hi$8 + hi$9) | 0));
      a.set(((start + v1) | 0), lo$3);
      elem$1$1 = hi$10;
      elem$1 = hi$6;
      if ((i === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i = ((1 + i) | 0)
    }
  };
  var value$2 = a.get(((start + bLen) | 0));
  var value$3 = elem$1;
  var lo$4 = ((value$2 - value$3) | 0);
  var hi$15 = ((((-2147483648) ^ lo$4) > ((-2147483648) ^ value$2)) ? (-1) : 0);
  var value$4 = elem$1$1;
  var hi$16 = (value$4 >> 31);
  var lo$5 = ((lo$4 + value$4) | 0);
  var hi$17 = ((((-2147483648) ^ lo$5) < ((-2147483648) ^ lo$4)) ? ((1 + ((hi$15 + hi$16) | 0)) | 0) : ((hi$15 + hi$16) | 0));
  a.set(((start + bLen) | 0), lo$5);
  return hi$17
});
$c_Ljava_math_Division$.prototype.divideArrayByInt__AI__AI__I__I__I = (function(dest, src, srcLength, divisor) {
  var rem = 0;
  var i = (((-1) + srcLength) | 0);
  while ((i >= 0)) {
    var value = rem;
    var value$1 = src.get(i);
    var this$2 = $m_sjsr_RuntimeLong$();
    var lo = this$2.divideUnsignedImpl__I__I__I__I__I(value$1, value, divisor, 0);
    var hi$3 = this$2.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
    var a0 = (65535 & lo);
    var a1 = ((lo >>> 16) | 0);
    var b0 = (65535 & divisor);
    var b1 = ((divisor >>> 16) | 0);
    var a0b0 = $imul(a0, b0);
    var a1b0 = $imul(a1, b0);
    var a0b1 = $imul(a0, b1);
    var lo$1 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
    var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
    var hi$4 = (((((($imul(hi$3, divisor) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
    var lo$2 = ((value$1 - lo$1) | 0);
    rem = lo$2;
    dest.set(i, lo);
    i = (((-1) + i) | 0)
  };
  return rem
});
$c_Ljava_math_Division$.prototype.gcdBinary__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(val1, val2) {
  var elem$1 = null;
  elem$1 = val1;
  var elem$1$1 = null;
  elem$1$1 = val2;
  var lsb1 = $as_Ljava_math_BigInteger(elem$1).getLowestSetBit__I();
  var lsb2 = $as_Ljava_math_BigInteger(elem$1$1).getLowestSetBit__I();
  var pow2Count = ((lsb1 < lsb2) ? lsb1 : lsb2);
  $m_Ljava_math_BitLevel$().inplaceShiftRight__Ljava_math_BigInteger__I__V($as_Ljava_math_BigInteger(elem$1), lsb1);
  $m_Ljava_math_BitLevel$().inplaceShiftRight__Ljava_math_BigInteger__I__V($as_Ljava_math_BigInteger(elem$1$1), lsb2);
  if (($as_Ljava_math_BigInteger(elem$1).compareTo__Ljava_math_BigInteger__I($as_Ljava_math_BigInteger(elem$1$1)) === 1)) {
    var swap = $as_Ljava_math_BigInteger(elem$1);
    elem$1 = $as_Ljava_math_BigInteger(elem$1$1);
    elem$1$1 = swap
  };
  _loop: while (true) {
    if ((($as_Ljava_math_BigInteger(elem$1$1).numberLength$2 === 1) && ($as_Ljava_math_BigInteger(elem$1$1).digits$2.get(0) > 0))) {
      var jsx$1 = $m_Ljava_math_BigInteger$();
      var value = $m_Ljava_math_Division$().gcdBinary__I__I__I($as_Ljava_math_BigInteger(elem$1).intValue__I(), $as_Ljava_math_BigInteger(elem$1$1).intValue__I());
      var hi = (value >> 31);
      elem$1$1 = jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value, hi))
    } else {
      if (($as_Ljava_math_BigInteger(elem$1$1).numberLength$2 > (1.2 * $as_Ljava_math_BigInteger(elem$1).numberLength$2))) {
        elem$1$1 = $as_Ljava_math_BigInteger(elem$1$1).remainder__Ljava_math_BigInteger__Ljava_math_BigInteger($as_Ljava_math_BigInteger(elem$1));
        var this$4 = $as_Ljava_math_BigInteger(elem$1$1);
        if ((this$4.sign$2 !== 0)) {
          $m_Ljava_math_BitLevel$().inplaceShiftRight__Ljava_math_BigInteger__I__V($as_Ljava_math_BigInteger(elem$1$1), $as_Ljava_math_BigInteger(elem$1$1).getLowestSetBit__I())
        }
      } else {
        do {
          $m_Ljava_math_Elementary$().inplaceSubtract__Ljava_math_BigInteger__Ljava_math_BigInteger__V($as_Ljava_math_BigInteger(elem$1$1), $as_Ljava_math_BigInteger(elem$1));
          $m_Ljava_math_BitLevel$().inplaceShiftRight__Ljava_math_BigInteger__I__V($as_Ljava_math_BigInteger(elem$1$1), $as_Ljava_math_BigInteger(elem$1$1).getLowestSetBit__I())
        } while (($as_Ljava_math_BigInteger(elem$1$1).compareTo__Ljava_math_BigInteger__I($as_Ljava_math_BigInteger(elem$1)) >= 0))
      };
      var swap$1 = $as_Ljava_math_BigInteger(elem$1$1);
      elem$1$1 = $as_Ljava_math_BigInteger(elem$1);
      elem$1 = swap$1;
      if (($as_Ljava_math_BigInteger(elem$1).sign$2 !== 0)) {
        continue _loop
      }
    };
    break
  };
  return $as_Ljava_math_BigInteger(elem$1$1).shiftLeft__I__Ljava_math_BigInteger(pow2Count)
});
var $d_Ljava_math_Division$ = new $TypeData().initClass({
  Ljava_math_Division$: 0
}, false, "java.math.Division$", {
  Ljava_math_Division$: 1,
  O: 1
});
$c_Ljava_math_Division$.prototype.$classData = $d_Ljava_math_Division$;
var $n_Ljava_math_Division$ = (void 0);
function $m_Ljava_math_Division$() {
  if ((!$n_Ljava_math_Division$)) {
    $n_Ljava_math_Division$ = new $c_Ljava_math_Division$().init___()
  };
  return $n_Ljava_math_Division$
}
/** @constructor */
function $c_Ljava_math_Elementary$() {
  $c_O.call(this)
}
$c_Ljava_math_Elementary$.prototype = new $h_O();
$c_Ljava_math_Elementary$.prototype.constructor = $c_Ljava_math_Elementary$;
/** @constructor */
function $h_Ljava_math_Elementary$() {
  /*<skip>*/
}
$h_Ljava_math_Elementary$.prototype = $c_Ljava_math_Elementary$.prototype;
$c_Ljava_math_Elementary$.prototype.init___ = (function() {
  return this
});
$c_Ljava_math_Elementary$.prototype.subtract__p1__AI__I__AI__I__AI = (function(a, aSize, b, bSize) {
  var res = $newArrayObject($d_I.getArrayOf(), [aSize]);
  this.subtract__p1__AI__AI__I__AI__I__V(res, a, aSize, b, bSize);
  return res
});
$c_Ljava_math_Elementary$.prototype.compareArrays__AI__AI__I__I = (function(a, b, size) {
  var i = (((-1) + size) | 0);
  while (((i >= 0) && (a.get(i) === b.get(i)))) {
    i = (((-1) + i) | 0)
  };
  if ((i < 0)) {
    return 0
  } else {
    var value = a.get(i);
    var value$1 = b.get(i);
    if ((((-2147483648) ^ value) < ((-2147483648) ^ value$1))) {
      return (-1)
    } else {
      return 1
    }
  }
});
$c_Ljava_math_Elementary$.prototype.inplaceSubtract__Ljava_math_BigInteger__Ljava_math_BigInteger__V = (function(op1, op2) {
  this.subtract__p1__AI__AI__I__AI__I__V(op1.digits$2, op1.digits$2, op1.numberLength$2, op2.digits$2, op2.numberLength$2);
  op1.cutOffLeadingZeroes__V();
  op1.java$math$BigInteger$$firstNonzeroDigit$2 = (-2)
});
$c_Ljava_math_Elementary$.prototype.inplaceAdd__AI__I__I__I = (function(a, aSize, addend) {
  var carry = addend;
  var i = 0;
  while (((carry !== 0) && (i < aSize))) {
    var value = carry;
    var value$1 = a.get(i);
    var lo = ((value + value$1) | 0);
    var hi$2 = ((((-2147483648) ^ lo) < ((-2147483648) ^ value)) ? 1 : 0);
    a.set(i, lo);
    carry = hi$2;
    i = ((1 + i) | 0)
  };
  return carry
});
$c_Ljava_math_Elementary$.prototype.add__p1__AI__I__AI__I__AI = (function(a, aSize, b, bSize) {
  var res = $newArrayObject($d_I.getArrayOf(), [((1 + aSize) | 0)]);
  this.add__p1__AI__AI__I__AI__I__V(res, a, aSize, b, bSize);
  return res
});
$c_Ljava_math_Elementary$.prototype.add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(op1, op2) {
  var op1Sign = op1.sign$2;
  var op2Sign = op2.sign$2;
  var op1Len = op1.numberLength$2;
  var op2Len = op2.numberLength$2;
  if ((op1Sign === 0)) {
    return op2
  } else if ((op2Sign === 0)) {
    return op1
  } else if ((((op1Len + op2Len) | 0) === 2)) {
    var value = op1.digits$2.get(0);
    var value$1 = op2.digits$2.get(0);
    if ((op1Sign === op2Sign)) {
      var lo = ((value + value$1) | 0);
      var hi$2 = ((((-2147483648) ^ lo) < ((-2147483648) ^ value)) ? 1 : 0);
      return ((hi$2 === 0) ? new $c_Ljava_math_BigInteger().init___I__I(op1Sign, lo) : new $c_Ljava_math_BigInteger().init___I__I__AI(op1Sign, 2, $m_s_Array$().apply__I__sc_Seq__AI(lo, new $c_sjs_js_WrappedArray().init___sjs_js_Array([hi$2]))))
    } else {
      var jsx$1 = $m_Ljava_math_BigInteger$();
      if ((op1Sign < 0)) {
        var lo$1 = ((value$1 - value) | 0);
        var hi$3 = ((((-2147483648) ^ lo$1) > ((-2147483648) ^ value$1)) ? (-1) : 0);
        var x_$_lo$2 = lo$1;
        var x_$_hi$2 = hi$3
      } else {
        var lo$2 = ((value - value$1) | 0);
        var hi$4 = ((((-2147483648) ^ lo$2) > ((-2147483648) ^ value)) ? (-1) : 0);
        var x_$_lo$2 = lo$2;
        var x_$_hi$2 = hi$4
      };
      return jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(x_$_lo$2, x_$_hi$2))
    }
  } else {
    if ((op1Sign === op2Sign)) {
      var res$2 = ((op1Len >= op2Len) ? this.add__p1__AI__I__AI__I__AI(op1.digits$2, op1Len, op2.digits$2, op2Len) : this.add__p1__AI__I__AI__I__AI(op2.digits$2, op2Len, op1.digits$2, op1Len));
      var x1_$_$$und1$f = op1Sign;
      var x1_$_$$und2$f = res$2
    } else {
      var cmp = ((op1Len !== op2Len) ? ((op1Len > op2Len) ? 1 : (-1)) : this.compareArrays__AI__AI__I__I(op1.digits$2, op2.digits$2, op1Len));
      if ((cmp === 0)) {
        return $m_Ljava_math_BigInteger$().ZERO$1
      };
      if ((cmp === 1)) {
        var _2 = this.subtract__p1__AI__I__AI__I__AI(op1.digits$2, op1Len, op2.digits$2, op2Len);
        var x1_$_$$und1$f = op1Sign;
        var x1_$_$$und2$f = _2
      } else {
        var _2$1 = this.subtract__p1__AI__I__AI__I__AI(op2.digits$2, op2Len, op1.digits$2, op1Len);
        var x1_$_$$und1$f = op2Sign;
        var x1_$_$$und2$f = _2$1
      }
    };
    var resSign = $uI(x1_$_$$und1$f);
    var resDigits = $asArrayOf_I(x1_$_$$und2$f, 1);
    var res$3 = new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, resDigits.u.length, resDigits);
    res$3.cutOffLeadingZeroes__V();
    return res$3
  }
});
$c_Ljava_math_Elementary$.prototype.subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(op1, op2) {
  var op1Sign = op1.sign$2;
  var op2Sign = op2.sign$2;
  var op1Len = op1.numberLength$2;
  var op2Len = op2.numberLength$2;
  if ((op2Sign === 0)) {
    return op1
  } else if ((op1Sign === 0)) {
    return op2.negate__Ljava_math_BigInteger()
  } else if ((((op1Len + op2Len) | 0) === 2)) {
    var value = op1.digits$2.get(0);
    var a_$_lo$2 = value;
    var a_$_hi$2 = 0;
    var value$1 = op2.digits$2.get(0);
    var b_$_lo$2 = value$1;
    var b_$_hi$2 = 0;
    if ((op1Sign < 0)) {
      var this$1_$_lo$2 = a_$_lo$2;
      var this$1_$_hi$2 = a_$_hi$2;
      var lo = this$1_$_lo$2;
      var hi$2 = this$1_$_hi$2;
      var lo$1 = ((-lo) | 0);
      var hi$3 = ((lo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      var jsx$1_$_lo$2 = lo$1;
      var jsx$1_$_hi$2 = hi$3;
      a_$_lo$2 = jsx$1_$_lo$2;
      a_$_hi$2 = jsx$1_$_hi$2
    };
    if ((op2Sign < 0)) {
      var this$4_$_lo$2 = b_$_lo$2;
      var this$4_$_hi$2 = b_$_hi$2;
      var lo$2 = this$4_$_lo$2;
      var hi$4 = this$4_$_hi$2;
      var lo$3 = ((-lo$2) | 0);
      var hi$5 = ((lo$2 !== 0) ? (~hi$4) : ((-hi$4) | 0));
      var jsx$2_$_lo$2 = lo$3;
      var jsx$2_$_hi$2 = hi$5;
      b_$_lo$2 = jsx$2_$_lo$2;
      b_$_hi$2 = jsx$2_$_hi$2
    };
    var jsx$3 = $m_Ljava_math_BigInteger$();
    var this$7_$_lo$2 = a_$_lo$2;
    var this$7_$_hi$2 = a_$_hi$2;
    var b$1_$_lo$2 = b_$_lo$2;
    var b$1_$_hi$2 = b_$_hi$2;
    var alo = this$7_$_lo$2;
    var ahi = this$7_$_hi$2;
    var bhi = b$1_$_hi$2;
    var lo$4 = ((alo - b$1_$_lo$2) | 0);
    var hi$6 = ((((-2147483648) ^ lo$4) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi - bhi) | 0)) | 0) : ((ahi - bhi) | 0));
    return jsx$3.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo$4, hi$6))
  } else {
    var cmp = ((op1Len !== op2Len) ? ((op1Len > op2Len) ? 1 : (-1)) : $m_Ljava_math_Elementary$().compareArrays__AI__AI__I__I(op1.digits$2, op2.digits$2, op1Len));
    if (((op1Sign === op2Sign) && (cmp === 0))) {
      return $m_Ljava_math_BigInteger$().ZERO$1
    };
    if ((cmp === (-1))) {
      var res = ((op1Sign === op2Sign) ? this.subtract__p1__AI__I__AI__I__AI(op2.digits$2, op2Len, op1.digits$2, op1Len) : this.add__p1__AI__I__AI__I__AI(op2.digits$2, op2Len, op1.digits$2, op1Len));
      var _1 = ((-op2Sign) | 0);
      var x1_$_$$und1$f = _1;
      var x1_$_$$und2$f = res
    } else if ((op1Sign === op2Sign)) {
      var _2 = this.subtract__p1__AI__I__AI__I__AI(op1.digits$2, op1Len, op2.digits$2, op2Len);
      var x1_$_$$und1$f = op1Sign;
      var x1_$_$$und2$f = _2
    } else {
      var _2$1 = this.add__p1__AI__I__AI__I__AI(op1.digits$2, op1Len, op2.digits$2, op2Len);
      var x1_$_$$und1$f = op1Sign;
      var x1_$_$$und2$f = _2$1
    };
    var resSign = $uI(x1_$_$$und1$f);
    var resDigits = $asArrayOf_I(x1_$_$$und2$f, 1);
    var res$2 = new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, resDigits.u.length, resDigits);
    res$2.cutOffLeadingZeroes__V();
    return res$2
  }
});
$c_Ljava_math_Elementary$.prototype.subtract__p1__AI__AI__I__AI__I__V = (function(res, a, aSize, b, bSize) {
  var i = 0;
  var borrow = 0;
  while ((i < bSize)) {
    var value = a.get(i);
    var value$1 = b.get(i);
    var lo = ((value - value$1) | 0);
    var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (-1) : 0);
    var value$2 = borrow;
    var hi$3 = (value$2 >> 31);
    var lo$1 = ((lo + value$2) | 0);
    var hi$4 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + ((hi$2 + hi$3) | 0)) | 0) : ((hi$2 + hi$3) | 0));
    res.set(i, lo$1);
    borrow = hi$4;
    i = ((1 + i) | 0)
  };
  while ((i < aSize)) {
    var value$3 = a.get(i);
    var value$4 = borrow;
    var hi$7 = (value$4 >> 31);
    var lo$2 = ((value$3 + value$4) | 0);
    var hi$8 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ value$3)) ? ((1 + hi$7) | 0) : hi$7);
    res.set(i, lo$2);
    borrow = hi$8;
    i = ((1 + i) | 0)
  }
});
$c_Ljava_math_Elementary$.prototype.add__p1__AI__AI__I__AI__I__V = (function(res, a, aSize, b, bSize) {
  var i = 1;
  var value = a.get(0);
  var value$1 = b.get(0);
  var lo = ((value + value$1) | 0);
  var hi$2 = ((((-2147483648) ^ lo) < ((-2147483648) ^ value)) ? 1 : 0);
  res.set(0, lo);
  var carry = hi$2;
  if ((aSize >= bSize)) {
    while ((i < bSize)) {
      var value$2 = a.get(i);
      var value$3 = b.get(i);
      var lo$1 = ((value$2 + value$3) | 0);
      var hi$6 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ value$2)) ? 1 : 0);
      var value$4 = carry;
      var lo$2 = ((lo$1 + value$4) | 0);
      var hi$8 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ lo$1)) ? ((1 + hi$6) | 0) : hi$6);
      res.set(i, lo$2);
      carry = hi$8;
      i = ((1 + i) | 0)
    };
    while ((i < aSize)) {
      var value$5 = a.get(i);
      var value$6 = carry;
      var lo$3 = ((value$5 + value$6) | 0);
      var hi$12 = ((((-2147483648) ^ lo$3) < ((-2147483648) ^ value$5)) ? 1 : 0);
      res.set(i, lo$3);
      carry = hi$12;
      i = ((1 + i) | 0)
    }
  } else {
    while ((i < aSize)) {
      var value$7 = a.get(i);
      var value$8 = b.get(i);
      var lo$4 = ((value$7 + value$8) | 0);
      var hi$16 = ((((-2147483648) ^ lo$4) < ((-2147483648) ^ value$7)) ? 1 : 0);
      var value$9 = carry;
      var lo$5 = ((lo$4 + value$9) | 0);
      var hi$18 = ((((-2147483648) ^ lo$5) < ((-2147483648) ^ lo$4)) ? ((1 + hi$16) | 0) : hi$16);
      res.set(i, lo$5);
      carry = hi$18;
      i = ((1 + i) | 0)
    };
    while ((i < bSize)) {
      var value$10 = b.get(i);
      var value$11 = carry;
      var lo$6 = ((value$10 + value$11) | 0);
      var hi$22 = ((((-2147483648) ^ lo$6) < ((-2147483648) ^ value$10)) ? 1 : 0);
      res.set(i, lo$6);
      carry = hi$22;
      i = ((1 + i) | 0)
    }
  };
  if ((carry !== 0)) {
    res.set(i, carry)
  }
});
var $d_Ljava_math_Elementary$ = new $TypeData().initClass({
  Ljava_math_Elementary$: 0
}, false, "java.math.Elementary$", {
  Ljava_math_Elementary$: 1,
  O: 1
});
$c_Ljava_math_Elementary$.prototype.$classData = $d_Ljava_math_Elementary$;
var $n_Ljava_math_Elementary$ = (void 0);
function $m_Ljava_math_Elementary$() {
  if ((!$n_Ljava_math_Elementary$)) {
    $n_Ljava_math_Elementary$ = new $c_Ljava_math_Elementary$().init___()
  };
  return $n_Ljava_math_Elementary$
}
/** @constructor */
function $c_Ljava_math_MathContext() {
  $c_O.call(this);
  this.precision$1 = 0;
  this.roundingMode$1 = null
}
$c_Ljava_math_MathContext.prototype = new $h_O();
$c_Ljava_math_MathContext.prototype.constructor = $c_Ljava_math_MathContext;
/** @constructor */
function $h_Ljava_math_MathContext() {
  /*<skip>*/
}
$h_Ljava_math_MathContext.prototype = $c_Ljava_math_MathContext.prototype;
$c_Ljava_math_MathContext.prototype.equals__O__Z = (function(x) {
  if ($is_Ljava_math_MathContext(x)) {
    var x2 = $as_Ljava_math_MathContext(x);
    if ((this.precision$1 === x2.precision$1)) {
      var x$2 = this.roundingMode$1;
      var x$3 = x2.roundingMode$1;
      return (x$2 === x$3)
    } else {
      return false
    }
  } else {
    return false
  }
});
$c_Ljava_math_MathContext.prototype.toString__T = (function() {
  return ((("precision=" + this.precision$1) + " roundingMode=") + this.roundingMode$1)
});
$c_Ljava_math_MathContext.prototype.init___I__Ljava_math_RoundingMode = (function(setPrecision, setRoundingMode) {
  this.precision$1 = setPrecision;
  this.roundingMode$1 = setRoundingMode;
  return this
});
$c_Ljava_math_MathContext.prototype.hashCode__I = (function() {
  return ((this.precision$1 << 3) | this.roundingMode$1.$$undordinal$1)
});
function $is_Ljava_math_MathContext(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_math_MathContext)))
}
function $as_Ljava_math_MathContext(obj) {
  return (($is_Ljava_math_MathContext(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.math.MathContext"))
}
function $isArrayOf_Ljava_math_MathContext(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_math_MathContext)))
}
function $asArrayOf_Ljava_math_MathContext(obj, depth) {
  return (($isArrayOf_Ljava_math_MathContext(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.math.MathContext;", depth))
}
var $d_Ljava_math_MathContext = new $TypeData().initClass({
  Ljava_math_MathContext: 0
}, false, "java.math.MathContext", {
  Ljava_math_MathContext: 1,
  O: 1
});
$c_Ljava_math_MathContext.prototype.$classData = $d_Ljava_math_MathContext;
/** @constructor */
function $c_Ljava_math_MathContext$() {
  $c_O.call(this);
  this.DECIMAL128$1 = null;
  this.DECIMAL32$1 = null;
  this.DECIMAL64$1 = null;
  this.UNLIMITED$1 = null
}
$c_Ljava_math_MathContext$.prototype = new $h_O();
$c_Ljava_math_MathContext$.prototype.constructor = $c_Ljava_math_MathContext$;
/** @constructor */
function $h_Ljava_math_MathContext$() {
  /*<skip>*/
}
$h_Ljava_math_MathContext$.prototype = $c_Ljava_math_MathContext$.prototype;
$c_Ljava_math_MathContext$.prototype.init___ = (function() {
  $n_Ljava_math_MathContext$ = this;
  $m_Ljava_math_MathContext$();
  var roundingMode = $m_Ljava_math_RoundingMode$().HALF$undEVEN$1;
  this.DECIMAL128$1 = new $c_Ljava_math_MathContext().init___I__Ljava_math_RoundingMode(34, roundingMode);
  $m_Ljava_math_MathContext$();
  var roundingMode$1 = $m_Ljava_math_RoundingMode$().HALF$undEVEN$1;
  this.DECIMAL32$1 = new $c_Ljava_math_MathContext().init___I__Ljava_math_RoundingMode(7, roundingMode$1);
  $m_Ljava_math_MathContext$();
  var roundingMode$2 = $m_Ljava_math_RoundingMode$().HALF$undEVEN$1;
  this.DECIMAL64$1 = new $c_Ljava_math_MathContext().init___I__Ljava_math_RoundingMode(16, roundingMode$2);
  $m_Ljava_math_MathContext$();
  var roundingMode$3 = $m_Ljava_math_RoundingMode$().HALF$undUP$1;
  this.UNLIMITED$1 = new $c_Ljava_math_MathContext().init___I__Ljava_math_RoundingMode(0, roundingMode$3);
  return this
});
var $d_Ljava_math_MathContext$ = new $TypeData().initClass({
  Ljava_math_MathContext$: 0
}, false, "java.math.MathContext$", {
  Ljava_math_MathContext$: 1,
  O: 1
});
$c_Ljava_math_MathContext$.prototype.$classData = $d_Ljava_math_MathContext$;
var $n_Ljava_math_MathContext$ = (void 0);
function $m_Ljava_math_MathContext$() {
  if ((!$n_Ljava_math_MathContext$)) {
    $n_Ljava_math_MathContext$ = new $c_Ljava_math_MathContext$().init___()
  };
  return $n_Ljava_math_MathContext$
}
/** @constructor */
function $c_Ljava_math_Multiplication$() {
  $c_O.call(this);
  this.TenPows$1 = null;
  this.FivePows$1 = null;
  this.BigTenPows$1 = null;
  this.BigFivePows$1 = null
}
$c_Ljava_math_Multiplication$.prototype = new $h_O();
$c_Ljava_math_Multiplication$.prototype.constructor = $c_Ljava_math_Multiplication$;
/** @constructor */
function $h_Ljava_math_Multiplication$() {
  /*<skip>*/
}
$h_Ljava_math_Multiplication$.prototype = $c_Ljava_math_Multiplication$.prototype;
$c_Ljava_math_Multiplication$.prototype.multiplyPAP__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(a, b) {
  var aLen = a.numberLength$2;
  var bLen = b.numberLength$2;
  var resLength = ((aLen + bLen) | 0);
  var resSign = ((a.sign$2 !== b.sign$2) ? (-1) : 1);
  if ((resLength === 2)) {
    var a$1 = a.digits$2.get(0);
    var b$1 = b.digits$2.get(0);
    var a0 = (65535 & a$1);
    var a1 = ((a$1 >>> 16) | 0);
    var b0 = (65535 & b$1);
    var b1 = ((b$1 >>> 16) | 0);
    var a0b0 = $imul(a0, b0);
    var a1b0 = $imul(a1, b0);
    var a0b1 = $imul(a0, b1);
    var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
    var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
    var hi$2 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
    return ((hi$2 === 0) ? new $c_Ljava_math_BigInteger().init___I__I(resSign, lo) : new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, 2, $m_s_Array$().apply__I__sc_Seq__AI(lo, new $c_sjs_js_WrappedArray().init___sjs_js_Array([hi$2]))))
  } else {
    var aDigits = a.digits$2;
    var bDigits = b.digits$2;
    var resDigits = $newArrayObject($d_I.getArrayOf(), [resLength]);
    this.multArraysPAP__AI__I__AI__I__AI__V(aDigits, aLen, bDigits, bLen, resDigits);
    var result = new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, resLength, resDigits);
    result.cutOffLeadingZeroes__V();
    return result
  }
});
$c_Ljava_math_Multiplication$.prototype.init___ = (function() {
  $n_Ljava_math_Multiplication$ = this;
  this.TenPows$1 = this.newArrayOfPows__p1__I__I__AI(10, 10);
  this.FivePows$1 = this.newArrayOfPows__p1__I__I__AI(14, 5);
  this.BigTenPows$1 = $newArrayObject($d_Ljava_math_BigInteger.getArrayOf(), [32]);
  this.BigFivePows$1 = $newArrayObject($d_Ljava_math_BigInteger.getArrayOf(), [32]);
  this.initialiseArrays__p1__V();
  return this
});
$c_Ljava_math_Multiplication$.prototype.square__AI__I__AI__AI = (function(a, aLen, res) {
  var elem$1 = 0;
  elem$1 = 0;
  var isEmpty$4 = (aLen <= 0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + aLen) | 0);
  if ((!isEmpty$4)) {
    var i = 0;
    while (true) {
      var v1 = i;
      elem$1 = 0;
      var x = ((1 + v1) | 0);
      var isEmpty$4$1 = (x >= aLen);
      var scala$collection$immutable$Range$$lastElement$4$1 = (((-1) + aLen) | 0);
      if ((!isEmpty$4$1)) {
        var i$1 = x;
        while (true) {
          var v1$1 = i$1;
          var a$1 = a.get(v1);
          var b = a.get(v1$1);
          var c = res.get(((v1 + v1$1) | 0));
          var d = elem$1;
          var a0 = (65535 & a$1);
          var a1 = ((a$1 >>> 16) | 0);
          var b0 = (65535 & b);
          var b1 = ((b >>> 16) | 0);
          var a0b0 = $imul(a0, b0);
          var a1b0 = $imul(a1, b0);
          var a0b1 = $imul(a0, b1);
          var lo$4 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
          var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
          var hi$13 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
          var lo$5 = ((lo$4 + c) | 0);
          var hi$15 = ((((-2147483648) ^ lo$5) < ((-2147483648) ^ lo$4)) ? ((1 + hi$13) | 0) : hi$13);
          var lo$6 = ((lo$5 + d) | 0);
          var hi$17 = ((((-2147483648) ^ lo$6) < ((-2147483648) ^ lo$5)) ? ((1 + hi$15) | 0) : hi$15);
          res.set(((v1 + v1$1) | 0), lo$6);
          elem$1 = hi$17;
          if ((i$1 === scala$collection$immutable$Range$$lastElement$4$1)) {
            break
          };
          i$1 = ((1 + i$1) | 0)
        }
      };
      res.set(((v1 + aLen) | 0), elem$1);
      if ((i === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i = ((1 + i) | 0)
    }
  };
  $m_Ljava_math_BitLevel$().shiftLeftOneBit__AI__AI__I__V(res, res, (aLen << 1));
  elem$1 = 0;
  var i$3 = 0;
  var index = 0;
  while ((i$3 < aLen)) {
    var a$2 = a.get(i$3);
    var b$1 = a.get(i$3);
    var c$1 = res.get(index);
    var d$1 = elem$1;
    var a0$1 = (65535 & a$2);
    var a1$1 = ((a$2 >>> 16) | 0);
    var b0$1 = (65535 & b$1);
    var b1$1 = ((b$1 >>> 16) | 0);
    var a0b0$1 = $imul(a0$1, b0$1);
    var a1b0$1 = $imul(a1$1, b0$1);
    var a0b1$1 = $imul(a0$1, b1$1);
    var lo$7 = ((a0b0$1 + (((a1b0$1 + a0b1$1) | 0) << 16)) | 0);
    var c1part$1 = ((((a0b0$1 >>> 16) | 0) + a0b1$1) | 0);
    var hi$20 = (((($imul(a1$1, b1$1) + ((c1part$1 >>> 16) | 0)) | 0) + (((((65535 & c1part$1) + a1b0$1) | 0) >>> 16) | 0)) | 0);
    var lo$8 = ((lo$7 + c$1) | 0);
    var hi$22 = ((((-2147483648) ^ lo$8) < ((-2147483648) ^ lo$7)) ? ((1 + hi$20) | 0) : hi$20);
    var lo$9 = ((lo$8 + d$1) | 0);
    var hi$24 = ((((-2147483648) ^ lo$9) < ((-2147483648) ^ lo$8)) ? ((1 + hi$22) | 0) : hi$22);
    res.set(index, lo$9);
    index = ((1 + index) | 0);
    var value$1 = res.get(index);
    var lo$10 = ((hi$24 + value$1) | 0);
    var hi$26 = ((((-2147483648) ^ lo$10) < ((-2147483648) ^ hi$24)) ? 1 : 0);
    res.set(index, lo$10);
    elem$1 = hi$26;
    i$3 = ((1 + i$3) | 0);
    index = ((1 + index) | 0)
  };
  return res
});
$c_Ljava_math_Multiplication$.prototype.pow__Ljava_math_BigInteger__I__Ljava_math_BigInteger = (function(base, exponent) {
  var exp = exponent;
  var res = $m_Ljava_math_BigInteger$().ONE$1;
  var acc = base;
  _loop: while (true) {
    if ((exp > 1)) {
      var res2 = (((1 & exp) !== 0) ? res.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(acc) : res);
      if ((acc.numberLength$2 === 1)) {
        var acc2 = acc.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(acc)
      } else {
        var a = $newArrayObject($d_I.getArrayOf(), [(acc.numberLength$2 << 1)]);
        var sq = this.square__AI__I__AI__AI(acc.digits$2, acc.numberLength$2, a);
        var acc2 = new $c_Ljava_math_BigInteger().init___I__AI(1, sq)
      };
      var temp$exp = (exp >> 1);
      exp = temp$exp;
      res = res2;
      acc = acc2;
      continue _loop
    } else {
      return res.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(acc)
    }
  }
});
$c_Ljava_math_Multiplication$.prototype.multPAP__p1__AI__AI__AI__I__I__V = (function(a, b, t, aLen, bLen) {
  if (((a === b) && (aLen === bLen))) {
    this.square__AI__I__AI__AI(a, aLen, t)
  } else {
    var isEmpty$4 = (aLen <= 0);
    var scala$collection$immutable$Range$$lastElement$4 = (((-1) + aLen) | 0);
    if ((!isEmpty$4)) {
      var i = 0;
      while (true) {
        var v1 = i;
        var elem$1 = 0;
        elem$1 = 0;
        var aI = a.get(v1);
        var isEmpty$4$1 = (bLen <= 0);
        var scala$collection$immutable$Range$$lastElement$4$1 = (((-1) + bLen) | 0);
        if ((!isEmpty$4$1)) {
          var i$1 = 0;
          while (true) {
            var v1$1 = i$1;
            var b$1 = b.get(v1$1);
            var c = t.get(((v1 + v1$1) | 0));
            var d = elem$1;
            var a0 = (65535 & aI);
            var a1 = ((aI >>> 16) | 0);
            var b0 = (65535 & b$1);
            var b1 = ((b$1 >>> 16) | 0);
            var a0b0 = $imul(a0, b0);
            var a1b0 = $imul(a1, b0);
            var a0b1 = $imul(a0, b1);
            var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
            var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
            var hi$6 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
            var lo$1 = ((lo + c) | 0);
            var hi$8 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + hi$6) | 0) : hi$6);
            var lo$2 = ((lo$1 + d) | 0);
            var hi$10 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ lo$1)) ? ((1 + hi$8) | 0) : hi$8);
            t.set(((v1 + v1$1) | 0), lo$2);
            elem$1 = hi$10;
            if ((i$1 === scala$collection$immutable$Range$$lastElement$4$1)) {
              break
            };
            i$1 = ((1 + i$1) | 0)
          }
        };
        t.set(((v1 + bLen) | 0), elem$1);
        if ((i === scala$collection$immutable$Range$$lastElement$4)) {
          break
        };
        i = ((1 + i) | 0)
      }
    }
  }
});
$c_Ljava_math_Multiplication$.prototype.initialiseArrays__p1__V = (function() {
  var elem$1_$_lo$2 = 0;
  var elem$1_$_hi$2 = 0;
  var jsx$1_$_lo$2 = 1;
  var jsx$1_$_hi$2 = 0;
  elem$1_$_lo$2 = jsx$1_$_lo$2;
  elem$1_$_hi$2 = jsx$1_$_hi$2;
  var i = 0;
  while (true) {
    var v1 = i;
    if ((v1 <= 18)) {
      this.BigFivePows$1.set(v1, $m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(elem$1_$_lo$2, elem$1_$_hi$2)));
      var jsx$3 = this.BigTenPows$1;
      var jsx$2 = $m_Ljava_math_BigInteger$();
      var this$5_$_lo$2 = elem$1_$_lo$2;
      var this$5_$_hi$2 = elem$1_$_hi$2;
      var lo = (((32 & v1) === 0) ? (this$5_$_lo$2 << v1) : 0);
      var hi = (((32 & v1) === 0) ? (((((this$5_$_lo$2 >>> 1) | 0) >>> ((31 - v1) | 0)) | 0) | (this$5_$_hi$2 << v1)) : (this$5_$_lo$2 << v1));
      jsx$3.set(v1, jsx$2.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo, hi)));
      var b_$_lo$2 = elem$1_$_lo$2;
      var b_$_hi$2 = elem$1_$_hi$2;
      var blo = b_$_lo$2;
      var b0 = (65535 & blo);
      var b1 = ((blo >>> 16) | 0);
      var a0b0 = $imul(5, b0);
      var a0b1 = $imul(5, b1);
      var lo$1 = ((a0b0 + (a0b1 << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$1 = (($imul(5, b_$_hi$2) + ((c1part >>> 16) | 0)) | 0);
      var jsx$4_$_lo$2 = lo$1;
      var jsx$4_$_hi$2 = hi$1;
      elem$1_$_lo$2 = jsx$4_$_lo$2;
      elem$1_$_hi$2 = jsx$4_$_hi$2
    } else {
      this.BigFivePows$1.set(v1, this.BigFivePows$1.get((((-1) + v1) | 0)).multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(this.BigFivePows$1.get(1)));
      this.BigTenPows$1.set(v1, this.BigTenPows$1.get((((-1) + v1) | 0)).multiply__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_BigInteger$().TEN$1))
    };
    if ((i === 31)) {
      break
    };
    i = ((1 + i) | 0)
  }
});
$c_Ljava_math_Multiplication$.prototype.newArrayOfPows__p1__I__I__AI = (function(len, pow) {
  var xs = $newArrayObject($d_I.getArrayOf(), [(((-1) + len) | 0)]);
  var elems$2 = null;
  elems$2 = [];
  var x1 = xs.u.length;
  switch (x1) {
    case (-1): {
      break
    }
  };
  var elem$1 = null;
  elem$1 = 1;
  var elem = elem$1;
  var unboxedElem = ((elem === null) ? 0 : elem);
  elems$2.push(unboxedElem);
  var i = 0;
  var len$1 = xs.u.length;
  while ((i < len$1)) {
    var idx = i;
    var arg1 = xs.get(idx);
    var arg1$1 = elem$1;
    var z = $uI(arg1$1);
    elem$1 = $imul(z, pow);
    var elem$2 = elem$1;
    var unboxedElem$1 = ((elem$2 === null) ? 0 : elem$2);
    elems$2.push(unboxedElem$1);
    i = ((1 + i) | 0)
  };
  return $makeNativeArrayWrapper($d_I.getArrayOf(), elems$2)
});
$c_Ljava_math_Multiplication$.prototype.karatsuba__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(val1, val2) {
  if ((val2.numberLength$2 > val1.numberLength$2)) {
    var x1_$_$$und1$f = val2;
    var x1_$_$$und2$f = val1
  } else {
    var x1_$_$$und1$f = val1;
    var x1_$_$$und2$f = val2
  };
  var op1 = $as_Ljava_math_BigInteger(x1_$_$$und1$f);
  var op2 = $as_Ljava_math_BigInteger(x1_$_$$und2$f);
  if ((op2.numberLength$2 < 63)) {
    return this.multiplyPAP__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(op1, op2)
  } else {
    var ndiv2 = (((-2) & op1.numberLength$2) << 4);
    var upperOp1 = op1.shiftRight__I__Ljava_math_BigInteger(ndiv2);
    var upperOp2 = op2.shiftRight__I__Ljava_math_BigInteger(ndiv2);
    var bi = upperOp1.shiftLeft__I__Ljava_math_BigInteger(ndiv2);
    var lowerOp1 = $m_Ljava_math_Elementary$().subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(op1, bi);
    var bi$1 = upperOp2.shiftLeft__I__Ljava_math_BigInteger(ndiv2);
    var lowerOp2 = $m_Ljava_math_Elementary$().subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(op2, bi$1);
    var upper = this.karatsuba__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(upperOp1, upperOp2);
    var lower = this.karatsuba__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(lowerOp1, lowerOp2);
    var middle = this.karatsuba__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_Elementary$().subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(upperOp1, lowerOp1), $m_Ljava_math_Elementary$().subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(lowerOp2, upperOp2));
    var this$1 = middle;
    var bi$2 = upper;
    var this$2 = $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$1, bi$2);
    middle = $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$2, lower);
    middle = middle.shiftLeft__I__Ljava_math_BigInteger(ndiv2);
    upper = upper.shiftLeft__I__Ljava_math_BigInteger((ndiv2 << 1));
    var this$3 = upper;
    var bi$3 = middle;
    var this$4 = $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$3, bi$3);
    return $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$4, lower)
  }
});
$c_Ljava_math_Multiplication$.prototype.multiplyByInt__p1__AI__AI__I__I__I = (function(res, a, aSize, factor) {
  var elem$1 = 0;
  elem$1 = 0;
  var isEmpty$4 = (aSize <= 0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + aSize) | 0);
  if ((!isEmpty$4)) {
    var i = 0;
    while (true) {
      var v1 = i;
      var a$1 = a.get(v1);
      var c = elem$1;
      var a0 = (65535 & a$1);
      var a1 = ((a$1 >>> 16) | 0);
      var b0 = (65535 & factor);
      var b1 = ((factor >>> 16) | 0);
      var a0b0 = $imul(a0, b0);
      var a1b0 = $imul(a1, b0);
      var a0b1 = $imul(a0, b1);
      var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$4 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
      var lo$1 = ((lo + c) | 0);
      var hi$6 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + hi$4) | 0) : hi$4);
      res.set(v1, lo$1);
      elem$1 = hi$6;
      if ((i === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i = ((1 + i) | 0)
    }
  };
  return elem$1
});
$c_Ljava_math_Multiplication$.prototype.multArraysPAP__AI__I__AI__I__AI__V = (function(aDigits, aLen, bDigits, bLen, resDigits) {
  if ((!((aLen === 0) || (bLen === 0)))) {
    if ((aLen === 1)) {
      resDigits.set(bLen, this.multiplyByInt__p1__AI__AI__I__I__I(resDigits, bDigits, bLen, aDigits.get(0)))
    } else if ((bLen === 1)) {
      resDigits.set(aLen, this.multiplyByInt__p1__AI__AI__I__I__I(resDigits, aDigits, aLen, bDigits.get(0)))
    } else {
      this.multPAP__p1__AI__AI__AI__I__I__V(aDigits, bDigits, resDigits, aLen, bLen)
    }
  }
});
$c_Ljava_math_Multiplication$.prototype.multiplyByPosInt__Ljava_math_BigInteger__I__Ljava_math_BigInteger = (function(bi, factor) {
  var resSign = bi.sign$2;
  var aNumberLength = bi.numberLength$2;
  var aDigits = bi.digits$2;
  if ((resSign === 0)) {
    return $m_Ljava_math_BigInteger$().ZERO$1
  } else if ((aNumberLength === 1)) {
    var a = aDigits.get(0);
    var a0 = (65535 & a);
    var a1 = ((a >>> 16) | 0);
    var b0 = (65535 & factor);
    var b1 = ((factor >>> 16) | 0);
    var a0b0 = $imul(a0, b0);
    var a1b0 = $imul(a1, b0);
    var a0b1 = $imul(a0, b1);
    var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
    var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
    var hi$2 = (((($imul(a1, b1) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
    return ((hi$2 === 0) ? new $c_Ljava_math_BigInteger().init___I__I(resSign, lo) : new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, 2, $m_s_Array$().apply__I__sc_Seq__AI(lo, new $c_sjs_js_WrappedArray().init___sjs_js_Array([hi$2]))))
  } else {
    var resLength = ((1 + aNumberLength) | 0);
    var resDigits = $newArrayObject($d_I.getArrayOf(), [resLength]);
    resDigits.set(aNumberLength, this.multiplyByInt__p1__AI__AI__I__I__I(resDigits, aDigits, aNumberLength, factor));
    var result = new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, resLength, resDigits);
    result.cutOffLeadingZeroes__V();
    return result
  }
});
$c_Ljava_math_Multiplication$.prototype.multiplyByFivePow__Ljava_math_BigInteger__I__Ljava_math_BigInteger = (function(bi, exp) {
  return ((exp < this.FivePows$1.u.length) ? this.multiplyByPosInt__Ljava_math_BigInteger__I__Ljava_math_BigInteger(bi, this.FivePows$1.get(exp)) : ((exp < this.BigFivePows$1.u.length) ? bi.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(this.BigFivePows$1.get(exp)) : bi.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(this.BigFivePows$1.get(1).pow__I__Ljava_math_BigInteger(exp))))
});
$c_Ljava_math_Multiplication$.prototype.powerOf10__J__Ljava_math_BigInteger = (function(exp) {
  var value = this.BigTenPows$1.u.length;
  var hi = (value >> 31);
  var ahi = exp.hi$2;
  if (((ahi === hi) ? (((-2147483648) ^ exp.lo$2) < ((-2147483648) ^ value)) : (ahi < hi))) {
    return this.BigTenPows$1.get(exp.lo$2)
  } else {
    var ahi$1 = exp.hi$2;
    if (((ahi$1 === 0) ? (((-2147483648) ^ exp.lo$2) <= (-2147483598)) : (ahi$1 < 0))) {
      return $m_Ljava_math_BigInteger$().TEN$1.pow__I__Ljava_math_BigInteger(exp.lo$2)
    } else {
      var ahi$2 = exp.hi$2;
      if (((ahi$2 === 0) ? (((-2147483648) ^ exp.lo$2) <= (-1)) : (ahi$2 < 0))) {
        return this.BigFivePows$1.get(1).pow__I__Ljava_math_BigInteger(exp.lo$2).shiftLeft__I__Ljava_math_BigInteger(exp.lo$2)
      } else {
        var powerOfFive = this.BigFivePows$1.get(1).pow__I__Ljava_math_BigInteger(2147483647);
        var res = powerOfFive;
        var bhi = exp.hi$2;
        var lo = (((-2147483647) + exp.lo$2) | 0);
        var hi$1 = ((((-2147483648) ^ lo) < 1) ? bhi : (((-1) + bhi) | 0));
        var longExp_$_lo$2 = lo;
        var longExp_$_hi$2 = hi$1;
        var this$2 = $m_sjsr_RuntimeLong$();
        var lo$1 = this$2.remainderImpl__I__I__I__I__I(exp.lo$2, exp.hi$2, 2147483647, 0);
        while (true) {
          var this$3_$_lo$2 = longExp_$_lo$2;
          var this$3_$_hi$2 = longExp_$_hi$2;
          var ahi$3 = this$3_$_hi$2;
          if (((ahi$3 === 0) ? (((-2147483648) ^ this$3_$_lo$2) > (-1)) : (ahi$3 > 0))) {
            res = res.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(powerOfFive);
            var b_$_lo$2 = longExp_$_lo$2;
            var b_$_hi$2 = longExp_$_hi$2;
            var bhi$1 = b_$_hi$2;
            var lo$2 = (((-2147483647) + b_$_lo$2) | 0);
            var hi$3 = ((((-2147483648) ^ lo$2) < 1) ? bhi$1 : (((-1) + bhi$1) | 0));
            var jsx$1_$_lo$2 = lo$2;
            var jsx$1_$_hi$2 = hi$3;
            longExp_$_lo$2 = jsx$1_$_lo$2;
            longExp_$_hi$2 = jsx$1_$_hi$2
          } else {
            break
          }
        };
        res = res.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(this.BigFivePows$1.get(1).pow__I__Ljava_math_BigInteger(lo$1));
        res = res.shiftLeft__I__Ljava_math_BigInteger(2147483647);
        var bhi$2 = exp.hi$2;
        var lo$3 = (((-2147483647) + exp.lo$2) | 0);
        var hi$4 = ((((-2147483648) ^ lo$3) < 1) ? bhi$2 : (((-1) + bhi$2) | 0));
        var jsx$2_$_lo$2 = lo$3;
        var jsx$2_$_hi$2 = hi$4;
        longExp_$_lo$2 = jsx$2_$_lo$2;
        longExp_$_hi$2 = jsx$2_$_hi$2;
        while (true) {
          var this$6_$_lo$2 = longExp_$_lo$2;
          var this$6_$_hi$2 = longExp_$_hi$2;
          var ahi$4 = this$6_$_hi$2;
          if (((ahi$4 === 0) ? (((-2147483648) ^ this$6_$_lo$2) > (-1)) : (ahi$4 > 0))) {
            res = res.shiftLeft__I__Ljava_math_BigInteger(2147483647);
            var b$1_$_lo$2 = longExp_$_lo$2;
            var b$1_$_hi$2 = longExp_$_hi$2;
            var bhi$3 = b$1_$_hi$2;
            var lo$4 = (((-2147483647) + b$1_$_lo$2) | 0);
            var hi$5 = ((((-2147483648) ^ lo$4) < 1) ? bhi$3 : (((-1) + bhi$3) | 0));
            var jsx$3_$_lo$2 = lo$4;
            var jsx$3_$_hi$2 = hi$5;
            longExp_$_lo$2 = jsx$3_$_lo$2;
            longExp_$_hi$2 = jsx$3_$_hi$2
          } else {
            break
          }
        };
        return res.shiftLeft__I__Ljava_math_BigInteger(lo$1)
      }
    }
  }
});
var $d_Ljava_math_Multiplication$ = new $TypeData().initClass({
  Ljava_math_Multiplication$: 0
}, false, "java.math.Multiplication$", {
  Ljava_math_Multiplication$: 1,
  O: 1
});
$c_Ljava_math_Multiplication$.prototype.$classData = $d_Ljava_math_Multiplication$;
var $n_Ljava_math_Multiplication$ = (void 0);
function $m_Ljava_math_Multiplication$() {
  if ((!$n_Ljava_math_Multiplication$)) {
    $n_Ljava_math_Multiplication$ = new $c_Ljava_math_Multiplication$().init___()
  };
  return $n_Ljava_math_Multiplication$
}
/** @constructor */
function $c_jl_Class() {
  $c_O.call(this);
  this.data$1 = null
}
$c_jl_Class.prototype = new $h_O();
$c_jl_Class.prototype.constructor = $c_jl_Class;
/** @constructor */
function $h_jl_Class() {
  /*<skip>*/
}
$h_jl_Class.prototype = $c_jl_Class.prototype;
$c_jl_Class.prototype.getName__T = (function() {
  return $as_T(this.data$1.name)
});
$c_jl_Class.prototype.isPrimitive__Z = (function() {
  return $uZ(this.data$1.isPrimitive)
});
$c_jl_Class.prototype.toString__T = (function() {
  return ((this.isInterface__Z() ? "interface " : (this.isPrimitive__Z() ? "" : "class ")) + this.getName__T())
});
$c_jl_Class.prototype.init___jl_ScalaJSClassData = (function(data) {
  this.data$1 = data;
  return this
});
$c_jl_Class.prototype.isInterface__Z = (function() {
  return $uZ(this.data$1.isInterface)
});
var $d_jl_Class = new $TypeData().initClass({
  jl_Class: 0
}, false, "java.lang.Class", {
  jl_Class: 1,
  O: 1
});
$c_jl_Class.prototype.$classData = $d_jl_Class;
/** @constructor */
function $c_jl_Long$StringRadixInfo() {
  $c_O.call(this);
  this.chunkLength$1 = 0;
  this.radixPowLength$1 = $m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong();
  this.paddingZeros$1 = null;
  this.overflowBarrier$1 = $m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong()
}
$c_jl_Long$StringRadixInfo.prototype = new $h_O();
$c_jl_Long$StringRadixInfo.prototype.constructor = $c_jl_Long$StringRadixInfo;
/** @constructor */
function $h_jl_Long$StringRadixInfo() {
  /*<skip>*/
}
$h_jl_Long$StringRadixInfo.prototype = $c_jl_Long$StringRadixInfo.prototype;
$c_jl_Long$StringRadixInfo.prototype.init___I__J__T__J = (function(chunkLength, radixPowLength, paddingZeros, overflowBarrier) {
  this.chunkLength$1 = chunkLength;
  this.radixPowLength$1 = radixPowLength;
  this.paddingZeros$1 = paddingZeros;
  this.overflowBarrier$1 = overflowBarrier;
  return this
});
function $is_jl_Long$StringRadixInfo(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_Long$StringRadixInfo)))
}
function $as_jl_Long$StringRadixInfo(obj) {
  return (($is_jl_Long$StringRadixInfo(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Long$StringRadixInfo"))
}
function $isArrayOf_jl_Long$StringRadixInfo(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Long$StringRadixInfo)))
}
function $asArrayOf_jl_Long$StringRadixInfo(obj, depth) {
  return (($isArrayOf_jl_Long$StringRadixInfo(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Long$StringRadixInfo;", depth))
}
var $d_jl_Long$StringRadixInfo = new $TypeData().initClass({
  jl_Long$StringRadixInfo: 0
}, false, "java.lang.Long$StringRadixInfo", {
  jl_Long$StringRadixInfo: 1,
  O: 1
});
$c_jl_Long$StringRadixInfo.prototype.$classData = $d_jl_Long$StringRadixInfo;
/** @constructor */
function $c_jl_System$() {
  $c_O.call(this);
  this.out$1 = null;
  this.err$1 = null;
  this.in$1 = null;
  this.getHighPrecisionTime$1 = null
}
$c_jl_System$.prototype = new $h_O();
$c_jl_System$.prototype.constructor = $c_jl_System$;
/** @constructor */
function $h_jl_System$() {
  /*<skip>*/
}
$h_jl_System$.prototype = $c_jl_System$.prototype;
$c_jl_System$.prototype.init___ = (function() {
  $n_jl_System$ = this;
  this.out$1 = new $c_jl_JSConsoleBasedPrintStream().init___jl_Boolean(false);
  this.err$1 = new $c_jl_JSConsoleBasedPrintStream().init___jl_Boolean(true);
  this.in$1 = null;
  var x = $g.performance;
  if ($uZ((!(!x)))) {
    var x$1 = $g.performance.now;
    if ($uZ((!(!x$1)))) {
      var jsx$1 = (function() {
        return $m_jl_System$().java$lang$System$$$anonfun$getHighPrecisionTime$1__D()
      })
    } else {
      var x$2 = $g.performance.webkitNow;
      if ($uZ((!(!x$2)))) {
        var jsx$1 = (function() {
          return $m_jl_System$().java$lang$System$$$anonfun$getHighPrecisionTime$2__D()
        })
      } else {
        var jsx$1 = (function() {
          return $m_jl_System$().java$lang$System$$$anonfun$getHighPrecisionTime$3__D()
        })
      }
    }
  } else {
    var jsx$1 = (function() {
      return $m_jl_System$().java$lang$System$$$anonfun$getHighPrecisionTime$4__D()
    })
  };
  this.getHighPrecisionTime$1 = jsx$1;
  return this
});
$c_jl_System$.prototype.java$lang$System$$$anonfun$getHighPrecisionTime$3__D = (function() {
  return $uD(new $g.Date().getTime())
});
$c_jl_System$.prototype.java$lang$System$$$anonfun$getHighPrecisionTime$1__D = (function() {
  return $uD($g.performance.now())
});
$c_jl_System$.prototype.java$lang$System$$$anonfun$getHighPrecisionTime$4__D = (function() {
  return $uD(new $g.Date().getTime())
});
$c_jl_System$.prototype.java$lang$System$$$anonfun$getHighPrecisionTime$2__D = (function() {
  return $uD($g.performance.webkitNow())
});
var $d_jl_System$ = new $TypeData().initClass({
  jl_System$: 0
}, false, "java.lang.System$", {
  jl_System$: 1,
  O: 1
});
$c_jl_System$.prototype.$classData = $d_jl_System$;
var $n_jl_System$ = (void 0);
function $m_jl_System$() {
  if ((!$n_jl_System$)) {
    $n_jl_System$ = new $c_jl_System$().init___()
  };
  return $n_jl_System$
}
/** @constructor */
function $c_ju_Arrays$() {
  $c_O.call(this)
}
$c_ju_Arrays$.prototype = new $h_O();
$c_ju_Arrays$.prototype.constructor = $c_ju_Arrays$;
/** @constructor */
function $h_ju_Arrays$() {
  /*<skip>*/
}
$h_ju_Arrays$.prototype = $c_ju_Arrays$.prototype;
$c_ju_Arrays$.prototype.init___ = (function() {
  return this
});
$c_ju_Arrays$.prototype.binarySearch__AJ__J__I = (function(a, key) {
  var startIndex = 0;
  var endIndex = a.u.length;
  _binarySearchImpl: while (true) {
    if ((startIndex === endIndex)) {
      return (((-1) - startIndex) | 0)
    } else {
      var mid = ((((startIndex + endIndex) | 0) >>> 1) | 0);
      var elem = a.get(mid);
      var t = $uJ(elem);
      var lo = t.lo$2;
      var hi = t.hi$2;
      var ahi = key.hi$2;
      if (((ahi === hi) ? (((-2147483648) ^ key.lo$2) < ((-2147483648) ^ lo)) : (ahi < hi))) {
        endIndex = mid;
        continue _binarySearchImpl
      } else if ($m_sr_BoxesRunTime$().equals__O__O__Z(key, elem)) {
        return mid
      } else {
        startIndex = ((1 + mid) | 0);
        continue _binarySearchImpl
      }
    }
  }
});
var $d_ju_Arrays$ = new $TypeData().initClass({
  ju_Arrays$: 0
}, false, "java.util.Arrays$", {
  ju_Arrays$: 1,
  O: 1
});
$c_ju_Arrays$.prototype.$classData = $d_ju_Arrays$;
var $n_ju_Arrays$ = (void 0);
function $m_ju_Arrays$() {
  if ((!$n_ju_Arrays$)) {
    $n_ju_Arrays$ = new $c_ju_Arrays$().init___()
  };
  return $n_ju_Arrays$
}
/** @constructor */
function $c_s_DeprecatedConsole() {
  $c_O.call(this)
}
$c_s_DeprecatedConsole.prototype = new $h_O();
$c_s_DeprecatedConsole.prototype.constructor = $c_s_DeprecatedConsole;
/** @constructor */
function $h_s_DeprecatedConsole() {
  /*<skip>*/
}
$h_s_DeprecatedConsole.prototype = $c_s_DeprecatedConsole.prototype;
/** @constructor */
function $c_s_FallbackArrayBuilding() {
  $c_O.call(this)
}
$c_s_FallbackArrayBuilding.prototype = new $h_O();
$c_s_FallbackArrayBuilding.prototype.constructor = $c_s_FallbackArrayBuilding;
/** @constructor */
function $h_s_FallbackArrayBuilding() {
  /*<skip>*/
}
$h_s_FallbackArrayBuilding.prototype = $c_s_FallbackArrayBuilding.prototype;
/** @constructor */
function $c_s_LowPriorityImplicits() {
  $c_O.call(this)
}
$c_s_LowPriorityImplicits.prototype = new $h_O();
$c_s_LowPriorityImplicits.prototype.constructor = $c_s_LowPriorityImplicits;
/** @constructor */
function $h_s_LowPriorityImplicits() {
  /*<skip>*/
}
$h_s_LowPriorityImplicits.prototype = $c_s_LowPriorityImplicits.prototype;
function $f_s_math_Ordered__$$less__O__Z($thiz, that) {
  return ($thiz.compare__O__I(that) < 0)
}
function $f_s_math_Ordered__$$less$eq__O__Z($thiz, that) {
  return ($thiz.compare__O__I(that) <= 0)
}
function $f_s_math_Ordered__$$greater$eq__O__Z($thiz, that) {
  return ($thiz.compare__O__I(that) >= 0)
}
function $f_s_math_Ordered__$$greater__O__Z($thiz, that) {
  return ($thiz.compare__O__I(that) > 0)
}
function $is_s_math_Ordered(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_math_Ordered)))
}
function $as_s_math_Ordered(obj) {
  return (($is_s_math_Ordered(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.math.Ordered"))
}
function $isArrayOf_s_math_Ordered(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_Ordered)))
}
function $asArrayOf_s_math_Ordered(obj, depth) {
  return (($isArrayOf_s_math_Ordered(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.math.Ordered;", depth))
}
/** @constructor */
function $c_s_math_Ordered$() {
  $c_O.call(this)
}
$c_s_math_Ordered$.prototype = new $h_O();
$c_s_math_Ordered$.prototype.constructor = $c_s_math_Ordered$;
/** @constructor */
function $h_s_math_Ordered$() {
  /*<skip>*/
}
$h_s_math_Ordered$.prototype = $c_s_math_Ordered$.prototype;
$c_s_math_Ordered$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Ordered$ = new $TypeData().initClass({
  s_math_Ordered$: 0
}, false, "scala.math.Ordered$", {
  s_math_Ordered$: 1,
  O: 1
});
$c_s_math_Ordered$.prototype.$classData = $d_s_math_Ordered$;
var $n_s_math_Ordered$ = (void 0);
function $m_s_math_Ordered$() {
  if ((!$n_s_math_Ordered$)) {
    $n_s_math_Ordered$ = new $c_s_math_Ordered$().init___()
  };
  return $n_s_math_Ordered$
}
/** @constructor */
function $c_s_package$() {
  $c_O.call(this);
  this.BigDecimal$1 = null;
  this.BigInt$1 = null;
  this.AnyRef$1 = null;
  this.Traversable$1 = null;
  this.Iterable$1 = null;
  this.Seq$1 = null;
  this.IndexedSeq$1 = null;
  this.Iterator$1 = null;
  this.List$1 = null;
  this.Nil$1 = null;
  this.$$colon$colon$1 = null;
  this.$$plus$colon$1 = null;
  this.$$colon$plus$1 = null;
  this.Stream$1 = null;
  this.$$hash$colon$colon$1 = null;
  this.Vector$1 = null;
  this.StringBuilder$1 = null;
  this.Range$1 = null;
  this.Equiv$1 = null;
  this.Fractional$1 = null;
  this.Integral$1 = null;
  this.Numeric$1 = null;
  this.Ordered$1 = null;
  this.Ordering$1 = null;
  this.Either$1 = null;
  this.Left$1 = null;
  this.Right$1 = null;
  this.bitmap$0$1 = 0
}
$c_s_package$.prototype = new $h_O();
$c_s_package$.prototype.constructor = $c_s_package$;
/** @constructor */
function $h_s_package$() {
  /*<skip>*/
}
$h_s_package$.prototype = $c_s_package$.prototype;
$c_s_package$.prototype.init___ = (function() {
  $n_s_package$ = this;
  this.AnyRef$1 = new $c_s_package$$anon$1().init___();
  this.Traversable$1 = $m_sc_Traversable$();
  this.Iterable$1 = $m_sc_Iterable$();
  this.Seq$1 = $m_sc_Seq$();
  this.IndexedSeq$1 = $m_sc_IndexedSeq$();
  this.Iterator$1 = $m_sc_Iterator$();
  this.List$1 = $m_sci_List$();
  this.Nil$1 = $m_sci_Nil$();
  this.$$colon$colon$1 = $m_sci_$colon$colon$();
  this.$$plus$colon$1 = $m_sc_$plus$colon$();
  this.$$colon$plus$1 = $m_sc_$colon$plus$();
  this.Stream$1 = $m_sci_Stream$();
  this.$$hash$colon$colon$1 = $m_sci_Stream$$hash$colon$colon$();
  this.Vector$1 = $m_sci_Vector$();
  this.StringBuilder$1 = $m_scm_StringBuilder$();
  this.Range$1 = $m_sci_Range$();
  this.Equiv$1 = $m_s_math_Equiv$();
  this.Fractional$1 = $m_s_math_Fractional$();
  this.Integral$1 = $m_s_math_Integral$();
  this.Numeric$1 = $m_s_math_Numeric$();
  this.Ordered$1 = $m_s_math_Ordered$();
  this.Ordering$1 = $m_s_math_Ordering$();
  this.Either$1 = $m_s_util_Either$();
  this.Left$1 = $m_s_util_Left$();
  this.Right$1 = $m_s_util_Right$();
  return this
});
$c_s_package$.prototype.BigInt__s_math_BigInt$ = (function() {
  return (((2 & this.bitmap$0$1) === 0) ? this.BigInt$lzycompute__p1__s_math_BigInt$() : this.BigInt$1)
});
$c_s_package$.prototype.BigInt$lzycompute__p1__s_math_BigInt$ = (function() {
  if (((2 & this.bitmap$0$1) === 0)) {
    this.BigInt$1 = $m_s_math_BigInt$();
    this.bitmap$0$1 = (((2 | this.bitmap$0$1) << 24) >> 24)
  };
  return this.BigInt$1
});
$c_s_package$.prototype.BigDecimal__s_math_BigDecimal$ = (function() {
  return (((1 & this.bitmap$0$1) === 0) ? this.BigDecimal$lzycompute__p1__s_math_BigDecimal$() : this.BigDecimal$1)
});
$c_s_package$.prototype.BigDecimal$lzycompute__p1__s_math_BigDecimal$ = (function() {
  if (((1 & this.bitmap$0$1) === 0)) {
    this.BigDecimal$1 = $m_s_math_BigDecimal$();
    this.bitmap$0$1 = (((1 | this.bitmap$0$1) << 24) >> 24)
  };
  return this.BigDecimal$1
});
var $d_s_package$ = new $TypeData().initClass({
  s_package$: 0
}, false, "scala.package$", {
  s_package$: 1,
  O: 1
});
$c_s_package$.prototype.$classData = $d_s_package$;
var $n_s_package$ = (void 0);
function $m_s_package$() {
  if ((!$n_s_package$)) {
    $n_s_package$ = new $c_s_package$().init___()
  };
  return $n_s_package$
}
/** @constructor */
function $c_s_reflect_ClassManifestFactory$() {
  $c_O.call(this);
  this.Byte$1 = null;
  this.Short$1 = null;
  this.Char$1 = null;
  this.Int$1 = null;
  this.Long$1 = null;
  this.Float$1 = null;
  this.Double$1 = null;
  this.Boolean$1 = null;
  this.Unit$1 = null;
  this.Any$1 = null;
  this.Object$1 = null;
  this.AnyVal$1 = null;
  this.Nothing$1 = null;
  this.Null$1 = null
}
$c_s_reflect_ClassManifestFactory$.prototype = new $h_O();
$c_s_reflect_ClassManifestFactory$.prototype.constructor = $c_s_reflect_ClassManifestFactory$;
/** @constructor */
function $h_s_reflect_ClassManifestFactory$() {
  /*<skip>*/
}
$h_s_reflect_ClassManifestFactory$.prototype = $c_s_reflect_ClassManifestFactory$.prototype;
$c_s_reflect_ClassManifestFactory$.prototype.init___ = (function() {
  $n_s_reflect_ClassManifestFactory$ = this;
  this.Byte$1 = $m_s_reflect_ManifestFactory$ByteManifest$();
  this.Short$1 = $m_s_reflect_ManifestFactory$ShortManifest$();
  this.Char$1 = $m_s_reflect_ManifestFactory$CharManifest$();
  this.Int$1 = $m_s_reflect_ManifestFactory$IntManifest$();
  this.Long$1 = $m_s_reflect_ManifestFactory$LongManifest$();
  this.Float$1 = $m_s_reflect_ManifestFactory$FloatManifest$();
  this.Double$1 = $m_s_reflect_ManifestFactory$DoubleManifest$();
  this.Boolean$1 = $m_s_reflect_ManifestFactory$BooleanManifest$();
  this.Unit$1 = $m_s_reflect_ManifestFactory$UnitManifest$();
  this.Any$1 = $m_s_reflect_ManifestFactory$AnyManifest$();
  this.Object$1 = $m_s_reflect_ManifestFactory$ObjectManifest$();
  this.AnyVal$1 = $m_s_reflect_ManifestFactory$AnyValManifest$();
  this.Nothing$1 = $m_s_reflect_ManifestFactory$NothingManifest$();
  this.Null$1 = $m_s_reflect_ManifestFactory$NullManifest$();
  return this
});
var $d_s_reflect_ClassManifestFactory$ = new $TypeData().initClass({
  s_reflect_ClassManifestFactory$: 0
}, false, "scala.reflect.ClassManifestFactory$", {
  s_reflect_ClassManifestFactory$: 1,
  O: 1
});
$c_s_reflect_ClassManifestFactory$.prototype.$classData = $d_s_reflect_ClassManifestFactory$;
var $n_s_reflect_ClassManifestFactory$ = (void 0);
function $m_s_reflect_ClassManifestFactory$() {
  if ((!$n_s_reflect_ClassManifestFactory$)) {
    $n_s_reflect_ClassManifestFactory$ = new $c_s_reflect_ClassManifestFactory$().init___()
  };
  return $n_s_reflect_ClassManifestFactory$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$() {
  $c_O.call(this)
}
$c_s_reflect_ManifestFactory$.prototype = new $h_O();
$c_s_reflect_ManifestFactory$.prototype.constructor = $c_s_reflect_ManifestFactory$;
/** @constructor */
function $h_s_reflect_ManifestFactory$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$.prototype = $c_s_reflect_ManifestFactory$.prototype;
$c_s_reflect_ManifestFactory$.prototype.init___ = (function() {
  return this
});
var $d_s_reflect_ManifestFactory$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$: 0
}, false, "scala.reflect.ManifestFactory$", {
  s_reflect_ManifestFactory$: 1,
  O: 1
});
$c_s_reflect_ManifestFactory$.prototype.$classData = $d_s_reflect_ManifestFactory$;
var $n_s_reflect_ManifestFactory$ = (void 0);
function $m_s_reflect_ManifestFactory$() {
  if ((!$n_s_reflect_ManifestFactory$)) {
    $n_s_reflect_ManifestFactory$ = new $c_s_reflect_ManifestFactory$().init___()
  };
  return $n_s_reflect_ManifestFactory$
}
/** @constructor */
function $c_s_reflect_package$() {
  $c_O.call(this);
  this.ClassManifest$1 = null;
  this.Manifest$1 = null
}
$c_s_reflect_package$.prototype = new $h_O();
$c_s_reflect_package$.prototype.constructor = $c_s_reflect_package$;
/** @constructor */
function $h_s_reflect_package$() {
  /*<skip>*/
}
$h_s_reflect_package$.prototype = $c_s_reflect_package$.prototype;
$c_s_reflect_package$.prototype.init___ = (function() {
  $n_s_reflect_package$ = this;
  this.ClassManifest$1 = $m_s_reflect_ClassManifestFactory$();
  this.Manifest$1 = $m_s_reflect_ManifestFactory$();
  return this
});
var $d_s_reflect_package$ = new $TypeData().initClass({
  s_reflect_package$: 0
}, false, "scala.reflect.package$", {
  s_reflect_package$: 1,
  O: 1
});
$c_s_reflect_package$.prototype.$classData = $d_s_reflect_package$;
var $n_s_reflect_package$ = (void 0);
function $m_s_reflect_package$() {
  if ((!$n_s_reflect_package$)) {
    $n_s_reflect_package$ = new $c_s_reflect_package$().init___()
  };
  return $n_s_reflect_package$
}
/** @constructor */
function $c_s_util_DynamicVariable() {
  $c_O.call(this);
  this.v$1 = null
}
$c_s_util_DynamicVariable.prototype = new $h_O();
$c_s_util_DynamicVariable.prototype.constructor = $c_s_util_DynamicVariable;
/** @constructor */
function $h_s_util_DynamicVariable() {
  /*<skip>*/
}
$h_s_util_DynamicVariable.prototype = $c_s_util_DynamicVariable.prototype;
$c_s_util_DynamicVariable.prototype.toString__T = (function() {
  return (("DynamicVariable(" + this.v$1) + ")")
});
$c_s_util_DynamicVariable.prototype.init___O = (function(init) {
  this.v$1 = init;
  return this
});
var $d_s_util_DynamicVariable = new $TypeData().initClass({
  s_util_DynamicVariable: 0
}, false, "scala.util.DynamicVariable", {
  s_util_DynamicVariable: 1,
  O: 1
});
$c_s_util_DynamicVariable.prototype.$classData = $d_s_util_DynamicVariable;
/** @constructor */
function $c_s_util_control_Breaks() {
  $c_O.call(this);
  this.scala$util$control$Breaks$$breakException$1 = null
}
$c_s_util_control_Breaks.prototype = new $h_O();
$c_s_util_control_Breaks.prototype.constructor = $c_s_util_control_Breaks;
/** @constructor */
function $h_s_util_control_Breaks() {
  /*<skip>*/
}
$h_s_util_control_Breaks.prototype = $c_s_util_control_Breaks.prototype;
$c_s_util_control_Breaks.prototype.init___ = (function() {
  this.scala$util$control$Breaks$$breakException$1 = new $c_s_util_control_BreakControl().init___();
  return this
});
var $d_s_util_control_Breaks = new $TypeData().initClass({
  s_util_control_Breaks: 0
}, false, "scala.util.control.Breaks", {
  s_util_control_Breaks: 1,
  O: 1
});
$c_s_util_control_Breaks.prototype.$classData = $d_s_util_control_Breaks;
/** @constructor */
function $c_s_util_hashing_MurmurHash3() {
  $c_O.call(this)
}
$c_s_util_hashing_MurmurHash3.prototype = new $h_O();
$c_s_util_hashing_MurmurHash3.prototype.constructor = $c_s_util_hashing_MurmurHash3;
/** @constructor */
function $h_s_util_hashing_MurmurHash3() {
  /*<skip>*/
}
$h_s_util_hashing_MurmurHash3.prototype = $c_s_util_hashing_MurmurHash3.prototype;
$c_s_util_hashing_MurmurHash3.prototype.mixLast__I__I__I = (function(hash, data) {
  var k = data;
  k = $imul((-862048943), k);
  var i = k;
  k = ((i << 15) | ((i >>> 17) | 0));
  k = $imul(461845907, k);
  return (hash ^ k)
});
$c_s_util_hashing_MurmurHash3.prototype.mix__I__I__I = (function(hash, data) {
  var h = this.mixLast__I__I__I(hash, data);
  var i = h;
  h = ((i << 13) | ((i >>> 19) | 0));
  return (((-430675100) + $imul(5, h)) | 0)
});
$c_s_util_hashing_MurmurHash3.prototype.avalanche__p1__I__I = (function(hash) {
  var h = hash;
  h = (h ^ ((h >>> 16) | 0));
  h = $imul((-2048144789), h);
  h = (h ^ ((h >>> 13) | 0));
  h = $imul((-1028477387), h);
  h = (h ^ ((h >>> 16) | 0));
  return h
});
$c_s_util_hashing_MurmurHash3.prototype.productHash__s_Product__I__I = (function(x, seed) {
  var arr = x.productArity__I();
  if ((arr === 0)) {
    var this$1 = x.productPrefix__T();
    return $m_sjsr_RuntimeString$().hashCode__T__I(this$1)
  } else {
    var h = seed;
    var i = 0;
    while ((i < arr)) {
      h = this.mix__I__I__I(h, $m_sr_Statics$().anyHash__O__I(x.productElement__I__O(i)));
      i = ((1 + i) | 0)
    };
    return this.finalizeHash__I__I__I(h, arr)
  }
});
$c_s_util_hashing_MurmurHash3.prototype.finalizeHash__I__I__I = (function(hash, length) {
  return this.avalanche__p1__I__I((hash ^ length))
});
$c_s_util_hashing_MurmurHash3.prototype.orderedHash__sc_TraversableOnce__I__I = (function(xs, seed) {
  var n = new $c_sr_IntRef().init___I(0);
  var h = new $c_sr_IntRef().init___I(seed);
  xs.foreach__F1__V(new $c_sjsr_AnonFunction1().init___sjs_js_Function1((function($this, n$1, h$1) {
    return (function(x$2) {
      h$1.elem$1 = $this.mix__I__I__I(h$1.elem$1, $m_sr_Statics$().anyHash__O__I(x$2));
      n$1.elem$1 = ((1 + n$1.elem$1) | 0)
    })
  })(this, n, h)));
  return this.finalizeHash__I__I__I(h.elem$1, n.elem$1)
});
$c_s_util_hashing_MurmurHash3.prototype.listHash__sci_List__I__I = (function(xs, seed) {
  var n = 0;
  var h = seed;
  var elems = xs;
  while ((!elems.isEmpty__Z())) {
    var head = elems.head__O();
    var this$1 = elems;
    var tail = this$1.tail__sci_List();
    h = this.mix__I__I__I(h, $m_sr_Statics$().anyHash__O__I(head));
    n = ((1 + n) | 0);
    elems = tail
  };
  return this.finalizeHash__I__I__I(h, n)
});
/** @constructor */
function $c_sc_$colon$plus$() {
  $c_O.call(this)
}
$c_sc_$colon$plus$.prototype = new $h_O();
$c_sc_$colon$plus$.prototype.constructor = $c_sc_$colon$plus$;
/** @constructor */
function $h_sc_$colon$plus$() {
  /*<skip>*/
}
$h_sc_$colon$plus$.prototype = $c_sc_$colon$plus$.prototype;
$c_sc_$colon$plus$.prototype.init___ = (function() {
  return this
});
var $d_sc_$colon$plus$ = new $TypeData().initClass({
  sc_$colon$plus$: 0
}, false, "scala.collection.$colon$plus$", {
  sc_$colon$plus$: 1,
  O: 1
});
$c_sc_$colon$plus$.prototype.$classData = $d_sc_$colon$plus$;
var $n_sc_$colon$plus$ = (void 0);
function $m_sc_$colon$plus$() {
  if ((!$n_sc_$colon$plus$)) {
    $n_sc_$colon$plus$ = new $c_sc_$colon$plus$().init___()
  };
  return $n_sc_$colon$plus$
}
/** @constructor */
function $c_sc_$plus$colon$() {
  $c_O.call(this)
}
$c_sc_$plus$colon$.prototype = new $h_O();
$c_sc_$plus$colon$.prototype.constructor = $c_sc_$plus$colon$;
/** @constructor */
function $h_sc_$plus$colon$() {
  /*<skip>*/
}
$h_sc_$plus$colon$.prototype = $c_sc_$plus$colon$.prototype;
$c_sc_$plus$colon$.prototype.init___ = (function() {
  return this
});
var $d_sc_$plus$colon$ = new $TypeData().initClass({
  sc_$plus$colon$: 0
}, false, "scala.collection.$plus$colon$", {
  sc_$plus$colon$: 1,
  O: 1
});
$c_sc_$plus$colon$.prototype.$classData = $d_sc_$plus$colon$;
var $n_sc_$plus$colon$ = (void 0);
function $m_sc_$plus$colon$() {
  if ((!$n_sc_$plus$colon$)) {
    $n_sc_$plus$colon$ = new $c_sc_$plus$colon$().init___()
  };
  return $n_sc_$plus$colon$
}
/** @constructor */
function $c_sc_Iterator$() {
  $c_O.call(this);
  this.empty$1 = null
}
$c_sc_Iterator$.prototype = new $h_O();
$c_sc_Iterator$.prototype.constructor = $c_sc_Iterator$;
/** @constructor */
function $h_sc_Iterator$() {
  /*<skip>*/
}
$h_sc_Iterator$.prototype = $c_sc_Iterator$.prototype;
$c_sc_Iterator$.prototype.init___ = (function() {
  $n_sc_Iterator$ = this;
  this.empty$1 = new $c_sc_Iterator$$anon$2().init___();
  return this
});
var $d_sc_Iterator$ = new $TypeData().initClass({
  sc_Iterator$: 0
}, false, "scala.collection.Iterator$", {
  sc_Iterator$: 1,
  O: 1
});
$c_sc_Iterator$.prototype.$classData = $d_sc_Iterator$;
var $n_sc_Iterator$ = (void 0);
function $m_sc_Iterator$() {
  if ((!$n_sc_Iterator$)) {
    $n_sc_Iterator$ = new $c_sc_Iterator$().init___()
  };
  return $n_sc_Iterator$
}
function $f_sc_TraversableOnce__mkString__T__T__T__T($thiz, start, sep, end) {
  var this$1 = $thiz.addString__scm_StringBuilder__T__T__T__scm_StringBuilder(new $c_scm_StringBuilder().init___(), start, sep, end);
  var this$2 = this$1.underlying$5;
  return this$2.content$1
}
function $f_sc_TraversableOnce__addString__scm_StringBuilder__T__T__T__scm_StringBuilder($thiz, b, start, sep, end) {
  var first = new $c_sr_BooleanRef().init___Z(true);
  b.append__T__scm_StringBuilder(start);
  $thiz.foreach__F1__V(new $c_sjsr_AnonFunction1().init___sjs_js_Function1((function($this, b$1, sep$1, first$1) {
    return (function(x$2) {
      if (first$1.elem$1) {
        b$1.append__O__scm_StringBuilder(x$2);
        first$1.elem$1 = false;
        return (void 0)
      } else {
        b$1.append__T__scm_StringBuilder(sep$1);
        return b$1.append__O__scm_StringBuilder(x$2)
      }
    })
  })($thiz, b, sep, first)));
  b.append__T__scm_StringBuilder(end);
  return b
}
function $f_sc_TraversableOnce__nonEmpty__Z($thiz) {
  return (!$thiz.isEmpty__Z())
}
/** @constructor */
function $c_scg_GenMapFactory() {
  $c_O.call(this)
}
$c_scg_GenMapFactory.prototype = new $h_O();
$c_scg_GenMapFactory.prototype.constructor = $c_scg_GenMapFactory;
/** @constructor */
function $h_scg_GenMapFactory() {
  /*<skip>*/
}
$h_scg_GenMapFactory.prototype = $c_scg_GenMapFactory.prototype;
/** @constructor */
function $c_scg_GenericCompanion() {
  $c_O.call(this)
}
$c_scg_GenericCompanion.prototype = new $h_O();
$c_scg_GenericCompanion.prototype.constructor = $c_scg_GenericCompanion;
/** @constructor */
function $h_scg_GenericCompanion() {
  /*<skip>*/
}
$h_scg_GenericCompanion.prototype = $c_scg_GenericCompanion.prototype;
/** @constructor */
function $c_sci_Stream$$hash$colon$colon$() {
  $c_O.call(this)
}
$c_sci_Stream$$hash$colon$colon$.prototype = new $h_O();
$c_sci_Stream$$hash$colon$colon$.prototype.constructor = $c_sci_Stream$$hash$colon$colon$;
/** @constructor */
function $h_sci_Stream$$hash$colon$colon$() {
  /*<skip>*/
}
$h_sci_Stream$$hash$colon$colon$.prototype = $c_sci_Stream$$hash$colon$colon$.prototype;
$c_sci_Stream$$hash$colon$colon$.prototype.init___ = (function() {
  return this
});
var $d_sci_Stream$$hash$colon$colon$ = new $TypeData().initClass({
  sci_Stream$$hash$colon$colon$: 0
}, false, "scala.collection.immutable.Stream$$hash$colon$colon$", {
  sci_Stream$$hash$colon$colon$: 1,
  O: 1
});
$c_sci_Stream$$hash$colon$colon$.prototype.$classData = $d_sci_Stream$$hash$colon$colon$;
var $n_sci_Stream$$hash$colon$colon$ = (void 0);
function $m_sci_Stream$$hash$colon$colon$() {
  if ((!$n_sci_Stream$$hash$colon$colon$)) {
    $n_sci_Stream$$hash$colon$colon$ = new $c_sci_Stream$$hash$colon$colon$().init___()
  };
  return $n_sci_Stream$$hash$colon$colon$
}
/** @constructor */
function $c_sci_StringOps$() {
  $c_O.call(this)
}
$c_sci_StringOps$.prototype = new $h_O();
$c_sci_StringOps$.prototype.constructor = $c_sci_StringOps$;
/** @constructor */
function $h_sci_StringOps$() {
  /*<skip>*/
}
$h_sci_StringOps$.prototype = $c_sci_StringOps$.prototype;
$c_sci_StringOps$.prototype.init___ = (function() {
  return this
});
$c_sci_StringOps$.prototype.equals$extension__T__O__Z = (function($$this, x$1) {
  if ($is_sci_StringOps(x$1)) {
    var StringOps$1 = ((x$1 === null) ? null : $as_sci_StringOps(x$1).repr$1);
    return ($$this === StringOps$1)
  } else {
    return false
  }
});
var $d_sci_StringOps$ = new $TypeData().initClass({
  sci_StringOps$: 0
}, false, "scala.collection.immutable.StringOps$", {
  sci_StringOps$: 1,
  O: 1
});
$c_sci_StringOps$.prototype.$classData = $d_sci_StringOps$;
var $n_sci_StringOps$ = (void 0);
function $m_sci_StringOps$() {
  if ((!$n_sci_StringOps$)) {
    $n_sci_StringOps$ = new $c_sci_StringOps$().init___()
  };
  return $n_sci_StringOps$
}
/** @constructor */
function $c_sjsr_Bits$() {
  $c_O.call(this);
  this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f = false;
  this.arrayBuffer$1 = null;
  this.int32Array$1 = null;
  this.float32Array$1 = null;
  this.float64Array$1 = null;
  this.areTypedArraysBigEndian$1 = false;
  this.highOffset$1 = 0;
  this.lowOffset$1 = 0
}
$c_sjsr_Bits$.prototype = new $h_O();
$c_sjsr_Bits$.prototype.constructor = $c_sjsr_Bits$;
/** @constructor */
function $h_sjsr_Bits$() {
  /*<skip>*/
}
$h_sjsr_Bits$.prototype = $c_sjsr_Bits$.prototype;
$c_sjsr_Bits$.prototype.init___ = (function() {
  $n_sjsr_Bits$ = this;
  var x = ((($g.ArrayBuffer && $g.Int32Array) && $g.Float32Array) && $g.Float64Array);
  this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f = $uZ((!(!x)));
  this.arrayBuffer$1 = (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f ? new $g.ArrayBuffer(8) : null);
  this.int32Array$1 = (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f ? new $g.Int32Array(this.arrayBuffer$1, 0, 2) : null);
  this.float32Array$1 = (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f ? new $g.Float32Array(this.arrayBuffer$1, 0, 2) : null);
  this.float64Array$1 = (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f ? new $g.Float64Array(this.arrayBuffer$1, 0, 1) : null);
  if ((!this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f)) {
    var jsx$1 = true
  } else {
    this.int32Array$1[0] = 16909060;
    var jsx$1 = ($uB(new $g.Int8Array(this.arrayBuffer$1, 0, 8)[0]) === 1)
  };
  this.areTypedArraysBigEndian$1 = jsx$1;
  this.highOffset$1 = (this.areTypedArraysBigEndian$1 ? 0 : 1);
  this.lowOffset$1 = (this.areTypedArraysBigEndian$1 ? 1 : 0);
  return this
});
$c_sjsr_Bits$.prototype.numberHashCode__D__I = (function(value) {
  var iv = $uI((value | 0));
  if (((iv === value) && ((1.0 / value) !== (-Infinity)))) {
    return iv
  } else {
    var t = this.doubleToLongBits__D__J(value);
    var lo = t.lo$2;
    var hi = t.hi$2;
    return (lo ^ hi)
  }
});
$c_sjsr_Bits$.prototype.doubleToLongBitsPolyfill__p1__D__J = (function(value) {
  if ((value !== value)) {
    var _3 = $uD($g.Math.pow(2.0, 51));
    var x1_$_$$und1$1 = false;
    var x1_$_$$und2$1 = 2047;
    var x1_$_$$und3$1 = _3
  } else if (((value === Infinity) || (value === (-Infinity)))) {
    var _1 = (value < 0);
    var x1_$_$$und1$1 = _1;
    var x1_$_$$und2$1 = 2047;
    var x1_$_$$und3$1 = 0.0
  } else if ((value === 0.0)) {
    var _1$1 = ((1 / value) === (-Infinity));
    var x1_$_$$und1$1 = _1$1;
    var x1_$_$$und2$1 = 0;
    var x1_$_$$und3$1 = 0.0
  } else {
    var s = (value < 0);
    var av = (s ? (-value) : value);
    if ((av >= $uD($g.Math.pow(2.0, (-1022))))) {
      var twoPowFbits = $uD($g.Math.pow(2.0, 52));
      var a = ($uD($g.Math.log(av)) / 0.6931471805599453);
      var x = $uD($g.Math.floor(a));
      var a$1 = $uI((x | 0));
      var e = ((a$1 < 1023) ? a$1 : 1023);
      var b = e;
      var twoPowE = $uD($g.Math.pow(2.0, b));
      if ((twoPowE > av)) {
        e = (((-1) + e) | 0);
        twoPowE = (twoPowE / 2)
      };
      var n = ((av / twoPowE) * twoPowFbits);
      var w = $uD($g.Math.floor(n));
      var f = (n - w);
      var f$1 = ((f < 0.5) ? w : ((f > 0.5) ? (1 + w) : (((w % 2) !== 0) ? (1 + w) : w)));
      if (((f$1 / twoPowFbits) >= 2)) {
        e = ((1 + e) | 0);
        f$1 = 1.0
      };
      if ((e > 1023)) {
        e = 2047;
        f$1 = 0.0
      } else {
        e = ((1023 + e) | 0);
        f$1 = (f$1 - twoPowFbits)
      };
      var _2 = e;
      var _3$1 = f$1;
      var x1_$_$$und1$1 = s;
      var x1_$_$$und2$1 = _2;
      var x1_$_$$und3$1 = _3$1
    } else {
      var n$1 = (av / $uD($g.Math.pow(2.0, (-1074))));
      var w$1 = $uD($g.Math.floor(n$1));
      var f$2 = (n$1 - w$1);
      var _3$2 = ((f$2 < 0.5) ? w$1 : ((f$2 > 0.5) ? (1 + w$1) : (((w$1 % 2) !== 0) ? (1 + w$1) : w$1)));
      var x1_$_$$und1$1 = s;
      var x1_$_$$und2$1 = 0;
      var x1_$_$$und3$1 = _3$2
    }
  };
  var s$1 = $uZ(x1_$_$$und1$1);
  var e$1 = $uI(x1_$_$$und2$1);
  var f$3 = $uD(x1_$_$$und3$1);
  var x$1 = (f$3 / 4.294967296E9);
  var hif = $uI((x$1 | 0));
  var hi = (((s$1 ? (-2147483648) : 0) | (e$1 << 20)) | hif);
  var lo = $uI((f$3 | 0));
  return new $c_sjsr_RuntimeLong().init___I__I(lo, hi)
});
$c_sjsr_Bits$.prototype.longBitsToDoublePolyfill__p1__J__D = (function(bits) {
  var lo = bits.hi$2;
  var x = bits.lo$2;
  var lo$1 = $uD((x >>> 0));
  var s = (lo < 0);
  var e = (2047 & (lo >> 20));
  var f = ((4.294967296E9 * (1048575 & lo)) + lo$1);
  if ((e === 2047)) {
    return ((f !== 0.0) ? (NaN) : (s ? (-Infinity) : Infinity))
  } else if ((e > 0)) {
    var b = (((-1023) + e) | 0);
    var x$1 = ($uD($g.Math.pow(2.0, b)) * (1 + (f / $uD($g.Math.pow(2.0, 52)))));
    return (s ? (-x$1) : x$1)
  } else if ((f !== 0.0)) {
    var x$2 = ($uD($g.Math.pow(2.0, (-1022))) * (f / $uD($g.Math.pow(2.0, 52))));
    return (s ? (-x$2) : x$2)
  } else {
    return (s ? (-0) : 0.0)
  }
});
$c_sjsr_Bits$.prototype.longBitsToDouble__J__D = (function(bits) {
  if (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f) {
    var jsx$2 = this.int32Array$1;
    var jsx$1 = this.highOffset$1;
    var lo = bits.hi$2;
    jsx$2[jsx$1] = lo;
    this.int32Array$1[this.lowOffset$1] = bits.lo$2;
    return $uD(this.float64Array$1[0])
  } else {
    return this.longBitsToDoublePolyfill__p1__J__D(bits)
  }
});
$c_sjsr_Bits$.prototype.doubleToLongBits__D__J = (function(value) {
  if (this.scala$scalajs$runtime$Bits$$$undareTypedArraysSupported$f) {
    this.float64Array$1[0] = value;
    var value$1 = $uI(this.int32Array$1[this.highOffset$1]);
    var value$2 = $uI(this.int32Array$1[this.lowOffset$1]);
    return new $c_sjsr_RuntimeLong().init___I__I(value$2, value$1)
  } else {
    return this.doubleToLongBitsPolyfill__p1__D__J(value)
  }
});
var $d_sjsr_Bits$ = new $TypeData().initClass({
  sjsr_Bits$: 0
}, false, "scala.scalajs.runtime.Bits$", {
  sjsr_Bits$: 1,
  O: 1
});
$c_sjsr_Bits$.prototype.$classData = $d_sjsr_Bits$;
var $n_sjsr_Bits$ = (void 0);
function $m_sjsr_Bits$() {
  if ((!$n_sjsr_Bits$)) {
    $n_sjsr_Bits$ = new $c_sjsr_Bits$().init___()
  };
  return $n_sjsr_Bits$
}
/** @constructor */
function $c_sjsr_RuntimeString$() {
  $c_O.call(this);
  this.CASE$undINSENSITIVE$undORDER$1 = null;
  this.bitmap$0$1 = false
}
$c_sjsr_RuntimeString$.prototype = new $h_O();
$c_sjsr_RuntimeString$.prototype.constructor = $c_sjsr_RuntimeString$;
/** @constructor */
function $h_sjsr_RuntimeString$() {
  /*<skip>*/
}
$h_sjsr_RuntimeString$.prototype = $c_sjsr_RuntimeString$.prototype;
$c_sjsr_RuntimeString$.prototype.init___ = (function() {
  return this
});
$c_sjsr_RuntimeString$.prototype.indexOf__T__I__I__I = (function(thiz, ch, fromIndex) {
  var str = this.fromCodePoint__p1__I__T(ch);
  return $uI(thiz.indexOf(str, fromIndex))
});
$c_sjsr_RuntimeString$.prototype.valueOf__O__T = (function(value) {
  return ((value === null) ? "null" : $objectToString(value))
});
$c_sjsr_RuntimeString$.prototype.toCharArray__T__AC = (function(thiz) {
  var length = $uI(thiz.length);
  var result = $newArrayObject($d_C.getArrayOf(), [length]);
  var i = 0;
  while ((i < length)) {
    var jsx$1 = i;
    var index = i;
    result.set(jsx$1, (65535 & $uI(thiz.charCodeAt(index))));
    i = ((1 + i) | 0)
  };
  return result
});
$c_sjsr_RuntimeString$.prototype.indexOf__T__I__I = (function(thiz, ch) {
  var str = this.fromCodePoint__p1__I__T(ch);
  return $uI(thiz.indexOf(str))
});
$c_sjsr_RuntimeString$.prototype.newString__AC__I__I__T = (function(value, offset, count) {
  var end = ((offset + count) | 0);
  if ((((offset < 0) || (end < offset)) || (end > value.u.length))) {
    throw new $c_jl_StringIndexOutOfBoundsException().init___()
  };
  var result = "";
  var i = offset;
  while ((i !== end)) {
    var jsx$1 = result;
    var c = value.get(i);
    result = (("" + jsx$1) + $as_T($g.String.fromCharCode(c)));
    i = ((1 + i) | 0)
  };
  return result
});
$c_sjsr_RuntimeString$.prototype.fromCodePoint__p1__I__T = (function(codePoint) {
  if ((((-65536) & codePoint) === 0)) {
    return $as_T($g.String.fromCharCode(codePoint))
  } else if (((codePoint < 0) || (codePoint > 1114111))) {
    throw new $c_jl_IllegalArgumentException().init___()
  } else {
    var offsetCp = (((-65536) + codePoint) | 0);
    return $as_T($g.String.fromCharCode((55296 | (offsetCp >> 10)), (56320 | (1023 & offsetCp))))
  }
});
$c_sjsr_RuntimeString$.prototype.hashCode__T__I = (function(thiz) {
  var res = 0;
  var mul = 1;
  var i = (((-1) + $uI(thiz.length)) | 0);
  while ((i >= 0)) {
    var jsx$1 = res;
    var index = i;
    res = ((jsx$1 + $imul((65535 & $uI(thiz.charCodeAt(index))), mul)) | 0);
    mul = $imul(31, mul);
    i = (((-1) + i) | 0)
  };
  return res
});
var $d_sjsr_RuntimeString$ = new $TypeData().initClass({
  sjsr_RuntimeString$: 0
}, false, "scala.scalajs.runtime.RuntimeString$", {
  sjsr_RuntimeString$: 1,
  O: 1
});
$c_sjsr_RuntimeString$.prototype.$classData = $d_sjsr_RuntimeString$;
var $n_sjsr_RuntimeString$ = (void 0);
function $m_sjsr_RuntimeString$() {
  if ((!$n_sjsr_RuntimeString$)) {
    $n_sjsr_RuntimeString$ = new $c_sjsr_RuntimeString$().init___()
  };
  return $n_sjsr_RuntimeString$
}
/** @constructor */
function $c_sjsr_package$() {
  $c_O.call(this)
}
$c_sjsr_package$.prototype = new $h_O();
$c_sjsr_package$.prototype.constructor = $c_sjsr_package$;
/** @constructor */
function $h_sjsr_package$() {
  /*<skip>*/
}
$h_sjsr_package$.prototype = $c_sjsr_package$.prototype;
$c_sjsr_package$.prototype.init___ = (function() {
  return this
});
$c_sjsr_package$.prototype.unwrapJavaScriptException__jl_Throwable__O = (function(th) {
  if ($is_sjs_js_JavaScriptException(th)) {
    var x2 = $as_sjs_js_JavaScriptException(th);
    var e = x2.exception$4;
    return e
  } else {
    return th
  }
});
$c_sjsr_package$.prototype.wrapJavaScriptException__O__jl_Throwable = (function(e) {
  if ($is_jl_Throwable(e)) {
    var x2 = $as_jl_Throwable(e);
    return x2
  } else {
    return new $c_sjs_js_JavaScriptException().init___O(e)
  }
});
var $d_sjsr_package$ = new $TypeData().initClass({
  sjsr_package$: 0
}, false, "scala.scalajs.runtime.package$", {
  sjsr_package$: 1,
  O: 1
});
$c_sjsr_package$.prototype.$classData = $d_sjsr_package$;
var $n_sjsr_package$ = (void 0);
function $m_sjsr_package$() {
  if ((!$n_sjsr_package$)) {
    $n_sjsr_package$ = new $c_sjsr_package$().init___()
  };
  return $n_sjsr_package$
}
/** @constructor */
function $c_sr_BoxesRunTime$() {
  $c_O.call(this)
}
$c_sr_BoxesRunTime$.prototype = new $h_O();
$c_sr_BoxesRunTime$.prototype.constructor = $c_sr_BoxesRunTime$;
/** @constructor */
function $h_sr_BoxesRunTime$() {
  /*<skip>*/
}
$h_sr_BoxesRunTime$.prototype = $c_sr_BoxesRunTime$.prototype;
$c_sr_BoxesRunTime$.prototype.init___ = (function() {
  return this
});
$c_sr_BoxesRunTime$.prototype.equalsCharObject__jl_Character__O__Z = (function(xc, y) {
  if ($is_jl_Character(y)) {
    var x2 = $as_jl_Character(y);
    return (xc.value$1 === x2.value$1)
  } else if ($is_jl_Number(y)) {
    var x3 = $as_jl_Number(y);
    if (((typeof x3) === "number")) {
      var x2$1 = $uD(x3);
      return (x2$1 === xc.value$1)
    } else if ($is_sjsr_RuntimeLong(x3)) {
      var t = $uJ(x3);
      var lo = t.lo$2;
      var hi = t.hi$2;
      var value = xc.value$1;
      var hi$1 = (value >> 31);
      return ((lo === value) && (hi === hi$1))
    } else {
      return ((x3 === null) ? (xc === null) : $objectEquals(x3, xc))
    }
  } else {
    return ((xc === null) && (y === null))
  }
});
$c_sr_BoxesRunTime$.prototype.equalsNumObject__jl_Number__O__Z = (function(xn, y) {
  if ($is_jl_Number(y)) {
    var x2 = $as_jl_Number(y);
    return this.equalsNumNum__jl_Number__jl_Number__Z(xn, x2)
  } else if ($is_jl_Character(y)) {
    var x3 = $as_jl_Character(y);
    if (((typeof xn) === "number")) {
      var x2$1 = $uD(xn);
      return (x2$1 === x3.value$1)
    } else if ($is_sjsr_RuntimeLong(xn)) {
      var t = $uJ(xn);
      var lo = t.lo$2;
      var hi = t.hi$2;
      var value = x3.value$1;
      var hi$1 = (value >> 31);
      return ((lo === value) && (hi === hi$1))
    } else {
      return ((xn === null) ? (x3 === null) : $objectEquals(xn, x3))
    }
  } else {
    return ((xn === null) ? (y === null) : $objectEquals(xn, y))
  }
});
$c_sr_BoxesRunTime$.prototype.equals__O__O__Z = (function(x, y) {
  if ((x === y)) {
    return true
  } else if ($is_jl_Number(x)) {
    var x2 = $as_jl_Number(x);
    return this.equalsNumObject__jl_Number__O__Z(x2, y)
  } else if ($is_jl_Character(x)) {
    var x3 = $as_jl_Character(x);
    return this.equalsCharObject__jl_Character__O__Z(x3, y)
  } else {
    return ((x === null) ? (y === null) : $objectEquals(x, y))
  }
});
$c_sr_BoxesRunTime$.prototype.equalsNumNum__jl_Number__jl_Number__Z = (function(xn, yn) {
  if (((typeof xn) === "number")) {
    var x2 = $uD(xn);
    if (((typeof yn) === "number")) {
      var x2$2 = $uD(yn);
      return (x2 === x2$2)
    } else if ($is_sjsr_RuntimeLong(yn)) {
      var t = $uJ(yn);
      var lo = t.lo$2;
      var hi = t.hi$2;
      return (x2 === $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(lo, hi))
    } else if ($is_s_math_ScalaNumber(yn)) {
      var x4 = $as_s_math_ScalaNumber(yn);
      return x4.equals__O__Z(x2)
    } else {
      return false
    }
  } else if ($is_sjsr_RuntimeLong(xn)) {
    var t$1 = $uJ(xn);
    var lo$1 = t$1.lo$2;
    var hi$1 = t$1.hi$2;
    if ($is_sjsr_RuntimeLong(yn)) {
      var t$2 = $uJ(yn);
      var lo$2 = t$2.lo$2;
      var hi$2 = t$2.hi$2;
      return ((lo$1 === lo$2) && (hi$1 === hi$2))
    } else if (((typeof yn) === "number")) {
      var x3$3 = $uD(yn);
      return ($m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(lo$1, hi$1) === x3$3)
    } else if ($is_s_math_ScalaNumber(yn)) {
      var x4$2 = $as_s_math_ScalaNumber(yn);
      return x4$2.equals__O__Z(new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$1))
    } else {
      return false
    }
  } else {
    return ((xn === null) ? (yn === null) : $objectEquals(xn, yn))
  }
});
var $d_sr_BoxesRunTime$ = new $TypeData().initClass({
  sr_BoxesRunTime$: 0
}, false, "scala.runtime.BoxesRunTime$", {
  sr_BoxesRunTime$: 1,
  O: 1
});
$c_sr_BoxesRunTime$.prototype.$classData = $d_sr_BoxesRunTime$;
var $n_sr_BoxesRunTime$ = (void 0);
function $m_sr_BoxesRunTime$() {
  if ((!$n_sr_BoxesRunTime$)) {
    $n_sr_BoxesRunTime$ = new $c_sr_BoxesRunTime$().init___()
  };
  return $n_sr_BoxesRunTime$
}
var $d_sr_Null$ = new $TypeData().initClass({
  sr_Null$: 0
}, false, "scala.runtime.Null$", {
  sr_Null$: 1,
  O: 1
});
/** @constructor */
function $c_sr_ScalaRunTime$() {
  $c_O.call(this)
}
$c_sr_ScalaRunTime$.prototype = new $h_O();
$c_sr_ScalaRunTime$.prototype.constructor = $c_sr_ScalaRunTime$;
/** @constructor */
function $h_sr_ScalaRunTime$() {
  /*<skip>*/
}
$h_sr_ScalaRunTime$.prototype = $c_sr_ScalaRunTime$.prototype;
$c_sr_ScalaRunTime$.prototype.init___ = (function() {
  return this
});
$c_sr_ScalaRunTime$.prototype.$$undtoString__s_Product__T = (function(x) {
  var this$1 = x.productIterator__sc_Iterator();
  var start = (x.productPrefix__T() + "(");
  return $f_sc_TraversableOnce__mkString__T__T__T__T(this$1, start, ",", ")")
});
var $d_sr_ScalaRunTime$ = new $TypeData().initClass({
  sr_ScalaRunTime$: 0
}, false, "scala.runtime.ScalaRunTime$", {
  sr_ScalaRunTime$: 1,
  O: 1
});
$c_sr_ScalaRunTime$.prototype.$classData = $d_sr_ScalaRunTime$;
var $n_sr_ScalaRunTime$ = (void 0);
function $m_sr_ScalaRunTime$() {
  if ((!$n_sr_ScalaRunTime$)) {
    $n_sr_ScalaRunTime$ = new $c_sr_ScalaRunTime$().init___()
  };
  return $n_sr_ScalaRunTime$
}
/** @constructor */
function $c_sr_Statics$() {
  $c_O.call(this)
}
$c_sr_Statics$.prototype = new $h_O();
$c_sr_Statics$.prototype.constructor = $c_sr_Statics$;
/** @constructor */
function $h_sr_Statics$() {
  /*<skip>*/
}
$h_sr_Statics$.prototype = $c_sr_Statics$.prototype;
$c_sr_Statics$.prototype.init___ = (function() {
  return this
});
$c_sr_Statics$.prototype.doubleHash__D__I = (function(dv) {
  var iv = $doubleToInt(dv);
  if ((iv === dv)) {
    return iv
  } else {
    var this$1 = $m_sjsr_RuntimeLong$();
    var lo = this$1.scala$scalajs$runtime$RuntimeLong$$fromDoubleImpl__D__I(dv);
    var hi = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
    return (($m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(lo, hi) === dv) ? (lo ^ hi) : $m_sjsr_Bits$().numberHashCode__D__I(dv))
  }
});
$c_sr_Statics$.prototype.anyHash__O__I = (function(x) {
  if ((x === null)) {
    return 0
  } else if (((typeof x) === "number")) {
    var x3 = $uD(x);
    return this.doubleHash__D__I(x3)
  } else if ($is_sjsr_RuntimeLong(x)) {
    var t = $uJ(x);
    var lo = t.lo$2;
    var hi = t.hi$2;
    return this.longHash__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi))
  } else {
    return $objectHashCode(x)
  }
});
$c_sr_Statics$.prototype.longHash__J__I = (function(lv) {
  var lo = lv.lo$2;
  var lo$1 = lv.hi$2;
  return ((lo$1 === (lo >> 31)) ? lo : (lo ^ lo$1))
});
var $d_sr_Statics$ = new $TypeData().initClass({
  sr_Statics$: 0
}, false, "scala.runtime.Statics$", {
  sr_Statics$: 1,
  O: 1
});
$c_sr_Statics$.prototype.$classData = $d_sr_Statics$;
var $n_sr_Statics$ = (void 0);
function $m_sr_Statics$() {
  if ((!$n_sr_Statics$)) {
    $n_sr_Statics$ = new $c_sr_Statics$().init___()
  };
  return $n_sr_Statics$
}
/** @constructor */
function $c_LLoan$() {
  $c_O.call(this)
}
$c_LLoan$.prototype = new $h_O();
$c_LLoan$.prototype.constructor = $c_LLoan$;
/** @constructor */
function $h_LLoan$() {
  /*<skip>*/
}
$h_LLoan$.prototype = $c_LLoan$.prototype;
$c_LLoan$.prototype.init___ = (function() {
  return this
});
$c_LLoan$.prototype.fromJSON__T__LLoan = (function(json) {
  var parsed = $g.JSON.parse(json);
  var this$2 = $m_s_Console$();
  var this$3 = $as_Ljava_io_PrintStream(this$2.outVar$2.v$1);
  this$3.java$lang$JSConsoleBasedPrintStream$$printString__T__V((parsed + "n"));
  var jsx$9 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.principal));
  var jsx$8 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.accruedInterest));
  var jsx$7 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.paymentInterval));
  var this$4 = $m_s_package$().BigDecimal__s_math_BigDecimal$();
  var x = $as_T(parsed.interestRate);
  var jsx$6 = this$4.exact__T__s_math_BigDecimal(x);
  var jsx$5 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.interestInterval));
  var jsx$4 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.startingDate));
  var jsx$3 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(parsed.totalPaid));
  var value = parsed.lastPayment;
  var this$10 = ((value === (void 0)) ? $m_s_None$() : new $c_s_Some().init___O(value));
  if (this$10.isEmpty__Z()) {
    var jsx$2 = $m_s_None$()
  } else {
    var arg1 = this$10.get__O();
    var self = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(arg1[0]));
    var y = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(arg1[1]));
    var jsx$2 = new $c_s_Some().init___O(new $c_T2().init___O__O(self, y))
  };
  var value$1 = parsed.nextPayment;
  var this$18 = ((value$1 === (void 0)) ? $m_s_None$() : new $c_s_Some().init___O(value$1));
  if (this$18.isEmpty__Z()) {
    var jsx$1 = $m_s_None$()
  } else {
    var arg1$1 = this$18.get__O();
    var self$1 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(arg1$1[0]));
    var y$1 = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt($as_T(arg1$1[1]));
    var jsx$1 = new $c_s_Some().init___O(new $c_T2().init___O__O(self$1, y$1))
  };
  return new ($a_LLoan())(jsx$9, jsx$8, jsx$7, jsx$6, jsx$5, jsx$4, jsx$3, jsx$2, jsx$1)
});
$c_LLoan$.prototype.apply__sjs_js_Object__LLoan = (function(args) {
  return this.fromJSON__T__LLoan($as_T($g.JSON.stringify(args)))
});
$c_LLoan$.prototype.toJSON__LLoan__T = (function(l) {
  return $as_T($g.JSON.stringify(l))
});
var $d_LLoan$ = new $TypeData().initClass({
  LLoan$: 0
}, false, "Loan$", {
  LLoan$: 1,
  O: 1,
  LJSJsonSerializer: 1
});
$c_LLoan$.prototype.$classData = $d_LLoan$;
var $n_LLoan$ = (void 0);
function $m_LLoan$() {
  if ((!$n_LLoan$)) {
    $n_LLoan$ = new $c_LLoan$().init___()
  };
  return $n_LLoan$
}
/** @constructor */
function $c_jl_Number() {
  $c_O.call(this)
}
$c_jl_Number.prototype = new $h_O();
$c_jl_Number.prototype.constructor = $c_jl_Number;
/** @constructor */
function $h_jl_Number() {
  /*<skip>*/
}
$h_jl_Number.prototype = $c_jl_Number.prototype;
function $is_jl_Number(obj) {
  return (!(!(((obj && obj.$classData) && obj.$classData.ancestors.jl_Number) || ((typeof obj) === "number"))))
}
function $as_jl_Number(obj) {
  return (($is_jl_Number(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Number"))
}
function $isArrayOf_jl_Number(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Number)))
}
function $asArrayOf_jl_Number(obj, depth) {
  return (($isArrayOf_jl_Number(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Number;", depth))
}
/** @constructor */
function $c_jl_Throwable() {
  $c_O.call(this);
  this.s$1 = null;
  this.e$1 = null;
  this.stackTrace$1 = null
}
$c_jl_Throwable.prototype = new $h_O();
$c_jl_Throwable.prototype.constructor = $c_jl_Throwable;
/** @constructor */
function $h_jl_Throwable() {
  /*<skip>*/
}
$h_jl_Throwable.prototype = $c_jl_Throwable.prototype;
$c_jl_Throwable.prototype.fillInStackTrace__jl_Throwable = (function() {
  var v = $g.Error.captureStackTrace;
  if ((v === (void 0))) {
    try {
      var e$1 = {}.undef()
    } catch (e) {
      var e$2 = $m_sjsr_package$().wrapJavaScriptException__O__jl_Throwable(e);
      if ((e$2 !== null)) {
        if ($is_sjs_js_JavaScriptException(e$2)) {
          var x5 = $as_sjs_js_JavaScriptException(e$2);
          var e$3 = x5.exception$4;
          var e$1 = e$3
        } else {
          var e$1;
          throw $m_sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(e$2)
        }
      } else {
        var e$1;
        throw e
      }
    };
    this.stackdata = e$1
  } else {
    $g.Error.captureStackTrace(this);
    this.stackdata = this
  };
  return this
});
$c_jl_Throwable.prototype.getMessage__T = (function() {
  return this.s$1
});
$c_jl_Throwable.prototype.toString__T = (function() {
  var className = $objectGetClass(this).getName__T();
  var message = this.getMessage__T();
  return ((message === null) ? className : ((className + ": ") + message))
});
$c_jl_Throwable.prototype.init___T__jl_Throwable = (function(s, e) {
  this.s$1 = s;
  this.e$1 = e;
  this.fillInStackTrace__jl_Throwable();
  return this
});
function $is_jl_Throwable(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_Throwable)))
}
function $as_jl_Throwable(obj) {
  return (($is_jl_Throwable(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Throwable"))
}
function $isArrayOf_jl_Throwable(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Throwable)))
}
function $asArrayOf_jl_Throwable(obj, depth) {
  return (($isArrayOf_jl_Throwable(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Throwable;", depth))
}
/** @constructor */
function $c_s_Predef$$anon$3() {
  $c_O.call(this)
}
$c_s_Predef$$anon$3.prototype = new $h_O();
$c_s_Predef$$anon$3.prototype.constructor = $c_s_Predef$$anon$3;
/** @constructor */
function $h_s_Predef$$anon$3() {
  /*<skip>*/
}
$h_s_Predef$$anon$3.prototype = $c_s_Predef$$anon$3.prototype;
$c_s_Predef$$anon$3.prototype.init___ = (function() {
  return this
});
var $d_s_Predef$$anon$3 = new $TypeData().initClass({
  s_Predef$$anon$3: 0
}, false, "scala.Predef$$anon$3", {
  s_Predef$$anon$3: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
$c_s_Predef$$anon$3.prototype.$classData = $d_s_Predef$$anon$3;
/** @constructor */
function $c_s_Predef$ArrayCharSequence() {
  $c_O.call(this);
  this.$$und$undarrayOfChars$1 = null
}
$c_s_Predef$ArrayCharSequence.prototype = new $h_O();
$c_s_Predef$ArrayCharSequence.prototype.constructor = $c_s_Predef$ArrayCharSequence;
/** @constructor */
function $h_s_Predef$ArrayCharSequence() {
  /*<skip>*/
}
$h_s_Predef$ArrayCharSequence.prototype = $c_s_Predef$ArrayCharSequence.prototype;
$c_s_Predef$ArrayCharSequence.prototype.subSequence__I__I__jl_CharSequence = (function(start, end) {
  return new $c_sr_ArrayCharSequence().init___AC__I__I(this.$$und$undarrayOfChars$1, start, end)
});
$c_s_Predef$ArrayCharSequence.prototype.toString__T = (function() {
  var xs = this.$$und$undarrayOfChars$1;
  var b = new $c_scm_StringBuilder().init___();
  var elem$1 = false;
  elem$1 = true;
  b.append__T__scm_StringBuilder("");
  var i = 0;
  var len = xs.u.length;
  while ((i < len)) {
    var idx = i;
    var c = xs.get(idx);
    var arg1 = new $c_jl_Character().init___C(c);
    if (elem$1) {
      b.append__O__scm_StringBuilder(arg1);
      elem$1 = false
    } else {
      b.append__T__scm_StringBuilder("");
      b.append__O__scm_StringBuilder(arg1)
    };
    i = ((1 + i) | 0)
  };
  b.append__T__scm_StringBuilder("");
  var this$7 = b.underlying$5;
  return this$7.content$1
});
$c_s_Predef$ArrayCharSequence.prototype.init___AC = (function(__arrayOfChars) {
  this.$$und$undarrayOfChars$1 = __arrayOfChars;
  return this
});
var $d_s_Predef$ArrayCharSequence = new $TypeData().initClass({
  s_Predef$ArrayCharSequence: 0
}, false, "scala.Predef$ArrayCharSequence", {
  s_Predef$ArrayCharSequence: 1,
  O: 1,
  jl_CharSequence: 1
});
$c_s_Predef$ArrayCharSequence.prototype.$classData = $d_s_Predef$ArrayCharSequence;
function $f_s_Product2__productElement__I__O($thiz, n) {
  switch (n) {
    case 0: {
      return $thiz.$$und1__O();
      break
    }
    case 1: {
      return $thiz.$$und2__O();
      break
    }
    default: {
      throw new $c_jl_IndexOutOfBoundsException().init___T(("" + n))
    }
  }
}
/** @constructor */
function $c_s_package$$anon$1() {
  $c_O.call(this)
}
$c_s_package$$anon$1.prototype = new $h_O();
$c_s_package$$anon$1.prototype.constructor = $c_s_package$$anon$1;
/** @constructor */
function $h_s_package$$anon$1() {
  /*<skip>*/
}
$h_s_package$$anon$1.prototype = $c_s_package$$anon$1.prototype;
$c_s_package$$anon$1.prototype.init___ = (function() {
  return this
});
$c_s_package$$anon$1.prototype.toString__T = (function() {
  return "object AnyRef"
});
var $d_s_package$$anon$1 = new $TypeData().initClass({
  s_package$$anon$1: 0
}, false, "scala.package$$anon$1", {
  s_package$$anon$1: 1,
  O: 1,
  s_Specializable: 1
});
$c_s_package$$anon$1.prototype.$classData = $d_s_package$$anon$1;
/** @constructor */
function $c_s_util_hashing_MurmurHash3$() {
  $c_s_util_hashing_MurmurHash3.call(this);
  this.seqSeed$2 = 0;
  this.mapSeed$2 = 0;
  this.setSeed$2 = 0
}
$c_s_util_hashing_MurmurHash3$.prototype = new $h_s_util_hashing_MurmurHash3();
$c_s_util_hashing_MurmurHash3$.prototype.constructor = $c_s_util_hashing_MurmurHash3$;
/** @constructor */
function $h_s_util_hashing_MurmurHash3$() {
  /*<skip>*/
}
$h_s_util_hashing_MurmurHash3$.prototype = $c_s_util_hashing_MurmurHash3$.prototype;
$c_s_util_hashing_MurmurHash3$.prototype.init___ = (function() {
  $n_s_util_hashing_MurmurHash3$ = this;
  this.seqSeed$2 = $m_sjsr_RuntimeString$().hashCode__T__I("Seq");
  this.mapSeed$2 = $m_sjsr_RuntimeString$().hashCode__T__I("Map");
  this.setSeed$2 = $m_sjsr_RuntimeString$().hashCode__T__I("Set");
  return this
});
$c_s_util_hashing_MurmurHash3$.prototype.seqHash__sc_Seq__I = (function(xs) {
  if ($is_sci_List(xs)) {
    var x2 = $as_sci_List(xs);
    return this.listHash__sci_List__I__I(x2, this.seqSeed$2)
  } else {
    return this.orderedHash__sc_TraversableOnce__I__I(xs, this.seqSeed$2)
  }
});
var $d_s_util_hashing_MurmurHash3$ = new $TypeData().initClass({
  s_util_hashing_MurmurHash3$: 0
}, false, "scala.util.hashing.MurmurHash3$", {
  s_util_hashing_MurmurHash3$: 1,
  s_util_hashing_MurmurHash3: 1,
  O: 1
});
$c_s_util_hashing_MurmurHash3$.prototype.$classData = $d_s_util_hashing_MurmurHash3$;
var $n_s_util_hashing_MurmurHash3$ = (void 0);
function $m_s_util_hashing_MurmurHash3$() {
  if ((!$n_s_util_hashing_MurmurHash3$)) {
    $n_s_util_hashing_MurmurHash3$ = new $c_s_util_hashing_MurmurHash3$().init___()
  };
  return $n_s_util_hashing_MurmurHash3$
}
function $f_sc_Iterator__isEmpty__Z($thiz) {
  return (!$thiz.hasNext__Z())
}
function $f_sc_Iterator__toString__T($thiz) {
  return (($thiz.hasNext__Z() ? "non-empty" : "empty") + " iterator")
}
function $f_sc_Iterator__foreach__F1__V($thiz, f) {
  while ($thiz.hasNext__Z()) {
    f.apply__O__O($thiz.next__O())
  }
}
/** @constructor */
function $c_scg_GenSetFactory() {
  $c_scg_GenericCompanion.call(this)
}
$c_scg_GenSetFactory.prototype = new $h_scg_GenericCompanion();
$c_scg_GenSetFactory.prototype.constructor = $c_scg_GenSetFactory;
/** @constructor */
function $h_scg_GenSetFactory() {
  /*<skip>*/
}
$h_scg_GenSetFactory.prototype = $c_scg_GenSetFactory.prototype;
/** @constructor */
function $c_scg_GenTraversableFactory() {
  $c_scg_GenericCompanion.call(this);
  this.ReusableCBFInstance$2 = null
}
$c_scg_GenTraversableFactory.prototype = new $h_scg_GenericCompanion();
$c_scg_GenTraversableFactory.prototype.constructor = $c_scg_GenTraversableFactory;
/** @constructor */
function $h_scg_GenTraversableFactory() {
  /*<skip>*/
}
$h_scg_GenTraversableFactory.prototype = $c_scg_GenTraversableFactory.prototype;
$c_scg_GenTraversableFactory.prototype.init___ = (function() {
  this.ReusableCBFInstance$2 = new $c_scg_GenTraversableFactory$$anon$1().init___scg_GenTraversableFactory(this);
  return this
});
/** @constructor */
function $c_scg_GenTraversableFactory$GenericCanBuildFrom() {
  $c_O.call(this);
  this.$$outer$1 = null
}
$c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype = new $h_O();
$c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype.constructor = $c_scg_GenTraversableFactory$GenericCanBuildFrom;
/** @constructor */
function $h_scg_GenTraversableFactory$GenericCanBuildFrom() {
  /*<skip>*/
}
$h_scg_GenTraversableFactory$GenericCanBuildFrom.prototype = $c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype;
$c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory = (function($$outer) {
  if (($$outer === null)) {
    throw $m_sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$1 = $$outer
  };
  return this
});
/** @constructor */
function $c_scg_MapFactory() {
  $c_scg_GenMapFactory.call(this)
}
$c_scg_MapFactory.prototype = new $h_scg_GenMapFactory();
$c_scg_MapFactory.prototype.constructor = $c_scg_MapFactory;
/** @constructor */
function $h_scg_MapFactory() {
  /*<skip>*/
}
$h_scg_MapFactory.prototype = $c_scg_MapFactory.prototype;
/** @constructor */
function $c_sci_List$$anon$1() {
  $c_O.call(this)
}
$c_sci_List$$anon$1.prototype = new $h_O();
$c_sci_List$$anon$1.prototype.constructor = $c_sci_List$$anon$1;
/** @constructor */
function $h_sci_List$$anon$1() {
  /*<skip>*/
}
$h_sci_List$$anon$1.prototype = $c_sci_List$$anon$1.prototype;
$c_sci_List$$anon$1.prototype.init___ = (function() {
  return this
});
$c_sci_List$$anon$1.prototype.apply__O__O = (function(x) {
  return this
});
$c_sci_List$$anon$1.prototype.toString__T = (function() {
  return "<function1>"
});
var $d_sci_List$$anon$1 = new $TypeData().initClass({
  sci_List$$anon$1: 0
}, false, "scala.collection.immutable.List$$anon$1", {
  sci_List$$anon$1: 1,
  O: 1,
  F1: 1
});
$c_sci_List$$anon$1.prototype.$classData = $d_sci_List$$anon$1;
/** @constructor */
function $c_sr_AbstractFunction1() {
  $c_O.call(this)
}
$c_sr_AbstractFunction1.prototype = new $h_O();
$c_sr_AbstractFunction1.prototype.constructor = $c_sr_AbstractFunction1;
/** @constructor */
function $h_sr_AbstractFunction1() {
  /*<skip>*/
}
$h_sr_AbstractFunction1.prototype = $c_sr_AbstractFunction1.prototype;
$c_sr_AbstractFunction1.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
function $c_sr_ArrayCharSequence() {
  $c_O.call(this);
  this.xs$1 = null;
  this.start$1 = 0;
  this.end$1 = 0
}
$c_sr_ArrayCharSequence.prototype = new $h_O();
$c_sr_ArrayCharSequence.prototype.constructor = $c_sr_ArrayCharSequence;
/** @constructor */
function $h_sr_ArrayCharSequence() {
  /*<skip>*/
}
$h_sr_ArrayCharSequence.prototype = $c_sr_ArrayCharSequence.prototype;
$c_sr_ArrayCharSequence.prototype.subSequence__I__I__jl_CharSequence = (function(start0, end0) {
  if ((start0 < 0)) {
    throw new $c_jl_ArrayIndexOutOfBoundsException().init___I(start0)
  } else if ((end0 > this.length__I())) {
    throw new $c_jl_ArrayIndexOutOfBoundsException().init___I(end0)
  } else if ((end0 <= start0)) {
    return new $c_sr_ArrayCharSequence().init___AC__I__I(this.xs$1, 0, 0)
  } else {
    var newlen = ((end0 - start0) | 0);
    var start1 = ((this.start$1 + start0) | 0);
    return new $c_sr_ArrayCharSequence().init___AC__I__I(this.xs$1, start1, ((start1 + newlen) | 0))
  }
});
$c_sr_ArrayCharSequence.prototype.toString__T = (function() {
  var x = this.start$1;
  var start = ((x > 0) ? x : 0);
  var x$1 = this.xs$1.u.length;
  var y = ((start + this.length__I()) | 0);
  var end = ((x$1 < y) ? x$1 : y);
  return ((start >= end) ? "" : $m_sjsr_RuntimeString$().newString__AC__I__I__T(this.xs$1, start, ((end - start) | 0)))
});
$c_sr_ArrayCharSequence.prototype.length__I = (function() {
  var y = ((this.end$1 - this.start$1) | 0);
  return ((y < 0) ? 0 : y)
});
$c_sr_ArrayCharSequence.prototype.init___AC__I__I = (function(xs, start, end) {
  this.xs$1 = xs;
  this.start$1 = start;
  this.end$1 = end;
  return this
});
var $d_sr_ArrayCharSequence = new $TypeData().initClass({
  sr_ArrayCharSequence: 0
}, false, "scala.runtime.ArrayCharSequence", {
  sr_ArrayCharSequence: 1,
  O: 1,
  jl_CharSequence: 1
});
$c_sr_ArrayCharSequence.prototype.$classData = $d_sr_ArrayCharSequence;
/** @constructor */
function $c_sr_BooleanRef() {
  $c_O.call(this);
  this.elem$1 = false
}
$c_sr_BooleanRef.prototype = new $h_O();
$c_sr_BooleanRef.prototype.constructor = $c_sr_BooleanRef;
/** @constructor */
function $h_sr_BooleanRef() {
  /*<skip>*/
}
$h_sr_BooleanRef.prototype = $c_sr_BooleanRef.prototype;
$c_sr_BooleanRef.prototype.toString__T = (function() {
  var value = this.elem$1;
  return ("" + value)
});
$c_sr_BooleanRef.prototype.init___Z = (function(elem) {
  this.elem$1 = elem;
  return this
});
var $d_sr_BooleanRef = new $TypeData().initClass({
  sr_BooleanRef: 0
}, false, "scala.runtime.BooleanRef", {
  sr_BooleanRef: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_sr_BooleanRef.prototype.$classData = $d_sr_BooleanRef;
var $d_sr_BoxedUnit = new $TypeData().initClass({
  sr_BoxedUnit: 0
}, false, "scala.runtime.BoxedUnit", {
  sr_BoxedUnit: 1,
  O: 1,
  Ljava_io_Serializable: 1
}, (void 0), (void 0), (function(x) {
  return (x === (void 0))
}));
/** @constructor */
function $c_sr_IntRef() {
  $c_O.call(this);
  this.elem$1 = 0
}
$c_sr_IntRef.prototype = new $h_O();
$c_sr_IntRef.prototype.constructor = $c_sr_IntRef;
/** @constructor */
function $h_sr_IntRef() {
  /*<skip>*/
}
$h_sr_IntRef.prototype = $c_sr_IntRef.prototype;
$c_sr_IntRef.prototype.toString__T = (function() {
  var value = this.elem$1;
  return ("" + value)
});
$c_sr_IntRef.prototype.init___I = (function(elem) {
  this.elem$1 = elem;
  return this
});
var $d_sr_IntRef = new $TypeData().initClass({
  sr_IntRef: 0
}, false, "scala.runtime.IntRef", {
  sr_IntRef: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_sr_IntRef.prototype.$classData = $d_sr_IntRef;
var $b_LBaseModule = (void 0);
function $a_LBaseModule() {
  if ((!$b_LBaseModule)) {
    /** @constructor */
    var $c_LBaseModule = (function $c_LBaseModule() {
      $g.Object.call(this)
    });
    /** @constructor */
    var $h_LBaseModule = (function $h_LBaseModule() {
      /*<skip>*/
    });
    $h_LBaseModule.prototype = $g.Object.prototype;
    $c_LBaseModule.prototype = new $h_LBaseModule();
    $c_LBaseModule.prototype.constructor = $c_LBaseModule;
    $b_LBaseModule = $c_LBaseModule
  };
  return $b_LBaseModule
}
/** @constructor */
function $c_Ljava_io_OutputStream() {
  $c_O.call(this)
}
$c_Ljava_io_OutputStream.prototype = new $h_O();
$c_Ljava_io_OutputStream.prototype.constructor = $c_Ljava_io_OutputStream;
/** @constructor */
function $h_Ljava_io_OutputStream() {
  /*<skip>*/
}
$h_Ljava_io_OutputStream.prototype = $c_Ljava_io_OutputStream.prototype;
/** @constructor */
function $c_Ljava_math_BigDecimal$() {
  $c_O.call(this);
  this.ZERO$1 = null;
  this.ONE$1 = null;
  this.TEN$1 = null;
  this.java$math$BigDecimal$$LongFivePows$1 = null;
  this.java$math$BigDecimal$$LongFivePowsBitLength$1 = null;
  this.LongTenPows$1 = null;
  this.java$math$BigDecimal$$LongTenPowsBitLength$1 = null;
  this.BigIntScaledByZero$1 = null;
  this.ZeroScaledBy$1 = null;
  this.java$math$BigDecimal$$CharZeros$1 = null
}
$c_Ljava_math_BigDecimal$.prototype = new $h_O();
$c_Ljava_math_BigDecimal$.prototype.constructor = $c_Ljava_math_BigDecimal$;
/** @constructor */
function $h_Ljava_math_BigDecimal$() {
  /*<skip>*/
}
$h_Ljava_math_BigDecimal$.prototype = $c_Ljava_math_BigDecimal$.prototype;
$c_Ljava_math_BigDecimal$.prototype.valueOf__J__I__Ljava_math_BigDecimal = (function(unscaledVal, scale) {
  return ((scale === 0) ? this.valueOf__J__Ljava_math_BigDecimal(unscaledVal) : (((((unscaledVal.lo$2 === 0) && (unscaledVal.hi$2 === 0)) && (scale >= 0)) && (scale < this.ZeroScaledBy$1.u.length)) ? this.ZeroScaledBy$1.get(scale) : new $c_Ljava_math_BigDecimal().init___J__I(unscaledVal, scale)))
});
$c_Ljava_math_BigDecimal$.prototype.init___ = (function() {
  $n_Ljava_math_BigDecimal$ = this;
  this.ZERO$1 = new $c_Ljava_math_BigDecimal().init___I__I(0, 0);
  this.ONE$1 = new $c_Ljava_math_BigDecimal().init___I__I(1, 0);
  this.TEN$1 = new $c_Ljava_math_BigDecimal().init___I__I(10, 0);
  this.java$math$BigDecimal$$LongFivePows$1 = this.newArrayOfPows__I__I__AJ(28, 5);
  var n = this.java$math$BigDecimal$$LongFivePows$1.u.length;
  var elems$2 = null;
  elems$2 = [];
  var i = 0;
  while ((i < n)) {
    var arg1 = i;
    var elem = this.bitLength__J__I(this.java$math$BigDecimal$$LongFivePows$1.get(arg1));
    elems$2.push(elem);
    i = ((1 + i) | 0)
  };
  this.java$math$BigDecimal$$LongFivePowsBitLength$1 = $makeNativeArrayWrapper($d_I.getArrayOf(), elems$2);
  this.LongTenPows$1 = this.newArrayOfPows__I__I__AJ(19, 10);
  var n$1 = this.LongTenPows$1.u.length;
  var elems$2$1 = null;
  elems$2$1 = [];
  var i$1 = 0;
  while ((i$1 < n$1)) {
    var arg1$1 = i$1;
    var elem$1 = this.bitLength__J__I(this.LongTenPows$1.get(arg1$1));
    elems$2$1.push(elem$1);
    i$1 = ((1 + i$1) | 0)
  };
  this.java$math$BigDecimal$$LongTenPowsBitLength$1 = $makeNativeArrayWrapper($d_I.getArrayOf(), elems$2$1);
  var elems$2$2 = null;
  elems$2$2 = [];
  var i$2 = 0;
  while ((i$2 < 11)) {
    var arg1$2 = i$2;
    var elem$2 = new $c_Ljava_math_BigDecimal().init___I__I(arg1$2, 0);
    elems$2$2.push(elem$2);
    i$2 = ((1 + i$2) | 0)
  };
  this.BigIntScaledByZero$1 = $makeNativeArrayWrapper($d_Ljava_math_BigDecimal.getArrayOf(), elems$2$2);
  var elems$2$3 = null;
  elems$2$3 = [];
  var i$3 = 0;
  while ((i$3 < 11)) {
    var arg1$3 = i$3;
    var elem$3 = new $c_Ljava_math_BigDecimal().init___I__I(0, arg1$3);
    elems$2$3.push(elem$3);
    i$3 = ((1 + i$3) | 0)
  };
  this.ZeroScaledBy$1 = $makeNativeArrayWrapper($d_Ljava_math_BigDecimal.getArrayOf(), elems$2$3);
  var elems$2$4 = null;
  elems$2$4 = [];
  var i$4 = 0;
  while ((i$4 < 100)) {
    elems$2$4.push(48);
    i$4 = ((1 + i$4) | 0)
  };
  this.java$math$BigDecimal$$CharZeros$1 = $makeNativeArrayWrapper($d_C.getArrayOf(), elems$2$4);
  return this
});
$c_Ljava_math_BigDecimal$.prototype.java$math$BigDecimal$$longCompareTo__J__J__I = (function(value1, value2) {
  var ahi = value1.hi$2;
  var bhi = value2.hi$2;
  if (((ahi === bhi) ? (((-2147483648) ^ value1.lo$2) > ((-2147483648) ^ value2.lo$2)) : (ahi > bhi))) {
    return 1
  } else {
    var ahi$1 = value1.hi$2;
    var bhi$1 = value2.hi$2;
    if (((ahi$1 === bhi$1) ? (((-2147483648) ^ value1.lo$2) < ((-2147483648) ^ value2.lo$2)) : (ahi$1 < bhi$1))) {
      return (-1)
    } else {
      return 0
    }
  }
});
$c_Ljava_math_BigDecimal$.prototype.java$math$BigDecimal$$roundingBehavior__I__I__Ljava_math_RoundingMode__I = (function(parityBit, fraction, roundingMode) {
  var absFraction = ((fraction < 0) ? ((-fraction) | 0) : fraction);
  var sigFraction = ((fraction === 0) ? 0 : ((fraction < 0) ? (-1) : 1));
  var x = $m_Ljava_math_RoundingMode$().UP$1;
  if ((x === roundingMode)) {
    return sigFraction
  } else {
    var x$3 = $m_Ljava_math_RoundingMode$().DOWN$1;
    if ((x$3 === roundingMode)) {
      return 0
    } else {
      var x$5 = $m_Ljava_math_RoundingMode$().CEILING$1;
      if ((x$5 === roundingMode)) {
        return ((sigFraction > 0) ? sigFraction : 0)
      } else {
        var x$7 = $m_Ljava_math_RoundingMode$().FLOOR$1;
        if ((x$7 === roundingMode)) {
          return ((sigFraction < 0) ? sigFraction : 0)
        } else {
          var x$9 = $m_Ljava_math_RoundingMode$().HALF$undUP$1;
          if ((x$9 === roundingMode)) {
            return ((absFraction >= 5) ? sigFraction : 0)
          } else {
            var x$11 = $m_Ljava_math_RoundingMode$().HALF$undDOWN$1;
            if ((x$11 === roundingMode)) {
              return ((absFraction > 5) ? sigFraction : 0)
            } else {
              var x$13 = $m_Ljava_math_RoundingMode$().HALF$undEVEN$1;
              if ((x$13 === roundingMode)) {
                return ((((absFraction + parityBit) | 0) > 5) ? sigFraction : 0)
              } else {
                var x$15 = $m_Ljava_math_RoundingMode$().UNNECESSARY$1;
                if ((x$15 === roundingMode)) {
                  if ((fraction === 0)) {
                    return 0
                  } else {
                    throw new $c_jl_ArithmeticException().init___T("Rounding necessary")
                  }
                } else {
                  throw new $c_s_MatchError().init___O(roundingMode)
                }
              }
            }
          }
        }
      }
    }
  }
});
$c_Ljava_math_BigDecimal$.prototype.java$math$BigDecimal$$bitLength__I__I = (function(sValue) {
  var smallValue = ((sValue < 0) ? (~sValue) : sValue);
  return ((32 - $clz32(smallValue)) | 0)
});
$c_Ljava_math_BigDecimal$.prototype.java$math$BigDecimal$$safeLongToInt__J__I = (function(longValue) {
  var ahi = longValue.hi$2;
  if (((ahi === (-1)) ? (((-2147483648) ^ longValue.lo$2) < 0) : (ahi < (-1)))) {
    var jsx$1 = true
  } else {
    var ahi$1 = longValue.hi$2;
    var jsx$1 = ((ahi$1 === 0) ? (((-2147483648) ^ longValue.lo$2) > (-1)) : (ahi$1 > 0))
  };
  if (jsx$1) {
    throw new $c_jl_ArithmeticException().init___T(("Out of int range: " + longValue))
  };
  return longValue.lo$2
});
$c_Ljava_math_BigDecimal$.prototype.valueOf__J__Ljava_math_BigDecimal = (function(unscaledVal) {
  var ahi = unscaledVal.hi$2;
  if ((ahi >= 0)) {
    var ahi$1 = unscaledVal.hi$2;
    var jsx$1 = ((ahi$1 === 0) ? (((-2147483648) ^ unscaledVal.lo$2) < (-2147483637)) : (ahi$1 < 0))
  } else {
    var jsx$1 = false
  };
  if (jsx$1) {
    return this.BigIntScaledByZero$1.get(unscaledVal.lo$2)
  } else {
    return new $c_Ljava_math_BigDecimal().init___J__I(unscaledVal, 0)
  }
});
$c_Ljava_math_BigDecimal$.prototype.bitLength__J__I = (function(sValue) {
  var ahi = sValue.hi$2;
  if ((ahi < 0)) {
    var lo = (~sValue.lo$2);
    var hi = (~sValue.hi$2);
    var x_$_lo$2 = lo;
    var x_$_hi$2 = hi;
    var t = new $c_sjsr_RuntimeLong().init___I__I(x_$_lo$2, x_$_hi$2)
  } else {
    var t = sValue
  };
  var lo$1 = t.lo$2;
  var hi$1 = t.hi$2;
  return ((64 - ((hi$1 !== 0) ? $clz32(hi$1) : ((32 + $clz32(lo$1)) | 0))) | 0)
});
$c_Ljava_math_BigDecimal$.prototype.newArrayOfPows__I__I__AJ = (function(len, pow) {
  var xs = $newArrayObject($d_J.getArrayOf(), [(((-1) + len) | 0)]);
  var elems$2 = null;
  elems$2 = [];
  var x1 = xs.u.length;
  switch (x1) {
    case (-1): {
      break
    }
  };
  var elem$1 = null;
  elem$1 = new $c_sjsr_RuntimeLong().init___I__I(1, 0);
  var elem = elem$1;
  var unboxedElem = ((elem === null) ? null : elem);
  elems$2.push(unboxedElem);
  var i = 0;
  var len$1 = xs.u.length;
  while ((i < len$1)) {
    var idx = i;
    var t$1 = xs.get(idx);
    var lo = t$1.lo$2;
    var hi = t$1.hi$2;
    var arg1 = elem$1;
    var t$2 = $uJ(arg1);
    var lo$1 = t$2.lo$2;
    var hi$1 = t$2.hi$2;
    var t$3 = $uJ(new $c_sjsr_RuntimeLong().init___I__I(lo, hi));
    var hi$3 = (pow >> 31);
    var a0 = (65535 & lo$1);
    var a1 = ((lo$1 >>> 16) | 0);
    var b0 = (65535 & pow);
    var b1 = ((pow >>> 16) | 0);
    var a0b0 = $imul(a0, b0);
    var a1b0 = $imul(a1, b0);
    var a0b1 = $imul(a0, b1);
    var lo$3 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
    var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
    var hi$4 = (((((((($imul(lo$1, hi$3) + $imul(hi$1, pow)) | 0) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
    elem$1 = new $c_sjsr_RuntimeLong().init___I__I(lo$3, hi$4);
    var elem$2 = elem$1;
    var unboxedElem$1 = ((elem$2 === null) ? null : elem$2);
    elems$2.push(unboxedElem$1);
    i = ((1 + i) | 0)
  };
  return $makeNativeArrayWrapper($d_J.getArrayOf(), elems$2)
});
$c_Ljava_math_BigDecimal$.prototype.java$math$BigDecimal$$zeroScaledBy__J__Ljava_math_BigDecimal = (function(longScale) {
  var value = longScale.lo$2;
  var hi = (value >> 31);
  if (((longScale.lo$2 === value) && (longScale.hi$2 === hi))) {
    return this.valueOf__J__I__Ljava_math_BigDecimal($m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong(), longScale.lo$2)
  } else {
    var ahi = longScale.hi$2;
    if ((ahi >= 0)) {
      return new $c_Ljava_math_BigDecimal().init___I__I(0, 2147483647)
    } else {
      return new $c_Ljava_math_BigDecimal().init___I__I(0, (-2147483648))
    }
  }
});
var $d_Ljava_math_BigDecimal$ = new $TypeData().initClass({
  Ljava_math_BigDecimal$: 0
}, false, "java.math.BigDecimal$", {
  Ljava_math_BigDecimal$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_Ljava_math_BigDecimal$.prototype.$classData = $d_Ljava_math_BigDecimal$;
var $n_Ljava_math_BigDecimal$ = (void 0);
function $m_Ljava_math_BigDecimal$() {
  if ((!$n_Ljava_math_BigDecimal$)) {
    $n_Ljava_math_BigDecimal$ = new $c_Ljava_math_BigDecimal$().init___()
  };
  return $n_Ljava_math_BigDecimal$
}
/** @constructor */
function $c_Ljava_math_BigInteger$() {
  $c_O.call(this);
  this.ONE$1 = null;
  this.TEN$1 = null;
  this.ZERO$1 = null;
  this.MINUS$undONE$1 = null;
  this.SMALL$undVALUES$1 = null;
  this.TWO$undPOWS$1 = null
}
$c_Ljava_math_BigInteger$.prototype = new $h_O();
$c_Ljava_math_BigInteger$.prototype.constructor = $c_Ljava_math_BigInteger$;
/** @constructor */
function $h_Ljava_math_BigInteger$() {
  /*<skip>*/
}
$h_Ljava_math_BigInteger$.prototype = $c_Ljava_math_BigInteger$.prototype;
$c_Ljava_math_BigInteger$.prototype.init___ = (function() {
  $n_Ljava_math_BigInteger$ = this;
  this.ONE$1 = new $c_Ljava_math_BigInteger().init___I__I(1, 1);
  this.TEN$1 = new $c_Ljava_math_BigInteger().init___I__I(1, 10);
  this.ZERO$1 = new $c_Ljava_math_BigInteger().init___I__I(0, 0);
  this.MINUS$undONE$1 = new $c_Ljava_math_BigInteger().init___I__I((-1), 1);
  var xs = new $c_sjs_js_WrappedArray().init___sjs_js_Array([this.ZERO$1, this.ONE$1, new $c_Ljava_math_BigInteger().init___I__I(1, 2), new $c_Ljava_math_BigInteger().init___I__I(1, 3), new $c_Ljava_math_BigInteger().init___I__I(1, 4), new $c_Ljava_math_BigInteger().init___I__I(1, 5), new $c_Ljava_math_BigInteger().init___I__I(1, 6), new $c_Ljava_math_BigInteger().init___I__I(1, 7), new $c_Ljava_math_BigInteger().init___I__I(1, 8), new $c_Ljava_math_BigInteger().init___I__I(1, 9), this.TEN$1]);
  var len = $uI(xs.array$6.length);
  var array = $newArrayObject($d_Ljava_math_BigInteger.getArrayOf(), [len]);
  var elem$1 = 0;
  elem$1 = 0;
  var this$4 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(xs, 0, $uI(xs.array$6.length));
  while (this$4.hasNext__Z()) {
    var arg1 = this$4.next__O();
    array.set(elem$1, arg1);
    elem$1 = ((1 + elem$1) | 0)
  };
  this.SMALL$undVALUES$1 = array;
  var elems$2 = null;
  elems$2 = [];
  var i = 0;
  while ((i < 32)) {
    var arg1$1 = i;
    var jsx$1 = $m_Ljava_math_BigInteger$();
    var lo = (((32 & arg1$1) === 0) ? (1 << arg1$1) : 0);
    var hi = (((32 & arg1$1) === 0) ? 0 : (1 << arg1$1));
    var elem = jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo, hi));
    var unboxedElem = ((elem === null) ? null : elem);
    elems$2.push(unboxedElem);
    i = ((1 + i) | 0)
  };
  this.TWO$undPOWS$1 = $makeNativeArrayWrapper($d_Ljava_math_BigInteger.getArrayOf(), elems$2);
  return this
});
$c_Ljava_math_BigInteger$.prototype.getPowerOfTwo__I__Ljava_math_BigInteger = (function(exp) {
  if ((exp < this.TWO$undPOWS$1.u.length)) {
    return this.TWO$undPOWS$1.get(exp)
  } else {
    var intCount = (exp >> 5);
    var bitN = (31 & exp);
    var resDigits = $newArrayObject($d_I.getArrayOf(), [((1 + intCount) | 0)]);
    resDigits.set(intCount, (1 << bitN));
    return new $c_Ljava_math_BigInteger().init___I__I__AI(1, ((1 + intCount) | 0), resDigits)
  }
});
$c_Ljava_math_BigInteger$.prototype.valueOf__J__Ljava_math_BigInteger = (function(lVal) {
  var ahi = lVal.hi$2;
  if ((ahi < 0)) {
    if ((!((lVal.lo$2 === (-1)) && (lVal.hi$2 === (-1))))) {
      var lo = lVal.lo$2;
      var hi = lVal.hi$2;
      var lo$1 = ((-lo) | 0);
      var hi$1 = ((lo !== 0) ? (~hi) : ((-hi) | 0));
      return new $c_Ljava_math_BigInteger().init___I__J((-1), new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$1))
    } else {
      return this.MINUS$undONE$1
    }
  } else {
    var ahi$1 = lVal.hi$2;
    if (((ahi$1 === 0) ? (((-2147483648) ^ lVal.lo$2) <= (-2147483638)) : (ahi$1 < 0))) {
      return this.SMALL$undVALUES$1.get(lVal.lo$2)
    } else {
      return new $c_Ljava_math_BigInteger().init___I__J(1, lVal)
    }
  }
});
var $d_Ljava_math_BigInteger$ = new $TypeData().initClass({
  Ljava_math_BigInteger$: 0
}, false, "java.math.BigInteger$", {
  Ljava_math_BigInteger$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_Ljava_math_BigInteger$.prototype.$classData = $d_Ljava_math_BigInteger$;
var $n_Ljava_math_BigInteger$ = (void 0);
function $m_Ljava_math_BigInteger$() {
  if ((!$n_Ljava_math_BigInteger$)) {
    $n_Ljava_math_BigInteger$ = new $c_Ljava_math_BigInteger$().init___()
  };
  return $n_Ljava_math_BigInteger$
}
/** @constructor */
function $c_Ljava_math_RoundingMode$() {
  $c_O.call(this);
  this.UP$1 = null;
  this.DOWN$1 = null;
  this.CEILING$1 = null;
  this.FLOOR$1 = null;
  this.HALF$undUP$1 = null;
  this.HALF$undDOWN$1 = null;
  this.HALF$undEVEN$1 = null;
  this.UNNECESSARY$1 = null;
  this.$$undvalues$1 = null
}
$c_Ljava_math_RoundingMode$.prototype = new $h_O();
$c_Ljava_math_RoundingMode$.prototype.constructor = $c_Ljava_math_RoundingMode$;
/** @constructor */
function $h_Ljava_math_RoundingMode$() {
  /*<skip>*/
}
$h_Ljava_math_RoundingMode$.prototype = $c_Ljava_math_RoundingMode$.prototype;
$c_Ljava_math_RoundingMode$.prototype.init___ = (function() {
  $n_Ljava_math_RoundingMode$ = this;
  this.UP$1 = new $c_Ljava_math_RoundingMode().init___T__I("UP", 0);
  this.DOWN$1 = new $c_Ljava_math_RoundingMode().init___T__I("DOWN", 1);
  this.CEILING$1 = new $c_Ljava_math_RoundingMode().init___T__I("CEILING", 2);
  this.FLOOR$1 = new $c_Ljava_math_RoundingMode().init___T__I("FLOOR", 3);
  this.HALF$undUP$1 = new $c_Ljava_math_RoundingMode().init___T__I("HALF_UP", 4);
  this.HALF$undDOWN$1 = new $c_Ljava_math_RoundingMode().init___T__I("HALF_DOWN", 5);
  this.HALF$undEVEN$1 = new $c_Ljava_math_RoundingMode().init___T__I("HALF_EVEN", 6);
  this.UNNECESSARY$1 = new $c_Ljava_math_RoundingMode().init___T__I("UNNECESSARY", 7);
  var xs = new $c_sjs_js_WrappedArray().init___sjs_js_Array([this.UP$1, this.DOWN$1, this.CEILING$1, this.FLOOR$1, this.HALF$undUP$1, this.HALF$undDOWN$1, this.HALF$undEVEN$1, this.UNNECESSARY$1]);
  var len = $uI(xs.array$6.length);
  var array = $newArrayObject($d_Ljava_math_RoundingMode.getArrayOf(), [len]);
  var elem$1 = 0;
  elem$1 = 0;
  var this$4 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(xs, 0, $uI(xs.array$6.length));
  while (this$4.hasNext__Z()) {
    var arg1 = this$4.next__O();
    array.set(elem$1, arg1);
    elem$1 = ((1 + elem$1) | 0)
  };
  this.$$undvalues$1 = array;
  return this
});
var $d_Ljava_math_RoundingMode$ = new $TypeData().initClass({
  Ljava_math_RoundingMode$: 0
}, false, "java.math.RoundingMode$", {
  Ljava_math_RoundingMode$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_Ljava_math_RoundingMode$.prototype.$classData = $d_Ljava_math_RoundingMode$;
var $n_Ljava_math_RoundingMode$ = (void 0);
function $m_Ljava_math_RoundingMode$() {
  if ((!$n_Ljava_math_RoundingMode$)) {
    $n_Ljava_math_RoundingMode$ = new $c_Ljava_math_RoundingMode$().init___()
  };
  return $n_Ljava_math_RoundingMode$
}
var $d_jl_Boolean = new $TypeData().initClass({
  jl_Boolean: 0
}, false, "java.lang.Boolean", {
  jl_Boolean: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return ((typeof x) === "boolean")
}));
/** @constructor */
function $c_jl_Character() {
  $c_O.call(this);
  this.value$1 = 0
}
$c_jl_Character.prototype = new $h_O();
$c_jl_Character.prototype.constructor = $c_jl_Character;
/** @constructor */
function $h_jl_Character() {
  /*<skip>*/
}
$h_jl_Character.prototype = $c_jl_Character.prototype;
$c_jl_Character.prototype.equals__O__Z = (function(that) {
  if ($is_jl_Character(that)) {
    var jsx$1 = this.value$1;
    var this$1 = $as_jl_Character(that);
    return (jsx$1 === this$1.value$1)
  } else {
    return false
  }
});
$c_jl_Character.prototype.toString__T = (function() {
  var c = this.value$1;
  return $as_T($g.String.fromCharCode(c))
});
$c_jl_Character.prototype.init___C = (function(value) {
  this.value$1 = value;
  return this
});
$c_jl_Character.prototype.hashCode__I = (function() {
  return this.value$1
});
function $is_jl_Character(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_Character)))
}
function $as_jl_Character(obj) {
  return (($is_jl_Character(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.Character"))
}
function $isArrayOf_jl_Character(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Character)))
}
function $asArrayOf_jl_Character(obj, depth) {
  return (($isArrayOf_jl_Character(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Character;", depth))
}
var $d_jl_Character = new $TypeData().initClass({
  jl_Character: 0
}, false, "java.lang.Character", {
  jl_Character: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
});
$c_jl_Character.prototype.$classData = $d_jl_Character;
/** @constructor */
function $c_jl_Character$() {
  $c_O.call(this);
  this.java$lang$Character$$charTypesFirst256$1 = null;
  this.charTypeIndices$1 = null;
  this.charTypes$1 = null;
  this.isMirroredIndices$1 = null;
  this.bitmap$0$1 = 0
}
$c_jl_Character$.prototype = new $h_O();
$c_jl_Character$.prototype.constructor = $c_jl_Character$;
/** @constructor */
function $h_jl_Character$() {
  /*<skip>*/
}
$h_jl_Character$.prototype = $c_jl_Character$.prototype;
$c_jl_Character$.prototype.init___ = (function() {
  return this
});
$c_jl_Character$.prototype.digit__C__I__I = (function(c, radix) {
  return (((radix > 36) || (radix < 2)) ? (-1) : ((((c >= 48) && (c <= 57)) && ((((-48) + c) | 0) < radix)) ? (((-48) + c) | 0) : ((((c >= 65) && (c <= 90)) && ((((-65) + c) | 0) < (((-10) + radix) | 0))) ? (((-55) + c) | 0) : ((((c >= 97) && (c <= 122)) && ((((-97) + c) | 0) < (((-10) + radix) | 0))) ? (((-87) + c) | 0) : ((((c >= 65313) && (c <= 65338)) && ((((-65313) + c) | 0) < (((-10) + radix) | 0))) ? (((-65303) + c) | 0) : ((((c >= 65345) && (c <= 65370)) && ((((-65345) + c) | 0) < (((-10) + radix) | 0))) ? (((-65303) + c) | 0) : (-1)))))))
});
var $d_jl_Character$ = new $TypeData().initClass({
  jl_Character$: 0
}, false, "java.lang.Character$", {
  jl_Character$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_jl_Character$.prototype.$classData = $d_jl_Character$;
var $n_jl_Character$ = (void 0);
function $m_jl_Character$() {
  if ((!$n_jl_Character$)) {
    $n_jl_Character$ = new $c_jl_Character$().init___()
  };
  return $n_jl_Character$
}
/** @constructor */
function $c_jl_Double$() {
  $c_O.call(this);
  this.doubleStrPat$1 = null;
  this.bitmap$0$1 = false
}
$c_jl_Double$.prototype = new $h_O();
$c_jl_Double$.prototype.constructor = $c_jl_Double$;
/** @constructor */
function $h_jl_Double$() {
  /*<skip>*/
}
$h_jl_Double$.prototype = $c_jl_Double$.prototype;
$c_jl_Double$.prototype.init___ = (function() {
  return this
});
$c_jl_Double$.prototype.doubleStrPat__p1__sjs_js_RegExp = (function() {
  return ((!this.bitmap$0$1) ? this.doubleStrPat$lzycompute__p1__sjs_js_RegExp() : this.doubleStrPat$1)
});
$c_jl_Double$.prototype.doubleStrPat$lzycompute__p1__sjs_js_RegExp = (function() {
  if ((!this.bitmap$0$1)) {
    this.doubleStrPat$1 = new $g.RegExp("^[x00-x20]*[+-]?(NaN|Infinity|(d+.?d*|.d+)([eE][+-]?d+)?)[fFdD]?[x00-x20]*$");
    this.bitmap$0$1 = true
  };
  return this.doubleStrPat$1
});
$c_jl_Double$.prototype.compare__D__D__I = (function(a, b) {
  if ((a !== a)) {
    return ((b !== b) ? 0 : 1)
  } else if ((b !== b)) {
    return (-1)
  } else if ((a === b)) {
    if ((a === 0.0)) {
      var ainf = (1.0 / a);
      return ((ainf === (1.0 / b)) ? 0 : ((ainf < 0) ? (-1) : 1))
    } else {
      return 0
    }
  } else {
    return ((a < b) ? (-1) : 1)
  }
});
$c_jl_Double$.prototype.parseDouble__T__D = (function(s) {
  if ($uZ(this.doubleStrPat__p1__sjs_js_RegExp().test(s))) {
    return $uD($g.parseFloat(s))
  } else {
    throw new $c_jl_NumberFormatException().init___T(new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["For input string: \"", "\""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([s])))
  }
});
var $d_jl_Double$ = new $TypeData().initClass({
  jl_Double$: 0
}, false, "java.lang.Double$", {
  jl_Double$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_jl_Double$.prototype.$classData = $d_jl_Double$;
var $n_jl_Double$ = (void 0);
function $m_jl_Double$() {
  if ((!$n_jl_Double$)) {
    $n_jl_Double$ = new $c_jl_Double$().init___()
  };
  return $n_jl_Double$
}
/** @constructor */
function $c_jl_Enum() {
  $c_O.call(this);
  this.$$undname$1 = null;
  this.$$undordinal$1 = 0
}
$c_jl_Enum.prototype = new $h_O();
$c_jl_Enum.prototype.constructor = $c_jl_Enum;
/** @constructor */
function $h_jl_Enum() {
  /*<skip>*/
}
$h_jl_Enum.prototype = $c_jl_Enum.prototype;
$c_jl_Enum.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
$c_jl_Enum.prototype.toString__T = (function() {
  return this.$$undname$1
});
$c_jl_Enum.prototype.init___T__I = (function(_name, _ordinal) {
  this.$$undname$1 = _name;
  this.$$undordinal$1 = _ordinal;
  return this
});
$c_jl_Enum.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this)
});
/** @constructor */
function $c_jl_Error() {
  $c_jl_Throwable.call(this)
}
$c_jl_Error.prototype = new $h_jl_Throwable();
$c_jl_Error.prototype.constructor = $c_jl_Error;
/** @constructor */
function $h_jl_Error() {
  /*<skip>*/
}
$h_jl_Error.prototype = $c_jl_Error.prototype;
/** @constructor */
function $c_jl_Exception() {
  $c_jl_Throwable.call(this)
}
$c_jl_Exception.prototype = new $h_jl_Throwable();
$c_jl_Exception.prototype.constructor = $c_jl_Exception;
/** @constructor */
function $h_jl_Exception() {
  /*<skip>*/
}
$h_jl_Exception.prototype = $c_jl_Exception.prototype;
/** @constructor */
function $c_jl_Integer$() {
  $c_O.call(this)
}
$c_jl_Integer$.prototype = new $h_O();
$c_jl_Integer$.prototype.constructor = $c_jl_Integer$;
/** @constructor */
function $h_jl_Integer$() {
  /*<skip>*/
}
$h_jl_Integer$.prototype = $c_jl_Integer$.prototype;
$c_jl_Integer$.prototype.init___ = (function() {
  return this
});
$c_jl_Integer$.prototype.fail$1__p1__T__sr_Nothing$ = (function(s$1) {
  throw new $c_jl_NumberFormatException().init___T(new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["For input string: \"", "\""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([s$1])))
});
$c_jl_Integer$.prototype.parseInt__T__I__I = (function(s, radix) {
  if ((s === null)) {
    var jsx$1 = true
  } else {
    var this$2 = new $c_sci_StringOps().init___T(s);
    var $$this = this$2.repr$1;
    var jsx$1 = ($uI($$this.length) === 0)
  };
  if (((jsx$1 || (radix < 2)) || (radix > 36))) {
    this.fail$1__p1__T__sr_Nothing$(s)
  } else {
    var i = ((((65535 & $uI(s.charCodeAt(0))) === 45) || ((65535 & $uI(s.charCodeAt(0))) === 43)) ? 1 : 0);
    var this$12 = new $c_sci_StringOps().init___T(s);
    var $$this$1 = this$12.repr$1;
    if (($uI($$this$1.length) <= i)) {
      this.fail$1__p1__T__sr_Nothing$(s)
    } else {
      while (true) {
        var jsx$2 = i;
        var this$16 = new $c_sci_StringOps().init___T(s);
        var $$this$2 = this$16.repr$1;
        if ((jsx$2 < $uI($$this$2.length))) {
          var jsx$3 = $m_jl_Character$();
          var index = i;
          if ((jsx$3.digit__C__I__I((65535 & $uI(s.charCodeAt(index))), radix) < 0)) {
            this.fail$1__p1__T__sr_Nothing$(s)
          };
          i = ((1 + i) | 0)
        } else {
          break
        }
      };
      var res = $uD($g.parseInt(s, radix));
      return (((res !== res) || ((res > 2147483647) || (res < (-2147483648)))) ? this.fail$1__p1__T__sr_Nothing$(s) : $doubleToInt(res))
    }
  }
});
var $d_jl_Integer$ = new $TypeData().initClass({
  jl_Integer$: 0
}, false, "java.lang.Integer$", {
  jl_Integer$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_jl_Integer$.prototype.$classData = $d_jl_Integer$;
var $n_jl_Integer$ = (void 0);
function $m_jl_Integer$() {
  if ((!$n_jl_Integer$)) {
    $n_jl_Integer$ = new $c_jl_Integer$().init___()
  };
  return $n_jl_Integer$
}
/** @constructor */
function $c_jl_Long$() {
  $c_O.call(this);
  this.StringRadixInfos$1 = null;
  this.bitmap$0$1 = false
}
$c_jl_Long$.prototype = new $h_O();
$c_jl_Long$.prototype.constructor = $c_jl_Long$;
/** @constructor */
function $h_jl_Long$() {
  /*<skip>*/
}
$h_jl_Long$.prototype = $c_jl_Long$.prototype;
$c_jl_Long$.prototype.init___ = (function() {
  return this
});
$c_jl_Long$.prototype.StringRadixInfos__p1__sjs_js_Array = (function() {
  return ((!this.bitmap$0$1) ? this.StringRadixInfos$lzycompute__p1__sjs_js_Array() : this.StringRadixInfos$1)
});
$c_jl_Long$.prototype.parseLong__T__I__J = (function(s, radix) {
  if ((s === "")) {
    this.parseLongError__p1__T__sr_Nothing$(s)
  };
  var start = 0;
  var neg = false;
  var x1 = (65535 & $uI(s.charCodeAt(0)));
  switch (x1) {
    case 43: {
      start = 1;
      break
    }
    case 45: {
      start = 1;
      neg = true;
      break
    }
  };
  var t = this.parseUnsignedLongInternal__T__I__I__J(s, radix, start);
  var lo = t.lo$2;
  var hi = t.hi$2;
  if (neg) {
    var lo$1 = ((-lo) | 0);
    var hi$1 = ((lo !== 0) ? (~hi) : ((-hi) | 0));
    if (((hi$1 === 0) ? (lo$1 !== 0) : (hi$1 > 0))) {
      this.parseLongError__p1__T__sr_Nothing$(s)
    };
    return new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$1)
  } else {
    if ((hi < 0)) {
      this.parseLongError__p1__T__sr_Nothing$(s)
    };
    return new $c_sjsr_RuntimeLong().init___I__I(lo, hi)
  }
});
$c_jl_Long$.prototype.parseLongError__p1__T__sr_Nothing$ = (function(s) {
  throw new $c_jl_NumberFormatException().init___T(new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["For input string: "\", "\""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([s])))
});
$c_jl_Long$.prototype.parseUnsignedLongInternal__T__I__I__J = (function(s, radix, start) {
  var length = $uI(s.length);
  if ((((start >= length) || (radix < 2)) || (radix > 36))) {
    this.parseLongError__p1__T__sr_Nothing$(s)
  } else {
    var radixInfo = $as_jl_Long$StringRadixInfo(this.StringRadixInfos__p1__sjs_js_Array()[radix]);
    var chunkLen = radixInfo.chunkLength$1;
    var firstChunkStart = start;
    while (true) {
      if ((firstChunkStart < length)) {
        var index = firstChunkStart;
        var jsx$1 = ((65535 & $uI(s.charCodeAt(index))) === 48)
      } else {
        var jsx$1 = false
      };
      if (jsx$1) {
        firstChunkStart = ((1 + firstChunkStart) | 0)
      } else {
        break
      }
    };
    if ((((length - firstChunkStart) | 0) > $imul(3, chunkLen))) {
      this.parseLongError__p1__T__sr_Nothing$(s)
    };
    var i = firstChunkStart;
    while ((i < length)) {
      var jsx$2 = $m_jl_Character$();
      var index$1 = i;
      if ((jsx$2.digit__C__I__I((65535 & $uI(s.charCodeAt(index$1))), radix) < 0)) {
        this.parseLongError__p1__T__sr_Nothing$(s)
      };
      i = ((1 + i) | 0)
    };
    var firstChunkLength = ((1 + (((((-1) + ((length - firstChunkStart) | 0)) | 0) % chunkLen) | 0)) | 0);
    var firstChunkEnd = ((firstChunkStart + firstChunkLength) | 0);
    var chunkStart = firstChunkStart;
    var chunk = $as_T(s.substring(chunkStart, firstChunkEnd));
    var chunkValueDouble = $uD($g.parseInt(chunk, radix));
    var x = $doubleToInt(chunkValueDouble);
    if ((firstChunkEnd === length)) {
      return new $c_sjsr_RuntimeLong().init___I__I(x, 0)
    } else {
      var t = radixInfo.radixPowLength$1;
      var lo = t.lo$2;
      var hi$1 = t.hi$2;
      var secondChunkEnd = ((firstChunkEnd + chunkLen) | 0);
      var a0 = (65535 & x);
      var a1 = ((x >>> 16) | 0);
      var b0 = (65535 & lo);
      var b1 = ((lo >>> 16) | 0);
      var a0b0 = $imul(a0, b0);
      var a1b0 = $imul(a1, b0);
      var a0b1 = $imul(a0, b1);
      var lo$1 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
      var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
      var hi$2 = (((((($imul(x, hi$1) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
      var chunk$1 = $as_T(s.substring(firstChunkEnd, secondChunkEnd));
      var chunkValueDouble$1 = $uD($g.parseInt(chunk$1, radix));
      var x$1 = $doubleToInt(chunkValueDouble$1);
      var lo$2 = ((lo$1 + x$1) | 0);
      var hi$4 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ lo$1)) ? ((1 + hi$2) | 0) : hi$2);
      if ((secondChunkEnd === length)) {
        return new $c_sjsr_RuntimeLong().init___I__I(lo$2, hi$4)
      } else {
        $m_s_Predef$().assert__Z__V((((secondChunkEnd + chunkLen) | 0) === length));
        var t$1 = radixInfo.overflowBarrier$1;
        var lo$3 = t$1.lo$2;
        var hi$5 = t$1.hi$2;
        var chunk$2 = $as_T(s.substring(secondChunkEnd, length));
        var chunkValueDouble$2 = $uD($g.parseInt(chunk$2, radix));
        var x$2 = $doubleToInt(chunkValueDouble$2);
        if (((hi$4 === hi$5) ? (((-2147483648) ^ lo$2) > ((-2147483648) ^ lo$3)) : (hi$4 > hi$5))) {
          this.parseLongError__p1__T__sr_Nothing$(s)
        };
        var a0$1 = (65535 & lo$2);
        var a1$1 = ((lo$2 >>> 16) | 0);
        var b0$1 = (65535 & lo);
        var b1$1 = ((lo >>> 16) | 0);
        var a0b0$1 = $imul(a0$1, b0$1);
        var a1b0$1 = $imul(a1$1, b0$1);
        var a0b1$1 = $imul(a0$1, b1$1);
        var lo$4 = ((a0b0$1 + (((a1b0$1 + a0b1$1) | 0) << 16)) | 0);
        var c1part$1 = ((((a0b0$1 >>> 16) | 0) + a0b1$1) | 0);
        var hi$7 = (((((((($imul(lo$2, hi$1) + $imul(hi$4, lo)) | 0) + $imul(a1$1, b1$1)) | 0) + ((c1part$1 >>> 16) | 0)) | 0) + (((((65535 & c1part$1) + a1b0$1) | 0) >>> 16) | 0)) | 0);
        var lo$5 = ((lo$4 + x$2) | 0);
        var hi$8 = ((((-2147483648) ^ lo$5) < ((-2147483648) ^ lo$4)) ? ((1 + hi$7) | 0) : hi$7);
        var hi$9 = ((-2147483648) ^ hi$8);
        if (((hi$9 === (-2147483648)) && (((-2147483648) ^ lo$5) < ((-2147483648) ^ x$2)))) {
          this.parseLongError__p1__T__sr_Nothing$(s)
        };
        return new $c_sjsr_RuntimeLong().init___I__I(lo$5, hi$8)
      }
    }
  }
});
$c_jl_Long$.prototype.StringRadixInfos$lzycompute__p1__sjs_js_Array = (function() {
  if ((!this.bitmap$0$1)) {
    var r = [];
    var i = 0;
    while (true) {
      var arg1 = i;
      r.push(null);
      if ((i === 1)) {
        break
      };
      i = ((1 + i) | 0)
    };
    var i$1 = 2;
    while (true) {
      var arg1$1 = i$1;
      var barrier = ((2147483647 / arg1$1) | 0);
      var radixPowLength = arg1$1;
      var chunkLength = 1;
      var paddingZeros = "0";
      while ((radixPowLength <= barrier)) {
        radixPowLength = $imul(radixPowLength, arg1$1);
        chunkLength = ((1 + chunkLength) | 0);
        paddingZeros = (paddingZeros + "0")
      };
      var value = radixPowLength;
      var hi = (value >> 31);
      var this$8 = $m_sjsr_RuntimeLong$();
      var lo = this$8.divideUnsignedImpl__I__I__I__I__I((-1), (-1), value, hi);
      var hi$1 = this$8.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      var elem = new $c_jl_Long$StringRadixInfo().init___I__J__T__J(chunkLength, new $c_sjsr_RuntimeLong().init___I__I(value, hi), paddingZeros, new $c_sjsr_RuntimeLong().init___I__I(lo, hi$1));
      r.push(elem);
      if ((i$1 === 36)) {
        break
      };
      i$1 = ((1 + i$1) | 0)
    };
    this.StringRadixInfos$1 = r;
    this.bitmap$0$1 = true
  };
  return this.StringRadixInfos$1
});
var $d_jl_Long$ = new $TypeData().initClass({
  jl_Long$: 0
}, false, "java.lang.Long$", {
  jl_Long$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_jl_Long$.prototype.$classData = $d_jl_Long$;
var $n_jl_Long$ = (void 0);
function $m_jl_Long$() {
  if ((!$n_jl_Long$)) {
    $n_jl_Long$ = new $c_jl_Long$().init___()
  };
  return $n_jl_Long$
}
/** @constructor */
function $c_s_Console$() {
  $c_s_DeprecatedConsole.call(this);
  this.outVar$2 = null;
  this.errVar$2 = null;
  this.inVar$2 = null
}
$c_s_Console$.prototype = new $h_s_DeprecatedConsole();
$c_s_Console$.prototype.constructor = $c_s_Console$;
/** @constructor */
function $h_s_Console$() {
  /*<skip>*/
}
$h_s_Console$.prototype = $c_s_Console$.prototype;
$c_s_Console$.prototype.init___ = (function() {
  $n_s_Console$ = this;
  this.outVar$2 = new $c_s_util_DynamicVariable().init___O($m_jl_System$().out$1);
  this.errVar$2 = new $c_s_util_DynamicVariable().init___O($m_jl_System$().err$1);
  this.inVar$2 = new $c_s_util_DynamicVariable().init___O(null);
  return this
});
var $d_s_Console$ = new $TypeData().initClass({
  s_Console$: 0
}, false, "scala.Console$", {
  s_Console$: 1,
  s_DeprecatedConsole: 1,
  O: 1,
  s_io_AnsiColor: 1
});
$c_s_Console$.prototype.$classData = $d_s_Console$;
var $n_s_Console$ = (void 0);
function $m_s_Console$() {
  if ((!$n_s_Console$)) {
    $n_s_Console$ = new $c_s_Console$().init___()
  };
  return $n_s_Console$
}
/** @constructor */
function $c_s_Predef$() {
  $c_s_LowPriorityImplicits.call(this);
  this.Map$2 = null;
  this.Set$2 = null;
  this.ClassManifest$2 = null;
  this.Manifest$2 = null;
  this.NoManifest$2 = null;
  this.StringCanBuildFrom$2 = null;
  this.singleton$und$less$colon$less$2 = null;
  this.scala$Predef$$singleton$und$eq$colon$eq$f = null
}
$c_s_Predef$.prototype = new $h_s_LowPriorityImplicits();
$c_s_Predef$.prototype.constructor = $c_s_Predef$;
/** @constructor */
function $h_s_Predef$() {
  /*<skip>*/
}
$h_s_Predef$.prototype = $c_s_Predef$.prototype;
$c_s_Predef$.prototype.init___ = (function() {
  $n_s_Predef$ = this;
  $m_s_package$();
  $m_sci_List$();
  this.Map$2 = $m_sci_Map$();
  this.Set$2 = $m_sci_Set$();
  this.ClassManifest$2 = $m_s_reflect_package$().ClassManifest$1;
  this.Manifest$2 = $m_s_reflect_package$().Manifest$1;
  this.NoManifest$2 = $m_s_reflect_NoManifest$();
  this.StringCanBuildFrom$2 = new $c_s_Predef$$anon$3().init___();
  this.singleton$und$less$colon$less$2 = new $c_s_Predef$$anon$1().init___();
  this.scala$Predef$$singleton$und$eq$colon$eq$f = new $c_s_Predef$$anon$2().init___();
  return this
});
$c_s_Predef$.prototype.assert__Z__V = (function(assertion) {
  if ((!assertion)) {
    throw new $c_jl_AssertionError().init___O("assertion failed")
  }
});
$c_s_Predef$.prototype.require__Z__V = (function(requirement) {
  if ((!requirement)) {
    throw new $c_jl_IllegalArgumentException().init___T("requirement failed")
  }
});
var $d_s_Predef$ = new $TypeData().initClass({
  s_Predef$: 0
}, false, "scala.Predef$", {
  s_Predef$: 1,
  s_LowPriorityImplicits: 1,
  O: 1,
  s_DeprecatedPredef: 1
});
$c_s_Predef$.prototype.$classData = $d_s_Predef$;
var $n_s_Predef$ = (void 0);
function $m_s_Predef$() {
  if ((!$n_s_Predef$)) {
    $n_s_Predef$ = new $c_s_Predef$().init___()
  };
  return $n_s_Predef$
}
/** @constructor */
function $c_s_StringContext$() {
  $c_O.call(this)
}
$c_s_StringContext$.prototype = new $h_O();
$c_s_StringContext$.prototype.constructor = $c_s_StringContext$;
/** @constructor */
function $h_s_StringContext$() {
  /*<skip>*/
}
$h_s_StringContext$.prototype = $c_s_StringContext$.prototype;
$c_s_StringContext$.prototype.init___ = (function() {
  return this
});
$c_s_StringContext$.prototype.treatEscapes0__p1__T__Z__T = (function(str, strict) {
  var len = $uI(str.length);
  var x1 = $m_sjsr_RuntimeString$().indexOf__T__I__I(str, 92);
  switch (x1) {
    case (-1): {
      return str;
      break
    }
    default: {
      return this.replace$1__p1__I__T__Z__I__T(x1, str, strict, len)
    }
  }
});
$c_s_StringContext$.prototype.loop$1__p1__I__I__T__Z__I__jl_StringBuilder__T = (function(i, next, str$1, strict$1, len$1, b$1) {
  _loop: while (true) {
    if ((next >= 0)) {
      if ((next > i)) {
        b$1.append__jl_CharSequence__I__I__jl_StringBuilder(str$1, i, next)
      };
      var idx = ((1 + next) | 0);
      if ((idx >= len$1)) {
        throw new $c_s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
      };
      var index = idx;
      var x1 = (65535 & $uI(str$1.charCodeAt(index)));
      switch (x1) {
        case 98: {
          var c = 8;
          break
        }
        case 116: {
          var c = 9;
          break
        }
        case 110: {
          var c = 10;
          break
        }
        case 102: {
          var c = 12;
          break
        }
        case 114: {
          var c = 13;
          break
        }
        case 34: {
          var c = 34;
          break
        }
        case 39: {
          var c = 39;
          break
        }
        case 92: {
          var c = 92;
          break
        }
        default: {
          if (((x1 >= 48) && (x1 <= 55))) {
            if (strict$1) {
              throw new $c_s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
            };
            var index$1 = idx;
            var leadch = (65535 & $uI(str$1.charCodeAt(index$1)));
            var oct = (((-48) + leadch) | 0);
            idx = ((1 + idx) | 0);
            if ((idx < len$1)) {
              var index$2 = idx;
              var jsx$2 = ((65535 & $uI(str$1.charCodeAt(index$2))) >= 48)
            } else {
              var jsx$2 = false
            };
            if (jsx$2) {
              var index$3 = idx;
              var jsx$1 = ((65535 & $uI(str$1.charCodeAt(index$3))) <= 55)
            } else {
              var jsx$1 = false
            };
            if (jsx$1) {
              var jsx$3 = oct;
              var index$4 = idx;
              oct = (((-48) + (((jsx$3 << 3) + (65535 & $uI(str$1.charCodeAt(index$4)))) | 0)) | 0);
              idx = ((1 + idx) | 0);
              if (((idx < len$1) && (leadch <= 51))) {
                var index$5 = idx;
                var jsx$5 = ((65535 & $uI(str$1.charCodeAt(index$5))) >= 48)
              } else {
                var jsx$5 = false
              };
              if (jsx$5) {
                var index$6 = idx;
                var jsx$4 = ((65535 & $uI(str$1.charCodeAt(index$6))) <= 55)
              } else {
                var jsx$4 = false
              };
              if (jsx$4) {
                var jsx$6 = oct;
                var index$7 = idx;
                oct = (((-48) + (((jsx$6 << 3) + (65535 & $uI(str$1.charCodeAt(index$7)))) | 0)) | 0);
                idx = ((1 + idx) | 0)
              }
            };
            idx = (((-1) + idx) | 0);
            var c = (65535 & oct)
          } else {
            var c;
            throw new $c_s_StringContext$InvalidEscapeException().init___T__I(str$1, next)
          }
        }
      };
      idx = ((1 + idx) | 0);
      b$1.append__C__jl_StringBuilder(c);
      var temp$i = idx;
      var temp$next = $m_sjsr_RuntimeString$().indexOf__T__I__I__I(str$1, 92, idx);
      i = temp$i;
      next = temp$next;
      continue _loop
    } else {
      if ((i < len$1)) {
        b$1.append__jl_CharSequence__I__I__jl_StringBuilder(str$1, i, len$1)
      };
      return b$1.content$1
    }
  }
});
$c_s_StringContext$.prototype.replace$1__p1__I__T__Z__I__T = (function(first, str$1, strict$1, len$1) {
  var b = new $c_jl_StringBuilder().init___();
  return this.loop$1__p1__I__I__T__Z__I__jl_StringBuilder__T(0, first, str$1, strict$1, len$1, b)
});
var $d_s_StringContext$ = new $TypeData().initClass({
  s_StringContext$: 0
}, false, "scala.StringContext$", {
  s_StringContext$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_StringContext$.prototype.$classData = $d_s_StringContext$;
var $n_s_StringContext$ = (void 0);
function $m_s_StringContext$() {
  if ((!$n_s_StringContext$)) {
    $n_s_StringContext$ = new $c_s_StringContext$().init___()
  };
  return $n_s_StringContext$
}
/** @constructor */
function $c_s_math_BigDecimal$() {
  $c_O.call(this);
  this.cache$1 = null;
  this.minCached$1 = 0;
  this.maxCached$1 = 0;
  this.defaultMathContext$1 = null;
  this.bitmap$0$1 = false
}
$c_s_math_BigDecimal$.prototype = new $h_O();
$c_s_math_BigDecimal$.prototype.constructor = $c_s_math_BigDecimal$;
/** @constructor */
function $h_s_math_BigDecimal$() {
  /*<skip>*/
}
$h_s_math_BigDecimal$.prototype = $c_s_math_BigDecimal$.prototype;
$c_s_math_BigDecimal$.prototype.init___ = (function() {
  $n_s_math_BigDecimal$ = this;
  this.minCached$1 = (-512);
  this.maxCached$1 = 512;
  this.defaultMathContext$1 = $m_Ljava_math_MathContext$().DECIMAL128$1;
  return this
});
$c_s_math_BigDecimal$.prototype.apply__J__Ljava_math_MathContext__s_math_BigDecimal = (function(l, mc) {
  return new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(new $c_Ljava_math_BigDecimal().init___J__Ljava_math_MathContext(l, mc), mc)
});
$c_s_math_BigDecimal$.prototype.cache$lzycompute__p1__As_math_BigDecimal = (function() {
  if ((!this.bitmap$0$1)) {
    this.cache$1 = $newArrayObject($d_s_math_BigDecimal.getArrayOf(), [((1 + ((this.maxCached$1 - this.minCached$1) | 0)) | 0)]);
    this.bitmap$0$1 = true
  };
  return this.cache$1
});
$c_s_math_BigDecimal$.prototype.decimal__D__Ljava_math_MathContext__s_math_BigDecimal = (function(d, mc) {
  return new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(new $c_Ljava_math_BigDecimal().init___T__Ljava_math_MathContext(("" + d), mc), mc)
});
$c_s_math_BigDecimal$.prototype.exact__s_math_BigInt__s_math_BigDecimal = (function(bi) {
  return this.exact__Ljava_math_BigDecimal__s_math_BigDecimal(new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger(bi.bigInteger$3))
});
$c_s_math_BigDecimal$.prototype.cache__p1__As_math_BigDecimal = (function() {
  return ((!this.bitmap$0$1) ? this.cache$lzycompute__p1__As_math_BigDecimal() : this.cache$1)
});
$c_s_math_BigDecimal$.prototype.apply__I__Ljava_math_MathContext__s_math_BigDecimal = (function(i, mc) {
  var x$2 = this.defaultMathContext$1;
  if (((((mc === null) ? (x$2 === null) : mc.equals__O__Z(x$2)) && (this.minCached$1 <= i)) && (i <= this.maxCached$1))) {
    var offset = ((i - this.minCached$1) | 0);
    var n = this.cache__p1__As_math_BigDecimal().get(offset);
    if ((n === null)) {
      var jsx$1 = $m_Ljava_math_BigDecimal$();
      var hi = (i >> 31);
      n = new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(jsx$1.valueOf__J__Ljava_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(i, hi)), mc);
      this.cache__p1__As_math_BigDecimal().set(offset, n)
    };
    return n
  } else {
    var hi$1 = (i >> 31);
    return this.apply__J__Ljava_math_MathContext__s_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(i, hi$1), mc)
  }
});
$c_s_math_BigDecimal$.prototype.exact__T__s_math_BigDecimal = (function(s) {
  return this.exact__Ljava_math_BigDecimal__s_math_BigDecimal(new $c_Ljava_math_BigDecimal().init___T(s))
});
$c_s_math_BigDecimal$.prototype.exact__Ljava_math_BigDecimal__s_math_BigDecimal = (function(repr) {
  var jsx$1 = repr.precision__I();
  var this$1 = this.defaultMathContext$1;
  if ((jsx$1 <= this$1.precision$1)) {
    var mc = this.defaultMathContext$1
  } else {
    var mc = new $c_Ljava_math_MathContext().init___I__Ljava_math_RoundingMode(repr.precision__I(), $m_Ljava_math_RoundingMode$().HALF$undEVEN$1)
  };
  return new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(repr, mc)
});
var $d_s_math_BigDecimal$ = new $TypeData().initClass({
  s_math_BigDecimal$: 0
}, false, "scala.math.BigDecimal$", {
  s_math_BigDecimal$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_BigDecimal$.prototype.$classData = $d_s_math_BigDecimal$;
var $n_s_math_BigDecimal$ = (void 0);
function $m_s_math_BigDecimal$() {
  if ((!$n_s_math_BigDecimal$)) {
    $n_s_math_BigDecimal$ = new $c_s_math_BigDecimal$().init___()
  };
  return $n_s_math_BigDecimal$
}
/** @constructor */
function $c_s_math_BigInt$() {
  $c_O.call(this);
  this.minCached$1 = 0;
  this.maxCached$1 = 0;
  this.cache$1 = null;
  this.scala$math$BigInt$$minusOne$1 = null
}
$c_s_math_BigInt$.prototype = new $h_O();
$c_s_math_BigInt$.prototype.constructor = $c_s_math_BigInt$;
/** @constructor */
function $h_s_math_BigInt$() {
  /*<skip>*/
}
$h_s_math_BigInt$.prototype = $c_s_math_BigInt$.prototype;
$c_s_math_BigInt$.prototype.init___ = (function() {
  $n_s_math_BigInt$ = this;
  this.minCached$1 = (-1024);
  this.maxCached$1 = 1024;
  this.cache$1 = $newArrayObject($d_s_math_BigInt.getArrayOf(), [((1 + ((this.maxCached$1 - this.minCached$1) | 0)) | 0)]);
  this.scala$math$BigInt$$minusOne$1 = $m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I((-1), (-1)));
  return this
});
$c_s_math_BigInt$.prototype.apply__I__s_math_BigInt = (function(i) {
  if (((this.minCached$1 <= i) && (i <= this.maxCached$1))) {
    var offset = ((i - this.minCached$1) | 0);
    var n = this.cache$1.get(offset);
    if ((n === null)) {
      var jsx$1 = $m_Ljava_math_BigInteger$();
      var hi = (i >> 31);
      n = new $c_s_math_BigInt().init___Ljava_math_BigInteger(jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(i, hi)));
      this.cache$1.set(offset, n)
    };
    return n
  } else {
    var jsx$2 = $m_Ljava_math_BigInteger$();
    var hi$1 = (i >> 31);
    return new $c_s_math_BigInt().init___Ljava_math_BigInteger(jsx$2.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(i, hi$1)))
  }
});
$c_s_math_BigInt$.prototype.apply__J__s_math_BigInt = (function(l) {
  var value = this.minCached$1;
  var hi = (value >> 31);
  var bhi = l.hi$2;
  if (((hi === bhi) ? (((-2147483648) ^ value) <= ((-2147483648) ^ l.lo$2)) : (hi < bhi))) {
    var value$1 = this.maxCached$1;
    var hi$1 = (value$1 >> 31);
    var ahi = l.hi$2;
    var jsx$1 = ((ahi === hi$1) ? (((-2147483648) ^ l.lo$2) <= ((-2147483648) ^ value$1)) : (ahi < hi$1))
  } else {
    var jsx$1 = false
  };
  if (jsx$1) {
    return this.apply__I__s_math_BigInt(l.lo$2)
  } else {
    return new $c_s_math_BigInt().init___Ljava_math_BigInteger($m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(l))
  }
});
$c_s_math_BigInt$.prototype.apply__T__s_math_BigInt = (function(x) {
  return new $c_s_math_BigInt().init___Ljava_math_BigInteger(new $c_Ljava_math_BigInteger().init___T(x))
});
var $d_s_math_BigInt$ = new $TypeData().initClass({
  s_math_BigInt$: 0
}, false, "scala.math.BigInt$", {
  s_math_BigInt$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_BigInt$.prototype.$classData = $d_s_math_BigInt$;
var $n_s_math_BigInt$ = (void 0);
function $m_s_math_BigInt$() {
  if ((!$n_s_math_BigInt$)) {
    $n_s_math_BigInt$ = new $c_s_math_BigInt$().init___()
  };
  return $n_s_math_BigInt$
}
/** @constructor */
function $c_s_math_Fractional$() {
  $c_O.call(this)
}
$c_s_math_Fractional$.prototype = new $h_O();
$c_s_math_Fractional$.prototype.constructor = $c_s_math_Fractional$;
/** @constructor */
function $h_s_math_Fractional$() {
  /*<skip>*/
}
$h_s_math_Fractional$.prototype = $c_s_math_Fractional$.prototype;
$c_s_math_Fractional$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Fractional$ = new $TypeData().initClass({
  s_math_Fractional$: 0
}, false, "scala.math.Fractional$", {
  s_math_Fractional$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_Fractional$.prototype.$classData = $d_s_math_Fractional$;
var $n_s_math_Fractional$ = (void 0);
function $m_s_math_Fractional$() {
  if ((!$n_s_math_Fractional$)) {
    $n_s_math_Fractional$ = new $c_s_math_Fractional$().init___()
  };
  return $n_s_math_Fractional$
}
/** @constructor */
function $c_s_math_Integral$() {
  $c_O.call(this)
}
$c_s_math_Integral$.prototype = new $h_O();
$c_s_math_Integral$.prototype.constructor = $c_s_math_Integral$;
/** @constructor */
function $h_s_math_Integral$() {
  /*<skip>*/
}
$h_s_math_Integral$.prototype = $c_s_math_Integral$.prototype;
$c_s_math_Integral$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Integral$ = new $TypeData().initClass({
  s_math_Integral$: 0
}, false, "scala.math.Integral$", {
  s_math_Integral$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_Integral$.prototype.$classData = $d_s_math_Integral$;
var $n_s_math_Integral$ = (void 0);
function $m_s_math_Integral$() {
  if ((!$n_s_math_Integral$)) {
    $n_s_math_Integral$ = new $c_s_math_Integral$().init___()
  };
  return $n_s_math_Integral$
}
/** @constructor */
function $c_s_math_Numeric$() {
  $c_O.call(this)
}
$c_s_math_Numeric$.prototype = new $h_O();
$c_s_math_Numeric$.prototype.constructor = $c_s_math_Numeric$;
/** @constructor */
function $h_s_math_Numeric$() {
  /*<skip>*/
}
$h_s_math_Numeric$.prototype = $c_s_math_Numeric$.prototype;
$c_s_math_Numeric$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Numeric$ = new $TypeData().initClass({
  s_math_Numeric$: 0
}, false, "scala.math.Numeric$", {
  s_math_Numeric$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_Numeric$.prototype.$classData = $d_s_math_Numeric$;
var $n_s_math_Numeric$ = (void 0);
function $m_s_math_Numeric$() {
  if ((!$n_s_math_Numeric$)) {
    $n_s_math_Numeric$ = new $c_s_math_Numeric$().init___()
  };
  return $n_s_math_Numeric$
}
/** @constructor */
function $c_s_math_ScalaNumber() {
  $c_jl_Number.call(this)
}
$c_s_math_ScalaNumber.prototype = new $h_jl_Number();
$c_s_math_ScalaNumber.prototype.constructor = $c_s_math_ScalaNumber;
/** @constructor */
function $h_s_math_ScalaNumber() {
  /*<skip>*/
}
$h_s_math_ScalaNumber.prototype = $c_s_math_ScalaNumber.prototype;
function $is_s_math_ScalaNumber(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_math_ScalaNumber)))
}
function $as_s_math_ScalaNumber(obj) {
  return (($is_s_math_ScalaNumber(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.math.ScalaNumber"))
}
function $isArrayOf_s_math_ScalaNumber(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_ScalaNumber)))
}
function $asArrayOf_s_math_ScalaNumber(obj, depth) {
  return (($isArrayOf_s_math_ScalaNumber(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.math.ScalaNumber;", depth))
}
/** @constructor */
function $c_s_util_Either$() {
  $c_O.call(this)
}
$c_s_util_Either$.prototype = new $h_O();
$c_s_util_Either$.prototype.constructor = $c_s_util_Either$;
/** @constructor */
function $h_s_util_Either$() {
  /*<skip>*/
}
$h_s_util_Either$.prototype = $c_s_util_Either$.prototype;
$c_s_util_Either$.prototype.init___ = (function() {
  return this
});
var $d_s_util_Either$ = new $TypeData().initClass({
  s_util_Either$: 0
}, false, "scala.util.Either$", {
  s_util_Either$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_util_Either$.prototype.$classData = $d_s_util_Either$;
var $n_s_util_Either$ = (void 0);
function $m_s_util_Either$() {
  if ((!$n_s_util_Either$)) {
    $n_s_util_Either$ = new $c_s_util_Either$().init___()
  };
  return $n_s_util_Either$
}
/** @constructor */
function $c_s_util_Left$() {
  $c_O.call(this)
}
$c_s_util_Left$.prototype = new $h_O();
$c_s_util_Left$.prototype.constructor = $c_s_util_Left$;
/** @constructor */
function $h_s_util_Left$() {
  /*<skip>*/
}
$h_s_util_Left$.prototype = $c_s_util_Left$.prototype;
$c_s_util_Left$.prototype.init___ = (function() {
  return this
});
$c_s_util_Left$.prototype.toString__T = (function() {
  return "Left"
});
var $d_s_util_Left$ = new $TypeData().initClass({
  s_util_Left$: 0
}, false, "scala.util.Left$", {
  s_util_Left$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_util_Left$.prototype.$classData = $d_s_util_Left$;
var $n_s_util_Left$ = (void 0);
function $m_s_util_Left$() {
  if ((!$n_s_util_Left$)) {
    $n_s_util_Left$ = new $c_s_util_Left$().init___()
  };
  return $n_s_util_Left$
}
/** @constructor */
function $c_s_util_Right$() {
  $c_O.call(this)
}
$c_s_util_Right$.prototype = new $h_O();
$c_s_util_Right$.prototype.constructor = $c_s_util_Right$;
/** @constructor */
function $h_s_util_Right$() {
  /*<skip>*/
}
$h_s_util_Right$.prototype = $c_s_util_Right$.prototype;
$c_s_util_Right$.prototype.init___ = (function() {
  return this
});
$c_s_util_Right$.prototype.toString__T = (function() {
  return "Right"
});
var $d_s_util_Right$ = new $TypeData().initClass({
  s_util_Right$: 0
}, false, "scala.util.Right$", {
  s_util_Right$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_util_Right$.prototype.$classData = $d_s_util_Right$;
var $n_s_util_Right$ = (void 0);
function $m_s_util_Right$() {
  if ((!$n_s_util_Right$)) {
    $n_s_util_Right$ = new $c_s_util_Right$().init___()
  };
  return $n_s_util_Right$
}
/** @constructor */
function $c_s_util_control_NoStackTrace$() {
  $c_O.call(this);
  this.$$undnoSuppression$1 = false
}
$c_s_util_control_NoStackTrace$.prototype = new $h_O();
$c_s_util_control_NoStackTrace$.prototype.constructor = $c_s_util_control_NoStackTrace$;
/** @constructor */
function $h_s_util_control_NoStackTrace$() {
  /*<skip>*/
}
$h_s_util_control_NoStackTrace$.prototype = $c_s_util_control_NoStackTrace$.prototype;
$c_s_util_control_NoStackTrace$.prototype.init___ = (function() {
  this.$$undnoSuppression$1 = false;
  return this
});
var $d_s_util_control_NoStackTrace$ = new $TypeData().initClass({
  s_util_control_NoStackTrace$: 0
}, false, "scala.util.control.NoStackTrace$", {
  s_util_control_NoStackTrace$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_util_control_NoStackTrace$.prototype.$classData = $d_s_util_control_NoStackTrace$;
var $n_s_util_control_NoStackTrace$ = (void 0);
function $m_s_util_control_NoStackTrace$() {
  if ((!$n_s_util_control_NoStackTrace$)) {
    $n_s_util_control_NoStackTrace$ = new $c_s_util_control_NoStackTrace$().init___()
  };
  return $n_s_util_control_NoStackTrace$
}
/** @constructor */
function $c_sc_IndexedSeq$$anon$1() {
  $c_scg_GenTraversableFactory$GenericCanBuildFrom.call(this)
}
$c_sc_IndexedSeq$$anon$1.prototype = new $h_scg_GenTraversableFactory$GenericCanBuildFrom();
$c_sc_IndexedSeq$$anon$1.prototype.constructor = $c_sc_IndexedSeq$$anon$1;
/** @constructor */
function $h_sc_IndexedSeq$$anon$1() {
  /*<skip>*/
}
$h_sc_IndexedSeq$$anon$1.prototype = $c_sc_IndexedSeq$$anon$1.prototype;
$c_sc_IndexedSeq$$anon$1.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory.call(this, $m_sc_IndexedSeq$());
  return this
});
var $d_sc_IndexedSeq$$anon$1 = new $TypeData().initClass({
  sc_IndexedSeq$$anon$1: 0
}, false, "scala.collection.IndexedSeq$$anon$1", {
  sc_IndexedSeq$$anon$1: 1,
  scg_GenTraversableFactory$GenericCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
$c_sc_IndexedSeq$$anon$1.prototype.$classData = $d_sc_IndexedSeq$$anon$1;
/** @constructor */
function $c_scg_GenSeqFactory() {
  $c_scg_GenTraversableFactory.call(this)
}
$c_scg_GenSeqFactory.prototype = new $h_scg_GenTraversableFactory();
$c_scg_GenSeqFactory.prototype.constructor = $c_scg_GenSeqFactory;
/** @constructor */
function $h_scg_GenSeqFactory() {
  /*<skip>*/
}
$h_scg_GenSeqFactory.prototype = $c_scg_GenSeqFactory.prototype;
/** @constructor */
function $c_scg_GenTraversableFactory$$anon$1() {
  $c_scg_GenTraversableFactory$GenericCanBuildFrom.call(this);
  this.$$outer$2 = null
}
$c_scg_GenTraversableFactory$$anon$1.prototype = new $h_scg_GenTraversableFactory$GenericCanBuildFrom();
$c_scg_GenTraversableFactory$$anon$1.prototype.constructor = $c_scg_GenTraversableFactory$$anon$1;
/** @constructor */
function $h_scg_GenTraversableFactory$$anon$1() {
  /*<skip>*/
}
$h_scg_GenTraversableFactory$$anon$1.prototype = $c_scg_GenTraversableFactory$$anon$1.prototype;
$c_scg_GenTraversableFactory$$anon$1.prototype.init___scg_GenTraversableFactory = (function($$outer) {
  if (($$outer === null)) {
    throw $m_sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  $c_scg_GenTraversableFactory$GenericCanBuildFrom.prototype.init___scg_GenTraversableFactory.call(this, $$outer);
  return this
});
var $d_scg_GenTraversableFactory$$anon$1 = new $TypeData().initClass({
  scg_GenTraversableFactory$$anon$1: 0
}, false, "scala.collection.generic.GenTraversableFactory$$anon$1", {
  scg_GenTraversableFactory$$anon$1: 1,
  scg_GenTraversableFactory$GenericCanBuildFrom: 1,
  O: 1,
  scg_CanBuildFrom: 1
});
$c_scg_GenTraversableFactory$$anon$1.prototype.$classData = $d_scg_GenTraversableFactory$$anon$1;
/** @constructor */
function $c_scg_ImmutableMapFactory() {
  $c_scg_MapFactory.call(this)
}
$c_scg_ImmutableMapFactory.prototype = new $h_scg_MapFactory();
$c_scg_ImmutableMapFactory.prototype.constructor = $c_scg_ImmutableMapFactory;
/** @constructor */
function $h_scg_ImmutableMapFactory() {
  /*<skip>*/
}
$h_scg_ImmutableMapFactory.prototype = $c_scg_ImmutableMapFactory.prototype;
/** @constructor */
function $c_sci_$colon$colon$() {
  $c_O.call(this)
}
$c_sci_$colon$colon$.prototype = new $h_O();
$c_sci_$colon$colon$.prototype.constructor = $c_sci_$colon$colon$;
/** @constructor */
function $h_sci_$colon$colon$() {
  /*<skip>*/
}
$h_sci_$colon$colon$.prototype = $c_sci_$colon$colon$.prototype;
$c_sci_$colon$colon$.prototype.init___ = (function() {
  return this
});
$c_sci_$colon$colon$.prototype.toString__T = (function() {
  return "::"
});
var $d_sci_$colon$colon$ = new $TypeData().initClass({
  sci_$colon$colon$: 0
}, false, "scala.collection.immutable.$colon$colon$", {
  sci_$colon$colon$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_$colon$colon$.prototype.$classData = $d_sci_$colon$colon$;
var $n_sci_$colon$colon$ = (void 0);
function $m_sci_$colon$colon$() {
  if ((!$n_sci_$colon$colon$)) {
    $n_sci_$colon$colon$ = new $c_sci_$colon$colon$().init___()
  };
  return $n_sci_$colon$colon$
}
/** @constructor */
function $c_sci_Range$() {
  $c_O.call(this);
  this.MAX$undPRINT$1 = 0
}
$c_sci_Range$.prototype = new $h_O();
$c_sci_Range$.prototype.constructor = $c_sci_Range$;
/** @constructor */
function $h_sci_Range$() {
  /*<skip>*/
}
$h_sci_Range$.prototype = $c_sci_Range$.prototype;
$c_sci_Range$.prototype.init___ = (function() {
  this.MAX$undPRINT$1 = 512;
  return this
});
$c_sci_Range$.prototype.description__p1__I__I__I__Z__T = (function(start, end, step, isInclusive) {
  return ((((start + (isInclusive ? " to " : " until ")) + end) + " by ") + step)
});
$c_sci_Range$.prototype.scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$ = (function(start, end, step, isInclusive) {
  throw new $c_jl_IllegalArgumentException().init___T((this.description__p1__I__I__I__Z__T(start, end, step, isInclusive) + ": seqs cannot contain more than Int.MaxValue elements."))
});
var $d_sci_Range$ = new $TypeData().initClass({
  sci_Range$: 0
}, false, "scala.collection.immutable.Range$", {
  sci_Range$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_Range$.prototype.$classData = $d_sci_Range$;
var $n_sci_Range$ = (void 0);
function $m_sci_Range$() {
  if ((!$n_sci_Range$)) {
    $n_sci_Range$ = new $c_sci_Range$().init___()
  };
  return $n_sci_Range$
}
/** @constructor */
function $c_scm_StringBuilder$() {
  $c_O.call(this)
}
$c_scm_StringBuilder$.prototype = new $h_O();
$c_scm_StringBuilder$.prototype.constructor = $c_scm_StringBuilder$;
/** @constructor */
function $h_scm_StringBuilder$() {
  /*<skip>*/
}
$h_scm_StringBuilder$.prototype = $c_scm_StringBuilder$.prototype;
$c_scm_StringBuilder$.prototype.init___ = (function() {
  return this
});
var $d_scm_StringBuilder$ = new $TypeData().initClass({
  scm_StringBuilder$: 0
}, false, "scala.collection.mutable.StringBuilder$", {
  scm_StringBuilder$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_scm_StringBuilder$.prototype.$classData = $d_scm_StringBuilder$;
var $n_scm_StringBuilder$ = (void 0);
function $m_scm_StringBuilder$() {
  if ((!$n_scm_StringBuilder$)) {
    $n_scm_StringBuilder$ = new $c_scm_StringBuilder$().init___()
  };
  return $n_scm_StringBuilder$
}
/** @constructor */
function $c_sjsr_AnonFunction1() {
  $c_sr_AbstractFunction1.call(this);
  this.f$2 = null
}
$c_sjsr_AnonFunction1.prototype = new $h_sr_AbstractFunction1();
$c_sjsr_AnonFunction1.prototype.constructor = $c_sjsr_AnonFunction1;
/** @constructor */
function $h_sjsr_AnonFunction1() {
  /*<skip>*/
}
$h_sjsr_AnonFunction1.prototype = $c_sjsr_AnonFunction1.prototype;
$c_sjsr_AnonFunction1.prototype.apply__O__O = (function(arg1) {
  return (0, this.f$2)(arg1)
});
$c_sjsr_AnonFunction1.prototype.init___sjs_js_Function1 = (function(f) {
  this.f$2 = f;
  return this
});
var $d_sjsr_AnonFunction1 = new $TypeData().initClass({
  sjsr_AnonFunction1: 0
}, false, "scala.scalajs.runtime.AnonFunction1", {
  sjsr_AnonFunction1: 1,
  sr_AbstractFunction1: 1,
  O: 1,
  F1: 1
});
$c_sjsr_AnonFunction1.prototype.$classData = $d_sjsr_AnonFunction1;
/** @constructor */
function $c_sjsr_RuntimeLong$() {
  $c_O.call(this);
  this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
  this.Zero$1 = null
}
$c_sjsr_RuntimeLong$.prototype = new $h_O();
$c_sjsr_RuntimeLong$.prototype.constructor = $c_sjsr_RuntimeLong$;
/** @constructor */
function $h_sjsr_RuntimeLong$() {
  /*<skip>*/
}
$h_sjsr_RuntimeLong$.prototype = $c_sjsr_RuntimeLong$.prototype;
$c_sjsr_RuntimeLong$.prototype.init___ = (function() {
  $n_sjsr_RuntimeLong$ = this;
  this.Zero$1 = new $c_sjsr_RuntimeLong().init___I__I(0, 0);
  return this
});
$c_sjsr_RuntimeLong$.prototype.Zero__sjsr_RuntimeLong = (function() {
  return this.Zero$1
});
$c_sjsr_RuntimeLong$.prototype.toUnsignedString__p1__I__I__T = (function(lo, hi) {
  if ((((-2097152) & hi) === 0)) {
    var this$5 = ((4.294967296E9 * hi) + $uD((lo >>> 0)));
    return ("" + this$5)
  } else {
    return $as_T(this.unsignedDivModHelper__p1__I__I__I__I__I__sjs_js_$bar(lo, hi, 1000000000, 0, 2))
  }
});
$c_sjsr_RuntimeLong$.prototype.divideImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException().init___T("/ by zero")
  };
  if ((ahi === (alo >> 31))) {
    if ((bhi === (blo >> 31))) {
      if (((alo === (-2147483648)) && (blo === (-1)))) {
        this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
        return (-2147483648)
      } else {
        var lo = ((alo / blo) | 0);
        this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (lo >> 31);
        return lo
      }
    } else if (((alo === (-2147483648)) && ((blo === (-2147483648)) && (bhi === 0)))) {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (-1);
      return (-1)
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      return 0
    }
  } else {
    var neg = (ahi < 0);
    if (neg) {
      var lo$1 = ((-alo) | 0);
      var hi = ((alo !== 0) ? (~ahi) : ((-ahi) | 0));
      var abs_$_lo$2 = lo$1;
      var abs_$_hi$2 = hi
    } else {
      var abs_$_lo$2 = alo;
      var abs_$_hi$2 = ahi
    };
    var neg$1 = (bhi < 0);
    if (neg$1) {
      var lo$2 = ((-blo) | 0);
      var hi$1 = ((blo !== 0) ? (~bhi) : ((-bhi) | 0));
      var abs$1_$_lo$2 = lo$2;
      var abs$1_$_hi$2 = hi$1
    } else {
      var abs$1_$_lo$2 = blo;
      var abs$1_$_hi$2 = bhi
    };
    var absRLo = this.unsigned$und$div__p1__I__I__I__I__I(abs_$_lo$2, abs_$_hi$2, abs$1_$_lo$2, abs$1_$_hi$2);
    if ((neg === neg$1)) {
      return absRLo
    } else {
      var hi$2 = this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ((absRLo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      return ((-absRLo) | 0)
    }
  }
});
$c_sjsr_RuntimeLong$.prototype.scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D = (function(lo, hi) {
  if ((hi < 0)) {
    var x = ((lo !== 0) ? (~hi) : ((-hi) | 0));
    var jsx$1 = $uD((x >>> 0));
    var x$1 = ((-lo) | 0);
    return (-((4.294967296E9 * jsx$1) + $uD((x$1 >>> 0))))
  } else {
    return ((4.294967296E9 * hi) + $uD((lo >>> 0)))
  }
});
$c_sjsr_RuntimeLong$.prototype.fromDouble__D__sjsr_RuntimeLong = (function(value) {
  var lo = this.scala$scalajs$runtime$RuntimeLong$$fromDoubleImpl__D__I(value);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f)
});
$c_sjsr_RuntimeLong$.prototype.scala$scalajs$runtime$RuntimeLong$$fromDoubleImpl__D__I = (function(value) {
  if ((value < (-9.223372036854776E18))) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (-2147483648);
    return 0
  } else if ((value >= 9.223372036854776E18)) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 2147483647;
    return (-1)
  } else {
    var rawLo = $uI((value | 0));
    var x = (value / 4.294967296E9);
    var rawHi = $uI((x | 0));
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (((value < 0) && (rawLo !== 0)) ? (((-1) + rawHi) | 0) : rawHi);
    return rawLo
  }
});
$c_sjsr_RuntimeLong$.prototype.unsigned$und$div__p1__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if ((((-2097152) & ahi) === 0)) {
    if ((((-2097152) & bhi) === 0)) {
      var aDouble = ((4.294967296E9 * ahi) + $uD((alo >>> 0)));
      var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0)));
      var rDouble = (aDouble / bDouble);
      var x = (rDouble / 4.294967296E9);
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = $uI((x | 0));
      return $uI((rDouble | 0))
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      return 0
    }
  } else if (((bhi === 0) && ((blo & (((-1) + blo) | 0)) === 0))) {
    var pow = ((31 - $clz32(blo)) | 0);
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ((ahi >>> pow) | 0);
    return (((alo >>> pow) | 0) | ((ahi << 1) << ((31 - pow) | 0)))
  } else if (((blo === 0) && ((bhi & (((-1) + bhi) | 0)) === 0))) {
    var pow$2 = ((31 - $clz32(bhi)) | 0);
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
    return ((ahi >>> pow$2) | 0)
  } else {
    return $uI(this.unsignedDivModHelper__p1__I__I__I__I__I__sjs_js_$bar(alo, ahi, blo, bhi, 0))
  }
});
$c_sjsr_RuntimeLong$.prototype.divideUnsignedImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException().init___T("/ by zero")
  };
  if ((ahi === 0)) {
    if ((bhi === 0)) {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      var x = ($uD((alo >>> 0)) / $uD((blo >>> 0)));
      return $uI((x | 0))
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      return 0
    }
  } else {
    return this.unsigned$und$div__p1__I__I__I__I__I(alo, ahi, blo, bhi)
  }
});
$c_sjsr_RuntimeLong$.prototype.scala$scalajs$runtime$RuntimeLong$$toString__I__I__T = (function(lo, hi) {
  return ((hi === (lo >> 31)) ? ("" + lo) : ((hi < 0) ? ("-" + this.toUnsignedString__p1__I__I__T(((-lo) | 0), ((lo !== 0) ? (~hi) : ((-hi) | 0)))) : this.toUnsignedString__p1__I__I__T(lo, hi)))
});
$c_sjsr_RuntimeLong$.prototype.scala$scalajs$runtime$RuntimeLong$$compare__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  return ((ahi === bhi) ? ((alo === blo) ? 0 : ((((-2147483648) ^ alo) < ((-2147483648) ^ blo)) ? (-1) : 1)) : ((ahi < bhi) ? (-1) : 1))
});
$c_sjsr_RuntimeLong$.prototype.unsignedDivModHelper__p1__I__I__I__I__I__sjs_js_$bar = (function(alo, ahi, blo, bhi, ask) {
  var shift = ((((bhi !== 0) ? $clz32(bhi) : ((32 + $clz32(blo)) | 0)) - ((ahi !== 0) ? $clz32(ahi) : ((32 + $clz32(alo)) | 0))) | 0);
  var n = shift;
  var lo = (((32 & n) === 0) ? (blo << n) : 0);
  var hi = (((32 & n) === 0) ? (((((blo >>> 1) | 0) >>> ((31 - n) | 0)) | 0) | (bhi << n)) : (blo << n));
  var bShiftLo = lo;
  var bShiftHi = hi;
  var remLo = alo;
  var remHi = ahi;
  var quotLo = 0;
  var quotHi = 0;
  while (((shift >= 0) && (((-2097152) & remHi) !== 0))) {
    var alo$1 = remLo;
    var ahi$1 = remHi;
    var blo$1 = bShiftLo;
    var bhi$1 = bShiftHi;
    if (((ahi$1 === bhi$1) ? (((-2147483648) ^ alo$1) >= ((-2147483648) ^ blo$1)) : (((-2147483648) ^ ahi$1) >= ((-2147483648) ^ bhi$1)))) {
      var lo$1 = remLo;
      var hi$1 = remHi;
      var lo$2 = bShiftLo;
      var hi$2 = bShiftHi;
      var lo$3 = ((lo$1 - lo$2) | 0);
      var hi$3 = ((((-2147483648) ^ lo$3) > ((-2147483648) ^ lo$1)) ? (((-1) + ((hi$1 - hi$2) | 0)) | 0) : ((hi$1 - hi$2) | 0));
      remLo = lo$3;
      remHi = hi$3;
      if ((shift < 32)) {
        quotLo = (quotLo | (1 << shift))
      } else {
        quotHi = (quotHi | (1 << shift))
      }
    };
    shift = (((-1) + shift) | 0);
    var lo$4 = bShiftLo;
    var hi$4 = bShiftHi;
    var lo$5 = (((lo$4 >>> 1) | 0) | (hi$4 << 31));
    var hi$5 = ((hi$4 >>> 1) | 0);
    bShiftLo = lo$5;
    bShiftHi = hi$5
  };
  var alo$2 = remLo;
  var ahi$2 = remHi;
  if (((ahi$2 === bhi) ? (((-2147483648) ^ alo$2) >= ((-2147483648) ^ blo)) : (((-2147483648) ^ ahi$2) >= ((-2147483648) ^ bhi)))) {
    var lo$6 = remLo;
    var hi$6 = remHi;
    var remDouble = ((4.294967296E9 * hi$6) + $uD((lo$6 >>> 0)));
    var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0)));
    if ((ask !== 1)) {
      var x = (remDouble / bDouble);
      var lo$7 = $uI((x | 0));
      var x$1 = (x / 4.294967296E9);
      var hi$7 = $uI((x$1 | 0));
      var lo$8 = quotLo;
      var hi$8 = quotHi;
      var lo$9 = ((lo$8 + lo$7) | 0);
      var hi$9 = ((((-2147483648) ^ lo$9) < ((-2147483648) ^ lo$8)) ? ((1 + ((hi$8 + hi$7) | 0)) | 0) : ((hi$8 + hi$7) | 0));
      quotLo = lo$9;
      quotHi = hi$9
    };
    if ((ask !== 0)) {
      var rem_mod_bDouble = (remDouble % bDouble);
      remLo = $uI((rem_mod_bDouble | 0));
      var x$2 = (rem_mod_bDouble / 4.294967296E9);
      remHi = $uI((x$2 | 0))
    }
  };
  if ((ask === 0)) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = quotHi;
    var a = quotLo;
    return a
  } else if ((ask === 1)) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = remHi;
    var a$1 = remLo;
    return a$1
  } else {
    var lo$10 = quotLo;
    var hi$10 = quotHi;
    var quot = ((4.294967296E9 * hi$10) + $uD((lo$10 >>> 0)));
    var this$25 = remLo;
    var remStr = ("" + this$25);
    var a$2 = ((("" + quot) + $as_T("000000000".substring($uI(remStr.length)))) + remStr);
    return a$2
  }
});
$c_sjsr_RuntimeLong$.prototype.remainderImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException().init___T("/ by zero")
  };
  if ((ahi === (alo >> 31))) {
    if ((bhi === (blo >> 31))) {
      if ((blo !== (-1))) {
        var lo = ((alo % blo) | 0);
        this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (lo >> 31);
        return lo
      } else {
        this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
        return 0
      }
    } else if (((alo === (-2147483648)) && ((blo === (-2147483648)) && (bhi === 0)))) {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      return 0
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ahi;
      return alo
    }
  } else {
    var neg = (ahi < 0);
    if (neg) {
      var lo$1 = ((-alo) | 0);
      var hi = ((alo !== 0) ? (~ahi) : ((-ahi) | 0));
      var abs_$_lo$2 = lo$1;
      var abs_$_hi$2 = hi
    } else {
      var abs_$_lo$2 = alo;
      var abs_$_hi$2 = ahi
    };
    var neg$1 = (bhi < 0);
    if (neg$1) {
      var lo$2 = ((-blo) | 0);
      var hi$1 = ((blo !== 0) ? (~bhi) : ((-bhi) | 0));
      var abs$1_$_lo$2 = lo$2;
      var abs$1_$_hi$2 = hi$1
    } else {
      var abs$1_$_lo$2 = blo;
      var abs$1_$_hi$2 = bhi
    };
    var absRLo = this.unsigned$und$percent__p1__I__I__I__I__I(abs_$_lo$2, abs_$_hi$2, abs$1_$_lo$2, abs$1_$_hi$2);
    if (neg) {
      var hi$2 = this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ((absRLo !== 0) ? (~hi$2) : ((-hi$2) | 0));
      return ((-absRLo) | 0)
    } else {
      return absRLo
    }
  }
});
$c_sjsr_RuntimeLong$.prototype.remainderUnsignedImpl__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if (((blo | bhi) === 0)) {
    throw new $c_jl_ArithmeticException().init___T("/ by zero")
  };
  if ((ahi === 0)) {
    if ((bhi === 0)) {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
      var x = ($uD((alo >>> 0)) % $uD((blo >>> 0)));
      return $uI((x | 0))
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ahi;
      return alo
    }
  } else {
    return this.unsigned$und$percent__p1__I__I__I__I__I(alo, ahi, blo, bhi)
  }
});
$c_sjsr_RuntimeLong$.prototype.unsigned$und$percent__p1__I__I__I__I__I = (function(alo, ahi, blo, bhi) {
  if ((((-2097152) & ahi) === 0)) {
    if ((((-2097152) & bhi) === 0)) {
      var aDouble = ((4.294967296E9 * ahi) + $uD((alo >>> 0)));
      var bDouble = ((4.294967296E9 * bhi) + $uD((blo >>> 0)));
      var rDouble = (aDouble % bDouble);
      var x = (rDouble / 4.294967296E9);
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = $uI((x | 0));
      return $uI((rDouble | 0))
    } else {
      this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = ahi;
      return alo
    }
  } else if (((bhi === 0) && ((blo & (((-1) + blo) | 0)) === 0))) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = 0;
    return (alo & (((-1) + blo) | 0))
  } else if (((blo === 0) && ((bhi & (((-1) + bhi) | 0)) === 0))) {
    this.scala$scalajs$runtime$RuntimeLong$$hiReturn$f = (ahi & (((-1) + bhi) | 0));
    return alo
  } else {
    return $uI(this.unsignedDivModHelper__p1__I__I__I__I__I__sjs_js_$bar(alo, ahi, blo, bhi, 1))
  }
});
var $d_sjsr_RuntimeLong$ = new $TypeData().initClass({
  sjsr_RuntimeLong$: 0
}, false, "scala.scalajs.runtime.RuntimeLong$", {
  sjsr_RuntimeLong$: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sjsr_RuntimeLong$.prototype.$classData = $d_sjsr_RuntimeLong$;
var $n_sjsr_RuntimeLong$ = (void 0);
function $m_sjsr_RuntimeLong$() {
  if ((!$n_sjsr_RuntimeLong$)) {
    $n_sjsr_RuntimeLong$ = new $c_sjsr_RuntimeLong$().init___()
  };
  return $n_sjsr_RuntimeLong$
}
var $d_sr_Nothing$ = new $TypeData().initClass({
  sr_Nothing$: 0
}, false, "scala.runtime.Nothing$", {
  sr_Nothing$: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
function $s_LLoan__inBreach__LLoan__Z($this) {
  if ($as_s_Option($this.nextPayment).isDefined__Z()) {
    var this$2 = $as_s_math_Ordered($as_T2($as_s_Option($this.nextPayment).get__O()).$$und1__O());
    var this$1 = $m_s_math_BigInt$();
    var i = $uI(new $g.Date().getUTCMilliseconds());
    var that = this$1.apply__I__s_math_BigInt(i);
    return $f_s_math_Ordered__$$less__O__Z(this$2, that)
  } else {
    return false
  }
}
function $s_LLoan__isFinished__LLoan__Z($this) {
  return $as_s_Option($this.nextPayment).isEmpty__Z()
}
function $s_LLoan__updated__LLoan__s_math_BigInt__s_math_BigInt__s_math_BigInt__s_Option__s_Option__LLoan($this, newPrincipal, newAccruedInterest, newTotalPaid, newLastPayment, newNextPayment) {
  return new ($a_LLoan())(newPrincipal, newAccruedInterest, $as_s_math_BigInt($this.paymentInterval), $as_s_math_BigDecimal($this.interestRate), $as_s_math_BigInt($this.interestInterval), $as_s_math_BigInt($this.startingDate), newTotalPaid, newLastPayment, newNextPayment)
}
function $s_LLoan__makePayment__LLoan__T__LLoan($this, amt) {
  var amount = $m_s_package$().BigInt__s_math_BigInt$().apply__T__s_math_BigInt(amt);
  if ($as_s_Option($this.nextPayment).isEmpty__Z()) {
    return $this
  } else {
    var newAccruedInterest = $as_s_math_BigInt($this.accruedInterest);
    var newPrincipal = $as_s_math_BigInt($this.principal);
    var this$1 = $as_s_math_BigInt($this.accruedInterest);
    if ($f_s_math_Ordered__$$less$eq__O__Z(this$1, amount)) {
      var this$2 = $m_s_math_BigInt$();
      newAccruedInterest = this$2.apply__I__s_math_BigInt(0);
      var jsx$1 = $as_s_math_BigInt($this.principal).$$minus__s_math_BigInt__s_math_BigInt(amount.$$minus__s_math_BigInt__s_math_BigInt($as_s_math_BigInt($this.accruedInterest)));
      var this$3 = $m_s_math_BigInt$();
      newPrincipal = jsx$1.max__s_math_BigInt__s_math_BigInt(this$3.apply__I__s_math_BigInt(0))
    } else {
      newAccruedInterest = newAccruedInterest.$$minus__s_math_BigInt__s_math_BigInt(amount)
    };
    var this$4 = $m_s_math_BigInt$();
    var i = $uI(new $g.Date().getUTCMilliseconds());
    var justPaid = new $c_T2().init___O__O(this$4.apply__I__s_math_BigInt(i), amount);
    var that = $as_T2($as_s_Option($this.nextPayment).get__O()).$$und2__O();
    if ($f_s_math_Ordered__$$greater$eq__O__Z(amount, that)) {
      var jsx$4 = $as_s_math_BigDecimal($this.interestRate);
      var this$5 = $m_s_package$().BigDecimal__s_math_BigDecimal$();
      var x = $as_s_math_BigInt($this.paymentInterval);
      var jsx$3 = this$5.exact__s_math_BigInt__s_math_BigDecimal(x);
      var this$6 = $m_s_package$().BigDecimal__s_math_BigDecimal$();
      var x$1 = $as_s_math_BigInt($this.interestInterval);
      var jsx$2 = jsx$4.$$times__s_math_BigDecimal__s_math_BigDecimal(jsx$3.$$div__s_math_BigDecimal__s_math_BigDecimal(this$6.exact__s_math_BigInt__s_math_BigDecimal(x$1)));
      var this$7 = $m_s_package$().BigDecimal__s_math_BigDecimal$();
      var x$2 = $as_s_math_BigInt($this.principal);
      var paymentAmount = jsx$2.$$times__s_math_BigDecimal__s_math_BigDecimal(this$7.exact__s_math_BigInt__s_math_BigDecimal(x$2));
      var this$8 = $m_s_math_BigDecimal$();
      var that$1 = this$8.apply__I__Ljava_math_MathContext__s_math_BigDecimal(0, this$8.defaultMathContext$1);
      if ($f_s_math_Ordered__$$greater__O__Z(paymentAmount, that$1)) {
        var this$9 = $m_s_math_BigInt$();
        var i$1 = $uI(new $g.Date().getUTCMilliseconds());
        var nextToPay = new $c_s_Some().init___O(new $c_T2().init___O__O(this$9.apply__I__s_math_BigInt(i$1).$$plus__s_math_BigInt__s_math_BigInt($as_s_math_BigInt($this.paymentInterval)), paymentAmount.toBigInt__s_math_BigInt()))
      } else {
        var nextToPay = $m_s_None$()
      }
    } else {
      var this$10 = $as_s_Option($this.nextPayment);
      if (this$10.isEmpty__Z()) {
        var nextToPay = $m_s_None$()
      } else {
        var arg1 = this$10.get__O();
        var n = $as_T2(arg1);
        var self = n.$$und1__O();
        var y = $as_s_math_BigInt(n.$$und2__O()).$$minus__s_math_BigInt__s_math_BigInt(amount);
        var nextToPay = new $c_s_Some().init___O(new $c_T2().init___O__O(self, y))
      }
    };
    return $this.updated(newPrincipal, newAccruedInterest, $as_s_math_BigInt($this.totalPaid).$$plus__s_math_BigInt__s_math_BigInt(amount), new $c_s_Some().init___O(justPaid), nextToPay)
  }
}
var $b_LLoan = (void 0);
function $a_LLoan() {
  if ((!$b_LLoan)) {
    /** @constructor */
    var $c_LLoan = (function $c_LLoan(arg$1, arg$2, arg$3, arg$4, arg$5, arg$6, arg$7, arg$8, arg$9) {
      var principal = $as_s_math_BigInt(arg$1);
      var accruedInterest = $as_s_math_BigInt(arg$2);
      var paymentInterval = $as_s_math_BigInt(arg$3);
      var interestRate = $as_s_math_BigDecimal(arg$4);
      var interestInterval = $as_s_math_BigInt(arg$5);
      var startingDate = $as_s_math_BigInt(arg$6);
      var totalPaid = $as_s_math_BigInt(arg$7);
      var lastPayment = $as_s_Option(arg$8);
      var nextPayment = $as_s_Option(arg$9);
      $a_LBaseModule().call(this);
      $g.Object.defineProperty(this, "principal", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "accruedInterest", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "paymentInterval", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "interestRate", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "interestInterval", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "startingDate", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "totalPaid", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "lastPayment", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "nextPayment", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "jsonSerializer", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "initialCapital", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "effectiveDate", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      $g.Object.defineProperty(this, "expirationDate", {
        "configurable": true,
        "enumerable": true,
        "writable": true,
        "value": null
      });
      this.principal = principal;
      this.accruedInterest = accruedInterest;
      this.paymentInterval = paymentInterval;
      this.interestRate = interestRate;
      this.interestInterval = interestInterval;
      this.startingDate = startingDate;
      this.totalPaid = totalPaid;
      this.lastPayment = lastPayment;
      this.nextPayment = nextPayment;
      this.jsonSerializer = $m_LLoan$();
      this.initialCapital = principal;
      this.effectiveDate = startingDate;
      this.expirationDate = $m_s_None$()
    });
    /** @constructor */
    var $h_LLoan = (function $h_LLoan() {
      /*<skip>*/
    });
    $h_LLoan.prototype = $a_LBaseModule().prototype;
    $c_LLoan.prototype = new $h_LLoan();
    $c_LLoan.prototype.constructor = $c_LLoan;
    $c_LLoan.prototype.inBreach = (function() {
      return $s_LLoan__inBreach__LLoan__Z(this)
    });
    $c_LLoan.prototype.isFinished = (function() {
      return $s_LLoan__isFinished__LLoan__Z(this)
    });
    $c_LLoan.prototype.updated = (function(arg$1, arg$2, arg$3, arg$4, arg$5) {
      var prep0 = $as_s_math_BigInt(arg$1);
      var prep1 = $as_s_math_BigInt(arg$2);
      var prep2 = $as_s_math_BigInt(arg$3);
      var prep3 = $as_s_Option(arg$4);
      var prep4 = $as_s_Option(arg$5);
      return $s_LLoan__updated__LLoan__s_math_BigInt__s_math_BigInt__s_math_BigInt__s_Option__s_Option__LLoan(this, prep0, prep1, prep2, prep3, prep4)
    });
    $c_LLoan.prototype.makePayment = (function(arg$1) {
      var prep0 = $as_T(arg$1);
      return $s_LLoan__makePayment__LLoan__T__LLoan(this, prep0)
    });
    $c_LLoan.fromJSON = (function(arg$1) {
      var prep0 = $as_T(arg$1);
      return $m_LLoan$().fromJSON__T__LLoan(prep0)
    });
    $c_LLoan.construct = (function(arg$1) {
      var prep0 = arg$1;
      return $m_LLoan$().apply__sjs_js_Object__LLoan(prep0)
    });
    $c_LLoan.toJSON = (function(arg$1) {
      var prep0 = arg$1;
      return $m_LLoan$().toJSON__LLoan__T(prep0)
    });
    $b_LLoan = $c_LLoan
  };
  return $b_LLoan
}
/** @constructor */
function $c_Ljava_io_FilterOutputStream() {
  $c_Ljava_io_OutputStream.call(this);
  this.out$2 = null
}
$c_Ljava_io_FilterOutputStream.prototype = new $h_Ljava_io_OutputStream();
$c_Ljava_io_FilterOutputStream.prototype.constructor = $c_Ljava_io_FilterOutputStream;
/** @constructor */
function $h_Ljava_io_FilterOutputStream() {
  /*<skip>*/
}
$h_Ljava_io_FilterOutputStream.prototype = $c_Ljava_io_FilterOutputStream.prototype;
$c_Ljava_io_FilterOutputStream.prototype.init___Ljava_io_OutputStream = (function(out) {
  this.out$2 = out;
  return this
});
/** @constructor */
function $c_Ljava_math_BigDecimal() {
  $c_jl_Number.call(this);
  this.$$undtoStringImage$2 = null;
  this.$$undhashCode$2 = 0;
  this.$$undintVal$2 = null;
  this.java$math$BigDecimal$$$undbitLength$2 = 0;
  this.java$math$BigDecimal$$$undsmallValue$2 = $m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong();
  this.java$math$BigDecimal$$$undscale$2 = 0;
  this.$$undprecision$2 = 0
}
$c_Ljava_math_BigDecimal.prototype = new $h_jl_Number();
$c_Ljava_math_BigDecimal.prototype.constructor = $c_Ljava_math_BigDecimal;
/** @constructor */
function $h_Ljava_math_BigDecimal() {
  /*<skip>*/
}
$h_Ljava_math_BigDecimal.prototype = $c_Ljava_math_BigDecimal.prototype;
$c_Ljava_math_BigDecimal.prototype.divide__Ljava_math_BigDecimal__Ljava_math_MathContext__Ljava_math_BigDecimal = (function(divisor, mc) {
  if ((((mc.precision$1 === 0) || this.isZero__p2__Z()) || divisor.isZero__p2__Z())) {
    return this.divide__Ljava_math_BigDecimal__Ljava_math_BigDecimal(divisor)
  };
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi = (value >> 31);
  var value$1 = divisor.java$math$BigDecimal$$$undscale$2;
  var hi$1 = (value$1 >> 31);
  var lo = ((value - value$1) | 0);
  var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  var value$2 = mc.precision$1;
  var hi$3 = (value$2 >> 31);
  var lo$1 = ((2 + value$2) | 0);
  var hi$4 = ((((-2147483648) ^ lo$1) < (-2147483646)) ? ((1 + hi$3) | 0) : hi$3);
  var value$3 = divisor.approxPrecision__p2__I();
  var hi$5 = (value$3 >> 31);
  var lo$2 = ((lo$1 + value$3) | 0);
  var hi$6 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ lo$1)) ? ((1 + ((hi$4 + hi$5) | 0)) | 0) : ((hi$4 + hi$5) | 0));
  var value$4 = this.approxPrecision__p2__I();
  var hi$7 = (value$4 >> 31);
  var lo$3 = ((lo$2 - value$4) | 0);
  var hi$8 = ((((-2147483648) ^ lo$3) > ((-2147483648) ^ lo$2)) ? (((-1) + ((hi$6 - hi$7) | 0)) | 0) : ((hi$6 - hi$7) | 0));
  if (((hi$8 === 0) ? (lo$3 !== 0) : (hi$8 > 0))) {
    var q = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger().multiply__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_Multiplication$().powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo$3, hi$8)));
    var lo$4 = ((lo + lo$3) | 0);
    var hi$9 = ((((-2147483648) ^ lo$4) < ((-2147483648) ^ lo)) ? ((1 + ((hi$2 + hi$8) | 0)) | 0) : ((hi$2 + hi$8) | 0));
    var x1_$_$$und1$f = q;
    var x1_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(lo$4, hi$9)
  } else {
    var _1 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var x1_$_$$und1$f = _1;
    var x1_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)
  };
  var quot = $as_Ljava_math_BigInteger(x1_$_$$und1$f);
  var t = $uJ(x1_$_$$und2$f);
  var lo$5 = t.lo$2;
  var hi$10 = t.hi$2;
  var t$1 = $uJ(new $c_sjsr_RuntimeLong().init___I__I(lo$5, hi$10));
  var lo$6 = t$1.lo$2;
  var hi$11 = t$1.hi$2;
  var qr = quot.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem(divisor.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger());
  var this$6 = qr.rem$1;
  if ((this$6.sign$2 !== 0)) {
    var compRem = qr.rem$1.shiftLeftOneBit__Ljava_math_BigInteger().compareTo__Ljava_math_BigInteger__I(divisor.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger());
    var jsx$1 = $m_Ljava_math_BigInteger$();
    var this$7 = qr.quot$1;
    var value$5 = $imul(this$7.sign$2, ((5 + compRem) | 0));
    var hi$12 = (value$5 >> 31);
    var bi = jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$5, hi$12));
    var this$8 = qr.quot$1.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_BigInteger$().TEN$1);
    var _1$1 = $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$8, bi);
    var lo$7 = ((1 + lo$6) | 0);
    var hi$13 = ((lo$7 === 0) ? ((1 + hi$11) | 0) : hi$11);
    var x1$2_$_$$und1$f = _1$1;
    var x1$2_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(lo$7, hi$13)
  } else {
    var lastPow = (((-1) + $m_Ljava_math_Multiplication$().BigTenPows$1.u.length) | 0);
    var i = 1;
    var iq = qr.quot$1;
    var scale_$_lo$2 = lo$6;
    var scale_$_hi$2 = hi$11;
    var x1$2_$_$$und1$f;
    var x1$2_$_$$und2$f;
    _loop: while (true) {
      if ((!iq.testBit__I__Z(0))) {
        var qr$1 = iq.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem($m_Ljava_math_Multiplication$().BigTenPows$1.get(i));
        var this$10 = qr$1.rem$1;
        if ((this$10.sign$2 === 0)) {
          var this$11_$_lo$2 = scale_$_lo$2;
          var this$11_$_hi$2 = scale_$_hi$2;
          var value$6 = i;
          var hi$14 = (value$6 >> 31);
          var alo = this$11_$_lo$2;
          var ahi = this$11_$_hi$2;
          var lo$8 = ((alo - value$6) | 0);
          var hi$15 = ((((-2147483648) ^ lo$8) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi - hi$14) | 0)) | 0) : ((ahi - hi$14) | 0));
          var jsx$2 = ((hi$15 === hi$2) ? (((-2147483648) ^ lo$8) >= ((-2147483648) ^ lo)) : (hi$15 > hi$2))
        } else {
          var jsx$2 = false
        };
        if (jsx$2) {
          var temp$i = ((i < lastPow) ? ((1 + i) | 0) : i);
          var temp$iq = qr$1.quot$1;
          var this$13_$_lo$2 = scale_$_lo$2;
          var this$13_$_hi$2 = scale_$_hi$2;
          var value$7 = i;
          var hi$16 = (value$7 >> 31);
          var alo$1 = this$13_$_lo$2;
          var ahi$1 = this$13_$_hi$2;
          var lo$9 = ((alo$1 - value$7) | 0);
          var hi$17 = ((((-2147483648) ^ lo$9) > ((-2147483648) ^ alo$1)) ? (((-1) + ((ahi$1 - hi$16) | 0)) | 0) : ((ahi$1 - hi$16) | 0));
          i = temp$i;
          iq = temp$iq;
          var jsx$3_$_lo$2 = lo$9;
          var jsx$3_$_hi$2 = hi$17;
          scale_$_lo$2 = jsx$3_$_lo$2;
          scale_$_hi$2 = jsx$3_$_hi$2;
          continue _loop
        };
        if ((i !== 1)) {
          i = 1;
          continue _loop
        };
        var _1$2 = iq;
        var _2_$_lo$2 = scale_$_lo$2;
        var _2_$_hi$2 = scale_$_hi$2;
        var x1$2_$_$$und1$f = _1$2;
        var x1$2_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(_2_$_lo$2, _2_$_hi$2);
        break
      } else {
        var _1$3 = iq;
        var _2$1_$_lo$2 = scale_$_lo$2;
        var _2$1_$_hi$2 = scale_$_hi$2;
        var x1$2_$_$$und1$f = _1$3;
        var x1$2_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(_2$1_$_lo$2, _2$1_$_hi$2);
        break
      }
    }
  };
  var integerQuot = $as_Ljava_math_BigInteger(x1$2_$_$$und1$f);
  var t$2 = $uJ(x1$2_$_$$und2$f);
  var lo$10 = t$2.lo$2;
  var hi$18 = t$2.hi$2;
  var t$3 = $uJ(new $c_sjsr_RuntimeLong().init___I__I(lo$10, hi$18));
  var lo$11 = t$3.lo$2;
  var hi$19 = t$3.hi$2;
  return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I__Ljava_math_MathContext(integerQuot, $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$11, hi$19)), mc)
});
$c_Ljava_math_BigDecimal.prototype.longValue__J = (function() {
  return (((this.java$math$BigDecimal$$$undscale$2 <= (-64)) || (this.java$math$BigDecimal$$$undscale$2 > this.approxPrecision__p2__I())) ? $m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong() : this.toBigInteger__Ljava_math_BigInteger().longValue__J())
});
$c_Ljava_math_BigDecimal.prototype.init___ = (function() {
  this.$$undtoStringImage$2 = null;
  this.$$undhashCode$2 = 0;
  this.java$math$BigDecimal$$$undbitLength$2 = 0;
  this.java$math$BigDecimal$$$undsmallValue$2 = $m_sjsr_RuntimeLong$().Zero__sjsr_RuntimeLong();
  this.java$math$BigDecimal$$$undscale$2 = 0;
  this.$$undprecision$2 = 0;
  return this
});
$c_Ljava_math_BigDecimal.prototype.valueExact__p2__I__J = (function(bitLengthOfType) {
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi = (value >> 31);
  var lo = ((-value) | 0);
  var hi$1 = ((value !== 0) ? (~hi) : ((-hi) | 0));
  var value$1 = this.approxPrecision__p2__I();
  var hi$2 = (value$1 >> 31);
  var lo$1 = ((lo + value$1) | 0);
  var hi$3 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + ((hi$1 + hi$2) | 0)) | 0) : ((hi$1 + hi$2) | 0));
  if (((hi$3 === 0) ? (((-2147483648) ^ lo$1) > (-2147483629)) : (hi$3 > 0))) {
    throw new $c_jl_ArithmeticException().init___T("Rounding necessary")
  };
  var bigInteger = this.toBigIntegerExact__Ljava_math_BigInteger();
  if (($m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(bigInteger) < bitLengthOfType)) {
    return bigInteger.longValue__J()
  } else {
    throw new $c_jl_ArithmeticException().init___T("Rounding necessary")
  }
});
$c_Ljava_math_BigDecimal.prototype.multiply__Ljava_math_BigDecimal__Ljava_math_MathContext__Ljava_math_BigDecimal = (function(multiplicand, mc) {
  var result = this.multiply__Ljava_math_BigDecimal__Ljava_math_BigDecimal(multiplicand);
  result.inplaceRound__p2__Ljava_math_MathContext__V(mc);
  return result
});
$c_Ljava_math_BigDecimal.prototype.equals__O__Z = (function(x) {
  if ($is_Ljava_math_BigDecimal(x)) {
    var x2 = $as_Ljava_math_BigDecimal(x);
    if ((x2.java$math$BigDecimal$$$undscale$2 === this.java$math$BigDecimal$$$undscale$2)) {
      if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
        var t = x2.java$math$BigDecimal$$$undsmallValue$2;
        var lo = t.lo$2;
        var hi = t.hi$2;
        var b = this.java$math$BigDecimal$$$undsmallValue$2;
        return ((lo === b.lo$2) && (hi === b.hi$2))
      } else {
        return $m_sr_BoxesRunTime$().equalsNumNum__jl_Number__jl_Number__Z(this.$$undintVal$2, x2.$$undintVal$2)
      }
    } else {
      return false
    }
  } else {
    return false
  }
});
$c_Ljava_math_BigDecimal.prototype.init___J__Ljava_math_MathContext = (function(lVal, mc) {
  $c_Ljava_math_BigDecimal.prototype.init___J__I.call(this, lVal, 0);
  this.inplaceRound__p2__Ljava_math_MathContext__V(mc);
  return this
});
$c_Ljava_math_BigDecimal.prototype.intValueExact__I = (function() {
  var t = this.valueExact__p2__I__J(32);
  var lo = t.lo$2;
  return lo
});
$c_Ljava_math_BigDecimal.prototype.isZero__p2__Z = (function() {
  if ((this.java$math$BigDecimal$$$undbitLength$2 === 0)) {
    var t = this.java$math$BigDecimal$$$undsmallValue$2;
    var lo = t.lo$2;
    var hi = t.hi$2;
    return (!((lo === (-1)) && (hi === (-1))))
  } else {
    return false
  }
});
$c_Ljava_math_BigDecimal.prototype.stripTrailingZeros__Ljava_math_BigDecimal = (function() {
  if (this.isZero__p2__Z()) {
    return this
  } else {
    var lastPow = (((-1) + $m_Ljava_math_Multiplication$().BigTenPows$1.u.length) | 0);
    var i = 1;
    var strippedBI = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var value = this.java$math$BigDecimal$$$undscale$2;
    var hi = (value >> 31);
    var scale_$_lo$2 = value;
    var scale_$_hi$2 = hi;
    var x1_$_$$und1$f;
    var x1_$_$$und2$f;
    _loop: while (true) {
      if ((!strippedBI.testBit__I__Z(0))) {
        var qr = strippedBI.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem($m_Ljava_math_Multiplication$().BigTenPows$1.get(i));
        var this$1 = qr.rem$1;
        if ((this$1.sign$2 === 0)) {
          var temp$i = ((i < lastPow) ? ((1 + i) | 0) : i);
          var temp$strippedBI = qr.quot$1;
          var this$2_$_lo$2 = scale_$_lo$2;
          var this$2_$_hi$2 = scale_$_hi$2;
          var value$1 = i;
          var hi$1 = (value$1 >> 31);
          var alo = this$2_$_lo$2;
          var ahi = this$2_$_hi$2;
          var lo = ((alo - value$1) | 0);
          var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi - hi$1) | 0)) | 0) : ((ahi - hi$1) | 0));
          i = temp$i;
          strippedBI = temp$strippedBI;
          var jsx$1_$_lo$2 = lo;
          var jsx$1_$_hi$2 = hi$2;
          scale_$_lo$2 = jsx$1_$_lo$2;
          scale_$_hi$2 = jsx$1_$_hi$2;
          continue _loop
        };
        if ((i !== 1)) {
          i = 1;
          continue _loop
        };
        var _1 = strippedBI;
        var _2_$_lo$2 = scale_$_lo$2;
        var _2_$_hi$2 = scale_$_hi$2;
        var x1_$_$$und1$f = _1;
        var x1_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(_2_$_lo$2, _2_$_hi$2);
        break
      } else {
        var _1$1 = strippedBI;
        var _2$1_$_lo$2 = scale_$_lo$2;
        var _2$1_$_hi$2 = scale_$_hi$2;
        var x1_$_$$und1$f = _1$1;
        var x1_$_$$und2$f = new $c_sjsr_RuntimeLong().init___I__I(_2$1_$_lo$2, _2$1_$_hi$2);
        break
      }
    };
    var strippedBI$1 = $as_Ljava_math_BigInteger(x1_$_$$und1$f);
    var t = $uJ(x1_$_$$und2$f);
    var lo$1 = t.lo$2;
    var hi$3 = t.hi$2;
    var t$1 = $uJ(new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$3));
    var lo$2 = t$1.lo$2;
    var hi$4 = t$1.hi$2;
    return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I(strippedBI$1, $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$2, hi$4)))
  }
});
$c_Ljava_math_BigDecimal.prototype.inplaceRound__p2__Ljava_math_MathContext__V = (function(mc) {
  var mcPrecision = mc.precision$1;
  var discardedPrecision = ((this.precision__I() - mcPrecision) | 0);
  var mcPrecGood = ((this.approxPrecision__p2__I() < mcPrecision) || (mcPrecision === 0));
  if ((!(mcPrecGood || (discardedPrecision <= 0)))) {
    if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
      this.smallRound__p2__Ljava_math_MathContext__I__V(mc, discardedPrecision)
    } else {
      var jsx$1 = $m_Ljava_math_Multiplication$();
      var hi = (discardedPrecision >> 31);
      var sizeOfFraction = jsx$1.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(discardedPrecision, hi));
      var integerAndFraction = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger().divideAndRemainder__Ljava_math_BigInteger__ALjava_math_BigInteger(sizeOfFraction);
      var value = this.java$math$BigDecimal$$$undscale$2;
      var hi$1 = (value >> 31);
      var hi$2 = (discardedPrecision >> 31);
      var lo = ((value - discardedPrecision) | 0);
      var hi$3 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi$1 - hi$2) | 0)) | 0) : ((hi$1 - hi$2) | 0));
      var this$2 = integerAndFraction.get(1);
      if ((this$2.sign$2 !== 0)) {
        var absBi = integerAndFraction.get(1).abs__Ljava_math_BigInteger();
        var compRem = absBi.shiftLeftOneBit__Ljava_math_BigInteger().compareTo__Ljava_math_BigInteger__I(sizeOfFraction);
        var parityBit = (integerAndFraction.get(0).testBit__I__Z(0) ? 1 : 0);
        var this$3 = integerAndFraction.get(1);
        var frac = $imul(this$3.sign$2, ((5 + compRem) | 0));
        var carry = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$roundingBehavior__I__I__Ljava_math_RoundingMode__I(parityBit, frac, mc.roundingMode$1);
        if ((carry !== 0)) {
          var jsx$2 = $m_Ljava_math_BigInteger$();
          var hi$4 = (carry >> 31);
          var bi = jsx$2.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(carry, hi$4));
          var this$4 = integerAndFraction.get(0);
          integerAndFraction.set(0, $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$4, bi))
        };
        var tempBD = new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger(integerAndFraction.get(0));
        if ((tempBD.precision__I() > mcPrecision)) {
          integerAndFraction.set(0, integerAndFraction.get(0).divide__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_BigInteger$().TEN$1));
          var lo$1 = (((-1) + lo) | 0);
          var hi$5 = ((lo$1 !== (-1)) ? hi$3 : (((-1) + hi$3) | 0));
          var newScale_$_lo$2 = lo$1;
          var newScale_$_hi$2 = hi$5
        } else {
          var newScale_$_lo$2 = lo;
          var newScale_$_hi$2 = hi$3
        }
      } else {
        var newScale_$_lo$2 = lo;
        var newScale_$_hi$2 = hi$3
      };
      this.java$math$BigDecimal$$$undscale$2 = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(newScale_$_lo$2, newScale_$_hi$2));
      this.$$undprecision$2 = mcPrecision;
      this.setUnscaledValue__p2__Ljava_math_BigInteger__V(integerAndFraction.get(0))
    }
  }
});
$c_Ljava_math_BigDecimal.prototype.init___Ljava_math_BigInteger__I = (function(unscaledVal, scale) {
  $c_Ljava_math_BigDecimal.prototype.init___.call(this);
  if ((unscaledVal === null)) {
    throw new $c_jl_NullPointerException().init___T("unscaledVal == null")
  };
  this.java$math$BigDecimal$$$undscale$2 = scale;
  this.setUnscaledValue__p2__Ljava_math_BigInteger__V(unscaledVal);
  return this
});
$c_Ljava_math_BigDecimal.prototype.toString__T = (function() {
  if ((this.$$undtoStringImage$2 !== null)) {
    return this.$$undtoStringImage$2
  } else if ((this.java$math$BigDecimal$$$undbitLength$2 < 32)) {
    this.$$undtoStringImage$2 = $m_Ljava_math_Conversion$().toDecimalScaledString__J__I__T(this.java$math$BigDecimal$$$undsmallValue$2, this.java$math$BigDecimal$$$undscale$2);
    return this.$$undtoStringImage$2
  } else {
    var this$1 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var intString = $m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$1);
    if ((this.java$math$BigDecimal$$$undscale$2 === 0)) {
      return intString
    } else {
      var this$2 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
      if ((this$2.sign$2 < 0)) {
        var begin = 2
      } else {
        var begin = 1
      };
      var end = $uI(intString.length);
      var value = this.java$math$BigDecimal$$$undscale$2;
      var hi = (value >> 31);
      var lo = ((-value) | 0);
      var hi$1 = ((value !== 0) ? (~hi) : ((-hi) | 0));
      var hi$2 = (end >> 31);
      var lo$1 = ((lo + end) | 0);
      var hi$3 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + ((hi$1 + hi$2) | 0)) | 0) : ((hi$1 + hi$2) | 0));
      var hi$4 = (begin >> 31);
      var lo$2 = ((lo$1 - begin) | 0);
      var hi$5 = ((((-2147483648) ^ lo$2) > ((-2147483648) ^ lo$1)) ? (((-1) + ((hi$3 - hi$4) | 0)) | 0) : ((hi$3 - hi$4) | 0));
      if (((this.java$math$BigDecimal$$$undscale$2 > 0) && ((hi$5 === (-1)) ? (((-2147483648) ^ lo$2) >= 2147483642) : (hi$5 > (-1))))) {
        if ((hi$5 >= 0)) {
          $m_Ljava_math_BigDecimal$();
          var pos = ((end - this.java$math$BigDecimal$$$undscale$2) | 0);
          $m_Ljava_math_BigDecimal$();
          var result = (($as_T(intString.substring(0, pos)) + ".") + $as_T(intString.substring(pos)))
        } else {
          $m_Ljava_math_BigDecimal$();
          $m_Ljava_math_BigDecimal$();
          var pos$1 = (((-1) + begin) | 0);
          $m_Ljava_math_BigDecimal$();
          var s = (($as_T(intString.substring(0, pos$1)) + "0.") + $as_T(intString.substring(pos$1)));
          var pos$2 = ((1 + begin) | 0);
          var xs = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$CharZeros$1;
          var b = new $c_scm_StringBuilder().init___();
          var elem$1 = false;
          elem$1 = true;
          b.append__T__scm_StringBuilder("");
          var i = 0;
          var len = xs.u.length;
          while ((i < len)) {
            var idx = i;
            var c = xs.get(idx);
            var arg1 = new $c_jl_Character().init___C(c);
            if (elem$1) {
              b.append__O__scm_StringBuilder(arg1);
              elem$1 = false
            } else {
              b.append__T__scm_StringBuilder("");
              b.append__O__scm_StringBuilder(arg1)
            };
            i = ((1 + i) | 0)
          };
          b.append__T__scm_StringBuilder("");
          var this$29 = b.underlying$5;
          var s2 = this$29.content$1;
          var s2Len = (((-1) - lo$2) | 0);
          $m_Ljava_math_BigDecimal$();
          var s2$1 = $as_T(s2.substring(0, s2Len));
          var result = ((("" + $as_T(s.substring(0, pos$2))) + s2$1) + $as_T(s.substring(pos$2)))
        }
      } else {
        var r0 = ((((end - begin) | 0) >= 1) ? ($m_Ljava_math_BigDecimal$(), $m_Ljava_math_BigDecimal$(), (($as_T(intString.substring(0, begin)) + ".") + $as_T(intString.substring(begin)))) : intString);
        var r1 = (r0 + "E");
        var r2 = (((hi$5 === 0) ? (lo$2 !== 0) : (hi$5 > 0)) ? (r1 + "+") : r1);
        var result = (r2 + $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toString__I__I__T(lo$2, hi$5))
      };
      this.$$undtoStringImage$2 = result;
      return this.$$undtoStringImage$2
    }
  }
});
$c_Ljava_math_BigDecimal.prototype.init___I__I = (function(smallValue, scale) {
  $c_Ljava_math_BigDecimal.prototype.init___.call(this);
  var hi = (smallValue >> 31);
  this.java$math$BigDecimal$$$undsmallValue$2 = new $c_sjsr_RuntimeLong().init___I__I(smallValue, hi);
  this.java$math$BigDecimal$$$undscale$2 = scale;
  this.java$math$BigDecimal$$$undbitLength$2 = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$bitLength__I__I(smallValue);
  return this
});
$c_Ljava_math_BigDecimal.prototype.signum__I = (function() {
  if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
    var t = this.java$math$BigDecimal$$$undsmallValue$2;
    var hi = t.hi$2;
    if ((hi < 0)) {
      return (-1)
    } else {
      var t$1 = this.java$math$BigDecimal$$$undsmallValue$2;
      var lo$1 = t$1.lo$2;
      var hi$1 = t$1.hi$2;
      if (((hi$1 === 0) ? (lo$1 !== 0) : (hi$1 > 0))) {
        return 1
      } else {
        return 0
      }
    }
  } else {
    var this$1 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    return this$1.sign$2
  }
});
$c_Ljava_math_BigDecimal.prototype.toBigInteger__Ljava_math_BigInteger = (function() {
  if (((this.java$math$BigDecimal$$$undscale$2 === 0) || this.isZero__p2__Z())) {
    return this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger()
  } else if ((this.java$math$BigDecimal$$$undscale$2 < 0)) {
    var jsx$2 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var jsx$1 = $m_Ljava_math_Multiplication$();
    var value = this.java$math$BigDecimal$$$undscale$2;
    var hi = (value >> 31);
    var lo = ((-value) | 0);
    var hi$1 = ((value !== 0) ? (~hi) : ((-hi) | 0));
    return jsx$2.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$1.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$1)))
  } else {
    var jsx$4 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var jsx$3 = $m_Ljava_math_Multiplication$();
    var value$1 = this.java$math$BigDecimal$$$undscale$2;
    var hi$2 = (value$1 >> 31);
    return jsx$4.divide__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$3.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$1, hi$2)))
  }
});
$c_Ljava_math_BigDecimal.prototype.init___T__Ljava_math_MathContext = (function(sVal, mc) {
  $c_Ljava_math_BigDecimal.prototype.init___AC__I__I.call(this, $m_sjsr_RuntimeString$().toCharArray__T__AC(sVal), 0, $uI(sVal.length));
  this.inplaceRound__p2__Ljava_math_MathContext__V(mc);
  return this
});
$c_Ljava_math_BigDecimal.prototype.multiply__Ljava_math_BigDecimal__Ljava_math_BigDecimal = (function(multiplicand) {
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi = (value >> 31);
  var value$1 = multiplicand.java$math$BigDecimal$$$undscale$2;
  var hi$1 = (value$1 >> 31);
  var lo = ((value + value$1) | 0);
  var hi$2 = ((((-2147483648) ^ lo) < ((-2147483648) ^ value)) ? ((1 + ((hi + hi$1) | 0)) | 0) : ((hi + hi$1) | 0));
  if ((this.isZero__p2__Z() || multiplicand.isZero__p2__Z())) {
    return $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$zeroScaledBy__J__Ljava_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2))
  } else if ((((this.java$math$BigDecimal$$$undbitLength$2 + multiplicand.java$math$BigDecimal$$$undbitLength$2) | 0) < 64)) {
    var t = this.java$math$BigDecimal$$$undsmallValue$2;
    var lo$1 = t.lo$2;
    var hi$3 = t.hi$2;
    var b = multiplicand.java$math$BigDecimal$$$undsmallValue$2;
    var blo = b.lo$2;
    var a0 = (65535 & lo$1);
    var a1 = ((lo$1 >>> 16) | 0);
    var b0 = (65535 & blo);
    var b1 = ((blo >>> 16) | 0);
    var a0b0 = $imul(a0, b0);
    var a1b0 = $imul(a1, b0);
    var a0b1 = $imul(a0, b1);
    var lo$2 = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
    var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
    var hi$4 = (((((((($imul(lo$1, b.hi$2) + $imul(hi$3, blo)) | 0) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
    if (((lo$2 === 0) && (hi$4 === (-2147483648)))) {
      var t$1 = this.java$math$BigDecimal$$$undsmallValue$2;
      var hi$5 = t$1.hi$2;
      var jsx$2 = (hi$5 < 0)
    } else {
      var jsx$2 = false
    };
    if (jsx$2) {
      var t$2 = multiplicand.java$math$BigDecimal$$$undsmallValue$2;
      var hi$6 = t$2.hi$2;
      var jsx$1 = (hi$6 < 0)
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I($m_Ljava_math_BigInteger$().getPowerOfTwo__I__Ljava_math_BigInteger(63), $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)))
    } else {
      return $m_Ljava_math_BigDecimal$().valueOf__J__I__Ljava_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(lo$2, hi$4), $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)))
    }
  } else {
    var unscaled = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger().multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(multiplicand.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger());
    return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I(unscaled, $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)))
  }
});
$c_Ljava_math_BigDecimal.prototype.scaleByPowerOfTen__I__Ljava_math_BigDecimal = (function(n) {
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi = (value >> 31);
  var hi$1 = (n >> 31);
  var lo = ((value - n) | 0);
  var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
    var t = this.java$math$BigDecimal$$$undsmallValue$2;
    var lo$1 = t.lo$2;
    var hi$3 = t.hi$2;
    if (((lo$1 === 0) && (hi$3 === 0))) {
      return $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$zeroScaledBy__J__Ljava_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2))
    } else {
      return $m_Ljava_math_BigDecimal$().valueOf__J__I__Ljava_math_BigDecimal(this.java$math$BigDecimal$$$undsmallValue$2, $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)))
    }
  } else {
    return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I(this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger(), $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)))
  }
});
$c_Ljava_math_BigDecimal.prototype.precision__I = (function() {
  if ((this.$$undprecision$2 === 0)) {
    if ((this.java$math$BigDecimal$$$undbitLength$2 === 0)) {
      var jsx$1 = 1
    } else if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
      var jsx$1 = this.decimalDigitsInLong__p2__J__I(this.java$math$BigDecimal$$$undsmallValue$2)
    } else {
      var decimalDigits = ((1 + $doubleToInt((0.3010299956639812 * (((-1) + this.java$math$BigDecimal$$$undbitLength$2) | 0)))) | 0);
      var jsx$3 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
      var jsx$2 = $m_Ljava_math_Multiplication$();
      var hi = (decimalDigits >> 31);
      var this$1 = jsx$3.divide__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$2.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(decimalDigits, hi)));
      if ((this$1.sign$2 !== 0)) {
        var jsx$1 = ((1 + decimalDigits) | 0)
      } else {
        var jsx$1 = decimalDigits
      }
    };
    this.$$undprecision$2 = jsx$1
  };
  return this.$$undprecision$2
});
$c_Ljava_math_BigDecimal.prototype.toBigIntegerExact__Ljava_math_BigInteger = (function() {
  if (((this.java$math$BigDecimal$$$undscale$2 === 0) || this.isZero__p2__Z())) {
    return this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger()
  } else if ((this.java$math$BigDecimal$$$undscale$2 < 0)) {
    var jsx$2 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var jsx$1 = $m_Ljava_math_Multiplication$();
    var value = this.java$math$BigDecimal$$$undscale$2;
    var hi = (value >> 31);
    var lo = ((-value) | 0);
    var hi$1 = ((value !== 0) ? (~hi) : ((-hi) | 0));
    return jsx$2.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$1.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$1)))
  } else {
    if (((this.java$math$BigDecimal$$$undscale$2 > this.approxPrecision__p2__I()) || (this.java$math$BigDecimal$$$undscale$2 > this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger().getLowestSetBit__I()))) {
      throw new $c_jl_ArithmeticException().init___T("Rounding necessary")
    };
    var jsx$4 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var jsx$3 = $m_Ljava_math_Multiplication$();
    var value$1 = this.java$math$BigDecimal$$$undscale$2;
    var hi$2 = (value$1 >> 31);
    var integerAndFraction = jsx$4.divideAndRemainder__Ljava_math_BigInteger__ALjava_math_BigInteger(jsx$3.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$1, hi$2)));
    var this$3 = integerAndFraction.get(1);
    if ((this$3.sign$2 !== 0)) {
      throw new $c_jl_ArithmeticException().init___T("Rounding necessary")
    };
    return integerAndFraction.get(0)
  }
});
$c_Ljava_math_BigDecimal.prototype.shortValueExact__S = (function() {
  var t = this.valueExact__p2__I__J(16);
  var lo = t.lo$2;
  return ((lo << 16) >> 16)
});
$c_Ljava_math_BigDecimal.prototype.divide__Ljava_math_BigDecimal__Ljava_math_BigDecimal = (function(divisor) {
  var thisUnscaled = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi = (value >> 31);
  var value$1 = divisor.java$math$BigDecimal$$$undscale$2;
  var hi$1 = (value$1 >> 31);
  var lo = ((value - value$1) | 0);
  var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  if (divisor.isZero__p2__Z()) {
    throw new $c_jl_ArithmeticException().init___T("Division by zero")
  } else if ((thisUnscaled.sign$2 === 0)) {
    return $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$zeroScaledBy__J__Ljava_math_BigDecimal(new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2))
  } else {
    var divisorUnscaled = divisor.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
    var lastPow = (((-1) + $m_Ljava_math_Multiplication$().BigFivePows$1.u.length) | 0);
    var gcd = thisUnscaled.gcd__Ljava_math_BigInteger__Ljava_math_BigInteger(divisorUnscaled);
    var p = thisUnscaled.divide__Ljava_math_BigInteger__Ljava_math_BigInteger(gcd);
    var q1 = divisorUnscaled.divide__Ljava_math_BigInteger__Ljava_math_BigInteger(gcd);
    var k = q1.getLowestSetBit__I();
    var i = 1;
    var q = q1.shiftRight__I__Ljava_math_BigInteger(k);
    var l = 0;
    var x1_$_$$und1$f;
    var x1_$_$$und2$f;
    _loop: while (true) {
      var qr = q.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem($m_Ljava_math_Multiplication$().BigFivePows$1.get(i));
      var this$2 = qr.rem$1;
      if ((this$2.sign$2 === 0)) {
        var temp$i = ((i < lastPow) ? ((1 + i) | 0) : i);
        var temp$q = qr.quot$1;
        var temp$l = ((l + i) | 0);
        i = temp$i;
        q = temp$q;
        l = temp$l;
        continue _loop
      };
      if ((i !== 1)) {
        i = 1;
        continue _loop
      };
      var _1 = q;
      var _2 = l;
      var x1_$_$$und1$f = _1;
      var x1_$_$$und2$f = _2;
      break
    };
    var q$1 = $as_Ljava_math_BigInteger(x1_$_$$und1$f);
    var l$1 = $uI(x1_$_$$und2$f);
    if ((!$m_sr_BoxesRunTime$().equalsNumNum__jl_Number__jl_Number__Z(q$1.abs__Ljava_math_BigInteger(), $m_Ljava_math_BigInteger$().ONE$1))) {
      throw new $c_jl_ArithmeticException().init___T("Non-terminating decimal expansion; no exact representable decimal result")
    };
    var p2 = ((q$1.sign$2 < 0) ? p.negate__Ljava_math_BigInteger() : p);
    var jsx$1 = $m_Ljava_math_BigDecimal$();
    var value$2 = ((k > l$1) ? k : l$1);
    var hi$3 = (value$2 >> 31);
    var lo$1 = ((lo + value$2) | 0);
    var hi$4 = ((((-2147483648) ^ lo$1) < ((-2147483648) ^ lo)) ? ((1 + ((hi$2 + hi$3) | 0)) | 0) : ((hi$2 + hi$3) | 0));
    var newScale = jsx$1.java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$4));
    var i$1 = ((k - l$1) | 0);
    var p3 = ((i$1 > 0) ? $m_Ljava_math_Multiplication$().multiplyByFivePow__Ljava_math_BigInteger__I__Ljava_math_BigInteger(p2, i$1) : p2.shiftLeft__I__Ljava_math_BigInteger(((-i$1) | 0)));
    return new $c_Ljava_math_BigDecimal().init___Ljava_math_BigInteger__I(p3, newScale)
  }
});
$c_Ljava_math_BigDecimal.prototype.setUnscaledValue__p2__Ljava_math_BigInteger__V = (function(unscaledVal) {
  this.$$undintVal$2 = unscaledVal;
  this.java$math$BigDecimal$$$undbitLength$2 = $m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(unscaledVal);
  if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
    this.java$math$BigDecimal$$$undsmallValue$2 = unscaledVal.longValue__J()
  }
});
$c_Ljava_math_BigDecimal.prototype.byteValueExact__B = (function() {
  var t = this.valueExact__p2__I__J(8);
  var lo = t.lo$2;
  return ((lo << 24) >> 24)
});
$c_Ljava_math_BigDecimal.prototype.smallRound__p2__Ljava_math_MathContext__I__V = (function(mc, discardedPrecision) {
  var t = $m_Ljava_math_BigDecimal$().LongTenPows$1.get(discardedPrecision);
  var lo = t.lo$2;
  var hi = t.hi$2;
  var value = this.java$math$BigDecimal$$$undscale$2;
  var hi$1 = (value >> 31);
  var hi$2 = (discardedPrecision >> 31);
  var lo$1 = ((value - discardedPrecision) | 0);
  var hi$3 = ((((-2147483648) ^ lo$1) > ((-2147483648) ^ value)) ? (((-1) + ((hi$1 - hi$2) | 0)) | 0) : ((hi$1 - hi$2) | 0));
  var t$1 = this.java$math$BigDecimal$$$undsmallValue$2;
  var lo$2 = t$1.lo$2;
  var hi$4 = t$1.hi$2;
  var this$2 = $m_sjsr_RuntimeLong$();
  var lo$3 = this$2.divideImpl__I__I__I__I__I(lo$2, hi$4, lo, hi);
  var hi$5 = this$2.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  var this$3 = $m_sjsr_RuntimeLong$();
  var lo$4 = this$3.remainderImpl__I__I__I__I__I(lo$2, hi$4, lo, hi);
  var hi$6 = this$3.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  if ((!((lo$4 === 0) && (hi$6 === 0)))) {
    var jsx$1 = $m_Ljava_math_BigDecimal$();
    if ((hi$6 < 0)) {
      var lo$5 = ((-lo$4) | 0);
      var hi$7 = ((lo$4 !== 0) ? (~hi$6) : ((-hi$6) | 0));
      var this$7_$_lo$2 = lo$5;
      var this$7_$_hi$2 = hi$7
    } else {
      var this$7_$_lo$2 = lo$4;
      var this$7_$_hi$2 = hi$6
    };
    var lo$6 = (this$7_$_lo$2 << 1);
    var hi$8 = (((this$7_$_lo$2 >>> 31) | 0) | (this$7_$_hi$2 << 1));
    var compRem = jsx$1.java$math$BigDecimal$$longCompareTo__J__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$6, hi$8), new $c_sjsr_RuntimeLong().init___I__I(lo, hi));
    var frac = $imul(((hi$6 < 0) ? (-1) : (((hi$6 === 0) && (lo$4 === 0)) ? 0 : 1)), ((5 + compRem) | 0));
    var value$1 = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$roundingBehavior__I__I__Ljava_math_RoundingMode__I((1 & lo$3), frac, mc.roundingMode$1);
    var hi$9 = (value$1 >> 31);
    var lo$7 = ((lo$3 + value$1) | 0);
    var hi$10 = ((((-2147483648) ^ lo$7) < ((-2147483648) ^ lo$3)) ? ((1 + ((hi$5 + hi$9) | 0)) | 0) : ((hi$5 + hi$9) | 0));
    if ((hi$10 < 0)) {
      var lo$8 = ((-lo$7) | 0);
      var hi$11 = ((lo$7 !== 0) ? (~hi$10) : ((-hi$10) | 0));
      var this$13_$_lo$2 = lo$8;
      var this$13_$_hi$2 = hi$11
    } else {
      var this$13_$_lo$2 = lo$7;
      var this$13_$_hi$2 = hi$10
    };
    var a = $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(this$13_$_lo$2, this$13_$_hi$2);
    var v = $g.Math.log10;
    if ((((!(v === (void 0))) ? $uD($g.Math.log10(a)) : ($uD($g.Math.log(a)) / 2.302585092994046)) >= mc.precision$1)) {
      var lo$9 = (((-1) + lo$1) | 0);
      var hi$12 = ((lo$9 !== (-1)) ? hi$3 : (((-1) + hi$3) | 0));
      var this$20 = $m_sjsr_RuntimeLong$();
      var lo$10 = this$20.divideImpl__I__I__I__I__I(lo$7, hi$10, 10, 0);
      var hi$13 = this$20.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      var x1_$_$$und1$f = null;
      var x1_$_$$und2$f = null;
      var x1_$_$$und1$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$9, hi$12);
      var x1_$_$$und2$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$10, hi$13)
    } else {
      var x1_$_$$und1$f = null;
      var x1_$_$$und2$f = null;
      var x1_$_$$und1$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$3);
      var x1_$_$$und2$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$7, hi$10)
    }
  } else {
    var x1_$_$$und1$f = null;
    var x1_$_$$und2$f = null;
    var x1_$_$$und1$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$1, hi$3);
    var x1_$_$$und2$mcJ$sp$f = new $c_sjsr_RuntimeLong().init___I__I(lo$3, hi$5)
  };
  var t$2 = x1_$_$$und1$mcJ$sp$f;
  var lo$11 = t$2.lo$2;
  var hi$14 = t$2.hi$2;
  var t$3 = x1_$_$$und2$mcJ$sp$f;
  var lo$12 = t$3.lo$2;
  var hi$15 = t$3.hi$2;
  this.java$math$BigDecimal$$$undscale$2 = $m_Ljava_math_BigDecimal$().java$math$BigDecimal$$safeLongToInt__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$11, hi$14));
  this.$$undprecision$2 = mc.precision$1;
  this.java$math$BigDecimal$$$undsmallValue$2 = new $c_sjsr_RuntimeLong().init___I__I(lo$12, hi$15);
  this.java$math$BigDecimal$$$undbitLength$2 = $m_Ljava_math_BigDecimal$().bitLength__J__I(new $c_sjsr_RuntimeLong().init___I__I(lo$12, hi$15));
  this.$$undintVal$2 = null
});
$c_Ljava_math_BigDecimal.prototype.approxPrecision__p2__I = (function() {
  return ((this.$$undprecision$2 > 0) ? this.$$undprecision$2 : ((1 + $doubleToInt((0.3010299956639812 * (((-1) + this.java$math$BigDecimal$$$undbitLength$2) | 0)))) | 0))
});
$c_Ljava_math_BigDecimal.prototype.doubleValue__D = (function() {
  var sign = this.signum__I();
  var value = this.java$math$BigDecimal$$$undbitLength$2;
  var hi = (value >> 31);
  var this$1 = $m_sjsr_RuntimeLong$();
  var value$1 = (this.java$math$BigDecimal$$$undscale$2 / 0.3010299956639812);
  var lo = this$1.scala$scalajs$runtime$RuntimeLong$$fromDoubleImpl__D__I(value$1);
  var hi$1 = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  var lo$1 = ((value - lo) | 0);
  var hi$2 = ((((-2147483648) ^ lo$1) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  if ((((hi$2 === (-1)) ? (((-2147483648) ^ lo$1) < 2147482574) : (hi$2 < (-1))) || (sign === 0))) {
    return (0.0 * sign)
  } else if (((hi$2 === 0) ? (((-2147483648) ^ lo$1) > (-2147482623)) : (hi$2 > 0))) {
    return (Infinity * sign)
  } else {
    var mantissa0 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger().abs__Ljava_math_BigInteger();
    var exponent = 1076;
    if ((this.java$math$BigDecimal$$$undscale$2 <= 0)) {
      var jsx$1 = $m_Ljava_math_Multiplication$();
      var value$2 = ((-this.java$math$BigDecimal$$$undscale$2) | 0);
      var hi$3 = (value$2 >> 31);
      var mantissa = mantissa0.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$1.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$2, hi$3)))
    } else {
      var jsx$2 = $m_Ljava_math_Multiplication$();
      var value$3 = this.java$math$BigDecimal$$$undscale$2;
      var hi$4 = (value$3 >> 31);
      var powerOfTen = jsx$2.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$3, hi$4));
      var k = ((100 - lo$1) | 0);
      if ((k > 0)) {
        exponent = ((exponent - k) | 0);
        var m = mantissa0.shiftLeft__I__Ljava_math_BigInteger(k)
      } else {
        var m = mantissa0
      };
      var qr = m.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem(powerOfTen);
      var compRem = qr.rem$1.shiftLeftOneBit__Ljava_math_BigInteger().compareTo__Ljava_math_BigInteger__I(powerOfTen);
      exponent = (((-2) + exponent) | 0);
      var this$3 = qr.quot$1.shiftLeft__I__Ljava_math_BigInteger(2);
      var jsx$3 = $m_Ljava_math_BigInteger$();
      var value$4 = ((1 + (($imul(compRem, ((3 + compRem) | 0)) / 2) | 0)) | 0);
      var hi$5 = (value$4 >> 31);
      var bi = jsx$3.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value$4, hi$5));
      var mantissa = $m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$3, bi)
    };
    var lowestSetBit = mantissa.getLowestSetBit__I();
    var discardedSize = (((-54) + $m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(mantissa)) | 0);
    var bits_$_lo$2 = 0;
    var bits_$_hi$2 = 0;
    var tempBits_$_lo$2 = 0;
    var tempBits_$_hi$2 = 0;
    if ((discardedSize > 0)) {
      var t = mantissa.shiftRight__I__Ljava_math_BigInteger(discardedSize).longValue__J();
      var lo$2 = t.lo$2;
      var hi$6 = t.hi$2;
      var jsx$4_$_lo$2 = lo$2;
      var jsx$4_$_hi$2 = hi$6;
      bits_$_lo$2 = jsx$4_$_lo$2;
      bits_$_hi$2 = jsx$4_$_hi$2;
      tempBits_$_lo$2 = bits_$_lo$2;
      tempBits_$_hi$2 = bits_$_hi$2;
      var b_$_lo$2 = bits_$_lo$2;
      var b_$_hi$2 = bits_$_hi$2;
      var lo$3 = (1 & b_$_lo$2);
      if (((lo$3 === 1) && (lowestSetBit < discardedSize))) {
        var jsx$5 = true
      } else {
        var b$1_$_lo$2 = bits_$_lo$2;
        var b$1_$_hi$2 = bits_$_hi$2;
        var lo$4 = (3 & b$1_$_lo$2);
        var jsx$5 = (lo$4 === 3)
      };
      if (jsx$5) {
        var b$2_$_lo$2 = bits_$_lo$2;
        var b$2_$_hi$2 = bits_$_hi$2;
        var bhi = b$2_$_hi$2;
        var lo$5 = ((2 + b$2_$_lo$2) | 0);
        var hi$7 = ((((-2147483648) ^ lo$5) < (-2147483646)) ? ((1 + bhi) | 0) : bhi);
        var jsx$6_$_lo$2 = lo$5;
        var jsx$6_$_hi$2 = hi$7;
        bits_$_lo$2 = jsx$6_$_lo$2;
        bits_$_hi$2 = jsx$6_$_hi$2
      }
    } else {
      var t$1 = mantissa.longValue__J();
      var lo$6 = t$1.lo$2;
      var hi$8 = t$1.hi$2;
      var n = ((-discardedSize) | 0);
      var lo$7 = (((32 & n) === 0) ? (lo$6 << n) : 0);
      var hi$9 = (((32 & n) === 0) ? (((((lo$6 >>> 1) | 0) >>> ((31 - n) | 0)) | 0) | (hi$8 << n)) : (lo$6 << n));
      var jsx$7_$_lo$2 = lo$7;
      var jsx$7_$_hi$2 = hi$9;
      bits_$_lo$2 = jsx$7_$_lo$2;
      bits_$_hi$2 = jsx$7_$_hi$2;
      tempBits_$_lo$2 = bits_$_lo$2;
      tempBits_$_hi$2 = bits_$_hi$2;
      var b$3_$_lo$2 = bits_$_lo$2;
      var b$3_$_hi$2 = bits_$_hi$2;
      var lo$8 = (3 & b$3_$_lo$2);
      if ((lo$8 === 3)) {
        var b$4_$_lo$2 = bits_$_lo$2;
        var b$4_$_hi$2 = bits_$_hi$2;
        var bhi$1 = b$4_$_hi$2;
        var lo$9 = ((2 + b$4_$_lo$2) | 0);
        var hi$10 = ((((-2147483648) ^ lo$9) < (-2147483646)) ? ((1 + bhi$1) | 0) : bhi$1);
        var jsx$8_$_lo$2 = lo$9;
        var jsx$8_$_hi$2 = hi$10;
        bits_$_lo$2 = jsx$8_$_lo$2;
        bits_$_hi$2 = jsx$8_$_hi$2
      }
    };
    var b$5_$_lo$2 = bits_$_lo$2;
    var b$5_$_hi$2 = bits_$_hi$2;
    var hi$11 = (4194304 & b$5_$_hi$2);
    if ((hi$11 === 0)) {
      var this$6_$_lo$2 = bits_$_lo$2;
      var this$6_$_hi$2 = bits_$_hi$2;
      var lo$10 = (((this$6_$_lo$2 >>> 1) | 0) | (this$6_$_hi$2 << 31));
      var hi$12 = (this$6_$_hi$2 >> 1);
      var jsx$9_$_lo$2 = lo$10;
      var jsx$9_$_hi$2 = hi$12;
      bits_$_lo$2 = jsx$9_$_lo$2;
      bits_$_hi$2 = jsx$9_$_hi$2;
      exponent = ((exponent + discardedSize) | 0)
    } else {
      var this$7_$_lo$2 = bits_$_lo$2;
      var this$7_$_hi$2 = bits_$_hi$2;
      var lo$11 = (((this$7_$_lo$2 >>> 2) | 0) | (this$7_$_hi$2 << 30));
      var hi$13 = (this$7_$_hi$2 >> 2);
      var jsx$10_$_lo$2 = lo$11;
      var jsx$10_$_hi$2 = hi$13;
      bits_$_lo$2 = jsx$10_$_lo$2;
      bits_$_hi$2 = jsx$10_$_hi$2;
      exponent = ((exponent + ((1 + discardedSize) | 0)) | 0)
    };
    if ((exponent > 2046)) {
      return (Infinity * sign)
    } else if ((exponent < (-53))) {
      return (0.0 * sign)
    } else {
      if ((exponent <= 0)) {
        var this$8_$_lo$2 = tempBits_$_lo$2;
        var this$8_$_hi$2 = tempBits_$_hi$2;
        var lo$12 = (((this$8_$_lo$2 >>> 1) | 0) | (this$8_$_hi$2 << 31));
        var hi$14 = (this$8_$_hi$2 >> 1);
        var jsx$11_$_lo$2 = lo$12;
        var jsx$11_$_hi$2 = hi$14;
        bits_$_lo$2 = jsx$11_$_lo$2;
        bits_$_hi$2 = jsx$11_$_hi$2;
        var this$9_$_lo$2 = bits_$_lo$2;
        var this$9_$_hi$2 = bits_$_hi$2;
        var n$1 = ((63 + exponent) | 0);
        var lo$13 = (((32 & n$1) === 0) ? ((((-1) >>> n$1) | 0) | ((-2) << ((31 - n$1) | 0))) : (((-1) >>> n$1) | 0));
        var hi$15 = (((32 & n$1) === 0) ? (((-1) >>> n$1) | 0) : 0);
        var lo$14 = (this$9_$_lo$2 & lo$13);
        var hi$16 = (this$9_$_hi$2 & hi$15);
        var jsx$12_$_lo$2 = lo$14;
        var jsx$12_$_hi$2 = hi$16;
        tempBits_$_lo$2 = jsx$12_$_lo$2;
        tempBits_$_hi$2 = jsx$12_$_hi$2;
        var this$10_$_lo$2 = bits_$_lo$2;
        var this$10_$_hi$2 = bits_$_hi$2;
        var n$2 = ((-exponent) | 0);
        var lo$15 = (((32 & n$2) === 0) ? (((this$10_$_lo$2 >>> n$2) | 0) | ((this$10_$_hi$2 << 1) << ((31 - n$2) | 0))) : (this$10_$_hi$2 >> n$2));
        var hi$17 = (((32 & n$2) === 0) ? (this$10_$_hi$2 >> n$2) : (this$10_$_hi$2 >> 31));
        var jsx$13_$_lo$2 = lo$15;
        var jsx$13_$_hi$2 = hi$17;
        bits_$_lo$2 = jsx$13_$_lo$2;
        bits_$_hi$2 = jsx$13_$_hi$2;
        var b$6_$_lo$2 = bits_$_lo$2;
        var b$6_$_hi$2 = bits_$_hi$2;
        var lo$16 = (3 & b$6_$_lo$2);
        if ((lo$16 === 3)) {
          var jsx$14 = true
        } else {
          var b$7_$_lo$2 = bits_$_lo$2;
          var b$7_$_hi$2 = bits_$_hi$2;
          var lo$17 = (1 & b$7_$_lo$2);
          if ((lo$17 === 1)) {
            var this$11_$_lo$2 = tempBits_$_lo$2;
            var this$11_$_hi$2 = tempBits_$_hi$2;
            var jsx$15 = (!((this$11_$_lo$2 === 0) && (this$11_$_hi$2 === 0)))
          } else {
            var jsx$15 = false
          };
          if (jsx$15) {
            var jsx$14 = (lowestSetBit < discardedSize)
          } else {
            var jsx$14 = false
          }
        };
        if (jsx$14) {
          var b$8_$_lo$2 = bits_$_lo$2;
          var b$8_$_hi$2 = bits_$_hi$2;
          var bhi$2 = b$8_$_hi$2;
          var lo$18 = ((1 + b$8_$_lo$2) | 0);
          var hi$18 = ((lo$18 === 0) ? ((1 + bhi$2) | 0) : bhi$2);
          var jsx$16_$_lo$2 = lo$18;
          var jsx$16_$_hi$2 = hi$18;
          bits_$_lo$2 = jsx$16_$_lo$2;
          bits_$_hi$2 = jsx$16_$_hi$2
        };
        exponent = 0;
        var this$13_$_lo$2 = bits_$_lo$2;
        var this$13_$_hi$2 = bits_$_hi$2;
        var lo$19 = (((this$13_$_lo$2 >>> 1) | 0) | (this$13_$_hi$2 << 31));
        var hi$19 = (this$13_$_hi$2 >> 1);
        var jsx$17_$_lo$2 = lo$19;
        var jsx$17_$_hi$2 = hi$19;
        bits_$_lo$2 = jsx$17_$_lo$2;
        bits_$_hi$2 = jsx$17_$_hi$2
      };
      var hi$20 = (sign >> 31);
      var hi$21 = ((-2147483648) & hi$20);
      var value$5 = exponent;
      var hi$23 = (value$5 << 20);
      var hi$24 = (hi$21 | hi$23);
      var b$9_$_lo$2 = bits_$_lo$2;
      var b$9_$_hi$2 = bits_$_hi$2;
      var lo$20 = b$9_$_lo$2;
      var hi$25 = (1048575 & b$9_$_hi$2);
      var hi$26 = (hi$24 | hi$25);
      return $m_sjsr_Bits$().longBitsToDouble__J__D(new $c_sjsr_RuntimeLong().init___I__I(lo$20, hi$26))
    }
  }
});
$c_Ljava_math_BigDecimal.prototype.init___J__I = (function(smallValue, scale) {
  $c_Ljava_math_BigDecimal.prototype.init___.call(this);
  this.java$math$BigDecimal$$$undsmallValue$2 = smallValue;
  this.java$math$BigDecimal$$$undscale$2 = scale;
  this.java$math$BigDecimal$$$undbitLength$2 = $m_Ljava_math_BigDecimal$().bitLength__J__I(smallValue);
  return this
});
$c_Ljava_math_BigDecimal.prototype.hashCode__I = (function() {
  if ((this.$$undhashCode$2 !== 0)) {
    return this.$$undhashCode$2
  } else if ((this.java$math$BigDecimal$$$undbitLength$2 < 64)) {
    var t = this.java$math$BigDecimal$$$undsmallValue$2;
    var lo = t.lo$2;
    this.$$undhashCode$2 = lo;
    var jsx$1 = this.$$undhashCode$2;
    var t$1 = this.java$math$BigDecimal$$$undsmallValue$2;
    var hi$1 = t$1.hi$2;
    this.$$undhashCode$2 = (($imul(33, jsx$1) + hi$1) | 0);
    this.$$undhashCode$2 = (($imul(17, this.$$undhashCode$2) + this.java$math$BigDecimal$$$undscale$2) | 0);
    return this.$$undhashCode$2
  } else {
    this.$$undhashCode$2 = (($imul(17, this.$$undintVal$2.hashCode__I()) + this.java$math$BigDecimal$$$undscale$2) | 0);
    return this.$$undhashCode$2
  }
});
$c_Ljava_math_BigDecimal.prototype.init___T = (function(sVal) {
  $c_Ljava_math_BigDecimal.prototype.init___AC__I__I.call(this, $m_sjsr_RuntimeString$().toCharArray__T__AC(sVal), 0, $uI(sVal.length));
  return this
});
$c_Ljava_math_BigDecimal.prototype.intValue__I = (function() {
  return (((this.java$math$BigDecimal$$$undscale$2 <= (-32)) || (this.java$math$BigDecimal$$$undscale$2 > this.approxPrecision__p2__I())) ? 0 : this.toBigInteger__Ljava_math_BigInteger().intValue__I())
});
$c_Ljava_math_BigDecimal.prototype.init___Ljava_math_BigInteger = (function(bi) {
  $c_Ljava_math_BigDecimal.prototype.init___Ljava_math_BigInteger__I.call(this, bi, 0);
  return this
});
$c_Ljava_math_BigDecimal.prototype.init___Ljava_math_BigInteger__I__Ljava_math_MathContext = (function(unscaledVal, scale, mc) {
  $c_Ljava_math_BigDecimal.prototype.init___Ljava_math_BigInteger__I.call(this, unscaledVal, scale);
  this.inplaceRound__p2__Ljava_math_MathContext__V(mc);
  return this
});
$c_Ljava_math_BigDecimal.prototype.init___AC__I__I = (function($in, offset, len) {
  $c_Ljava_math_BigDecimal.prototype.init___.call(this);
  var last = (((-1) + ((offset + len) | 0)) | 0);
  if (($in === null)) {
    throw new $c_jl_NullPointerException().init___T("in == null")
  };
  if (((((last >= $in.u.length) || (offset < 0)) || (len <= 0)) || (last < 0))) {
    throw new $c_jl_NumberFormatException().init___T(new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["Bad offset/length: offset=", " len=", " in.length=", ""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([offset, len, $in.u.length])))
  };
  var index = offset;
  if (((offset <= last) && ($in.get(offset) === 43))) {
    index = ((1 + index) | 0);
    if ((index < last)) {
      $m_Ljava_math_BigDecimal$();
      var c = $in.get(index);
      var array = [new $c_jl_Character().init___C(43), new $c_jl_Character().init___C(45)];
      var elem = new $c_jl_Character().init___C(c);
      var i = 0;
      while (true) {
        if ((i < $uI(array.length))) {
          var index$1 = i;
          var arg1 = array[index$1];
          var jsx$2 = ($m_sr_BoxesRunTime$().equals__O__O__Z(arg1, elem) === false)
        } else {
          var jsx$2 = false
        };
        if (jsx$2) {
          i = ((1 + i) | 0)
        } else {
          break
        }
      };
      var jsx$1 = (i !== $uI(array.length))
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      throw new $c_jl_NumberFormatException().init___T(("For input string: " + $in.toString__T()))
    }
  } else {
    var isMinus = ((index <= last) && ($in.get(index) === 45));
    if ((((1 + index) | 0) < last)) {
      $m_Ljava_math_BigDecimal$();
      var c$1 = $in.get(((1 + index) | 0));
      var array$1 = [new $c_jl_Character().init___C(43), new $c_jl_Character().init___C(45)];
      var elem$1 = new $c_jl_Character().init___C(c$1);
      var i$1 = 0;
      while (true) {
        if ((i$1 < $uI(array$1.length))) {
          var index$2 = i$1;
          var arg1$1 = array$1[index$2];
          var jsx$3 = ($m_sr_BoxesRunTime$().equals__O__O__Z(arg1$1, elem$1) === false)
        } else {
          var jsx$3 = false
        };
        if (jsx$3) {
          i$1 = ((1 + i$1) | 0)
        } else {
          break
        }
      };
      var nextIsSign = (i$1 !== $uI(array$1.length))
    } else {
      var nextIsSign = false
    };
    if ((isMinus && nextIsSign)) {
      throw new $c_jl_NumberFormatException().init___T(("For input string: " + $in.toString__T()))
    }
  };
  var begin = index;
  var counter = 0;
  var wasNonZero = false;
  while (true) {
    if ((index <= last)) {
      $m_Ljava_math_BigDecimal$();
      var c$2 = $in.get(index);
      var array$2 = [new $c_jl_Character().init___C(46), new $c_jl_Character().init___C(101), new $c_jl_Character().init___C(69)];
      var elem$2 = new $c_jl_Character().init___C(c$2);
      var i$2 = 0;
      while (true) {
        if ((i$2 < $uI(array$2.length))) {
          var index$3 = i$2;
          var arg1$2 = array$2[index$3];
          var jsx$5 = ($m_sr_BoxesRunTime$().equals__O__O__Z(arg1$2, elem$2) === false)
        } else {
          var jsx$5 = false
        };
        if (jsx$5) {
          i$2 = ((1 + i$2) | 0)
        } else {
          break
        }
      };
      var jsx$4 = (!(i$2 !== $uI(array$2.length)))
    } else {
      var jsx$4 = false
    };
    if (jsx$4) {
      if ((!wasNonZero)) {
        if (($in.get(index) === 48)) {
          counter = ((1 + counter) | 0)
        } else {
          wasNonZero = true
        }
      };
      index = ((1 + index) | 0)
    } else {
      break
    }
  };
  var this$25 = new $c_s_Predef$ArrayCharSequence().init___AC($in);
  var end = index;
  var u = new $c_sr_ArrayCharSequence().init___AC__I__I(this$25.$$und$undarrayOfChars$1, begin, end).toString__T();
  var b = ((index - begin) | 0);
  if (((index <= last) && ($in.get(index) === 46))) {
    index = ((1 + index) | 0);
    var begin$2 = index;
    while (true) {
      if ((index <= last)) {
        $m_Ljava_math_BigDecimal$();
        var c$3 = $in.get(index);
        var array$3 = [new $c_jl_Character().init___C(101), new $c_jl_Character().init___C(69)];
        var elem$3 = new $c_jl_Character().init___C(c$3);
        var i$3 = 0;
        while (true) {
          if ((i$3 < $uI(array$3.length))) {
            var index$4 = i$3;
            var arg1$3 = array$3[index$4];
            var jsx$7 = ($m_sr_BoxesRunTime$().equals__O__O__Z(arg1$3, elem$3) === false)
          } else {
            var jsx$7 = false
          };
          if (jsx$7) {
            i$3 = ((1 + i$3) | 0)
          } else {
            break
          }
        };
        var jsx$6 = (!(i$3 !== $uI(array$3.length)))
      } else {
        var jsx$6 = false
      };
      if (jsx$6) {
        if ((!wasNonZero)) {
          if (($in.get(index) === 48)) {
            counter = ((1 + counter) | 0)
          } else {
            wasNonZero = true
          }
        };
        index = ((1 + index) | 0)
      } else {
        break
      }
    };
    this.java$math$BigDecimal$$$undscale$2 = ((index - begin$2) | 0);
    var this$34 = new $c_s_Predef$ArrayCharSequence().init___AC($in);
    var end$1 = ((begin$2 + this.java$math$BigDecimal$$$undscale$2) | 0);
    var _1 = (("" + u) + new $c_sr_ArrayCharSequence().init___AC__I__I(this$34.$$und$undarrayOfChars$1, begin$2, end$1).toString__T());
    var _2 = ((b + this.java$math$BigDecimal$$$undscale$2) | 0);
    var x1_$_$$und1$f = _1;
    var x1_$_$$und2$f = _2
  } else {
    this.java$math$BigDecimal$$$undscale$2 = 0;
    var x1_$_$$und1$f = u;
    var x1_$_$$und2$f = b
  };
  var unscaled = $as_T(x1_$_$$und1$f);
  var bufLength = $uI(x1_$_$$und2$f);
  if ((index <= last)) {
    $m_Ljava_math_BigDecimal$();
    var c$4 = $in.get(index);
    var array$4 = [new $c_jl_Character().init___C(101), new $c_jl_Character().init___C(69)];
    var elem$4 = new $c_jl_Character().init___C(c$4);
    var i$4 = 0;
    while (true) {
      if ((i$4 < $uI(array$4.length))) {
        var index$5 = i$4;
        var arg1$4 = array$4[index$5];
        var jsx$9 = ($m_sr_BoxesRunTime$().equals__O__O__Z(arg1$4, elem$4) === false)
      } else {
        var jsx$9 = false
      };
      if (jsx$9) {
        i$4 = ((1 + i$4) | 0)
      } else {
        break
      }
    };
    var jsx$8 = (i$4 !== $uI(array$4.length))
  } else {
    var jsx$8 = false
  };
  if (jsx$8) {
    index = ((1 + index) | 0);
    var indexIsPlus = ((index <= last) && ($in.get(index) === 43));
    var nextIsNotMinus = ((((1 + index) | 0) <= last) && ($in.get(((1 + index) | 0)) !== 45));
    var begin$3 = ((indexIsPlus && nextIsNotMinus) ? ((1 + index) | 0) : index);
    var this$42 = $m_sjsr_RuntimeString$();
    var count = ((((1 + last) | 0) - begin$3) | 0);
    var scaleString = this$42.newString__AC__I__I__T($in, begin$3, count);
    var value = this.java$math$BigDecimal$$$undscale$2;
    var hi = (value >> 31);
    var this$43 = $m_jl_Integer$();
    var value$1 = this$43.parseInt__T__I__I(scaleString, 10);
    var hi$1 = (value$1 >> 31);
    var lo = ((value - value$1) | 0);
    var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
    this.java$math$BigDecimal$$$undscale$2 = lo;
    var value$2 = this.java$math$BigDecimal$$$undscale$2;
    var hi$3 = (value$2 >> 31);
    if ((!((lo === value$2) && (hi$2 === hi$3)))) {
      throw new $c_jl_NumberFormatException().init___T("Scale out of range")
    }
  };
  if ((bufLength < 19)) {
    var this$45 = $m_jl_Long$();
    this.java$math$BigDecimal$$$undsmallValue$2 = this$45.parseLong__T__I__J(unscaled, 10);
    this.java$math$BigDecimal$$$undbitLength$2 = $m_Ljava_math_BigDecimal$().bitLength__J__I(this.java$math$BigDecimal$$$undsmallValue$2)
  } else {
    this.setUnscaledValue__p2__Ljava_math_BigInteger__V(new $c_Ljava_math_BigInteger().init___T(unscaled))
  };
  return this
});
$c_Ljava_math_BigDecimal.prototype.floatValue__F = (function() {
  var value = this.java$math$BigDecimal$$$undbitLength$2;
  var hi = (value >> 31);
  var this$1 = $m_sjsr_RuntimeLong$();
  var value$1 = (this.java$math$BigDecimal$$$undscale$2 / 0.3010299956639812);
  var lo = this$1.scala$scalajs$runtime$RuntimeLong$$fromDoubleImpl__D__I(value$1);
  var hi$1 = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  var lo$1 = ((value - lo) | 0);
  var hi$2 = ((((-2147483648) ^ lo$1) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  var floatResult0 = $fround(this.signum__I());
  var floatResult = ((((hi$2 === (-1)) ? (((-2147483648) ^ lo$1) < 2147483499) : (hi$2 < (-1))) || (floatResult0 === 0.0)) ? $fround((0.0 * floatResult0)) : (((hi$2 === 0) ? (((-2147483648) ^ lo$1) > (-2147483519)) : (hi$2 > 0)) ? $fround((Infinity * floatResult0)) : $fround(this.doubleValue__D())));
  return floatResult
});
$c_Ljava_math_BigDecimal.prototype.decimalDigitsInLong__p2__J__I = (function(value) {
  if (((value.lo$2 === 0) && (value.hi$2 === (-2147483648)))) {
    return 19
  } else {
    var jsx$3 = $m_ju_Arrays$();
    var jsx$2 = $m_Ljava_math_BigDecimal$().LongTenPows$1;
    var ahi = value.hi$2;
    if ((ahi < 0)) {
      var lo = value.lo$2;
      var hi = value.hi$2;
      var lo$1 = ((-lo) | 0);
      var hi$1 = ((lo !== 0) ? (~hi) : ((-hi) | 0));
      var x_$_lo$2 = lo$1;
      var x_$_hi$2 = hi$1;
      var jsx$1 = new $c_sjsr_RuntimeLong().init___I__I(x_$_lo$2, x_$_hi$2)
    } else {
      var jsx$1 = value
    };
    var index = jsx$3.binarySearch__AJ__J__I(jsx$2, jsx$1);
    return ((index < 0) ? (((-1) - index) | 0) : ((1 + index) | 0))
  }
});
$c_Ljava_math_BigDecimal.prototype.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger = (function() {
  if ((this.$$undintVal$2 === null)) {
    this.$$undintVal$2 = $m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(this.java$math$BigDecimal$$$undsmallValue$2)
  };
  return this.$$undintVal$2
});
$c_Ljava_math_BigDecimal.prototype.compareTo__Ljava_math_BigDecimal__I = (function(bi) {
  var thisSign = this.signum__I();
  var valueSign = bi.signum__I();
  if ((thisSign === valueSign)) {
    if ((((this.java$math$BigDecimal$$$undscale$2 === bi.java$math$BigDecimal$$$undscale$2) && (this.java$math$BigDecimal$$$undbitLength$2 < 64)) && (bi.java$math$BigDecimal$$$undbitLength$2 < 64))) {
      var t = this.java$math$BigDecimal$$$undsmallValue$2;
      var lo = t.lo$2;
      var hi = t.hi$2;
      var b = bi.java$math$BigDecimal$$$undsmallValue$2;
      var bhi = b.hi$2;
      if (((hi === bhi) ? (((-2147483648) ^ lo) < ((-2147483648) ^ b.lo$2)) : (hi < bhi))) {
        return (-1)
      } else {
        var t$1 = this.java$math$BigDecimal$$$undsmallValue$2;
        var lo$1 = t$1.lo$2;
        var hi$1 = t$1.hi$2;
        var b$1 = bi.java$math$BigDecimal$$$undsmallValue$2;
        var bhi$1 = b$1.hi$2;
        if (((hi$1 === bhi$1) ? (((-2147483648) ^ lo$1) > ((-2147483648) ^ b$1.lo$2)) : (hi$1 > bhi$1))) {
          return 1
        } else {
          return 0
        }
      }
    } else {
      var value = this.java$math$BigDecimal$$$undscale$2;
      var hi$2 = (value >> 31);
      var value$1 = bi.java$math$BigDecimal$$$undscale$2;
      var hi$3 = (value$1 >> 31);
      var lo$2 = ((value - value$1) | 0);
      var hi$4 = ((((-2147483648) ^ lo$2) > ((-2147483648) ^ value)) ? (((-1) + ((hi$2 - hi$3) | 0)) | 0) : ((hi$2 - hi$3) | 0));
      var diffPrecision = ((this.approxPrecision__p2__I() - bi.approxPrecision__p2__I()) | 0);
      var hi$5 = (diffPrecision >> 31);
      var lo$3 = ((1 + lo$2) | 0);
      var hi$6 = ((lo$3 === 0) ? ((1 + hi$4) | 0) : hi$4);
      if (((hi$5 === hi$6) ? (((-2147483648) ^ diffPrecision) > ((-2147483648) ^ lo$3)) : (hi$5 > hi$6))) {
        return thisSign
      } else {
        var hi$7 = (diffPrecision >> 31);
        var lo$4 = (((-1) + lo$2) | 0);
        var hi$8 = ((lo$4 !== (-1)) ? hi$4 : (((-1) + hi$4) | 0));
        if (((hi$7 === hi$8) ? (((-2147483648) ^ diffPrecision) < ((-2147483648) ^ lo$4)) : (hi$7 < hi$8))) {
          return ((-thisSign) | 0)
        } else {
          var t$2 = this.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
          var v = bi.java$math$BigDecimal$$getUnscaledValue__Ljava_math_BigInteger();
          if ((hi$4 < 0)) {
            var jsx$1 = $m_Ljava_math_Multiplication$();
            var lo$5 = ((-lo$2) | 0);
            var hi$9 = ((lo$2 !== 0) ? (~hi$4) : ((-hi$4) | 0));
            var _1 = t$2.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(jsx$1.powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo$5, hi$9)));
            var x1_$_$$und1$f = _1;
            var x1_$_$$und2$f = v
          } else if (((hi$4 === 0) ? (lo$2 !== 0) : (hi$4 > 0))) {
            var _2 = v.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_Multiplication$().powerOf10__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(lo$2, hi$4)));
            var x1_$_$$und1$f = t$2;
            var x1_$_$$und2$f = _2
          } else {
            var x1_$_$$und1$f = t$2;
            var x1_$_$$und2$f = v
          };
          var thisUnscaled = $as_Ljava_math_BigInteger(x1_$_$$und1$f);
          var valUnscaled = $as_Ljava_math_BigInteger(x1_$_$$und2$f);
          return thisUnscaled.compareTo__Ljava_math_BigInteger__I(valUnscaled)
        }
      }
    }
  } else {
    return ((thisSign < valueSign) ? (-1) : 1)
  }
});
function $is_Ljava_math_BigDecimal(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_math_BigDecimal)))
}
function $as_Ljava_math_BigDecimal(obj) {
  return (($is_Ljava_math_BigDecimal(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.math.BigDecimal"))
}
function $isArrayOf_Ljava_math_BigDecimal(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_math_BigDecimal)))
}
function $asArrayOf_Ljava_math_BigDecimal(obj, depth) {
  return (($isArrayOf_Ljava_math_BigDecimal(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.math.BigDecimal;", depth))
}
var $d_Ljava_math_BigDecimal = new $TypeData().initClass({
  Ljava_math_BigDecimal: 0
}, false, "java.math.BigDecimal", {
  Ljava_math_BigDecimal: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
});
$c_Ljava_math_BigDecimal.prototype.$classData = $d_Ljava_math_BigDecimal;
/** @constructor */
function $c_Ljava_math_BigInteger() {
  $c_jl_Number.call(this);
  this.digits$2 = null;
  this.numberLength$2 = 0;
  this.sign$2 = 0;
  this.java$math$BigInteger$$firstNonzeroDigit$2 = 0;
  this.$$undhashCode$2 = 0
}
$c_Ljava_math_BigInteger.prototype = new $h_jl_Number();
$c_Ljava_math_BigInteger.prototype.constructor = $c_Ljava_math_BigInteger;
/** @constructor */
function $h_Ljava_math_BigInteger() {
  /*<skip>*/
}
$h_Ljava_math_BigInteger.prototype = $c_Ljava_math_BigInteger.prototype;
$c_Ljava_math_BigInteger.prototype.pow__I__Ljava_math_BigInteger = (function(exp) {
  if ((exp < 0)) {
    throw new $c_jl_ArithmeticException().init___T("Negative exponent")
  } else if ((exp === 0)) {
    return $m_Ljava_math_BigInteger$().ONE$1
  } else if ((((exp === 1) || this.equals__O__Z($m_Ljava_math_BigInteger$().ONE$1)) || this.equals__O__Z($m_Ljava_math_BigInteger$().ZERO$1))) {
    return this
  } else if ((!this.testBit__I__Z(0))) {
    var x = 1;
    while ((!this.testBit__I__Z(x))) {
      x = ((1 + x) | 0)
    };
    return $m_Ljava_math_BigInteger$().getPowerOfTwo__I__Ljava_math_BigInteger($imul(x, exp)).multiply__Ljava_math_BigInteger__Ljava_math_BigInteger(this.shiftRight__I__Ljava_math_BigInteger(x).pow__I__Ljava_math_BigInteger(exp))
  } else {
    return $m_Ljava_math_Multiplication$().pow__Ljava_math_BigInteger__I__Ljava_math_BigInteger(this, exp)
  }
});
$c_Ljava_math_BigInteger.prototype.longValue__J = (function() {
  if ((this.numberLength$2 > 1)) {
    var value = this.digits$2.get(1);
    var value$1 = this.digits$2.get(0);
    var value$3_$_lo$2 = value$1;
    var value$3_$_hi$2 = value
  } else {
    var value$2 = this.digits$2.get(0);
    var value$3_$_lo$2 = value$2;
    var value$3_$_hi$2 = 0
  };
  var value$4 = this.sign$2;
  var hi$3 = (value$4 >> 31);
  var blo = value$3_$_lo$2;
  var a0 = (65535 & value$4);
  var a1 = ((value$4 >>> 16) | 0);
  var b0 = (65535 & blo);
  var b1 = ((blo >>> 16) | 0);
  var a0b0 = $imul(a0, b0);
  var a1b0 = $imul(a1, b0);
  var a0b1 = $imul(a0, b1);
  var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
  var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
  var hi$4 = (((((((($imul(value$4, value$3_$_hi$2) + $imul(hi$3, blo)) | 0) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, hi$4)
});
$c_Ljava_math_BigInteger.prototype.init___ = (function() {
  this.java$math$BigInteger$$firstNonzeroDigit$2 = (-2);
  this.$$undhashCode$2 = 0;
  return this
});
$c_Ljava_math_BigInteger.prototype.divide__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(divisor) {
  if ((divisor.sign$2 === 0)) {
    throw new $c_jl_ArithmeticException().init___T("BigInteger divide by zero")
  };
  var divisorSign = divisor.sign$2;
  if (divisor.isOne__Z()) {
    return ((divisor.sign$2 > 0) ? this : this.negate__Ljava_math_BigInteger())
  } else {
    var thisSign = this.sign$2;
    var thisLen = this.numberLength$2;
    var divisorLen = divisor.numberLength$2;
    if ((((thisLen + divisorLen) | 0) === 2)) {
      var value = this.digits$2.get(0);
      var value$1 = divisor.digits$2.get(0);
      var this$1 = $m_sjsr_RuntimeLong$();
      var lo = this$1.divideImpl__I__I__I__I__I(value, 0, value$1, 0);
      var hi$2 = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
      var bi_$_lo$2 = lo;
      var bi_$_hi$2 = hi$2;
      if ((thisSign !== divisorSign)) {
        var this$2_$_lo$2 = bi_$_lo$2;
        var this$2_$_hi$2 = bi_$_hi$2;
        var lo$1 = this$2_$_lo$2;
        var hi$3 = this$2_$_hi$2;
        var lo$2 = ((-lo$1) | 0);
        var hi$4 = ((lo$1 !== 0) ? (~hi$3) : ((-hi$3) | 0));
        var jsx$1_$_lo$2 = lo$2;
        var jsx$1_$_hi$2 = hi$4;
        bi_$_lo$2 = jsx$1_$_lo$2;
        bi_$_hi$2 = jsx$1_$_hi$2
      };
      return $m_Ljava_math_BigInteger$().valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(bi_$_lo$2, bi_$_hi$2))
    } else {
      var cmp = ((thisLen !== divisorLen) ? ((thisLen > divisorLen) ? 1 : (-1)) : $m_Ljava_math_Elementary$().compareArrays__AI__AI__I__I(this.digits$2, divisor.digits$2, thisLen));
      if ((cmp === 0)) {
        return ((thisSign === divisorSign) ? $m_Ljava_math_BigInteger$().ONE$1 : $m_Ljava_math_BigInteger$().MINUS$undONE$1)
      } else if ((cmp === (-1))) {
        return $m_Ljava_math_BigInteger$().ZERO$1
      } else {
        var resLength = ((1 + ((thisLen - divisorLen) | 0)) | 0);
        var resDigits = $newArrayObject($d_I.getArrayOf(), [resLength]);
        var resSign = ((thisSign === divisorSign) ? 1 : (-1));
        if ((divisorLen === 1)) {
          $m_Ljava_math_Division$().divideArrayByInt__AI__AI__I__I__I(resDigits, this.digits$2, thisLen, divisor.digits$2.get(0))
        } else {
          $m_Ljava_math_Division$().divide__AI__I__AI__I__AI__I__AI(resDigits, resLength, this.digits$2, thisLen, divisor.digits$2, divisorLen)
        };
        var result = new $c_Ljava_math_BigInteger().init___I__I__AI(resSign, resLength, resDigits);
        result.cutOffLeadingZeroes__V();
        return result
      }
    }
  }
});
$c_Ljava_math_BigInteger.prototype.remainder__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(divisor) {
  if ((divisor.sign$2 === 0)) {
    throw new $c_jl_ArithmeticException().init___T("BigInteger divide by zero")
  };
  var thisLen = this.numberLength$2;
  var divisorLen = divisor.numberLength$2;
  var cmp = ((thisLen !== divisorLen) ? ((thisLen > divisorLen) ? 1 : (-1)) : $m_Ljava_math_Elementary$().compareArrays__AI__AI__I__I(this.digits$2, divisor.digits$2, thisLen));
  if ((cmp === (-1))) {
    return this
  } else {
    var resDigits = $newArrayObject($d_I.getArrayOf(), [divisorLen]);
    if ((divisorLen === 1)) {
      resDigits.set(0, $m_Ljava_math_Division$().remainderArrayByInt__AI__I__I__I(this.digits$2, thisLen, divisor.digits$2.get(0)))
    } else {
      var qLen = ((1 + ((thisLen - divisorLen) | 0)) | 0);
      resDigits = $m_Ljava_math_Division$().divide__AI__I__AI__I__AI__I__AI(null, qLen, this.digits$2, thisLen, divisor.digits$2, divisorLen)
    };
    var result = new $c_Ljava_math_BigInteger().init___I__I__AI(this.sign$2, divisorLen, resDigits);
    result.cutOffLeadingZeroes__V();
    return result
  }
});
$c_Ljava_math_BigInteger.prototype.equals__O__Z = (function(x) {
  if ($is_Ljava_math_BigInteger(x)) {
    var x2 = $as_Ljava_math_BigInteger(x);
    return (((this.sign$2 === x2.sign$2) && (this.numberLength$2 === x2.numberLength$2)) && this.equalsArrays__AI__Z(x2.digits$2))
  } else {
    return false
  }
});
$c_Ljava_math_BigInteger.prototype.max__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(bi) {
  return ((this.compareTo__Ljava_math_BigInteger__I(bi) === 1) ? this : bi)
});
$c_Ljava_math_BigInteger.prototype.toString__T = (function() {
  return $m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this)
});
$c_Ljava_math_BigInteger.prototype.init___I__I = (function(sign, value) {
  $c_Ljava_math_BigInteger.prototype.init___.call(this);
  this.sign$2 = sign;
  this.numberLength$2 = 1;
  this.digits$2 = $m_s_Array$().apply__I__sc_Seq__AI(value, new $c_sjs_js_WrappedArray().init___sjs_js_Array([]));
  return this
});
$c_Ljava_math_BigInteger.prototype.getFirstNonzeroDigit__I = (function() {
  if ((this.java$math$BigInteger$$firstNonzeroDigit$2 === (-2))) {
    if ((this.sign$2 === 0)) {
      var jsx$1 = (-1)
    } else {
      var i = 0;
      while ((this.digits$2.get(i) === 0)) {
        i = ((1 + i) | 0)
      };
      var jsx$1 = i
    };
    this.java$math$BigInteger$$firstNonzeroDigit$2 = jsx$1
  };
  return this.java$math$BigInteger$$firstNonzeroDigit$2
});
$c_Ljava_math_BigInteger.prototype.copy__Ljava_math_BigInteger = (function() {
  var copyDigits = $newArrayObject($d_I.getArrayOf(), [this.numberLength$2]);
  $systemArraycopy(this.digits$2, 0, copyDigits, 0, this.numberLength$2);
  return new $c_Ljava_math_BigInteger().init___I__I__AI(this.sign$2, this.numberLength$2, copyDigits)
});
$c_Ljava_math_BigInteger.prototype.equalsArrays__AI__Z = (function(b) {
  var end = this.numberLength$2;
  var this$4 = new $c_sci_Range().init___I__I__I(0, end, 1);
  var this$5 = new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this$4, 0, this$4.length__I());
  var res = true;
  while ((res && this$5.hasNext__Z())) {
    var arg1 = this$5.next__O();
    var i = $uI(arg1);
    res = (this.digits$2.get(i) === b.get(i))
  };
  return res
});
$c_Ljava_math_BigInteger.prototype.abs__Ljava_math_BigInteger = (function() {
  return ((this.sign$2 < 0) ? new $c_Ljava_math_BigInteger().init___I__I__AI(1, this.numberLength$2, this.digits$2) : this)
});
$c_Ljava_math_BigInteger.prototype.setFromString__p2__T__I__V = (function(s, radix) {
  if ((((s === "") || (s === "+")) || (s === "-"))) {
    throw new $c_jl_NumberFormatException().init___T("Zero length BigInteger")
  };
  var stringLength0 = $uI(s.length);
  if (((65535 & $uI(s.charCodeAt(0))) === 45)) {
    var _3 = (((-1) + stringLength0) | 0);
    var x1_$_$$und1$1 = (-1);
    var x1_$_$$und2$1 = 1;
    var x1_$_$$und3$1 = _3
  } else if (((65535 & $uI(s.charCodeAt(0))) === 43)) {
    var _3$1 = (((-1) + stringLength0) | 0);
    var x1_$_$$und1$1 = 1;
    var x1_$_$$und2$1 = 1;
    var x1_$_$$und3$1 = _3$1
  } else {
    var x1_$_$$und1$1 = 1;
    var x1_$_$$und2$1 = 0;
    var x1_$_$$und3$1 = stringLength0
  };
  var _sign = $uI(x1_$_$$und1$1);
  var startChar = $uI(x1_$_$$und2$1);
  var stringLength = $uI(x1_$_$$und3$1);
  var isEmpty$4 = (startChar >= stringLength0);
  var scala$collection$immutable$Range$$lastElement$4 = (((-1) + stringLength0) | 0);
  if ((!isEmpty$4)) {
    var i = startChar;
    while (true) {
      var v1 = i;
      var c = (65535 & $uI(s.charCodeAt(v1)));
      if (((c === 43) || (c === 45))) {
        throw new $c_jl_NumberFormatException().init___T("Illegal embedded sign character")
      };
      if ((i === scala$collection$immutable$Range$$lastElement$4)) {
        break
      };
      i = ((1 + i) | 0)
    }
  };
  var charsPerInt = $m_Ljava_math_Conversion$().DigitFitInInt$1.get(radix);
  var bigRadixDigitsLength = ((stringLength / charsPerInt) | 0);
  var topChars = ((stringLength % charsPerInt) | 0);
  if ((topChars !== 0)) {
    bigRadixDigitsLength = ((1 + bigRadixDigitsLength) | 0)
  };
  var _digits = $newArrayObject($d_I.getArrayOf(), [bigRadixDigitsLength]);
  var bigRadix = $m_Ljava_math_Conversion$().BigRadices$1.get((((-2) + radix) | 0));
  var digitIndex = 0;
  var substrEnd = ((startChar + ((topChars === 0) ? charsPerInt : topChars)) | 0);
  var newDigit = 0;
  var substrStart = startChar;
  while ((substrStart < stringLength0)) {
    var jsx$1 = $m_jl_Integer$();
    var beginIndex = substrStart;
    var endIndex = substrEnd;
    var bigRadixDigit = jsx$1.parseInt__T__I__I($as_T(s.substring(beginIndex, endIndex)), radix);
    var this$13 = $m_Ljava_math_Multiplication$();
    var aSize = digitIndex;
    newDigit = this$13.multiplyByInt__p1__AI__AI__I__I__I(_digits, _digits, aSize, bigRadix);
    newDigit = ((newDigit + $m_Ljava_math_Elementary$().inplaceAdd__AI__I__I__I(_digits, digitIndex, bigRadixDigit)) | 0);
    _digits.set(digitIndex, newDigit);
    digitIndex = ((1 + digitIndex) | 0);
    substrStart = substrEnd;
    substrEnd = ((substrStart + charsPerInt) | 0)
  };
  this.sign$2 = _sign;
  this.numberLength$2 = digitIndex;
  this.digits$2 = _digits;
  this.cutOffLeadingZeroes__V()
});
$c_Ljava_math_BigInteger.prototype.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem = (function(divisor) {
  var divisorSign = divisor.sign$2;
  if ((divisorSign === 0)) {
    throw new $c_jl_ArithmeticException().init___T("BigInteger divide by zero")
  };
  var divisorLen = divisor.numberLength$2;
  var divisorDigits = divisor.digits$2;
  if ((divisorLen === 1)) {
    return $m_Ljava_math_Division$().divideAndRemainderByInteger__Ljava_math_BigInteger__I__I__Ljava_math_BigInteger$QuotAndRem(this, divisorDigits.get(0), divisorSign)
  } else {
    var thisDigits = this.digits$2;
    var thisLen = this.numberLength$2;
    var cmp = ((thisLen !== divisorLen) ? ((thisLen > divisorLen) ? 1 : (-1)) : $m_Ljava_math_Elementary$().compareArrays__AI__AI__I__I(thisDigits, divisorDigits, thisLen));
    if ((cmp < 0)) {
      return new $c_Ljava_math_BigInteger$QuotAndRem().init___Ljava_math_BigInteger__Ljava_math_BigInteger($m_Ljava_math_BigInteger$().ZERO$1, this)
    } else {
      var thisSign = this.sign$2;
      var quotientLength = ((1 + ((thisLen - divisorLen) | 0)) | 0);
      var quotientSign = ((thisSign === divisorSign) ? 1 : (-1));
      var quotientDigits = $newArrayObject($d_I.getArrayOf(), [quotientLength]);
      var remainderDigits = $m_Ljava_math_Division$().divide__AI__I__AI__I__AI__I__AI(quotientDigits, quotientLength, thisDigits, thisLen, divisorDigits, divisorLen);
      var result0 = new $c_Ljava_math_BigInteger().init___I__I__AI(quotientSign, quotientLength, quotientDigits);
      var result1 = new $c_Ljava_math_BigInteger().init___I__I__AI(thisSign, divisorLen, remainderDigits);
      result0.cutOffLeadingZeroes__V();
      result1.cutOffLeadingZeroes__V();
      return new $c_Ljava_math_BigInteger$QuotAndRem().init___Ljava_math_BigInteger__Ljava_math_BigInteger(result0, result1)
    }
  }
});
$c_Ljava_math_BigInteger.prototype.cutOffLeadingZeroes__V = (function() {
  _loop: while (true) {
    if ((this.numberLength$2 > 0)) {
      this.numberLength$2 = (((-1) + this.numberLength$2) | 0);
      if ((this.digits$2.get(this.numberLength$2) === 0)) {
        continue _loop
      }
    };
    break
  };
  if ((this.digits$2.get(this.numberLength$2) === 0)) {
    this.sign$2 = 0
  };
  this.numberLength$2 = ((1 + this.numberLength$2) | 0)
});
$c_Ljava_math_BigInteger.prototype.testBit__I__Z = (function(n) {
  var intCount = (n >> 5);
  if ((n === 0)) {
    return ((1 & this.digits$2.get(0)) !== 0)
  } else if ((n < 0)) {
    throw new $c_jl_ArithmeticException().init___T("Negative bit address")
  } else if ((intCount >= this.numberLength$2)) {
    return (this.sign$2 < 0)
  } else if ((!((this.sign$2 < 0) && (intCount < this.getFirstNonzeroDigit__I())))) {
    var digit = this.digits$2.get(intCount);
    if ((this.sign$2 < 0)) {
      digit = ((this.getFirstNonzeroDigit__I() === intCount) ? ((-digit) | 0) : (~digit))
    };
    var i = (1 << (31 & n));
    return ((digit & i) !== 0)
  } else {
    return false
  }
});
$c_Ljava_math_BigInteger.prototype.getLowestSetBit__I = (function() {
  if ((this.sign$2 === 0)) {
    return (-1)
  } else {
    var i = this.getFirstNonzeroDigit__I();
    var i$1 = this.digits$2.get(i);
    return (((i << 5) + ((i$1 === 0) ? 32 : ((31 - $clz32((i$1 & ((-i$1) | 0)))) | 0))) | 0)
  }
});
$c_Ljava_math_BigInteger.prototype.negate__Ljava_math_BigInteger = (function() {
  return ((this.sign$2 === 0) ? this : new $c_Ljava_math_BigInteger().init___I__I__AI(((-this.sign$2) | 0), this.numberLength$2, this.digits$2))
});
$c_Ljava_math_BigInteger.prototype.init___I__I__AI = (function(sign, numberLength, digits) {
  $c_Ljava_math_BigInteger.prototype.init___.call(this);
  this.sign$2 = sign;
  this.numberLength$2 = numberLength;
  this.digits$2 = digits;
  return this
});
$c_Ljava_math_BigInteger.prototype.shiftLeftOneBit__Ljava_math_BigInteger = (function() {
  return ((this.sign$2 === 0) ? this : $m_Ljava_math_BitLevel$().shiftLeftOneBit__Ljava_math_BigInteger__Ljava_math_BigInteger(this))
});
$c_Ljava_math_BigInteger.prototype.init___T__I = (function(s, radix) {
  $c_Ljava_math_BigInteger.prototype.init___.call(this);
  $m_Ljava_math_BigInteger$();
  if ((s === null)) {
    throw new $c_jl_NullPointerException().init___()
  };
  if (((radix < 2) || (radix > 36))) {
    throw new $c_jl_NumberFormatException().init___T("Radix out of range")
  };
  if ((s === null)) {
    throw new $c_jl_NullPointerException().init___()
  };
  if ((s === "")) {
    throw new $c_jl_NumberFormatException().init___T("Zero length BigInteger")
  };
  this.setFromString__p2__T__I__V(s, radix);
  return this
});
$c_Ljava_math_BigInteger.prototype.hashCode__I = (function() {
  if ((this.$$undhashCode$2 !== 0)) {
    return this.$$undhashCode$2
  } else {
    var end = this.numberLength$2;
    var isEmpty$4 = (end <= 0);
    var scala$collection$immutable$Range$$lastElement$4 = (((-1) + end) | 0);
    if ((!isEmpty$4)) {
      var i = 0;
      while (true) {
        var v1 = i;
        this.$$undhashCode$2 = (($imul(33, this.$$undhashCode$2) + this.digits$2.get(v1)) | 0);
        if ((i === scala$collection$immutable$Range$$lastElement$4)) {
          break
        };
        i = ((1 + i) | 0)
      }
    };
    this.$$undhashCode$2 = $imul(this.$$undhashCode$2, this.sign$2);
    return this.$$undhashCode$2
  }
});
$c_Ljava_math_BigInteger.prototype.init___T = (function(s) {
  $c_Ljava_math_BigInteger.prototype.init___T__I.call(this, s, 10);
  return this
});
$c_Ljava_math_BigInteger.prototype.divideAndRemainder__Ljava_math_BigInteger__ALjava_math_BigInteger = (function(divisor) {
  return this.divideAndRemainderImpl__Ljava_math_BigInteger__Ljava_math_BigInteger$QuotAndRem(divisor).toArray__ALjava_math_BigInteger()
});
$c_Ljava_math_BigInteger.prototype.shiftLeft__I__Ljava_math_BigInteger = (function(n) {
  return (((n === 0) || (this.sign$2 === 0)) ? this : ((n > 0) ? $m_Ljava_math_BitLevel$().shiftLeft__Ljava_math_BigInteger__I__Ljava_math_BigInteger(this, n) : $m_Ljava_math_BitLevel$().shiftRight__Ljava_math_BigInteger__I__Ljava_math_BigInteger(this, ((-n) | 0))))
});
$c_Ljava_math_BigInteger.prototype.isOne__Z = (function() {
  return ((this.numberLength$2 === 1) && (this.digits$2.get(0) === 1))
});
$c_Ljava_math_BigInteger.prototype.intValue__I = (function() {
  return $imul(this.sign$2, this.digits$2.get(0))
});
$c_Ljava_math_BigInteger.prototype.multiply__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(bi) {
  if (((bi.sign$2 === 0) || (this.sign$2 === 0))) {
    return $m_Ljava_math_BigInteger$().ZERO$1
  } else {
    var this$1 = $m_Ljava_math_Multiplication$();
    return this$1.karatsuba__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this, bi)
  }
});
$c_Ljava_math_BigInteger.prototype.init___I__J = (function(sign, lVal) {
  $c_Ljava_math_BigInteger.prototype.init___.call(this);
  this.sign$2 = sign;
  var lo = lVal.hi$2;
  if ((lo === 0)) {
    this.numberLength$2 = 1;
    this.digits$2 = $m_s_Array$().apply__I__sc_Seq__AI(lVal.lo$2, new $c_sjs_js_WrappedArray().init___sjs_js_Array([]))
  } else {
    this.numberLength$2 = 2;
    this.digits$2 = $m_s_Array$().apply__I__sc_Seq__AI(lVal.lo$2, new $c_sjs_js_WrappedArray().init___sjs_js_Array([lo]))
  };
  return this
});
$c_Ljava_math_BigInteger.prototype.gcd__Ljava_math_BigInteger__Ljava_math_BigInteger = (function(bi) {
  var val1 = this.abs__Ljava_math_BigInteger();
  var val2 = bi.abs__Ljava_math_BigInteger();
  if ((val1.sign$2 === 0)) {
    return val2
  } else if ((val2.sign$2 === 0)) {
    return val1
  } else if ((((val1.numberLength$2 === 1) && (val1.digits$2.get(0) > 0)) && ((val2.numberLength$2 === 1) && (val2.digits$2.get(0) > 0)))) {
    var jsx$1 = $m_Ljava_math_BigInteger$();
    var value = $m_Ljava_math_Division$().gcdBinary__I__I__I(val1.intValue__I(), val2.intValue__I());
    var hi = (value >> 31);
    return jsx$1.valueOf__J__Ljava_math_BigInteger(new $c_sjsr_RuntimeLong().init___I__I(value, hi))
  } else {
    return $m_Ljava_math_Division$().gcdBinary__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(val1.copy__Ljava_math_BigInteger(), val2.copy__Ljava_math_BigInteger())
  }
});
$c_Ljava_math_BigInteger.prototype.shiftRight__I__Ljava_math_BigInteger = (function(n) {
  return (((n === 0) || (this.sign$2 === 0)) ? this : ((n > 0) ? $m_Ljava_math_BitLevel$().shiftRight__Ljava_math_BigInteger__I__Ljava_math_BigInteger(this, n) : $m_Ljava_math_BitLevel$().shiftLeft__Ljava_math_BigInteger__I__Ljava_math_BigInteger(this, ((-n) | 0))))
});
$c_Ljava_math_BigInteger.prototype.init___I__AI = (function(signum, digits) {
  $c_Ljava_math_BigInteger.prototype.init___.call(this);
  if ((digits.u.length === 0)) {
    this.sign$2 = 0;
    this.numberLength$2 = 1;
    this.digits$2 = $m_s_Array$().apply__I__sc_Seq__AI(0, new $c_sjs_js_WrappedArray().init___sjs_js_Array([]))
  } else {
    this.sign$2 = signum;
    this.numberLength$2 = digits.u.length;
    this.digits$2 = digits;
    this.cutOffLeadingZeroes__V()
  };
  return this
});
$c_Ljava_math_BigInteger.prototype.compareTo__Ljava_math_BigInteger__I = (function(bi) {
  return ((this.sign$2 > bi.sign$2) ? 1 : ((this.sign$2 < bi.sign$2) ? (-1) : ((this.numberLength$2 > bi.numberLength$2) ? this.sign$2 : ((this.numberLength$2 < bi.numberLength$2) ? ((-bi.sign$2) | 0) : $imul(this.sign$2, $m_Ljava_math_Elementary$().compareArrays__AI__AI__I__I(this.digits$2, bi.digits$2, this.numberLength$2))))))
});
function $is_Ljava_math_BigInteger(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_math_BigInteger)))
}
function $as_Ljava_math_BigInteger(obj) {
  return (($is_Ljava_math_BigInteger(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.math.BigInteger"))
}
function $isArrayOf_Ljava_math_BigInteger(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_math_BigInteger)))
}
function $asArrayOf_Ljava_math_BigInteger(obj, depth) {
  return (($isArrayOf_Ljava_math_BigInteger(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.math.BigInteger;", depth))
}
var $d_Ljava_math_BigInteger = new $TypeData().initClass({
  Ljava_math_BigInteger: 0
}, false, "java.math.BigInteger", {
  Ljava_math_BigInteger: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
});
$c_Ljava_math_BigInteger.prototype.$classData = $d_Ljava_math_BigInteger;
/** @constructor */
function $c_Ljava_math_RoundingMode() {
  $c_jl_Enum.call(this)
}
$c_Ljava_math_RoundingMode.prototype = new $h_jl_Enum();
$c_Ljava_math_RoundingMode.prototype.constructor = $c_Ljava_math_RoundingMode;
/** @constructor */
function $h_Ljava_math_RoundingMode() {
  /*<skip>*/
}
$h_Ljava_math_RoundingMode.prototype = $c_Ljava_math_RoundingMode.prototype;
$c_Ljava_math_RoundingMode.prototype.init___T__I = (function(name, ordinal) {
  $c_jl_Enum.prototype.init___T__I.call(this, name, ordinal);
  return this
});
var $d_Ljava_math_RoundingMode = new $TypeData().initClass({
  Ljava_math_RoundingMode: 0
}, false, "java.math.RoundingMode", {
  Ljava_math_RoundingMode: 1,
  jl_Enum: 1,
  O: 1,
  jl_Comparable: 1,
  Ljava_io_Serializable: 1
});
$c_Ljava_math_RoundingMode.prototype.$classData = $d_Ljava_math_RoundingMode;
function $is_T(obj) {
  return ((typeof obj) === "string")
}
function $as_T(obj) {
  return (($is_T(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.String"))
}
function $isArrayOf_T(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T)))
}
function $asArrayOf_T(obj, depth) {
  return (($isArrayOf_T(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.String;", depth))
}
var $d_T = new $TypeData().initClass({
  T: 0
}, false, "java.lang.String", {
  T: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_CharSequence: 1,
  jl_Comparable: 1
}, (void 0), (void 0), $is_T);
/** @constructor */
function $c_jl_AssertionError() {
  $c_jl_Error.call(this)
}
$c_jl_AssertionError.prototype = new $h_jl_Error();
$c_jl_AssertionError.prototype.constructor = $c_jl_AssertionError;
/** @constructor */
function $h_jl_AssertionError() {
  /*<skip>*/
}
$h_jl_AssertionError.prototype = $c_jl_AssertionError.prototype;
$c_jl_AssertionError.prototype.init___O = (function(o) {
  var s = $objectToString(o);
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_AssertionError = new $TypeData().initClass({
  jl_AssertionError: 0
}, false, "java.lang.AssertionError", {
  jl_AssertionError: 1,
  jl_Error: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_AssertionError.prototype.$classData = $d_jl_AssertionError;
function $isArrayOf_jl_Byte(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Byte)))
}
function $asArrayOf_jl_Byte(obj, depth) {
  return (($isArrayOf_jl_Byte(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Byte;", depth))
}
var $d_jl_Byte = new $TypeData().initClass({
  jl_Byte: 0
}, false, "java.lang.Byte", {
  jl_Byte: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return $isByte(x)
}));
/** @constructor */
function $c_jl_CloneNotSupportedException() {
  $c_jl_Exception.call(this)
}
$c_jl_CloneNotSupportedException.prototype = new $h_jl_Exception();
$c_jl_CloneNotSupportedException.prototype.constructor = $c_jl_CloneNotSupportedException;
/** @constructor */
function $h_jl_CloneNotSupportedException() {
  /*<skip>*/
}
$h_jl_CloneNotSupportedException.prototype = $c_jl_CloneNotSupportedException.prototype;
$c_jl_CloneNotSupportedException.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
var $d_jl_CloneNotSupportedException = new $TypeData().initClass({
  jl_CloneNotSupportedException: 0
}, false, "java.lang.CloneNotSupportedException", {
  jl_CloneNotSupportedException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_CloneNotSupportedException.prototype.$classData = $d_jl_CloneNotSupportedException;
function $isArrayOf_jl_Double(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Double)))
}
function $asArrayOf_jl_Double(obj, depth) {
  return (($isArrayOf_jl_Double(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Double;", depth))
}
var $d_jl_Double = new $TypeData().initClass({
  jl_Double: 0
}, false, "java.lang.Double", {
  jl_Double: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return ((typeof x) === "number")
}));
function $isArrayOf_jl_Float(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Float)))
}
function $asArrayOf_jl_Float(obj, depth) {
  return (($isArrayOf_jl_Float(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Float;", depth))
}
var $d_jl_Float = new $TypeData().initClass({
  jl_Float: 0
}, false, "java.lang.Float", {
  jl_Float: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return $isFloat(x)
}));
function $isArrayOf_jl_Integer(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Integer)))
}
function $asArrayOf_jl_Integer(obj, depth) {
  return (($isArrayOf_jl_Integer(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Integer;", depth))
}
var $d_jl_Integer = new $TypeData().initClass({
  jl_Integer: 0
}, false, "java.lang.Integer", {
  jl_Integer: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return $isInt(x)
}));
/** @constructor */
function $c_jl_JSConsoleBasedPrintStream$DummyOutputStream() {
  $c_Ljava_io_OutputStream.call(this)
}
$c_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype = new $h_Ljava_io_OutputStream();
$c_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype.constructor = $c_jl_JSConsoleBasedPrintStream$DummyOutputStream;
/** @constructor */
function $h_jl_JSConsoleBasedPrintStream$DummyOutputStream() {
  /*<skip>*/
}
$h_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype = $c_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype;
$c_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype.init___ = (function() {
  return this
});
var $d_jl_JSConsoleBasedPrintStream$DummyOutputStream = new $TypeData().initClass({
  jl_JSConsoleBasedPrintStream$DummyOutputStream: 0
}, false, "java.lang.JSConsoleBasedPrintStream$DummyOutputStream", {
  jl_JSConsoleBasedPrintStream$DummyOutputStream: 1,
  Ljava_io_OutputStream: 1,
  O: 1,
  Ljava_io_Closeable: 1,
  Ljava_io_Flushable: 1
});
$c_jl_JSConsoleBasedPrintStream$DummyOutputStream.prototype.$classData = $d_jl_JSConsoleBasedPrintStream$DummyOutputStream;
function $isArrayOf_jl_Long(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Long)))
}
function $asArrayOf_jl_Long(obj, depth) {
  return (($isArrayOf_jl_Long(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Long;", depth))
}
var $d_jl_Long = new $TypeData().initClass({
  jl_Long: 0
}, false, "java.lang.Long", {
  jl_Long: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return $is_sjsr_RuntimeLong(x)
}));
/** @constructor */
function $c_jl_RuntimeException() {
  $c_jl_Exception.call(this)
}
$c_jl_RuntimeException.prototype = new $h_jl_Exception();
$c_jl_RuntimeException.prototype.constructor = $c_jl_RuntimeException;
/** @constructor */
function $h_jl_RuntimeException() {
  /*<skip>*/
}
$h_jl_RuntimeException.prototype = $c_jl_RuntimeException.prototype;
function $isArrayOf_jl_Short(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_Short)))
}
function $asArrayOf_jl_Short(obj, depth) {
  return (($isArrayOf_jl_Short(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.Short;", depth))
}
var $d_jl_Short = new $TypeData().initClass({
  jl_Short: 0
}, false, "java.lang.Short", {
  jl_Short: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
}, (void 0), (void 0), (function(x) {
  return $isShort(x)
}));
/** @constructor */
function $c_jl_StringBuilder() {
  $c_O.call(this);
  this.content$1 = null
}
$c_jl_StringBuilder.prototype = new $h_O();
$c_jl_StringBuilder.prototype.constructor = $c_jl_StringBuilder;
/** @constructor */
function $h_jl_StringBuilder() {
  /*<skip>*/
}
$h_jl_StringBuilder.prototype = $c_jl_StringBuilder.prototype;
$c_jl_StringBuilder.prototype.init___ = (function() {
  $c_jl_StringBuilder.prototype.init___T.call(this, "");
  return this
});
$c_jl_StringBuilder.prototype.append__T__jl_StringBuilder = (function(s) {
  this.content$1 = (("" + this.content$1) + ((s === null) ? "null" : s));
  return this
});
$c_jl_StringBuilder.prototype.subSequence__I__I__jl_CharSequence = (function(start, end) {
  var thiz = this.content$1;
  return $as_T(thiz.substring(start, end))
});
$c_jl_StringBuilder.prototype.toString__T = (function() {
  return this.content$1
});
$c_jl_StringBuilder.prototype.append__O__jl_StringBuilder = (function(obj) {
  return ((obj === null) ? this.append__T__jl_StringBuilder(null) : this.append__T__jl_StringBuilder($objectToString(obj)))
});
$c_jl_StringBuilder.prototype.init___I = (function(initialCapacity) {
  $c_jl_StringBuilder.prototype.init___T.call(this, "");
  return this
});
$c_jl_StringBuilder.prototype.append__jl_CharSequence__I__I__jl_StringBuilder = (function(csq, start, end) {
  return ((csq === null) ? this.append__jl_CharSequence__I__I__jl_StringBuilder("null", start, end) : this.append__T__jl_StringBuilder($objectToString($charSequenceSubSequence(csq, start, end))))
});
$c_jl_StringBuilder.prototype.append__C__jl_StringBuilder = (function(c) {
  return this.append__T__jl_StringBuilder($as_T($g.String.fromCharCode(c)))
});
$c_jl_StringBuilder.prototype.init___T = (function(content) {
  this.content$1 = content;
  return this
});
var $d_jl_StringBuilder = new $TypeData().initClass({
  jl_StringBuilder: 0
}, false, "java.lang.StringBuilder", {
  jl_StringBuilder: 1,
  O: 1,
  jl_CharSequence: 1,
  jl_Appendable: 1,
  Ljava_io_Serializable: 1
});
$c_jl_StringBuilder.prototype.$classData = $d_jl_StringBuilder;
/** @constructor */
function $c_s_Array$() {
  $c_s_FallbackArrayBuilding.call(this)
}
$c_s_Array$.prototype = new $h_s_FallbackArrayBuilding();
$c_s_Array$.prototype.constructor = $c_s_Array$;
/** @constructor */
function $h_s_Array$() {
  /*<skip>*/
}
$h_s_Array$.prototype = $c_s_Array$.prototype;
$c_s_Array$.prototype.init___ = (function() {
  return this
});
$c_s_Array$.prototype.apply__I__sc_Seq__AI = (function(x, xs) {
  var array = $newArrayObject($d_I.getArrayOf(), [((1 + xs.length__I()) | 0)]);
  array.set(0, x);
  var elem$1 = 0;
  elem$1 = 1;
  var this$2 = xs.iterator__sc_Iterator();
  while (this$2.hasNext__Z()) {
    var arg1 = this$2.next__O();
    var x$2 = $uI(arg1);
    array.set(elem$1, x$2);
    elem$1 = ((1 + elem$1) | 0)
  };
  return array
});
var $d_s_Array$ = new $TypeData().initClass({
  s_Array$: 0
}, false, "scala.Array$", {
  s_Array$: 1,
  s_FallbackArrayBuilding: 1,
  O: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_Array$.prototype.$classData = $d_s_Array$;
var $n_s_Array$ = (void 0);
function $m_s_Array$() {
  if ((!$n_s_Array$)) {
    $n_s_Array$ = new $c_s_Array$().init___()
  };
  return $n_s_Array$
}
/** @constructor */
function $c_s_Predef$$eq$colon$eq() {
  $c_O.call(this)
}
$c_s_Predef$$eq$colon$eq.prototype = new $h_O();
$c_s_Predef$$eq$colon$eq.prototype.constructor = $c_s_Predef$$eq$colon$eq;
/** @constructor */
function $h_s_Predef$$eq$colon$eq() {
  /*<skip>*/
}
$h_s_Predef$$eq$colon$eq.prototype = $c_s_Predef$$eq$colon$eq.prototype;
$c_s_Predef$$eq$colon$eq.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
function $c_s_Predef$$less$colon$less() {
  $c_O.call(this)
}
$c_s_Predef$$less$colon$less.prototype = new $h_O();
$c_s_Predef$$less$colon$less.prototype.constructor = $c_s_Predef$$less$colon$less;
/** @constructor */
function $h_s_Predef$$less$colon$less() {
  /*<skip>*/
}
$h_s_Predef$$less$colon$less.prototype = $c_s_Predef$$less$colon$less.prototype;
$c_s_Predef$$less$colon$less.prototype.toString__T = (function() {
  return "<function1>"
});
/** @constructor */
function $c_s_math_Equiv$() {
  $c_O.call(this)
}
$c_s_math_Equiv$.prototype = new $h_O();
$c_s_math_Equiv$.prototype.constructor = $c_s_math_Equiv$;
/** @constructor */
function $h_s_math_Equiv$() {
  /*<skip>*/
}
$h_s_math_Equiv$.prototype = $c_s_math_Equiv$.prototype;
$c_s_math_Equiv$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Equiv$ = new $TypeData().initClass({
  s_math_Equiv$: 0
}, false, "scala.math.Equiv$", {
  s_math_Equiv$: 1,
  O: 1,
  s_math_LowPriorityEquiv: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_Equiv$.prototype.$classData = $d_s_math_Equiv$;
var $n_s_math_Equiv$ = (void 0);
function $m_s_math_Equiv$() {
  if ((!$n_s_math_Equiv$)) {
    $n_s_math_Equiv$ = new $c_s_math_Equiv$().init___()
  };
  return $n_s_math_Equiv$
}
/** @constructor */
function $c_s_math_Ordering$() {
  $c_O.call(this)
}
$c_s_math_Ordering$.prototype = new $h_O();
$c_s_math_Ordering$.prototype.constructor = $c_s_math_Ordering$;
/** @constructor */
function $h_s_math_Ordering$() {
  /*<skip>*/
}
$h_s_math_Ordering$.prototype = $c_s_math_Ordering$.prototype;
$c_s_math_Ordering$.prototype.init___ = (function() {
  return this
});
var $d_s_math_Ordering$ = new $TypeData().initClass({
  s_math_Ordering$: 0
}, false, "scala.math.Ordering$", {
  s_math_Ordering$: 1,
  O: 1,
  s_math_LowPriorityOrderingImplicits: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_math_Ordering$.prototype.$classData = $d_s_math_Ordering$;
var $n_s_math_Ordering$ = (void 0);
function $m_s_math_Ordering$() {
  if ((!$n_s_math_Ordering$)) {
    $n_s_math_Ordering$ = new $c_s_math_Ordering$().init___()
  };
  return $n_s_math_Ordering$
}
/** @constructor */
function $c_s_reflect_NoManifest$() {
  $c_O.call(this)
}
$c_s_reflect_NoManifest$.prototype = new $h_O();
$c_s_reflect_NoManifest$.prototype.constructor = $c_s_reflect_NoManifest$;
/** @constructor */
function $h_s_reflect_NoManifest$() {
  /*<skip>*/
}
$h_s_reflect_NoManifest$.prototype = $c_s_reflect_NoManifest$.prototype;
$c_s_reflect_NoManifest$.prototype.init___ = (function() {
  return this
});
$c_s_reflect_NoManifest$.prototype.toString__T = (function() {
  return "<?>"
});
var $d_s_reflect_NoManifest$ = new $TypeData().initClass({
  s_reflect_NoManifest$: 0
}, false, "scala.reflect.NoManifest$", {
  s_reflect_NoManifest$: 1,
  O: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_reflect_NoManifest$.prototype.$classData = $d_s_reflect_NoManifest$;
var $n_s_reflect_NoManifest$ = (void 0);
function $m_s_reflect_NoManifest$() {
  if ((!$n_s_reflect_NoManifest$)) {
    $n_s_reflect_NoManifest$ = new $c_s_reflect_NoManifest$().init___()
  };
  return $n_s_reflect_NoManifest$
}
/** @constructor */
function $c_sc_AbstractIterator() {
  $c_O.call(this)
}
$c_sc_AbstractIterator.prototype = new $h_O();
$c_sc_AbstractIterator.prototype.constructor = $c_sc_AbstractIterator;
/** @constructor */
function $h_sc_AbstractIterator() {
  /*<skip>*/
}
$h_sc_AbstractIterator.prototype = $c_sc_AbstractIterator.prototype;
$c_sc_AbstractIterator.prototype.isEmpty__Z = (function() {
  return $f_sc_Iterator__isEmpty__Z(this)
});
$c_sc_AbstractIterator.prototype.toString__T = (function() {
  return $f_sc_Iterator__toString__T(this)
});
$c_sc_AbstractIterator.prototype.foreach__F1__V = (function(f) {
  $f_sc_Iterator__foreach__F1__V(this, f)
});
$c_sc_AbstractIterator.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return $f_sc_TraversableOnce__addString__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
/** @constructor */
function $c_scg_SetFactory() {
  $c_scg_GenSetFactory.call(this)
}
$c_scg_SetFactory.prototype = new $h_scg_GenSetFactory();
$c_scg_SetFactory.prototype.constructor = $c_scg_SetFactory;
/** @constructor */
function $h_scg_SetFactory() {
  /*<skip>*/
}
$h_scg_SetFactory.prototype = $c_scg_SetFactory.prototype;
/** @constructor */
function $c_sci_Map$() {
  $c_scg_ImmutableMapFactory.call(this)
}
$c_sci_Map$.prototype = new $h_scg_ImmutableMapFactory();
$c_sci_Map$.prototype.constructor = $c_sci_Map$;
/** @constructor */
function $h_sci_Map$() {
  /*<skip>*/
}
$h_sci_Map$.prototype = $c_sci_Map$.prototype;
$c_sci_Map$.prototype.init___ = (function() {
  return this
});
var $d_sci_Map$ = new $TypeData().initClass({
  sci_Map$: 0
}, false, "scala.collection.immutable.Map$", {
  sci_Map$: 1,
  scg_ImmutableMapFactory: 1,
  scg_MapFactory: 1,
  scg_GenMapFactory: 1,
  O: 1
});
$c_sci_Map$.prototype.$classData = $d_sci_Map$;
var $n_sci_Map$ = (void 0);
function $m_sci_Map$() {
  if ((!$n_sci_Map$)) {
    $n_sci_Map$ = new $c_sci_Map$().init___()
  };
  return $n_sci_Map$
}
/** @constructor */
function $c_sjsr_RuntimeLong() {
  $c_jl_Number.call(this);
  this.lo$2 = 0;
  this.hi$2 = 0
}
$c_sjsr_RuntimeLong.prototype = new $h_jl_Number();
$c_sjsr_RuntimeLong.prototype.constructor = $c_sjsr_RuntimeLong;
/** @constructor */
function $h_sjsr_RuntimeLong() {
  /*<skip>*/
}
$h_sjsr_RuntimeLong.prototype = $c_sjsr_RuntimeLong.prototype;
$c_sjsr_RuntimeLong.prototype.longValue__J = (function() {
  return $uJ(this)
});
$c_sjsr_RuntimeLong.prototype.$$bar__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  return new $c_sjsr_RuntimeLong().init___I__I((this.lo$2 | b.lo$2), (this.hi$2 | b.hi$2))
});
$c_sjsr_RuntimeLong.prototype.$$greater$eq__sjsr_RuntimeLong__Z = (function(b) {
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  return ((ahi === bhi) ? (((-2147483648) ^ this.lo$2) >= ((-2147483648) ^ b.lo$2)) : (ahi > bhi))
});
$c_sjsr_RuntimeLong.prototype.byteValue__B = (function() {
  return ((this.lo$2 << 24) >> 24)
});
$c_sjsr_RuntimeLong.prototype.equals__O__Z = (function(that) {
  if ($is_sjsr_RuntimeLong(that)) {
    var x2 = $as_sjsr_RuntimeLong(that);
    return ((this.lo$2 === x2.lo$2) && (this.hi$2 === x2.hi$2))
  } else {
    return false
  }
});
$c_sjsr_RuntimeLong.prototype.$$less__sjsr_RuntimeLong__Z = (function(b) {
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  return ((ahi === bhi) ? (((-2147483648) ^ this.lo$2) < ((-2147483648) ^ b.lo$2)) : (ahi < bhi))
});
$c_sjsr_RuntimeLong.prototype.$$times__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  var alo = this.lo$2;
  var blo = b.lo$2;
  var a0 = (65535 & alo);
  var a1 = ((alo >>> 16) | 0);
  var b0 = (65535 & blo);
  var b1 = ((blo >>> 16) | 0);
  var a0b0 = $imul(a0, b0);
  var a1b0 = $imul(a1, b0);
  var a0b1 = $imul(a0, b1);
  var lo = ((a0b0 + (((a1b0 + a0b1) | 0) << 16)) | 0);
  var c1part = ((((a0b0 >>> 16) | 0) + a0b1) | 0);
  var hi = (((((((($imul(alo, b.hi$2) + $imul(this.hi$2, blo)) | 0) + $imul(a1, b1)) | 0) + ((c1part >>> 16) | 0)) | 0) + (((((65535 & c1part) + a1b0) | 0) >>> 16) | 0)) | 0);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, hi)
});
$c_sjsr_RuntimeLong.prototype.init___I__I__I = (function(l, m, h) {
  $c_sjsr_RuntimeLong.prototype.init___I__I.call(this, (l | (m << 22)), ((m >> 10) | (h << 12)));
  return this
});
$c_sjsr_RuntimeLong.prototype.$$percent__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  var this$1 = $m_sjsr_RuntimeLong$();
  var lo = this$1.remainderImpl__I__I__I__I__I(this.lo$2, this.hi$2, b.lo$2, b.hi$2);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f)
});
$c_sjsr_RuntimeLong.prototype.toString__T = (function() {
  return $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toString__I__I__T(this.lo$2, this.hi$2)
});
$c_sjsr_RuntimeLong.prototype.init___I__I = (function(lo, hi) {
  this.lo$2 = lo;
  this.hi$2 = hi;
  return this
});
$c_sjsr_RuntimeLong.prototype.compareTo__O__I = (function(x$1) {
  var that = $as_sjsr_RuntimeLong(x$1);
  return $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$compare__I__I__I__I__I(this.lo$2, this.hi$2, that.lo$2, that.hi$2)
});
$c_sjsr_RuntimeLong.prototype.$$less$eq__sjsr_RuntimeLong__Z = (function(b) {
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  return ((ahi === bhi) ? (((-2147483648) ^ this.lo$2) <= ((-2147483648) ^ b.lo$2)) : (ahi < bhi))
});
$c_sjsr_RuntimeLong.prototype.$$amp__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  return new $c_sjsr_RuntimeLong().init___I__I((this.lo$2 & b.lo$2), (this.hi$2 & b.hi$2))
});
$c_sjsr_RuntimeLong.prototype.$$greater$greater$greater__I__sjsr_RuntimeLong = (function(n) {
  return new $c_sjsr_RuntimeLong().init___I__I((((32 & n) === 0) ? (((this.lo$2 >>> n) | 0) | ((this.hi$2 << 1) << ((31 - n) | 0))) : ((this.hi$2 >>> n) | 0)), (((32 & n) === 0) ? ((this.hi$2 >>> n) | 0) : 0))
});
$c_sjsr_RuntimeLong.prototype.$$greater__sjsr_RuntimeLong__Z = (function(b) {
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  return ((ahi === bhi) ? (((-2147483648) ^ this.lo$2) > ((-2147483648) ^ b.lo$2)) : (ahi > bhi))
});
$c_sjsr_RuntimeLong.prototype.$$less$less__I__sjsr_RuntimeLong = (function(n) {
  return new $c_sjsr_RuntimeLong().init___I__I((((32 & n) === 0) ? (this.lo$2 << n) : 0), (((32 & n) === 0) ? (((((this.lo$2 >>> 1) | 0) >>> ((31 - n) | 0)) | 0) | (this.hi$2 << n)) : (this.lo$2 << n)))
});
$c_sjsr_RuntimeLong.prototype.init___I = (function(value) {
  $c_sjsr_RuntimeLong.prototype.init___I__I.call(this, value, (value >> 31));
  return this
});
$c_sjsr_RuntimeLong.prototype.toInt__I = (function() {
  return this.lo$2
});
$c_sjsr_RuntimeLong.prototype.notEquals__sjsr_RuntimeLong__Z = (function(b) {
  return (!((this.lo$2 === b.lo$2) && (this.hi$2 === b.hi$2)))
});
$c_sjsr_RuntimeLong.prototype.unary$und$minus__sjsr_RuntimeLong = (function() {
  var lo = this.lo$2;
  var hi = this.hi$2;
  return new $c_sjsr_RuntimeLong().init___I__I(((-lo) | 0), ((lo !== 0) ? (~hi) : ((-hi) | 0)))
});
$c_sjsr_RuntimeLong.prototype.$$plus__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  var alo = this.lo$2;
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  var lo = ((alo + b.lo$2) | 0);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, ((((-2147483648) ^ lo) < ((-2147483648) ^ alo)) ? ((1 + ((ahi + bhi) | 0)) | 0) : ((ahi + bhi) | 0)))
});
$c_sjsr_RuntimeLong.prototype.shortValue__S = (function() {
  return ((this.lo$2 << 16) >> 16)
});
$c_sjsr_RuntimeLong.prototype.$$greater$greater__I__sjsr_RuntimeLong = (function(n) {
  return new $c_sjsr_RuntimeLong().init___I__I((((32 & n) === 0) ? (((this.lo$2 >>> n) | 0) | ((this.hi$2 << 1) << ((31 - n) | 0))) : (this.hi$2 >> n)), (((32 & n) === 0) ? (this.hi$2 >> n) : (this.hi$2 >> 31)))
});
$c_sjsr_RuntimeLong.prototype.toDouble__D = (function() {
  return $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(this.lo$2, this.hi$2)
});
$c_sjsr_RuntimeLong.prototype.$$div__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  var this$1 = $m_sjsr_RuntimeLong$();
  var lo = this$1.divideImpl__I__I__I__I__I(this.lo$2, this.hi$2, b.lo$2, b.hi$2);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f)
});
$c_sjsr_RuntimeLong.prototype.doubleValue__D = (function() {
  return $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(this.lo$2, this.hi$2)
});
$c_sjsr_RuntimeLong.prototype.hashCode__I = (function() {
  return (this.lo$2 ^ this.hi$2)
});
$c_sjsr_RuntimeLong.prototype.intValue__I = (function() {
  return this.lo$2
});
$c_sjsr_RuntimeLong.prototype.unary$und$tilde__sjsr_RuntimeLong = (function() {
  return new $c_sjsr_RuntimeLong().init___I__I((~this.lo$2), (~this.hi$2))
});
$c_sjsr_RuntimeLong.prototype.compareTo__jl_Long__I = (function(that) {
  return $m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$compare__I__I__I__I__I(this.lo$2, this.hi$2, that.lo$2, that.hi$2)
});
$c_sjsr_RuntimeLong.prototype.floatValue__F = (function() {
  return $fround($m_sjsr_RuntimeLong$().scala$scalajs$runtime$RuntimeLong$$toDouble__I__I__D(this.lo$2, this.hi$2))
});
$c_sjsr_RuntimeLong.prototype.$$minus__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  var alo = this.lo$2;
  var ahi = this.hi$2;
  var bhi = b.hi$2;
  var lo = ((alo - b.lo$2) | 0);
  return new $c_sjsr_RuntimeLong().init___I__I(lo, ((((-2147483648) ^ lo) > ((-2147483648) ^ alo)) ? (((-1) + ((ahi - bhi) | 0)) | 0) : ((ahi - bhi) | 0)))
});
$c_sjsr_RuntimeLong.prototype.$$up__sjsr_RuntimeLong__sjsr_RuntimeLong = (function(b) {
  return new $c_sjsr_RuntimeLong().init___I__I((this.lo$2 ^ b.lo$2), (this.hi$2 ^ b.hi$2))
});
$c_sjsr_RuntimeLong.prototype.equals__sjsr_RuntimeLong__Z = (function(b) {
  return ((this.lo$2 === b.lo$2) && (this.hi$2 === b.hi$2))
});
function $is_sjsr_RuntimeLong(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sjsr_RuntimeLong)))
}
function $as_sjsr_RuntimeLong(obj) {
  return (($is_sjsr_RuntimeLong(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.scalajs.runtime.RuntimeLong"))
}
function $isArrayOf_sjsr_RuntimeLong(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjsr_RuntimeLong)))
}
function $asArrayOf_sjsr_RuntimeLong(obj, depth) {
  return (($isArrayOf_sjsr_RuntimeLong(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.scalajs.runtime.RuntimeLong;", depth))
}
var $d_sjsr_RuntimeLong = new $TypeData().initClass({
  sjsr_RuntimeLong: 0
}, false, "scala.scalajs.runtime.RuntimeLong", {
  sjsr_RuntimeLong: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  jl_Comparable: 1
});
$c_sjsr_RuntimeLong.prototype.$classData = $d_sjsr_RuntimeLong;
/** @constructor */
function $c_jl_ArithmeticException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_ArithmeticException.prototype = new $h_jl_RuntimeException();
$c_jl_ArithmeticException.prototype.constructor = $c_jl_ArithmeticException;
/** @constructor */
function $h_jl_ArithmeticException() {
  /*<skip>*/
}
$h_jl_ArithmeticException.prototype = $c_jl_ArithmeticException.prototype;
$c_jl_ArithmeticException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
function $is_jl_ArithmeticException(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.jl_ArithmeticException)))
}
function $as_jl_ArithmeticException(obj) {
  return (($is_jl_ArithmeticException(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.lang.ArithmeticException"))
}
function $isArrayOf_jl_ArithmeticException(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.jl_ArithmeticException)))
}
function $asArrayOf_jl_ArithmeticException(obj, depth) {
  return (($isArrayOf_jl_ArithmeticException(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.lang.ArithmeticException;", depth))
}
var $d_jl_ArithmeticException = new $TypeData().initClass({
  jl_ArithmeticException: 0
}, false, "java.lang.ArithmeticException", {
  jl_ArithmeticException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_ArithmeticException.prototype.$classData = $d_jl_ArithmeticException;
/** @constructor */
function $c_jl_ClassCastException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_ClassCastException.prototype = new $h_jl_RuntimeException();
$c_jl_ClassCastException.prototype.constructor = $c_jl_ClassCastException;
/** @constructor */
function $h_jl_ClassCastException() {
  /*<skip>*/
}
$h_jl_ClassCastException.prototype = $c_jl_ClassCastException.prototype;
$c_jl_ClassCastException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_ClassCastException = new $TypeData().initClass({
  jl_ClassCastException: 0
}, false, "java.lang.ClassCastException", {
  jl_ClassCastException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_ClassCastException.prototype.$classData = $d_jl_ClassCastException;
/** @constructor */
function $c_jl_IllegalArgumentException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_IllegalArgumentException.prototype = new $h_jl_RuntimeException();
$c_jl_IllegalArgumentException.prototype.constructor = $c_jl_IllegalArgumentException;
/** @constructor */
function $h_jl_IllegalArgumentException() {
  /*<skip>*/
}
$h_jl_IllegalArgumentException.prototype = $c_jl_IllegalArgumentException.prototype;
$c_jl_IllegalArgumentException.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
$c_jl_IllegalArgumentException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_IllegalArgumentException = new $TypeData().initClass({
  jl_IllegalArgumentException: 0
}, false, "java.lang.IllegalArgumentException", {
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_IllegalArgumentException.prototype.$classData = $d_jl_IllegalArgumentException;
/** @constructor */
function $c_jl_IndexOutOfBoundsException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_IndexOutOfBoundsException.prototype = new $h_jl_RuntimeException();
$c_jl_IndexOutOfBoundsException.prototype.constructor = $c_jl_IndexOutOfBoundsException;
/** @constructor */
function $h_jl_IndexOutOfBoundsException() {
  /*<skip>*/
}
$h_jl_IndexOutOfBoundsException.prototype = $c_jl_IndexOutOfBoundsException.prototype;
$c_jl_IndexOutOfBoundsException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_IndexOutOfBoundsException = new $TypeData().initClass({
  jl_IndexOutOfBoundsException: 0
}, false, "java.lang.IndexOutOfBoundsException", {
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_IndexOutOfBoundsException.prototype.$classData = $d_jl_IndexOutOfBoundsException;
/** @constructor */
function $c_jl_NullPointerException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_NullPointerException.prototype = new $h_jl_RuntimeException();
$c_jl_NullPointerException.prototype.constructor = $c_jl_NullPointerException;
/** @constructor */
function $h_jl_NullPointerException() {
  /*<skip>*/
}
$h_jl_NullPointerException.prototype = $c_jl_NullPointerException.prototype;
$c_jl_NullPointerException.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
$c_jl_NullPointerException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_NullPointerException = new $TypeData().initClass({
  jl_NullPointerException: 0
}, false, "java.lang.NullPointerException", {
  jl_NullPointerException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_NullPointerException.prototype.$classData = $d_jl_NullPointerException;
/** @constructor */
function $c_jl_UnsupportedOperationException() {
  $c_jl_RuntimeException.call(this)
}
$c_jl_UnsupportedOperationException.prototype = new $h_jl_RuntimeException();
$c_jl_UnsupportedOperationException.prototype.constructor = $c_jl_UnsupportedOperationException;
/** @constructor */
function $h_jl_UnsupportedOperationException() {
  /*<skip>*/
}
$h_jl_UnsupportedOperationException.prototype = $c_jl_UnsupportedOperationException.prototype;
$c_jl_UnsupportedOperationException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_UnsupportedOperationException = new $TypeData().initClass({
  jl_UnsupportedOperationException: 0
}, false, "java.lang.UnsupportedOperationException", {
  jl_UnsupportedOperationException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_UnsupportedOperationException.prototype.$classData = $d_jl_UnsupportedOperationException;
/** @constructor */
function $c_ju_NoSuchElementException() {
  $c_jl_RuntimeException.call(this)
}
$c_ju_NoSuchElementException.prototype = new $h_jl_RuntimeException();
$c_ju_NoSuchElementException.prototype.constructor = $c_ju_NoSuchElementException;
/** @constructor */
function $h_ju_NoSuchElementException() {
  /*<skip>*/
}
$h_ju_NoSuchElementException.prototype = $c_ju_NoSuchElementException.prototype;
$c_ju_NoSuchElementException.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
$c_ju_NoSuchElementException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_ju_NoSuchElementException = new $TypeData().initClass({
  ju_NoSuchElementException: 0
}, false, "java.util.NoSuchElementException", {
  ju_NoSuchElementException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_ju_NoSuchElementException.prototype.$classData = $d_ju_NoSuchElementException;
/** @constructor */
function $c_s_MatchError() {
  $c_jl_RuntimeException.call(this);
  this.objString$4 = null;
  this.obj$4 = null;
  this.bitmap$0$4 = false
}
$c_s_MatchError.prototype = new $h_jl_RuntimeException();
$c_s_MatchError.prototype.constructor = $c_s_MatchError;
/** @constructor */
function $h_s_MatchError() {
  /*<skip>*/
}
$h_s_MatchError.prototype = $c_s_MatchError.prototype;
$c_s_MatchError.prototype.objString$lzycompute__p4__T = (function() {
  if ((!this.bitmap$0$4)) {
    this.objString$4 = ((this.obj$4 === null) ? "null" : this.liftedTree1$1__p4__T());
    this.bitmap$0$4 = true
  };
  return this.objString$4
});
$c_s_MatchError.prototype.ofClass$1__p4__T = (function() {
  var this$1 = this.obj$4;
  return ("of class " + $objectGetClass(this$1).getName__T())
});
$c_s_MatchError.prototype.liftedTree1$1__p4__T = (function() {
  try {
    return ((($objectToString(this.obj$4) + " (") + this.ofClass$1__p4__T()) + ")")
  } catch (e) {
    var e$2 = $m_sjsr_package$().wrapJavaScriptException__O__jl_Throwable(e);
    if ((e$2 !== null)) {
      return ("an instance " + this.ofClass$1__p4__T())
    } else {
      throw e
    }
  }
});
$c_s_MatchError.prototype.getMessage__T = (function() {
  return this.objString__p4__T()
});
$c_s_MatchError.prototype.objString__p4__T = (function() {
  return ((!this.bitmap$0$4) ? this.objString$lzycompute__p4__T() : this.objString$4)
});
$c_s_MatchError.prototype.init___O = (function(obj) {
  this.obj$4 = obj;
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
var $d_s_MatchError = new $TypeData().initClass({
  s_MatchError: 0
}, false, "scala.MatchError", {
  s_MatchError: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_s_MatchError.prototype.$classData = $d_s_MatchError;
/** @constructor */
function $c_s_Option() {
  $c_O.call(this)
}
$c_s_Option.prototype = new $h_O();
$c_s_Option.prototype.constructor = $c_s_Option;
/** @constructor */
function $h_s_Option() {
  /*<skip>*/
}
$h_s_Option.prototype = $c_s_Option.prototype;
$c_s_Option.prototype.isDefined__Z = (function() {
  return (!this.isEmpty__Z())
});
function $is_s_Option(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_Option)))
}
function $as_s_Option(obj) {
  return (($is_s_Option(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Option"))
}
function $isArrayOf_s_Option(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_Option)))
}
function $asArrayOf_s_Option(obj, depth) {
  return (($isArrayOf_s_Option(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Option;", depth))
}
/** @constructor */
function $c_s_Predef$$anon$1() {
  $c_s_Predef$$less$colon$less.call(this)
}
$c_s_Predef$$anon$1.prototype = new $h_s_Predef$$less$colon$less();
$c_s_Predef$$anon$1.prototype.constructor = $c_s_Predef$$anon$1;
/** @constructor */
function $h_s_Predef$$anon$1() {
  /*<skip>*/
}
$h_s_Predef$$anon$1.prototype = $c_s_Predef$$anon$1.prototype;
$c_s_Predef$$anon$1.prototype.init___ = (function() {
  return this
});
$c_s_Predef$$anon$1.prototype.apply__O__O = (function(x) {
  return x
});
var $d_s_Predef$$anon$1 = new $TypeData().initClass({
  s_Predef$$anon$1: 0
}, false, "scala.Predef$$anon$1", {
  s_Predef$$anon$1: 1,
  s_Predef$$less$colon$less: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_Predef$$anon$1.prototype.$classData = $d_s_Predef$$anon$1;
/** @constructor */
function $c_s_Predef$$anon$2() {
  $c_s_Predef$$eq$colon$eq.call(this)
}
$c_s_Predef$$anon$2.prototype = new $h_s_Predef$$eq$colon$eq();
$c_s_Predef$$anon$2.prototype.constructor = $c_s_Predef$$anon$2;
/** @constructor */
function $h_s_Predef$$anon$2() {
  /*<skip>*/
}
$h_s_Predef$$anon$2.prototype = $c_s_Predef$$anon$2.prototype;
$c_s_Predef$$anon$2.prototype.init___ = (function() {
  return this
});
$c_s_Predef$$anon$2.prototype.apply__O__O = (function(x) {
  return x
});
var $d_s_Predef$$anon$2 = new $TypeData().initClass({
  s_Predef$$anon$2: 0
}, false, "scala.Predef$$anon$2", {
  s_Predef$$anon$2: 1,
  s_Predef$$eq$colon$eq: 1,
  O: 1,
  F1: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_Predef$$anon$2.prototype.$classData = $d_s_Predef$$anon$2;
/** @constructor */
function $c_s_StringContext() {
  $c_O.call(this);
  this.parts$1 = null
}
$c_s_StringContext.prototype = new $h_O();
$c_s_StringContext.prototype.constructor = $c_s_StringContext;
/** @constructor */
function $h_s_StringContext() {
  /*<skip>*/
}
$h_s_StringContext.prototype = $c_s_StringContext.prototype;
$c_s_StringContext.prototype.productPrefix__T = (function() {
  return "StringContext"
});
$c_s_StringContext.prototype.productArity__I = (function() {
  return 1
});
$c_s_StringContext.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if ($is_s_StringContext(x$1)) {
    var StringContext$1 = $as_s_StringContext(x$1);
    var x = this.parts$1;
    var x$2 = StringContext$1.parts$1;
    return ((x === null) ? (x$2 === null) : x.equals__O__Z(x$2))
  } else {
    return false
  }
});
$c_s_StringContext.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0: {
      return this.parts$1;
      break
    }
    default: {
      throw new $c_jl_IndexOutOfBoundsException().init___T(("" + x$1))
    }
  }
});
$c_s_StringContext.prototype.toString__T = (function() {
  return $m_sr_ScalaRunTime$().$$undtoString__s_Product__T(this)
});
$c_s_StringContext.prototype.checkLengths__sc_Seq__V = (function(args) {
  if ((this.parts$1.length__I() !== ((1 + args.length__I()) | 0))) {
    throw new $c_jl_IllegalArgumentException().init___T((((("wrong number of arguments (" + args.length__I()) + ") for interpolated string with ") + this.parts$1.length__I()) + " parts"))
  }
});
$c_s_StringContext.prototype.s__sc_Seq__T = (function(args) {
  var f = (function($this) {
    return (function(str$2) {
      var str = $as_T(str$2);
      var this$1 = $m_s_StringContext$();
      return this$1.treatEscapes0__p1__T__Z__T(str, false)
    })
  })(this);
  this.checkLengths__sc_Seq__V(args);
  var pi = this.parts$1.iterator__sc_Iterator();
  var ai = args.iterator__sc_Iterator();
  var arg1 = pi.next__O();
  var bldr = new $c_jl_StringBuilder().init___T($as_T(f(arg1)));
  while (ai.hasNext__Z()) {
    bldr.append__O__jl_StringBuilder(ai.next__O());
    var arg1$1 = pi.next__O();
    bldr.append__T__jl_StringBuilder($as_T(f(arg1$1)))
  };
  return bldr.content$1
});
$c_s_StringContext.prototype.init___sc_Seq = (function(parts) {
  this.parts$1 = parts;
  return this
});
$c_s_StringContext.prototype.hashCode__I = (function() {
  var this$2 = $m_s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
$c_s_StringContext.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
function $is_s_StringContext(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_StringContext)))
}
function $as_s_StringContext(obj) {
  return (($is_s_StringContext(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.StringContext"))
}
function $isArrayOf_s_StringContext(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_StringContext)))
}
function $asArrayOf_s_StringContext(obj, depth) {
  return (($isArrayOf_s_StringContext(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.StringContext;", depth))
}
var $d_s_StringContext = new $TypeData().initClass({
  s_StringContext: 0
}, false, "scala.StringContext", {
  s_StringContext: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_StringContext.prototype.$classData = $d_s_StringContext;
/** @constructor */
function $c_s_util_control_BreakControl() {
  $c_jl_Throwable.call(this)
}
$c_s_util_control_BreakControl.prototype = new $h_jl_Throwable();
$c_s_util_control_BreakControl.prototype.constructor = $c_s_util_control_BreakControl;
/** @constructor */
function $h_s_util_control_BreakControl() {
  /*<skip>*/
}
$h_s_util_control_BreakControl.prototype = $c_s_util_control_BreakControl.prototype;
$c_s_util_control_BreakControl.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
$c_s_util_control_BreakControl.prototype.fillInStackTrace__jl_Throwable = (function() {
  return $f_s_util_control_NoStackTrace__fillInStackTrace__jl_Throwable(this)
});
var $d_s_util_control_BreakControl = new $TypeData().initClass({
  s_util_control_BreakControl: 0
}, false, "scala.util.control.BreakControl", {
  s_util_control_BreakControl: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_util_control_ControlThrowable: 1,
  s_util_control_NoStackTrace: 1
});
$c_s_util_control_BreakControl.prototype.$classData = $d_s_util_control_BreakControl;
function $f_sc_GenSeqLike__equals__O__Z($thiz, that) {
  if ($is_sc_GenSeq(that)) {
    var x2 = $as_sc_GenSeq(that);
    return $thiz.sameElements__sc_GenIterable__Z(x2)
  } else {
    return false
  }
}
/** @constructor */
function $c_sc_Iterable$() {
  $c_scg_GenTraversableFactory.call(this)
}
$c_sc_Iterable$.prototype = new $h_scg_GenTraversableFactory();
$c_sc_Iterable$.prototype.constructor = $c_sc_Iterable$;
/** @constructor */
function $h_sc_Iterable$() {
  /*<skip>*/
}
$h_sc_Iterable$.prototype = $c_sc_Iterable$.prototype;
$c_sc_Iterable$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  return this
});
var $d_sc_Iterable$ = new $TypeData().initClass({
  sc_Iterable$: 0
}, false, "scala.collection.Iterable$", {
  sc_Iterable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
$c_sc_Iterable$.prototype.$classData = $d_sc_Iterable$;
var $n_sc_Iterable$ = (void 0);
function $m_sc_Iterable$() {
  if ((!$n_sc_Iterable$)) {
    $n_sc_Iterable$ = new $c_sc_Iterable$().init___()
  };
  return $n_sc_Iterable$
}
/** @constructor */
function $c_sc_Iterator$$anon$2() {
  $c_sc_AbstractIterator.call(this)
}
$c_sc_Iterator$$anon$2.prototype = new $h_sc_AbstractIterator();
$c_sc_Iterator$$anon$2.prototype.constructor = $c_sc_Iterator$$anon$2;
/** @constructor */
function $h_sc_Iterator$$anon$2() {
  /*<skip>*/
}
$h_sc_Iterator$$anon$2.prototype = $c_sc_Iterator$$anon$2.prototype;
$c_sc_Iterator$$anon$2.prototype.init___ = (function() {
  return this
});
$c_sc_Iterator$$anon$2.prototype.next__O = (function() {
  this.next__sr_Nothing$()
});
$c_sc_Iterator$$anon$2.prototype.next__sr_Nothing$ = (function() {
  throw new $c_ju_NoSuchElementException().init___T("next on empty iterator")
});
$c_sc_Iterator$$anon$2.prototype.hasNext__Z = (function() {
  return false
});
var $d_sc_Iterator$$anon$2 = new $TypeData().initClass({
  sc_Iterator$$anon$2: 0
}, false, "scala.collection.Iterator$$anon$2", {
  sc_Iterator$$anon$2: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
$c_sc_Iterator$$anon$2.prototype.$classData = $d_sc_Iterator$$anon$2;
/** @constructor */
function $c_sc_LinearSeqLike$$anon$1() {
  $c_sc_AbstractIterator.call(this);
  this.these$2 = null
}
$c_sc_LinearSeqLike$$anon$1.prototype = new $h_sc_AbstractIterator();
$c_sc_LinearSeqLike$$anon$1.prototype.constructor = $c_sc_LinearSeqLike$$anon$1;
/** @constructor */
function $h_sc_LinearSeqLike$$anon$1() {
  /*<skip>*/
}
$h_sc_LinearSeqLike$$anon$1.prototype = $c_sc_LinearSeqLike$$anon$1.prototype;
$c_sc_LinearSeqLike$$anon$1.prototype.init___sc_LinearSeqLike = (function($$outer) {
  this.these$2 = $$outer;
  return this
});
$c_sc_LinearSeqLike$$anon$1.prototype.next__O = (function() {
  if (this.hasNext__Z()) {
    var result = this.these$2.head__O();
    this.these$2 = $as_sc_LinearSeqLike(this.these$2.tail__O());
    return result
  } else {
    return $m_sc_Iterator$().empty$1.next__O()
  }
});
$c_sc_LinearSeqLike$$anon$1.prototype.hasNext__Z = (function() {
  return (!this.these$2.isEmpty__Z())
});
var $d_sc_LinearSeqLike$$anon$1 = new $TypeData().initClass({
  sc_LinearSeqLike$$anon$1: 0
}, false, "scala.collection.LinearSeqLike$$anon$1", {
  sc_LinearSeqLike$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
$c_sc_LinearSeqLike$$anon$1.prototype.$classData = $d_sc_LinearSeqLike$$anon$1;
/** @constructor */
function $c_sc_Traversable$() {
  $c_scg_GenTraversableFactory.call(this);
  this.breaks$3 = null
}
$c_sc_Traversable$.prototype = new $h_scg_GenTraversableFactory();
$c_sc_Traversable$.prototype.constructor = $c_sc_Traversable$;
/** @constructor */
function $h_sc_Traversable$() {
  /*<skip>*/
}
$h_sc_Traversable$.prototype = $c_sc_Traversable$.prototype;
$c_sc_Traversable$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  $n_sc_Traversable$ = this;
  this.breaks$3 = new $c_s_util_control_Breaks().init___();
  return this
});
var $d_sc_Traversable$ = new $TypeData().initClass({
  sc_Traversable$: 0
}, false, "scala.collection.Traversable$", {
  sc_Traversable$: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
$c_sc_Traversable$.prototype.$classData = $d_sc_Traversable$;
var $n_sc_Traversable$ = (void 0);
function $m_sc_Traversable$() {
  if ((!$n_sc_Traversable$)) {
    $n_sc_Traversable$ = new $c_sc_Traversable$().init___()
  };
  return $n_sc_Traversable$
}
/** @constructor */
function $c_scg_ImmutableSetFactory() {
  $c_scg_SetFactory.call(this)
}
$c_scg_ImmutableSetFactory.prototype = new $h_scg_SetFactory();
$c_scg_ImmutableSetFactory.prototype.constructor = $c_scg_ImmutableSetFactory;
/** @constructor */
function $h_scg_ImmutableSetFactory() {
  /*<skip>*/
}
$h_scg_ImmutableSetFactory.prototype = $c_scg_ImmutableSetFactory.prototype;
/** @constructor */
function $c_sr_ScalaRunTime$$anon$1() {
  $c_sc_AbstractIterator.call(this);
  this.c$2 = 0;
  this.cmax$2 = 0;
  this.x$2$2 = null
}
$c_sr_ScalaRunTime$$anon$1.prototype = new $h_sc_AbstractIterator();
$c_sr_ScalaRunTime$$anon$1.prototype.constructor = $c_sr_ScalaRunTime$$anon$1;
/** @constructor */
function $h_sr_ScalaRunTime$$anon$1() {
  /*<skip>*/
}
$h_sr_ScalaRunTime$$anon$1.prototype = $c_sr_ScalaRunTime$$anon$1.prototype;
$c_sr_ScalaRunTime$$anon$1.prototype.next__O = (function() {
  var result = this.x$2$2.productElement__I__O(this.c$2);
  this.c$2 = ((1 + this.c$2) | 0);
  return result
});
$c_sr_ScalaRunTime$$anon$1.prototype.init___s_Product = (function(x$2) {
  this.x$2$2 = x$2;
  this.c$2 = 0;
  this.cmax$2 = x$2.productArity__I();
  return this
});
$c_sr_ScalaRunTime$$anon$1.prototype.hasNext__Z = (function() {
  return (this.c$2 < this.cmax$2)
});
var $d_sr_ScalaRunTime$$anon$1 = new $TypeData().initClass({
  sr_ScalaRunTime$$anon$1: 0
}, false, "scala.runtime.ScalaRunTime$$anon$1", {
  sr_ScalaRunTime$$anon$1: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1
});
$c_sr_ScalaRunTime$$anon$1.prototype.$classData = $d_sr_ScalaRunTime$$anon$1;
/** @constructor */
function $c_Ljava_io_PrintStream() {
  $c_Ljava_io_FilterOutputStream.call(this);
  this.encoder$3 = null;
  this.autoFlush$3 = false;
  this.charset$3 = null;
  this.closing$3 = false;
  this.java$io$PrintStream$$closed$3 = false;
  this.errorFlag$3 = false;
  this.bitmap$0$3 = false
}
$c_Ljava_io_PrintStream.prototype = new $h_Ljava_io_FilterOutputStream();
$c_Ljava_io_PrintStream.prototype.constructor = $c_Ljava_io_PrintStream;
/** @constructor */
function $h_Ljava_io_PrintStream() {
  /*<skip>*/
}
$h_Ljava_io_PrintStream.prototype = $c_Ljava_io_PrintStream.prototype;
$c_Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream__Z__Ljava_nio_charset_Charset = (function(_out, autoFlush, charset) {
  this.autoFlush$3 = autoFlush;
  this.charset$3 = charset;
  $c_Ljava_io_FilterOutputStream.prototype.init___Ljava_io_OutputStream.call(this, _out);
  this.closing$3 = false;
  this.java$io$PrintStream$$closed$3 = false;
  this.errorFlag$3 = false;
  return this
});
function $is_Ljava_io_PrintStream(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.Ljava_io_PrintStream)))
}
function $as_Ljava_io_PrintStream(obj) {
  return (($is_Ljava_io_PrintStream(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "java.io.PrintStream"))
}
function $isArrayOf_Ljava_io_PrintStream(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.Ljava_io_PrintStream)))
}
function $asArrayOf_Ljava_io_PrintStream(obj, depth) {
  return (($isArrayOf_Ljava_io_PrintStream(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Ljava.io.PrintStream;", depth))
}
/** @constructor */
function $c_T2() {
  $c_O.call(this);
  this.$$und1$f = null;
  this.$$und2$f = null
}
$c_T2.prototype = new $h_O();
$c_T2.prototype.constructor = $c_T2;
/** @constructor */
function $h_T2() {
  /*<skip>*/
}
$h_T2.prototype = $c_T2.prototype;
$c_T2.prototype.productPrefix__T = (function() {
  return "Tuple2"
});
$c_T2.prototype.productArity__I = (function() {
  return 2
});
$c_T2.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if ($is_T2(x$1)) {
    var Tuple2$1 = $as_T2(x$1);
    return ($m_sr_BoxesRunTime$().equals__O__O__Z(this.$$und1__O(), Tuple2$1.$$und1__O()) && $m_sr_BoxesRunTime$().equals__O__O__Z(this.$$und2__O(), Tuple2$1.$$und2__O()))
  } else {
    return false
  }
});
$c_T2.prototype.productElement__I__O = (function(n) {
  return $f_s_Product2__productElement__I__O(this, n)
});
$c_T2.prototype.init___O__O = (function(_1, _2) {
  this.$$und1$f = _1;
  this.$$und2$f = _2;
  return this
});
$c_T2.prototype.toString__T = (function() {
  return (((("(" + this.$$und1__O()) + ",") + this.$$und2__O()) + ")")
});
$c_T2.prototype.$$und2__O = (function() {
  return this.$$und2$f
});
$c_T2.prototype.hashCode__I = (function() {
  var this$2 = $m_s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
$c_T2.prototype.$$und1__O = (function() {
  return this.$$und1$f
});
$c_T2.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
function $is_T2(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.T2)))
}
function $as_T2(obj) {
  return (($is_T2(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Tuple2"))
}
function $isArrayOf_T2(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T2)))
}
function $asArrayOf_T2(obj, depth) {
  return (($isArrayOf_T2(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Tuple2;", depth))
}
var $d_T2 = new $TypeData().initClass({
  T2: 0
}, false, "scala.Tuple2", {
  T2: 1,
  O: 1,
  s_Product2: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_T2.prototype.$classData = $d_T2;
/** @constructor */
function $c_jl_ArrayIndexOutOfBoundsException() {
  $c_jl_IndexOutOfBoundsException.call(this)
}
$c_jl_ArrayIndexOutOfBoundsException.prototype = new $h_jl_IndexOutOfBoundsException();
$c_jl_ArrayIndexOutOfBoundsException.prototype.constructor = $c_jl_ArrayIndexOutOfBoundsException;
/** @constructor */
function $h_jl_ArrayIndexOutOfBoundsException() {
  /*<skip>*/
}
$h_jl_ArrayIndexOutOfBoundsException.prototype = $c_jl_ArrayIndexOutOfBoundsException.prototype;
$c_jl_ArrayIndexOutOfBoundsException.prototype.init___I = (function(index) {
  var s = ("Array index out of range: " + index);
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
$c_jl_ArrayIndexOutOfBoundsException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_ArrayIndexOutOfBoundsException = new $TypeData().initClass({
  jl_ArrayIndexOutOfBoundsException: 0
}, false, "java.lang.ArrayIndexOutOfBoundsException", {
  jl_ArrayIndexOutOfBoundsException: 1,
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_ArrayIndexOutOfBoundsException.prototype.$classData = $d_jl_ArrayIndexOutOfBoundsException;
/** @constructor */
function $c_jl_NumberFormatException() {
  $c_jl_IllegalArgumentException.call(this)
}
$c_jl_NumberFormatException.prototype = new $h_jl_IllegalArgumentException();
$c_jl_NumberFormatException.prototype.constructor = $c_jl_NumberFormatException;
/** @constructor */
function $h_jl_NumberFormatException() {
  /*<skip>*/
}
$h_jl_NumberFormatException.prototype = $c_jl_NumberFormatException.prototype;
$c_jl_NumberFormatException.prototype.init___T = (function(s) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_jl_NumberFormatException = new $TypeData().initClass({
  jl_NumberFormatException: 0
}, false, "java.lang.NumberFormatException", {
  jl_NumberFormatException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_NumberFormatException.prototype.$classData = $d_jl_NumberFormatException;
/** @constructor */
function $c_jl_StringIndexOutOfBoundsException() {
  $c_jl_IndexOutOfBoundsException.call(this)
}
$c_jl_StringIndexOutOfBoundsException.prototype = new $h_jl_IndexOutOfBoundsException();
$c_jl_StringIndexOutOfBoundsException.prototype.constructor = $c_jl_StringIndexOutOfBoundsException;
/** @constructor */
function $h_jl_StringIndexOutOfBoundsException() {
  /*<skip>*/
}
$h_jl_StringIndexOutOfBoundsException.prototype = $c_jl_StringIndexOutOfBoundsException.prototype;
$c_jl_StringIndexOutOfBoundsException.prototype.init___ = (function() {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
var $d_jl_StringIndexOutOfBoundsException = new $TypeData().initClass({
  jl_StringIndexOutOfBoundsException: 0
}, false, "java.lang.StringIndexOutOfBoundsException", {
  jl_StringIndexOutOfBoundsException: 1,
  jl_IndexOutOfBoundsException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_jl_StringIndexOutOfBoundsException.prototype.$classData = $d_jl_StringIndexOutOfBoundsException;
/** @constructor */
function $c_s_None$() {
  $c_s_Option.call(this)
}
$c_s_None$.prototype = new $h_s_Option();
$c_s_None$.prototype.constructor = $c_s_None$;
/** @constructor */
function $h_s_None$() {
  /*<skip>*/
}
$h_s_None$.prototype = $c_s_None$.prototype;
$c_s_None$.prototype.init___ = (function() {
  return this
});
$c_s_None$.prototype.productPrefix__T = (function() {
  return "None"
});
$c_s_None$.prototype.productArity__I = (function() {
  return 0
});
$c_s_None$.prototype.isEmpty__Z = (function() {
  return true
});
$c_s_None$.prototype.get__O = (function() {
  this.get__sr_Nothing$()
});
$c_s_None$.prototype.productElement__I__O = (function(x$1) {
  throw new $c_jl_IndexOutOfBoundsException().init___T(("" + x$1))
});
$c_s_None$.prototype.toString__T = (function() {
  return "None"
});
$c_s_None$.prototype.get__sr_Nothing$ = (function() {
  throw new $c_ju_NoSuchElementException().init___T("None.get")
});
$c_s_None$.prototype.hashCode__I = (function() {
  return 2433880
});
$c_s_None$.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
var $d_s_None$ = new $TypeData().initClass({
  s_None$: 0
}, false, "scala.None$", {
  s_None$: 1,
  s_Option: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_None$.prototype.$classData = $d_s_None$;
var $n_s_None$ = (void 0);
function $m_s_None$() {
  if ((!$n_s_None$)) {
    $n_s_None$ = new $c_s_None$().init___()
  };
  return $n_s_None$
}
/** @constructor */
function $c_s_Some() {
  $c_s_Option.call(this);
  this.value$2 = null
}
$c_s_Some.prototype = new $h_s_Option();
$c_s_Some.prototype.constructor = $c_s_Some;
/** @constructor */
function $h_s_Some() {
  /*<skip>*/
}
$h_s_Some.prototype = $c_s_Some.prototype;
$c_s_Some.prototype.productPrefix__T = (function() {
  return "Some"
});
$c_s_Some.prototype.productArity__I = (function() {
  return 1
});
$c_s_Some.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if ($is_s_Some(x$1)) {
    var Some$1 = $as_s_Some(x$1);
    return $m_sr_BoxesRunTime$().equals__O__O__Z(this.value$2, Some$1.value$2)
  } else {
    return false
  }
});
$c_s_Some.prototype.isEmpty__Z = (function() {
  return false
});
$c_s_Some.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0: {
      return this.value$2;
      break
    }
    default: {
      throw new $c_jl_IndexOutOfBoundsException().init___T(("" + x$1))
    }
  }
});
$c_s_Some.prototype.get__O = (function() {
  return this.value$2
});
$c_s_Some.prototype.toString__T = (function() {
  return $m_sr_ScalaRunTime$().$$undtoString__s_Product__T(this)
});
$c_s_Some.prototype.init___O = (function(value) {
  this.value$2 = value;
  return this
});
$c_s_Some.prototype.hashCode__I = (function() {
  var this$2 = $m_s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
$c_s_Some.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
function $is_s_Some(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_Some)))
}
function $as_s_Some(obj) {
  return (($is_s_Some(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.Some"))
}
function $isArrayOf_s_Some(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_Some)))
}
function $asArrayOf_s_Some(obj, depth) {
  return (($isArrayOf_s_Some(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.Some;", depth))
}
var $d_s_Some = new $TypeData().initClass({
  s_Some: 0
}, false, "scala.Some", {
  s_Some: 1,
  s_Option: 1,
  O: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_s_Some.prototype.$classData = $d_s_Some;
/** @constructor */
function $c_s_StringContext$InvalidEscapeException() {
  $c_jl_IllegalArgumentException.call(this);
  this.index$5 = 0
}
$c_s_StringContext$InvalidEscapeException.prototype = new $h_jl_IllegalArgumentException();
$c_s_StringContext$InvalidEscapeException.prototype.constructor = $c_s_StringContext$InvalidEscapeException;
/** @constructor */
function $h_s_StringContext$InvalidEscapeException() {
  /*<skip>*/
}
$h_s_StringContext$InvalidEscapeException.prototype = $c_s_StringContext$InvalidEscapeException.prototype;
$c_s_StringContext$InvalidEscapeException.prototype.init___T__I = (function(str, index) {
  this.index$5 = index;
  var jsx$3 = new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["invalid escape ", " index ", " in "\", "\". Use  for literal ."]));
  $m_s_Predef$().require__Z__V(((index >= 0) && (index < $uI(str.length))));
  if ((index === (((-1) + $uI(str.length)) | 0))) {
    var jsx$1 = "at terminal"
  } else {
    var jsx$2 = new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["'", "' not one of ", " at"]));
    var index$1 = ((1 + index) | 0);
    var c = (65535 & $uI(str.charCodeAt(index$1)));
    var jsx$1 = jsx$2.s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([new $c_jl_Character().init___C(c), "[b, t, n, f, r, , \", ']"]))
  };
  var s = jsx$3.s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([jsx$1, index, str]));
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, s, null);
  return this
});
var $d_s_StringContext$InvalidEscapeException = new $TypeData().initClass({
  s_StringContext$InvalidEscapeException: 0
}, false, "scala.StringContext$InvalidEscapeException", {
  s_StringContext$InvalidEscapeException: 1,
  jl_IllegalArgumentException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1
});
$c_s_StringContext$InvalidEscapeException.prototype.$classData = $d_s_StringContext$InvalidEscapeException;
function $f_sc_TraversableLike__isPartLikelySynthetic$1__psc_TraversableLike__T__I__Z($thiz, fqn$1, partStart$1) {
  var firstChar = (65535 & $uI(fqn$1.charCodeAt(partStart$1)));
  return (((firstChar > 90) && (firstChar < 127)) || (firstChar < 65))
}
function $f_sc_TraversableLike__toString__T($thiz) {
  return $thiz.mkString__T__T__T__T(($thiz.stringPrefix__T() + "("), ", ", ")")
}
function $f_sc_TraversableLike__stringPrefix__T($thiz) {
  var this$1 = $thiz.repr__O();
  var fqn = $objectGetClass(this$1).getName__T();
  var pos = (((-1) + $uI(fqn.length)) | 0);
  while (true) {
    if ((pos !== (-1))) {
      var index = pos;
      var jsx$1 = ((65535 & $uI(fqn.charCodeAt(index))) === 36)
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      pos = (((-1) + pos) | 0)
    } else {
      break
    }
  };
  if ((pos === (-1))) {
    var jsx$2 = true
  } else {
    var index$1 = pos;
    var jsx$2 = ((65535 & $uI(fqn.charCodeAt(index$1))) === 46)
  };
  if (jsx$2) {
    return ""
  };
  var result = "";
  while (true) {
    var partEnd = ((1 + pos) | 0);
    while (true) {
      if ((pos !== (-1))) {
        var index$2 = pos;
        var jsx$4 = ((65535 & $uI(fqn.charCodeAt(index$2))) <= 57)
      } else {
        var jsx$4 = false
      };
      if (jsx$4) {
        var index$3 = pos;
        var jsx$3 = ((65535 & $uI(fqn.charCodeAt(index$3))) >= 48)
      } else {
        var jsx$3 = false
      };
      if (jsx$3) {
        pos = (((-1) + pos) | 0)
      } else {
        break
      }
    };
    var lastNonDigit = pos;
    while (true) {
      if ((pos !== (-1))) {
        var index$4 = pos;
        var jsx$6 = ((65535 & $uI(fqn.charCodeAt(index$4))) !== 36)
      } else {
        var jsx$6 = false
      };
      if (jsx$6) {
        var index$5 = pos;
        var jsx$5 = ((65535 & $uI(fqn.charCodeAt(index$5))) !== 46)
      } else {
        var jsx$5 = false
      };
      if (jsx$5) {
        pos = (((-1) + pos) | 0)
      } else {
        break
      }
    };
    var partStart = ((1 + pos) | 0);
    if (((pos === lastNonDigit) && (partEnd !== $uI(fqn.length)))) {
      return result
    };
    while (true) {
      if ((pos !== (-1))) {
        var index$6 = pos;
        var jsx$7 = ((65535 & $uI(fqn.charCodeAt(index$6))) === 36)
      } else {
        var jsx$7 = false
      };
      if (jsx$7) {
        pos = (((-1) + pos) | 0)
      } else {
        break
      }
    };
    if ((pos === (-1))) {
      var atEnd = true
    } else {
      var index$7 = pos;
      var atEnd = ((65535 & $uI(fqn.charCodeAt(index$7))) === 46)
    };
    if ((atEnd || (!$f_sc_TraversableLike__isPartLikelySynthetic$1__psc_TraversableLike__T__I__Z($thiz, fqn, partStart)))) {
      var part = $as_T(fqn.substring(partStart, partEnd));
      var thiz = result;
      if ((thiz === null)) {
        throw new $c_jl_NullPointerException().init___()
      };
      if ((thiz === "")) {
        result = part
      } else {
        result = ((("" + part) + new $c_jl_Character().init___C(46)) + result)
      };
      if (atEnd) {
        return result
      }
    }
  }
}
/** @constructor */
function $c_scg_SeqFactory() {
  $c_scg_GenSeqFactory.call(this)
}
$c_scg_SeqFactory.prototype = new $h_scg_GenSeqFactory();
$c_scg_SeqFactory.prototype.constructor = $c_scg_SeqFactory;
/** @constructor */
function $h_scg_SeqFactory() {
  /*<skip>*/
}
$h_scg_SeqFactory.prototype = $c_scg_SeqFactory.prototype;
/** @constructor */
function $c_sci_Set$() {
  $c_scg_ImmutableSetFactory.call(this)
}
$c_sci_Set$.prototype = new $h_scg_ImmutableSetFactory();
$c_sci_Set$.prototype.constructor = $c_sci_Set$;
/** @constructor */
function $h_sci_Set$() {
  /*<skip>*/
}
$h_sci_Set$.prototype = $c_sci_Set$.prototype;
$c_sci_Set$.prototype.init___ = (function() {
  return this
});
var $d_sci_Set$ = new $TypeData().initClass({
  sci_Set$: 0
}, false, "scala.collection.immutable.Set$", {
  sci_Set$: 1,
  scg_ImmutableSetFactory: 1,
  scg_SetFactory: 1,
  scg_GenSetFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_GenericSeqCompanion: 1
});
$c_sci_Set$.prototype.$classData = $d_sci_Set$;
var $n_sci_Set$ = (void 0);
function $m_sci_Set$() {
  if ((!$n_sci_Set$)) {
    $n_sci_Set$ = new $c_sci_Set$().init___()
  };
  return $n_sci_Set$
}
/** @constructor */
function $c_sci_VectorIterator() {
  $c_sc_AbstractIterator.call(this);
  this.endIndex$2 = 0;
  this.blockIndex$2 = 0;
  this.lo$2 = 0;
  this.endLo$2 = 0;
  this.$$undhasNext$2 = false;
  this.depth$2 = 0;
  this.display0$2 = null;
  this.display1$2 = null;
  this.display2$2 = null;
  this.display3$2 = null;
  this.display4$2 = null;
  this.display5$2 = null
}
$c_sci_VectorIterator.prototype = new $h_sc_AbstractIterator();
$c_sci_VectorIterator.prototype.constructor = $c_sci_VectorIterator;
/** @constructor */
function $h_sci_VectorIterator() {
  /*<skip>*/
}
$h_sci_VectorIterator.prototype = $c_sci_VectorIterator.prototype;
$c_sci_VectorIterator.prototype.next__O = (function() {
  if ((!this.$$undhasNext$2)) {
    throw new $c_ju_NoSuchElementException().init___T("reached iterator end")
  };
  var res = this.display0$2.get(this.lo$2);
  this.lo$2 = ((1 + this.lo$2) | 0);
  if ((this.lo$2 === this.endLo$2)) {
    if ((((this.blockIndex$2 + this.lo$2) | 0) < this.endIndex$2)) {
      var newBlockIndex = ((32 + this.blockIndex$2) | 0);
      var xor = (this.blockIndex$2 ^ newBlockIndex);
      $f_sci_VectorPointer__gotoNextBlockStart__I__I__V(this, newBlockIndex, xor);
      this.blockIndex$2 = newBlockIndex;
      var x = ((this.endIndex$2 - this.blockIndex$2) | 0);
      this.endLo$2 = ((x < 32) ? x : 32);
      this.lo$2 = 0
    } else {
      this.$$undhasNext$2 = false
    }
  };
  return res
});
$c_sci_VectorIterator.prototype.display3__AO = (function() {
  return this.display3$2
});
$c_sci_VectorIterator.prototype.depth__I = (function() {
  return this.depth$2
});
$c_sci_VectorIterator.prototype.display5$und$eq__AO__V = (function(x$1) {
  this.display5$2 = x$1
});
$c_sci_VectorIterator.prototype.init___I__I = (function(_startIndex, endIndex) {
  this.endIndex$2 = endIndex;
  this.blockIndex$2 = ((-32) & _startIndex);
  this.lo$2 = (31 & _startIndex);
  var x = ((endIndex - this.blockIndex$2) | 0);
  this.endLo$2 = ((x < 32) ? x : 32);
  this.$$undhasNext$2 = (((this.blockIndex$2 + this.lo$2) | 0) < endIndex);
  return this
});
$c_sci_VectorIterator.prototype.display0__AO = (function() {
  return this.display0$2
});
$c_sci_VectorIterator.prototype.display2$und$eq__AO__V = (function(x$1) {
  this.display2$2 = x$1
});
$c_sci_VectorIterator.prototype.display4__AO = (function() {
  return this.display4$2
});
$c_sci_VectorIterator.prototype.display1$und$eq__AO__V = (function(x$1) {
  this.display1$2 = x$1
});
$c_sci_VectorIterator.prototype.hasNext__Z = (function() {
  return this.$$undhasNext$2
});
$c_sci_VectorIterator.prototype.display4$und$eq__AO__V = (function(x$1) {
  this.display4$2 = x$1
});
$c_sci_VectorIterator.prototype.display1__AO = (function() {
  return this.display1$2
});
$c_sci_VectorIterator.prototype.display5__AO = (function() {
  return this.display5$2
});
$c_sci_VectorIterator.prototype.depth$und$eq__I__V = (function(x$1) {
  this.depth$2 = x$1
});
$c_sci_VectorIterator.prototype.display2__AO = (function() {
  return this.display2$2
});
$c_sci_VectorIterator.prototype.display0$und$eq__AO__V = (function(x$1) {
  this.display0$2 = x$1
});
$c_sci_VectorIterator.prototype.display3$und$eq__AO__V = (function(x$1) {
  this.display3$2 = x$1
});
var $d_sci_VectorIterator = new $TypeData().initClass({
  sci_VectorIterator: 0
}, false, "scala.collection.immutable.VectorIterator", {
  sci_VectorIterator: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sci_VectorPointer: 1
});
$c_sci_VectorIterator.prototype.$classData = $d_sci_VectorIterator;
/** @constructor */
function $c_sjsr_UndefinedBehaviorError() {
  $c_jl_Error.call(this)
}
$c_sjsr_UndefinedBehaviorError.prototype = new $h_jl_Error();
$c_sjsr_UndefinedBehaviorError.prototype.constructor = $c_sjsr_UndefinedBehaviorError;
/** @constructor */
function $h_sjsr_UndefinedBehaviorError() {
  /*<skip>*/
}
$h_sjsr_UndefinedBehaviorError.prototype = $c_sjsr_UndefinedBehaviorError.prototype;
$c_sjsr_UndefinedBehaviorError.prototype.fillInStackTrace__jl_Throwable = (function() {
  return $c_jl_Throwable.prototype.fillInStackTrace__jl_Throwable.call(this)
});
$c_sjsr_UndefinedBehaviorError.prototype.init___jl_Throwable = (function(cause) {
  $c_sjsr_UndefinedBehaviorError.prototype.init___T__jl_Throwable.call(this, ("An undefined behavior was detected" + ((cause === null) ? "" : (": " + cause.getMessage__T()))), cause);
  return this
});
$c_sjsr_UndefinedBehaviorError.prototype.init___T__jl_Throwable = (function(message, cause) {
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, message, cause);
  return this
});
var $d_sjsr_UndefinedBehaviorError = new $TypeData().initClass({
  sjsr_UndefinedBehaviorError: 0
}, false, "scala.scalajs.runtime.UndefinedBehaviorError", {
  sjsr_UndefinedBehaviorError: 1,
  jl_Error: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_util_control_ControlThrowable: 1,
  s_util_control_NoStackTrace: 1
});
$c_sjsr_UndefinedBehaviorError.prototype.$classData = $d_sjsr_UndefinedBehaviorError;
/** @constructor */
function $c_jl_JSConsoleBasedPrintStream() {
  $c_Ljava_io_PrintStream.call(this);
  this.isErr$4 = null;
  this.flushed$4 = false;
  this.buffer$4 = null
}
$c_jl_JSConsoleBasedPrintStream.prototype = new $h_Ljava_io_PrintStream();
$c_jl_JSConsoleBasedPrintStream.prototype.constructor = $c_jl_JSConsoleBasedPrintStream;
/** @constructor */
function $h_jl_JSConsoleBasedPrintStream() {
  /*<skip>*/
}
$h_jl_JSConsoleBasedPrintStream.prototype = $c_jl_JSConsoleBasedPrintStream.prototype;
$c_jl_JSConsoleBasedPrintStream.prototype.init___jl_Boolean = (function(isErr) {
  this.isErr$4 = isErr;
  var out = new $c_jl_JSConsoleBasedPrintStream$DummyOutputStream().init___();
  $c_Ljava_io_PrintStream.prototype.init___Ljava_io_OutputStream__Z__Ljava_nio_charset_Charset.call(this, out, false, null);
  this.flushed$4 = true;
  this.buffer$4 = "";
  return this
});
$c_jl_JSConsoleBasedPrintStream.prototype.java$lang$JSConsoleBasedPrintStream$$printString__T__V = (function(s) {
  var rest = s;
  while ((rest !== "")) {
    var thiz = rest;
    var nlPos = $uI(thiz.indexOf("n"));
    if ((nlPos < 0)) {
      this.buffer$4 = (("" + this.buffer$4) + rest);
      this.flushed$4 = false;
      rest = ""
    } else {
      var jsx$1 = this.buffer$4;
      var thiz$1 = rest;
      this.doWriteLine__p4__T__V((("" + jsx$1) + $as_T(thiz$1.substring(0, nlPos))));
      this.buffer$4 = "";
      this.flushed$4 = true;
      var thiz$2 = rest;
      var beginIndex = ((1 + nlPos) | 0);
      rest = $as_T(thiz$2.substring(beginIndex))
    }
  }
});
$c_jl_JSConsoleBasedPrintStream.prototype.doWriteLine__p4__T__V = (function(line) {
  var x = $g.console;
  if ($uZ((!(!x)))) {
    var x$1 = this.isErr$4;
    if ($uZ(x$1)) {
      var x$2 = $g.console.error;
      var jsx$1 = $uZ((!(!x$2)))
    } else {
      var jsx$1 = false
    };
    if (jsx$1) {
      $g.console.error(line)
    } else {
      $g.console.log(line)
    }
  }
});
var $d_jl_JSConsoleBasedPrintStream = new $TypeData().initClass({
  jl_JSConsoleBasedPrintStream: 0
}, false, "java.lang.JSConsoleBasedPrintStream", {
  jl_JSConsoleBasedPrintStream: 1,
  Ljava_io_PrintStream: 1,
  Ljava_io_FilterOutputStream: 1,
  Ljava_io_OutputStream: 1,
  O: 1,
  Ljava_io_Closeable: 1,
  Ljava_io_Flushable: 1,
  jl_Appendable: 1
});
$c_jl_JSConsoleBasedPrintStream.prototype.$classData = $d_jl_JSConsoleBasedPrintStream;
/** @constructor */
function $c_sc_Seq$() {
  $c_scg_SeqFactory.call(this)
}
$c_sc_Seq$.prototype = new $h_scg_SeqFactory();
$c_sc_Seq$.prototype.constructor = $c_sc_Seq$;
/** @constructor */
function $h_sc_Seq$() {
  /*<skip>*/
}
$h_sc_Seq$.prototype = $c_sc_Seq$.prototype;
$c_sc_Seq$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  return this
});
var $d_sc_Seq$ = new $TypeData().initClass({
  sc_Seq$: 0
}, false, "scala.collection.Seq$", {
  sc_Seq$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
$c_sc_Seq$.prototype.$classData = $d_sc_Seq$;
var $n_sc_Seq$ = (void 0);
function $m_sc_Seq$() {
  if ((!$n_sc_Seq$)) {
    $n_sc_Seq$ = new $c_sc_Seq$().init___()
  };
  return $n_sc_Seq$
}
/** @constructor */
function $c_scg_IndexedSeqFactory() {
  $c_scg_SeqFactory.call(this)
}
$c_scg_IndexedSeqFactory.prototype = new $h_scg_SeqFactory();
$c_scg_IndexedSeqFactory.prototype.constructor = $c_scg_IndexedSeqFactory;
/** @constructor */
function $h_scg_IndexedSeqFactory() {
  /*<skip>*/
}
$h_scg_IndexedSeqFactory.prototype = $c_scg_IndexedSeqFactory.prototype;
/** @constructor */
function $c_s_reflect_AnyValManifest() {
  $c_O.call(this);
  this.toString$1 = null
}
$c_s_reflect_AnyValManifest.prototype = new $h_O();
$c_s_reflect_AnyValManifest.prototype.constructor = $c_s_reflect_AnyValManifest;
/** @constructor */
function $h_s_reflect_AnyValManifest() {
  /*<skip>*/
}
$h_s_reflect_AnyValManifest.prototype = $c_s_reflect_AnyValManifest.prototype;
$c_s_reflect_AnyValManifest.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
$c_s_reflect_AnyValManifest.prototype.toString__T = (function() {
  return this.toString$1
});
$c_s_reflect_AnyValManifest.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this)
});
/** @constructor */
function $c_s_reflect_ManifestFactory$ClassTypeManifest() {
  $c_O.call(this);
  this.prefix$1 = null;
  this.runtimeClass1$1 = null;
  this.typeArguments$1 = null
}
$c_s_reflect_ManifestFactory$ClassTypeManifest.prototype = new $h_O();
$c_s_reflect_ManifestFactory$ClassTypeManifest.prototype.constructor = $c_s_reflect_ManifestFactory$ClassTypeManifest;
/** @constructor */
function $h_s_reflect_ManifestFactory$ClassTypeManifest() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$ClassTypeManifest.prototype = $c_s_reflect_ManifestFactory$ClassTypeManifest.prototype;
/** @constructor */
function $c_sc_IndexedSeq$() {
  $c_scg_IndexedSeqFactory.call(this);
  this.ReusableCBF$6 = null
}
$c_sc_IndexedSeq$.prototype = new $h_scg_IndexedSeqFactory();
$c_sc_IndexedSeq$.prototype.constructor = $c_sc_IndexedSeq$;
/** @constructor */
function $h_sc_IndexedSeq$() {
  /*<skip>*/
}
$h_sc_IndexedSeq$.prototype = $c_sc_IndexedSeq$.prototype;
$c_sc_IndexedSeq$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  $n_sc_IndexedSeq$ = this;
  this.ReusableCBF$6 = new $c_sc_IndexedSeq$$anon$1().init___();
  return this
});
var $d_sc_IndexedSeq$ = new $TypeData().initClass({
  sc_IndexedSeq$: 0
}, false, "scala.collection.IndexedSeq$", {
  sc_IndexedSeq$: 1,
  scg_IndexedSeqFactory: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1
});
$c_sc_IndexedSeq$.prototype.$classData = $d_sc_IndexedSeq$;
var $n_sc_IndexedSeq$ = (void 0);
function $m_sc_IndexedSeq$() {
  if ((!$n_sc_IndexedSeq$)) {
    $n_sc_IndexedSeq$ = new $c_sc_IndexedSeq$().init___()
  };
  return $n_sc_IndexedSeq$
}
/** @constructor */
function $c_sc_IndexedSeqLike$Elements() {
  $c_sc_AbstractIterator.call(this);
  this.end$2 = 0;
  this.index$2 = 0;
  this.$$outer$2 = null
}
$c_sc_IndexedSeqLike$Elements.prototype = new $h_sc_AbstractIterator();
$c_sc_IndexedSeqLike$Elements.prototype.constructor = $c_sc_IndexedSeqLike$Elements;
/** @constructor */
function $h_sc_IndexedSeqLike$Elements() {
  /*<skip>*/
}
$h_sc_IndexedSeqLike$Elements.prototype = $c_sc_IndexedSeqLike$Elements.prototype;
$c_sc_IndexedSeqLike$Elements.prototype.next__O = (function() {
  if ((this.index$2 >= this.end$2)) {
    $m_sc_Iterator$().empty$1.next__O()
  };
  var x = this.$$outer$2.apply__I__O(this.index$2);
  this.index$2 = ((1 + this.index$2) | 0);
  return x
});
$c_sc_IndexedSeqLike$Elements.prototype.init___sc_IndexedSeqLike__I__I = (function($$outer, start, end) {
  this.end$2 = end;
  if (($$outer === null)) {
    throw $m_sjsr_package$().unwrapJavaScriptException__jl_Throwable__O(null)
  } else {
    this.$$outer$2 = $$outer
  };
  this.index$2 = start;
  return this
});
$c_sc_IndexedSeqLike$Elements.prototype.hasNext__Z = (function() {
  return (this.index$2 < this.end$2)
});
var $d_sc_IndexedSeqLike$Elements = new $TypeData().initClass({
  sc_IndexedSeqLike$Elements: 0
}, false, "scala.collection.IndexedSeqLike$Elements", {
  sc_IndexedSeqLike$Elements: 1,
  sc_AbstractIterator: 1,
  O: 1,
  sc_Iterator: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_BufferedIterator: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sc_IndexedSeqLike$Elements.prototype.$classData = $d_sc_IndexedSeqLike$Elements;
/** @constructor */
function $c_sjs_js_JavaScriptException() {
  $c_jl_RuntimeException.call(this);
  this.exception$4 = null
}
$c_sjs_js_JavaScriptException.prototype = new $h_jl_RuntimeException();
$c_sjs_js_JavaScriptException.prototype.constructor = $c_sjs_js_JavaScriptException;
/** @constructor */
function $h_sjs_js_JavaScriptException() {
  /*<skip>*/
}
$h_sjs_js_JavaScriptException.prototype = $c_sjs_js_JavaScriptException.prototype;
$c_sjs_js_JavaScriptException.prototype.productPrefix__T = (function() {
  return "JavaScriptException"
});
$c_sjs_js_JavaScriptException.prototype.productArity__I = (function() {
  return 1
});
$c_sjs_js_JavaScriptException.prototype.fillInStackTrace__jl_Throwable = (function() {
  var e = this.exception$4;
  this.stackdata = e;
  return this
});
$c_sjs_js_JavaScriptException.prototype.equals__O__Z = (function(x$1) {
  if ((this === x$1)) {
    return true
  } else if ($is_sjs_js_JavaScriptException(x$1)) {
    var JavaScriptException$1 = $as_sjs_js_JavaScriptException(x$1);
    return $m_sr_BoxesRunTime$().equals__O__O__Z(this.exception$4, JavaScriptException$1.exception$4)
  } else {
    return false
  }
});
$c_sjs_js_JavaScriptException.prototype.productElement__I__O = (function(x$1) {
  switch (x$1) {
    case 0: {
      return this.exception$4;
      break
    }
    default: {
      throw new $c_jl_IndexOutOfBoundsException().init___T(("" + x$1))
    }
  }
});
$c_sjs_js_JavaScriptException.prototype.getMessage__T = (function() {
  return $objectToString(this.exception$4)
});
$c_sjs_js_JavaScriptException.prototype.init___O = (function(exception) {
  this.exception$4 = exception;
  $c_jl_Throwable.prototype.init___T__jl_Throwable.call(this, null, null);
  return this
});
$c_sjs_js_JavaScriptException.prototype.hashCode__I = (function() {
  var this$2 = $m_s_util_hashing_MurmurHash3$();
  return this$2.productHash__s_Product__I__I(this, (-889275714))
});
$c_sjs_js_JavaScriptException.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
function $is_sjs_js_JavaScriptException(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sjs_js_JavaScriptException)))
}
function $as_sjs_js_JavaScriptException(obj) {
  return (($is_sjs_js_JavaScriptException(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.scalajs.js.JavaScriptException"))
}
function $isArrayOf_sjs_js_JavaScriptException(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sjs_js_JavaScriptException)))
}
function $asArrayOf_sjs_js_JavaScriptException(obj, depth) {
  return (($isArrayOf_sjs_js_JavaScriptException(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.scalajs.js.JavaScriptException;", depth))
}
var $d_sjs_js_JavaScriptException = new $TypeData().initClass({
  sjs_js_JavaScriptException: 0
}, false, "scala.scalajs.js.JavaScriptException", {
  sjs_js_JavaScriptException: 1,
  jl_RuntimeException: 1,
  jl_Exception: 1,
  jl_Throwable: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_Product: 1,
  s_Equals: 1,
  s_Serializable: 1
});
$c_sjs_js_JavaScriptException.prototype.$classData = $d_sjs_js_JavaScriptException;
/** @constructor */
function $c_s_math_BigDecimal() {
  $c_s_math_ScalaNumber.call(this);
  this.bigDecimal$3 = null;
  this.mc$3 = null;
  this.computedHashCode$3 = 0
}
$c_s_math_BigDecimal.prototype = new $h_s_math_ScalaNumber();
$c_s_math_BigDecimal.prototype.constructor = $c_s_math_BigDecimal;
/** @constructor */
function $h_s_math_BigDecimal() {
  /*<skip>*/
}
$h_s_math_BigDecimal.prototype = $c_s_math_BigDecimal.prototype;
$c_s_math_BigDecimal.prototype.isValidInt__Z = (function() {
  try {
    this.bigDecimal$3.intValueExact__I();
    return true
  } catch (e) {
    if ($is_jl_ArithmeticException(e)) {
      return false
    } else {
      throw e
    }
  }
});
$c_s_math_BigDecimal.prototype.toBigInt__s_math_BigInt = (function() {
  return new $c_s_math_BigInt().init___Ljava_math_BigInteger(this.bigDecimal$3.toBigInteger__Ljava_math_BigInteger())
});
$c_s_math_BigDecimal.prototype.longValue__J = (function() {
  return this.bigDecimal$3.longValue__J()
});
$c_s_math_BigDecimal.prototype.init___Ljava_math_BigDecimal__Ljava_math_MathContext = (function(bigDecimal, mc) {
  this.bigDecimal$3 = bigDecimal;
  this.mc$3 = mc;
  if ((bigDecimal === null)) {
    throw new $c_jl_IllegalArgumentException().init___T("null value for BigDecimal")
  };
  if ((mc === null)) {
    throw new $c_jl_IllegalArgumentException().init___T("null MathContext for BigDecimal")
  };
  this.computedHashCode$3 = 1565550863;
  return this
});
$c_s_math_BigDecimal.prototype.isValidShort__Z = (function() {
  try {
    this.bigDecimal$3.shortValueExact__S();
    return true
  } catch (e) {
    if ($is_jl_ArithmeticException(e)) {
      return false
    } else {
      throw e
    }
  }
});
$c_s_math_BigDecimal.prototype.$$div__s_math_BigDecimal__s_math_BigDecimal = (function(that) {
  return new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(this.bigDecimal$3.divide__Ljava_math_BigDecimal__Ljava_math_MathContext__Ljava_math_BigDecimal(that.bigDecimal$3, this.mc$3), this.mc$3)
});
$c_s_math_BigDecimal.prototype.byteValue__B = (function() {
  return ((this.bigDecimal$3.intValue__I() << 24) >> 24)
});
$c_s_math_BigDecimal.prototype.equals__O__Z = (function(that) {
  if ($is_s_math_BigDecimal(that)) {
    var x2 = $as_s_math_BigDecimal(that);
    return this.equals__s_math_BigDecimal__Z(x2)
  } else if ($is_s_math_BigInt(that)) {
    var x3 = $as_s_math_BigInt(that);
    var this$1 = x3.bigInteger$3;
    var jsx$2 = $m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(this$1);
    var jsx$1 = this.bigDecimal$3.precision__I();
    var this$2 = this.bigDecimal$3;
    if ((jsx$2 > (3.3219280948873626 * (((-2) + ((jsx$1 - this$2.java$math$BigDecimal$$$undscale$2) | 0)) | 0)))) {
      var this$3 = this.toBigIntExact__s_Option();
      if ((!this$3.isEmpty__Z())) {
        var arg1 = this$3.get__O();
        var x$1 = $as_s_math_BigInt(arg1);
        return x3.equals__s_math_BigInt__Z(x$1)
      } else {
        return false
      }
    } else {
      return false
    }
  } else if (((typeof that) === "number")) {
    var x4 = $uD(that);
    if ((!((x4 === Infinity) || (x4 === (-Infinity))))) {
      var d = this.bigDecimal$3.doubleValue__D();
      if (((!((d === Infinity) || (d === (-Infinity)))) && (d === x4))) {
        var this$10 = $m_s_math_BigDecimal$();
        return this.equals__s_math_BigDecimal__Z(this$10.decimal__D__Ljava_math_MathContext__s_math_BigDecimal(d, this$10.defaultMathContext$1))
      } else {
        return false
      }
    } else {
      return false
    }
  } else if ($isFloat(that)) {
    var x5 = $uF(that);
    if ((!((x5 === Infinity) || (x5 === (-Infinity))))) {
      var f = this.bigDecimal$3.floatValue__F();
      if (((!((f === Infinity) || (f === (-Infinity)))) && (f === x5))) {
        var this$17 = $m_s_math_BigDecimal$();
        return this.equals__s_math_BigDecimal__Z(this$17.decimal__D__Ljava_math_MathContext__s_math_BigDecimal(f, this$17.defaultMathContext$1))
      } else {
        return false
      }
    } else {
      return false
    }
  } else {
    return (this.isValidLong__Z() && $f_s_math_ScalaNumericAnyConversions__unifiedPrimitiveEquals__O__Z(this, that))
  }
});
$c_s_math_BigDecimal.prototype.isWhole__Z = (function() {
  var this$1 = this.bigDecimal$3;
  if ((this$1.java$math$BigDecimal$$$undscale$2 <= 0)) {
    return true
  } else {
    var this$2 = this.bigDecimal$3.stripTrailingZeros__Ljava_math_BigDecimal();
    return (this$2.java$math$BigDecimal$$$undscale$2 <= 0)
  }
});
$c_s_math_BigDecimal.prototype.isValidChar__Z = (function() {
  return ((this.isValidInt__Z() && (this.bigDecimal$3.intValueExact__I() >= 0)) && (this.bigDecimal$3.intValueExact__I() <= 65535))
});
$c_s_math_BigDecimal.prototype.computeHashCode__p3__V = (function() {
  if (this.isWhole__Z()) {
    var jsx$3 = this.bigDecimal$3.precision__I();
    var this$1 = this.bigDecimal$3;
    var jsx$2 = (((jsx$3 - this$1.java$math$BigDecimal$$$undscale$2) | 0) < 4934)
  } else {
    var jsx$2 = false
  };
  if (jsx$2) {
    var jsx$1 = this.toBigInt__s_math_BigInt().hashCode__I()
  } else if (this.isDecimalDouble__Z()) {
    var jsx$1 = $m_sr_Statics$().doubleHash__D__I(this.bigDecimal$3.doubleValue__D())
  } else {
    var temp = this.bigDecimal$3.stripTrailingZeros__Ljava_math_BigDecimal();
    var jsx$1 = $m_s_util_hashing_MurmurHash3$().mixLast__I__I__I(temp.scaleByPowerOfTen__I__Ljava_math_BigDecimal(temp.java$math$BigDecimal$$$undscale$2).toBigInteger__Ljava_math_BigInteger().hashCode__I(), temp.java$math$BigDecimal$$$undscale$2)
  };
  this.computedHashCode$3 = jsx$1
});
$c_s_math_BigDecimal.prototype.toString__T = (function() {
  return this.bigDecimal$3.toString__T()
});
$c_s_math_BigDecimal.prototype.isValidByte__Z = (function() {
  try {
    this.bigDecimal$3.byteValueExact__B();
    return true
  } catch (e) {
    if ($is_jl_ArithmeticException(e)) {
      return false
    } else {
      throw e
    }
  }
});
$c_s_math_BigDecimal.prototype.compare__O__I = (function(that) {
  var that$1 = $as_s_math_BigDecimal(that);
  return this.bigDecimal$3.compareTo__Ljava_math_BigDecimal__I(that$1.bigDecimal$3)
});
$c_s_math_BigDecimal.prototype.isDecimalDouble__Z = (function() {
  var d = this.bigDecimal$3.doubleValue__D();
  if ((!((d === Infinity) || (d === (-Infinity))))) {
    var this$4 = $m_s_math_BigDecimal$();
    return this.equals__s_math_BigDecimal__Z(this$4.decimal__D__Ljava_math_MathContext__s_math_BigDecimal(d, this$4.defaultMathContext$1))
  } else {
    return false
  }
});
$c_s_math_BigDecimal.prototype.toBigIntExact__s_Option = (function() {
  if (this.isWhole__Z()) {
    try {
      return new $c_s_Some().init___O(new $c_s_math_BigInt().init___Ljava_math_BigInteger(this.bigDecimal$3.toBigIntegerExact__Ljava_math_BigInteger()))
    } catch (e) {
      if ($is_jl_ArithmeticException(e)) {
        return $m_s_None$()
      } else {
        throw e
      }
    }
  } else {
    return $m_s_None$()
  }
});
$c_s_math_BigDecimal.prototype.shortValue__S = (function() {
  return ((this.bigDecimal$3.intValue__I() << 16) >> 16)
});
$c_s_math_BigDecimal.prototype.doubleValue__D = (function() {
  return this.bigDecimal$3.doubleValue__D()
});
$c_s_math_BigDecimal.prototype.hashCode__I = (function() {
  if ((this.computedHashCode$3 === 1565550863)) {
    this.computeHashCode__p3__V()
  };
  return this.computedHashCode$3
});
$c_s_math_BigDecimal.prototype.$$times__s_math_BigDecimal__s_math_BigDecimal = (function(that) {
  return new $c_s_math_BigDecimal().init___Ljava_math_BigDecimal__Ljava_math_MathContext(this.bigDecimal$3.multiply__Ljava_math_BigDecimal__Ljava_math_MathContext__Ljava_math_BigDecimal(that.bigDecimal$3, this.mc$3), this.mc$3)
});
$c_s_math_BigDecimal.prototype.intValue__I = (function() {
  return this.bigDecimal$3.intValue__I()
});
$c_s_math_BigDecimal.prototype.isValidLong__Z = (function() {
  try {
    var this$1 = this.bigDecimal$3;
    this$1.valueExact__p2__I__J(64);
    return true
  } catch (e) {
    if ($is_jl_ArithmeticException(e)) {
      return false
    } else {
      throw e
    }
  }
});
$c_s_math_BigDecimal.prototype.floatValue__F = (function() {
  return this.bigDecimal$3.floatValue__F()
});
$c_s_math_BigDecimal.prototype.equals__s_math_BigDecimal__Z = (function(that) {
  return (this.bigDecimal$3.compareTo__Ljava_math_BigDecimal__I(that.bigDecimal$3) === 0)
});
function $is_s_math_BigDecimal(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_math_BigDecimal)))
}
function $as_s_math_BigDecimal(obj) {
  return (($is_s_math_BigDecimal(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.math.BigDecimal"))
}
function $isArrayOf_s_math_BigDecimal(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_BigDecimal)))
}
function $asArrayOf_s_math_BigDecimal(obj, depth) {
  return (($isArrayOf_s_math_BigDecimal(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.math.BigDecimal;", depth))
}
var $d_s_math_BigDecimal = new $TypeData().initClass({
  s_math_BigDecimal: 0
}, false, "scala.math.BigDecimal", {
  s_math_BigDecimal: 1,
  s_math_ScalaNumber: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_math_ScalaNumericConversions: 1,
  s_math_ScalaNumericAnyConversions: 1,
  s_Serializable: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1
});
$c_s_math_BigDecimal.prototype.$classData = $d_s_math_BigDecimal;
/** @constructor */
function $c_s_math_BigInt() {
  $c_s_math_ScalaNumber.call(this);
  this.bigInteger$3 = null
}
$c_s_math_BigInt.prototype = new $h_s_math_ScalaNumber();
$c_s_math_BigInt.prototype.constructor = $c_s_math_BigInt;
/** @constructor */
function $h_s_math_BigInt() {
  /*<skip>*/
}
$h_s_math_BigInt.prototype = $c_s_math_BigInt.prototype;
$c_s_math_BigInt.prototype.isValidInt__Z = (function() {
  var this$1 = $m_s_math_BigInt$();
  var that = this$1.apply__I__s_math_BigInt((-2147483648));
  if ($f_s_math_Ordered__$$greater$eq__O__Z(this, that)) {
    var this$2 = $m_s_math_BigInt$();
    var that$1 = this$2.apply__I__s_math_BigInt(2147483647);
    return $f_s_math_Ordered__$$less$eq__O__Z(this, that$1)
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.longValue__J = (function() {
  return this.bigInteger$3.longValue__J()
});
$c_s_math_BigInt.prototype.isValidShort__Z = (function() {
  var this$1 = $m_s_math_BigInt$();
  var that = this$1.apply__I__s_math_BigInt((-32768));
  if ($f_s_math_Ordered__$$greater$eq__O__Z(this, that)) {
    var this$2 = $m_s_math_BigInt$();
    var that$1 = this$2.apply__I__s_math_BigInt(32767);
    return $f_s_math_Ordered__$$less$eq__O__Z(this, that$1)
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.byteValue__B = (function() {
  return ((this.bigInteger$3.intValue__I() << 24) >> 24)
});
$c_s_math_BigInt.prototype.equals__O__Z = (function(that) {
  if ($is_s_math_BigInt(that)) {
    var x2 = $as_s_math_BigInt(that);
    return this.equals__s_math_BigInt__Z(x2)
  } else if ($is_s_math_BigDecimal(that)) {
    var x3 = $as_s_math_BigDecimal(that);
    return x3.equals__O__Z(this)
  } else if (((typeof that) === "number")) {
    var x4 = $uD(that);
    if (this.isValidDouble__Z()) {
      var this$1 = this.bigInteger$3;
      return ($m_jl_Double$().parseDouble__T__D($m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$1)) === x4)
    } else {
      return false
    }
  } else if ($isFloat(that)) {
    var x5 = $uF(that);
    if (this.isValidFloat__Z()) {
      var this$2 = this.bigInteger$3;
      var s = $m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$2);
      return ($fround($m_jl_Double$().parseDouble__T__D(s)) === x5)
    } else {
      return false
    }
  } else {
    return (this.isValidLong__Z() && $f_s_math_ScalaNumericAnyConversions__unifiedPrimitiveEquals__O__Z(this, that))
  }
});
$c_s_math_BigInt.prototype.equals__s_math_BigInt__Z = (function(that) {
  return (this.bigInteger$3.compareTo__Ljava_math_BigInteger__I(that.bigInteger$3) === 0)
});
$c_s_math_BigInt.prototype.bitLengthOverflow__p3__Z = (function() {
  var shifted = this.bigInteger$3.shiftRight__I__Ljava_math_BigInteger(2147483647);
  return ((shifted.sign$2 !== 0) && (!shifted.equals__O__Z($m_s_math_BigInt$().scala$math$BigInt$$minusOne$1)))
});
$c_s_math_BigInt.prototype.isValidDouble__Z = (function() {
  var this$1 = this.bigInteger$3;
  var bitLen = $m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(this$1);
  if ((bitLen <= 53)) {
    var jsx$1 = true
  } else {
    var lowest = this.bigInteger$3.getLowestSetBit__I();
    var jsx$1 = (((bitLen <= 1024) && (lowest >= (((-53) + bitLen) | 0))) && (lowest < 1024))
  };
  if (jsx$1) {
    return (!this.bitLengthOverflow__p3__Z())
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.isValidChar__Z = (function() {
  var this$1 = $m_s_math_BigInt$();
  var that = this$1.apply__I__s_math_BigInt(0);
  if ($f_s_math_Ordered__$$greater$eq__O__Z(this, that)) {
    var this$2 = $m_s_math_BigInt$();
    var that$1 = this$2.apply__I__s_math_BigInt(65535);
    return $f_s_math_Ordered__$$less$eq__O__Z(this, that$1)
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.toString__T = (function() {
  var this$1 = this.bigInteger$3;
  return $m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$1)
});
$c_s_math_BigInt.prototype.isValidByte__Z = (function() {
  var this$1 = $m_s_math_BigInt$();
  var that = this$1.apply__I__s_math_BigInt((-128));
  if ($f_s_math_Ordered__$$greater$eq__O__Z(this, that)) {
    var this$2 = $m_s_math_BigInt$();
    var that$1 = this$2.apply__I__s_math_BigInt(127);
    return $f_s_math_Ordered__$$less$eq__O__Z(this, that$1)
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.compare__O__I = (function(that) {
  var that$1 = $as_s_math_BigInt(that);
  return this.bigInteger$3.compareTo__Ljava_math_BigInteger__I(that$1.bigInteger$3)
});
$c_s_math_BigInt.prototype.isValidFloat__Z = (function() {
  var this$1 = this.bigInteger$3;
  var bitLen = $m_Ljava_math_BitLevel$().bitLength__Ljava_math_BigInteger__I(this$1);
  if ((bitLen <= 24)) {
    var jsx$1 = true
  } else {
    var lowest = this.bigInteger$3.getLowestSetBit__I();
    var jsx$1 = (((bitLen <= 128) && (lowest >= (((-24) + bitLen) | 0))) && (lowest < 128))
  };
  if (jsx$1) {
    return (!this.bitLengthOverflow__p3__Z())
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.$$plus__s_math_BigInt__s_math_BigInt = (function(that) {
  var this$1 = this.bigInteger$3;
  var bi = that.bigInteger$3;
  return new $c_s_math_BigInt().init___Ljava_math_BigInteger($m_Ljava_math_Elementary$().add__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$1, bi))
});
$c_s_math_BigInt.prototype.$$minus__s_math_BigInt__s_math_BigInt = (function(that) {
  var this$1 = this.bigInteger$3;
  var bi = that.bigInteger$3;
  return new $c_s_math_BigInt().init___Ljava_math_BigInteger($m_Ljava_math_Elementary$().subtract__Ljava_math_BigInteger__Ljava_math_BigInteger__Ljava_math_BigInteger(this$1, bi))
});
$c_s_math_BigInt.prototype.shortValue__S = (function() {
  return ((this.bigInteger$3.intValue__I() << 16) >> 16)
});
$c_s_math_BigInt.prototype.max__s_math_BigInt__s_math_BigInt = (function(that) {
  return new $c_s_math_BigInt().init___Ljava_math_BigInteger(this.bigInteger$3.max__Ljava_math_BigInteger__Ljava_math_BigInteger(that.bigInteger$3))
});
$c_s_math_BigInt.prototype.doubleValue__D = (function() {
  var this$1 = this.bigInteger$3;
  return $m_jl_Double$().parseDouble__T__D($m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$1))
});
$c_s_math_BigInt.prototype.hashCode__I = (function() {
  return (this.isValidLong__Z() ? $f_s_math_ScalaNumericAnyConversions__unifiedPrimitiveHashcode__I(this) : $m_sr_Statics$().anyHash__O__I(this.bigInteger$3))
});
$c_s_math_BigInt.prototype.intValue__I = (function() {
  return this.bigInteger$3.intValue__I()
});
$c_s_math_BigInt.prototype.init___Ljava_math_BigInteger = (function(bigInteger) {
  this.bigInteger$3 = bigInteger;
  return this
});
$c_s_math_BigInt.prototype.isValidLong__Z = (function() {
  var this$1 = $m_s_math_BigInt$();
  var that = this$1.apply__J__s_math_BigInt(new $c_sjsr_RuntimeLong().init___I__I(0, (-2147483648)));
  if ($f_s_math_Ordered__$$greater$eq__O__Z(this, that)) {
    var this$2 = $m_s_math_BigInt$();
    var that$1 = this$2.apply__J__s_math_BigInt(new $c_sjsr_RuntimeLong().init___I__I((-1), 2147483647));
    return $f_s_math_Ordered__$$less$eq__O__Z(this, that$1)
  } else {
    return false
  }
});
$c_s_math_BigInt.prototype.floatValue__F = (function() {
  var this$1 = this.bigInteger$3;
  var s = $m_Ljava_math_Conversion$().toDecimalScaledString__Ljava_math_BigInteger__T(this$1);
  return $fround($m_jl_Double$().parseDouble__T__D(s))
});
function $is_s_math_BigInt(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.s_math_BigInt)))
}
function $as_s_math_BigInt(obj) {
  return (($is_s_math_BigInt(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.math.BigInt"))
}
function $isArrayOf_s_math_BigInt(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.s_math_BigInt)))
}
function $asArrayOf_s_math_BigInt(obj, depth) {
  return (($isArrayOf_s_math_BigInt(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.math.BigInt;", depth))
}
var $d_s_math_BigInt = new $TypeData().initClass({
  s_math_BigInt: 0
}, false, "scala.math.BigInt", {
  s_math_BigInt: 1,
  s_math_ScalaNumber: 1,
  jl_Number: 1,
  O: 1,
  Ljava_io_Serializable: 1,
  s_math_ScalaNumericConversions: 1,
  s_math_ScalaNumericAnyConversions: 1,
  s_Serializable: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1
});
$c_s_math_BigInt.prototype.$classData = $d_s_math_BigInt;
/** @constructor */
function $c_s_reflect_ManifestFactory$BooleanManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$BooleanManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$BooleanManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$BooleanManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$BooleanManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$BooleanManifest$.prototype = $c_s_reflect_ManifestFactory$BooleanManifest$.prototype;
$c_s_reflect_ManifestFactory$BooleanManifest$.prototype.init___ = (function() {
  this.toString$1 = "Boolean";
  return this
});
var $d_s_reflect_ManifestFactory$BooleanManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$BooleanManifest$: 0
}, false, "scala.reflect.ManifestFactory$BooleanManifest$", {
  s_reflect_ManifestFactory$BooleanManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$BooleanManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$BooleanManifest$;
var $n_s_reflect_ManifestFactory$BooleanManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$BooleanManifest$() {
  if ((!$n_s_reflect_ManifestFactory$BooleanManifest$)) {
    $n_s_reflect_ManifestFactory$BooleanManifest$ = new $c_s_reflect_ManifestFactory$BooleanManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$BooleanManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$ByteManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$ByteManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$ByteManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$ByteManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$ByteManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$ByteManifest$.prototype = $c_s_reflect_ManifestFactory$ByteManifest$.prototype;
$c_s_reflect_ManifestFactory$ByteManifest$.prototype.init___ = (function() {
  this.toString$1 = "Byte";
  return this
});
var $d_s_reflect_ManifestFactory$ByteManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$ByteManifest$: 0
}, false, "scala.reflect.ManifestFactory$ByteManifest$", {
  s_reflect_ManifestFactory$ByteManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$ByteManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$ByteManifest$;
var $n_s_reflect_ManifestFactory$ByteManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$ByteManifest$() {
  if ((!$n_s_reflect_ManifestFactory$ByteManifest$)) {
    $n_s_reflect_ManifestFactory$ByteManifest$ = new $c_s_reflect_ManifestFactory$ByteManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$ByteManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$CharManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$CharManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$CharManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$CharManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$CharManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$CharManifest$.prototype = $c_s_reflect_ManifestFactory$CharManifest$.prototype;
$c_s_reflect_ManifestFactory$CharManifest$.prototype.init___ = (function() {
  this.toString$1 = "Char";
  return this
});
var $d_s_reflect_ManifestFactory$CharManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$CharManifest$: 0
}, false, "scala.reflect.ManifestFactory$CharManifest$", {
  s_reflect_ManifestFactory$CharManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$CharManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$CharManifest$;
var $n_s_reflect_ManifestFactory$CharManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$CharManifest$() {
  if ((!$n_s_reflect_ManifestFactory$CharManifest$)) {
    $n_s_reflect_ManifestFactory$CharManifest$ = new $c_s_reflect_ManifestFactory$CharManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$CharManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$DoubleManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$DoubleManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$DoubleManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$DoubleManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$DoubleManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$DoubleManifest$.prototype = $c_s_reflect_ManifestFactory$DoubleManifest$.prototype;
$c_s_reflect_ManifestFactory$DoubleManifest$.prototype.init___ = (function() {
  this.toString$1 = "Double";
  return this
});
var $d_s_reflect_ManifestFactory$DoubleManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$DoubleManifest$: 0
}, false, "scala.reflect.ManifestFactory$DoubleManifest$", {
  s_reflect_ManifestFactory$DoubleManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$DoubleManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$DoubleManifest$;
var $n_s_reflect_ManifestFactory$DoubleManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$DoubleManifest$() {
  if ((!$n_s_reflect_ManifestFactory$DoubleManifest$)) {
    $n_s_reflect_ManifestFactory$DoubleManifest$ = new $c_s_reflect_ManifestFactory$DoubleManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$DoubleManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$FloatManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$FloatManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$FloatManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$FloatManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$FloatManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$FloatManifest$.prototype = $c_s_reflect_ManifestFactory$FloatManifest$.prototype;
$c_s_reflect_ManifestFactory$FloatManifest$.prototype.init___ = (function() {
  this.toString$1 = "Float";
  return this
});
var $d_s_reflect_ManifestFactory$FloatManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$FloatManifest$: 0
}, false, "scala.reflect.ManifestFactory$FloatManifest$", {
  s_reflect_ManifestFactory$FloatManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$FloatManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$FloatManifest$;
var $n_s_reflect_ManifestFactory$FloatManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$FloatManifest$() {
  if ((!$n_s_reflect_ManifestFactory$FloatManifest$)) {
    $n_s_reflect_ManifestFactory$FloatManifest$ = new $c_s_reflect_ManifestFactory$FloatManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$FloatManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$IntManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$IntManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$IntManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$IntManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$IntManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$IntManifest$.prototype = $c_s_reflect_ManifestFactory$IntManifest$.prototype;
$c_s_reflect_ManifestFactory$IntManifest$.prototype.init___ = (function() {
  this.toString$1 = "Int";
  return this
});
var $d_s_reflect_ManifestFactory$IntManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$IntManifest$: 0
}, false, "scala.reflect.ManifestFactory$IntManifest$", {
  s_reflect_ManifestFactory$IntManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$IntManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$IntManifest$;
var $n_s_reflect_ManifestFactory$IntManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$IntManifest$() {
  if ((!$n_s_reflect_ManifestFactory$IntManifest$)) {
    $n_s_reflect_ManifestFactory$IntManifest$ = new $c_s_reflect_ManifestFactory$IntManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$IntManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$LongManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$LongManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$LongManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$LongManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$LongManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$LongManifest$.prototype = $c_s_reflect_ManifestFactory$LongManifest$.prototype;
$c_s_reflect_ManifestFactory$LongManifest$.prototype.init___ = (function() {
  this.toString$1 = "Long";
  return this
});
var $d_s_reflect_ManifestFactory$LongManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$LongManifest$: 0
}, false, "scala.reflect.ManifestFactory$LongManifest$", {
  s_reflect_ManifestFactory$LongManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$LongManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$LongManifest$;
var $n_s_reflect_ManifestFactory$LongManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$LongManifest$() {
  if ((!$n_s_reflect_ManifestFactory$LongManifest$)) {
    $n_s_reflect_ManifestFactory$LongManifest$ = new $c_s_reflect_ManifestFactory$LongManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$LongManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$PhantomManifest() {
  $c_s_reflect_ManifestFactory$ClassTypeManifest.call(this);
  this.toString$2 = null
}
$c_s_reflect_ManifestFactory$PhantomManifest.prototype = new $h_s_reflect_ManifestFactory$ClassTypeManifest();
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.constructor = $c_s_reflect_ManifestFactory$PhantomManifest;
/** @constructor */
function $h_s_reflect_ManifestFactory$PhantomManifest() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$PhantomManifest.prototype = $c_s_reflect_ManifestFactory$PhantomManifest.prototype;
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.equals__O__Z = (function(that) {
  return (this === that)
});
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.toString__T = (function() {
  return this.toString$2
});
$c_s_reflect_ManifestFactory$PhantomManifest.prototype.hashCode__I = (function() {
  return $systemIdentityHashCode(this)
});
/** @constructor */
function $c_s_reflect_ManifestFactory$ShortManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$ShortManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$ShortManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$ShortManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$ShortManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$ShortManifest$.prototype = $c_s_reflect_ManifestFactory$ShortManifest$.prototype;
$c_s_reflect_ManifestFactory$ShortManifest$.prototype.init___ = (function() {
  this.toString$1 = "Short";
  return this
});
var $d_s_reflect_ManifestFactory$ShortManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$ShortManifest$: 0
}, false, "scala.reflect.ManifestFactory$ShortManifest$", {
  s_reflect_ManifestFactory$ShortManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$ShortManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$ShortManifest$;
var $n_s_reflect_ManifestFactory$ShortManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$ShortManifest$() {
  if ((!$n_s_reflect_ManifestFactory$ShortManifest$)) {
    $n_s_reflect_ManifestFactory$ShortManifest$ = new $c_s_reflect_ManifestFactory$ShortManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$ShortManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$UnitManifest$() {
  $c_s_reflect_AnyValManifest.call(this)
}
$c_s_reflect_ManifestFactory$UnitManifest$.prototype = new $h_s_reflect_AnyValManifest();
$c_s_reflect_ManifestFactory$UnitManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$UnitManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$UnitManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$UnitManifest$.prototype = $c_s_reflect_ManifestFactory$UnitManifest$.prototype;
$c_s_reflect_ManifestFactory$UnitManifest$.prototype.init___ = (function() {
  this.toString$1 = "Unit";
  return this
});
var $d_s_reflect_ManifestFactory$UnitManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$UnitManifest$: 0
}, false, "scala.reflect.ManifestFactory$UnitManifest$", {
  s_reflect_ManifestFactory$UnitManifest$: 1,
  s_reflect_AnyValManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$UnitManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$UnitManifest$;
var $n_s_reflect_ManifestFactory$UnitManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$UnitManifest$() {
  if ((!$n_s_reflect_ManifestFactory$UnitManifest$)) {
    $n_s_reflect_ManifestFactory$UnitManifest$ = new $c_s_reflect_ManifestFactory$UnitManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$UnitManifest$
}
function $f_sc_IterableLike__sameElements__sc_GenIterable__Z($thiz, that) {
  var these = $thiz.iterator__sc_Iterator();
  var those = that.iterator__sc_Iterator();
  while ((these.hasNext__Z() && those.hasNext__Z())) {
    if ((!$m_sr_BoxesRunTime$().equals__O__O__Z(these.next__O(), those.next__O()))) {
      return false
    }
  };
  return ((!these.hasNext__Z()) && (!those.hasNext__Z()))
}
/** @constructor */
function $c_sci_List$() {
  $c_scg_SeqFactory.call(this);
  this.partialNotApplied$5 = null
}
$c_sci_List$.prototype = new $h_scg_SeqFactory();
$c_sci_List$.prototype.constructor = $c_sci_List$;
/** @constructor */
function $h_sci_List$() {
  /*<skip>*/
}
$h_sci_List$.prototype = $c_sci_List$.prototype;
$c_sci_List$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  $n_sci_List$ = this;
  this.partialNotApplied$5 = new $c_sci_List$$anon$1().init___();
  return this
});
var $d_sci_List$ = new $TypeData().initClass({
  sci_List$: 0
}, false, "scala.collection.immutable.List$", {
  sci_List$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_List$.prototype.$classData = $d_sci_List$;
var $n_sci_List$ = (void 0);
function $m_sci_List$() {
  if ((!$n_sci_List$)) {
    $n_sci_List$ = new $c_sci_List$().init___()
  };
  return $n_sci_List$
}
/** @constructor */
function $c_sci_Stream$() {
  $c_scg_SeqFactory.call(this)
}
$c_sci_Stream$.prototype = new $h_scg_SeqFactory();
$c_sci_Stream$.prototype.constructor = $c_sci_Stream$;
/** @constructor */
function $h_sci_Stream$() {
  /*<skip>*/
}
$h_sci_Stream$.prototype = $c_sci_Stream$.prototype;
$c_sci_Stream$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  return this
});
var $d_sci_Stream$ = new $TypeData().initClass({
  sci_Stream$: 0
}, false, "scala.collection.immutable.Stream$", {
  sci_Stream$: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_Stream$.prototype.$classData = $d_sci_Stream$;
var $n_sci_Stream$ = (void 0);
function $m_sci_Stream$() {
  if ((!$n_sci_Stream$)) {
    $n_sci_Stream$ = new $c_sci_Stream$().init___()
  };
  return $n_sci_Stream$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$AnyManifest$() {
  $c_s_reflect_ManifestFactory$PhantomManifest.call(this)
}
$c_s_reflect_ManifestFactory$AnyManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$AnyManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$AnyManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$AnyManifest$.prototype = $c_s_reflect_ManifestFactory$AnyManifest$.prototype;
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.init___ = (function() {
  this.toString$2 = "Any";
  var prefix = $m_s_None$();
  var typeArguments = $m_sci_Nil$();
  this.prefix$1 = prefix;
  this.runtimeClass1$1 = $d_O.getClassOf();
  this.typeArguments$1 = typeArguments;
  return this
});
var $d_s_reflect_ManifestFactory$AnyManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$AnyManifest$: 0
}, false, "scala.reflect.ManifestFactory$AnyManifest$", {
  s_reflect_ManifestFactory$AnyManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$AnyManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$AnyManifest$;
var $n_s_reflect_ManifestFactory$AnyManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$AnyManifest$() {
  if ((!$n_s_reflect_ManifestFactory$AnyManifest$)) {
    $n_s_reflect_ManifestFactory$AnyManifest$ = new $c_s_reflect_ManifestFactory$AnyManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$AnyManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$AnyValManifest$() {
  $c_s_reflect_ManifestFactory$PhantomManifest.call(this)
}
$c_s_reflect_ManifestFactory$AnyValManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$AnyValManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$AnyValManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$AnyValManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$AnyValManifest$.prototype = $c_s_reflect_ManifestFactory$AnyValManifest$.prototype;
$c_s_reflect_ManifestFactory$AnyValManifest$.prototype.init___ = (function() {
  this.toString$2 = "AnyVal";
  var prefix = $m_s_None$();
  var typeArguments = $m_sci_Nil$();
  this.prefix$1 = prefix;
  this.runtimeClass1$1 = $d_O.getClassOf();
  this.typeArguments$1 = typeArguments;
  return this
});
var $d_s_reflect_ManifestFactory$AnyValManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$AnyValManifest$: 0
}, false, "scala.reflect.ManifestFactory$AnyValManifest$", {
  s_reflect_ManifestFactory$AnyValManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$AnyValManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$AnyValManifest$;
var $n_s_reflect_ManifestFactory$AnyValManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$AnyValManifest$() {
  if ((!$n_s_reflect_ManifestFactory$AnyValManifest$)) {
    $n_s_reflect_ManifestFactory$AnyValManifest$ = new $c_s_reflect_ManifestFactory$AnyValManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$AnyValManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$NothingManifest$() {
  $c_s_reflect_ManifestFactory$PhantomManifest.call(this)
}
$c_s_reflect_ManifestFactory$NothingManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$NothingManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$NothingManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$NothingManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$NothingManifest$.prototype = $c_s_reflect_ManifestFactory$NothingManifest$.prototype;
$c_s_reflect_ManifestFactory$NothingManifest$.prototype.init___ = (function() {
  this.toString$2 = "Nothing";
  var prefix = $m_s_None$();
  var typeArguments = $m_sci_Nil$();
  this.prefix$1 = prefix;
  this.runtimeClass1$1 = $d_sr_Nothing$.getClassOf();
  this.typeArguments$1 = typeArguments;
  return this
});
var $d_s_reflect_ManifestFactory$NothingManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$NothingManifest$: 0
}, false, "scala.reflect.ManifestFactory$NothingManifest$", {
  s_reflect_ManifestFactory$NothingManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$NothingManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$NothingManifest$;
var $n_s_reflect_ManifestFactory$NothingManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$NothingManifest$() {
  if ((!$n_s_reflect_ManifestFactory$NothingManifest$)) {
    $n_s_reflect_ManifestFactory$NothingManifest$ = new $c_s_reflect_ManifestFactory$NothingManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$NothingManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$NullManifest$() {
  $c_s_reflect_ManifestFactory$PhantomManifest.call(this)
}
$c_s_reflect_ManifestFactory$NullManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$NullManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$NullManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$NullManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$NullManifest$.prototype = $c_s_reflect_ManifestFactory$NullManifest$.prototype;
$c_s_reflect_ManifestFactory$NullManifest$.prototype.init___ = (function() {
  this.toString$2 = "Null";
  var prefix = $m_s_None$();
  var typeArguments = $m_sci_Nil$();
  this.prefix$1 = prefix;
  this.runtimeClass1$1 = $d_sr_Null$.getClassOf();
  this.typeArguments$1 = typeArguments;
  return this
});
var $d_s_reflect_ManifestFactory$NullManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$NullManifest$: 0
}, false, "scala.reflect.ManifestFactory$NullManifest$", {
  s_reflect_ManifestFactory$NullManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$NullManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$NullManifest$;
var $n_s_reflect_ManifestFactory$NullManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$NullManifest$() {
  if ((!$n_s_reflect_ManifestFactory$NullManifest$)) {
    $n_s_reflect_ManifestFactory$NullManifest$ = new $c_s_reflect_ManifestFactory$NullManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$NullManifest$
}
/** @constructor */
function $c_s_reflect_ManifestFactory$ObjectManifest$() {
  $c_s_reflect_ManifestFactory$PhantomManifest.call(this)
}
$c_s_reflect_ManifestFactory$ObjectManifest$.prototype = new $h_s_reflect_ManifestFactory$PhantomManifest();
$c_s_reflect_ManifestFactory$ObjectManifest$.prototype.constructor = $c_s_reflect_ManifestFactory$ObjectManifest$;
/** @constructor */
function $h_s_reflect_ManifestFactory$ObjectManifest$() {
  /*<skip>*/
}
$h_s_reflect_ManifestFactory$ObjectManifest$.prototype = $c_s_reflect_ManifestFactory$ObjectManifest$.prototype;
$c_s_reflect_ManifestFactory$ObjectManifest$.prototype.init___ = (function() {
  this.toString$2 = "Object";
  var prefix = $m_s_None$();
  var typeArguments = $m_sci_Nil$();
  this.prefix$1 = prefix;
  this.runtimeClass1$1 = $d_O.getClassOf();
  this.typeArguments$1 = typeArguments;
  return this
});
var $d_s_reflect_ManifestFactory$ObjectManifest$ = new $TypeData().initClass({
  s_reflect_ManifestFactory$ObjectManifest$: 0
}, false, "scala.reflect.ManifestFactory$ObjectManifest$", {
  s_reflect_ManifestFactory$ObjectManifest$: 1,
  s_reflect_ManifestFactory$PhantomManifest: 1,
  s_reflect_ManifestFactory$ClassTypeManifest: 1,
  O: 1,
  s_reflect_Manifest: 1,
  s_reflect_ClassTag: 1,
  s_reflect_ClassManifestDeprecatedApis: 1,
  s_reflect_OptManifest: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  s_Equals: 1
});
$c_s_reflect_ManifestFactory$ObjectManifest$.prototype.$classData = $d_s_reflect_ManifestFactory$ObjectManifest$;
var $n_s_reflect_ManifestFactory$ObjectManifest$ = (void 0);
function $m_s_reflect_ManifestFactory$ObjectManifest$() {
  if ((!$n_s_reflect_ManifestFactory$ObjectManifest$)) {
    $n_s_reflect_ManifestFactory$ObjectManifest$ = new $c_s_reflect_ManifestFactory$ObjectManifest$().init___()
  };
  return $n_s_reflect_ManifestFactory$ObjectManifest$
}
function $is_sc_GenSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_GenSeq)))
}
function $as_sc_GenSeq(obj) {
  return (($is_sc_GenSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.GenSeq"))
}
function $isArrayOf_sc_GenSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_GenSeq)))
}
function $asArrayOf_sc_GenSeq(obj, depth) {
  return (($isArrayOf_sc_GenSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.GenSeq;", depth))
}
/** @constructor */
function $c_sci_Vector$() {
  $c_scg_IndexedSeqFactory.call(this);
  this.NIL$6 = null
}
$c_sci_Vector$.prototype = new $h_scg_IndexedSeqFactory();
$c_sci_Vector$.prototype.constructor = $c_sci_Vector$;
/** @constructor */
function $h_sci_Vector$() {
  /*<skip>*/
}
$h_sci_Vector$.prototype = $c_sci_Vector$.prototype;
$c_sci_Vector$.prototype.init___ = (function() {
  $c_scg_GenTraversableFactory.prototype.init___.call(this);
  $n_sci_Vector$ = this;
  this.NIL$6 = new $c_sci_Vector().init___I__I__I(0, 0, 0);
  return this
});
var $d_sci_Vector$ = new $TypeData().initClass({
  sci_Vector$: 0
}, false, "scala.collection.immutable.Vector$", {
  sci_Vector$: 1,
  scg_IndexedSeqFactory: 1,
  scg_SeqFactory: 1,
  scg_GenSeqFactory: 1,
  scg_GenTraversableFactory: 1,
  scg_GenericCompanion: 1,
  O: 1,
  scg_TraversableFactory: 1,
  scg_GenericSeqCompanion: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_Vector$.prototype.$classData = $d_sci_Vector$;
var $n_sci_Vector$ = (void 0);
function $m_sci_Vector$() {
  if ((!$n_sci_Vector$)) {
    $n_sci_Vector$ = new $c_sci_Vector$().init___()
  };
  return $n_sci_Vector$
}
/** @constructor */
function $c_sc_AbstractTraversable() {
  $c_O.call(this)
}
$c_sc_AbstractTraversable.prototype = new $h_O();
$c_sc_AbstractTraversable.prototype.constructor = $c_sc_AbstractTraversable;
/** @constructor */
function $h_sc_AbstractTraversable() {
  /*<skip>*/
}
$h_sc_AbstractTraversable.prototype = $c_sc_AbstractTraversable.prototype;
$c_sc_AbstractTraversable.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  return $f_sc_TraversableOnce__mkString__T__T__T__T(this, start, sep, end)
});
$c_sc_AbstractTraversable.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return $f_sc_TraversableOnce__addString__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
$c_sc_AbstractTraversable.prototype.repr__O = (function() {
  return this
});
$c_sc_AbstractTraversable.prototype.stringPrefix__T = (function() {
  return $f_sc_TraversableLike__stringPrefix__T(this)
});
function $f_sc_SeqLike__lengthCompare__I__I($thiz, len) {
  if ((len < 0)) {
    return 1
  } else {
    var i = 0;
    var it = $thiz.iterator__sc_Iterator();
    while (it.hasNext__Z()) {
      if ((i === len)) {
        return (it.hasNext__Z() ? 1 : 0)
      };
      it.next__O();
      i = ((1 + i) | 0)
    };
    return ((i - len) | 0)
  }
}
function $f_sc_SeqLike__isEmpty__Z($thiz) {
  return ($thiz.lengthCompare__I__I(0) === 0)
}
function $is_sc_LinearSeqLike(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeqLike)))
}
function $as_sc_LinearSeqLike(obj) {
  return (($is_sc_LinearSeqLike(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.LinearSeqLike"))
}
function $isArrayOf_sc_LinearSeqLike(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeqLike)))
}
function $asArrayOf_sc_LinearSeqLike(obj, depth) {
  return (($isArrayOf_sc_LinearSeqLike(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.LinearSeqLike;", depth))
}
function $f_sc_IndexedSeqOptimized__lengthCompare__I__I($thiz, len) {
  return (($thiz.length__I() - len) | 0)
}
function $f_sc_IndexedSeqOptimized__sameElements__sc_GenIterable__Z($thiz, that) {
  if ($is_sc_IndexedSeq(that)) {
    var x2 = $as_sc_IndexedSeq(that);
    var len = $thiz.length__I();
    if ((len === x2.length__I())) {
      var i = 0;
      while (((i < len) && $m_sr_BoxesRunTime$().equals__O__O__Z($thiz.apply__I__O(i), x2.apply__I__O(i)))) {
        i = ((1 + i) | 0)
      };
      return (i === len)
    } else {
      return false
    }
  } else {
    return $f_sc_IterableLike__sameElements__sc_GenIterable__Z($thiz, that)
  }
}
function $f_sc_IndexedSeqOptimized__isEmpty__Z($thiz) {
  return ($thiz.length__I() === 0)
}
function $f_sc_IndexedSeqOptimized__foreach__F1__V($thiz, f) {
  var i = 0;
  var len = $thiz.length__I();
  while ((i < len)) {
    f.apply__O__O($thiz.apply__I__O(i));
    i = ((1 + i) | 0)
  }
}
function $f_sc_LinearSeqOptimized__lengthCompare__I__I($thiz, len) {
  if ((len < 0)) {
    return 1
  } else {
    var i = 0;
    var xs = $thiz;
    return $f_sc_LinearSeqOptimized__loop$1__psc_LinearSeqOptimized__I__sc_LinearSeqOptimized__I__I($thiz, i, xs, len)
  }
}
function $f_sc_LinearSeqOptimized__sameElements__sc_GenIterable__Z($thiz, that) {
  if ($is_sc_LinearSeq(that)) {
    var x2 = $as_sc_LinearSeq(that);
    if (($thiz === x2)) {
      return true
    } else {
      var these = $thiz;
      var those = x2;
      while ((((!these.isEmpty__Z()) && (!those.isEmpty__Z())) && $m_sr_BoxesRunTime$().equals__O__O__Z(these.head__O(), those.head__O()))) {
        these = $as_sc_LinearSeqOptimized(these.tail__O());
        those = $as_sc_LinearSeq(those.tail__O())
      };
      return (these.isEmpty__Z() && those.isEmpty__Z())
    }
  } else {
    return $f_sc_IterableLike__sameElements__sc_GenIterable__Z($thiz, that)
  }
}
function $f_sc_LinearSeqOptimized__apply__I__O($thiz, n) {
  var rest = $thiz.drop__I__sc_LinearSeqOptimized(n);
  if (((n < 0) || rest.isEmpty__Z())) {
    throw new $c_jl_IndexOutOfBoundsException().init___T(("" + n))
  };
  return rest.head__O()
}
function $f_sc_LinearSeqOptimized__length__I($thiz) {
  var these = $thiz;
  var len = 0;
  while ((!these.isEmpty__Z())) {
    len = ((1 + len) | 0);
    these = $as_sc_LinearSeqOptimized(these.tail__O())
  };
  return len
}
function $f_sc_LinearSeqOptimized__last__O($thiz) {
  if ($thiz.isEmpty__Z()) {
    throw new $c_ju_NoSuchElementException().init___()
  };
  var these = $thiz;
  var nx = $as_sc_LinearSeqOptimized(these.tail__O());
  while ((!nx.isEmpty__Z())) {
    these = nx;
    nx = $as_sc_LinearSeqOptimized(nx.tail__O())
  };
  return these.head__O()
}
function $f_sc_LinearSeqOptimized__loop$1__psc_LinearSeqOptimized__I__sc_LinearSeqOptimized__I__I($thiz, i, xs, len$1) {
  _loop: while (true) {
    if ((i === len$1)) {
      return (xs.isEmpty__Z() ? 0 : 1)
    } else if (xs.isEmpty__Z()) {
      return (-1)
    } else {
      var temp$i = ((1 + i) | 0);
      var temp$xs = $as_sc_LinearSeqOptimized(xs.tail__O());
      i = temp$i;
      xs = temp$xs;
      continue _loop
    }
  }
}
function $is_sc_LinearSeqOptimized(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeqOptimized)))
}
function $as_sc_LinearSeqOptimized(obj) {
  return (($is_sc_LinearSeqOptimized(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.LinearSeqOptimized"))
}
function $isArrayOf_sc_LinearSeqOptimized(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeqOptimized)))
}
function $asArrayOf_sc_LinearSeqOptimized(obj, depth) {
  return (($isArrayOf_sc_LinearSeqOptimized(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.LinearSeqOptimized;", depth))
}
/** @constructor */
function $c_sc_AbstractIterable() {
  $c_sc_AbstractTraversable.call(this)
}
$c_sc_AbstractIterable.prototype = new $h_sc_AbstractTraversable();
$c_sc_AbstractIterable.prototype.constructor = $c_sc_AbstractIterable;
/** @constructor */
function $h_sc_AbstractIterable() {
  /*<skip>*/
}
$h_sc_AbstractIterable.prototype = $c_sc_AbstractIterable.prototype;
$c_sc_AbstractIterable.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return $f_sc_IterableLike__sameElements__sc_GenIterable__Z(this, that)
});
$c_sc_AbstractIterable.prototype.foreach__F1__V = (function(f) {
  var this$1 = this.iterator__sc_Iterator();
  $f_sc_Iterator__foreach__F1__V(this$1, f)
});
/** @constructor */
function $c_sci_StringOps() {
  $c_O.call(this);
  this.repr$1 = null
}
$c_sci_StringOps.prototype = new $h_O();
$c_sci_StringOps.prototype.constructor = $c_sci_StringOps;
/** @constructor */
function $h_sci_StringOps() {
  /*<skip>*/
}
$h_sci_StringOps.prototype = $c_sci_StringOps.prototype;
$c_sci_StringOps.prototype.apply__I__O = (function(idx) {
  var $$this = this.repr$1;
  var c = (65535 & $uI($$this.charCodeAt(idx)));
  return new $c_jl_Character().init___C(c)
});
$c_sci_StringOps.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_IndexedSeqOptimized__lengthCompare__I__I(this, len)
});
$c_sci_StringOps.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return $f_sc_IndexedSeqOptimized__sameElements__sc_GenIterable__Z(this, that)
});
$c_sci_StringOps.prototype.isEmpty__Z = (function() {
  return $f_sc_IndexedSeqOptimized__isEmpty__Z(this)
});
$c_sci_StringOps.prototype.equals__O__Z = (function(x$1) {
  return $m_sci_StringOps$().equals$extension__T__O__Z(this.repr$1, x$1)
});
$c_sci_StringOps.prototype.mkString__T__T__T__T = (function(start, sep, end) {
  return $f_sc_TraversableOnce__mkString__T__T__T__T(this, start, sep, end)
});
$c_sci_StringOps.prototype.toString__T = (function() {
  var $$this = this.repr$1;
  return $$this
});
$c_sci_StringOps.prototype.foreach__F1__V = (function(f) {
  $f_sc_IndexedSeqOptimized__foreach__F1__V(this, f)
});
$c_sci_StringOps.prototype.compare__O__I = (function(that) {
  var other = $as_T(that);
  var $$this = this.repr$1;
  return (($$this === other) ? 0 : ($uZ(($$this < other)) ? (-1) : 1))
});
$c_sci_StringOps.prototype.iterator__sc_Iterator = (function() {
  var $$this = this.repr$1;
  return new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, $uI($$this.length))
});
$c_sci_StringOps.prototype.length__I = (function() {
  var $$this = this.repr$1;
  return $uI($$this.length)
});
$c_sci_StringOps.prototype.addString__scm_StringBuilder__T__T__T__scm_StringBuilder = (function(b, start, sep, end) {
  return $f_sc_TraversableOnce__addString__scm_StringBuilder__T__T__T__scm_StringBuilder(this, b, start, sep, end)
});
$c_sci_StringOps.prototype.repr__O = (function() {
  return this.repr$1
});
$c_sci_StringOps.prototype.hashCode__I = (function() {
  var $$this = this.repr$1;
  return $m_sjsr_RuntimeString$().hashCode__T__I($$this)
});
$c_sci_StringOps.prototype.init___T = (function(repr) {
  this.repr$1 = repr;
  return this
});
$c_sci_StringOps.prototype.stringPrefix__T = (function() {
  return $f_sc_TraversableLike__stringPrefix__T(this)
});
function $is_sci_StringOps(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_StringOps)))
}
function $as_sci_StringOps(obj) {
  return (($is_sci_StringOps(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.StringOps"))
}
function $isArrayOf_sci_StringOps(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_StringOps)))
}
function $asArrayOf_sci_StringOps(obj, depth) {
  return (($isArrayOf_sci_StringOps(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.StringOps;", depth))
}
var $d_sci_StringOps = new $TypeData().initClass({
  sci_StringOps: 0
}, false, "scala.collection.immutable.StringOps", {
  sci_StringOps: 1,
  O: 1,
  sci_StringLike: 1,
  sc_IndexedSeqOptimized: 1,
  sc_IndexedSeqLike: 1,
  sc_SeqLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenIterableLike: 1,
  sc_GenSeqLike: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1
});
$c_sci_StringOps.prototype.$classData = $d_sci_StringOps;
function $is_sc_IndexedSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_IndexedSeq)))
}
function $as_sc_IndexedSeq(obj) {
  return (($is_sc_IndexedSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.IndexedSeq"))
}
function $isArrayOf_sc_IndexedSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_IndexedSeq)))
}
function $asArrayOf_sc_IndexedSeq(obj, depth) {
  return (($isArrayOf_sc_IndexedSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.IndexedSeq;", depth))
}
function $is_sc_LinearSeq(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sc_LinearSeq)))
}
function $as_sc_LinearSeq(obj) {
  return (($is_sc_LinearSeq(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.LinearSeq"))
}
function $isArrayOf_sc_LinearSeq(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sc_LinearSeq)))
}
function $asArrayOf_sc_LinearSeq(obj, depth) {
  return (($isArrayOf_sc_LinearSeq(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.LinearSeq;", depth))
}
/** @constructor */
function $c_sc_AbstractSeq() {
  $c_sc_AbstractIterable.call(this)
}
$c_sc_AbstractSeq.prototype = new $h_sc_AbstractIterable();
$c_sc_AbstractSeq.prototype.constructor = $c_sc_AbstractSeq;
/** @constructor */
function $h_sc_AbstractSeq() {
  /*<skip>*/
}
$h_sc_AbstractSeq.prototype = $c_sc_AbstractSeq.prototype;
$c_sc_AbstractSeq.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_SeqLike__lengthCompare__I__I(this, len)
});
$c_sc_AbstractSeq.prototype.equals__O__Z = (function(that) {
  return $f_sc_GenSeqLike__equals__O__Z(this, that)
});
$c_sc_AbstractSeq.prototype.isEmpty__Z = (function() {
  return $f_sc_SeqLike__isEmpty__Z(this)
});
$c_sc_AbstractSeq.prototype.toString__T = (function() {
  return $f_sc_TraversableLike__toString__T(this)
});
/** @constructor */
function $c_scm_AbstractSeq() {
  $c_sc_AbstractSeq.call(this)
}
$c_scm_AbstractSeq.prototype = new $h_sc_AbstractSeq();
$c_scm_AbstractSeq.prototype.constructor = $c_scm_AbstractSeq;
/** @constructor */
function $h_scm_AbstractSeq() {
  /*<skip>*/
}
$h_scm_AbstractSeq.prototype = $c_scm_AbstractSeq.prototype;
/** @constructor */
function $c_sci_Range() {
  $c_sc_AbstractSeq.call(this);
  this.start$4 = 0;
  this.end$4 = 0;
  this.step$4 = 0;
  this.isEmpty$4 = false;
  this.scala$collection$immutable$Range$$numRangeElements$4 = 0;
  this.scala$collection$immutable$Range$$lastElement$4 = 0
}
$c_sci_Range.prototype = new $h_sc_AbstractSeq();
$c_sci_Range.prototype.constructor = $c_sci_Range;
/** @constructor */
function $h_sci_Range() {
  /*<skip>*/
}
$h_sci_Range.prototype = $c_sci_Range.prototype;
$c_sci_Range.prototype.isInclusive__Z = (function() {
  return false
});
$c_sci_Range.prototype.apply__I__O = (function(idx) {
  return this.apply$mcII$sp__I__I(idx)
});
$c_sci_Range.prototype.apply__O__O = (function(v1) {
  var idx = $uI(v1);
  return this.apply$mcII$sp__I__I(idx)
});
$c_sci_Range.prototype.isEmpty__Z = (function() {
  return this.isEmpty$4
});
$c_sci_Range.prototype.longLength__p4__J = (function() {
  var t = this.gap__p4__J();
  var lo = t.lo$2;
  var hi$1 = t.hi$2;
  var value = this.step$4;
  var hi = (value >> 31);
  var this$1 = $m_sjsr_RuntimeLong$();
  var lo$1 = this$1.divideImpl__I__I__I__I__I(lo, hi$1, value, hi);
  var hi$2 = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  var value$1 = (this.hasStub__p4__Z() ? 1 : 0);
  var hi$3 = (value$1 >> 31);
  var lo$2 = ((lo$1 + value$1) | 0);
  var hi$4 = ((((-2147483648) ^ lo$2) < ((-2147483648) ^ lo$1)) ? ((1 + ((hi$2 + hi$3) | 0)) | 0) : ((hi$2 + hi$3) | 0));
  return new $c_sjsr_RuntimeLong().init___I__I(lo$2, hi$4)
});
$c_sci_Range.prototype.equals__O__Z = (function(other) {
  if ($is_sci_Range(other)) {
    var x2 = $as_sci_Range(other);
    if (this.isEmpty$4) {
      return x2.isEmpty$4
    } else if (($f_sc_TraversableOnce__nonEmpty__Z(x2) && (this.start$4 === x2.start$4))) {
      var l0 = this.last__I();
      return ((l0 === x2.last__I()) && ((this.start$4 === l0) || (this.step$4 === x2.step$4)))
    } else {
      return false
    }
  } else {
    return $f_sc_GenSeqLike__equals__O__Z(this, other)
  }
});
$c_sci_Range.prototype.apply$mcII$sp__I__I = (function(idx) {
  this.scala$collection$immutable$Range$$validateMaxLength__V();
  if (((idx < 0) || (idx >= this.scala$collection$immutable$Range$$numRangeElements$4))) {
    throw new $c_jl_IndexOutOfBoundsException().init___T(("" + idx))
  } else {
    return ((this.start$4 + $imul(this.step$4, idx)) | 0)
  }
});
$c_sci_Range.prototype.init___I__I__I = (function(start, end, step) {
  this.start$4 = start;
  this.end$4 = end;
  this.step$4 = step;
  this.isEmpty$4 = ((((start > end) && (step > 0)) || ((start < end) && (step < 0))) || ((start === end) && (!this.isInclusive__Z())));
  if ((step === 0)) {
    var jsx$1;
    throw new $c_jl_IllegalArgumentException().init___T("step cannot be 0.")
  } else if (this.isEmpty$4) {
    var jsx$1 = 0
  } else {
    var t = this.longLength__p4__J();
    var lo = t.lo$2;
    var hi = t.hi$2;
    var jsx$1 = (((hi === 0) ? (((-2147483648) ^ lo) > (-1)) : (hi > 0)) ? (-1) : lo)
  };
  this.scala$collection$immutable$Range$$numRangeElements$4 = jsx$1;
  switch (step) {
    case 1: {
      var jsx$2 = (this.isInclusive__Z() ? end : (((-1) + end) | 0));
      break
    }
    case (-1): {
      var jsx$2 = (this.isInclusive__Z() ? end : ((1 + end) | 0));
      break
    }
    default: {
      var t$1 = this.gap__p4__J();
      var lo$1 = t$1.lo$2;
      var hi$2 = t$1.hi$2;
      var hi$1 = (step >> 31);
      var this$1 = $m_sjsr_RuntimeLong$();
      var lo$2 = this$1.remainderImpl__I__I__I__I__I(lo$1, hi$2, step, hi$1);
      var jsx$2 = ((lo$2 !== 0) ? ((end - lo$2) | 0) : (this.isInclusive__Z() ? end : ((end - step) | 0)))
    }
  };
  this.scala$collection$immutable$Range$$lastElement$4 = jsx$2;
  return this
});
$c_sci_Range.prototype.toString__T = (function() {
  var preposition = (this.isInclusive__Z() ? "to" : "until");
  var stepped = ((this.step$4 === 1) ? "" : new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array([" by ", ""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([this.step$4])));
  var prefix = (this.isEmpty$4 ? "empty " : ((!this.isExact__p4__Z()) ? "inexact " : ""));
  return new $c_s_StringContext().init___sc_Seq(new $c_sjs_js_WrappedArray().init___sjs_js_Array(["", "Range ", " ", " ", "", ""])).s__sc_Seq__T(new $c_sjs_js_WrappedArray().init___sjs_js_Array([prefix, this.start$4, preposition, this.end$4, stepped]))
});
$c_sci_Range.prototype.foreach__F1__V = (function(f) {
  if ((!this.isEmpty$4)) {
    var i = this.start$4;
    while (true) {
      f.apply__O__O(i);
      if ((i === this.scala$collection$immutable$Range$$lastElement$4)) {
        return (void 0)
      };
      i = ((i + this.step$4) | 0)
    }
  }
});
$c_sci_Range.prototype.hasStub__p4__Z = (function() {
  return (this.isInclusive__Z() || (!this.isExact__p4__Z()))
});
$c_sci_Range.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, this.length__I())
});
$c_sci_Range.prototype.scala$collection$immutable$Range$$validateMaxLength__V = (function() {
  if ((this.scala$collection$immutable$Range$$numRangeElements$4 < 0)) {
    $m_sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(this.start$4, this.end$4, this.step$4, this.isInclusive__Z())
  }
});
$c_sci_Range.prototype.length__I = (function() {
  return ((this.scala$collection$immutable$Range$$numRangeElements$4 < 0) ? $m_sci_Range$().scala$collection$immutable$Range$$fail__I__I__I__Z__sr_Nothing$(this.start$4, this.end$4, this.step$4, this.isInclusive__Z()) : this.scala$collection$immutable$Range$$numRangeElements$4)
});
$c_sci_Range.prototype.isExact__p4__Z = (function() {
  var t = this.gap__p4__J();
  var lo = t.lo$2;
  var hi$1 = t.hi$2;
  var value = this.step$4;
  var hi = (value >> 31);
  var this$1 = $m_sjsr_RuntimeLong$();
  var lo$1 = this$1.remainderImpl__I__I__I__I__I(lo, hi$1, value, hi);
  var hi$2 = this$1.scala$scalajs$runtime$RuntimeLong$$hiReturn$f;
  return ((lo$1 === 0) && (hi$2 === 0))
});
$c_sci_Range.prototype.last__I = (function() {
  if (this.isEmpty$4) {
    var this$1 = $m_sci_Nil$();
    return $uI($f_sc_LinearSeqOptimized__last__O(this$1))
  } else {
    return this.scala$collection$immutable$Range$$lastElement$4
  }
});
$c_sci_Range.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
$c_sci_Range.prototype.gap__p4__J = (function() {
  var value = this.end$4;
  var hi = (value >> 31);
  var value$1 = this.start$4;
  var hi$1 = (value$1 >> 31);
  var lo = ((value - value$1) | 0);
  var hi$2 = ((((-2147483648) ^ lo) > ((-2147483648) ^ value)) ? (((-1) + ((hi - hi$1) | 0)) | 0) : ((hi - hi$1) | 0));
  return new $c_sjsr_RuntimeLong().init___I__I(lo, hi$2)
});
function $is_sci_Range(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_Range)))
}
function $as_sci_Range(obj) {
  return (($is_sci_Range(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.Range"))
}
function $isArrayOf_sci_Range(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_Range)))
}
function $asArrayOf_sci_Range(obj, depth) {
  return (($isArrayOf_sci_Range(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.Range;", depth))
}
var $d_sci_Range = new $TypeData().initClass({
  sci_Range: 0
}, false, "scala.collection.immutable.Range", {
  sci_Range: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_IndexedSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  sc_CustomParallelizable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_Range.prototype.$classData = $d_sci_Range;
/** @constructor */
function $c_sci_List() {
  $c_sc_AbstractSeq.call(this)
}
$c_sci_List.prototype = new $h_sc_AbstractSeq();
$c_sci_List.prototype.constructor = $c_sci_List;
/** @constructor */
function $h_sci_List() {
  /*<skip>*/
}
$h_sci_List.prototype = $c_sci_List.prototype;
$c_sci_List.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_LinearSeqOptimized__lengthCompare__I__I(this, len)
});
$c_sci_List.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return $f_sc_LinearSeqOptimized__sameElements__sc_GenIterable__Z(this, that)
});
$c_sci_List.prototype.apply__O__O = (function(v1) {
  var n = $uI(v1);
  return $f_sc_LinearSeqOptimized__apply__I__O(this, n)
});
$c_sci_List.prototype.drop__I__sc_LinearSeqOptimized = (function(n) {
  return this.drop__I__sci_List(n)
});
$c_sci_List.prototype.foreach__F1__V = (function(f) {
  var these = this;
  while ((!these.isEmpty__Z())) {
    f.apply__O__O(these.head__O());
    var this$1 = these;
    these = this$1.tail__sci_List()
  }
});
$c_sci_List.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_LinearSeqLike$$anon$1().init___sc_LinearSeqLike(this)
});
$c_sci_List.prototype.drop__I__sci_List = (function(n) {
  var these = this;
  var count = n;
  while (((!these.isEmpty__Z()) && (count > 0))) {
    var this$1 = these;
    these = this$1.tail__sci_List();
    count = (((-1) + count) | 0)
  };
  return these
});
$c_sci_List.prototype.length__I = (function() {
  return $f_sc_LinearSeqOptimized__length__I(this)
});
$c_sci_List.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
$c_sci_List.prototype.stringPrefix__T = (function() {
  return "List"
});
function $is_sci_List(obj) {
  return (!(!((obj && obj.$classData) && obj.$classData.ancestors.sci_List)))
}
function $as_sci_List(obj) {
  return (($is_sci_List(obj) || (obj === null)) ? obj : $throwClassCastException(obj, "scala.collection.immutable.List"))
}
function $isArrayOf_sci_List(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.sci_List)))
}
function $asArrayOf_sci_List(obj, depth) {
  return (($isArrayOf_sci_List(obj, depth) || (obj === null)) ? obj : $throwArrayCastException(obj, "Lscala.collection.immutable.List;", depth))
}
/** @constructor */
function $c_sci_Vector() {
  $c_sc_AbstractSeq.call(this);
  this.startIndex$4 = 0;
  this.endIndex$4 = 0;
  this.focus$4 = 0;
  this.dirty$4 = false;
  this.depth$4 = 0;
  this.display0$4 = null;
  this.display1$4 = null;
  this.display2$4 = null;
  this.display3$4 = null;
  this.display4$4 = null;
  this.display5$4 = null
}
$c_sci_Vector.prototype = new $h_sc_AbstractSeq();
$c_sci_Vector.prototype.constructor = $c_sci_Vector;
/** @constructor */
function $h_sci_Vector() {
  /*<skip>*/
}
$h_sci_Vector.prototype = $c_sci_Vector.prototype;
$c_sci_Vector.prototype.checkRangeConvert__p4__I__I = (function(index) {
  var idx = ((index + this.startIndex$4) | 0);
  if (((index >= 0) && (idx < this.endIndex$4))) {
    return idx
  } else {
    throw new $c_jl_IndexOutOfBoundsException().init___T(("" + index))
  }
});
$c_sci_Vector.prototype.display3__AO = (function() {
  return this.display3$4
});
$c_sci_Vector.prototype.apply__I__O = (function(index) {
  var idx = this.checkRangeConvert__p4__I__I(index);
  var xor = (idx ^ this.focus$4);
  return $f_sci_VectorPointer__getElem__I__I__O(this, idx, xor)
});
$c_sci_Vector.prototype.lengthCompare__I__I = (function(len) {
  return ((this.length__I() - len) | 0)
});
$c_sci_Vector.prototype.depth__I = (function() {
  return this.depth$4
});
$c_sci_Vector.prototype.apply__O__O = (function(v1) {
  return this.apply__I__O($uI(v1))
});
$c_sci_Vector.prototype.initIterator__sci_VectorIterator__V = (function(s) {
  var depth = this.depth$4;
  $f_sci_VectorPointer__initFrom__sci_VectorPointer__I__V(s, this, depth);
  if (this.dirty$4) {
    var index = this.focus$4;
    $f_sci_VectorPointer__stabilize__I__V(s, index)
  };
  if ((s.depth$2 > 1)) {
    var index$1 = this.startIndex$4;
    var xor = (this.startIndex$4 ^ this.focus$4);
    $f_sci_VectorPointer__gotoPos__I__I__V(s, index$1, xor)
  }
});
$c_sci_Vector.prototype.init___I__I__I = (function(startIndex, endIndex, focus) {
  this.startIndex$4 = startIndex;
  this.endIndex$4 = endIndex;
  this.focus$4 = focus;
  this.dirty$4 = false;
  return this
});
$c_sci_Vector.prototype.display5$und$eq__AO__V = (function(x$1) {
  this.display5$4 = x$1
});
$c_sci_Vector.prototype.display0__AO = (function() {
  return this.display0$4
});
$c_sci_Vector.prototype.display2$und$eq__AO__V = (function(x$1) {
  this.display2$4 = x$1
});
$c_sci_Vector.prototype.display4__AO = (function() {
  return this.display4$4
});
$c_sci_Vector.prototype.iterator__sc_Iterator = (function() {
  return this.iterator__sci_VectorIterator()
});
$c_sci_Vector.prototype.display1$und$eq__AO__V = (function(x$1) {
  this.display1$4 = x$1
});
$c_sci_Vector.prototype.length__I = (function() {
  return ((this.endIndex$4 - this.startIndex$4) | 0)
});
$c_sci_Vector.prototype.display4$und$eq__AO__V = (function(x$1) {
  this.display4$4 = x$1
});
$c_sci_Vector.prototype.display1__AO = (function() {
  return this.display1$4
});
$c_sci_Vector.prototype.display5__AO = (function() {
  return this.display5$4
});
$c_sci_Vector.prototype.iterator__sci_VectorIterator = (function() {
  var s = new $c_sci_VectorIterator().init___I__I(this.startIndex$4, this.endIndex$4);
  this.initIterator__sci_VectorIterator__V(s);
  return s
});
$c_sci_Vector.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
$c_sci_Vector.prototype.depth$und$eq__I__V = (function(x$1) {
  this.depth$4 = x$1
});
$c_sci_Vector.prototype.display2__AO = (function() {
  return this.display2$4
});
$c_sci_Vector.prototype.display0$und$eq__AO__V = (function(x$1) {
  this.display0$4 = x$1
});
$c_sci_Vector.prototype.display3$und$eq__AO__V = (function(x$1) {
  this.display3$4 = x$1
});
var $d_sci_Vector = new $TypeData().initClass({
  sci_Vector: 0
}, false, "scala.collection.immutable.Vector", {
  sci_Vector: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_IndexedSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  sci_VectorPointer: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1,
  sc_CustomParallelizable: 1
});
$c_sci_Vector.prototype.$classData = $d_sci_Vector;
/** @constructor */
function $c_sci_Nil$() {
  $c_sci_List.call(this)
}
$c_sci_Nil$.prototype = new $h_sci_List();
$c_sci_Nil$.prototype.constructor = $c_sci_Nil$;
/** @constructor */
function $h_sci_Nil$() {
  /*<skip>*/
}
$h_sci_Nil$.prototype = $c_sci_Nil$.prototype;
$c_sci_Nil$.prototype.productPrefix__T = (function() {
  return "Nil"
});
$c_sci_Nil$.prototype.head__O = (function() {
  this.head__sr_Nothing$()
});
$c_sci_Nil$.prototype.init___ = (function() {
  return this
});
$c_sci_Nil$.prototype.productArity__I = (function() {
  return 0
});
$c_sci_Nil$.prototype.isEmpty__Z = (function() {
  return true
});
$c_sci_Nil$.prototype.tail__sci_List = (function() {
  throw new $c_jl_UnsupportedOperationException().init___T("tail of empty list")
});
$c_sci_Nil$.prototype.equals__O__Z = (function(that) {
  if ($is_sc_GenSeq(that)) {
    var x2 = $as_sc_GenSeq(that);
    return x2.isEmpty__Z()
  } else {
    return false
  }
});
$c_sci_Nil$.prototype.productElement__I__O = (function(x$1) {
  throw new $c_jl_IndexOutOfBoundsException().init___T(("" + x$1))
});
$c_sci_Nil$.prototype.head__sr_Nothing$ = (function() {
  throw new $c_ju_NoSuchElementException().init___T("head of empty list")
});
$c_sci_Nil$.prototype.tail__O = (function() {
  return this.tail__sci_List()
});
$c_sci_Nil$.prototype.productIterator__sc_Iterator = (function() {
  return new $c_sr_ScalaRunTime$$anon$1().init___s_Product(this)
});
var $d_sci_Nil$ = new $TypeData().initClass({
  sci_Nil$: 0
}, false, "scala.collection.immutable.Nil$", {
  sci_Nil$: 1,
  sci_List: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  sci_LinearSeq: 1,
  sci_Seq: 1,
  sci_Iterable: 1,
  sci_Traversable: 1,
  s_Immutable: 1,
  sc_LinearSeq: 1,
  sc_LinearSeqLike: 1,
  s_Product: 1,
  sc_LinearSeqOptimized: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_sci_Nil$.prototype.$classData = $d_sci_Nil$;
var $n_sci_Nil$ = (void 0);
function $m_sci_Nil$() {
  if ((!$n_sci_Nil$)) {
    $n_sci_Nil$ = new $c_sci_Nil$().init___()
  };
  return $n_sci_Nil$
}
/** @constructor */
function $c_scm_AbstractBuffer() {
  $c_scm_AbstractSeq.call(this)
}
$c_scm_AbstractBuffer.prototype = new $h_scm_AbstractSeq();
$c_scm_AbstractBuffer.prototype.constructor = $c_scm_AbstractBuffer;
/** @constructor */
function $h_scm_AbstractBuffer() {
  /*<skip>*/
}
$h_scm_AbstractBuffer.prototype = $c_scm_AbstractBuffer.prototype;
/** @constructor */
function $c_scm_StringBuilder() {
  $c_scm_AbstractSeq.call(this);
  this.underlying$5 = null
}
$c_scm_StringBuilder.prototype = new $h_scm_AbstractSeq();
$c_scm_StringBuilder.prototype.constructor = $c_scm_StringBuilder;
/** @constructor */
function $h_scm_StringBuilder() {
  /*<skip>*/
}
$h_scm_StringBuilder.prototype = $c_scm_StringBuilder.prototype;
$c_scm_StringBuilder.prototype.init___ = (function() {
  $c_scm_StringBuilder.prototype.init___I__T.call(this, 16, "");
  return this
});
$c_scm_StringBuilder.prototype.apply__I__O = (function(idx) {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  var c = (65535 & $uI(thiz.charCodeAt(idx)));
  return new $c_jl_Character().init___C(c)
});
$c_scm_StringBuilder.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_IndexedSeqOptimized__lengthCompare__I__I(this, len)
});
$c_scm_StringBuilder.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return $f_sc_IndexedSeqOptimized__sameElements__sc_GenIterable__Z(this, that)
});
$c_scm_StringBuilder.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  var c = (65535 & $uI(thiz.charCodeAt(index)));
  return new $c_jl_Character().init___C(c)
});
$c_scm_StringBuilder.prototype.isEmpty__Z = (function() {
  return $f_sc_IndexedSeqOptimized__isEmpty__Z(this)
});
$c_scm_StringBuilder.prototype.subSequence__I__I__jl_CharSequence = (function(start, end) {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return $as_T(thiz.substring(start, end))
});
$c_scm_StringBuilder.prototype.toString__T = (function() {
  var this$1 = this.underlying$5;
  return this$1.content$1
});
$c_scm_StringBuilder.prototype.foreach__F1__V = (function(f) {
  $f_sc_IndexedSeqOptimized__foreach__F1__V(this, f)
});
$c_scm_StringBuilder.prototype.compare__O__I = (function(that) {
  var other = $as_T(that);
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return ((thiz === other) ? 0 : ($uZ((thiz < other)) ? (-1) : 1))
});
$c_scm_StringBuilder.prototype.append__T__scm_StringBuilder = (function(s) {
  this.underlying$5.append__T__jl_StringBuilder(s);
  return this
});
$c_scm_StringBuilder.prototype.iterator__sc_Iterator = (function() {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, $uI(thiz.length))
});
$c_scm_StringBuilder.prototype.init___I__T = (function(initCapacity, initValue) {
  $c_scm_StringBuilder.prototype.init___jl_StringBuilder.call(this, new $c_jl_StringBuilder().init___I((($uI(initValue.length) + initCapacity) | 0)).append__T__jl_StringBuilder(initValue));
  return this
});
$c_scm_StringBuilder.prototype.length__I = (function() {
  var this$1 = this.underlying$5;
  var thiz = this$1.content$1;
  return $uI(thiz.length)
});
$c_scm_StringBuilder.prototype.init___jl_StringBuilder = (function(underlying) {
  this.underlying$5 = underlying;
  return this
});
$c_scm_StringBuilder.prototype.append__O__scm_StringBuilder = (function(x) {
  this.underlying$5.append__T__jl_StringBuilder($m_sjsr_RuntimeString$().valueOf__O__T(x));
  return this
});
$c_scm_StringBuilder.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
var $d_scm_StringBuilder = new $TypeData().initClass({
  scm_StringBuilder: 0
}, false, "scala.collection.mutable.StringBuilder", {
  scm_StringBuilder: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  jl_CharSequence: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  scm_IndexedSeqLike: 1,
  sci_StringLike: 1,
  sc_IndexedSeqOptimized: 1,
  s_math_Ordered: 1,
  jl_Comparable: 1,
  scm_ReusableBuilder: 1,
  scm_Builder: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  s_Serializable: 1,
  Ljava_io_Serializable: 1
});
$c_scm_StringBuilder.prototype.$classData = $d_scm_StringBuilder;
/** @constructor */
function $c_sjs_js_WrappedArray() {
  $c_scm_AbstractBuffer.call(this);
  this.array$6 = null
}
$c_sjs_js_WrappedArray.prototype = new $h_scm_AbstractBuffer();
$c_sjs_js_WrappedArray.prototype.constructor = $c_sjs_js_WrappedArray;
/** @constructor */
function $h_sjs_js_WrappedArray() {
  /*<skip>*/
}
$h_sjs_js_WrappedArray.prototype = $c_sjs_js_WrappedArray.prototype;
$c_sjs_js_WrappedArray.prototype.apply__I__O = (function(index) {
  return this.array$6[index]
});
$c_sjs_js_WrappedArray.prototype.lengthCompare__I__I = (function(len) {
  return $f_sc_IndexedSeqOptimized__lengthCompare__I__I(this, len)
});
$c_sjs_js_WrappedArray.prototype.apply__O__O = (function(v1) {
  var index = $uI(v1);
  return this.array$6[index]
});
$c_sjs_js_WrappedArray.prototype.sameElements__sc_GenIterable__Z = (function(that) {
  return $f_sc_IndexedSeqOptimized__sameElements__sc_GenIterable__Z(this, that)
});
$c_sjs_js_WrappedArray.prototype.isEmpty__Z = (function() {
  return $f_sc_IndexedSeqOptimized__isEmpty__Z(this)
});
$c_sjs_js_WrappedArray.prototype.foreach__F1__V = (function(f) {
  $f_sc_IndexedSeqOptimized__foreach__F1__V(this, f)
});
$c_sjs_js_WrappedArray.prototype.iterator__sc_Iterator = (function() {
  return new $c_sc_IndexedSeqLike$Elements().init___sc_IndexedSeqLike__I__I(this, 0, $uI(this.array$6.length))
});
$c_sjs_js_WrappedArray.prototype.length__I = (function() {
  return $uI(this.array$6.length)
});
$c_sjs_js_WrappedArray.prototype.hashCode__I = (function() {
  return $m_s_util_hashing_MurmurHash3$().seqHash__sc_Seq__I(this)
});
$c_sjs_js_WrappedArray.prototype.init___sjs_js_Array = (function(array) {
  this.array$6 = array;
  return this
});
$c_sjs_js_WrappedArray.prototype.stringPrefix__T = (function() {
  return "WrappedArray"
});
var $d_sjs_js_WrappedArray = new $TypeData().initClass({
  sjs_js_WrappedArray: 0
}, false, "scala.scalajs.js.WrappedArray", {
  sjs_js_WrappedArray: 1,
  scm_AbstractBuffer: 1,
  scm_AbstractSeq: 1,
  sc_AbstractSeq: 1,
  sc_AbstractIterable: 1,
  sc_AbstractTraversable: 1,
  O: 1,
  sc_Traversable: 1,
  sc_TraversableLike: 1,
  scg_HasNewBuilder: 1,
  scg_FilterMonadic: 1,
  sc_TraversableOnce: 1,
  sc_GenTraversableOnce: 1,
  sc_GenTraversableLike: 1,
  sc_Parallelizable: 1,
  sc_GenTraversable: 1,
  scg_GenericTraversableTemplate: 1,
  sc_Iterable: 1,
  sc_GenIterable: 1,
  sc_GenIterableLike: 1,
  sc_IterableLike: 1,
  s_Equals: 1,
  sc_Seq: 1,
  s_PartialFunction: 1,
  F1: 1,
  sc_GenSeq: 1,
  sc_GenSeqLike: 1,
  sc_SeqLike: 1,
  scm_Seq: 1,
  scm_Iterable: 1,
  scm_Traversable: 1,
  s_Mutable: 1,
  scm_SeqLike: 1,
  scm_Cloneable: 1,
  s_Cloneable: 1,
  jl_Cloneable: 1,
  scm_Buffer: 1,
  scm_BufferLike: 1,
  scg_Growable: 1,
  scg_Clearable: 1,
  scg_Shrinkable: 1,
  sc_script_Scriptable: 1,
  scg_Subtractable: 1,
  scm_IndexedSeq: 1,
  sc_IndexedSeq: 1,
  sc_IndexedSeqLike: 1,
  scm_IndexedSeqLike: 1,
  scm_ArrayLike: 1,
  scm_IndexedSeqOptimized: 1,
  sc_IndexedSeqOptimized: 1,
  scm_Builder: 1
});
$c_sjs_js_WrappedArray.prototype.$classData = $d_sjs_js_WrappedArray;
$e.co = ($e.co || {});
$e.co.Topl = ($e.co.Topl || {});
$e.co.Topl.Loan = $a_LLoan();
}.call(this);
//# sourceMappingURL=contract-modules-fastopt.js.map