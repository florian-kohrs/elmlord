(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $author$project$Entities$Cons = F2(
	function (a, b) {
		return {$: 'Cons', a: a, b: b};
	});
var $author$project$Entities$Model$Lord = F4(
	function (entity, gold, land, agent) {
		return {agent: agent, entity: entity, gold: gold, land: land};
	});
var $author$project$Entities$Model$WorldEntity = F4(
	function (army, faction, position, name) {
		return {army: army, faction: faction, name: name, position: position};
	});
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $author$project$PathAgent$getAgent = function (speed) {
	return {speed: speed, target: $elm$core$Maybe$Nothing, usedMovement: 0.0};
};
var $author$project$Vector$Point = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $author$project$Vector$addPoints = F2(
	function (p1, p2) {
		return A2($author$project$Vector$Point, p1.x + p2.x, p1.y + p2.y);
	});
var $author$project$MapData$mapSize = 15;
var $author$project$MapData$mapHeight = $author$project$MapData$mapSize;
var $author$project$MapData$mapWidth = $author$project$MapData$mapSize;
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $author$project$MapData$hashMapPoint = function (p) {
	var shiftedP = A2(
		$author$project$Vector$addPoints,
		p,
		A2($author$project$Vector$Point, $author$project$MapData$mapWidth, $author$project$MapData$mapHeight));
	return (shiftedP.x << 16) + shiftedP.y;
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $author$project$Pathfinder$addCircumjacentToDict = $elm$core$List$foldl(
	F2(
		function (p, dict2) {
			return A3(
				$elm$core$Dict$insert,
				$author$project$MapData$hashMapPoint(p),
				_Utils_Tuple0,
				dict2);
		}));
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$MaybeExt$hasValue = function (m) {
	if (m.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $author$project$ListExt$insertToSortedList = F3(
	function (a, f, xs) {
		if (!xs.b) {
			return _List_fromArray(
				[a]);
		} else {
			var x = xs.a;
			var xs2 = xs.b;
			return (_Utils_cmp(
				f(a),
				f(x)) < 1) ? A2(
				$elm$core$List$cons,
				a,
				A2($elm$core$List$cons, x, xs2)) : A2(
				$elm$core$List$cons,
				x,
				A3($author$project$ListExt$insertToSortedList, a, f, xs2));
		}
	});
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$member = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$get, key, dict);
		if (_v0.$ === 'Just') {
			return true;
		} else {
			return false;
		}
	});
var $elm$core$Basics$not = _Basics_not;
var $author$project$Pathfinder$getClosestFreeFieldAt_ = F6(
	function (start, closest, tails, nav, invalidFields, usedFields) {
		getClosestFreeFieldAt_:
		while (true) {
			if ($author$project$MaybeExt$hasValue(
				nav.timeToCrossField(closest)) && (!A2(
				$elm$core$Dict$member,
				$author$project$MapData$hashMapPoint(closest),
				invalidFields))) {
				return $elm$core$Maybe$Just(closest);
			} else {
				var circumjacent = A2(
					$elm$core$List$filter,
					function (p) {
						return !A2(
							$elm$core$Dict$member,
							$author$project$MapData$hashMapPoint(p),
							usedFields);
					},
					A2(nav.getCircumjacentFields, closest, true));
				var tails2 = A3(
					$elm$core$List$foldl,
					F2(
						function (p, ts2) {
							return A3(
								$author$project$ListExt$insertToSortedList,
								p,
								nav.getMinDistanceBetween(start),
								ts2);
						}),
					tails,
					circumjacent);
				if (!tails2.b) {
					return $elm$core$Maybe$Nothing;
				} else {
					var x = tails2.a;
					var xs = tails2.b;
					var $temp$start = start,
						$temp$closest = x,
						$temp$tails = xs,
						$temp$nav = nav,
						$temp$invalidFields = invalidFields,
						$temp$usedFields = A2($author$project$Pathfinder$addCircumjacentToDict, usedFields, circumjacent);
					start = $temp$start;
					closest = $temp$closest;
					tails = $temp$tails;
					nav = $temp$nav;
					invalidFields = $temp$invalidFields;
					usedFields = $temp$usedFields;
					continue getClosestFreeFieldAt_;
				}
			}
		}
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Pathfinder$getClosestFreeFieldAt = F3(
	function (p, nav, invalidDict) {
		return A2(
			$elm$core$Maybe$withDefault,
			A2($author$project$Vector$Point, 0, 0),
			A6($author$project$Pathfinder$getClosestFreeFieldAt_, p, p, _List_Nil, nav, invalidDict, $elm$core$Dict$empty));
	});
var $author$project$Faction$Faction1 = {$: 'Faction1'};
var $author$project$Faction$Faction2 = {$: 'Faction2'};
var $author$project$Faction$Faction3 = {$: 'Faction3'};
var $author$project$Faction$Faction4 = {$: 'Faction4'};
var $author$project$Faction$getFaction = function (i) {
	return (!i) ? $author$project$Faction$Faction1 : ((i === 1) ? $author$project$Faction$Faction2 : ((i === 2) ? $author$project$Faction$Faction3 : $author$project$Faction$Faction4));
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $author$project$Map$Model$CanWalkOn = function (a) {
	return {$: 'CanWalkOn', a: a};
};
var $author$project$Map$Model$CantWalkOn = {$: 'CantWalkOn'};
var $author$project$Map$terrainToMove = function (t) {
	switch (t.$) {
		case 'Grass':
			return $author$project$Map$Model$CanWalkOn(1);
		case 'Water':
			return $author$project$Map$Model$CantWalkOn;
		case 'Forest':
			return $author$project$Map$Model$CanWalkOn(0.8);
		default:
			return $author$project$Map$Model$CanWalkOn(0.5);
	}
};
var $author$project$Map$canMoveOnTile = function (mapTile) {
	var _v0 = $author$project$Map$terrainToMove(mapTile.terrain);
	if (_v0.$ === 'CantWalkOn') {
		return false;
	} else {
		return true;
	}
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $elm$core$Basics$round = _Basics_round;
var $author$project$Pathfinder$getNav = function (map) {
	return {
		getCircumjacentFields: F2(
			function (p, useEveryTile) {
				var sign = (!A2($elm$core$Basics$modBy, 2, p.x)) ? 1 : (-1);
				var canUseTile = function (point) {
					var _v0 = A2(
						$elm$core$Dict$get,
						$author$project$MapData$hashMapPoint(point),
						map);
					if (_v0.$ === 'Nothing') {
						return false;
					} else {
						var t = _v0.a;
						return useEveryTile || $author$project$Map$canMoveOnTile(t);
					}
				};
				return A2(
					$elm$core$List$filter,
					function (point) {
						return (_Utils_cmp(
							$elm$core$Basics$abs(point.x),
							$author$project$MapData$mapSize) < 1) && ((_Utils_cmp(
							$elm$core$Basics$abs(point.y),
							$author$project$MapData$mapSize) < 1) && canUseTile(point));
					},
					_List_fromArray(
						[
							A2($author$project$Vector$Point, p.x, p.y + 1),
							A2($author$project$Vector$Point, p.x, p.y - 1),
							A2($author$project$Vector$Point, p.x + 1, p.y + sign),
							A2($author$project$Vector$Point, p.x - 1, p.y + sign),
							A2($author$project$Vector$Point, p.x + 1, p.y),
							A2($author$project$Vector$Point, p.x - 1, p.y)
						]));
			}),
		getMinDistanceBetween: F2(
			function (p1, p2) {
				var xDiff = $elm$core$Basics$abs(p1.x - p2.x);
				var yDiff = A2(
					$elm$core$Basics$max,
					0,
					($elm$core$Basics$abs(p1.y - p2.y) - (xDiff / 2)) - A2(
						$elm$core$Basics$modBy,
						2,
						$elm$core$Basics$round(xDiff)));
				return xDiff + yDiff;
			}),
		timeToCrossField: function (p) {
			var _v1 = A2(
				$elm$core$Dict$get,
				$author$project$MapData$hashMapPoint(p),
				map);
			if (_v1.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var t = _v1.a;
				var _v2 = $author$project$Map$terrainToMove(t.terrain);
				if (_v2.$ === 'CantWalkOn') {
					return $elm$core$Maybe$Nothing;
				} else {
					var speedFactor = _v2.a;
					return $elm$core$Maybe$Just(1 / speedFactor);
				}
			}
		}
	};
};
var $author$project$Entities$Model$castleNames = _List_fromArray(
	['Stathford', 'Wingston', 'Boroughton', 'Peterbrugh', 'Wimborne', 'Westwend', 'Kingcardine', 'Helmfirth', 'Accrington', 'Mournstead', 'Alcombey', 'Aeberuthey', 'Bradford', 'Bamborourgh', 'Everton']);
var $author$project$Entities$Model$Castle = {$: 'Castle'};
var $author$project$Troops$Archer = {$: 'Archer'};
var $author$project$Troops$Knight = {$: 'Knight'};
var $author$project$Troops$Spear = {$: 'Spear'};
var $author$project$Troops$Sword = {$: 'Sword'};
var $author$project$Troops$troopTypeList = _List_fromArray(
	[$author$project$Troops$Sword, $author$project$Troops$Spear, $author$project$Troops$Archer, $author$project$Troops$Knight]);
var $author$project$Troops$troopTypeToInt = function (t) {
	switch (t.$) {
		case 'Archer':
			return 0;
		case 'Spear':
			return 1;
		case 'Sword':
			return 2;
		default:
			return 3;
	}
};
var $author$project$Troops$emptyTroops = A3(
	$elm$core$List$foldl,
	F2(
		function (t, dict) {
			return A3(
				$elm$core$Dict$insert,
				$author$project$Troops$troopTypeToInt(t),
				0,
				dict);
		}),
	$elm$core$Dict$empty,
	$author$project$Troops$troopTypeList);
var $author$project$Building$Barracks = {$: 'Barracks'};
var $author$project$Building$Fortress = {$: 'Fortress'};
var $author$project$Building$Marketplace = {$: 'Marketplace'};
var $author$project$Building$startBuildings = _List_fromArray(
	[
		{buildingType: $author$project$Building$Marketplace, level: 0, name: 'Marketplace'},
		{buildingType: $author$project$Building$Barracks, level: 0, name: 'Barracks'},
		{buildingType: $author$project$Building$Fortress, level: 0, name: 'Fortress'}
	]);
var $author$project$Entities$createCapitalFor = F2(
	function (e, name) {
		return {
			buildings: $author$project$Building$startBuildings,
			entity: {army: $elm$core$Dict$empty, faction: e.faction, name: name, position: e.position},
			income: 5.0,
			isSieged: false,
			recruitLimits: $author$project$Troops$emptyTroops,
			settlementType: $author$project$Entities$Model$Castle
		};
	});
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$ListExt$getElementAt = F2(
	function (i, l) {
		return (i <= 0) ? $elm$core$List$head(l) : A2(
			$elm$core$Maybe$andThen,
			function (tail) {
				return A2($author$project$ListExt$getElementAt, i - 1, tail);
			},
			$elm$core$List$tail(l));
	});
var $author$project$Entities$editSettlmentInfoPosition = F2(
	function (p, i) {
		return _Utils_update(
			i,
			{position: p});
	});
var $author$project$Main$getSafeSettlementInfo = F3(
	function (i, m, dict) {
		return A2(
			$author$project$Entities$editSettlmentInfoPosition,
			A3(
				$author$project$Pathfinder$getClosestFreeFieldAt,
				i.position,
				$author$project$Pathfinder$getNav(m),
				dict),
			i);
	});
var $author$project$Main$getSafeSettlementInfos = F3(
	function (m, usedFields, infos) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (info, _v0) {
					var result = _v0.a;
					var usedFields2 = _v0.b;
					var newInfo = A3($author$project$Main$getSafeSettlementInfo, info, m, usedFields2);
					return _Utils_Tuple2(
						A2($elm$core$List$cons, newInfo, result),
						A3(
							$elm$core$Dict$insert,
							$author$project$MapData$hashMapPoint(newInfo.position),
							_Utils_Tuple0,
							usedFields2));
				}),
			_Utils_Tuple2(_List_Nil, usedFields),
			infos).a;
	});
var $author$project$Troops$startTroops = A3(
	$elm$core$List$foldl,
	F2(
		function (_v0, dict) {
			var t = _v0.a;
			var v = _v0.b;
			return A3(
				$elm$core$Dict$insert,
				$author$project$Troops$troopTypeToInt(t),
				v,
				dict);
		}),
	$elm$core$Dict$empty,
	_List_fromArray(
		[
			_Utils_Tuple2($author$project$Troops$Archer, 10),
			_Utils_Tuple2($author$project$Troops$Spear, 45),
			_Utils_Tuple2($author$project$Troops$Sword, 20),
			_Utils_Tuple2($author$project$Troops$Knight, 5)
		]));
var $author$project$Entities$getSettlementFor = function (info) {
	return {
		buildings: $author$project$Building$startBuildings,
		entity: {army: $author$project$Troops$startTroops, faction: info.faction, name: info.name, position: info.position},
		income: 1.5,
		isSieged: false,
		recruitLimits: $author$project$Troops$emptyTroops,
		settlementType: info.sType
	};
};
var $author$project$Entities$Model$SettlementInfo = F4(
	function (sType, position, name, faction) {
		return {faction: faction, name: name, position: position, sType: sType};
	});
var $author$project$Entities$Model$Village = {$: 'Village'};
var $author$project$Vector$Vector = F2(
	function (x, y) {
		return {x: x, y: y};
	});
var $elm$core$Basics$cos = _Basics_cos;
var $elm$core$Basics$pi = _Basics_pi;
var $elm$core$Basics$sin = _Basics_sin;
var $author$project$Vector$toPoint = function (v) {
	return {
		x: $elm$core$Basics$round(v.x),
		y: $elm$core$Basics$round(v.y)
	};
};
var $author$project$Main$villageCaptialDistance = 7;
var $author$project$Main$getVillagesPosition = F4(
	function (max, q, i, p) {
		var rad = (0.5 * $elm$core$Basics$pi) * ((i / max) + ((-q) + 2));
		var distanceFromCapital = $author$project$Main$villageCaptialDistance;
		var x = $elm$core$Basics$sin(rad) * distanceFromCapital;
		var y = $elm$core$Basics$cos(rad) * distanceFromCapital;
		return A2(
			$author$project$Vector$addPoints,
			p,
			$author$project$Vector$toPoint(
				A2($author$project$Vector$Vector, x, y)));
	});
var $author$project$Entities$Model$villageNames = _List_fromArray(
	['Haran', 'Hillfar', 'Waekefield', 'Sudbury', 'Murkwell', 'Caerfyrddin', 'Llanybydder', 'Galssop', 'Farnworth', 'Porthaethwy', 'Favorsham', 'Kilead', 'Kald', 'Holsworthy', 'Wolfwater', 'Southwold', 'Marnmouth', 'Kilmarnock', 'Far Water', 'Aylesbury', 'Dornwich', 'Haran', 'Murkwell', 'Drumnacanvy', 'Waeldestone', 'Bracklewhyte', 'Peatsland', 'Ballachulish', 'Arbington', 'Torrine']);
var $author$project$Main$getVillagesInQuadrant = F4(
	function (m, e, q, i) {
		return A2(
			$elm$core$List$map,
			function (index) {
				return A4(
					$author$project$Entities$Model$SettlementInfo,
					$author$project$Entities$Model$Village,
					A4($author$project$Main$getVillagesPosition, i, q, index, e.position),
					A2(
						$elm$core$Maybe$withDefault,
						e.name + (' ' + ($elm$core$String$fromInt(i) + 'th Village`')),
						A2($author$project$ListExt$getElementAt, (q * 4) + index, $author$project$Entities$Model$villageNames)),
					e.faction);
			},
			A2($elm$core$List$range, 1, i));
	});
var $author$project$Main$villagesPerLord = 3;
var $author$project$Main$initSettlementsFor = F4(
	function (m, usedFields, e, i) {
		return A2(
			$elm$core$List$cons,
			A2(
				$author$project$Entities$createCapitalFor,
				e,
				A2(
					$elm$core$Maybe$withDefault,
					e.name + '`s Capital`',
					A2($author$project$ListExt$getElementAt, i, $author$project$Entities$Model$castleNames))),
			A2(
				$elm$core$List$map,
				$author$project$Entities$getSettlementFor,
				A3(
					$author$project$Main$getSafeSettlementInfos,
					m,
					usedFields,
					A4($author$project$Main$getVillagesInQuadrant, m, e, i, $author$project$Main$villagesPerLord))));
	});
var $author$project$Vector$pointOnCircle = F2(
	function (radius, radiant) {
		var deltaY = $elm$core$Basics$sin($elm$core$Basics$pi * radiant) * radius;
		var deltaX = $elm$core$Basics$cos($elm$core$Basics$pi * radiant) * radius;
		return A2($author$project$Vector$Vector, deltaX, deltaY);
	});
var $author$project$Main$initPlayer = F3(
	function (m, i, rad) {
		var entity = A4(
			$author$project$Entities$Model$WorldEntity,
			$author$project$Troops$startTroops,
			$author$project$Faction$getFaction(i),
			A3(
				$author$project$Pathfinder$getClosestFreeFieldAt,
				$author$project$Vector$toPoint(
					A2($author$project$Vector$pointOnCircle, $author$project$MapData$mapSize * 1, rad)),
				$author$project$Pathfinder$getNav(m),
				$elm$core$Dict$empty),
			'Lord ' + $elm$core$String$fromInt(i));
		return A4(
			$author$project$Entities$Model$Lord,
			entity,
			250,
			A4($author$project$Main$initSettlementsFor, m, $elm$core$Dict$empty, entity, i),
			$author$project$PathAgent$getAgent(5));
	});
var $author$project$Main$testLordWorldEntity = {
	army: $author$project$Troops$startTroops,
	faction: $author$project$Faction$Faction1,
	name: 'RÜDIGER',
	position: {x: 0, y: 0}
};
var $author$project$Main$testWorldEntity = {
	army: $author$project$Troops$startTroops,
	faction: $author$project$Faction$Faction1,
	name: 'Malaca',
	position: {x: 0, y: 0}
};
var $author$project$Main$testSetelement = {buildings: $author$project$Building$startBuildings, entity: $author$project$Main$testWorldEntity, income: 3.19, isSieged: false, recruitLimits: $author$project$Troops$emptyTroops, settlementType: $author$project$Entities$Model$Castle};
var $author$project$Main$testLord = {
	agent: $author$project$PathAgent$getAgent(6),
	entity: $author$project$Main$testLordWorldEntity,
	gold: 10050,
	land: _List_fromArray(
		[$author$project$Main$testSetelement])
};
var $author$project$Main$initPlayers = F2(
	function (m, count) {
		var lords = A2(
			$elm$core$List$map,
			function (i) {
				return A3($author$project$Main$initPlayer, m.map, i, 2 * ((i / count) + 0.125));
			},
			A2($elm$core$List$range, 0, count - 1));
		return _Utils_update(
			m,
			{
				lords: A2($author$project$Entities$Cons, $author$project$Main$testLord, lords)
			});
	});
var $author$project$DateExt$Date = F2(
	function (year, month) {
		return {month: month, year: year};
	});
var $author$project$Main$GameSetup = function (a) {
	return {$: 'GameSetup', a: a};
};
var $author$project$DateExt$Jan = {$: 'Jan'};
var $author$project$Main$MainMenue = function (a) {
	return {$: 'MainMenue', a: a};
};
var $author$project$Main$Menue = {$: 'Menue'};
var $author$project$Main$Model = F7(
	function (lords, gameState, selectedPoint, date, map, errorMsg, event) {
		return {date: date, errorMsg: errorMsg, event: event, gameState: gameState, lords: lords, map: map, selectedPoint: selectedPoint};
	});
var $author$project$Map$Model$MapTile = F6(
	function (indices, point, terrain, settlement, lords, faction) {
		return {faction: faction, indices: indices, lords: lords, point: point, settlement: settlement, terrain: terrain};
	});
var $author$project$Map$Model$Forest = {$: 'Forest'};
var $author$project$Map$Model$Grass = {$: 'Grass'};
var $author$project$Map$Model$Mountain = {$: 'Mountain'};
var $author$project$Map$Model$Water = {$: 'Water'};
var $author$project$Map$heighProgressToTerrain = function (f) {
	return (f < 0.2) ? $author$project$Map$Model$Water : ((f < 0.5) ? $author$project$Map$Model$Grass : ((f < 0.85) ? $author$project$Map$Model$Forest : $author$project$Map$Model$Mountain));
};
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Basics$sqrt = _Basics_sqrt;
var $author$project$Noise$f2 = 0.5 * ($elm$core$Basics$sqrt(3) - 1);
var $author$project$Noise$g2 = (3 - $elm$core$Basics$sqrt(3)) / 6;
var $author$project$Noise$getCornerOffset2d = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? _Utils_Tuple2(1, 0) : _Utils_Tuple2(0, 1);
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $elm$core$Debug$todo = _Debug_todo;
var $author$project$Noise$get = F2(
	function (arr, i) {
		var _v0 = A2($elm$core$Array$get, i, arr);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return x;
		} else {
			return _Debug_todo(
				'Noise',
				{
					start: {line: 90, column: 13},
					end: {line: 90, column: 23}
				})('Error getting item');
		}
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Noise$grad3 = $elm$core$Array$fromList(
	_List_fromArray(
		[1, 1, 0, -1, 1, 0, 1, -1, 0, -1, -1, 0, 1, 0, 1, -1, 0, 1, 1, 0, -1, -1, 0, -1, 0, 1, 1, 0, -1, 1, 0, 1, -1, 0, -1, -1]));
var $author$project$Noise$getN2d = F6(
	function (x, y, i, j, perm, permMod12) {
		var t = (0.5 - (x * x)) - (y * y);
		if (t < 0) {
			return 0;
		} else {
			var tt = t * t;
			var gi = A2(
				$author$project$Noise$get,
				permMod12,
				i + A2($author$project$Noise$get, perm, j)) * 3;
			return (tt * tt) * ((A2($author$project$Noise$get, $author$project$Noise$grad3, gi) * x) + (A2($author$project$Noise$get, $author$project$Noise$grad3, gi + 1) * y));
		}
	});
var $author$project$Noise$noise2d = F3(
	function (_v0, xin, yin) {
		var perm = _v0.perm;
		var permMod12 = _v0.permMod12;
		var s = (xin + yin) * $author$project$Noise$f2;
		var j = $elm$core$Basics$floor(yin + s);
		var jj = j & 255;
		var i = $elm$core$Basics$floor(xin + s);
		var ii = i & 255;
		var t = (i + j) * $author$project$Noise$g2;
		var y0_ = j - t;
		var y0 = yin - y0_;
		var y2 = (y0 - 1) + (2 * $author$project$Noise$g2);
		var x0_ = i - t;
		var x0 = xin - x0_;
		var n0 = A6($author$project$Noise$getN2d, x0, y0, ii, jj, perm, permMod12);
		var x2 = (x0 - 1) + (2 * $author$project$Noise$g2);
		var n2 = A6($author$project$Noise$getN2d, x2, y2, ii + 1, jj + 1, perm, permMod12);
		var _v1 = A2($author$project$Noise$getCornerOffset2d, x0, y0);
		var i1 = _v1.a;
		var j1 = _v1.b;
		var x1 = (x0 - i1) + $author$project$Noise$g2;
		var y1 = (y0 - j1) + $author$project$Noise$g2;
		var n1 = A6($author$project$Noise$getN2d, x1, y1, ii + i1, jj + j1, perm, permMod12);
		return 70 * ((n0 + n1) + n2);
	});
var $author$project$MapData$noiseScale = 0.1;
var $author$project$Vector$scale = F2(
	function (v, f) {
		return A2($author$project$Vector$Vector, v.x * f, v.y * f);
	});
var $author$project$Vector$toVector = function (p) {
	return A2($author$project$Vector$Vector, p.x, p.y);
};
var $author$project$MapGenerator$getTerrainFor = F2(
	function (p, n) {
		var yDiff = 1 - ($elm$core$Basics$abs(p.y) / $author$project$MapData$mapSize);
		var xDiff = 1 - ($elm$core$Basics$abs(p.x) / $author$project$MapData$mapSize);
		var noiseP = A2(
			$author$project$Vector$scale,
			$author$project$Vector$toVector(p),
			$author$project$MapData$noiseScale);
		var height = ((A3($author$project$Noise$noise2d, n, noiseP.x, noiseP.y) + 1) / 2) * $elm$core$Basics$sin(
			(A2(
				$elm$core$Basics$min,
				1,
				A2($elm$core$Basics$min, xDiff, yDiff) * 2) * $elm$core$Basics$pi) / 2);
		return $author$project$Map$heighProgressToTerrain(height);
	});
var $author$project$MapData$hexRadius = 15;
var $author$project$MapData$spaceBetweenHexes = 3;
var $author$project$MapData$getXPosForIndex = function (i) {
	var absI = i + $author$project$MapData$mapWidth;
	return (((absI + 1) * $author$project$MapData$hexRadius) * 2) + ($author$project$MapData$spaceBetweenHexes * (absI - 1));
};
var $author$project$MapData$rad = 0.35;
var $author$project$Vector$y = function (v) {
	return v.y;
};
var $author$project$MapData$getYPosForIndex = function (i) {
	var absI = i + $author$project$MapData$mapHeight;
	return (((absI + 1) * $author$project$Vector$y(
		A2($author$project$Vector$pointOnCircle, $author$project$MapData$hexRadius, $author$project$MapData$rad))) * 2) + ($author$project$MapData$spaceBetweenHexes * (absI - 1));
};
var $author$project$MapData$tileRowXOffset = $elm$core$Basics$round($author$project$MapData$hexRadius + ($author$project$MapData$spaceBetweenHexes / 2));
var $author$project$MapData$rowXOffset = function (x) {
	var shiftedX = x + $author$project$MapData$mapHeight;
	return A2(
		$author$project$Vector$Vector,
		-(shiftedX * A2($author$project$Vector$pointOnCircle, $author$project$MapData$hexRadius, $author$project$MapData$rad).x),
		A2($elm$core$Basics$modBy, 2, shiftedX) * $author$project$MapData$tileRowXOffset);
};
var $author$project$MapData$mapPositionForIndex = function (p) {
	var offset = $author$project$MapData$rowXOffset(p.x);
	return A2(
		$author$project$Vector$Vector,
		$author$project$MapData$getXPosForIndex(p.x) + offset.x,
		$author$project$MapData$getYPosForIndex(p.y) + offset.y);
};
var $author$project$MapGenerator$buildHexagon = F2(
	function (p, n) {
		return A6(
			$author$project$Map$Model$MapTile,
			p,
			$author$project$MapData$mapPositionForIndex(p),
			A2($author$project$MapGenerator$getTerrainFor, p, n),
			$elm$core$Maybe$Nothing,
			_List_Nil,
			$author$project$Faction$Faction1);
	});
var $author$project$MapGenerator$buildHexagons = F3(
	function (height, i, n) {
		var indexOffset = $author$project$MapData$mapHeight;
		if (i >= 0) {
			var point = A2($author$project$Vector$Point, i - indexOffset, height);
			return A3(
				$elm$core$Dict$insert,
				$author$project$MapData$hashMapPoint(point),
				A2($author$project$MapGenerator$buildHexagon, point, n),
				A3($author$project$MapGenerator$buildHexagons, height, i - 1, n));
		} else {
			return $elm$core$Dict$empty;
		}
	});
var $author$project$MapGenerator$buildHexagonRow = function (i) {
	return A2($author$project$MapGenerator$buildHexagons, i, $author$project$MapData$mapWidth * 2);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $author$project$MapGenerator$createMap_ = F2(
	function (i, n) {
		return (i >= 0) ? A2(
			$elm$core$Dict$union,
			A2($author$project$MapGenerator$buildHexagonRow, i - $author$project$MapData$mapHeight, n),
			A2($author$project$MapGenerator$createMap_, i - 1, n)) : $elm$core$Dict$empty;
	});
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$core$Elm$JsArray$appendN = _JsArray_appendN;
var $elm$core$Elm$JsArray$slice = _JsArray_slice;
var $elm$core$Array$appendHelpBuilder = F2(
	function (tail, builder) {
		var tailLen = $elm$core$Elm$JsArray$length(tail);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(builder.tail)) - tailLen;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, builder.tail, tail);
		return (notAppended < 0) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: A3($elm$core$Elm$JsArray$slice, notAppended, tailLen, tail)
		} : ((!notAppended) ? {
			nodeList: A2(
				$elm$core$List$cons,
				$elm$core$Array$Leaf(appended),
				builder.nodeList),
			nodeListSize: builder.nodeListSize + 1,
			tail: $elm$core$Elm$JsArray$empty
		} : {nodeList: builder.nodeList, nodeListSize: builder.nodeListSize, tail: appended});
	});
var $elm$core$Elm$JsArray$push = _JsArray_push;
var $elm$core$Elm$JsArray$singleton = _JsArray_singleton;
var $elm$core$Elm$JsArray$unsafeSet = _JsArray_unsafeSet;
var $elm$core$Array$insertTailInTree = F4(
	function (shift, index, tail, tree) {
		var pos = $elm$core$Array$bitMask & (index >>> shift);
		if (_Utils_cmp(
			pos,
			$elm$core$Elm$JsArray$length(tree)) > -1) {
			if (shift === 5) {
				return A2(
					$elm$core$Elm$JsArray$push,
					$elm$core$Array$Leaf(tail),
					tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, $elm$core$Elm$JsArray$empty));
				return A2($elm$core$Elm$JsArray$push, newSub, tree);
			}
		} else {
			var value = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (value.$ === 'SubTree') {
				var subTree = value.a;
				var newSub = $elm$core$Array$SubTree(
					A4($elm$core$Array$insertTailInTree, shift - $elm$core$Array$shiftStep, index, tail, subTree));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			} else {
				var newSub = $elm$core$Array$SubTree(
					A4(
						$elm$core$Array$insertTailInTree,
						shift - $elm$core$Array$shiftStep,
						index,
						tail,
						$elm$core$Elm$JsArray$singleton(value)));
				return A3($elm$core$Elm$JsArray$unsafeSet, pos, newSub, tree);
			}
		}
	});
var $elm$core$Array$unsafeReplaceTail = F2(
	function (newTail, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var originalTailLen = $elm$core$Elm$JsArray$length(tail);
		var newTailLen = $elm$core$Elm$JsArray$length(newTail);
		var newArrayLen = len + (newTailLen - originalTailLen);
		if (_Utils_eq(newTailLen, $elm$core$Array$branchFactor)) {
			var overflow = _Utils_cmp(newArrayLen >>> $elm$core$Array$shiftStep, 1 << startShift) > 0;
			if (overflow) {
				var newShift = startShift + $elm$core$Array$shiftStep;
				var newTree = A4(
					$elm$core$Array$insertTailInTree,
					newShift,
					len,
					newTail,
					$elm$core$Elm$JsArray$singleton(
						$elm$core$Array$SubTree(tree)));
				return A4($elm$core$Array$Array_elm_builtin, newArrayLen, newShift, newTree, $elm$core$Elm$JsArray$empty);
			} else {
				return A4(
					$elm$core$Array$Array_elm_builtin,
					newArrayLen,
					startShift,
					A4($elm$core$Array$insertTailInTree, startShift, len, newTail, tree),
					$elm$core$Elm$JsArray$empty);
			}
		} else {
			return A4($elm$core$Array$Array_elm_builtin, newArrayLen, startShift, tree, newTail);
		}
	});
var $elm$core$Array$appendHelpTree = F2(
	function (toAppend, array) {
		var len = array.a;
		var tree = array.c;
		var tail = array.d;
		var itemsToAppend = $elm$core$Elm$JsArray$length(toAppend);
		var notAppended = ($elm$core$Array$branchFactor - $elm$core$Elm$JsArray$length(tail)) - itemsToAppend;
		var appended = A3($elm$core$Elm$JsArray$appendN, $elm$core$Array$branchFactor, tail, toAppend);
		var newArray = A2($elm$core$Array$unsafeReplaceTail, appended, array);
		if (notAppended < 0) {
			var nextTail = A3($elm$core$Elm$JsArray$slice, notAppended, itemsToAppend, toAppend);
			return A2($elm$core$Array$unsafeReplaceTail, nextTail, newArray);
		} else {
			return newArray;
		}
	});
var $elm$core$Elm$JsArray$foldl = _JsArray_foldl;
var $elm$core$Array$builderFromArray = function (_v0) {
	var len = _v0.a;
	var tree = _v0.c;
	var tail = _v0.d;
	var helper = F2(
		function (node, acc) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return A3($elm$core$Elm$JsArray$foldl, helper, acc, subTree);
			} else {
				return A2($elm$core$List$cons, node, acc);
			}
		});
	return {
		nodeList: A3($elm$core$Elm$JsArray$foldl, helper, _List_Nil, tree),
		nodeListSize: (len / $elm$core$Array$branchFactor) | 0,
		tail: tail
	};
};
var $elm$core$Array$append = F2(
	function (a, _v0) {
		var aTail = a.d;
		var bLen = _v0.a;
		var bTree = _v0.c;
		var bTail = _v0.d;
		if (_Utils_cmp(bLen, $elm$core$Array$branchFactor * 4) < 1) {
			var foldHelper = F2(
				function (node, array) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, array, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpTree, leaf, array);
					}
				});
			return A2(
				$elm$core$Array$appendHelpTree,
				bTail,
				A3($elm$core$Elm$JsArray$foldl, foldHelper, a, bTree));
		} else {
			var foldHelper = F2(
				function (node, builder) {
					if (node.$ === 'SubTree') {
						var tree = node.a;
						return A3($elm$core$Elm$JsArray$foldl, foldHelper, builder, tree);
					} else {
						var leaf = node.a;
						return A2($elm$core$Array$appendHelpBuilder, leaf, builder);
					}
				});
			return A2(
				$elm$core$Array$builderToArray,
				true,
				A2(
					$elm$core$Array$appendHelpBuilder,
					bTail,
					A3(
						$elm$core$Elm$JsArray$foldl,
						foldHelper,
						$elm$core$Array$builderFromArray(a),
						bTree)));
		}
	});
var $elm$core$Elm$JsArray$map = _JsArray_map;
var $elm$core$Array$map = F2(
	function (func, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = function (node) {
			if (node.$ === 'SubTree') {
				var subTree = node.a;
				return $elm$core$Array$SubTree(
					A2($elm$core$Elm$JsArray$map, helper, subTree));
			} else {
				var values = node.a;
				return $elm$core$Array$Leaf(
					A2($elm$core$Elm$JsArray$map, func, values));
			}
		};
		return A4(
			$elm$core$Array$Array_elm_builtin,
			len,
			startShift,
			A2($elm$core$Elm$JsArray$map, helper, tree),
			A2($elm$core$Elm$JsArray$map, func, tail));
	});
var $author$project$Noise$generatePermMod12 = function (perm) {
	return A2(
		$elm$core$Array$map,
		function (i) {
			return A2($elm$core$Basics$modBy, 12, i);
		},
		perm);
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$int = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v0 = (_Utils_cmp(a, b) < 0) ? _Utils_Tuple2(a, b) : _Utils_Tuple2(b, a);
				var lo = _v0.a;
				var hi = _v0.b;
				var range = (hi - lo) + 1;
				if (!((range - 1) & range)) {
					return _Utils_Tuple2(
						(((range - 1) & $elm$random$Random$peel(seed0)) >>> 0) + lo,
						$elm$random$Random$next(seed0));
				} else {
					var threshhold = (((-range) >>> 0) % range) >>> 0;
					var accountForBias = function (seed) {
						accountForBias:
						while (true) {
							var x = $elm$random$Random$peel(seed);
							var seedN = $elm$random$Random$next(seed);
							if (_Utils_cmp(x, threshhold) < 0) {
								var $temp$seed = seedN;
								seed = $temp$seed;
								continue accountForBias;
							} else {
								return _Utils_Tuple2((x % range) + lo, seedN);
							}
						}
					};
					return accountForBias(seed0);
				}
			});
	});
var $elm$core$Array$length = function (_v0) {
	var len = _v0.a;
	return len;
};
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
			});
	});
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $owanturist$elm_union_find$UnionFind$findFast = F2(
	function (id, dict) {
		findFast:
		while (true) {
			var _v0 = A2($elm$core$Dict$get, id, dict);
			if (_v0.$ === 'Nothing') {
				return id;
			} else {
				var cursor = _v0.a;
				if (_Utils_eq(id, cursor)) {
					return id;
				} else {
					var $temp$id = cursor,
						$temp$dict = dict;
					id = $temp$id;
					dict = $temp$dict;
					continue findFast;
				}
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$find = F2(
	function (id, _v0) {
		var dict = _v0.b;
		return A2($owanturist$elm_union_find$UnionFind$findFast, id, dict);
	});
var $elm$core$Array$isEmpty = function (_v0) {
	var len = _v0.a;
	return !len;
};
var $owanturist$elm_union_find$UnionFind$QuickUnionPathCompression = F2(
	function (a, b) {
		return {$: 'QuickUnionPathCompression', a: a, b: b};
	});
var $owanturist$elm_union_find$UnionFind$quickUnionPathCompression = A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, 0, $elm$core$Dict$empty);
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $owanturist$elm_union_find$UnionFind$findCompressed = F2(
	function (id, dict) {
		var _v0 = A2($elm$core$Dict$get, id, dict);
		if (_v0.$ === 'Nothing') {
			return _Utils_Tuple2(
				id,
				A3($elm$core$Dict$insert, id, id, dict));
		} else {
			var cursor = _v0.a;
			if (_Utils_eq(id, cursor)) {
				return _Utils_Tuple2(id, dict);
			} else {
				var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, cursor, dict);
				var parent = _v1.a;
				var nextDict = _v1.b;
				return _Utils_Tuple2(
					parent,
					A3($elm$core$Dict$insert, id, parent, nextDict));
			}
		}
	});
var $owanturist$elm_union_find$UnionFind$union = F3(
	function (left, right, _v0) {
		var count_ = _v0.a;
		var dict = _v0.b;
		var _v1 = A2($owanturist$elm_union_find$UnionFind$findCompressed, left, dict);
		var leftRoot = _v1.a;
		var leftDict = _v1.b;
		var _v2 = A2($owanturist$elm_union_find$UnionFind$findCompressed, right, leftDict);
		var rightRoot = _v2.a;
		var rightDict = _v2.b;
		return _Utils_eq(leftRoot, rightRoot) ? A2($owanturist$elm_union_find$UnionFind$QuickUnionPathCompression, count_, rightDict) : A2(
			$owanturist$elm_union_find$UnionFind$QuickUnionPathCompression,
			count_ + 1,
			A3($elm$core$Dict$insert, leftRoot, rightRoot, rightDict));
	});
var $elm_community$random_extra$Utils$selectUniqByIndexes = F2(
	function (values, randomIndexes) {
		var modByLength = $elm$core$Basics$modBy(
			$elm$core$Array$length(values));
		var step = F2(
			function (randomIndex, _v1) {
				var uf = _v1.a;
				var acc = _v1.b;
				var leaderOfElement = A2($owanturist$elm_union_find$UnionFind$find, randomIndex, uf);
				var leaderOfNextElement = A2(
					$owanturist$elm_union_find$UnionFind$find,
					modByLength(leaderOfElement + 1),
					uf);
				var _v0 = A2($elm$core$Array$get, leaderOfElement, values);
				if (_v0.$ === 'Nothing') {
					return _Utils_Tuple2(uf, acc);
				} else {
					var value = _v0.a;
					return _Utils_Tuple2(
						A3($owanturist$elm_union_find$UnionFind$union, leaderOfElement, leaderOfNextElement, uf),
						A2($elm$core$List$cons, value, acc));
				}
			});
		return $elm$core$Array$isEmpty(values) ? _List_Nil : A3(
			$elm$core$List$foldr,
			step,
			_Utils_Tuple2($owanturist$elm_union_find$UnionFind$quickUnionPathCompression, _List_Nil),
			randomIndexes).b;
	});
var $elm_community$random_extra$Random$Array$shuffle = function (values) {
	var length = $elm$core$Array$length(values);
	return A2(
		$elm$random$Random$map,
		A2(
			$elm$core$Basics$composeR,
			$elm_community$random_extra$Utils$selectUniqByIndexes(values),
			$elm$core$Array$fromList),
		A2(
			$elm$random$Random$list,
			length,
			A2($elm$random$Random$int, 0, length - 1)));
};
var $author$project$Noise$permGenerattor = $elm_community$random_extra$Random$Array$shuffle(
	$elm$core$Array$fromList(
		A2($elm$core$List$range, 0, 255)));
var $author$project$Noise$reverseArray = function (array) {
	return $elm$core$Array$fromList(
		$elm$core$List$reverse(
			$elm$core$Array$toList(array)));
};
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $author$project$Noise$permutationTable = function (seed) {
	var _v0 = function (_v1) {
		var list = _v1.a;
		var seed__ = _v1.b;
		return _Utils_Tuple2(
			A2(
				$elm$core$Array$append,
				list,
				$author$project$Noise$reverseArray(list)),
			seed__);
	}(
		A2($elm$random$Random$step, $author$project$Noise$permGenerattor, seed));
	var perm = _v0.a;
	var seed_ = _v0.b;
	return _Utils_Tuple2(
		{
			perm: perm,
			permMod12: $author$project$Noise$generatePermMod12(perm)
		},
		seed_);
};
var $author$project$MapData$seed = 3;
var $author$project$MapGenerator$createMap = function () {
	var perm = $author$project$Noise$permutationTable(
		$elm$random$Random$initialSeed($author$project$MapData$seed)).a;
	return A2($author$project$MapGenerator$createMap_, $author$project$MapData$mapHeight * 2, perm);
}();
var $author$project$Event$Important = {$: 'Important'};
var $author$project$Event$Minor = {$: 'Minor'};
var $author$project$Main$testEvents = _List_fromArray(
	[
		{eventType: $author$project$Event$Important, header: 'Hallo leude', index: 0, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'Ich der zweite', index: 1, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 2, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 3, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 4, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 5, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 6, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 7, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'},
		{eventType: $author$project$Event$Minor, header: 'WARUM?!??!', index: 8, text: 'lorem ipsum lorem ipsum lorem ipsum lorem ipsum'}
	]);
var $author$project$Main$testEventState = {events: $author$project$Main$testEvents, state: true};
var $author$project$Main$initialModel = function () {
	var map = $author$project$MapGenerator$createMap;
	return A7(
		$author$project$Main$Model,
		A2($author$project$Entities$Cons, $author$project$Main$testLord, _List_Nil),
		$author$project$Main$GameSetup(
			$author$project$Main$MainMenue($author$project$Main$Menue)),
		$elm$core$Maybe$Nothing,
		A2($author$project$DateExt$Date, 1017, $author$project$DateExt$Jan),
		map,
		'',
		$author$project$Main$testEventState);
}();
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$startGame = function (playerCount) {
	return _Utils_Tuple2(
		A2($author$project$Main$initPlayers, $author$project$Main$initialModel, playerCount),
		$elm$core$Platform$Cmd$none);
};
var $author$project$Main$GameMenue = {$: 'GameMenue'};
var $author$project$Main$GameOver = function (a) {
	return {$: 'GameOver', a: a};
};
var $author$project$DateExt$Apr = {$: 'Apr'};
var $author$project$DateExt$Aug = {$: 'Aug'};
var $author$project$DateExt$Dec = {$: 'Dec'};
var $author$project$DateExt$Feb = {$: 'Feb'};
var $author$project$DateExt$Jul = {$: 'Jul'};
var $author$project$DateExt$Jun = {$: 'Jun'};
var $author$project$DateExt$Mar = {$: 'Mar'};
var $author$project$DateExt$May = {$: 'May'};
var $author$project$DateExt$Nov = {$: 'Nov'};
var $author$project$DateExt$Oct = {$: 'Oct'};
var $author$project$DateExt$Sep = {$: 'Sep'};
var $author$project$DateExt$intToMonth = function (i) {
	var _v0 = A2($elm$core$Basics$modBy, 12, i);
	switch (_v0) {
		case 1:
			return $author$project$DateExt$Jan;
		case 2:
			return $author$project$DateExt$Feb;
		case 3:
			return $author$project$DateExt$Mar;
		case 4:
			return $author$project$DateExt$Apr;
		case 5:
			return $author$project$DateExt$May;
		case 6:
			return $author$project$DateExt$Jun;
		case 7:
			return $author$project$DateExt$Jul;
		case 8:
			return $author$project$DateExt$Aug;
		case 9:
			return $author$project$DateExt$Sep;
		case 10:
			return $author$project$DateExt$Oct;
		case 11:
			return $author$project$DateExt$Nov;
		default:
			return $author$project$DateExt$Dec;
	}
};
var $author$project$DateExt$monthToInt = function (m) {
	switch (m.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var $author$project$DateExt$addMonths = F2(
	function (i, date) {
		var newYears = ((($author$project$DateExt$monthToInt(date.month) + i) - 1) / 12) | 0;
		var newMonth = $author$project$DateExt$intToMonth(
			$author$project$DateExt$monthToInt(date.month) + i);
		return A2($author$project$DateExt$Date, date.year + newYears, newMonth);
	});
var $author$project$Main$appendCmd = function (m) {
	return _Utils_Tuple2(m, $elm$core$Platform$Cmd$none);
};
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $author$project$Troops$mergeTroops = F2(
	function (a1, a2) {
		return A6(
			$elm$core$Dict$merge,
			$elm$core$Dict$insert,
			F4(
				function (k, v1, v2, r) {
					return A3($elm$core$Dict$insert, k, v1 + v2, r);
				}),
			$elm$core$Dict$insert,
			a1,
			a2,
			$elm$core$Dict$empty);
	});
var $author$project$Entities$sumLordSettlementTroops = function (lord) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (s, dict) {
				return A2($author$project$Troops$mergeTroops, dict, s.entity.army);
			}),
		$elm$core$Dict$empty,
		lord.land);
};
var $author$project$Entities$sumLordTroops = function (lord) {
	return A2(
		$author$project$Troops$mergeTroops,
		lord.entity.army,
		$author$project$Entities$sumLordSettlementTroops(lord));
};
var $author$project$Building$buildingToBonus = function (b) {
	switch (b.$) {
		case 'Marketplace':
			return 1.5;
		case 'Barracks':
			return 1;
		default:
			return 10;
	}
};
var $author$project$Building$resolveBonusFromBuildings = F2(
	function (l, b) {
		var building = $elm$core$List$head(
			A2(
				$elm$core$List$filter,
				function (x) {
					return _Utils_eq(x.buildingType, b);
				},
				l));
		if (building.$ === 'Nothing') {
			return 0;
		} else {
			var v = building.a;
			return $author$project$Building$buildingToBonus(b) * v.level;
		}
	});
var $author$project$Entities$sumSettlementsIncome = function (s) {
	return A3(
		$elm$core$List$foldr,
		F2(
			function (x, v) {
				return (x.income + A2($author$project$Building$resolveBonusFromBuildings, x.buildings, $author$project$Building$Marketplace)) + v;
			}),
		0,
		s);
};
var $author$project$Troops$intToTroopType = function (i) {
	switch (i) {
		case 0:
			return $author$project$Troops$Archer;
		case 1:
			return $author$project$Troops$Spear;
		case 2:
			return $author$project$Troops$Sword;
		case 3:
			return $author$project$Troops$Knight;
		default:
			return $author$project$Troops$Spear;
	}
};
var $author$project$Troops$troopWage = function (t) {
	switch (t.$) {
		case 'Archer':
			return 0.4;
		case 'Spear':
			return 0.2;
		case 'Sword':
			return 0.5;
		default:
			return 1.0;
	}
};
var $author$project$Entities$sumTroopWages = A2(
	$elm$core$Dict$foldl,
	F3(
		function (k, v, wages) {
			return (v * $author$project$Troops$troopWage(
				$author$project$Troops$intToTroopType(k))) + wages;
		}),
	0);
var $author$project$Entities$calculateRoundIncome = function (lord) {
	return $author$project$Entities$sumSettlementsIncome(lord.land) - $author$project$Entities$sumTroopWages(
		$author$project$Entities$sumLordTroops(lord));
};
var $author$project$Entities$applyLordGoldIncome = function (lord) {
	return _Utils_update(
		lord,
		{
			gold: lord.gold + $author$project$Entities$calculateRoundIncome(lord)
		});
};
var $elm$core$Dict$map = F2(
	function (func, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				A2(func, key, value),
				A2($elm$core$Dict$map, func, left),
				A2($elm$core$Dict$map, func, right));
		}
	});
var $author$project$Entities$applySettlementNewRecruits = $elm$core$List$map(
	function (s) {
		return _Utils_update(
			s,
			{
				recruitLimits: A2(
					$elm$core$Dict$map,
					F2(
						function (t, amount) {
							return amount + $elm$core$Basics$round(
								5.0 + A2($author$project$Building$resolveBonusFromBuildings, s.buildings, $author$project$Building$Barracks));
						}),
					s.recruitLimits)
			});
	});
var $author$project$Entities$applyLordNewRecruits = function (lord) {
	return _Utils_update(
		lord,
		{
			land: $author$project$Entities$applySettlementNewRecruits(lord.land)
		});
};
var $author$project$PathAgent$resetUsedMovement = function (a) {
	return _Utils_update(
		a,
		{usedMovement: 0});
};
var $author$project$Entities$resetUsedMovement = function (lord) {
	return _Utils_update(
		lord,
		{
			agent: $author$project$PathAgent$resetUsedMovement(lord.agent)
		});
};
var $author$project$Main$endRoundForLord = function (l) {
	return $author$project$Entities$applyLordNewRecruits(
		$author$project$Entities$resetUsedMovement(
			$author$project$Entities$applyLordGoldIncome(l)));
};
var $author$project$Entities$getPlayer = function (_v0) {
	var p = _v0.a;
	return p;
};
var $author$project$Main$getPlayer = function (model) {
	return $author$project$Entities$getPlayer(model.lords);
};
var $author$project$Entities$npcs = function (_v0) {
	var ls = _v0.b;
	return ls;
};
var $author$project$Entities$setPosition = F2(
	function (entity, pos) {
		return _Utils_update(
			entity,
			{position: pos});
	});
var $author$project$Main$updateAI = function (lord) {
	return _Utils_update(
		lord,
		{
			entity: A2(
				$author$project$Entities$setPosition,
				lord.entity,
				A2(
					$author$project$Vector$addPoints,
					A2($author$project$Vector$Point, 1, 1),
					lord.entity.position))
		});
};
var $author$project$Main$updateAIsAfterPlayerRound = function (lords) {
	return A2(
		$elm$core$List$map,
		function (l) {
			return $author$project$Main$endRoundForLord(
				$author$project$Main$updateAI(l));
		},
		lords);
};
var $author$project$Main$BattleView = function (a) {
	return {$: 'BattleView', a: a};
};
var $elm$core$Basics$neq = _Utils_notEqual;
var $author$project$Main$checkLordLost = F3(
	function (k, n, l) {
		return k ? A2(
			$elm$core$List$filter,
			function (x) {
				return !_Utils_eq(x.entity.name, n);
			},
			l) : l;
	});
var $author$project$Battle$calculateEntityCasualties = F2(
	function (armyBefore, armyAfter) {
		return A6(
			$elm$core$Dict$merge,
			F3(
				function (k, v, r) {
					return A3($elm$core$Dict$insert, k, 0, r);
				}),
			F4(
				function (k, v1, v2, r) {
					return A3($elm$core$Dict$insert, k, v2 - v1, r);
				}),
			F3(
				function (k, v2, r) {
					return A3($elm$core$Dict$insert, k, 0, r);
				}),
			armyBefore,
			armyAfter,
			$elm$core$Dict$empty);
	});
var $elm$core$Dict$values = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2($elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var $author$project$Troops$sumTroops = function (a) {
	return A3(
		$elm$core$List$foldl,
		$elm$core$Basics$add,
		0,
		$elm$core$Dict$values(a));
};
var $author$project$Battle$checkDefenderArmy = F2(
	function (defender, settle) {
		if (settle.$ === 'Nothing') {
			return !$author$project$Troops$sumTroops(defender.entity.army);
		} else {
			var s = settle.a;
			return !$author$project$Troops$sumTroops(s.entity.army);
		}
	});
var $author$project$Entities$getLordCapital = function (l) {
	getLordCapital:
	while (true) {
		if (!l.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var x = l.a;
			var xs = l.b;
			if (_Utils_eq(x.settlementType, $author$project$Entities$Model$Castle)) {
				return $elm$core$Maybe$Just(x);
			} else {
				var $temp$l = xs;
				l = $temp$l;
				continue getLordCapital;
			}
		}
	}
};
var $author$project$Battle$lordBattleAftermath = function (lord) {
	if (!$author$project$Troops$sumTroops(lord.entity.army)) {
		var _v0 = $author$project$Entities$getLordCapital(lord.land);
		if (_v0.$ === 'Nothing') {
			return lord;
		} else {
			var settle = _v0.a;
			return _Utils_update(
				lord,
				{
					entity: A2($author$project$Entities$setPosition, lord.entity, settle.entity.position)
				});
		}
	} else {
		return lord;
	}
};
var $author$project$Battle$constructBattleResult = F6(
	function (bS, attacker, defender, settle, aCasu, dCasu) {
		return _Utils_update(
			bS,
			{
				attacker: $author$project$Battle$lordBattleAftermath(attacker),
				attackerCasualties: aCasu,
				defender: $author$project$Battle$lordBattleAftermath(defender),
				defenderCasualties: dCasu,
				finished: (!$author$project$Troops$sumTroops(attacker.entity.army)) || A2($author$project$Battle$checkDefenderArmy, defender, settle),
				round: bS.round + 1,
				settlement: settle
			});
	});
var $author$project$Troops$troopDefense = function (t) {
	switch (t.$) {
		case 'Archer':
			return 30;
		case 'Spear':
			return 50;
		case 'Sword':
			return 70;
		default:
			return 100;
	}
};
var $author$project$Battle$calcCasualties = F3(
	function (t, amount, d) {
		return A2(
			$elm$core$Basics$max,
			0,
			amount - $elm$core$Basics$round(
				d / $author$project$Troops$troopDefense(t)));
	});
var $author$project$Battle$calcTroopCasualties = F3(
	function (army, d, a) {
		return A2(
			$elm$core$Dict$map,
			F2(
				function (k, v) {
					return A3(
						$author$project$Battle$calcCasualties,
						$author$project$Troops$intToTroopType(k),
						v,
						(d + 100.0) * (v / a));
				}),
			army);
	});
var $author$project$Battle$evaluateLordCasualities = F2(
	function (w, d) {
		return _Utils_update(
			w,
			{
				army: A3(
					$author$project$Battle$calcTroopCasualties,
					w.army,
					d,
					$author$project$Troops$sumTroops(w.army))
			});
	});
var $author$project$Troops$battlefieldBonus = function (t) {
	switch (t.$) {
		case 'Archer':
			return 1.15;
		case 'Spear':
			return 1.25;
		case 'Sword':
			return 1.2;
		default:
			return 1.2;
	}
};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$member = F2(
	function (x, xs) {
		return A2(
			$elm$core$List$any,
			function (a) {
				return _Utils_eq(a, x);
			},
			xs);
	});
var $author$project$OperatorExt$ternary = F3(
	function (bool, op1, op2) {
		return bool ? op1 : op2;
	});
var $author$project$Map$terrainToBonus = function (ter) {
	switch (ter.$) {
		case 'Grass':
			return _List_fromArray(
				[$author$project$Troops$Knight, $author$project$Troops$Sword]);
		case 'Forest':
			return _List_fromArray(
				[$author$project$Troops$Archer]);
		case 'Mountain':
			return _List_fromArray(
				[$author$project$Troops$Spear]);
		default:
			return _List_Nil;
	}
};
var $author$project$Troops$troopDamage = function (t) {
	switch (t.$) {
		case 'Archer':
			return 15;
		case 'Spear':
			return 10;
		case 'Sword':
			return 12;
		default:
			return 25;
	}
};
var $author$project$Battle$sumTroopsDamage = F3(
	function (army, ter, siegeBonus) {
		var bonusTroopTypes = $author$project$Map$terrainToBonus(ter);
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (k, v, dmg) {
					return dmg + (((siegeBonus * A3(
						$author$project$OperatorExt$ternary,
						A2(
							$elm$core$List$member,
							$author$project$Troops$intToTroopType(k),
							bonusTroopTypes),
						$author$project$Troops$battlefieldBonus(
							$author$project$Troops$intToTroopType(k)),
						1)) * $author$project$Troops$troopDamage(
						$author$project$Troops$intToTroopType(k))) * v);
				}),
			0,
			army);
	});
var $author$project$Battle$evaluateBattle = F4(
	function (w, army, ter, siegeBonus) {
		return A2(
			$author$project$Battle$evaluateLordCasualities,
			w,
			A3($author$project$Battle$sumTroopsDamage, army, ter, siegeBonus));
	});
var $author$project$Battle$evaluateLordBattle = F2(
	function (bS, ter) {
		var tempDefender = bS.defender;
		var tempAttacker = bS.attacker;
		var newDefender = _Utils_update(
			tempDefender,
			{
				entity: A4($author$project$Battle$evaluateBattle, tempDefender.entity, bS.attacker.entity.army, ter, 1)
			});
		var newAttacker = _Utils_update(
			tempAttacker,
			{
				entity: A4($author$project$Battle$evaluateBattle, tempAttacker.entity, bS.defender.entity.army, ter, 1)
			});
		var defenderCasualties = A2($author$project$Battle$calculateEntityCasualties, bS.defender.entity.army, newDefender.entity.army);
		var attackerCasualties = A2($author$project$Battle$calculateEntityCasualties, bS.attacker.entity.army, newAttacker.entity.army);
		return A6($author$project$Battle$constructBattleResult, bS, newAttacker, newDefender, bS.settlement, attackerCasualties, defenderCasualties);
	});
var $author$project$Entities$getSettlementBonus = F2(
	function (s, l) {
		return _Utils_eq(s.settlementType, $author$project$Entities$Model$Village) ? 1.1 : A3(
			$elm$core$List$foldr,
			F2(
				function (_v0, y) {
					return 0.1 + y;
				}),
			1,
			l);
	});
var $author$project$Entities$isLordOnSettlement = F2(
	function (lord, s) {
		return _Utils_eq(lord.entity.position, s.entity.position) && _Utils_eq(lord.entity.faction, s.entity.faction);
	});
var $author$project$Entities$updateEntitiesArmy = F2(
	function (army, e) {
		return _Utils_update(
			e,
			{army: army});
	});
var $author$project$Battle$transferTroops = F2(
	function (l, s) {
		var newLord = _Utils_update(
			l,
			{
				entity: A2($author$project$Entities$updateEntitiesArmy, $elm$core$Dict$empty, l.entity)
			});
		var newArmy = A2($author$project$Troops$mergeTroops, l.entity.army, s.entity.army);
		var newSettlement = _Utils_update(
			s,
			{
				entity: A2($author$project$Entities$updateEntitiesArmy, newArmy, s.entity)
			});
		return _Utils_Tuple2(newLord, newSettlement);
	});
var $author$project$Battle$siegeBattleSetDefender = F2(
	function (bS, settle) {
		return A2($author$project$Entities$isLordOnSettlement, bS.defender, settle) ? A2($author$project$Battle$transferTroops, bS.defender, settle) : _Utils_Tuple2(bS.defender, settle);
	});
var $author$project$Battle$evaluateSiegeBattle = F3(
	function (bS, settle, ter) {
		var tempAttacker = bS.attacker;
		var _v0 = A2($author$project$Battle$siegeBattleSetDefender, bS, settle);
		var transferedDefender = _v0.a;
		var transferedSettle = _v0.b;
		var newAttacker = _Utils_update(
			tempAttacker,
			{
				entity: A4(
					$author$project$Battle$evaluateBattle,
					tempAttacker.entity,
					transferedSettle.entity.army,
					ter,
					A2($author$project$Entities$getSettlementBonus, settle, bS.defender.land))
			});
		var attackerCasualties = A2($author$project$Battle$calculateEntityCasualties, bS.attacker.entity.army, newAttacker.entity.army);
		var newSettle = _Utils_update(
			transferedSettle,
			{
				entity: A4($author$project$Battle$evaluateBattle, transferedSettle.entity, bS.attacker.entity.army, ter, 1)
			});
		var defenderCasualties = A2($author$project$Battle$calculateEntityCasualties, bS.defender.entity.army, newSettle.entity.army);
		return A6(
			$author$project$Battle$constructBattleResult,
			bS,
			newAttacker,
			transferedDefender,
			$elm$core$Maybe$Just(newSettle),
			attackerCasualties,
			defenderCasualties);
	});
var $author$project$Battle$evaluateBattleResult = F2(
	function (bS, t) {
		if (bS.siege) {
			var _v0 = bS.settlement;
			if (_v0.$ === 'Nothing') {
				return bS;
			} else {
				var settle = _v0.a;
				return A3($author$project$Battle$evaluateSiegeBattle, bS, settle, t);
			}
		} else {
			return A2($author$project$Battle$evaluateLordBattle, bS, t);
		}
	});
var $author$project$Battle$battleFleeTroopLoss = 0.4;
var $author$project$Entities$updatePlayerArmy = F2(
	function (l, t) {
		return _Utils_update(
			l,
			{
				entity: A2($author$project$Entities$updateEntitiesArmy, t, l.entity)
			});
	});
var $author$project$Battle$fleeBattle = function (bS) {
	return A2(
		$author$project$Entities$updatePlayerArmy,
		bS.attacker,
		A2(
			$elm$core$Dict$map,
			F2(
				function (k, v) {
					return $elm$core$Basics$round(v * (1 - $author$project$Battle$battleFleeTroopLoss));
				}),
			bS.attacker.entity.army));
};
var $author$project$MaybeExt$foldMaybe = F3(
	function (f, n, m) {
		if (m.$ === 'Nothing') {
			return n;
		} else {
			var a = m.a;
			return f(a);
		}
	});
var $author$project$Map$getTerrainForPoint = F2(
	function (p, map) {
		return A3(
			$author$project$MaybeExt$foldMaybe,
			function ($) {
				return $.terrain;
			},
			$author$project$Map$Model$Grass,
			A2(
				$elm$core$Dict$get,
				$author$project$MapData$hashMapPoint(p),
				map));
	});
var $author$project$Entities$updateEntityFaction = F2(
	function (fa, we) {
		return _Utils_update(
			we,
			{army: $author$project$Troops$emptyTroops, faction: fa});
	});
var $author$project$Battle$handleSettlementTransfer = F4(
	function (attacker, defender, aFunc, ndl) {
		return _Utils_Tuple3(
			_Utils_update(
				attacker,
				{
					land: _Utils_ap(
						A2(
							$elm$core$List$map,
							function (x) {
								return _Utils_update(
									x,
									{
										entity: A2($author$project$Entities$updateEntityFaction, attacker.entity.faction, x.entity)
									});
							},
							A2($elm$core$List$filter, aFunc, defender.land)),
						attacker.land)
				}),
			_Utils_update(
				defender,
				{land: ndl}),
			!$elm$core$List$length(ndl));
	});
var $author$project$Battle$siegeBattleAftermath = F2(
	function (bS, s) {
		var defender = bS.defender;
		var attacker = bS.attacker;
		return ($author$project$Troops$sumTroops(s.entity.army) <= 0) ? (_Utils_eq(s.settlementType, $author$project$Entities$Model$Castle) ? A4(
			$author$project$Battle$handleSettlementTransfer,
			attacker,
			defender,
			function (y) {
				return !_Utils_eq(y.settlementType, $author$project$Entities$Model$Castle);
			},
			_List_Nil) : A4(
			$author$project$Battle$handleSettlementTransfer,
			attacker,
			defender,
			function (y) {
				return _Utils_eq(y.entity.name, s.entity.name);
			},
			A2(
				$elm$core$List$filter,
				function (y) {
					return !_Utils_eq(y.entity.name, s.entity.name);
				},
				defender.land))) : _Utils_Tuple3(attacker, defender, false);
	});
var $author$project$Battle$skipBattle = F2(
	function (bS, ter) {
		skipBattle:
		while (true) {
			var newBattleStats = A2($author$project$Battle$evaluateBattleResult, bS, ter);
			if (newBattleStats.finished) {
				return newBattleStats;
			} else {
				var $temp$bS = newBattleStats,
					$temp$ter = ter;
				bS = $temp$bS;
				ter = $temp$ter;
				continue skipBattle;
			}
		}
	});
var $author$project$Entities$tailLordList = function (_v0) {
	var ps = _v0.b;
	return ps;
};
var $author$project$Main$updateLordsAfterBattle = F4(
	function (player, enemyLords, model, state) {
		return _Utils_update(
			model,
			{
				gameState: state,
				lords: A2($author$project$Entities$Cons, player, enemyLords)
			});
	});
var $author$project$Main$updateBattle = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StartSkirmish':
				var bS = msg.a;
				var newBattleStats = A2(
					$author$project$Battle$evaluateBattleResult,
					bS,
					A2($author$project$Map$getTerrainForPoint, bS.attacker.entity.position, model.map));
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							$author$project$Main$BattleView(newBattleStats))
					});
			case 'SkipSkirmishes':
				var bS = msg.a;
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							$author$project$Main$BattleView(
								A2(
									$author$project$Battle$skipBattle,
									bS,
									A2($author$project$Map$getTerrainForPoint, bS.attacker.entity.position, model.map))))
					});
			case 'FleeBattle':
				var bS = msg.a;
				return A4(
					$author$project$Main$updateLordsAfterBattle,
					$author$project$Battle$fleeBattle(bS),
					A2(
						$elm$core$List$map,
						function (x) {
							return A3(
								$author$project$OperatorExt$ternary,
								_Utils_eq(x.entity.name, bS.defender.entity.name),
								bS.defender,
								x);
						},
						$author$project$Entities$tailLordList(model.lords)),
					model,
					$author$project$Main$GameSetup($author$project$Main$GameMenue));
			default:
				var bS = msg.a;
				var _v1 = bS.settlement;
				if (_v1.$ === 'Nothing') {
					return A4(
						$author$project$Main$updateLordsAfterBattle,
						bS.attacker,
						A2(
							$elm$core$List$map,
							function (x) {
								return A3(
									$author$project$OperatorExt$ternary,
									_Utils_eq(x.entity.name, bS.defender.entity.name),
									bS.defender,
									x);
							},
							$author$project$Entities$tailLordList(model.lords)),
						model,
						$author$project$Main$GameSetup($author$project$Main$GameMenue));
				} else {
					var settle = _v1.a;
					var _v2 = A2($author$project$Battle$siegeBattleAftermath, bS, settle);
					var newAttacker = _v2.a;
					var newDefender = _v2.b;
					var lordKilled = _v2.c;
					var newEnemyLords = A3(
						$author$project$Main$checkLordLost,
						lordKilled,
						newDefender.entity.name,
						A2(
							$elm$core$List$map,
							function (x) {
								return A3(
									$author$project$OperatorExt$ternary,
									_Utils_eq(x.entity.name, bS.defender.entity.name),
									bS.defender,
									x);
							},
							$author$project$Entities$tailLordList(model.lords)));
					return A4(
						$author$project$Main$updateLordsAfterBattle,
						newAttacker,
						newEnemyLords,
						model,
						A3(
							$author$project$OperatorExt$ternary,
							$elm$core$List$length(
								$author$project$Entities$tailLordList(model.lords)) > 0,
							$author$project$Main$GameSetup($author$project$Main$GameMenue),
							$author$project$Main$GameOver(true)));
				}
		}
	});
var $author$project$Event$clearEvents = function (e) {
	return _Utils_update(
		e,
		{events: _List_Nil});
};
var $author$project$Event$removeEvent = F2(
	function (e, i) {
		return _Utils_update(
			e,
			{
				events: A2(
					$elm$core$List$filter,
					function (x) {
						return !_Utils_eq(x.index, i);
					},
					e.events)
			});
	});
var $author$project$Event$updateEventState = function (e) {
	return _Utils_update(
		e,
		{state: !e.state});
};
var $author$project$Main$updateEvent = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'DeleteEvent':
				var index = msg.a;
				return _Utils_update(
					model,
					{
						event: A2($author$project$Event$removeEvent, model.event, index)
					});
			case 'SwitchEventView':
				return _Utils_update(
					model,
					{
						event: $author$project$Event$updateEventState(model.event)
					});
			default:
				return _Utils_update(
					model,
					{
						event: $author$project$Event$clearEvents(model.event)
					});
		}
	});
var $author$project$Msg$RestrictedView = {$: 'RestrictedView'};
var $author$project$Main$SettlementView = F3(
	function (a, b, c) {
		return {$: 'SettlementView', a: a, b: b, c: c};
	});
var $author$project$Msg$StandardView = {$: 'StandardView'};
var $author$project$Entities$findLordWithSettlement = function (settlement) {
	return A2(
		$elm$core$List$foldr,
		F2(
			function (l, r) {
				return _Utils_eq(l.entity.faction, settlement.entity.faction) ? $elm$core$Maybe$Just(l) : r;
			}),
		$elm$core$Maybe$Nothing);
};
var $author$project$Entities$flattenLordList = function (_v0) {
	var p = _v0.a;
	var ps = _v0.b;
	return A2($elm$core$List$cons, p, ps);
};
var $author$project$Pathfinder$Model$PathInfo = F2(
	function (nav, target) {
		return {nav: nav, target: target};
	});
var $author$project$MapAction$SubModel$MoveTo = function (a) {
	return {$: 'MoveTo', a: a};
};
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $author$project$MapAction$actionsOnPoint = F2(
	function (p, dict) {
		return A3(
			$author$project$MaybeExt$foldMaybe,
			function (l) {
				return $elm$core$List$concat(
					A2(
						$elm$core$List$map,
						function ($) {
							return $.action;
						},
						l));
			},
			_List_Nil,
			A2(
				$elm$core$Dict$get,
				$author$project$MapData$hashMapPoint(p),
				dict));
	});
var $author$project$Main$hasActionOnPoint = F3(
	function (p, msg, dict) {
		return A2(
			$elm$core$List$member,
			msg,
			A2($author$project$MapAction$actionsOnPoint, p, dict));
	});
var $author$project$Main$canMoveToPoint = F2(
	function (dict, p) {
		return A3(
			$author$project$Main$hasActionOnPoint,
			p,
			$author$project$MapAction$SubModel$MoveTo(p),
			dict);
	});
var $author$project$MapAction$Model$InteractableSvg = F2(
	function (svg, action) {
		return {action: action, svg: svg};
	});
var $author$project$MapAction$Model$SvgItem = F2(
	function (a, b) {
		return {$: 'SvgItem', a: a, b: b};
	});
var $author$project$Msg$Click = function (a) {
	return {$: 'Click', a: a};
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $elm$svg$Svg$Attributes$height = _VirtualDom_attribute('height');
var $elm$svg$Svg$trustedNode = _VirtualDom_nodeNS('http://www.w3.org/2000/svg');
var $elm$svg$Svg$image = $elm$svg$Svg$trustedNode('image');
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$svg$Svg$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Attributes$width = _VirtualDom_attribute('width');
var $elm$svg$Svg$Attributes$x = _VirtualDom_attribute('x');
var $elm$svg$Svg$Attributes$xlinkHref = function (value) {
	return A3(
		_VirtualDom_attributeNS,
		'http://www.w3.org/1999/xlink',
		'xlink:href',
		_VirtualDom_noJavaScriptUri(value));
};
var $elm$svg$Svg$Attributes$y = _VirtualDom_attribute('y');
var $author$project$BasicDrawing$getImage = F3(
	function (imgName, indices, scale) {
		var size = A2(
			$author$project$Vector$scale,
			A2($author$project$Vector$Vector, $author$project$MapData$hexRadius * 1.5, $author$project$MapData$hexRadius * 2),
			scale);
		var pos = $author$project$MapData$mapPositionForIndex(indices);
		return A2(
			$elm$svg$Svg$image,
			_List_fromArray(
				[
					$elm$svg$Svg$Events$onClick(
					$author$project$Msg$Click(indices)),
					$elm$svg$Svg$Attributes$x(
					$elm$core$String$fromFloat(pos.x - (size.x / 2))),
					$elm$svg$Svg$Attributes$y(
					$elm$core$String$fromFloat(pos.y - (size.y / 2))),
					$elm$svg$Svg$Attributes$width(
					$elm$core$String$fromFloat(size.x)),
					$elm$svg$Svg$Attributes$height(
					$elm$core$String$fromFloat(size.y)),
					$elm$svg$Svg$Attributes$xlinkHref(imgName)
				]),
			_List_Nil);
	});
var $author$project$Map$terrainToImageName = function (t) {
	switch (t.$) {
		case 'Grass':
			return $elm$core$Maybe$Nothing;
		case 'Water':
			return $elm$core$Maybe$Nothing;
		case 'Forest':
			return $elm$core$Maybe$Just('./assets/images/map/tree.png');
		default:
			return $elm$core$Maybe$Just('./assets/images/map/mountain_icon.png');
	}
};
var $author$project$Map$Drawer$getImageForTile = function (t) {
	return A2(
		$elm$core$Maybe$andThen,
		function (imageName) {
			return $elm$core$Maybe$Just(
				A3($author$project$BasicDrawing$getImage, imageName, t.indices, 0.9));
		},
		$author$project$Map$terrainToImageName(t.terrain));
};
var $author$project$MapData$imageTileZIndex = 2;
var $author$project$Map$Drawer$getImageItemForTile = function (t) {
	return A2(
		$elm$core$Maybe$andThen,
		function (img) {
			return $elm$core$Maybe$Just(
				A2($author$project$MapAction$Model$SvgItem, $author$project$MapData$imageTileZIndex, img));
		},
		$author$project$Map$Drawer$getImageForTile(t));
};
var $author$project$Map$Drawer$getMapTileAction = function (tile) {
	return $author$project$Map$canMoveOnTile(tile) ? _List_fromArray(
		[
			$author$project$MapAction$SubModel$MoveTo(tile.indices)
		]) : _List_Nil;
};
var $author$project$Vector$add = F2(
	function (v1, v2) {
		return A2($author$project$Vector$Vector, v1.x + v2.x, v1.y + v2.y);
	});
var $author$project$Vector$flipOnX = function (v) {
	return _Utils_update(
		v,
		{x: -v.x});
};
var $author$project$Vector$flipOnY = function (v) {
	return _Utils_update(
		v,
		{y: -v.y});
};
var $author$project$BasicDrawing$calculateHexagonPoints = F2(
	function (v, r) {
		var topLeft = A2($author$project$Vector$pointOnCircle, r, $author$project$MapData$rad);
		var tr = A2(
			$author$project$Vector$add,
			$author$project$Vector$flipOnX(topLeft),
			v);
		var tl = A2($author$project$Vector$add, topLeft, v);
		var middleLeft = A2($author$project$Vector$pointOnCircle, r, 0);
		var ml = A2($author$project$Vector$add, middleLeft, v);
		var mr = A2(
			$author$project$Vector$add,
			$author$project$Vector$flipOnX(middleLeft),
			v);
		var bottomLeft = $author$project$Vector$flipOnY(topLeft);
		var br = A2(
			$author$project$Vector$add,
			$author$project$Vector$flipOnX(bottomLeft),
			v);
		var bl = A2($author$project$Vector$add, bottomLeft, v);
		return _List_fromArray(
			[tl, ml, bl, br, mr, tr]);
	});
var $author$project$MapData$defaultTileZIndex = 0;
var $elm$svg$Svg$Attributes$fill = _VirtualDom_attribute('fill');
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $elm$svg$Svg$Attributes$opacity = _VirtualDom_attribute('opacity');
var $elm$svg$Svg$Attributes$overflow = _VirtualDom_attribute('overflow');
var $elm$svg$Svg$Attributes$points = _VirtualDom_attribute('points');
var $author$project$BasicDrawing$pointsToHexagonPoints = A2(
	$elm$core$List$foldl,
	F2(
		function (v, r) {
			return r + ($elm$core$String$fromFloat(v.x) + (',' + ($elm$core$String$fromFloat(v.y) + ' ')));
		}),
	'');
var $elm$svg$Svg$polygon = $elm$svg$Svg$trustedNode('polygon');
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $elm$svg$Svg$Attributes$stroke = _VirtualDom_attribute('stroke');
var $elm$svg$Svg$Attributes$strokeWidth = _VirtualDom_attribute('stroke-width');
var $author$project$Faction$factionColor = function (faction) {
	switch (faction.$) {
		case 'Faction1':
			return '#ff4c4c';
		case 'Faction2':
			return 'blue';
		case 'Faction3':
			return 'green';
		default:
			return 'yellow';
	}
};
var $author$project$Map$terrainToColor = function (t) {
	switch (t.$) {
		case 'Grass':
			return '#00cd00';
		case 'Water':
			return '#4ca1d2';
		case 'Forest':
			return '#008000';
		default:
			return '#ba8f30';
	}
};
var $author$project$Map$Drawer$styleMapTile = function (tile) {
	return {
		backgroundColor: $author$project$Map$terrainToColor(tile.terrain),
		strokeColor: $author$project$Faction$factionColor(tile.faction),
		strokeWidth: '2px'
	};
};
var $author$project$Map$Drawer$showMapTile = function (tile) {
	var tileDesign = $author$project$Map$Drawer$styleMapTile(tile);
	return A2(
		$author$project$MapAction$Model$SvgItem,
		$author$project$MapData$defaultTileZIndex,
		A2(
			$elm$svg$Svg$polygon,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$Click(tile.indices)),
					$elm$svg$Svg$Attributes$overflow('visible'),
					$elm$svg$Svg$Attributes$fill(tileDesign.backgroundColor),
					$elm$svg$Svg$Attributes$stroke(tileDesign.strokeColor),
					$elm$svg$Svg$Attributes$strokeWidth(tileDesign.strokeWidth),
					$elm$svg$Svg$Attributes$points(
					$author$project$BasicDrawing$pointsToHexagonPoints(
						A2($author$project$BasicDrawing$calculateHexagonPoints, tile.point, $author$project$MapData$hexRadius))),
					$elm$svg$Svg$Attributes$opacity('0.7')
				]),
			A3(
				$author$project$MaybeExt$foldMaybe,
				$elm$core$List$singleton,
				_List_Nil,
				$author$project$Map$Drawer$getImageForTile(tile))));
};
var $author$project$Map$Drawer$tileToClickAction = function (tile) {
	return A2(
		$elm$core$List$cons,
		A2(
			$author$project$MapAction$Model$InteractableSvg,
			$author$project$Map$Drawer$showMapTile(tile),
			$author$project$Map$Drawer$getMapTileAction(tile)),
		A3(
			$author$project$MaybeExt$foldMaybe,
			function (img) {
				return _List_fromArray(
					[
						A2($author$project$MapAction$Model$InteractableSvg, img, _List_Nil)
					]);
			},
			_List_Nil,
			$author$project$Map$Drawer$getImageItemForTile(tile)));
};
var $author$project$Map$Drawer$drawMap = function (m) {
	return A2(
		$elm$core$Dict$map,
		F2(
			function (_v0, tile) {
				return $author$project$Map$Drawer$tileToClickAction(tile);
			}),
		m);
};
var $author$project$Main$drawnMap = function (map) {
	return $author$project$Map$Drawer$drawMap(map);
};
var $author$project$Pathfinder$Model$Path = F2(
	function (target, path) {
		return {path: path, target: target};
	});
var $author$project$Pathfinder$Model$PathPart = function (a) {
	return {$: 'PathPart', a: a};
};
var $author$project$Pathfinder$totalDistance = function (_v0) {
	var p = _v0.a;
	return p.previousDistance + p.minDistanceToTarget;
};
var $author$project$Pathfinder$addSortedPathTail = F2(
	function (tails, p) {
		if (!tails.b) {
			return _List_fromArray(
				[p]);
		} else {
			var t = tails.a;
			var ts = tails.b;
			return (_Utils_cmp(
				$author$project$Pathfinder$totalDistance(p),
				$author$project$Pathfinder$totalDistance(t)) < 1) ? A2(
				$elm$core$List$cons,
				p,
				A2($elm$core$List$cons, t, ts)) : A2(
				$elm$core$List$cons,
				t,
				A2($author$project$Pathfinder$addSortedPathTail, ts, p));
		}
	});
var $author$project$Pathfinder$closestPath = F2(
	function (tails, dict) {
		closestPath:
		while (true) {
			if (!tails.b) {
				return _Utils_Tuple2(_List_Nil, $elm$core$Maybe$Nothing);
			} else {
				var p = tails.a.a;
				var ps = tails.b;
				if (A2(
					$elm$core$Dict$member,
					$author$project$MapData$hashMapPoint(p.position),
					dict)) {
					var $temp$tails = ps,
						$temp$dict = dict;
					tails = $temp$tails;
					dict = $temp$dict;
					continue closestPath;
				} else {
					return _Utils_Tuple2(
						ps,
						$elm$core$Maybe$Just(
							$author$project$Pathfinder$Model$PathPart(p)));
				}
			}
		}
	});
var $author$project$Pathfinder$createPathPart = F3(
	function (p, parent, info) {
		return $author$project$Pathfinder$Model$PathPart(
			{
				minDistanceToTarget: A2(info.nav.getMinDistanceBetween, p, info.target),
				parent: parent,
				position: p,
				previousDistance: A3(
					$author$project$MaybeExt$foldMaybe,
					function (_v0) {
						var part = _v0.a;
						return A2(
							$elm$core$Maybe$withDefault,
							9999,
							info.nav.timeToCrossField(p)) + part.previousDistance;
					},
					0,
					parent)
			});
	});
var $author$project$Vector$pointEqual = F2(
	function (p1, p2) {
		return _Utils_eq(p1.x, p2.x) && _Utils_eq(p1.y, p2.y);
	});
var $author$project$Pathfinder$Model$PathTile = F2(
	function (indices, timeLoss) {
		return {indices: indices, timeLoss: timeLoss};
	});
var $author$project$Pathfinder$pathTimeLoss = function (_v0) {
	var p = _v0.a;
	return A3(
		$author$project$MaybeExt$foldMaybe,
		function (_v1) {
			var parent = _v1.a;
			return p.previousDistance - parent.previousDistance;
		},
		0,
		p.parent);
};
var $author$project$Pathfinder$pathPartToTile = function (_v0) {
	var p = _v0.a;
	return A2(
		$author$project$Pathfinder$Model$PathTile,
		p.position,
		$author$project$Pathfinder$pathTimeLoss(
			$author$project$Pathfinder$Model$PathPart(p)));
};
var $author$project$Pathfinder$toPath_ = F2(
	function (_v0, ps) {
		var p = _v0.a;
		return A3(
			$author$project$MaybeExt$foldMaybe,
			function (parent) {
				return A2(
					$author$project$Pathfinder$toPath_,
					parent,
					A2(
						$elm$core$List$cons,
						$author$project$Pathfinder$pathPartToTile(
							$author$project$Pathfinder$Model$PathPart(p)),
						ps));
			},
			A2(
				$elm$core$List$cons,
				$author$project$Pathfinder$pathPartToTile(
					$author$project$Pathfinder$Model$PathPart(p)),
				ps),
			p.parent);
	});
var $author$project$Pathfinder$toPath = function (p) {
	return A2($author$project$Pathfinder$toPath_, p, _List_Nil);
};
var $author$project$Pathfinder$buildPath = F4(
	function (_v0, tails, dict, info) {
		buildPath:
		while (true) {
			var closest = _v0.a;
			if (A2($author$project$Vector$pointEqual, closest.position, info.target)) {
				return $elm$core$Maybe$Just(
					A2(
						$author$project$Pathfinder$Model$Path,
						info.target,
						$author$project$Pathfinder$toPath(
							$author$project$Pathfinder$Model$PathPart(closest))));
			} else {
				var circumjacent = A2(
					$elm$core$List$filter,
					function (p) {
						return !A2(
							$elm$core$Dict$member,
							$author$project$MapData$hashMapPoint(p),
							dict);
					},
					A2(info.nav.getCircumjacentFields, closest.position, false));
				var tails2 = A3(
					$elm$core$List$foldl,
					F2(
						function (p, ts2) {
							return A2(
								$author$project$Pathfinder$addSortedPathTail,
								ts2,
								A3(
									$author$project$Pathfinder$createPathPart,
									p,
									$elm$core$Maybe$Just(
										$author$project$Pathfinder$Model$PathPart(closest)),
									info));
						}),
					tails,
					circumjacent);
				var _v1 = A2($author$project$Pathfinder$closestPath, tails2, dict);
				if (_v1.b.$ === 'Nothing') {
					var _v2 = _v1.b;
					return $elm$core$Maybe$Nothing;
				} else {
					var newTails = _v1.a;
					var newClosest = _v1.b.a.a;
					var $temp$_v0 = $author$project$Pathfinder$Model$PathPart(newClosest),
						$temp$tails = tails2,
						$temp$dict = A3(
						$elm$core$Dict$insert,
						$author$project$MapData$hashMapPoint(newClosest.position),
						_Utils_Tuple0,
						dict),
						$temp$info = info;
					_v0 = $temp$_v0;
					tails = $temp$tails;
					dict = $temp$dict;
					info = $temp$info;
					continue buildPath;
				}
			}
		}
	});
var $elm$core$Dict$singleton = F2(
	function (key, value) {
		return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
	});
var $author$project$Pathfinder$getPath = F2(
	function (from, info) {
		return A4(
			$author$project$Pathfinder$buildPath,
			A3($author$project$Pathfinder$createPathPart, from, $elm$core$Maybe$Nothing, info),
			_List_Nil,
			A2(
				$elm$core$Dict$singleton,
				$author$project$MapData$hashMapPoint(from),
				_Utils_Tuple0),
			info);
	});
var $author$project$Main$getPathTo = F3(
	function (from, to, map) {
		return A2(
			$author$project$Main$canMoveToPoint,
			$author$project$Main$drawnMap(map),
			to) ? A2(
			$author$project$Pathfinder$getPath,
			from,
			A2(
				$author$project$Pathfinder$Model$PathInfo,
				$author$project$Pathfinder$getNav(map),
				to)) : $elm$core$Maybe$Nothing;
	});
var $author$project$PathAgent$followPath = F2(
	function (l, _v0) {
		followPath:
		while (true) {
			var used = _v0.a;
			var p = _v0.b;
			if (!l.b) {
				return _Utils_Tuple2(used, p);
			} else {
				var _v2 = l.a;
				var t = _v2.a;
				var turn = _v2.b;
				var ts = l.b;
				if (turn > 1) {
					return _Utils_Tuple2(used, p);
				} else {
					var $temp$l = ts,
						$temp$_v0 = _Utils_Tuple2(t.timeLoss + used, t.indices);
					l = $temp$l;
					_v0 = $temp$_v0;
					continue followPath;
				}
			}
		}
	});
var $author$project$PathAgent$newMoveSimulator = F2(
	function (move, usedMove) {
		return {maxMove: move, turn: 1, turnUsedMove: usedMove};
	});
var $author$project$PathAgent$moveSimulatorFromAgent = function (a) {
	return A2($author$project$PathAgent$newMoveSimulator, a.speed, a.usedMovement);
};
var $author$project$PathAgent$canReachInRound = F3(
	function (speed, usedSpeed, distance) {
		return (!usedSpeed) || (_Utils_cmp(speed - usedSpeed, distance) > -1);
	});
var $author$project$PathAgent$simulateDistance = F2(
	function (f, sim) {
		return A3($author$project$PathAgent$canReachInRound, sim.maxMove, sim.turnUsedMove, f) ? _Utils_update(
			sim,
			{turnUsedMove: sim.turnUsedMove + f}) : _Utils_update(
			sim,
			{turn: sim.turn + 1, turnUsedMove: f});
	});
var $author$project$PathAgent$pathPartsToTime = F2(
	function (a, ts) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (t, _v0) {
					var r = _v0.a;
					var sim = _v0.b;
					var simulator = A2($author$project$PathAgent$simulateDistance, t.timeLoss, sim);
					return _Utils_Tuple2(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(t, simulator.turn),
							r),
						simulator);
				}),
			_Utils_Tuple2(
				_List_Nil,
				$author$project$PathAgent$moveSimulatorFromAgent(a)),
			ts).a;
	});
var $author$project$PathAgent$moveAlongPath = F3(
	function (p, start, a) {
		return A2(
			$author$project$PathAgent$followPath,
			$elm$core$List$reverse(
				A2($author$project$PathAgent$pathPartsToTime, a, p.path)),
			_Utils_Tuple2(a.usedMovement, start));
	});
var $author$project$PathAgent$setUsedMovement = F2(
	function (f, a) {
		return _Utils_update(
			a,
			{usedMovement: f});
	});
var $author$project$Main$LordView = function (a) {
	return {$: 'LordView', a: a};
};
var $author$project$Main$updateLordAction = F3(
	function (msg, lord, m) {
		if (msg.$ === 'ViewLord') {
			return _Utils_update(
				m,
				{
					gameState: $author$project$Main$GameSetup(
						$author$project$Main$LordView(lord))
				});
		} else {
			return _Utils_update(
				m,
				{
					gameState: $author$project$Main$GameSetup(
						$author$project$Main$BattleView(
							{
								attacker: $author$project$Main$getPlayer(m),
								attackerCasualties: $author$project$Troops$emptyTroops,
								defender: lord,
								defenderCasualties: $author$project$Troops$emptyTroops,
								finished: false,
								round: 1,
								settlement: $elm$core$Maybe$Nothing,
								siege: false
							}))
				});
		}
	});
var $author$project$Main$updateMaptileAction = F2(
	function (model, ma) {
		switch (ma.$) {
			case 'LordMsg':
				var msg = ma.a;
				var lord = ma.b;
				return A3($author$project$Main$updateLordAction, msg, lord, model);
			case 'SettlementMsg':
				var msg = ma.a;
				var settlement = ma.b;
				switch (msg.$) {
					case 'ViewSettlement':
						return _Utils_update(
							model,
							{
								gameState: $author$project$Main$GameSetup(
									A3(
										$author$project$Main$SettlementView,
										$author$project$Main$getPlayer(model),
										settlement,
										$author$project$Msg$RestrictedView))
							});
					case 'EnterSettlement':
						return _Utils_update(
							model,
							{
								gameState: $author$project$Main$GameSetup(
									A3(
										$author$project$Main$SettlementView,
										$author$project$Main$getPlayer(model),
										settlement,
										$author$project$Msg$StandardView))
							});
					default:
						var defender = A2(
							$author$project$Entities$findLordWithSettlement,
							settlement,
							$author$project$Entities$flattenLordList(model.lords));
						if (defender.$ === 'Nothing') {
							return model;
						} else {
							var l = defender.a;
							return _Utils_update(
								model,
								{
									gameState: $author$project$Main$GameSetup(
										$author$project$Main$BattleView(
											{
												attacker: $author$project$Main$getPlayer(model),
												attackerCasualties: $author$project$Troops$emptyTroops,
												defender: l,
												defenderCasualties: $author$project$Troops$emptyTroops,
												finished: false,
												round: 1,
												settlement: $elm$core$Maybe$Just(settlement),
												siege: true
											}))
								});
						}
				}
			default:
				var p = ma.a;
				var player = $author$project$Main$getPlayer(model);
				var _v3 = A3($author$project$Main$getPathTo, player.entity.position, p, model.map);
				if (_v3.$ === 'Nothing') {
					return model;
				} else {
					var path = _v3.a;
					var _v4 = A3($author$project$PathAgent$moveAlongPath, path, player.entity.position, player.agent);
					var usedMove = _v4.a;
					var point = _v4.b;
					var newPlayer = _Utils_update(
						player,
						{
							agent: A2($author$project$PathAgent$setUsedMovement, usedMove, player.agent),
							entity: A2($author$project$Entities$setPosition, player.entity, point)
						});
					return _Utils_update(
						model,
						{
							lords: A2(
								$author$project$Entities$Cons,
								newPlayer,
								$author$project$Entities$npcs(model.lords))
						});
				}
		}
	});
var $author$project$Main$Campaign = {$: 'Campaign'};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Ports$startMusic = _Platform_outgoingPort('startMusic', $elm$json$Json$Encode$string);
var $author$project$Main$updateMenue = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StartGame':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup($author$project$Main$GameMenue)
						}),
					$author$project$Ports$startMusic('play'));
			case 'ShowMenue':
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup(
								$author$project$Main$MainMenue($author$project$Main$Menue))
						}));
			case 'ShowDocumentation':
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup(
								$author$project$Main$MainMenue($author$project$Main$Menue))
						}));
			case 'SetCampaingn':
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup(
								$author$project$Main$MainMenue($author$project$Main$Campaign))
						}));
			default:
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup(
								$author$project$Main$MainMenue($author$project$Main$Menue))
						}));
		}
	});
var $author$project$Msg$BuildingView = {$: 'BuildingView'};
var $author$project$Msg$RecruitView = {$: 'RecruitView'};
var $author$project$Msg$StationView = {$: 'StationView'};
var $author$project$Entities$getPossibleTroopAmount = F2(
	function (army, t) {
		var _v0 = A2(
			$elm$core$Dict$get,
			$author$project$Troops$troopTypeToInt(t),
			army);
		if (_v0.$ === 'Nothing') {
			return 0;
		} else {
			var amount = _v0.a;
			return A2($elm$core$Basics$min, 5, amount);
		}
	});
var $author$project$Troops$troopCost = function (t) {
	switch (t.$) {
		case 'Archer':
			return 50;
		case 'Spear':
			return 45;
		case 'Sword':
			return 60;
		default:
			return 120;
	}
};
var $author$project$Entities$updateSettlementRecruits = F3(
	function (s, t, add) {
		return $elm$core$List$map(
			function (x) {
				return _Utils_update(
					x,
					{
						recruitLimits: _Utils_eq(x.entity.name, s.entity.name) ? A2(
							$elm$core$Dict$map,
							F2(
								function (k, v) {
									return _Utils_eq(
										k,
										$author$project$Troops$troopTypeToInt(t)) ? (v + add) : v;
								}),
							x.recruitLimits) : x.recruitLimits
					});
			});
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$Troops$updateTroops = F3(
	function (army, t, i) {
		return A3(
			$elm$core$Dict$update,
			$author$project$Troops$troopTypeToInt(t),
			function (v) {
				return $elm$core$Maybe$Just(
					A2($elm$core$Maybe$withDefault, 0, v) + i);
			},
			army);
	});
var $author$project$Entities$buyTroops = F3(
	function (l, t, s) {
		var amount = A2($author$project$Entities$getPossibleTroopAmount, s.recruitLimits, t);
		return _Utils_update(
			l,
			{
				entity: A2(
					$author$project$Entities$updateEntitiesArmy,
					A3($author$project$Troops$updateTroops, l.entity.army, t, amount),
					l.entity),
				gold: l.gold - ((((100.0 - A2($author$project$Building$resolveBonusFromBuildings, s.buildings, $author$project$Building$Fortress)) / 100) * $author$project$Troops$troopCost(t)) * (amount / 5.0)),
				land: A4($author$project$Entities$updateSettlementRecruits, s, t, -amount, l.land)
			});
	});
var $author$project$Entities$updateSettlementTroops = F4(
	function (l, s, t, a) {
		return A2(
			$elm$core$List$map,
			function (x) {
				return _Utils_update(
					x,
					{
						entity: A3(
							$author$project$OperatorExt$ternary,
							_Utils_eq(x.entity.name, s),
							A2(
								$author$project$Entities$updateEntitiesArmy,
								A3($author$project$Troops$updateTroops, x.entity.army, t, a),
								x.entity),
							x.entity)
					});
			},
			l);
	});
var $author$project$Entities$stationTroops = F3(
	function (l, t, s) {
		var amount = A2($author$project$Entities$getPossibleTroopAmount, l.entity.army, t);
		return _Utils_update(
			l,
			{
				entity: A2(
					$author$project$Entities$updateEntitiesArmy,
					A3($author$project$Troops$updateTroops, l.entity.army, t, (-1) * amount),
					l.entity),
				land: A4($author$project$Entities$updateSettlementTroops, l.land, s.entity.name, t, amount)
			});
	});
var $author$project$Entities$takeTroops = F3(
	function (l, t, s) {
		var amount = A2($author$project$Entities$getPossibleTroopAmount, s.entity.army, t);
		return _Utils_update(
			l,
			{
				entity: A2(
					$author$project$Entities$updateEntitiesArmy,
					A3($author$project$Troops$updateTroops, l.entity.army, t, amount),
					l.entity),
				land: A4($author$project$Entities$updateSettlementTroops, l.land, s.entity.name, t, (-1) * amount)
			});
	});
var $author$project$Entities$getSettlementByName = F2(
	function (l, s) {
		var _v0 = A2(
			$elm$core$List$filter,
			function (x) {
				return _Utils_eq(x.entity.name, s);
			},
			l);
		if (!_v0.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var x = _v0.a;
			return $elm$core$Maybe$Just(x);
		}
	});
var $author$project$Main$updateMultipleTroopStats = F4(
	function (l, s, u, m) {
		var newSettle = A2(
			$author$project$Entities$getSettlementByName,
			$author$project$Entities$getPlayer(l).land,
			s.entity.name);
		if (newSettle.$ === 'Nothing') {
			return m;
		} else {
			var x = newSettle.a;
			return _Utils_update(
				m,
				{
					gameState: $author$project$Main$GameSetup(
						A3(
							$author$project$Main$SettlementView,
							$author$project$Entities$getPlayer(l),
							x,
							u)),
					lords: l
				});
		}
	});
var $author$project$Entities$updatePlayer = F2(
	function (_v0, np) {
		var ps = _v0.b;
		return A2($author$project$Entities$Cons, np, ps);
	});
var $author$project$Building$upgradeBuildingType = F2(
	function (b, bt) {
		return A2(
			$elm$core$List$map,
			function (x) {
				return _Utils_update(
					x,
					{
						level: A3(
							$author$project$OperatorExt$ternary,
							_Utils_eq(x.buildingType, bt),
							x.level + 1,
							x.level)
					});
			},
			b);
	});
var $author$project$Entities$updateSettlementBuildings = F3(
	function (l, s, b) {
		return A2(
			$elm$core$List$map,
			function (x) {
				return _Utils_update(
					x,
					{
						buildings: A3(
							$author$project$OperatorExt$ternary,
							_Utils_eq(x.entity.name, s),
							A2($author$project$Building$upgradeBuildingType, x.buildings, b),
							x.buildings)
					});
			},
			l);
	});
var $author$project$Building$upgradeCostBase = function (b) {
	switch (b.$) {
		case 'Marketplace':
			return 550;
		case 'Barracks':
			return 350;
		default:
			return 500;
	}
};
var $author$project$Entities$upgradeBuilding = F3(
	function (l, b, s) {
		return _Utils_update(
			l,
			{
				gold: l.gold - ($author$project$Building$upgradeCostBase(b.buildingType) * (b.level + 1)),
				land: A3($author$project$Entities$updateSettlementBuildings, l.land, s.entity.name, b.buildingType)
			});
	});
var $author$project$Main$updateSettlementStats = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'BuyTroops':
				var t = msg.a;
				var s = msg.b;
				return A4(
					$author$project$Main$updateMultipleTroopStats,
					A2(
						$author$project$Entities$updatePlayer,
						model.lords,
						A3(
							$author$project$Entities$buyTroops,
							$author$project$Entities$getPlayer(model.lords),
							t,
							s)),
					s,
					$author$project$Msg$RecruitView,
					model);
			case 'StationTroops':
				var t = msg.a;
				var s = msg.b;
				return A4(
					$author$project$Main$updateMultipleTroopStats,
					A2(
						$author$project$Entities$updatePlayer,
						model.lords,
						A3(
							$author$project$Entities$stationTroops,
							$author$project$Entities$getPlayer(model.lords),
							t,
							s)),
					s,
					$author$project$Msg$StationView,
					model);
			case 'TakeTroops':
				var t = msg.a;
				var s = msg.b;
				return A4(
					$author$project$Main$updateMultipleTroopStats,
					A2(
						$author$project$Entities$updatePlayer,
						model.lords,
						A3(
							$author$project$Entities$takeTroops,
							$author$project$Entities$getPlayer(model.lords),
							t,
							s)),
					s,
					$author$project$Msg$StationView,
					model);
			default:
				var b = msg.a;
				var s = msg.b;
				return A4(
					$author$project$Main$updateMultipleTroopStats,
					A2(
						$author$project$Entities$updatePlayer,
						model.lords,
						A3(
							$author$project$Entities$upgradeBuilding,
							$author$project$Entities$getPlayer(model.lords),
							b,
							s)),
					s,
					$author$project$Msg$BuildingView,
					model);
		}
	});
var $author$project$Main$updateSettlementUI = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ShowBuyTroops':
				var s = msg.a;
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							A3(
								$author$project$Main$SettlementView,
								$author$project$Entities$getPlayer(model.lords),
								s,
								$author$project$Msg$RecruitView))
					});
			case 'ShowStationTroops':
				var s = msg.a;
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							A3(
								$author$project$Main$SettlementView,
								$author$project$Entities$getPlayer(model.lords),
								s,
								$author$project$Msg$StationView))
					});
			case 'ShowSettlement':
				var s = msg.a;
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							A3(
								$author$project$Main$SettlementView,
								$author$project$Entities$getPlayer(model.lords),
								s,
								$author$project$Msg$StandardView))
					});
			default:
				var s = msg.a;
				return _Utils_update(
					model,
					{
						gameState: $author$project$Main$GameSetup(
							A3(
								$author$project$Main$SettlementView,
								$author$project$Entities$getPlayer(model.lords),
								s,
								$author$project$Msg$BuildingView))
					});
		}
	});
var $author$project$Main$updateSettlement = F2(
	function (msg, model) {
		if (msg.$ === 'UIMsg') {
			var umsg = msg.a;
			return A2($author$project$Main$updateSettlementUI, umsg, model);
		} else {
			var tmsg = msg.a;
			return A2($author$project$Main$updateSettlementStats, tmsg, model);
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'EndRound':
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							date: A2($author$project$DateExt$addMonths, 1, model.date),
							lords: A2(
								$author$project$Entities$Cons,
								$author$project$Main$endRoundForLord(
									$author$project$Main$getPlayer(model)),
								$author$project$Main$updateAIsAfterPlayerRound(
									$author$project$Entities$npcs(model.lords)))
						}));
			case 'EndGame':
				var bool = msg.a;
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameOver(bool)
						}));
			case 'CloseModal':
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							gameState: $author$project$Main$GameSetup($author$project$Main$GameMenue)
						}));
			case 'BattleAction':
				var bmsg = msg.a;
				return $author$project$Main$appendCmd(
					A2($author$project$Main$updateBattle, bmsg, model));
			case 'MenueAction':
				var mmsg = msg.a;
				return A2($author$project$Main$updateMenue, mmsg, model);
			case 'SettlementAction':
				var action = msg.a;
				return $author$project$Main$appendCmd(
					A2($author$project$Main$updateSettlement, action, model));
			case 'EventAction':
				var emsg = msg.a;
				return $author$project$Main$appendCmd(
					A2($author$project$Main$updateEvent, emsg, model));
			case 'MapTileAction':
				var action = msg.a;
				return $author$project$Main$appendCmd(
					A2($author$project$Main$updateMaptileAction, model, action));
			default:
				var p = msg.a;
				return $author$project$Main$appendCmd(
					_Utils_update(
						model,
						{
							selectedPoint: $elm$core$Maybe$Just(p)
						}));
		}
	});
var $elm$html$Html$audio = _VirtualDom_node('audio');
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $author$project$Main$addStylesheet = function (href) {
	return A3(
		$elm$html$Html$node,
		'link',
		_List_fromArray(
			[
				A2($elm$html$Html$Attributes$attribute, 'Rel', 'stylesheet'),
				A2($elm$html$Html$Attributes$attribute, 'property', 'stylesheet'),
				A2($elm$html$Html$Attributes$attribute, 'href', './assets/styles/' + (href + '.css'))
			]),
		_List_Nil);
};
var $author$project$MapAction$getSvg = function (_v0) {
	var svg = _v0.b;
	return svg;
};
var $author$project$MapAction$allSvgs = function (a) {
	return $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			$elm$core$List$map(
				function (svg) {
					return $author$project$MapAction$getSvg(svg.svg);
				}),
			$elm$core$Dict$values(a)));
};
var $author$project$Main$allSettlements = function (m) {
	return $elm$core$List$concat(
		A2(
			$elm$core$List$map,
			function ($) {
				return $.land;
			},
			$author$project$Entities$flattenLordList(m.lords)));
};
var $author$project$Pathfinder$cutFirstStepFromPath = function (p) {
	return A2(
		$author$project$Pathfinder$Model$Path,
		p.target,
		A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			$elm$core$List$tail(p.path)));
};
var $author$project$MapAction$getZIndex = function (_v0) {
	var i = _v0.a;
	return i;
};
var $author$project$MapAction$addToMap = F2(
	function (k, v) {
		return A2(
			$elm$core$Dict$update,
			k,
			function (vs) {
				return $elm$core$Maybe$Just(
					A3(
						$author$project$MaybeExt$foldMaybe,
						A2(
							$author$project$ListExt$insertToSortedList,
							v,
							function (svg) {
								return $author$project$MapAction$getZIndex(svg.svg);
							}),
						_List_fromArray(
							[v]),
						vs));
			});
	});
var $author$project$Pathfinder$Drawer$getPathPartAction = function (_v0) {
	return _List_Nil;
};
var $author$project$Pathfinder$Drawer$getSvgForPathPart = F2(
	function (i, p) {
		return A3(
			$author$project$BasicDrawing$getImage,
			'./assets/images/letters/' + ($elm$core$String$fromInt(i) + '.png'),
			p,
			1);
	});
var $author$project$MapData$pathZIndex = 5;
var $author$project$Pathfinder$Drawer$showPathPart = F2(
	function (i, p) {
		return A2(
			$author$project$MapAction$Model$SvgItem,
			$author$project$MapData$pathZIndex,
			A2($author$project$Pathfinder$Drawer$getSvgForPathPart, i, p));
	});
var $author$project$Pathfinder$Drawer$drawPathPart = F2(
	function (i, p) {
		return A2(
			$author$project$MapAction$addToMap,
			$author$project$MapData$hashMapPoint(p),
			A2(
				$author$project$MapAction$Model$InteractableSvg,
				A2($author$project$Pathfinder$Drawer$showPathPart, i, p),
				$author$project$Pathfinder$Drawer$getPathPartAction(p)));
	});
var $author$project$Pathfinder$Drawer$drawPath = F3(
	function (agent, path, dict) {
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, newDict) {
					var t = _v0.a;
					var turns = _v0.b;
					return A3(
						$author$project$Pathfinder$Drawer$drawPathPart,
						A2($elm$core$Basics$min, 9, turns),
						t.indices,
						newDict);
				}),
			dict,
			A2($author$project$PathAgent$pathPartsToTime, agent, path.path));
	});
var $author$project$Main$getSelectedPath = function (m) {
	var player = $author$project$Entities$getPlayer(m.lords);
	var _v0 = m.selectedPoint;
	if (_v0.$ === 'Nothing') {
		return $elm$core$Maybe$Nothing;
	} else {
		var point = _v0.a;
		return A3($author$project$Main$getPathTo, player.entity.position, point, m.map);
	}
};
var $author$project$Main$buildPathSvgs = F2(
	function (m, mapDict) {
		var player = $author$project$Entities$getPlayer(m.lords);
		var _v0 = $author$project$Main$getSelectedPath(m);
		if (_v0.$ === 'Nothing') {
			return mapDict;
		} else {
			var path = _v0.a;
			return A3(
				$author$project$Pathfinder$Drawer$drawPath,
				player.agent,
				$author$project$Pathfinder$cutFirstStepFromPath(path),
				mapDict);
		}
	});
var $author$project$MapAction$SubModel$EngageLord = {$: 'EngageLord'};
var $author$project$MapAction$SubModel$LordMsg = F2(
	function (a, b) {
		return {$: 'LordMsg', a: a, b: b};
	});
var $author$project$MapAction$SubModel$ViewLord = {$: 'ViewLord'};
var $author$project$Entities$isLordInOwnSettlement = function (lord) {
	return A2(
		$elm$core$List$any,
		$elm$core$Basics$eq(lord.entity.position),
		A2(
			$elm$core$List$map,
			function (s) {
				return s.entity.position;
			},
			lord.land));
};
var $author$project$Entities$Drawer$getLordAction = F2(
	function (player, lord) {
		var action = (_Utils_eq(player.entity.position, lord.entity.position) && ((!_Utils_eq(lord.entity.name, player.entity.name)) && (!$author$project$Entities$isLordInOwnSettlement(lord)))) ? $elm$core$Maybe$Just($author$project$MapAction$SubModel$EngageLord) : $elm$core$Maybe$Nothing;
		return A2(
			$elm$core$List$cons,
			A2($author$project$MapAction$SubModel$LordMsg, $author$project$MapAction$SubModel$ViewLord, lord),
			A3(
				$author$project$MaybeExt$foldMaybe,
				function (a) {
					return _List_fromArray(
						[
							A2($author$project$MapAction$SubModel$LordMsg, a, lord)
						]);
				},
				_List_Nil,
				action));
	});
var $author$project$Entities$factionToImage = function (fac) {
	switch (fac.$) {
		case 'Faction1':
			return 'faction1.png';
		case 'Faction2':
			return 'faction2.png';
		case 'Faction3':
			return 'faction3.png';
		default:
			return 'faction4.png';
	}
};
var $author$project$Entities$lordToMapIcon = function (l) {
	return './assets/images/profiles/mini/' + $author$project$Entities$factionToImage(l.entity.faction);
};
var $author$project$Entities$Drawer$getSvgForLord = function (l) {
	return A3(
		$author$project$BasicDrawing$getImage,
		$author$project$Entities$lordToMapIcon(l),
		l.entity.position,
		1);
};
var $author$project$MapData$lordZIndex = 9;
var $author$project$Entities$Drawer$showLord = function (lord) {
	return A2(
		$author$project$MapAction$Model$SvgItem,
		$author$project$MapData$lordZIndex,
		$author$project$Entities$Drawer$getSvgForLord(lord));
};
var $author$project$Entities$Drawer$drawLord = F2(
	function (player, l) {
		var drawnLord = A2(
			$author$project$MapAction$Model$InteractableSvg,
			$author$project$Entities$Drawer$showLord(l),
			A2($author$project$Entities$Drawer$getLordAction, player, l));
		return A2(
			$author$project$MapAction$addToMap,
			$author$project$MapData$hashMapPoint(l.entity.position),
			drawnLord);
	});
var $author$project$MapAction$SubModel$EnterSettlement = {$: 'EnterSettlement'};
var $author$project$MapAction$SubModel$SettlementMsg = F2(
	function (a, b) {
		return {$: 'SettlementMsg', a: a, b: b};
	});
var $author$project$MapAction$SubModel$SiegeSettlement = {$: 'SiegeSettlement'};
var $author$project$MapAction$SubModel$ViewSettlement = {$: 'ViewSettlement'};
var $author$project$Entities$Drawer$getSettlementAction = F2(
	function (player, s) {
		var action = _Utils_eq(player.entity.position, s.entity.position) ? (A2(
			$elm$core$List$member,
			s.entity.position,
			A2(
				$elm$core$List$map,
				function (l) {
					return l.entity.position;
				},
				player.land)) ? $elm$core$Maybe$Just($author$project$MapAction$SubModel$EnterSettlement) : $elm$core$Maybe$Just($author$project$MapAction$SubModel$SiegeSettlement)) : $elm$core$Maybe$Nothing;
		return A2(
			$elm$core$List$cons,
			A2($author$project$MapAction$SubModel$SettlementMsg, $author$project$MapAction$SubModel$ViewSettlement, s),
			A3(
				$author$project$MaybeExt$foldMaybe,
				function (a) {
					return _List_fromArray(
						[
							A2($author$project$MapAction$SubModel$SettlementMsg, a, s)
						]);
				},
				_List_Nil,
				action));
	});
var $author$project$Entities$getSettlementNameByType = function (s) {
	if (s.$ === 'Village') {
		return 'Village';
	} else {
		return 'Castle';
	}
};
var $author$project$Entities$getSettlementImage = function (s) {
	return './assets/images/settlements/' + ($author$project$Entities$getSettlementNameByType(s.settlementType) + '.png');
};
var $author$project$Entities$Drawer$getSvgForSettlement = function (s) {
	return A3(
		$author$project$BasicDrawing$getImage,
		$author$project$Entities$getSettlementImage(s),
		s.entity.position,
		1);
};
var $author$project$MapData$settlementZIndex = 7;
var $author$project$Entities$Drawer$showSettlement = function (s) {
	return A2(
		$author$project$MapAction$Model$SvgItem,
		$author$project$MapData$settlementZIndex,
		$author$project$Entities$Drawer$getSvgForSettlement(s));
};
var $author$project$MapData$settlementStrokeWidth = 5;
var $author$project$Entities$Drawer$getSvgBorderForSettlement = function (s) {
	return A2(
		$elm$svg$Svg$polygon,
		_List_fromArray(
			[
				$elm$svg$Svg$Events$onClick(
				$author$project$Msg$Click(s.entity.position)),
				$elm$svg$Svg$Attributes$overflow('visible'),
				$elm$svg$Svg$Attributes$stroke(
				$author$project$Faction$factionColor(s.entity.faction)),
				$elm$svg$Svg$Attributes$strokeWidth(
				$elm$core$String$fromInt($author$project$MapData$settlementStrokeWidth) + 'px'),
				$elm$svg$Svg$Attributes$points(
				$author$project$BasicDrawing$pointsToHexagonPoints(
					A2(
						$author$project$BasicDrawing$calculateHexagonPoints,
						$author$project$MapData$mapPositionForIndex(s.entity.position),
						$author$project$MapData$hexRadius))),
				$elm$svg$Svg$Attributes$opacity('0.8')
			]),
		_List_Nil);
};
var $author$project$MapData$settlementBorderZIndex = 6;
var $author$project$Entities$Drawer$showSettlementBorder = function (s) {
	return A2(
		$author$project$MapAction$Model$SvgItem,
		$author$project$MapData$settlementBorderZIndex,
		$author$project$Entities$Drawer$getSvgBorderForSettlement(s));
};
var $author$project$Entities$Drawer$drawSettlement = F3(
	function (player, s, dict) {
		var drawnSettlementBorder = A2(
			$author$project$MapAction$Model$InteractableSvg,
			$author$project$Entities$Drawer$showSettlementBorder(s),
			_List_Nil);
		var drawnSettlement = A2(
			$author$project$MapAction$Model$InteractableSvg,
			$author$project$Entities$Drawer$showSettlement(s),
			A2($author$project$Entities$Drawer$getSettlementAction, player, s));
		return A3(
			$author$project$MapAction$addToMap,
			$author$project$MapData$hashMapPoint(s.entity.position),
			drawnSettlementBorder,
			A3(
				$author$project$MapAction$addToMap,
				$author$project$MapData$hashMapPoint(s.entity.position),
				drawnSettlement,
				dict));
	});
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $author$project$MapAction$isZAllowedOn = F2(
	function (z, main) {
		return (((!_Utils_eq(main, $author$project$MapData$lordZIndex)) && (!_Utils_eq(main, $author$project$MapData$settlementZIndex))) || (!_Utils_eq(z, $author$project$MapData$imageTileZIndex))) && ((!_Utils_eq(main, $author$project$MapData$pathZIndex)) || (!_Utils_eq(z, $author$project$MapData$imageTileZIndex)));
	});
var $author$project$MapAction$isZAllowedIn = F2(
	function (i, is) {
		return A2(
			$elm$core$List$all,
			$author$project$MapAction$isZAllowedOn(i),
			is);
	});
var $author$project$MapAction$isSvgAllowedIn = F2(
	function (svg, svgs) {
		return A2(
			$author$project$MapAction$isZAllowedIn,
			$author$project$MapAction$getZIndex(svg.svg),
			A2(
				$elm$core$List$map,
				function (s) {
					return $author$project$MapAction$getZIndex(s.svg);
				},
				svgs));
	});
var $author$project$Main$filterInteractables = A2(
	$elm$core$List$foldr,
	F2(
		function (svg, r) {
			return A2($author$project$MapAction$isSvgAllowedIn, svg, r) ? A2($elm$core$List$cons, svg, r) : r;
		}),
	_List_Nil);
var $author$project$Main$filterMapSvgs = $elm$core$Dict$map(
	F2(
		function (_v0, v) {
			return $author$project$Main$filterInteractables(v);
		}));
var $author$project$Main$buildAllMapSvgs = function (m) {
	return $author$project$Main$filterMapSvgs(
		A2(
			$author$project$Main$buildPathSvgs,
			m,
			A3(
				$elm$core$List$foldl,
				$author$project$Entities$Drawer$drawSettlement(
					$author$project$Main$getPlayer(m)),
				A3(
					$elm$core$List$foldl,
					$author$project$Entities$Drawer$drawLord(
						$author$project$Main$getPlayer(m)),
					$author$project$Main$drawnMap(m.map),
					$author$project$Entities$flattenLordList(m.lords)),
				$author$project$Main$allSettlements(m))));
};
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $author$project$Msg$BattleAction = function (a) {
	return {$: 'BattleAction', a: a};
};
var $author$project$Msg$EndBattle = function (a) {
	return {$: 'EndBattle', a: a};
};
var $author$project$Msg$FleeBattle = function (a) {
	return {$: 'FleeBattle', a: a};
};
var $author$project$Msg$SkipSkirmishes = function (a) {
	return {$: 'SkipSkirmishes', a: a};
};
var $author$project$Msg$StartSkirmish = function (a) {
	return {$: 'StartSkirmish', a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $elm$html$Html$span = _VirtualDom_node('span');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $author$project$Templates$BattleTemplate$generateActionButtonsByState = function (bS) {
	return bS.finished ? _List_fromArray(
		[
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$BattleAction(
						$author$project$Msg$EndBattle(bS)))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Leave battlefield')
						]))
				]))
		]) : _List_fromArray(
		[
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$BattleAction(
						$author$project$Msg$StartSkirmish(bS)))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Start skirmish')
						]))
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$BattleAction(
						$author$project$Msg$SkipSkirmishes(bS)))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Skip skirmishes')
						]))
				])),
			A2(
			$elm$html$Html$button,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$BattleAction(
						$author$project$Msg$FleeBattle(bS)))
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text('Flee battle')
						]))
				]))
		]);
};
var $elm$html$Html$img = _VirtualDom_node('img');
var $author$project$Templates$HelperTemplate$roundDigits = function (v) {
	var parts = A2(
		$elm$core$String$split,
		'.',
		$elm$core$String$fromFloat(v));
	if (!parts.b) {
		return '0.00';
	} else {
		if (!parts.b.b) {
			var x = parts.a;
			return x + '.00';
		} else {
			var x = parts.a;
			var _v1 = parts.b;
			var xs = _v1.a;
			return x + ('.' + A2($elm$core$String$left, 2, xs));
		}
	}
};
var $elm$html$Html$Attributes$src = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'src',
		_VirtualDom_noJavaScriptOrHtmlUri(url));
};
var $author$project$Templates$BattleTemplate$generateSettlementBonus = function (bS) {
	var _v0 = bS.settlement;
	if (_v0.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		var settle = _v0.a;
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('battle-terrain-bonus')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							$author$project$Entities$getSettlementImage(settle))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'+' + ($author$project$Templates$HelperTemplate$roundDigits(
								(A2($author$project$Entities$getSettlementBonus, settle, bS.defender.land) * 100) - 100) + '%'))
						]))
				]));
	}
};
var $author$project$Templates$BattleTemplate$generateStatusText = function (bS) {
	return bS.finished ? A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				A3(
					$author$project$OperatorExt$ternary,
					!$author$project$Troops$sumTroops(bS.attacker.entity.army),
					'negative-income battle-skirmish-text',
					'positive-income battle-skirmish-text'))
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				A3(
					$author$project$OperatorExt$ternary,
					!$author$project$Troops$sumTroops(bS.attacker.entity.army),
					'My lord, we have lost, we will return to our castle!',
					'My lord, we were victorious, we repeled them!'))
			])) : A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('battle-skirmish-text')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				'Skirmish-Round: ' + $elm$core$String$fromInt(bS.round))
			]));
};
var $elm$core$String$toLower = _String_toLower;
var $author$project$Troops$troopName = function (t) {
	switch (t.$) {
		case 'Archer':
			return 'Archer';
		case 'Spear':
			return 'Spear';
		case 'Sword':
			return 'Sword';
		default:
			return 'Knight';
	}
};
var $author$project$Templates$BattleTemplate$generateTerrainBonuses = function (t) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('battle-terrain-bonus')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$img,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$src(
						'./assets/images/troops/' + ($elm$core$String$toLower(
							$author$project$Troops$troopName(t)) + '.png'))
					]),
				_List_Nil),
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						'+' + ($author$project$Templates$HelperTemplate$roundDigits(
							($author$project$Troops$battlefieldBonus(t) * 100) - 100) + '%'))
					]))
			]));
};
var $author$project$Map$terrainToName = function (t) {
	switch (t.$) {
		case 'Grass':
			return 'Grass';
		case 'Water':
			return 'Water';
		case 'Forest':
			return 'Forest';
		default:
			return 'Mountain';
	}
};
var $author$project$Templates$BattleTemplate$generateActionOverview = F2(
	function (bS, ter) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('battle-action-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('battle-terrain-info')
						]),
					_Utils_ap(
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Battlefield-Terrain')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$src('./assets/images/map/tree_image_color.png')
											]),
										_List_Nil),
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												$author$project$Map$terrainToName(ter))
											]))
									]))
							]),
						_Utils_ap(
							A2(
								$elm$core$List$map,
								$author$project$Templates$BattleTemplate$generateTerrainBonuses,
								$author$project$Map$terrainToBonus(ter)),
							_List_fromArray(
								[
									$author$project$Templates$BattleTemplate$generateSettlementBonus(bS)
								])))),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('battle-versus-text')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text('VS.')
						])),
					$author$project$Templates$BattleTemplate$generateStatusText(bS),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					$author$project$Templates$BattleTemplate$generateActionButtonsByState(bS))
				]));
	});
var $author$project$Templates$BattleTemplate$generateTroopOverview = F3(
	function (troop, amount, casuAmount) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('battle-troop-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							'./assets/images/troops/' + ($elm$core$String$toLower(
								$author$project$Troops$troopName(troop)) + '.png'))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(amount) + ('  ' + $author$project$Troops$troopName(troop)))
						])),
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('battle-troop-casualties')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3(
								$author$project$OperatorExt$ternary,
								casuAmount < 0,
								'( ' + ($elm$core$String$fromInt(casuAmount) + ('  ' + ($author$project$Troops$troopName(troop) + ')'))),
								' '))
						]))
				]));
	});
var $author$project$Templates$BattleTemplate$generateArmyOverview = F3(
	function (we, image, casu) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('battle-army-overview')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(image)
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(we.name)
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					A6(
						$elm$core$Dict$merge,
						F3(
							function (k, v1, r) {
								return A2(
									$elm$core$List$cons,
									A3(
										$author$project$Templates$BattleTemplate$generateTroopOverview,
										$author$project$Troops$intToTroopType(k),
										v1,
										0),
									r);
							}),
						F4(
							function (k, v1, v2, r) {
								return A2(
									$elm$core$List$cons,
									A3(
										$author$project$Templates$BattleTemplate$generateTroopOverview,
										$author$project$Troops$intToTroopType(k),
										v1,
										v2),
									r);
							}),
						F3(
							function (k, v2, r) {
								return A2(
									$elm$core$List$cons,
									A3(
										$author$project$Templates$BattleTemplate$generateTroopOverview,
										$author$project$Troops$intToTroopType(k),
										0,
										v2),
									r);
							}),
						we.army,
						casu,
						_List_Nil))
				]));
	});
var $author$project$Entities$getPlayerImage = function (l) {
	return './assets/images/profiles/' + $author$project$Entities$factionToImage(l.entity.faction);
};
var $author$project$Templates$BattleTemplate$determineBattleMap = F2(
	function (bS, t) {
		if (bS.siege) {
			var _v0 = bS.settlement;
			if (_v0.$ === 'Nothing') {
				return _List_Nil;
			} else {
				var settle = _v0.a;
				return _List_fromArray(
					[
						A3(
						$author$project$Templates$BattleTemplate$generateArmyOverview,
						bS.attacker.entity,
						$author$project$Entities$getPlayerImage(bS.attacker),
						bS.attackerCasualties),
						A2($author$project$Templates$BattleTemplate$generateActionOverview, bS, t),
						A3(
						$author$project$Templates$BattleTemplate$generateArmyOverview,
						settle.entity,
						$author$project$Entities$getSettlementImage(settle),
						bS.defenderCasualties)
					]);
			}
		} else {
			return _List_fromArray(
				[
					A3(
					$author$project$Templates$BattleTemplate$generateArmyOverview,
					bS.attacker.entity,
					$author$project$Entities$getPlayerImage(bS.attacker),
					bS.attackerCasualties),
					A2($author$project$Templates$BattleTemplate$generateActionOverview, bS, t),
					A3(
					$author$project$Templates$BattleTemplate$generateArmyOverview,
					bS.defender.entity,
					$author$project$Entities$getPlayerImage(bS.defender),
					bS.defenderCasualties)
				]);
		}
	});
var $author$project$Templates$BattleTemplate$generateBattleTemplate = F2(
	function (bS, t) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('modal-background')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('battle-modal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('battle-modal-main')
								]),
							A2($author$project$Templates$BattleTemplate$determineBattleMap, bS, t))
						]))
				]));
	});
var $author$project$Templates$EndTemplate$losingDesc = 'My lord our castle was taken, the campaign is a lost cause.';
var $author$project$Templates$EndTemplate$losingTitle = 'We have lost!';
var $author$project$Templates$EndTemplate$winningDesc = 'My lord we have destroyed the last enemy, the campaign is done, we showed them our supremacy.';
var $author$project$Templates$EndTemplate$winningTitle = 'We have won!';
var $author$project$Templates$EndTemplate$generateEndData = function (res) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('end-modal-title')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class(
							A3($author$project$OperatorExt$ternary, res, 'winning-color', 'losing-color'))
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3($author$project$OperatorExt$ternary, res, $author$project$Templates$EndTemplate$winningTitle, $author$project$Templates$EndTemplate$losingTitle))
						]))
				])),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('end-modal-desc')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							A3($author$project$OperatorExt$ternary, res, $author$project$Templates$EndTemplate$winningDesc, $author$project$Templates$EndTemplate$losingDesc))
						]))
				])),
			A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$button,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Go to the main menue')
								]))
						]))
				]))
		]);
};
var $author$project$Templates$EndTemplate$generateEndTemplate = function (bool) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('modal-background')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('end-modal')
					]),
				$author$project$Templates$EndTemplate$generateEndData(bool))
			]));
};
var $author$project$Msg$CloseModal = {$: 'CloseModal'};
var $author$project$Templates$HelperTemplate$troopToHtml = F3(
	function (t, amount, cls) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class(cls)
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							'./assets/images/troops/' + ($elm$core$String$toLower(
								$author$project$Troops$troopName(t)) + '.png'))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(amount) + ('  ' + $author$project$Troops$troopName(t)))
						]))
				]));
	});
var $author$project$Templates$LordTemplate$generateLordTemplate = function (l) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('modal-background')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('lord-modal')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('settlement-modal-close-container')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick($author$project$Msg$CloseModal),
										$elm$html$Html$Attributes$class('settlement-modal-close-btn lord-modal-close-btn')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('X')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('lord-title')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text(l.entity.name)
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('lord-image')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$src(
										'./assets/images/profiles/' + $author$project$Entities$factionToImage(l.entity.faction)),
										$elm$html$Html$Attributes$class('box-shadow')
									]),
								_List_Nil)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('lord-stats')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('lord-data box-shadow')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$src('./assets/images/general/ducats_icon.png')
											]),
										_List_Nil),
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												'Gold: ' + $elm$core$String$fromFloat(l.gold))
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('lord-data box-shadow')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$img,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$src('./assets/images/map/Castle.png')
											]),
										_List_Nil),
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text(
												'Settlements: ' + $elm$core$String$fromInt(
													$elm$core$List$length(l.land)))
											]))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('lord-troops box-shadow')
									]),
								A2(
									$elm$core$List$cons,
									A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('lord-troop-header')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Current-Army')
													]))
											])),
									A3(
										$elm$core$Dict$foldr,
										F3(
											function (k, v, r) {
												return A2(
													$elm$core$List$cons,
													A3(
														$author$project$Templates$HelperTemplate$troopToHtml,
														$author$project$Troops$intToTroopType(k),
														v,
														'lord-troop-container'),
													r);
											}),
										_List_Nil,
										l.entity.army)))
							]))
					]))
			]));
};
var $author$project$Entities$combineSettlementName = function (settlement) {
	return $author$project$Entities$getSettlementNameByType(settlement.settlementType) + (' - ' + settlement.entity.name);
};
var $author$project$Msg$SettlementAction = function (a) {
	return {$: 'SettlementAction', a: a};
};
var $author$project$Msg$ShowBuildings = function (a) {
	return {$: 'ShowBuildings', a: a};
};
var $author$project$Msg$ShowBuyTroops = function (a) {
	return {$: 'ShowBuyTroops', a: a};
};
var $author$project$Msg$ShowSettlement = function (a) {
	return {$: 'ShowSettlement', a: a};
};
var $author$project$Msg$ShowStationTroops = function (a) {
	return {$: 'ShowStationTroops', a: a};
};
var $author$project$Msg$UIMsg = function (a) {
	return {$: 'UIMsg', a: a};
};
var $author$project$Msg$TroopMsg = function (a) {
	return {$: 'TroopMsg', a: a};
};
var $author$project$Msg$UpgradeBuilding = F2(
	function (a, b) {
		return {$: 'UpgradeBuilding', a: a, b: b};
	});
var $elm$json$Json$Encode$bool = _Json_wrap;
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $author$project$Building$buildingToBonusInfo = F2(
	function (b, i) {
		switch (b.$) {
			case 'Marketplace':
				return '+' + ($elm$core$String$fromFloat(
					i * $author$project$Building$buildingToBonus(b)) + ' Ducats per turn');
			case 'Barracks':
				return '+' + ($elm$core$String$fromFloat(
					i * $author$project$Building$buildingToBonus(b)) + ' Recruits per turn');
			default:
				return '-' + ($elm$core$String$fromFloat(
					i * $author$project$Building$buildingToBonus(b)) + '% Troopcost');
		}
	});
var $author$project$Templates$SettlementTemplate$displayBuildingBonus = function (_v0) {
	var b = _v0.a;
	var i = _v0.b;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('buildings-info-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						A3(
							$author$project$OperatorExt$ternary,
							_Utils_cmp(b.level, i) > -1,
							'positive-income',
							'negative-income'))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						'Level ' + ($elm$core$String$fromInt(i) + ':'))
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						A3(
							$author$project$OperatorExt$ternary,
							_Utils_cmp(b.level, i) > -1,
							'positive-income',
							'negative-income'))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A2($author$project$Building$buildingToBonusInfo, b.buildingType, i))
					])),
				A2(
				$elm$html$Html$span,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class(
						A3(
							$author$project$OperatorExt$ternary,
							_Utils_cmp(b.level, i) > -1,
							'positive-income',
							'negative-income'))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text(
						A3(
							$author$project$OperatorExt$ternary,
							i >= 1,
							'Cost: ' + $elm$core$String$fromFloat(
								$author$project$Building$upgradeCostBase(b.buildingType) * i),
							''))
					]))
			]));
};
var $author$project$Templates$SettlementTemplate$validateBuildingUpgrade = F2(
	function (b, l) {
		return !(((l.gold - ($author$project$Building$upgradeCostBase(b.buildingType) * (b.level + 1))) > 0) && (b.level <= 2));
	});
var $author$project$Templates$SettlementTemplate$displayBuildingComponents = function (_v0) {
	var b = _v0.a;
	var l = _v0.b;
	var s = _v0.c;
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('settlement-building-component')
			]),
		_List_fromArray(
			[
				A2($elm$html$Html$div, _List_Nil, _List_Nil),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(b.name)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('tooltip')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('info-icon'),
								$elm$html$Html$Attributes$src('./assets/images/general/info.png')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tooltiptext building-level-tooltip')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Upgrade infos')
									])),
							A2(
								$elm$core$List$map,
								$author$project$Templates$SettlementTemplate$displayBuildingBonus,
								_List_fromArray(
									[
										_Utils_Tuple2(b, 0),
										_Utils_Tuple2(b, 1),
										_Utils_Tuple2(b, 2),
										_Utils_Tuple2(b, 3)
									]))))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$SettlementAction(
									$author$project$Msg$TroopMsg(
										A2($author$project$Msg$UpgradeBuilding, b, s)))),
								$elm$html$Html$Attributes$class(
								A3(
									$author$project$OperatorExt$ternary,
									A2($author$project$Templates$SettlementTemplate$validateBuildingUpgrade, b, l),
									'troop-disabled-button',
									'tooltip')),
								$elm$html$Html$Attributes$disabled(
								A2($author$project$Templates$SettlementTemplate$validateBuildingUpgrade, b, l))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$img,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('troop-station-icon'),
										$elm$html$Html$Attributes$src('./assets/images/general/arrow_up.png')
									]),
								_List_Nil),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('tooltiptext building-upgrade-tooltip')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Upgrade building')
											])),
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('positive-income')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text(
												'Upgrade ' + (b.name + (' to level ' + $elm$core$String$fromInt(b.level + 1))))
											]))
									]))
							]))
					]))
			]));
};
var $author$project$Msg$BuyTroops = F2(
	function (a, b) {
		return {$: 'BuyTroops', a: a, b: b};
	});
var $author$project$Templates$SettlementTemplate$validateBuyTroops = F3(
	function (t, s, l) {
		return !(((l.gold - ($author$project$Troops$troopCost(t) * (1 - (A2($author$project$Building$resolveBonusFromBuildings, s.buildings, $author$project$Building$Fortress) / 100)))) >= 0) && A3(
			$author$project$MaybeExt$foldMaybe,
			function (v) {
				return v > 0;
			},
			false,
			A2(
				$elm$core$Dict$get,
				$author$project$Troops$troopTypeToInt(t),
				s.recruitLimits)));
	});
var $author$project$Templates$SettlementTemplate$generateRecruitTroopContainer = F5(
	function (t, aAmount, sAmount, s, l) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('troop-recruiting-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							'./assets/images/troops/' + ($elm$core$String$toLower(
								$author$project$Troops$troopName(t)) + '.png'))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'[' + ($elm$core$String$fromInt(aAmount) + ']'))
						])),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'[' + ($elm$core$String$fromInt(sAmount) + ']'))
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$String$fromFloat(
										((100.0 - A2($author$project$Building$resolveBonusFromBuildings, s.buildings, $author$project$Building$Fortress)) / 100) * $author$project$Troops$troopCost(t)))
								])),
							A2(
							$elm$html$Html$img,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$src('./assets/images/general/ducats_icon.png')
								]),
							_List_Nil)
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Msg$SettlementAction(
								$author$project$Msg$TroopMsg(
									A2($author$project$Msg$BuyTroops, t, s)))),
							$elm$html$Html$Attributes$class(
							A3(
								$author$project$OperatorExt$ternary,
								A3($author$project$Templates$SettlementTemplate$validateBuyTroops, t, s, l),
								'troop-disabled-button',
								'tooltip')),
							$elm$html$Html$Attributes$disabled(
							A3($author$project$Templates$SettlementTemplate$validateBuyTroops, t, s, l))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('+')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('tooltiptext troop-recruiting-tooltip')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Monthly wage')
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('negative-income')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											'- ' + ($elm$core$String$fromFloat(
												$author$project$Troops$troopWage(t)) + ' Ducats'))
										]))
								]))
						]))
				]));
	});
var $author$project$Msg$StationTroops = F2(
	function (a, b) {
		return {$: 'StationTroops', a: a, b: b};
	});
var $author$project$Msg$TakeTroops = F2(
	function (a, b) {
		return {$: 'TakeTroops', a: a, b: b};
	});
var $author$project$Templates$SettlementTemplate$validateStationTroops = function (amount) {
	return !(amount > 0);
};
var $author$project$Templates$SettlementTemplate$generateStationTroopContainer = F4(
	function (lt, ltAmount, stAmount, sE) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('troop-stationing-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							'./assets/images/troops/' + ($elm$core$String$toLower(
								$author$project$Troops$troopName(lt)) + '.png'))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							'[' + ($elm$core$String$fromInt(ltAmount) + ']'))
						])),
					A2(
					$elm$html$Html$div,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									'[' + ($elm$core$String$fromInt(stAmount) + ']'))
								]))
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Msg$SettlementAction(
								$author$project$Msg$TroopMsg(
									A2($author$project$Msg$TakeTroops, lt, sE)))),
							$elm$html$Html$Attributes$class(
							A3(
								$author$project$OperatorExt$ternary,
								$author$project$Templates$SettlementTemplate$validateStationTroops(stAmount),
								'troop-disabled-button',
								'')),
							$elm$html$Html$Attributes$disabled(
							$author$project$Templates$SettlementTemplate$validateStationTroops(stAmount))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$img,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('troop-station-icon'),
									$elm$html$Html$Attributes$src('./assets/images/general/arrow_up.png')
								]),
							_List_Nil)
						])),
					A2(
					$elm$html$Html$button,
					_List_fromArray(
						[
							$elm$html$Html$Events$onClick(
							$author$project$Msg$SettlementAction(
								$author$project$Msg$TroopMsg(
									A2($author$project$Msg$StationTroops, lt, sE)))),
							$elm$html$Html$Attributes$class(
							A3(
								$author$project$OperatorExt$ternary,
								$author$project$Templates$SettlementTemplate$validateStationTroops(ltAmount),
								'troop-disabled-button',
								'tooltip')),
							$elm$html$Html$Attributes$disabled(
							$author$project$Templates$SettlementTemplate$validateStationTroops(ltAmount))
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$img,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('troop-station-icon'),
									$elm$html$Html$Attributes$src('./assets/images/general/arrow_down.png')
								]),
							_List_Nil)
						]))
				]));
	});
var $author$project$Templates$SettlementTemplate$validateSettlement = F2(
	function (l, s) {
		return _List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('settlement-enemy-overview')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(
								A3(
									$author$project$OperatorExt$ternary,
									_Utils_eq(l.entity.faction, s.entity.faction),
									'This is our settlement!',
									'This is an enemy settlement!'))
							]))
					]))
			]);
	});
var $author$project$Templates$SettlementTemplate$settlementStateToAction = F3(
	function (lord, settlement, uistate) {
		switch (uistate.$) {
			case 'StandardView':
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$SettlementAction(
									$author$project$Msg$UIMsg(
										$author$project$Msg$ShowBuyTroops(settlement))))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Recruit troops')
									]))
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$SettlementAction(
									$author$project$Msg$UIMsg(
										$author$project$Msg$ShowStationTroops(settlement))))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Station troops')
									]))
							])),
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$SettlementAction(
									$author$project$Msg$UIMsg(
										$author$project$Msg$ShowBuildings(settlement))))
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Upgrade buildings')
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('settlement-info box-shadow')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('header-span')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Settlement Info')
									])),
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('income-span')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										'Income: +' + ($author$project$Templates$HelperTemplate$roundDigits(
											settlement.income + A2($author$project$Building$resolveBonusFromBuildings, settlement.buildings, $author$project$Building$Marketplace)) + ' Ducats'))
									])),
								A2(
								$elm$html$Html$div,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('stationed-troops-overview')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('troop-span')
											]),
										_List_fromArray(
											[
												$elm$html$Html$text('Stationed Troops: ')
											])),
										A2(
										$elm$html$Html$div,
										_List_Nil,
										A3(
											$elm$core$Dict$foldr,
											F3(
												function (k, v, r) {
													return A2(
														$elm$core$List$cons,
														A3(
															$author$project$Templates$HelperTemplate$troopToHtml,
															$author$project$Troops$intToTroopType(k),
															v,
															'stationed-troop-container troop-container'),
														r);
												}),
											_List_Nil,
											settlement.entity.army))
									]))
							]))
					]);
			case 'RecruitView':
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('settlement-troop-recruiting')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Recruit troops')
									])),
							A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('troop-recruiting-header')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('troop-settlement-header')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src('./assets/images/troops/troop_icon.png')
														]),
													_List_Nil)
												])),
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('troop-army-header')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src(
															$author$project$Entities$getSettlementImage(settlement))
														]),
													_List_Nil)
												]))
										])),
								_Utils_ap(
									A6(
										$elm$core$Dict$merge,
										F3(
											function (k, v1, r) {
												return A2(
													$elm$core$List$cons,
													A5(
														$author$project$Templates$SettlementTemplate$generateRecruitTroopContainer,
														$author$project$Troops$intToTroopType(k),
														v1,
														0,
														settlement,
														lord),
													r);
											}),
										F4(
											function (k, v1, v2, r) {
												return A2(
													$elm$core$List$cons,
													A5(
														$author$project$Templates$SettlementTemplate$generateRecruitTroopContainer,
														$author$project$Troops$intToTroopType(k),
														v1,
														v2,
														settlement,
														lord),
													r);
											}),
										F3(
											function (k, v2, r) {
												return A2(
													$elm$core$List$cons,
													A5(
														$author$project$Templates$SettlementTemplate$generateRecruitTroopContainer,
														$author$project$Troops$intToTroopType(k),
														0,
														v2,
														settlement,
														lord),
													r);
											}),
										lord.entity.army,
										settlement.recruitLimits,
										_List_Nil),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$elm$html$Html$Events$onClick(
													$author$project$Msg$SettlementAction(
														$author$project$Msg$UIMsg(
															$author$project$Msg$ShowSettlement(settlement))))
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$span,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text('Back')
														]))
												]))
										])))))
					]);
			case 'StationView':
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('settlement-troop-stationing')
							]),
						A2(
							$elm$core$List$cons,
							A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Station troops')
									])),
							A2(
								$elm$core$List$cons,
								A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('troop-recruiting-header')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('troop-settlement-header')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src('./assets/images/troops/troop_icon.png')
														]),
													_List_Nil)
												])),
											A2(
											$elm$html$Html$div,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('troop-army-header')
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$img,
													_List_fromArray(
														[
															$elm$html$Html$Attributes$src(
															$author$project$Entities$getSettlementImage(settlement))
														]),
													_List_Nil)
												]))
										])),
								_Utils_ap(
									A6(
										$elm$core$Dict$merge,
										F3(
											function (k, v1, r) {
												return A2(
													$elm$core$List$cons,
													A4(
														$author$project$Templates$SettlementTemplate$generateStationTroopContainer,
														$author$project$Troops$intToTroopType(k),
														v1,
														0,
														settlement),
													r);
											}),
										F4(
											function (k, v1, v2, r) {
												return A2(
													$elm$core$List$cons,
													A4(
														$author$project$Templates$SettlementTemplate$generateStationTroopContainer,
														$author$project$Troops$intToTroopType(k),
														v1,
														v2,
														settlement),
													r);
											}),
										F3(
											function (k, v2, r) {
												return A2(
													$elm$core$List$cons,
													A4(
														$author$project$Templates$SettlementTemplate$generateStationTroopContainer,
														$author$project$Troops$intToTroopType(k),
														0,
														v2,
														settlement),
													r);
											}),
										lord.entity.army,
										settlement.entity.army,
										_List_Nil),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$button,
											_List_fromArray(
												[
													$elm$html$Html$Events$onClick(
													$author$project$Msg$SettlementAction(
														$author$project$Msg$UIMsg(
															$author$project$Msg$ShowSettlement(settlement))))
												]),
											_List_fromArray(
												[
													A2(
													$elm$html$Html$span,
													_List_Nil,
													_List_fromArray(
														[
															$elm$html$Html$text('Back')
														]))
												]))
										])))))
					]);
			case 'RestrictedView':
				return _Utils_ap(
					A2($author$project$Templates$SettlementTemplate$validateSettlement, lord, settlement),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-info box-shadow')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('header-span')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text('Settlement Info')
										])),
									A2(
									$elm$html$Html$span,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('income-span')
										]),
									_List_fromArray(
										[
											$elm$html$Html$text(
											'Income: +' + ($elm$core$String$fromFloat(settlement.income) + ' Ducats'))
										])),
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$class('stationed-troops-overview')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('troop-span')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text('Stationed Troops: ')
												])),
											A2(
											$elm$html$Html$div,
											_List_Nil,
											A3(
												$elm$core$Dict$foldr,
												F3(
													function (k, v, r) {
														return A2(
															$elm$core$List$cons,
															A3(
																$author$project$Templates$HelperTemplate$troopToHtml,
																$author$project$Troops$intToTroopType(k),
																v,
																'stationed-troop-container troop-container'),
															r);
													}),
												_List_Nil,
												settlement.entity.army))
										]))
								]))
						]));
			default:
				return _List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('settlement-building-upgrading')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Upgrade buildings')
									])),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								A2(
									$elm$core$List$map,
									$author$project$Templates$SettlementTemplate$displayBuildingComponents,
									A2(
										$elm$core$List$map,
										function (x) {
											return _Utils_Tuple3(x, lord, settlement);
										},
										settlement.buildings))),
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Msg$SettlementAction(
													$author$project$Msg$UIMsg(
														$author$project$Msg$ShowSettlement(settlement))))
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Back')
													]))
											]))
									]))
							]))
					]);
		}
	});
var $author$project$Templates$SettlementTemplate$generateSettlementModalTemplate = F3(
	function (lord, settlement, uistate) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('modal-background')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('settlement-modal')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-modal-close-container')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick($author$project$Msg$CloseModal),
											$elm$html$Html$Attributes$class('settlement-modal-close-btn')
										]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('X')
												]))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-modal-name')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text(
											$author$project$Entities$combineSettlementName(settlement))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-lordship box-shadow')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$div,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$img,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$src(
													'./assets/images/profiles/' + $author$project$Entities$factionToImage(lord.entity.faction)),
													$elm$html$Html$Attributes$class('settlement-lord-icon')
												]),
											_List_Nil)
										])),
									A2(
									$elm$html$Html$div,
									_List_Nil,
									_List_fromArray(
										[
											A2(
											$elm$html$Html$span,
											_List_fromArray(
												[
													$elm$html$Html$Attributes$class('settlement-lord-text')
												]),
											_List_fromArray(
												[
													$elm$html$Html$text(lord.entity.name)
												]))
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-action-container')
								]),
							A3($author$project$Templates$SettlementTemplate$settlementStateToAction, lord, settlement, uistate)),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('settlement-illustration-container box-shadow')
								]),
							_List_fromArray(
								[
									A2(
									$elm$html$Html$img,
									_List_fromArray(
										[
											$elm$html$Html$Attributes$src('./assets/images/illustrations/example_ilustration.png')
										]),
									_List_Nil)
								]))
						]))
				]));
	});
var $author$project$Main$findModalWindow = function (model) {
	var _v0 = model.gameState;
	if (_v0.$ === 'GameSetup') {
		var uistate = _v0.a;
		switch (uistate.$) {
			case 'SettlementView':
				var l = uistate.a;
				var s = uistate.b;
				var u = uistate.c;
				return A3($author$project$Templates$SettlementTemplate$generateSettlementModalTemplate, l, s, u);
			case 'LordView':
				var l = uistate.a;
				return $author$project$Templates$LordTemplate$generateLordTemplate(l);
			case 'BattleView':
				var bS = uistate.a;
				return A2(
					$author$project$Templates$BattleTemplate$generateBattleTemplate,
					bS,
					A2($author$project$Map$getTerrainForPoint, bS.attacker.entity.position, model.map));
			default:
				return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		}
	} else {
		var bool = _v0.a;
		return $author$project$Templates$EndTemplate$generateEndTemplate(bool);
	}
};
var $author$project$Msg$ClearEvents = {$: 'ClearEvents'};
var $author$project$Msg$EventAction = function (a) {
	return {$: 'EventAction', a: a};
};
var $author$project$Msg$DeleteEvent = function (a) {
	return {$: 'DeleteEvent', a: a};
};
var $author$project$Templates$EventTemplate$generateEventComponent = function (e) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				'event-logs-component ' + A3(
					$author$project$OperatorExt$ternary,
					_Utils_eq(e.eventType, $author$project$Event$Important),
					'important-log',
					'minor-log'))
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(e.header)
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$Msg$EventAction(
							$author$project$Msg$DeleteEvent(e.index))),
						$elm$html$Html$Attributes$class('event-logs-close')
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('x')
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text(e.text)
							]))
					]))
			]));
};
var $author$project$Templates$EventTemplate$generateEventOverview = function (event) {
	return event.state ? A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('event-logs box-shadow')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('event-logs-header')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Events')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$EventAction($author$project$Msg$ClearEvents))
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Clear events')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('event-logs-body')
					]),
				A2($elm$core$List$map, $author$project$Templates$EventTemplate$generateEventComponent, event.events))
			])) : A2($elm$html$Html$div, _List_Nil, _List_Nil);
};
var $author$project$Msg$EndGame = function (a) {
	return {$: 'EndGame', a: a};
};
var $author$project$Templates$HeaderTemplate$lordToRevenues = function (l) {
	return _List_fromArray(
		[
			_Utils_Tuple2(
			'Settlements:',
			$author$project$Entities$sumSettlementsIncome(l.land)),
			_Utils_Tuple2(
			'Armies:',
			$author$project$Entities$sumTroopWages(
				$author$project$Entities$sumLordTroops(l)) * (-1))
		]);
};
var $author$project$Templates$HeaderTemplate$revenueToSpan = function (_v0) {
	var name = _v0.a;
	var value = _v0.b;
	return (value > 0) ? A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('positive-income')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				name + ('  +' + ($author$project$Templates$HelperTemplate$roundDigits(value) + ' Ducats')))
			])) : A2(
		$elm$html$Html$span,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('negative-income')
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(
				name + (' ' + ($author$project$Templates$HelperTemplate$roundDigits(value) + ' Ducats')))
			]));
};
var $author$project$Templates$HeaderTemplate$revenuesToTemplate = function (rev) {
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('revenue-container')
			]),
		_List_fromArray(
			[
				$author$project$Templates$HeaderTemplate$revenueToSpan(rev)
			]));
};
var $author$project$Templates$HeaderTemplate$headerGoldTemplate = function (lord) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$img,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					$author$project$Msg$EndGame(true)),
					$elm$html$Html$Attributes$src('./assets/images/general/ducats_icon.png'),
					$elm$html$Html$Attributes$class('page-header-images')
				]),
			_List_Nil),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('tooltip')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-header-span')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$Templates$HelperTemplate$roundDigits(lord.gold) + ' Ducats'),
							$author$project$Templates$HeaderTemplate$revenueToSpan(
							_Utils_Tuple2(
								'',
								A3(
									$elm$core$List$foldr,
									$elm$core$Basics$add,
									0,
									A2(
										$elm$core$List$map,
										$elm$core$Tuple$second,
										$author$project$Templates$HeaderTemplate$lordToRevenues(lord)))))
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tooltiptext gold-tooltip')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Monthly revenue')
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							A2(
								$elm$core$List$map,
								$author$project$Templates$HeaderTemplate$revenuesToTemplate,
								$author$project$Templates$HeaderTemplate$lordToRevenues(lord))),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('revenue-result-container')
								]),
							_List_fromArray(
								[
									$author$project$Templates$HeaderTemplate$revenueToSpan(
									_Utils_Tuple2(
										'Revenue',
										A3(
											$elm$core$List$foldr,
											$elm$core$Basics$add,
											0,
											A2(
												$elm$core$List$map,
												$elm$core$Tuple$second,
												$author$project$Templates$HeaderTemplate$lordToRevenues(lord)))))
								]))
						]))
				]))
		]);
};
var $author$project$Msg$SwitchEventView = {$: 'SwitchEventView'};
var $author$project$Templates$HeaderTemplate$headerSettingsTemplate = _List_fromArray(
	[
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('page-setting-container tooltip')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$img,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$src('./assets/images/general/audio_on_icon.png'),
						$elm$html$Html$Attributes$class('page-image-settings')
					]),
				_List_Nil),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('tooltip')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tooltiptext settings-tooltip')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Mute or unmute the gamesounds')
							]))
					]))
			])),
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('page-settings-grid')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Events$onClick(
						$author$project$Msg$EventAction($author$project$Msg$SwitchEventView)),
						$elm$html$Html$Attributes$class('page-setting-container tooltip')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$img,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$src('./assets/images/general/event.png'),
								$elm$html$Html$Attributes$class('page-image-settings')
							]),
						_List_Nil),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('tooltip')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('tooltiptext settings-tooltip')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Hide / Show the event logs')
									]))
							]))
					]))
			]))
	]);
var $author$project$Templates$HeaderTemplate$generateTroopTooltip = F3(
	function (aT, aAmount, sAmount) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('troop-container')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$img,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src(
							'./assets/images/troops/' + ($elm$core$String$toLower(
								$author$project$Troops$troopName(aT)) + '.png'))
						]),
					_List_Nil),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(aAmount) + ('  ' + $author$project$Troops$troopName(aT)))
						])),
					A2(
					$elm$html$Html$span,
					_List_Nil,
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(sAmount) + ('  ' + $author$project$Troops$troopName(aT)))
						]))
				]));
	});
var $author$project$Templates$HeaderTemplate$headerTroopTemplate = function (lord) {
	var lordSettlementTroops = $author$project$Entities$sumLordSettlementTroops(lord);
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$img,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$src('./assets/images/troops/troop_icon.png'),
					$elm$html$Html$Attributes$class('page-header-images')
				]),
			_List_Nil),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('tooltip')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-header-span')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$elm$core$String$fromInt(
								A3(
									$elm$core$Dict$foldl,
									F3(
										function (k, v, r) {
											return v + r;
										}),
									0,
									lord.entity.army)) + ' Troops')
						])),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('tooltiptext troop-tooltip')
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Current Troops')
								])),
							A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('troop-container-header troop-container')
								]),
							_List_fromArray(
								[
									A2($elm$html$Html$div, _List_Nil, _List_Nil),
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('In the Army')
										])),
									A2(
									$elm$html$Html$span,
									_List_Nil,
									_List_fromArray(
										[
											$elm$html$Html$text('Stantioned')
										]))
								])),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							A3(
								$elm$core$Dict$foldr,
								F3(
									function (k, v, r) {
										var _v0 = A2($elm$core$Dict$get, k, lordSettlementTroops);
										if (_v0.$ === 'Nothing') {
											return A2(
												$elm$core$List$cons,
												A3(
													$author$project$Templates$HeaderTemplate$generateTroopTooltip,
													$author$project$Troops$intToTroopType(k),
													v,
													0),
												r);
										} else {
											var amount = _v0.a;
											return A2(
												$elm$core$List$cons,
												A3(
													$author$project$Templates$HeaderTemplate$generateTroopTooltip,
													$author$project$Troops$intToTroopType(k),
													v,
													amount),
												r);
										}
									}),
								_List_Nil,
								lord.entity.army))
						]))
				]))
		]);
};
var $author$project$Msg$EndRound = {$: 'EndRound'};
var $author$project$DateExt$showMonth = function (m) {
	switch (m.$) {
		case 'Jan':
			return 'January';
		case 'Feb':
			return 'February';
		case 'Mar':
			return 'March';
		case 'Apr':
			return 'April';
		case 'May':
			return 'May';
		case 'Jun':
			return 'Juni';
		case 'Jul':
			return 'July';
		case 'Aug':
			return 'August';
		case 'Sep':
			return 'September';
		case 'Oct':
			return 'October';
		case 'Nov':
			return 'November';
		default:
			return 'December';
	}
};
var $author$project$DateExt$showDate = function (date) {
	return $author$project$DateExt$showMonth(date.month) + (' ' + ($elm$core$String$fromInt(date.year) + ' AD'));
};
var $author$project$Templates$HeaderTemplate$headerTurnTemplate = function (date) {
	return _List_fromArray(
		[
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('page-turn-handler-header')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-turn-button'),
							$elm$html$Html$Events$onClick($author$project$Msg$EndRound)
						]),
					_List_fromArray(
						[
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('End turn')
								]))
						]))
				])),
			A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('page-turn-date-header')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$span,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-header-span')
						]),
					_List_fromArray(
						[
							$elm$html$Html$text(
							$author$project$DateExt$showDate(date))
						]))
				]))
		]);
};
var $author$project$Templates$HeaderTemplate$generateHeaderTemplate = F2(
	function (lord, date) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('page-header')
				]),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-turn-header')
						]),
					$author$project$Templates$HeaderTemplate$headerTurnTemplate(date)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-gold-header')
						]),
					$author$project$Templates$HeaderTemplate$headerGoldTemplate(lord)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-troop-header')
						]),
					$author$project$Templates$HeaderTemplate$headerTroopTemplate(lord)),
					A2(
					$elm$html$Html$div,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$class('page-settings-header')
						]),
					$author$project$Templates$HeaderTemplate$headerSettingsTemplate)
				]));
	});
var $author$project$Msg$MapTileAction = function (a) {
	return {$: 'MapTileAction', a: a};
};
var $author$project$MapAction$Viewer$showLordTileMsg = function (lordTileMsg) {
	if (lordTileMsg.$ === 'ViewLord') {
		return 'View';
	} else {
		return 'Engage';
	}
};
var $author$project$MapAction$Viewer$showSettlementTileMsg = function (msg) {
	switch (msg.$) {
		case 'ViewSettlement':
			return 'View';
		case 'EnterSettlement':
			return 'Enter';
		default:
			return 'Siege';
	}
};
var $author$project$MapAction$Viewer$mapTileMsgToToolTip = function (m) {
	switch (m.$) {
		case 'LordMsg':
			var msg = m.a;
			var l = m.b;
			return $author$project$MapAction$Viewer$showLordTileMsg(msg) + (' ' + l.entity.name);
		case 'SettlementMsg':
			var msg = m.a;
			var s = m.b;
			return $author$project$MapAction$Viewer$showSettlementTileMsg(msg) + (' ' + s.entity.name);
		default:
			return 'Move here';
	}
};
var $author$project$Templates$MapActionTemplate$generateMapActionButtons = function (svga) {
	return A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				$elm$html$Html$Events$onClick(
				$author$project$Msg$MapTileAction(svga))
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$span,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$author$project$MapAction$Viewer$mapTileMsgToToolTip(svga))
					]))
			]));
};
var $author$project$Templates$MapActionTemplate$generateMapActionTemplate = F2(
	function (p, dict) {
		if (p.$ === 'Nothing') {
			return A2($elm$html$Html$div, _List_Nil, _List_Nil);
		} else {
			var x = p.a;
			var actions = A2($author$project$MapAction$actionsOnPoint, x, dict);
			return A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('map-action-menu')
					]),
				A2(
					$elm$core$List$cons,
					A2(
						$elm$html$Html$span,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Map Actions')
							])),
					A2($elm$core$List$map, $author$project$Templates$MapActionTemplate$generateMapActionButtons, actions)));
		}
	});
var $author$project$Main$stylessheets = _List_fromArray(
	['main_styles', 'battle_styles', 'end_styles', 'event_styles', 'header_styles', 'lord_styles', 'mapaction_styles', 'settlement_styles', 'start_styles', 'tooltip_styles']);
var $elm$svg$Svg$svg = $elm$svg$Svg$trustedNode('svg');
var $elm$core$Debug$toString = _Debug_toString;
var $elm$svg$Svg$Attributes$viewBox = _VirtualDom_attribute('viewBox');
var $author$project$Main$setGameView = function (model) {
	var allClickActions = $author$project$Main$buildAllMapSvgs(model);
	return A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('page-container')
			]),
		_List_fromArray(
			[
				$author$project$Main$findModalWindow(model),
				A2(
				$author$project$Templates$HeaderTemplate$generateHeaderTemplate,
				$author$project$Entities$getPlayer(model.lords),
				model.date),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('page-map')
					]),
				_Utils_ap(
					A2($elm$core$List$map, $author$project$Main$addStylesheet, $author$project$Main$stylessheets),
					_List_fromArray(
						[
							A2($author$project$Templates$MapActionTemplate$generateMapActionTemplate, model.selectedPoint, allClickActions),
							A2(
							$elm$html$Html$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$elm$svg$Svg$svg,
									_List_fromArray(
										[
											$elm$svg$Svg$Attributes$viewBox('0 0 850 1000'),
											$elm$svg$Svg$Attributes$fill('none')
										]),
									$author$project$MapAction$allSvgs(allClickActions))
								])),
							$author$project$Templates$EventTemplate$generateEventOverview(model.event),
							A2(
							$elm$html$Html$span,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(
									$elm$core$Debug$toString(
										$author$project$Entities$getPlayer(model.lords).land))
								]))
						])))
			]));
};
var $author$project$Msg$MenueAction = function (a) {
	return {$: 'MenueAction', a: a};
};
var $author$project$Msg$ShowMenue = {$: 'ShowMenue'};
var $author$project$Msg$StartGame = {$: 'StartGame'};
var $elm$html$Html$input = _VirtualDom_node('input');
var $author$project$Templates$StartTemplate$startCampaign = _List_fromArray(
	[
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('start-logo-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$img,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$src('./assets/images/general/logo.png')
					]),
				_List_Nil)
			])),
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('campaign-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('start-header')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('start-header-text')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('M\'lord, what is your name?')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('campaign-actions')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('campaign-name-container')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('campaign-name')
									]),
								_List_fromArray(
									[
										$elm$html$Html$text('Name:')
									])),
								A2(
								$elm$html$Html$input,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('campaign-input')
									]),
								_List_Nil)
							])),
						A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('campaign-buttons-container')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$button,
										_List_fromArray(
											[
												$elm$html$Html$Events$onClick(
												$author$project$Msg$MenueAction($author$project$Msg$StartGame)),
												$elm$html$Html$Attributes$class('start-buttons start-campaign-button')
											]),
										_List_fromArray(
											[
												A2(
												$elm$html$Html$span,
												_List_Nil,
												_List_fromArray(
													[
														$elm$html$Html$text('Start Campaign')
													]))
											]))
									]))
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$button,
						_List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$author$project$Msg$MenueAction($author$project$Msg$ShowMenue)),
								$elm$html$Html$Attributes$class('back-btn')
							]),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$span,
								_List_Nil,
								_List_fromArray(
									[
										$elm$html$Html$text('Back')
									]))
							]))
					]))
			]))
	]);
var $author$project$Msg$SetCampaingn = {$: 'SetCampaingn'};
var $author$project$Templates$StartTemplate$startMenuTemplate = _List_fromArray(
	[
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('start-logo-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$img,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$src('./assets/images/general/logo.png')
					]),
				_List_Nil)
			])),
		A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('start-container')
			]),
		_List_fromArray(
			[
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('start-header')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$span,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('start-header-text')
							]),
						_List_fromArray(
							[
								$elm$html$Html$text('Welcome mylord, what is your decision?')
							]))
					])),
				A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('start-actions')
					]),
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Events$onClick(
										$author$project$Msg$MenueAction($author$project$Msg$SetCampaingn)),
										$elm$html$Html$Attributes$class('start-buttons')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Start Campaign')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('start-buttons')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Documentation')
											]))
									]))
							])),
						A2(
						$elm$html$Html$div,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$button,
								_List_fromArray(
									[
										$elm$html$Html$Attributes$class('start-buttons')
									]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$span,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Credits')
											]))
									]))
							]))
					]))
			]))
	]);
var $author$project$Main$setMenueView = F2(
	function (model, state) {
		return A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('main-container')
				]),
			function () {
				if (state.$ === 'Menue') {
					return _Utils_ap(
						A2($elm$core$List$map, $author$project$Main$addStylesheet, $author$project$Main$stylessheets),
						$author$project$Templates$StartTemplate$startMenuTemplate);
				} else {
					return _Utils_ap(
						A2($elm$core$List$map, $author$project$Main$addStylesheet, $author$project$Main$stylessheets),
						$author$project$Templates$StartTemplate$startCampaign);
				}
			}());
	});
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		A2(
			$elm$core$List$cons,
			function () {
				var _v0 = model.gameState;
				if (_v0.$ === 'GameSetup') {
					var uistate = _v0.a;
					if (uistate.$ === 'MainMenue') {
						var state = uistate.a;
						return A2($author$project$Main$setMenueView, model, state);
					} else {
						return $author$project$Main$setGameView(model);
					}
				} else {
					return $author$project$Main$setGameView(model);
				}
			}(),
			_List_fromArray(
				[
					A2(
					$elm$html$Html$audio,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$src('./assets/sounds/menue.mp3'),
							$elm$html$Html$Attributes$id('audio-player')
						]),
					_List_Nil)
				])));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{
		init: function (_v0) {
			return $author$project$Main$startGame(4);
		},
		subscriptions: function (_v1) {
			return $elm$core$Platform$Sub$none;
		},
		update: $author$project$Main$update,
		view: $author$project$Main$view
	});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));