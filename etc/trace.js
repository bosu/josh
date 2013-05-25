#include "rts/storage/ClosureTypes.h"
#include "Cmm.h"
#undef Sp
#undef Hp

var _max_count = 7500;

function get_caller(i) {
	var stk = new Error().stack.split("\n");
	for (; stk[i].indexOf("i8_heap") > 0
		|| stk[i].indexOf("malloc") > 0
		|| stk[i].match(/_closure/)
		; i++) ;
	return stk[i];
}

function dump_any(a) {
	if (a instanceof Function)
		return dump_array(a(OP_ARR));
	else
		return a;
}

function dump_array(arr) {
	var mem = [];
	for (var i = 0; i < arr.length; i++)
		if (arr[i] instanceof Function)
			mem.push("\"" + arr[i]._creator + "\"");
		else if (arr[i] == undefined)
			mem[i] = "null";
		else
			mem.push(arr[i]);
	return mem;
}

function trace(op, idx, arr, off, args) {
	switch (op) {
	case OP_ADD:
		break;
	case OP_STORE_W16:
		break;
	case OP_STORE_W32:
		var i = idx(off);
		if (i & TAG_MASK)
			throw new Error("Bad OP_STORE_W32 at " + i);
		if (args[1] == undefined)
			throw new Error("Store undefined");
		break;
	case OP_SUB:
		break;
	case OP_PTR:
		break;
	case OP_AND:
		break;
	case OP_OR:
		break;
	case OP_SHR:
		break;
	case OP_LOAD_W16:
		break;
	case OP_STORE_W8:
		break;
	case OP_LOAD_W8:
		break;
	case OP_ARR:
		break;
	case OP_LOAD_W32:
	default:
		var i = idx(off);
		if (i & ((1 << 2) - 1))
			throw new Error("Bad OP_LOAD_W32 at " + i);
		if (arr[ i >> 2 ] == undefined)
			throw new Error("OP_LOAD_W32: returns undefined");
	}
	if (--_max_count < 0)
		throw new Error("count done");

	var loc = "\"" + get_caller(4) + "\"";
	var mem = dump_array(arr);
	if (op == OP_STORE_W32 || op == OP_STORE_W16)
		arr._setters[ idx(off) >> TAG_BITS ] = loc;

	for (var i = 0; i < arr._setters.length; i++)
		if (arr._setters[i] == undefined)
			arr._setters[i] = "null";

	var ix = idx(off);
	if (isNaN(ix))
		ix = -1;
	console.log(", {\n\"caller\": " + loc + "\n, \"idx\": " + ix
			+ "\n, \"memory\": [ " + mem.join(", ") + " ]\n"
			+ ", \"creator\": \"" + arr._creator + "\"\n"
			+ ", \"setters\": [ " + arr._setters.join(", ") + " ]\n}");
}

function capture(arr, func, idx, off) {
	if (isNaN(idx(off)))
		throw new Error("idx(off) is NaN at " + off);
	if (!arr._creator) {
		arr._creator = get_caller(5);
		var setters = [];
		for (var i = 0; i < arr.length; i++)
			setters.push("\"" + arr._creator + "\"");
		arr._setters = setters;
	}
	func._creator = arr._creator;
}
