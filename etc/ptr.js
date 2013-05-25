#include "rts/storage/ClosureTypes.h"
#include "Cmm.h"
#undef Sp
#undef Hp

var _nest_level = 0;
const STACK_MAX = 0x0ffffff0;

function _stack_idx(off) { return STACK_MAX - off; }
function _heap_idx(off) { return off; }

const OP_ADD = 1;
const OP_STORE_W16 = 2;
const OP_STORE_W32 = 3;
const OP_SUB = 4;
const OP_PTR = 5;
const OP_AND = 6;
const OP_OR = 7;
const OP_SHR = 8;
const OP_LOAD_W16 = 9;
const OP_LOAD_W32 = 10;
const OP_STORE_W8 = 11;
const OP_LOAD_W8 = 12;
const OP_ARR = 13;
const OP_STORE_W64 = 14;
const OP_LOAD_W64 = 15;

function store_w8(arr, i, a) {
	var at = (i & ~TAG_MASK) >> TAG_BITS;
	var v = arr[at] || 0;
	var s = 8 * (i & TAG_MASK);
	arr[at] = (v & (~(0xff << s))) | (a << s);
}

function _ptr(idx, arr, off) {
	var the_func = function(op) {
	// trace(op, idx, arr, off, arguments);
	switch (op) {
case OP_ADD:
	var sum = off;
	for (var i = 1; i < arguments.length; i++)
		sum += arguments[i];
	return _ptr(idx, arr, sum);

case OP_STORE_W16:
	var i = idx(off);
	var at = (i & ~TAG_MASK) >> TAG_BITS;
	var v = arr[at];
	var a = arguments[1];
	arr[at] = (i & TAG_MASK) ? ((v & 0xffff) | (a << 16)) : ((v & (~0xffff)) | a);
	return;

case OP_STORE_W8: return store_w8(arr, idx(off), arguments[1]);
case OP_STORE_W32:
case OP_STORE_W64:
	var i = idx(off);
	arr[ i >> TAG_BITS ] = arguments[1];
	return;

case OP_SUB:
	var sum = off;
	for (var i = 1; i < arguments.length; i++)
		sum -= arguments[i];
	return _ptr(idx, arr, sum);
case OP_PTR: return off;
case OP_AND: return _ptr(idx, arr, off & arguments[1]);
case OP_OR: return _ptr(idx, arr, off | arguments[1]);
case OP_SHR: return _ptr(idx, arr, off >> arguments[1]);

case OP_LOAD_W16:
	var i = idx(off);
	var v = arr[(i & ~TAG_MASK) >> TAG_BITS];
	return (i & TAG_MASK) ? v >> 16 : v & 0xffff;

case OP_LOAD_W8:
	var i = idx(off);
	var v = arr[(i & ~TAG_MASK) >> TAG_BITS];
	var s = 8 * (i & TAG_MASK);
	return (v >> s) & 0xff;

case OP_ARR: return arr;
case OP_LOAD_W64:
case OP_LOAD_W32:
default:
	return arr[ idx(off) >> TAG_BITS ];
} }
	// capture(arr, the_func, idx, off);
	return the_func;
}

function heap(arr, off) { return _ptr(_heap_idx, arr, off); }
function stack() { return _ptr(_stack_idx, [], STACK_MAX + (1 << TAG_BITS)); }

function i8_heap(arr) {
	var ha = [];
	for (var i = 0; i < arr.length; i++)
		store_w8(ha, i, arr[i]);
	return heap(ha, 0);
}

function c_to_js_string(p) {
	var a = [];
	for (var i = 0; p(OP_ADD, i)(OP_LOAD_W8); i++)
		a.push(p(OP_ADD, i)(OP_LOAD_W8));
	return String.fromCharCode.apply(null, a);
}

function newCAF() { return 1; }
function threadPaused() {}

function get_ret_type(p) { return p(OP_ADD, -4)(OP_LOAD_W16); }
function stack_frame_size(p) {
	var iit = get_ret_type(p);
	switch (iit) {
	case RET_FUN:
	case RET_BIG:
	case RET_BCO:
		throw new Error("stack_frame_size: " + iit);
	default:
		return (1 + BITMAP_SIZE(p(OP_ADD, -8)(OP_LOAD_W32))) << TAG_BITS;
	}
}

function raiseExceptionHelper(br, tso) {
	var p = Sp;
	while (1) {
		var frame = p(OP_LOAD_W32);
		var iid = get_ret_type(frame);
		var next = p(OP_ADD, stack_frame_size(frame));
		switch (iid) {
		case CATCH_FRAME:
			tso(OP_ADD, 12)(OP_LOAD_W32)(OP_ADD, 12)(OP_STORE_W32, p);
			return CATCH_FRAME;
		default:
			/* TODO: handle UPDATE_FRAME */
			p = next;
			break;
		}
	}
}

function isatty(v) {
	if (v == 1)
		return 1;
	throw new Error("isatty: unknown fd: " + v);
}

function maybePerformBlockedException() { return 0; /* no exception */ }
function allocatePinned() { return heap([], 0); }

function hs_intToInt64(x) { return x; }
function hs_int64ToWord64(x) { return x; }
function hs_eqWord64(a, b) { return a[0] == b[0] && a[1] == b[1]; }
function errorBelch2(a, b) {
	// a is %s
	console.error(c_to_js_string(b));
}
function shutdownHaskellAndExit(ec) { process.exit(ec); }

function malloc() { return heap([], 0); }

function rts_stop_on_exception() { return 0; }
function weak_ptr_list() { return heap([ null ], 0); }

function RtsFlags() { return RtsFlags; }
function BaseReg() { return RtsFlags; }

function getStablePtr(x) { return x; }
function createAdjustor(cconv, stable_ptr, cb, type_str) {
	return function() {
		var args = [ stable_ptr ];
		for (var i = 0; i < arguments.length; i++)
			args.push(arguments[i]);
		return cb.apply(null, args);
	};
}

function freeHaskellFunctionPtr() {}

function rts_mkInt(a) {
	return heap([ ghczmprim_GHCziTypes_Izh_con_info, a ], 0);
}

function rts_mkPtr(a) {
	return heap([ base_GHCziPtr_Ptr_con_info, a ], 0);
}

function rts_apply(f, arg) {
	return heap([ stg_ap_2_upd_info, 0, f, arg ], 0);
}

function pushClosure(sp, closure) {
	sp = sp(OP_ADD, -4);
	sp(OP_STORE_W32, closure);
	return sp;
}

function createClosureThread(closure, base) {
	var tso = heap([ 0, 0, 0, heap([ null, null, 0 ], 0), 0, 0 ], 0);
	var sp = stack ();
	sp = pushClosure(sp, 0); /* to avoid loading undefined */
	sp = pushClosure(sp, base);
	sp = pushClosure(sp, stg_ap_v_info);
	sp = pushClosure(sp, closure);
	sp = pushClosure(sp, stg_enter_info);
	tso(OP_ADD, 12)(OP_LOAD_W32)(OP_ADD, 12)(OP_STORE_W32, sp);
	return tso;
}

function createIOThread(cap, stack_size, closure) {
	return createClosureThread(closure, StgReturn);
}

var _regs = [];
function push_regs() {
	_regs.push([ R1, R2, R3, Hp, Sp ]);
}

function pop_regs() {
	var rs = _regs.pop();
	R1 = rs[0];
	R2 = rs[1];
	R3 = rs[2];
	Hp = rs[3];
	Sp = rs[4];
}

function rts_evalIO(p) {
	var cur = CurrentTSO;
	push_regs();
	var tso = createClosureThread(p, stg_forceIO_info);
	run_loop(tso);
	var ret = R1;
	switch_to(cur);
	pop_regs();
	if (!_nest_level)
		run_loop(_threads.pop());
	return ret;
}

function rts_getInt(p) {
	return p(OP_ADD, 3)(OP_LOAD_W32);
}

function call_unregister(cb) { cb(); }

var _threads = [];
function scheduleThread(cap, tso) {
	_threads.push(tso);
}

function switch_to(tso) {
	CurrentTSO = tso;
	Sp = tso(OP_ADD, 12)(OP_LOAD_W32)(OP_ADD, 12)(OP_LOAD_W32);
	return Sp(OP_LOAD_W32);
}

function StgReturn() {
	var tso = _threads.pop();
	return tso ? switch_to(tso) : null;
}

function dirty_STACK(cap, stk) {
	stk(OP_ADD, 8)(OP_STORE_W32, 1);
}

function tryWakeupThread(cap, tso) {
	// TODO: the real function is much more interesting
	_threads.push(tso);
}

function run_loop(tso) {
	_nest_level++;
	for (var f = switch_to(tso); f; f = f());
	_nest_level--;
}
