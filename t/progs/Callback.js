function twice(cb, i) {
	return cb(cb(i));
}

function output(i) {
	console.log(i);
}
