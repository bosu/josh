function start_server(cb) {
	var http = require("http");
	var srv = http.createServer(function(req, resp) { cb([ req, resp ]); })
	srv.listen(8888);
	return function() { srv.close() };
}

function response(a) { return a[1]; }
function answer(resp) {
	resp.writeHead(200, {"Content-Type": "text/plain"});
	resp.write("Hello World\n");
	resp.end();
}
