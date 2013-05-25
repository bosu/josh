function set_location(lno) {
	window.top.document.getElementById('arrays').src = "arrays.html#L" + lno;
	var mf = window.top.document.getElementById('milestones').contentWindow.document;
	var old = mf.getElementsByClassName("selected");
	for (var i = 0; i < old.length; i++)
		old[i].classList.remove("selected");

	var nw = mf.getElementsByName(lno);
	for (var i = 0; i < nw.length; i++)
		nw[i].classList.add("selected");
	return true;
}
