//// Contains the filesystem and will call every files that are needed by the
//// compiler



//Provides: std_exit_cmo
var std_exit_cmo = "\\067\\097\\109\\108\\049\\057\\057\\057\\079\\048\\048\\055\\000\\000\\000\\048\\099\\000\\000\\000\\056\\000\\000\\000\\000\\000\\000\\000\\080\\000\\000\\000\\033\\000\\000\\000\\058\\000\\000\\000\\057\\000\\000\\000\\000\\000\\000\\000\\132\\149\\166\\190\\000\\000\\000\\100\\000\\000\\000\\018\\000\\000\\000\\070\\000\\000\\000\\063\\008\\000\\000\\036\\000\\040\\083\\116\\100\\095\\101\\120\\105\\116\\080\\096\\160\\160\\145\\176\\064\\042\\080\\101\\114\\118\\097\\115\\105\\118\\101\\115\\065\\072\\160\\160\\146\\176\\064\\004\\010\\065\\092\\064\\160\\160\\004\\012\\048\\014\\085\\111\\108\\229\\022\\016\\039\\239\\072\\087\\181\\144\\226\\173\\195\\160\\160\\042\\080\\101\\114\\118\\097\\115\\105\\118\\101\\115\\048\\072\\054\\194\\084\\240\\234\\202\\217\\047\\191\\103\\171\\197\\037\\253\\218\\064\\064\\064\\064\\064";

function read_encoded_binary_file(str) {
    var res = "";
    for ( var i=0 ; i < str.length ; i+=4 )
	res += String.fromCharCode(str.substring(i+1, i+4));
    return new MlString(res);
}

var stdout = "";

// Provides: caml_global_filesystem
// Requires: stdlib_cma, std_exit_cmo
var caml_global_filesystem = new Array();
var std_exit_cmo_read = read_encoded_binary_file(std_exit_cmo);
var stdlib_cma_read = read_encoded_binary_file(stdlib_cma);

function add_to_filemanager(name, content) {
    caml_global_filesystem[name] = content;
}

function reset_filemanager() {
    caml_global_filesystem = new Array();
    caml_global_filesystem["std_exit.cmo"] = std_exit_cmo_read;
    caml_global_filesystem["stdlib.cma"] = stdlib_cma_read;
}

function get_from_filemanager(name) {
    return caml_global_filesystem[name];
}


//Provides: caml_sys_open
//Requires: MlString, caml_raise_sys_error, caml_global_data, caml_global_filesystem
function caml_sys_open (x, y) {
    var id = x.toJsString();

    var v = caml_global_data.interfaces[x];
    var f = caml_global_filesystem[id];

    if ( list_mem(y, 1) ) {
	if (f != undefined) {
	    if ( list_mem(y, 4) ) { // Open_trunc
		var s = new MlString("");
		s.offset = 0;
		s.title = id;
		caml_global_filesystem[id] = s;
	    }
	    return caml_global_filesystem[id];
	} else if (v) {
	    var s = new MlString (v);
	    s.offset = 0;
	    s.title = id;
	    return s;
	} else {
	    if ( list_mem(y, 3) ) {
		var s = new MlString("");
		caml_global_filesystem[id] = s;
		s.offset = 0;
		s.title = id;
		return s;
	    } else
		caml_raise_sys_error (x + ": no such file or directory");
	}
    } else if ( list_mem(y, 0) ) {
	if (f != undefined) {
	    var copy = new MlString(caml_global_filesystem[id].getBytes());
	    copy.offset = 0;
            copy.title = id;
	    return copy;
	} else if (v) {
	    var s = new MlString (v);
	    s.offset = 0;
	    s.title = id;
	    return s;
	} else {
	    return new MlString("");
	}
    }
}

//Provides: caml_sys_file_exists
//Requires: caml_global_data, caml_global_filesystem
function caml_sys_file_exists (x) {
    var b = (caml_global_data.interfaces[x])?1:0;
    if ( !b ) {
	var s = x.toJsString();
	return (caml_global_filesystem[s])?1:0;
    } else return b;
}

//Provides: caml_ml_open_descriptor_in
function caml_ml_open_descriptor_in (x) { return x; }

//Provides: caml_ml_open_descriptor_out
function caml_ml_open_descriptor_out (x) { return x; }
