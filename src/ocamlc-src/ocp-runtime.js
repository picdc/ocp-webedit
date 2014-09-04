//Provides: caml_marshal_constants
var caml_marshal_constants = {
  PREFIX_SMALL_BLOCK:  0x80,
  PREFIX_SMALL_INT:    0x40,
  PREFIX_SMALL_STRING: 0x20,
  CODE_INT8:     0x00,  CODE_INT16:    0x01,  CODE_INT32:      0x02,
  CODE_INT64:    0x03,  CODE_SHARED8:  0x04,  CODE_SHARED16:   0x05,
  CODE_SHARED32: 0x06,  CODE_BLOCK32:  0x08,  CODE_BLOCK64:    0x13,
  CODE_STRING8:  0x09,  CODE_STRING32: 0x0A,  CODE_DOUBLE_BIG: 0x0B,
  CODE_DOUBLE_LITTLE:         0x0C, CODE_DOUBLE_ARRAY8_BIG:  0x0D,
  CODE_DOUBLE_ARRAY8_LITTLE:  0x0E, CODE_DOUBLE_ARRAY32_BIG: 0x0F,
  CODE_DOUBLE_ARRAY32_LITTLE: 0x07, CODE_CODEPOINTER:        0x10,
  CODE_INFIXPOINTER:          0x11, CODE_CUSTOM:             0x12,
  CODE_DOUBLE_NATIVE:         0x0C //We suppose the arch is in LittleEndian
}

//Provides: caml_marshal_data_size mutable
//Requires: caml_failwith
function caml_marshal_data_size (s, ofs) {
    function get32(s,i) {
        return (s.get(i) << 24) | (s.get(i + 1) << 16) |
            (s.get(i + 2) << 8) | s.get(i + 3);
    }
    if (get32(s, ofs) != (0x8495A6BE|0))
        caml_failwith("Marshal.data_size: bad object");
    return (get32(s, ofs + 4));
}


//Provides: caml_input_value_from_string mutable
//Requires: caml_failwith, MlStringFromArray, MlString, caml_marshal_constants
//Requires: caml_int64_float_of_bits, caml_int64_of_bytes
var caml_input_value_from_string = function (){
    function ArrayReader (a, i) { this.a = a; this.i = i; }
    ArrayReader.prototype = {
        read8u:function () { return this.a[this.i++]; },
        read8s:function () { return this.a[this.i++] << 24 >> 24; },
        read16u:function () {
            var a = this.a, i = this.i;
            this.i = i + 2;
            return (a[i] << 8) | a[i + 1]
        },
        read16s:function () {
            var a = this.a, i = this.i;
            this.i = i + 2;
            return (a[i] << 24 >> 16) | a[i + 1];
        },
        read32u:function () {
            var a = this.a, i = this.i;
            this.i = i + 4;
            return ((a[i] << 24) | (a[i+1] << 16) | (a[i+2] << 8) | a[i+3]) >>> 0;
        },
        read32s:function () {
            var a = this.a, i = this.i;
            this.i = i + 4;
            return (a[i] << 24) | (a[i+1] << 16) | (a[i+2] << 8) | a[i+3];
        },
        read64s:function () {
            var t = [];
            for (var j=0 ; j<8 ; j++ ) t[j] = this.read8u ();
            var res = caml_int64_of_bytes(t);
            return res;
        },
        readstr:function (len) {
            var i = this.i;
            this.i = i + len;
            return new MlStringFromArray(this.a.slice(i, i + len));
        }
    }
    function StringReader (s, i) { this.s = s; this.i = i; }
    StringReader.prototype = {
        read8u:function () { return this.s.charCodeAt(this.i++); },
        read8s:function () { return this.s.charCodeAt(this.i++) << 24 >> 24; },
        read16u:function () {
            var s = this.s, i = this.i;
            this.i = i + 2;
            return (s.charCodeAt(i) << 8) | s.charCodeAt(i + 1)
        },
        read16s:function () {
            var s = this.s, i = this.i;
            this.i = i + 2;
            return (s.charCodeAt(i) << 24 >> 16) | s.charCodeAt(i + 1);
        },
        read32u:function () {
            var s = this.s, i = this.i;
            this.i = i + 4;
            return ((s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
                    (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3)) >>> 0;
        },
        read32s:function () {
            var s = this.s, i = this.i;
            this.i = i + 4;
            return (s.charCodeAt(i) << 24) | (s.charCodeAt(i+1) << 16) |
                (s.charCodeAt(i+2) << 8) | s.charCodeAt(i+3);
        },
        readstr:function (len) {
            var i = this.i;
            this.i = i + len;
            return new MlString(this.s.substring(i, i + len));
        }
    }
    function caml_float_of_bytes (a) {
        return caml_int64_float_of_bits (caml_int64_of_bytes (a));
    }

    return function (s, ofs) {
        if ( ofs == undefined ) ofs = 0;
        var reader = s.array?new ArrayReader (s.array, ofs):
            new StringReader (s.getFullBytes(), ofs);
        var magic = reader.read32u ();
        var block_len = reader.read32u ();
        var num_objects = reader.read32u ();
        var size_32 = reader.read32u ();
        var size_64 = reader.read32u ();
        var stack = [];
        var intern_obj_table = (num_objects > 0)?[]:null;
        var obj_counter = 0;
        function intern_rec () {
            var cst = caml_marshal_constants;
            var code = reader.read8u ();
            if (code >= cst.PREFIX_SMALL_INT) {
                if (code >= cst.PREFIX_SMALL_BLOCK) {
                    var tag = code & 0xF;
                    var size = (code >> 4) & 0x7;
                    var v = [tag];
                    if (size == 0) return v;
                    if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                    stack.push(v, size);
                    return v;
                } else {
                    return (code & 0x3F);
                }
            } else {
                if (code >= cst.PREFIX_SMALL_STRING) {
                    var len = code & 0x1F;
                    var v = reader.readstr (len);
                    if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                    return v;
                } else {
                    if (code == undefined ) code = 0;
                    switch(code) {
                    case cst.CODE_INT8:
                        return reader.read8s ();
                    case cst.CODE_INT16: 
                        return reader.read16s ();
                    case cst.CODE_INT32:
                        return reader.read32s ();
                    case cst.CODE_INT64:
                        var res = reader.read64s ();
                        return res;
                    case cst.CODE_SHARED8:
                        var ofs = reader.read8u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_SHARED16: 
                        var ofs = reader.read16u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_SHARED32:
                        var ofs = reader.read32u ();
                        return intern_obj_table[obj_counter - ofs];
                    case cst.CODE_BLOCK32:
                        var header = reader.read32u ();
                        var tag = header & 0xFF;
                        var size = header >> 10;
                        var v = [tag];
                        if (size == 0) return v;
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        stack.push(v, size);
                        return v;
                    case cst.CODE_BLOCK64:
                        caml_failwith ("input_value: data block too large");
                        break;
                    case cst.CODE_STRING8:
                        var len = reader.read8u();
                        var v = reader.readstr (len);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_STRING32:
                        var len = reader.read32u();
                        var v = reader.readstr (len);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_LITTLE:
                        var t = [];
                        for (var i = 0;i < 8;i++) t[7 - i] = reader.read8u ();
                        var v = caml_float_of_bytes (t);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_BIG:
                        var t = [];
                        for (var i = 0;i < 8;i++) t[i] = reader.read8u ();
                        var v = caml_float_of_bytes (t);
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        return v;
                    case cst.CODE_DOUBLE_ARRAY8_LITTLE:
                        var len = reader.read8u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
                            v[i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY8_BIG:
                        var len = reader.read8u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            v [i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY32_LITTLE:
                        var len = reader.read32u();
                        var v = [0];
                        if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[7 - j] = reader.read8u();
                            v[i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_DOUBLE_ARRAY32_BIG:
                        var len = reader.read32u();
                        var v = [0];
                        for (var i = 1;i <= len;i++) {
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            v [i] = caml_float_of_bytes (t);
                        }
                        return v;
                    case cst.CODE_CODEPOINTER:
                    case cst.CODE_INFIXPOINTER:
                        caml_failwith ("input_value: code pointer");
                        break;
                    case cst.CODE_CUSTOM:
                        var c, s = "";
                        while ((c = reader.read8u ()) != 0) s += String.fromCharCode (c);
                        switch(s) {
                        case "_j":
                            // Int64
                            var t = [];
                            for (var j = 0;j < 8;j++) t[j] = reader.read8u();
                            var v = caml_int64_of_bytes (t);
                            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                            return v;
                        case "_i":
                            // Int32
                            var v = reader.read32s ();
                            if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                            return v;
                        case "_n":
                            switch ( reader.read8u() ) {
                            case 1: 
                                var v = reader.read32s ();
                                if (intern_obj_table) intern_obj_table[obj_counter++] = v;
                                return [245, v];
                            case 2:
                                caml_failwith("input_value: native integer value too large");
                            default:
                                caml_failwith("input_value: ill-formed native integer");
                            }
                        default:
                            caml_failwith("input_value: unknown custom block identifier");
                        }
                    default:
                        caml_failwith ("input_value: ill-formed message");
                    }
                }
            }
        }
        var res = intern_rec ();
        while (stack.length > 0) {
            var size = stack.pop();
            var v = stack.pop();
            var d = v.length;
            if (d < size) stack.push(v, size);
            v[d] = intern_rec ();
        }
        s.offset = reader.i;
        return res;
    }
}();


var obj_counter;
var extern_value_area = new Array();

function is_long(v) { return (!(v instanceof Array || v instanceof MlString)); }

function is_value_unseen(v) {
    cf = offset_of_value_seen(v);
    var res = ( cf == null);
    return res;
}

function offset_of_value_seen(v) {
    for (val in extern_value_area)
        if (extern_value_area[val][1] === v) return extern_value_area[val][0];
    return null;
}

function nativeint_to_bytes(v) { 
    return [(v[1] >> 24) & 0xff, (v[1] >> 16) & 0xff, 
            (v[1] >> 8) & 0xff, (v[1] & 0xff)]
}

function extern_record_location(v) {
    var i = obj_counter;
    extern_value_area.push([i, v]);
    obj_counter++;
    return 0;
}

//Provides: caml_output_val
//Requires: caml_marshal_constants, caml_int64_to_bytes, caml_failwith
var caml_output_val = function (){
    function Writer () { this.chunk = []; }
    Writer.prototype = {
	chunk_idx:20, block_len:0, size_32:0, size_64:0,
	write:function (size, value) {
	    for (var i = size - 8;i >= 0;i -= 8)
		this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
	},
	write_code:function (size, code, value) {
	    this.chunk[this.chunk_idx++] = code;
	    for (var i = size - 8;i >= 0;i -= 8)
		this.chunk[this.chunk_idx++] = (value >> i) & 0xFF;
	},
	finalize:function () {
	    this.block_len = this.chunk_idx - 20;
	    this.chunk_idx = 0;
	    this.write (32, 0x8495A6BE);
	    this.write (32, this.block_len);
	    this.write (32, obj_counter);
	    this.write (32, this.size_32);
	    this.write (32, this.size_64);
	    return this.chunk;
	}
    }
    return function (v) {
	var writer = new Writer ();
        obj_counter = 0;
        extern_value_area = new Array();
	var stack = [];
	function extern_rec (v) {
            // console.debug(v);

	    var cst = caml_marshal_constants;
            if ( is_long(v) ) {
                // Numeric value
		if (v >= 0 && v < 0x40)
		    writer.write (8, cst.PREFIX_SMALL_INT + v);
		else if (v >= -(1 << 7) && v < (1 << 7))
		    writer.write_code(8, cst.CODE_INT8, v);
		else if (v >= -(1 << 15) && v < (1 << 15))
		    writer.write_code(16, cst.CODE_INT16, v);
		else
		    writer.write_code(32, cst.CODE_INT32, v);
            } else if ( v instanceof Array && v[0] === (v[0]|0) && v[0] == 255) {
                // Int 64
		writer.write (8, cst.CODE_CUSTOM);
		for (var i = 0; i < 3; i++) writer.write (8, "_j\0".charCodeAt(i));
		var b = caml_int64_to_bytes (v);
		for (var i = 0; i < 8; i++) writer.write (8, b[i]);
		writer.size_32 += 4;
		writer.size_64 += 3;
		extern_record_location(v);         
                return;
            } else if (v instanceof Array && v[0] === (v[0]|0) && v[0] == 245) {
                //Nativeint
                // console.debug(v);

                writer.write (8, cst.CODE_CUSTOM);
                        
                //Equivalent to writeblock
		for (var i = 0; i < 3; i++) 
                    writer.write (8, "_n\0".charCodeAt(i));

                writer.write (8, (1|0));

                var b = nativeint_to_bytes(v);
                for (var i = 0; i < 4; i++) { 
                    // console.log(i);
                    writer.write (8, b[i]);
                }

                writer.size_32 += 3;
                writer.size_64 += 3;
                extern_record_location(v);
                return;

            } else if (v instanceof Array && v[0] === (v[0]|0) && v[0] == 253) {
                
                function float_to_bytes (f) {
                    return caml_int64_to_bytes(caml_int64_bits_of_float(f));
                }
                //Float
                writer.write(8, cst.CODE_DOUBLE_NATIVE);

                var f = float_to_bytes(v);
                for (var i = 7; i >= 0; i--)
                    writer.write (8, f[i]);

                writer.size_32 += 1 + 2;
                writer.size_64 += 1 + 1;
                extern_record_location(v);
                return;
  
            } else if ( is_value_unseen(v) ) {
	        if (v instanceof Array && v[0] === (v[0]|0)) {
    
                    if (v[0] < 16 && v.length - 1 < 8) {
		        writer.write (8, cst.PREFIX_SMALL_BLOCK + v[0] + ((v.length - 1)<<4));
                    }
		    else {
		        writer.write_code(32, cst.CODE_BLOCK32, ((v.length - 1) << 10) | v[0]);
                    }
		    writer.size_32 += v.length;
		    writer.size_64 += v.length;
		    if (v.length > 1) stack.push (v, 1);
                    extern_record_location(v);
	        } else if (v instanceof MlString) {
                    var len = v.getLen();
		    if (len < 0x20) 
		        writer.write (8, cst.PREFIX_SMALL_STRING + len);
		    else if (len < 0x100)
		        writer.write_code (8, cst.CODE_STRING8, len);
		    else
		        writer.write_code (32, cst.CODE_STRING32, len);
		    for (var i = 0;i < len;i++) writer.write (8, v.get(i));
		    writer.size_32 += 1 + (((len + 4) / 4)|0);
		    writer.size_64 += 1 + (((len + 8) / 8)|0);
                    extern_record_location(v);
	        }

            } else if ( (cf = offset_of_value_seen(v)) != null ) {
                var d = obj_counter - cf;
                if ( d < 0x100 )
                    writer.write_code (8, cst.CODE_SHARED8, d);
                else if ( d < 0x10000 ) 
                    writer.write_code (16, cst.CODE_SHARED16, d);
                else writer.write_code (32, cst.CODE_SHARED32, d);
            } else
                caml_failwith("output_value: abstract value (outside heap)");
      	}
	extern_rec (v);
	while (stack.length > 0) {
	    var i = stack.pop ();
	    var v = stack.pop ();
            
	    if (i + 1 < v.length) stack.push (v, i + 1);
	    
            // if (v[i].length == 3 && v[i][1] instanceof MlString 
            //     && v[i][1].bytes == "Nativeint52869257")
            //     extern_rec([258, v[i][2]]);
            // else {
            extern_rec (v[i]);
	}
	writer.finalize ();
	return writer.chunk;
    }
} ();


//Provides: caml_raise_sys_error
//Requires: caml_raise_with_string, caml_global_data
function caml_raise_sys_error (msg) {
    caml_raise_with_string(caml_global_data[2], msg);
}



function my_caml_blit_string(x, i, s, j, l) {
    var x_str = x.getBytes().slice(i, l);
    var s_str = s.getBytes();
    var sl = s_str.length;

    var str = s_str.substring(0, j) + x_str + s_str.substring(j+l, sl);
    var str = new MlString(str);
    s.bytes = str.bytes;
    s.fullBytes = str.bytes;
    s.last = str.last;
    s.offset = j+l;
    return 0;
}


//Provides: caml_ml_input
function caml_ml_input (f, s, i, l) {
    var l2 = f.getLen() - f.offset;
    if (l2 < l) l = l2;
    var str = read_in_input(f, l);
    my_caml_blit_string(str, 0, s, i, l);
    return l;
}

//Provides: caml_input_value
//Requires: caml_marshal_data_size, caml_input_value_from_string
function caml_input_value (s) {
    caml_marshal_data_size (s, s.offset);
    var res = caml_input_value_from_string(s, s.offset);
    return res;
}


//Provides: caml_get_global_data
//Requires: caml_global_data
function caml_get_global_data () { return caml_global_data; }



function MlstringToBlob(str){
    var str = str.toArray();
    var a = new Uint8Array(str.length);
    for (k in str)  a[k] = str[k];
    return new Blob([a.buffer], {type: "application/octet-stream;charset=utf-8"})
}




function createBinaryString (nMask) {
    // nMask must be between -2147483648 and 2147483647
    for (var nFlag = 0, nShifted = nMask, sMask = ""; nFlag < 32; nFlag++, sMask += String(nShifted >>> 31), nShifted <<= 1);
    return sMask;
}

function list_mem(l, o) {
    if (l[0]) { return false; }
    else if (l[1] == o) { return true; }
    else if (l[2] == 0) { return false; }
    else { return list_mem(l[2], o); }
} 

function mlstrdebug(s) {
    var a = s.toArray();
    var s = "";
    for (v in a) {
	var i = a[v];
	if (i >= 33 && i <= 126 ) {
	    s += String.fromCharCode(i);
	} else {
	    s += "\\";
	    if ( i < 10 ) s += "00";
	    else if ( i < 100 ) s += "0";
	    s += i;
	}
    }
    console.debug(s);
}

//Provides: add_to_output
function add_to_output(x, s, p, l) {

    // console.debug(s);
    if ( ! (x instanceof MlString) ) { 
	if ( x == 1 || x == 2 ) // stdout et stderr
	    stdout += s.getBytes().slice(p,p+l);
        return 0;
    }

    var xoffset = x.offset;
    var x_str = x.getBytes();
    var s = s.getBytes().slice(p, l);

    var xl = x_str.length;
    var sl = s.length;
    var off = xoffset + sl;
    var len = (off>xl)?off:xl;;
    var str = x_str.substring(0, xoffset) + s + x_str.substring(off, len);
    var str = new MlString(str);

    x.bytes = str.bytes;
    x.fullBytes = str.bytes;
    x.len = str.len;
    x.last = str.last;
    x.offset = off;
    return 0;
}

//Provides: caml_ml_channel_size
function caml_ml_channel_size(x) { 
    return x.len; 
}

function read_in_input(x, len) {
    var ofs = x.offset;
    var str = new MlString(x.getBytes().slice(ofs, ofs+len));
    x.offset += len;
    return str;
}


//Provides: caml_ml_output
//Requires: add_to_output
function caml_ml_output (x, s, p, l) {
    add_to_output(x, s, p, l);
    return 0;
}

//Provides: caml_ml_output_char
//Requires: caml_ml_output, add_to_output
function caml_ml_output_char (x, c) {
    return add_to_output(x, new MlString(String.fromCharCode(c)), 0, 1);
}

function binaryToInt8(s) {
    var i = 0;
    for ( var n=0 ; n<8 ; n++ ) i += s[7-n] * (1 << n);
    return i;
}

//Provides: caml_ml_output_int
//Requires: caml_ml_output, add_to_output
function caml_ml_output_int (x, i) {
    var res = "";
    var s = createBinaryString(i);
    for ( var n=0 ; n<32 ; n+=8 ) {
	var str = s.slice(n, n+8);
	res += String.fromCharCode(binaryToInt8(str));
    }

    var res = new MlString(res);
    return add_to_output(x, res, 0, res.getLen());
}

//Provides: caml_output_value
//Requires: caml_ml_output, caml_output_value_to_string, add_to_output
function caml_output_value (x, v, fl) {
    var str = caml_output_value_to_string(v);
    return add_to_output(x, str, 0, str.getLen());
}

//Provides: caml_sys_get_argv const
//Requires: MlString
// WARNING WARNING WARNING WARNING WARNING WARNING WARNING WARNING
function caml_sys_get_argv () {
    // caml_failwith("caml_sys_get_argv : not implemented");
    return [0, 0, 0];
}

//Provides: caml_sys_remove
function caml_sys_remove() { return 0; }

//Provides: caml_sys_exit
function caml_sys_exit(x) { return x; }

//Provides: caml_ml_seek_in
function caml_ml_seek_in(x, i) {
    x.offset = i;
    return 0;
}

//Provides: caml_ml_seek_out
function caml_ml_seek_out(x, i) {
    x.offset = i;
    return 0;
}

//Provides: caml_md5_chan
function caml_md5_chan(x, l) {
    var str = x.getBytes();
    var len = (l==-1)?str.length:l;
    var s = caml_md5_string(x, 0, len);
    return s;
}


//Provides: caml_ml_pos_out
function caml_ml_pos_out(x) { return x.offset; }

//Provides: caml_ml_close_channel
function caml_ml_close_channel(x) { return 0; }

//Provides: caml_sys_close
function caml_sys_close(x) { return 0; }


//Provides: caml_ml_input_int
function caml_ml_input_int(x) {
    var a = read_in_input(x, 4).toArray();
    var i = (a[0] << 24) | (a[1] << 16) | (a[2] << 8) | a[3];
    return i;
}



//Provides: caml_sys_system_command
function caml_sys_system_command(str) {
    caml_failwith("caml_sys_system_command : not implemented");
}


/// Floats primitives ///

//Provides: caml_int64_bits_of_float const
function caml_int64_bits_of_float (x) {
    //console.log("### caml_int64_bits_of_float ####");
    var x = x[1];
  if (!isFinite(x)) {
    if (isNaN(x)) return [255, 1, 0, 0xfff0];
    return (x > 0)?[255,0,0,0x7ff0]:[255,0,0,0xfff0];
  }
  var sign = (x>=0)?0:0x8000;
  if (sign) x = -x;
  var exp = Math.floor(Math.LOG2E*Math.log(x)) + 1023;
  if (exp <= 0) {
    exp = 0;
    x /= Math.pow(2,-1026);
  } else {
    x /= Math.pow(2,exp-1027);
    if (x < 16) { x *= 2; exp -=1; }
    if (exp == 0) { x /= 2; }
  }
  var k = Math.pow(2,24);
  var r3 = x|0;
  x = (x - r3) * k;
  var r2 = x|0;
  x = (x - r2) * k;
  var r1 = x|0;
  r3 = (r3 &0xf) | sign | exp << 4;
  return [255, r1, r2, r3];
}


//Provides: caml_modf_float const
function caml_modf_float (x) {
    //console.log("### caml_modf_float ####");
    var x = x[1];
  if (isFinite (x)) {
    var neg = (1/x) < 0;
    x = Math.abs(x);
    var i = Math.floor (x);
    var f = x - i;
    if (neg) { i = -i; f = -f; }
    return [0, [253, f], [253, i]];
  }
  if (isNaN (x)) return [0, [253, NaN], [253, NaN]];
  return [0, [253, 1/x], [253, x]];
}
//Provides: caml_ldexp_float const
function caml_ldexp_float (x,exp) {
    //console.log("### caml_ldexp_float ####");
    var x = x[1];
  exp |= 0;
  if (exp > 1023) {
    exp -= 1023;
    x *= Math.pow(2, 1023);
    if (exp > 1023) {  // in case x is subnormal
      exp -= 1023;
      x *= Math.pow(2, 1023);
    }
  }
  if (exp < -1023) {
    exp += 1023;
    x *= Math.pow(2, -1023);
  }
  x *= Math.pow(2, exp);
  return [253, x];
}
//Provides: caml_frexp_float const
function caml_frexp_float (x) {
    //console.log("### caml_frexp_float ####");
    var x = x[1];
  if ((x == 0) || !isFinite(x)) return [0, x, 0];
  var neg = x < 0;
  if (neg) x = - x;
  var exp = Math.floor(Math.LOG2E*Math.log(x)) + 1;
  x *= Math.pow(2,-exp);
  if (x < 0.5) { x *= 2; exp -= 1; }
  if (neg) x = - x;
  return [0, [253, x], exp];
}

//Provides: caml_float_compare const
function caml_float_compare (x, y) {
    //console.log("### caml_float_compare ####");
    var x = x[1], y = y[1];
  if (x === y) return 0;
  if (x < y) return -1;
  if (x > y) return 1;
  if (x === x) return 1;
  if (y === y) return -1;
  return 0;
}

//Provides: caml_copysign_float const
function caml_copysign_float (x, y) {
  //console.log("### caml_copysign_float ####");
    var x = x[1], y = y[1];
  if (y == 0) y = 1 / y;
  x = Math.abs(x);
  return [253, (y < 0)?(-x):x];
}

//Provides: caml_expm1_float const
function caml_expm1_float (x) {
    //console.log("### caml_expm1_float ####");
    var x = x[1];
  var y = Math.exp(x), z = y - 1;
  return [253, (Math.abs(x)>1?z:(z==0?x:x*z/Math.log(y)))];
}

//Provides: caml_log1p_float const
function caml_log1p_float (x) {
    //console.log("### caml_logp1_float ####");
    var x = x[1];
  var y = 1 + x, z = y - 1;
  return [253, (z==0?x:x*Math.log(y)/z)];
}

//Provides: caml_hypot_float const
function caml_hypot_float (x, y) {
    //console.log("### caml_hypot_float ####");
    var x = x[1], y = y[1];
  var x = Math.abs(x), y = Math.abs(y);
  var a = Math.max(x, y), b = Math.min(x,y) / (a?a:1);
  return [253, (a * Math.sqrt(1 + b*b))];
}

// FIX: these five functions only give approximate results.
//Provides: caml_log10_float const
function caml_log10_float (x) { //console.log("### caml_log10_float ####");
    return [253, Math.LOG10E * Math.log(x[1])]; }
//Provides: caml_cosh_float const
function caml_cosh_float (x) { //console.log("### caml_cosh_float ####");
    return [253, (Math.exp(x) + Math.exp(-x[1])) / 2]; }
//Provides: caml_sinh_float const
function caml_sinh_float (x) { //console.log("### caml_sinh_float ####");
    return [253, (Math.exp(x) - Math.exp(-x[1])) / 2]; }
//Provides: caml_tanh_float const
function caml_tanh_float (x) {
  //console.log("### caml_tanh_float ####");
    var y = Math.exp(x[1]), z = Math.exp(-x[1]);
  return [253, (y + z) / (y - z)];
}



//Provides: caml_int64_to_float const
function caml_int64_to_float (x) {
  //console.log("### caml_int64_to_float ####");
    return [253,
          ((x[3] << 16) * Math.pow(2, 32) + x[2] * Math.pow(2, 24)) + x[1]];
}

//Provides: caml_int64_of_float const
//Requires: caml_int64_offset
function caml_int64_of_float (x) {
//console.log("### caml_int64_of_float ####");
        var x = x[1];
  if (x < 0) x = Math.ceil(x);
  return [255,
          x & 0xffffff,
          Math.floor(x * caml_int64_offset) & 0xffffff,
          Math.floor(x * caml_int64_offset * caml_int64_offset) & 0xffff];
}


//Provides: caml_float_of_string mutable
//Requires: caml_failwith
function caml_float_of_string(s) {
  //console.log("#### caml_float_of_string ####");
  var res;
  s = s.getFullBytes();
  res = +s;
  if ((s.length > 0) && (res === res)) return [253, res];
  s = s.replace(/_/g,"");
  res = +s;
  if (((s.length > 0) && (res === res)) || /^[+-]?nan$/i.test(s)) return [253, res];
  caml_failwith("float_of_string");
}


//Provides: caml_format_float const
//Requires: caml_parse_format, caml_finish_formatting
function caml_format_float (fmt, x) {
  //console.log("#### caml_format_float ####");
  var x = x[1];
    var s, f = caml_parse_format(fmt);
  var prec = (f.prec < 0)?6:f.prec;
  if (x < 0) { f.sign = -1; x = -x; }
  if (isNaN(x)) { s = "nan"; f.filler = ' '; }
  else if (!isFinite(x)) { s = "inf"; f.filler = ' '; }
  else
    switch (f.conv) {
    case 'e':
      var s = x.toExponential(prec);
      // exponent should be at least two digits
      var i = s.length;
      if (s.charAt(i - 3) == 'e')
        s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
      break;
    case 'f':
      s = x.toFixed(prec); break;
    case 'g':
      prec = prec?prec:1;
      s = x.toExponential(prec - 1);
      var j = s.indexOf('e');
      var exp = +s.slice(j + 1);
      if (exp < -4 || x.toFixed(0).length > prec) {
        // remove trailing zeroes
        var i = j - 1; while (s.charAt(i) == '0') i--;
        if (s.charAt(i) == '.') i--;
        s = s.slice(0, i + 1) + s.slice(j);
        i = s.length;
        if (s.charAt(i - 3) == 'e')
          s = s.slice (0, i - 1) + '0' + s.slice (i - 1);
        break;
      } else {
        var p = prec;
        if (exp < 0) { p -= exp + 1; s = x.toFixed(p); }
        else while (s = x.toFixed(p), s.length > prec + 1) p--;
        if (p) {
          // remove trailing zeroes
          var i = s.length - 1; while (s.charAt(i) == '0') i--;
          if (s.charAt(i) == '.') i--;
          s = s.slice(0, i + 1);
        }
      }
      break;
    }
  return caml_finish_formatting(f, s);
}


//Specific to Js_of_ocaml 

//Provides: caml_js_from_float const
function caml_js_from_float(x) { 
    //console.log("### caml_js_from_float ####");
    return x[1]; }
//Provides: caml_js_to_float const
function caml_js_to_float(x) { 
    //console.log("### caml_js_from_float ####");
    
return [253, x]; }


// Js_of_ocaml runtime support
// http://www.ocsigen.org/js_of_ocaml/
// Copyright (C) 2010 Jérôme Vouillon
// Laboratoire PPS - CNRS Université Paris Diderot
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

// Weak API, but without the weak semantics

//Provides: caml_weak_create
function caml_weak_create (n) {
  var x = [251];
  x.length = n + 2;
  return x;
}
//Provides: caml_weak_set
function caml_weak_set(x, i, v) { x[i + 1] = v; return 0; }
//Provides: caml_weak_get mutable
function caml_weak_get(x, i) { return (x[i + 1]===undefined)?0:x[i + 1]; }
//Provides: caml_weak_get_copy mutable
//Requires: caml_weak_get
function caml_weak_get_copy(x, i) {
  var y = caml_weak_get(x, i);
  if (y === 0) return y;
  var z = y[1];
  if (z instanceof Array) return [0, z.slice()];
  return y;
}
//Provides: caml_weak_check mutable
function caml_weak_check(x, i) {
  return x[i + 1]!==undefined && x[i + 1] !==0;
}
//Provides: caml_weak_blit
//Requires: caml_array_blit
var caml_weak_blit = caml_array_blit;


