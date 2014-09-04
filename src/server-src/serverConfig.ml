
open CommonConfig

let output_server_conf server_conf services =
  let oc = open_out server_conf in
  Printf.fprintf oc
"netplex {
  controller {
    max_level=\"debug\";
    logging {
      type=\"stderr\";
    }
  };
  service {
    name=\"ace-edit_service\";
    protocol {
      name=\"ace-edit_protocol\";
      address {
        type=\"internet\";
	bind=\"0.0.0.0:%d\";
      };
    };
    processor {
      type=\"ace-edit_processor\";
      timeout=300.0;
      timeout_next_request=15.0;
      access_log=\"enabled\";
      suppress_broken_pipe=true;
      host {
        pref_name = \"%s\";
        pref_port = %d;
        names=\"*:0\";
	uri {
	  path=\"/\";
	  service {
	    type=\"file\";
	    docroot=\"%s\";
	    media_types_file=\"/etc/mime.types\";
	    default_media_type=\"text/html\";
	    enable_gzip=true;
	    index_files=\"index.html\";
	    enable_listings=true;
	  };
	};
"
internal_port hostname internal_port www_directory;
  List.iter (fun (path, handler) ->
    Printf.fprintf oc
"
	uri {
	  path=\"%s\";
	  service {
	    type=\"dynamic\";
	    handler=\"%s\";
	  };
	};
" path handler) services;
    Printf.fprintf oc
"
      };
    };
    workload_manager {
      type=\"dynamic\";
      max_jobs_per_thread = 1;
      min_free_jobs_capacity = 2;
      max_free_jobs_capacity = 5;
      max_threads = 50;
    };
  };
}
";
close_out oc
