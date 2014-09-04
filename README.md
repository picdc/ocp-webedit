./opam.install.depend

The commands needed are in the file mklib.sh. After running ocp-build build, i think the script mklib.sh will do the job.

cd src/ocaml-src
ocp-build init
ocp-build webedit_admin
./mklib.sh  
cd ../..
./configure


Another thing:  to use the package 'admin', something must be changed in admintool.ml beforehand , which are the two variables 'root_directory' and 'data_directory'.

They are defined in the /src/common-src/commonConfig.ml, but I haven't figured out how to use ocp-build to include that file so that I can just write 'open CommonConfig' in admintool.ml to use them, so I just defined them as two virables.
