1AC_INIT(ocp-webedit,0.1)
CONFIGURE_ARGS=$*

AC_COPYRIGHT(Copyright 2013 OcamlPro SAS)
AC_CONFIG_MACRO_DIR([m4])

AC_PROG_OCAML
AC_PROG_CAMLP4
if test "$OCAMLC" = "no"; then
   AC_MSG_ERROR([You must install the OCaml compiler])
fi

AC_PROG_CC

AC_ARG_ENABLE(version-check,
 [  --disable-version-check            do not check OCaml version],
 [VERSION_CHECK="$enableval"],
 [VERSION_CHECK="yes"])

enableval=no
AC_ARG_ENABLE(localhost,
 [ --enable-localhost            test locally],
 [LOCALHOST="$enableval"],
 [LOCALHOST="no"])

AX_SPLIT_VERSION(OCAMLVERSION)
OCAML_MAJOR_VERSION=$AX_MAJOR_VERSION
OCAML_MINOR_VERSION=$AX_MINOR_VERSION
OCAML_POINT_VERSION=$AX_POINT_VERSION

# Check that OCaml version is greater or equal to 4.00.0
if test "$VERSION_CHECK" = "yes" ; then
   AX_COMPARE_VERSION( [$OCAMLVERSION], [lt], [3.12.1],
     AC_MSG_ERROR([Your version of OCaml: $OCAMLVERSION is not supported]))
fi

if test "x$prefix" = "xNONE"; then
#  prefix=$ac_default_prefix
  prefix="`eval dirname $OCAMLLIB`"
  libdir=$OCAMLLIB
  base="`eval basename $prefix`"
  echo base=$base
  if test "x$base" = "xlib"; then
    prefix="`eval dirname $prefix`"
  fi
else
  libdir=${libdir}/ocaml
fi
test "x$exec_prefix" = xNONE && exec_prefix='${prefix}'

libdir=${libdir}/typerex

bindir="`eval echo ${bindir}`"
bindir="`eval echo ${bindir}`"
mandir="`eval echo ${mandir}`"
mandir="`eval echo ${mandir}`"
libdir="`eval echo ${libdir}`"
libdir="`eval echo ${libdir}`"
datarootdir="`eval echo ${datarootdir}`"
datarootdir="`eval echo ${datarootdir}`"
datadir=${datarootdir}/typerex

COMPILER_LIBS=`ocp-build query -libdir compiler-libs`
JS_COMPILER_LIBS=`ocp-build query -libdir js_of_ocaml.compiler`
JS_OF_OCAML=`ocp-build query -libdir js_of_ocaml`  

if test x$LOCALHOST = xyes; then
  OFFLINE_MODE="true"
  INTERNAL_PORT="4444"
  HOSTNAME="localhost"
  SERVER_NAME="$HOSTNAME:$INTERNAL_PORT"
  REDIR_PREFIX=""
  ROOT_DIRECTORY=`cd ..;pwd`"/site/$OCAMLVERSION"
  DATA_DIRECTORY="$ROOT_DIRECTORY/data"
  WWW_DIRECTORY="$ROOT_DIRECTORY/www"
else
  OFFLINE_MODE="false"
  INTERNAL_PORT="4444"
  HOSTNAME="edit.ocamlpro.com"
  SERVER_NAME="$HOSTNAME"
  REDIR_PREFIX="api"
  ROOT_DIRECTORY="/home/edit.ocamlpro.com"
  DATA_DIRECTORY="$ROOT_DIRECTORY/data"
  WWW_DIRECTORY="$ROOT_DIRECTORY/www"
fi



AC_SUBST(OFFLINE_MODE)
AC_SUBST(SERVER_NAME)
AC_SUBST(DATA_DIRECTORY)
AC_SUBST(REDIR_PREFIX)
AC_SUBST(INTERNAL_PORT)
AC_SUBST(ROOT_DIRECTORY)
AC_SUBST(WWW_DIRECTORY)
AC_SUBST(HOSTNAME)

AC_SUBST(COMPILER_LIBS)
AC_SUBST(JS_COMPILER_LIBS)
AC_SUBST(JS_OF_OCAML)
AC_SUBST(OCAML_MAJOR_VERSION)
AC_SUBST(OCAML_MINOR_VERSION)
AC_SUBST(OCAML_POINT_VERSION)
AC_SUBST(CONFIGURE_ARGS)
AC_SUBST(VERSION)
AC_CONFIG_FILES(
  Makefile \
  ../src/common-src/commonConfig.ml \
)
AC_OUTPUT

echo Server will be installed in $ROOT_DIRECTORY
