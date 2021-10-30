dnl @synopsis AX_CXX_CHECK_LIB(libname, functioname, action-if, action-if-not)
dnl
dnl The standard AC_CHECK_LIB can not test functions in namespaces.
dnl Therefore AC_CHECK_LIB(cgicc, cgicc::Cgicc::getVersion) will always
dnl fail. We need to decompose the functionname into a series of namespaces
dnl where it gets declared so that it can be used for a link test.
dnl
dnl In the first version I did allow namespace::functionname to be a
dnl reference to a void-argument global functionname (just wrapped in a
dnl namespace) like its C counterparts would be - but in reality such
dnl thing does not exist. The only global / static functions are always
dnl made const-functions which is an attribute mangled along into the
dnl library function export name. 
dnl
dnl The normal usage will ask for a test of a class-member function which
dnl should be presented with a full function spec with arguments given in 
dnl parentheses following the function name - if the function to test for 
dnl does expect arguments then you should add default initial values in the 
dnl prototype (even if they do not exist originally, these are used only 
dnl locally to build a correct function call in the configure test script).
dnl
dnl In the current version if you do omit the parenthesis from the macro
dnl argument then the macro will assume that you want to check for the
dnl class name - which is really to check for default constructor being
dnl exported from the given library name. 
dnl
dnl   EXAMPLE:
dnl AX_CXX_CHECK_LIB(cgicc, [cgicc::HTTPCookie])
dnl AX_CXX_CHECK_LIB(cgicc, [cgicc::Cgicc::getVersion () const],
dnl AX_CXX_CHECK_LIB(boost_regex, [boost::RegEx::Position (int i = 0) const])
dnl
dnl Result:
dnl Just as the usual AX_CXX_CHECK_LIB - defines HAVE_LIBCGICC 
dnl and adds the libraries to the default library path (and
dnl uses internally the normal ac_check_lib cache symbol
dnl like ac_cv_lib_cgicc_cgicc__Cgicc)
dnl
dnl Footnote: The C++ language is not good at creating stable library
dnl interfaces at the binary level - a lot of functionality is usually being 
dnl given as inline functions plus there is hardly a chance to create opaque 
dnl types. Therefore most C++ library tests will only do compile tests using
dnl the header files. Doing a check_lib is however good to check the link
dnl dependency before hitting it as an error in the build later.
dnl
dnl @category C++
dnl @author Guido U. Draheim
dnl @vesion 2006-12-18

AC_DEFUN([AX_CXX_CHECK_LIB],
[m4_ifval([$3], , [AH_CHECK_LIB([$1])])dnl
AS_LITERAL_IF([$1],
	      [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
	      [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])dnl
AC_CACHE_CHECK([for $2 in -l$1], ac_Lib,
[ac_check_lib_save_LIBS=$LIBS
LIBS="-l$1 $5 $LIBS"
case "$2" 
in *::*::*\(*)
AC_LINK_IFELSE([AC_LANG_PROGRAM([
 namespace `echo "$2" | sed -e "s/::.*//"` 
 { class `echo "$2" | sed -e "s/.*::\\(.*\\)::.*/\\1/" -e "s/(.*//"` 
   { public: int `echo "$2" | sed -e "s/.*:://" -e "/(/!s/..*/&()/"`;
   };
 }
],[`echo "$2" | sed  -e "s/(.*//" -e "s/\\(.*\\)::\\(.*\\)/((\\1*)(0))->\\2/g"`()])],
	       [AS_VAR_SET(ac_Lib, yes)],
	       [AS_VAR_SET(ac_Lib, no)])
;; *::*::*)
AC_LINK_IFELSE([AC_LANG_PROGRAM([
 namespace `echo "$2" | sed -e "s/::.*//"` 
 { namespace `echo "$2" | sed -e "s/.*::\\(.*\\)::.*/\\1/"` 
   { class `echo "$2" | sed -e "s/.*:://"` 
      { public: `echo "$2" | sed -e "s/.*:://"` ();
      };
   }
 }
],[new $2()])],
	       [AS_VAR_SET(ac_Lib, yes)],
	       [AS_VAR_SET(ac_Lib, no)])
;; *::*\(*)
AC_LINK_IFELSE([AC_LANG_PROGRAM([
 class `echo "$2" | sed -e "s/\\(.*\\)::.*/\\1/" -e "s/(.*//"` 
   { public: int `echo "$2" | sed -e "s/.*:://" -e "/(/!s/..*/&()/"`;
   };
],[`echo "$2" | sed  -e "s/(.*//" -e "s/\\(.*\\)::\\(.*\\)/((\\1*)(0))->\\2/g"`()])],
	       [AS_VAR_SET(ac_Lib, yes)],
	       [AS_VAR_SET(ac_Lib, no)])
;; *::*)
AC_LINK_IFELSE([AC_LANG_PROGRAM([
 namespace `echo "$2" | sed -e "s/::.*//"` 
 { class `echo "$2" | sed -e "s/.*:://"`
   { public: `echo "$2" | sed -e "s/.*:://"` ();
   };
 }
],[new $2()])],
	       [AS_VAR_SET(ac_Lib, yes)],
	       [AS_VAR_SET(ac_Lib, no)])
;; *)
AC_LINK_IFELSE([AC_LANG_CALL([], [$2])],
	       [AS_VAR_SET(ac_Lib, yes)],
	       [AS_VAR_SET(ac_Lib, no)])
;; esac
LIBS=$ac_check_lib_save_LIBS])
AS_IF([test AS_VAR_GET(ac_Lib) = yes],
      [m4_default([$3], [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIB$1))
  LIBS="-l$1 $LIBS"
])],
      [$4])dnl
AS_VAR_POPDEF([ac_Lib])dnl
])# AC_CHECK_LIB
