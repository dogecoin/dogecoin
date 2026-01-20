# ===========================================================================
#    https://www.gnu.org/software/autoconf-archive/ax_boost_date_time.html
# ===========================================================================
#
# SYNOPSIS
#
#   AX_BOOST_DATE_TIME
#
# DESCRIPTION
#
#   Test for Date_Time library from the Boost C++ libraries. The macro
#   requires a preceding call to AX_BOOST_BASE. Further documentation is
#   available at <http://randspringer.de/boost/index.html>.
#
#   This macro calls:
#
#     AC_SUBST(BOOST_DATE_TIME_LIB)
#
#   And sets:
#
#     HAVE_BOOST_DATE_TIME
#
# LICENSE
#
#   Copyright (c) 2008 Thomas Porschberg <thomas@randspringer.de>
#   Copyright (c) 2008 Michael Tindal
#
#   Copying and distribution of this file, with or without modification, are
#   permitted in any medium without royalty provided the copyright notice
#   and this notice are preserved. This file is offered as-is, without any
#   warranty.

#serial 23

AC_DEFUN([AX_BOOST_DATE_TIME],
[
	AC_ARG_WITH([boost-date-time],
	AS_HELP_STRING([--with-boost-date-time@<:@=special-lib@:>@],
                   [use the Date_Time library from boost - it is possible to specify a certain library for the linker
                        e.g. --with-boost-date-time=boost_date_time-gcc-mt-d-1_33_1 ]),
        [
        if test "$withval" = "no"; then
			want_boost="no"
        elif test "$withval" = "yes"; then
            want_boost="yes"
            ax_boost_user_date_time_lib=""
        else
		    want_boost="yes"
		ax_boost_user_date_time_lib="$withval"
		fi
        ],
        [want_boost="yes"]
	)

	if test "x$want_boost" = "xyes"; then
        AC_REQUIRE([AC_PROG_CC])
		CPPFLAGS_SAVED="$CPPFLAGS"
		CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"
		export CPPFLAGS

		LDFLAGS_SAVED="$LDFLAGS"
		LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"
		export LDFLAGS

        AC_CACHE_CHECK(whether the Boost::Date_Time library is available,
					   ax_cv_boost_date_time,
        [AC_LANG_PUSH([C++])
		 AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[@%:@include <boost/date_time/gregorian/gregorian_types.hpp>]],
                                   [[using namespace boost::gregorian; date d(2002,Jan,10);
                                     return 0;
                                   ]])],
         ax_cv_boost_date_time=yes, ax_cv_boost_date_time=no)
         AC_LANG_POP([C++])
		])
		if test "x$ax_cv_boost_date_time" = "xyes"; then
			AC_DEFINE(HAVE_BOOST_DATE_TIME,,[define if the Boost::Date_Time library is available])
            BOOSTLIBDIR=`echo $BOOST_LDFLAGS | sed -e 's/@<:@^\/@:>@*//'`
            if test "x$ax_boost_user_date_time_lib" = "x"; then
                for libextension in `ls $BOOSTLIBDIR/libboost_date_time*.so* $BOOSTLIBDIR/libboost_date_time*.dylib* $BOOSTLIBDIR/libboost_date_time*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^lib\(boost_date_time.*\)\.so.*$;\1;' -e 's;^lib\(boost_date_time.*\)\.dylib.*$;\1;' -e 's;^lib\(boost_date_time.*\)\.a*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_DATE_TIME_LIB="-l$ax_lib"; AC_SUBST(BOOST_DATE_TIME_LIB) link_date_time="yes"; break],
                                 [link_date_time="no"])
				done
                if test "x$link_date_time" != "xyes"; then
                for libextension in `ls $BOOSTLIBDIR/boost_date_time*.dll* $BOOSTLIBDIR/boost_date_time*.a* 2>/dev/null | sed 's,.*/,,' | sed -e 's;^\(boost_date_time.*\)\.dll.*$;\1;' -e 's;^\(boost_date_time.*\)\.a.*$;\1;'` ; do
                     ax_lib=${libextension}
				    AC_CHECK_LIB($ax_lib, exit,
                                 [BOOST_DATE_TIME_LIB="-l$ax_lib"; AC_SUBST(BOOST_DATE_TIME_LIB) link_date_time="yes"; break],
                                 [link_date_time="no"])
				done
                fi

            else
               for ax_lib in $ax_boost_user_date_time_lib boost_date_time-$ax_boost_user_date_time_lib; do
				      AC_CHECK_LIB($ax_lib, main,
                                   [BOOST_DATE_TIME_LIB="-l$ax_lib"; AC_SUBST(BOOST_DATE_TIME_LIB) link_date_time="yes"; break],
                                   [link_date_time="no"])
                  done

            fi
            if test "x$ax_lib" = "x"; then
                AC_MSG_ERROR(Could not find a version of the Boost::Date_Time library!)
            fi
			if test "x$link_date_time" != "xyes"; then
				AC_MSG_ERROR(Could not link against $ax_lib !)
			fi
		fi

		CPPFLAGS="$CPPFLAGS_SAVED"
	LDFLAGS="$LDFLAGS_SAVED"
	fi
])
