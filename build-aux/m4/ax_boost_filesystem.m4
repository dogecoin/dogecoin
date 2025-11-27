# -----------------------------------------------------------------------------
# AX_BOOST_FILESYSTEM (Optimized Version)
# Checks for the Boost Filesystem library and sets linkage flags.
#
# Requires: AX_BOOST_BASE (must be called previously)
# Sets: BOOST_FILESYSTEM_LIB, HAVE_BOOST_FILESYSTEM
# -----------------------------------------------------------------------------
# serial 26

AC_DEFUN([AX_BOOST_FILESYSTEM],
[
    # Ensure basic C compiler is configured, necessary for AC_COMPILE_IFELSE
    AC_REQUIRE([AC_PROG_CC])

    AC_ARG_WITH([boost-filesystem],
        AS_HELP_STRING([--with-boost-filesystem@<:@=special-lib@:>@],
            [use the Filesystem library from boost - it is possible to specify a certain library for the linker
                 e.g. --with-boost-filesystem=boost_filesystem-gcc-mt ]),
        [
            # Handle user input for the --with-boost-filesystem argument
            if test "$withval" = "no"; then
                want_boost="no"
            elif test "$withval" = "yes"; then
                want_boost="yes"
                ax_boost_user_filesystem_lib=""
            else
                want_boost="yes"
                ax_boost_user_filesystem_lib="$withval"
            fi
        ],
        [want_boost="yes"]
    )

    if test "x$want_boost" = "xyes"; then
        # Save current compiler/linker flags before modifying for Boost check
        CPPFLAGS_SAVED="$CPPFLAGS"
        CPPFLAGS="$CPPFLAGS $BOOST_CPPFLAGS"

        LDFLAGS_SAVED="$LDFLAGS"
        LDFLAGS="$LDFLAGS $BOOST_LDFLAGS"

        LIBS_SAVED=$LIBS
        # BOOST_SYSTEM_LIB is typically required for Filesystem on many platforms
        LIBS="$LIBS $BOOST_SYSTEM_LIB"

        # Check if the required headers compile
        AC_CACHE_CHECK([whether the Boost::Filesystem library is available],
            [ax_cv_boost_filesystem],
            [AC_LANG_PUSH([C++])
            AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[@%:@include <boost/filesystem/path.hpp>]],
                [[using namespace boost::filesystem;
                path my_path( "foo/bar/data.txt" );
                return 0;]])],
                [ax_cv_boost_filesystem=yes], [ax_cv_boost_filesystem=no])
            AC_LANG_POP([C++])
        ])

        if test "x$ax_cv_boost_filesystem" = "xyes"; then
            AC_DEFINE(HAVE_BOOST_FILESYSTEM,,[define if the Boost::Filesystem library is available])

            # Extract the library directory path from BOOST_LDFLAGS
            BOOSTLIBDIR=`echo $BOOST_LDFLAGS | sed -e 's/^-L//'`
            ax_lib=
            link_filesystem="no"

            if test "x$ax_boost_user_filesystem_lib" = "x"; then
                # User did not specify a library name, try to find it automatically.
                # 1. Search for standard 'libboost_filesystem*' names.
                for libextension in `ls -r $BOOSTLIBDIR/libboost_filesystem* 2>/dev/null | sed 's,.*/lib,,' | sed 's,\..*,,'` ; do
                    ax_lib=${libextension}
                    AC_CHECK_LIB($ax_lib, exit,
                        [BOOST_FILESYSTEM_LIB="-l$ax_lib"; AC_SUBST(BOOST_FILESYSTEM_LIB) link_filesystem="yes"; break],
                        [link_filesystem="no"])
                done

                # 2. If not found, search for non-'lib' prefixed names (common on some systems).
                if test "x$link_filesystem" != "xyes"; then
                    for libextension in `ls -r $BOOSTLIBDIR/boost_filesystem* 2>/dev/null | sed 's,.*/,,' | sed -e 's,\..*,,'` ; do
                        ax_lib=${libextension}
                        AC_CHECK_LIB($ax_lib, exit,
                            [BOOST_FILESYSTEM_LIB="-l$ax_lib"; AC_SUBST(BOOST_FILESYSTEM_LIB) link_filesystem="yes"; break],
                            [link_filesystem="no"])
                    done
                fi
            else
                # User specified a custom library name (e.g., --with-boost-filesystem=boost_filesystem-gcc-mt)
                for ax_lib in $ax_boost_user_filesystem_lib "boost_filesystem-$ax_boost_user_filesystem_lib"; do
                    AC_CHECK_LIB($ax_lib, exit,
                        [BOOST_FILESYSTEM_LIB="-l$ax_lib"; AC_SUBST(BOOST_FILESYSTEM_LIB) link_filesystem="yes"; break],
                        [link_filesystem="no"])
                done
            fi

            # Final verification and error handling
            if test "x$link_filesystem" != "xyes"; then
                if test "x$ax_lib" = "x"; then
                    AC_MSG_ERROR([Could not find a version of the boost_filesystem library in $BOOSTLIBDIR!])
                else
                    AC_MSG_ERROR([Could not link against library $ax_lib !])
                fi
            fi
        fi

        # Restore saved compiler/linker flags
        CPPFLAGS="$CPPFLAGS_SAVED"
        LDFLAGS="$LDFLAGS_SAVED"
        LIBS="$LIBS_SAVED"
    fi
])
