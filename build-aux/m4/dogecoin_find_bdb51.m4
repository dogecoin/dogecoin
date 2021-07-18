dnl Copyright (c) 2013-2015 The Bitcoin Core developers
dnl Distributed under the MIT software license, see the accompanying
dnl file COPYING or http://www.opensource.org/licenses/mit-license.php.

AC_DEFUN([BITCOIN_FIND_BDB51],[
  AC_ARG_VAR(BDB_CFLAGS, [C compiler flags for BerkeleyDB, bypasses autodetection])
  AC_ARG_VAR(BDB_LIBS, [Linker flags for BerkeleyDB, bypasses autodetection])
  if test "x$BDB_CFLAGS" = "x"; then
    AC_MSG_CHECKING([for Berkeley DB C++ headers])
    BDB_CPPFLAGS=
    bdbpath=X
    bdb51path=X
    bdbdirlist=
    for _vn in 5.1 ''; do
      for _pfx in b lib ''; do
        bdbdirlist="$bdbdirlist ${_pfx}db${_vn}"
      done
    done
    for searchpath in $bdbdirlist ''; do
      test -n "${searchpath}" && searchpath="${searchpath}/"
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <${searchpath}db_cxx.h>
      ]],[[
        #if !((DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR >= 1) || DB_VERSION_MAJOR > 5)
          #error "failed to find bdb 5.1+"
        #endif
      ]])],[
        if test "x$bdbpath" = "xX"; then
          bdbpath="${searchpath}"
        fi
      ],[
        continue
      ])
      AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        #include <${searchpath}db_cxx.h>
      ]],[[
        #if !(DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR == 1)
          #error "failed to find bdb 5.1"
        #endif
      ]])],[
        bdb51path="${searchpath}"
        break
      ],[])
    done
    if test "x$bdbpath" = "xX"; then
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([libdb_cxx headers missing, ]AC_PACKAGE_NAME[ requires this library for wallet functionality (--disable-wallet to disable wallet functionality)])
    elif test "x$bdb51path" = "xX"; then
      BITCOIN_SUBDIR_TO_INCLUDE(BDB_CPPFLAGS,[${bdbpath}],db_cxx)
      AC_ARG_WITH([incompatible-bdb],[AS_HELP_STRING([--with-incompatible-bdb], [allow using a bdb version other than 5.1])],[
        AC_MSG_WARN([Found Berkeley DB other than 5.1; wallets opened by this build will not be portable!])
      ],[
        AC_MSG_ERROR([Found Berkeley DB other than 5.1, required for portable wallets (--with-incompatible-bdb to ignore or --disable-wallet to disable wallet functionality)])
      ])
    else
      BITCOIN_SUBDIR_TO_INCLUDE(BDB_CPPFLAGS,[${bdb51path}],db_cxx)
      bdbpath="${bdb51path}"
    fi
  else
    BDB_CPPFLAGS=${BDB_CFLAGS}
  fi
  AC_SUBST(BDB_CPPFLAGS)
  if test "x$BDB_LIBS" = "x"; then
    # TODO: Ideally this could find the library version and make sure it matches the headers being used
    for searchlib in db_cxx-5.1 db_cxx; do
      AC_CHECK_LIB([$searchlib],[main],[
        BDB_LIBS="-l${searchlib}"
        break
      ])
    done
    if test "x$BDB_LIBS" = "x"; then
        AC_MSG_ERROR([libdb_cxx missing, ]AC_PACKAGE_NAME[ requires this library for wallet functionality (--disable-wallet to disable wallet functionality)])
    fi
  fi
  AC_SUBST(BDB_LIBS)
])