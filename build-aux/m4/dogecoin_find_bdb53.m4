dnl Copyright (c) 2013-2017 The Bitcoin Core developers
dnl Copyright (c) 2021 The Dogecoin Core developers
dnl Distributed under the MIT software license, see the accompanying
dnl file COPYING or http://www.opensource.org/licenses/mit-license.php.

AC_DEFUN([BITCOIN_FIND_BDB53],[
  AC_ARG_VAR([BDB_CFLAGS], [C compiler flags for BerkeleyDB, bypasses autodetection])
  AC_ARG_VAR([BDB_LIBS], [Linker flags for BerkeleyDB, bypasses autodetection])

  AC_MSG_CHECKING([for Berkeley DB C++ headers])

  dnl if BDB_CFLAGS is specified, add it to CPPFLAGS and override the searchpath
  dnl store the current CPPFLAGS to restore later
  if test "x$BDB_CFLAGS" != "x"; then
    TCFLAGS="${CPPFLAGS}"
    CPPFLAGS="${CPPFLAGS} ${BDB_CFLAGS}"
  else
    BDB_CPPFLAGS=
    BDB_LIBS=
    bdbdirlist=
    for _vn in 5.3 53 5 ''; do
      for _pfx in b lib ''; do
        bdbdirlist="$bdbdirlist ${_pfx}db${_vn}"
      done
    done
  fi

  bdbpath=X
  bdb53path=X

  for searchpath in $bdbdirlist ''; do
    test -n "${searchpath}" && searchpath="${searchpath}/"
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
      #include <${searchpath}db_cxx.h>
    ]],[[
      #if !((DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR >= 3) || DB_VERSION_MAJOR > 5)
        #error "failed to find bdb 5.3+"
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
      #if !(DB_VERSION_MAJOR == 5 && DB_VERSION_MINOR == 3)
        #error "failed to find bdb 5.3"
      #endif
    ]])],[
      bdb53path="${searchpath}"
      break
    ],[])
  done
  if test "x$bdbpath" = "xX"; then
    AC_MSG_RESULT([no])
    AC_MSG_ERROR([libdb_cxx headers missing, Dogecoin Core requires this library for wallet functionality (--disable-wallet to disable wallet functionality)])
  elif test "x$bdb53path" = "xX"; then
    BITCOIN_SUBDIR_TO_INCLUDE(BDB_CPPFLAGS,[${bdbpath}],db_cxx)
    AC_ARG_WITH([incompatible-bdb],[AS_HELP_STRING([--with-incompatible-bdb], [allow using a bdb version other than 5.3])],[
      AC_MSG_WARN([Found Berkeley DB other than 5.3; wallets opened by this build will not be portable!])
    ],[
      AC_MSG_ERROR([Found Berkeley DB other than 5.3, required for portable wallets (--with-incompatible-bdb to ignore or --disable-wallet to disable wallet functionality)])
    ])
  else
    BITCOIN_SUBDIR_TO_INCLUDE(BDB_CPPFLAGS,[${bdb53path}],db_cxx)
    bdbpath="${bdb53path}"
  fi

  dnl restore original CPPFLAGS and fixate the now checked flags.
  if test "x$BDB_CFLAGS" != "x"; then
    CPPFLAGS="${TCFLAGS}"
    BDB_CPPFLAGS=${BDB_CFLAGS}
  fi

  AC_SUBST(BDB_CPPFLAGS)

  if test "$BDB_LIBS" = ""; then
    # TODO: Ideally this could find the library version and make sure it matches the headers being used
    for searchlib in db_cxx-5.3 db_cxx; do
      AC_CHECK_LIB([$searchlib],[main],[
        BDB_LIBS="-l${searchlib}"
        break
      ])
    done
    if test "x$BDB_LIBS" = "x"; then
        AC_MSG_ERROR([libdb_cxx missing, Dogecoin Core requires this library for wallet functionality (--disable-wallet to disable wallet functionality)])
    fi
  else
    TLIBS="${LIBS}"
    LIBS="${LIBS} ${BDB_LIBS}"
    AC_SEARCH_LIBS([main],[],[],[
        AC_MSG_ERROR([libdb_cxx missing, Dogecoin Core requires this library for wallet functionality (--disable-wallet to disable wallet functionality)])
    ])
    LIBS="${TLIBS}"
  fi
  AC_SUBST(BDB_LIBS)
])
