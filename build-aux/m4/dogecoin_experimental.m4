dnl Copyright (c) 2022 The Dogecoin Core developers
dnl Distributed under the MIT software license, see the accompanying
dnl file COPYING or http://www.opensource.org/licenses/mit-license.php.

dnl Helper function to make experimental flag checking easy
dnl experimental functions simply call this macro inside their checks
AC_DEFUN([DOGECOIN_REQUIRE_EXPERIMENTAL],[
  if test x$allow_experimental != xyes; then
    AC_MSG_ERROR([Experimental features need to be enabled explicitly. Use --enable-experimental.])
  fi
])
