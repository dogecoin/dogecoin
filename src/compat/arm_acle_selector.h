// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

// Replace the broken arm_acle.h included in Ubuntu's package
// libgcc-9-dev-arm64-cross (9.4.0-1ubuntu1~20.04.1cross2)
// with the patched version from 8c92fcb13a4979232787f0476bf7469ccbb03617
// at https://gcc.gnu.org/git/?p=gcc.git
#if (__GNUC__ == 9 && __GNUC_MINOR__ == 4)
# include "compat/arm_acle_patched.h"
#else
# include <arm_acle.h>
#endif
