#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# SYNOPSIS
#
#   AX_CABAL_VERSION([VERSION],[ACTION-IF-TRUE],[ACTION-IF-FALSE])
#
# DESCRIPTION
#
#   Check the Cabal package version (not the cabal-install version).
#   The Cabal package major version number may be used to infer how
#   cabal-install may be used.
#

AC_DEFUN([AX_CABAL_VERSION],[
    AC_REQUIRE([AC_PROG_SED])

    AS_IF([test "x$GHC_PKG" = "x"],[
        AC_MSG_WARN([could not find the GHC package manager])
        $3
    ],[
        AC_MSG_CHECKING([for Cabal version])
        cabal_version=`$GHC_PKG latest Cabal | $SED 's/^Cabal-//'`
        AS_IF([test "x$cabal_version" = "x"],[
            AC_MSG_RESULT(not found)
            $3
        ],[
            AC_MSG_RESULT($cabal_version)

            ax_cabal_version="$1"
    
            cabal_version_major=`echo "$cabal_version" | $SED 's/\..*$//'`
            AC_SUBST([CABAL_VERSION],[$cabal_version])
            AC_SUBST([CABAL_VERSION_MAJOR],[$cabal_version_major])
    
            AX_COMPARE_VERSION([$ax_cabal_version],[le],[$cabal_version],[
                :
                $2
            ],[
                :
                $3
            ])
        ])
    ])
])
