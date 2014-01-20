#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# SYNOPSIS
#
#   AX_ERLANG_SUBST_OTP_VER
#   AX_ERLANG_REQUIRE_OTP_VER([VER],[ACTION-IF-FOUND],[ACTION-IF-NOT-FOUND])
#
# DESCRIPTION
#
#   Based on the implementation of AC_ERLANG_SUBST_ERTS_VER which was
#   written by Romain Lenglet.  Under GPL like other autoconf macros.
#
#   AX_ERLANG_SUBST_OTP_VER sets:
#
#     ERLANG_OTP_VER (e.g., "17.1-rc0")
#     ERLANG_OTP_VER_MAJOR (e.g., "17")
#     ERLANG_OTP_VER_RELEASE_CANDIDATE (e.g., "0")
#     ERLANG_OTP_VER_TYPE (e.g., "0", backwards compatability)
#     ERLANG_OTP_VER_MINOR (e.g., "1")
#
#   For backwards compatability the script also handles pre 17.0 versions:
#     ERLANG_OTP_VER (e.g., "R14B01")
#     ERLANG_OTP_VER_MAJOR (e.g., "14")
#     ERLANG_OTP_VER_RELEASE_CANDIDATE (e.g., "B", forwards compatability)
#     ERLANG_OTP_VER_TYPE (e.g., "B")
#     ERLANG_OTP_VER_MINOR (e.g., "01")
#
#

AC_DEFUN([AX_ERLANG_SUBST_OTP_VER],
[
    AC_REQUIRE([AC_ERLANG_NEED_ERLC])
    AC_REQUIRE([AC_ERLANG_NEED_ERL])
    AC_CACHE_CHECK([for the Erlang OTP release],
        [ax_cv_erlang_otp_ver],
        [AC_LANG_PUSH([Erlang])
         AC_RUN_IFELSE(
            [AC_LANG_PROGRAM([], [
                Version = erlang:system_info(otp_release),
                file:write_file("conftest.out", Version),
                ReturnValue = 0,
                halt(ReturnValue)])],
            [ax_cv_erlang_otp_ver=`cat conftest.out`
             rm -f conftest.out],
            [rm -f conftest.out
            AC_MSG_FAILURE([test Erlang program execution failed])])
        AC_LANG_POP([Erlang])
    ])
    ax_erlang_otp_ver_major=`expr $ax_cv_erlang_otp_ver : 'R\?\([[0-9]]*\)'`
    if test $ax_erlang_otp_ver_major -ge 17; then
        AC_CACHE_CHECK([for the Erlang OTP correction package],
            [ax_cv_erlang_otp_corr_ver],
            [AC_LANG_PUSH([Erlang])
             AC_RUN_IFELSE(
                [AC_LANG_PROGRAM([], [
                    Version = erlang:system_info(otp_correction_package),
                    file:write_file("conftest.out", Version),
                    ReturnValue = 0,
                    halt(ReturnValue)])],
                [ax_cv_erlang_otp_corr_ver=`cat conftest.out`
                 rm -f conftest.out],
                [rm -f conftest.out
                 AC_MSG_FAILURE([test Erlang program execution failed])])
            AC_LANG_POP([Erlang])
        ])
        ax_erlang_otp_ver_release_candidate=`expr $ax_cv_erlang_otp_ver : '.*-rc\([[0-9]]\+\)'`
        ax_erlang_otp_ver_minor=`expr $ax_cv_erlang_otp_ver : '[[0-9]]\+.\([[0-9]]\+\)'`
    else
        ax_erlang_otp_ver_release_candidate=`expr $ax_cv_erlang_otp_ver : 'R?[[0-9]]*\([[A-Z]]*\)'`
        ax_erlang_otp_ver_minor=`expr $ax_cv_erlang_otp_ver : 'R[[0-9]]*[[A-Z]]*\([[0-9]]*\)'`
    fi
    AC_SUBST([ERLANG_OTP_VER], [$ax_cv_erlang_otp_ver])
    AC_SUBST([ERLANG_OTP_VER_MAJOR], [$ax_erlang_otp_ver_major])
    AC_SUBST([ERLANG_OTP_VER_TYPE], [$ax_erlang_otp_ver_release_candidate])
    AC_SUBST([ERLANG_OTP_VER_RELEASE_CANDIDATE],
                [$ax_erlang_otp_ver_release_candidate])
    AC_SUBST([ERLANG_OTP_VER_MINOR], [$ax_erlang_otp_ver_minor])
])

AC_DEFUN([AX_ERLANG_REQUIRE_OTP_VER],
[
    erlang_otp_version_req=ifelse([$1], ,R10B01,$1)
    erlang_otp_version_req_major=`expr $erlang_otp_version_req : 'R\?\([[0-9]]*\)'`
    if test $erlang_otp_version_req_major -ge 17; then
       erlang_otp_version_req_release_candidate=`expr $erlang_otp_version_req : '.*-rc\([[0-9]]\+\)'`
       erlang_otp_version_req_minor=`expr $erlang_otp_version_req : '[[0-9]]\+.\([[0-9]]\+\)'`
       if test "MINOR$erlang_otp_version_req_minor" = "MINOR"; then
         erlang_otp_version_req_minor="0"
       fi
       if test "RC$erlang_otp_version_req_release_candidate" = "RC"; then
          erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000 \+ $erlang_otp_version_req_minor \* 10 \+ 9`
       else
          erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000 \+ $erlang_otp_version_req_minor \* 10 \+ $erlang_otp_version_req_release_candidate`
       fi
    else
       erlang_otp_version_req_type=`expr $erlang_otp_version_req : 'R[[0-9]]*\([[A-Z]]*\)'`
       erlang_otp_version_req_minor=`expr $erlang_otp_version_req : 'R[[0-9]]*[[A-Z]]*\([[0-9]]*\)'`
       if test $erlang_otp_version_req_type = A; then
           erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000 \+ $erlang_otp_version_req_minor \* 10`
       elif test $erlang_otp_version_req_type = B; then
           erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000 \+ $erlang_otp_version_req_minor \* 10 \+ 9`
       fi
    fi

    AC_MSG_CHECKING(for Erlang >= $erlang_otp_version_req)
    succeeded=no

    AC_REQUIRE([AC_ERLANG_NEED_ERLC])
    AC_REQUIRE([AC_ERLANG_NEED_ERL])
    AC_LANG_PUSH([Erlang])
    AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([], [
            case erlang:system_info(otp_release) of
               [[\$R, Maj1, Maj2, T1 | MinT]] ->
                  %% Pre 17.0 release version
                  Min = if
                     MinT == [[]] ->
                        0;
                     true ->
                        [[MinL|_]] = string:tokens(MinT,"-"), %% Remove -1 from R16B03-1
                        list_to_integer(MinL)
                  end,
                  Version = if
                     T1 == \$A ->
                        list_to_integer([[Maj1, Maj2]]) * 1000 + Min * 10;
                     T1 == \$B ->
                        list_to_integer([[Maj1, Maj2]]) * 1000 + Min * 10 + 9
                  end;
               MajVsn ->
                  %% 17.0 and later release version
                  CorrPack = erlang:system_info(otp_correction_package),
                  [[_,Min|_]] = string:tokens(CorrPack,".-"),
                  RC = case string:tokens(CorrPack,"-") of
                      [[_,"rc"++RCString]] -> list_to_integer(RCString);
                      _ -> 9
                  end,
                  Version = list_to_integer(MajVsn) * 1000 + list_to_integer(Min) * 10 + RC
            end,
            file:write_file("conftest.out", integer_to_list(Version)),
            ReturnValue = 0,
            halt(ReturnValue)])],
        [ax_erlang_otp_ver=`cat conftest.out`
        succeeded=yes
        rm -f conftest.out],
        [rm -f conftest.out
        AC_MSG_FAILURE([test Erlang program execution failed])
    ])
    AC_LANG_POP([Erlang])

    if test "x$succeeded" = "xyes" && test $ax_erlang_otp_ver -ge $erlang_otp_version_req_int; then
        AC_MSG_RESULT(yes)
        # execute ACTION-IF-FOUND (if present):
        ifelse([$2], , :, [$2])
    else
        AC_MSG_RESULT(no)
        # execute ACTION-IF-NOT-FOUND (if present):
        ifelse([$3], , :, [$3])
    fi
])

