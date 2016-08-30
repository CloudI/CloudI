#-*-Mode:m4;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=m4 fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
#     ERLANG_OTP_VER_MINOR (e.g., "1")
#     ERLANG_OTP_VER_PATCH (e.g., "")
#     ERLANG_OTP_VER_RELEASE_CANDIDATE (e.g., "0")
#
#   For backwards compatability the script also handles pre 17.0 versions:
#     ERLANG_OTP_VER (e.g., "R14B01")
#     ERLANG_OTP_VER_MAJOR (e.g., "14")
#     ERLANG_OTP_VER_MINOR (e.g., "01")
#     ERLANG_OTP_VER_PATCH (e.g., "")
#     ERLANG_OTP_VER_RELEASE_CANDIDATE (e.g., "")
#
#   WARNING: After the 17.0 Erlang/OTP release, both
#            ERLANG_OTP_VER_MINOR and ERLANG_OTP_VER_RELEASE_CANDIDATE will be
#            no longer correct due to the Erlang/OTP team not wanting to
#            provide detailed version information.  For autoconf, you should
#            instead prefer AC_ERLANG_CHECK_LIB and AC_ERLANG_SUBST_ERTS_VER
#            to check the version of individual OTP applications.
#            An OTP_VERSION file may be present in the installation.
#            If it is, it is checked.
#   (http://erlang.org/pipermail/erlang-questions/2014-April/078590.html)

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
    ax_erlang_otp_ver_major=`expr $ax_cv_erlang_otp_ver : 'R*\([[0-9]]*\)'`
    if test "$ax_erlang_otp_ver_major" -ge 17; then
        AC_CACHE_CHECK([for the Erlang OTP version],
            [ax_cv_erlang_otp_package_ver],
            [AC_LANG_PUSH([Erlang])
             AC_RUN_IFELSE(
                [AC_LANG_PROGRAM([], [
                    Major = erlang:system_info(otp_release),
                    Version = try erlang:system_info(otp_correction_package)
                    catch
                        error:badarg ->
                            VersionPath = filename:join([[code:root_dir(),
                                                          "releases", Major,
                                                          "OTP_VERSION"]]),
                            case file:read_file(VersionPath) of
                                {ok, FileVersion} ->
                                    %% 17.1.x and after
                                    [[DetailedVersion |
                                      _]] = binary:split(FileVersion,
                                                         [[<<"\r">>, <<"\n">>,
                                                           <<" ">>]]),
                                    binary_to_list(DetailedVersion);
                                {error, _} ->
                                    Major ++ ".0.0"
                            end
                    end,
                    file:write_file("conftest.out", Version),
                    ReturnValue = 0,
                    halt(ReturnValue)])],
                [ax_cv_erlang_otp_package_ver=`cat conftest.out`
                 rm -f conftest.out],
                [rm -f conftest.out
                 AC_MSG_FAILURE([test Erlang program execution failed])])
             AC_LANG_POP([Erlang])
        ])
        ax_erlang_otp_ver_minor=`expr $ax_cv_erlang_otp_package_ver : '[[0-9]]*\.\([[0-9]]*\)'`
        ax_erlang_otp_ver_patch=`expr $ax_cv_erlang_otp_package_ver : '[[0-9]]*\.[[0-9]]*.\([[0-9]]*\)'`
        ax_erlang_otp_ver_release_candidate=`expr $ax_cv_erlang_otp_package_ver : '.*-rc\([[0-8]]\)'`
        AC_SUBST([ERLANG_OTP_VER],
                 ["${ax_erlang_otp_ver_major}_${ax_erlang_otp_ver_minor}"])
    else
        ax_erlang_otp_ver_minor=`expr $ax_cv_erlang_otp_ver : 'R[[0-9]]*[[AB]]\([[0-9]]*\)'`
        ax_erlang_otp_ver_patch=""
        ax_erlang_otp_ver_type=`expr $ax_cv_erlang_otp_ver : 'R[[0-9]]*\([[AB]]\)'`
        if test "$ax_erlang_otp_ver_type" = "A"; then
            ax_erlang_otp_ver_release_candidate="0"
        elif test "$ax_erlang_otp_ver_type" = "B"; then
            ax_erlang_otp_ver_release_candidate=""
        else
            AC_MSG_FAILURE([invalid version])
        fi
        AC_SUBST([ERLANG_OTP_VER],
                 ["${ax_erlang_otp_ver_major}${ax_erlang_otp_ver_type}${ax_erlang_otp_ver_minor}"])
    fi
    AC_SUBST([ERLANG_OTP_VER_MAJOR], [$ax_erlang_otp_ver_major])
    AC_SUBST([ERLANG_OTP_VER_MINOR], [$ax_erlang_otp_ver_minor])
    AC_SUBST([ERLANG_OTP_VER_PATCH], [$ax_erlang_otp_ver_patch])
    AC_SUBST([ERLANG_OTP_VER_RELEASE_CANDIDATE],
             [$ax_erlang_otp_ver_release_candidate])
])

AC_DEFUN([AX_ERLANG_REQUIRE_OTP_VER],
[
    erlang_otp_version_req=ifelse([$1], ,R10B01,$1)
    erlang_otp_version_req_major=`expr $erlang_otp_version_req : 'R*\([[0-9]]*\)'`
    if test "$erlang_otp_version_req_major" -ge 17; then
        erlang_otp_version_req_minor=`expr $erlang_otp_version_req : '[[0-9]]*\.\([[0-9]]*\)'`
        erlang_otp_version_req_patch=`expr $erlang_otp_version_req : '[[0-9]]*\.[[0-9]]*.\([[0-9]]*\)'`
        erlang_otp_version_req_release_candidate=`expr $erlang_otp_version_req : '.*-rc\([[0-9]]\)'`
        if test "x$erlang_otp_version_req_minor" = "x"; then
            erlang_otp_version_req_minor="0"
        fi
        if test "x$erlang_otp_version_req_patch" = "x"; then
            erlang_otp_version_req_patch="0"
        fi
        if test "x$erlang_otp_version_req_release_candidate" = "x"; then
            erlang_otp_version_req_release_candidate="0"
        fi
        erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000000 \+ $erlang_otp_version_req_minor \* 1000 \+ $erlang_otp_version_req_patch \* 10 \+ $erlang_otp_version_req_release_candidate`
    else
        erlang_otp_version_req_minor=`expr $erlang_otp_version_req : 'R[[0-9]]*[[AB]]\([[0-9]]*\)'`
        erlang_otp_version_req_type=`expr $erlang_otp_version_req : 'R[[0-9]]*\([[AB]]*\)'`
        if test "$erlang_otp_version_req_type" = "B"; then
            erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000000 \+ $erlang_otp_version_req_minor \* 1000 \+ 999`
        elif test "$erlang_otp_version_req_type" = "A"; then
            erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000000 \+ $erlang_otp_version_req_minor \* 1000`
        elif test "x$erlang_otp_version_req_type" = "x"; then
            erlang_otp_version_req_int=`expr $erlang_otp_version_req_major \* 1000000`
        else
            AC_MSG_FAILURE([invalid version])
        fi
    fi

    AC_MSG_CHECKING(for Erlang >= $erlang_otp_version_req)

    AC_REQUIRE([AC_ERLANG_NEED_ERLC])
    AC_REQUIRE([AC_ERLANG_NEED_ERL])
    AC_LANG_PUSH([Erlang])
    AC_RUN_IFELSE(
        [AC_LANG_PROGRAM([], [
            Version = case erlang:system_info(otp_release) of
                [[\$R, Major1, Major2, T1 | MinorT]] ->
                    %% before 17.0
                    Minor = if
                        MinorT == [[]] ->
                            0;
                        true ->
                            %% Remove -1 from R16B03-1
                            [[MinorL | _]] = string:tokens(MinorT, "-"),
                            list_to_integer(MinorL)
                    end,
                    if
                        T1 == \$B ->
                            list_to_integer([[Major1, Major2]]) * 1000000 +
                            Minor * 1000 + 999;
                        T1 == \$A ->
                            list_to_integer([[Major1, Major2]]) * 1000000 +
                            Minor * 1000
                    end;
                Major ->
                    %% 17.0 and after
                    Package = try erlang:system_info(otp_correction_package)
                    catch
                        error:badarg ->
                            VersionPath = filename:join([[code:root_dir(),
                                                          "releases", Major,
                                                          "OTP_VERSION"]]),
                            case file:read_file(VersionPath) of
                                {ok, FileVersion} ->
                                    %% 17.1.x and after
                                    [[DetailedVersion |
                                      _]] = binary:split(FileVersion,
                                                         [[<<"\r">>, <<"\n">>,
                                                           <<" ">>]]),
                                    binary_to_list(DetailedVersion);
                                {error, _} ->
                                    Major ++ ".0.0"
                            end
                    end,
                    [[_, Minor | RCString]] = string:tokens(Package, ".-"),
                    {Patch, RC} = case RCString of
                        [["rc" ++ RCStr | _]] ->
                            {"0", RCStr};
                        [[PatchStr, "rc" ++ RCStr | _]] ->
                            {PatchStr, RCStr};
                        [[PatchStr | _]] ->
                            {PatchStr, "0"};
                        [[]] ->
                            {"0", "0"}
                    end,
                    list_to_integer(Major) * 1000000 +
                    list_to_integer(Minor) * 1000 +
                    list_to_integer(Patch) * 10 +
                    list_to_integer(RC)
            end,
            file:write_file("conftest.out", integer_to_list(Version)),
            ReturnValue = 0,
            halt(ReturnValue)])],
        [ax_erlang_otp_ver=`cat conftest.out`
         rm -f conftest.out],
        [rm -f conftest.out
         AC_MSG_FAILURE([test Erlang program execution failed])
    ])
    AC_LANG_POP([Erlang])

    if test "$ax_erlang_otp_ver" -ge "$erlang_otp_version_req_int"; then
        AC_MSG_RESULT(yes)
        # execute ACTION-IF-FOUND (if present):
        ifelse([$2], , :, [$2])
    else
        AC_MSG_RESULT(no)
        # execute ACTION-IF-NOT-FOUND (if present):
        ifelse([$3], , :, [$3])
    fi
])

