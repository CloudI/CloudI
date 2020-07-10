#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/perl"

install-exec-hook:
	$(MKDIR_P) $(instdir)
	$(MKDIR_P) $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang.pm $(instdir)
	$(INSTALL_DATA) $(srcdir)/Erlang/Exception.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/InputException.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangAtom.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangBinary.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangFunction.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangList.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangPid.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangPort.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangReference.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OtpErlangString.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/OutputException.pm \
                    $(instdir)/Erlang
	$(INSTALL_DATA) $(srcdir)/Erlang/ParseException.pm \
                    $(instdir)/Erlang
	$(MKDIR_P) $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/API.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/ForwardAsyncException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/ForwardSyncException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/InvalidInputException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/MessageDecodingException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/ReturnAsyncException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/ReturnSyncException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/TerminateException.pm \
                    $(instdir)/CloudI
	$(INSTALL_DATA) $(srcdir)/CloudI/AssertionError.pm \
                    $(instdir)/CloudI

