#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

instdir = "$(DESTDIR)$(cloudi_prefix)/api/java"
CLEANFILES = cloudi.jar

cloudi.jar: $(srcdir)/manifest.txt \
            org/cloudi/API.class \
            org/cloudi/Function9.class \
            org/cloudi/FunctionInterface9.class \
            org/cloudi/FunctionInterface10.class \
            org/cloudi/FunctionObject9.class \
            org/cloudi/FunctionObject10.class \
            com/ericsson/otp/erlang/AbstractConnection.class \
            com/ericsson/otp/erlang/AbstractNode.class \
            com/ericsson/otp/erlang/GenericQueue.class \
            com/ericsson/otp/erlang/Link.class \
            com/ericsson/otp/erlang/Links.class \
            com/ericsson/otp/erlang/OtpAuthException.class \
            com/ericsson/otp/erlang/OtpConnection.class \
            com/ericsson/otp/erlang/OtpCookedConnection.class \
            com/ericsson/otp/erlang/OtpEpmd.class \
            com/ericsson/otp/erlang/OtpErlangAtom.class \
            com/ericsson/otp/erlang/OtpErlangBinary.class \
            com/ericsson/otp/erlang/OtpErlangBitstr.class \
            com/ericsson/otp/erlang/OtpErlangBoolean.class \
            com/ericsson/otp/erlang/OtpErlangByte.class \
            com/ericsson/otp/erlang/OtpErlangChar.class \
            com/ericsson/otp/erlang/OtpErlangDecodeException.class \
            com/ericsson/otp/erlang/OtpErlangDouble.class \
            com/ericsson/otp/erlang/OtpErlangException.class \
            com/ericsson/otp/erlang/OtpErlangExit.class \
            com/ericsson/otp/erlang/OtpErlangExternalFun.class \
            com/ericsson/otp/erlang/OtpErlangFloat.class \
            com/ericsson/otp/erlang/OtpErlangFun.class \
            com/ericsson/otp/erlang/OtpErlangInt.class \
            com/ericsson/otp/erlang/OtpErlangList.class \
            com/ericsson/otp/erlang/OtpErlangLong.class \
            com/ericsson/otp/erlang/OtpErlangMap.class \
            com/ericsson/otp/erlang/OtpErlangObject.class \
            com/ericsson/otp/erlang/OtpErlangPid.class \
            com/ericsson/otp/erlang/OtpErlangPort.class \
            com/ericsson/otp/erlang/OtpErlangRangeException.class \
            com/ericsson/otp/erlang/OtpErlangRef.class \
            com/ericsson/otp/erlang/OtpErlangShort.class \
            com/ericsson/otp/erlang/OtpErlangString.class \
            com/ericsson/otp/erlang/OtpErlangTuple.class \
            com/ericsson/otp/erlang/OtpErlangUInt.class \
            com/ericsson/otp/erlang/OtpErlangUShort.class \
            com/ericsson/otp/erlang/OtpException.class \
            com/ericsson/otp/erlang/OtpExternal.class \
            com/ericsson/otp/erlang/OtpInputStream.class \
            com/ericsson/otp/erlang/OtpLocalNode.class \
            com/ericsson/otp/erlang/OtpMbox.class \
            com/ericsson/otp/erlang/OtpMD5.class \
            com/ericsson/otp/erlang/OtpMsg.class \
            com/ericsson/otp/erlang/OtpNode.class \
            com/ericsson/otp/erlang/OtpNodeStatus.class \
            com/ericsson/otp/erlang/OtpOutputStream.class \
            com/ericsson/otp/erlang/OtpPeer.class \
            com/ericsson/otp/erlang/OtpSelf.class \
            com/ericsson/otp/erlang/OtpServer.class \
            com/ericsson/otp/erlang/OtpServerSocketTransport.class \
            com/ericsson/otp/erlang/OtpServerTransport.class \
            com/ericsson/otp/erlang/OtpSocketTransportFactory.class \
            com/ericsson/otp/erlang/OtpSocketTransport.class \
            com/ericsson/otp/erlang/OtpSystem.class \
            com/ericsson/otp/erlang/OtpTransportFactory.class \
            com/ericsson/otp/erlang/OtpTransport.class
	$(JAR) cvfm cloudi.jar $(srcdir)/manifest.txt com org

all-local: cloudi.jar

clean-local:
	rm -rf org com

install-exec-hook: cloudi.jar
	$(MKDIR_P) $(instdir)
	$(INSTALL_DATA) cloudi.jar $(instdir)

