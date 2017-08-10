#-*-Mode:make;coding:utf-8;tab-width:4;c-basic-offset:4-*-
# ex: set ft=make fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:

jardir = $(top_builddir)/api/java/jar
JAVACFLAGS = -Xlint
FILES = $(srcdir)/AbstractConnection.java \
        $(srcdir)/AbstractNode.java \
        $(srcdir)/GenericQueue.java \
        $(srcdir)/Link.java \
        $(srcdir)/Links.java \
        $(srcdir)/OtpAuthException.java \
        $(srcdir)/OtpConnection.java \
        $(srcdir)/OtpCookedConnection.java \
        $(srcdir)/OtpEpmd.java \
        $(srcdir)/OtpErlangAtom.java \
        $(srcdir)/OtpErlangBinary.java \
        $(srcdir)/OtpErlangBitstr.java \
        $(srcdir)/OtpErlangBoolean.java \
        $(srcdir)/OtpErlangByte.java \
        $(srcdir)/OtpErlangChar.java \
        $(srcdir)/OtpErlangDecodeException.java \
        $(srcdir)/OtpErlangDouble.java \
        $(srcdir)/OtpErlangException.java \
        $(srcdir)/OtpErlangExit.java \
        $(srcdir)/OtpErlangExternalFun.java \
        $(srcdir)/OtpErlangFloat.java \
        $(srcdir)/OtpErlangFun.java \
        $(srcdir)/OtpErlangInt.java \
        $(srcdir)/OtpErlangList.java \
        $(srcdir)/OtpErlangLong.java \
        $(srcdir)/OtpErlangMap.java \
        $(srcdir)/OtpErlangObject.java \
        $(srcdir)/OtpErlangPid.java \
        $(srcdir)/OtpErlangPort.java \
        $(srcdir)/OtpErlangRangeException.java \
        $(srcdir)/OtpErlangRef.java \
        $(srcdir)/OtpErlangShort.java \
        $(srcdir)/OtpErlangString.java \
        $(srcdir)/OtpErlangTuple.java \
        $(srcdir)/OtpErlangUInt.java \
        $(srcdir)/OtpErlangUShort.java \
        $(srcdir)/OtpException.java \
        $(srcdir)/OtpExternal.java \
        $(srcdir)/OtpInputStream.java \
        $(srcdir)/OtpLocalNode.java \
        $(srcdir)/OtpMbox.java \
        $(srcdir)/OtpMD5.java \
        $(srcdir)/OtpMsg.java \
        $(srcdir)/OtpNode.java \
        $(srcdir)/OtpNodeStatus.java \
        $(srcdir)/OtpOutputStream.java \
        $(srcdir)/OtpPeer.java \
        $(srcdir)/OtpSelf.java \
        $(srcdir)/OtpServer.java \
        $(srcdir)/OtpServerSocketTransport.java \
        $(srcdir)/OtpServerTransport.java \
        $(srcdir)/OtpSocketTransportFactory.java \
        $(srcdir)/OtpSocketTransport.java \
        $(srcdir)/OtpSystem.java \
        $(srcdir)/OtpTransportFactory.java \
        $(srcdir)/OtpTransport.java

all-local: $(FILES)
	$(JAVAC) $(JAVACFLAGS) -d $(jardir) $(FILES)
