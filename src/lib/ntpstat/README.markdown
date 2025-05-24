# `ntpstat`: Erlang ntpstat Client Interface

A minimal ntpstat client interface for real-time use.
The original ntpstat executable was created by G. Richard Keech and
relies on a NTP mode 6 control message
(operation code 2, does not require authentication).

Using a NTP mode 6 control message for NTP monitoring is an
efficient low-latency approach for getting NTP data.
The NTP mode 6 control messages are not meant to be exposed to the Internet
and are instead isolated to a secure LAN or the localhost interface.
By default, the ntpstat source code uses 127.0.0.1 (localhost) for all
NTP mode 6 control message requests.

## Supported NTP Servers

While NTP mode 6 control message use is the best standard approach for
monitoring any NTP server, some NTP servers have not been able to implement
mode 6 control message support.
The NTP servers that lack mode 6 control messages are not implementing it
due to security concerns and not being required by NTP v4.
The alternative would be brittle slow parsing of shell command output
that is specific to each NTP server implementation
(an approach that will not be pursued in this repository).

### Supported

* [ntp.org ntpd](https://ntp.org)
* [ntpsec.org ntpd](https://ntpsec.org)

### Unsupported

* [chrony](https://chrony-project.org)
* [OpenNTPD](https://openntpd.org)

### NTP Mode 6 Control Message Documentation

More information about NTP mode 6 control messages is available at:

* [RFC1305](https://tools.ietf.org/html/rfc1305) is the main source of information (in Appendix B)
* [RFC9327](https://tools.ietf.org/html/rfc9327) (referenced from [RFC8633](https://tools.ietf.org/html/rfc8633)) has historical information but also contains errors
* [https://docs.ntpsec.org/latest/mode6.html](https://docs.ntpsec.org/latest/mode6.html) has important information not available elsewhere

## Build

    rebar compile

## Author

Michael Truog (mjtruog at protonmail dot com)

## License

MIT License

