[![Maintenance badge](https://img.shields.io/maintenance/yes/2020)](https://riskbook.com/)

**Maintainer wanted**
Supercede no longer uses haskell-xmpp.
If you're using this library, please volunteer to take over maintainership.

![Logo](https://raw.githubusercontent.com/riskbook/haskell-xmpp/master/haskell-logo-xmpp.svg)


Fully functional haskell-xmpp bindings used in production
at riskbook.
This integrates well with ejabbered for example.

Haskell XMPP (eXtensible Message Passing Protocol, a.k.a. Jabber) library.
Unlike package network-protocol-xmpp, which uses libxml-sax, this library uses HaXml and supports MUC.
However, MUC support of the moment is worse than that in package XMPP.
This library make extensive use of STM and threads to simplify writing message-handling code.

As of version 2 riskbook maintains this.
We patched the orignal work by dmitry with these fixes:
- [x] Duplicate Show instances
- [x] Missing FlexibleContexts
- [x] GADTs for Stanzas

## Contributing
Feel free to submit any PR or issue.

Questions can be send to: support@riskbook.com

## License
BSD3

## Special thanks
A special thanks to Dmitry Astapov for making the original release.

### Logo sources
+ https://commons.wikimedia.org/wiki/File:XMPP_logo.svg
+ https://commons.wikimedia.org/wiki/File:Haskell-Logo.svg
