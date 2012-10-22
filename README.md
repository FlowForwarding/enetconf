# NETCONF for Erlang

**enetconf** is a NETCONF 1.0/1.1 server implemented in Erlang.

# Caution!

**The code you find here is in a very early stage of development**
**and should not be used anywhere. It's being developed to enable**
**support for OF-Config in [LINC][linc] - OpenFlow 1.2 software switch.**
**Please refer to the list below for the unimplemented features.**

## Unsupported features

Current implementation doesn't support the following features of NETCONF:

 * Transport protocols other then SSH: TLS, BEEP and SOAP,
 * The `commit`, `discard-changes`, `cancel-commit` and `validate` operations,
 * The `kill-session` operation,
 * Proper support for capabilities,
 * Additional informations in the `<rpc-error>` response: `error-app-tag`,
   `error-path` and `error-message`,
 * Also note that the included parser does not handle `rpc-reply` and
   `rpc-error` responses as those are not needed on the server side,
 * Other things.

[linc]:
https://github.com/FlowForwarding/LINC-Switch
