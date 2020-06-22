{
  nix.binaryCaches = [
    "https://nixcache.reflex-frp.org"
    "https://cache.dhall-lang.org"
    "https://dhall.cachix.org"
    "https://bs-platform.cachix.org"
  ];
  nix.binaryCachePublicKeys = [
    "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI="
    "cache.dhall-lang.org:I9/H18WHd60olG5GsIjolp7CtepSgJmM2CsO813VTmM="
    "dhall.cachix.org-1:8laGciue2JBwD49ICFtg+cIF8ddDaW7OFBjDb/dHEAo="
  ];
}

