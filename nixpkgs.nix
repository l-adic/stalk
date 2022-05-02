/*
NIXPKGS version
Any archive of nixpkgs can be used.
The simplest update solution is to look at
http://github.com/NixOS/nixpkgs and pick the latest commit for
the desired branch. The archive can then be fetched at:
https://github.com/NixOS/nixpkgs/archive/COMMIT_NUMBER.tar.gz;
and the control sum computed using `sha256`.
*/

let
  # release-21.11
  sha256 = "0wh9nq1z7z25g8h1nhf0mfgsy4xkp23b8b61058kpkpvi3yr0fcm";
  rev = "ac1191a866bf974d7e246cc7bb0b9c7e80d7cd7a";
in
import (fetchTarball {
  inherit sha256;
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
})
