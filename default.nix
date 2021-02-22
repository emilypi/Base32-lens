{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, base32, bytestring, lens, lib, text
      , text-short
      }:
      mkDerivation {
        pname = "base32-lens";
        version = "0.1.1.0";
        src = ./.;
        libraryHaskellDepends = [
          base base32 bytestring lens text text-short
        ];
        homepage = "https://github.com/emilypi/base32-lens";
        description = "Optics for the Base32 library";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
