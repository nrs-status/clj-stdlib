{
  description = "A very basic flake";

  inputs = { 
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable"; 
    frontArmToPlane.url = "github:nrs-status/frontArmToPlane";
  };

  outputs = { self, nixpkgs, frontArmToPlane }:
    let pkgs = import nixpkgs { system = "x86_64-linux"; };
    in {

      packages.x86_64-linux.default = self.packages.x86_64-linux.hello;
      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = (with pkgs; [
          leiningen
          clojure
          jdk23
          cljfmt
          babashka
          clj-kondo
        ]) ++ [ frontArmToPlane.packages.x86_64-linux.wUnstable.montezuma_circles_scroll.nixvim.base ];
      };

    };
}
