{
  description = "A Nix-flake-based Scala development environment";

  # Mirrored unstable nixpkgs, but from FlakeHub.
  inputs.nixpkgs.url = "https://flakehub.com/f/NixOS/nixpkgs/0.1.*.tar.gz";

  outputs = { self, nixpkgs }:
    let
      javaVersion = 11;
      overlays = [
        (final: prev: rec {
          jdk = prev."jdk${toString javaVersion}";
          sbt = prev.sbt.override { jre = jdk; };
          scala = prev.scala.override { jre = jdk; };

          # For Docusaurus site.
          nodejs = prev.nodejs_latest;
          pnpm = prev.nodePackages.pnpm;
          yarn = (prev.yarn.override { inherit nodejs; });
        })
      ];
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forEachSupportedSystem = f: nixpkgs.lib.genAttrs supportedSystems (system: f {
        pkgs = import nixpkgs { inherit overlays system; };
      });
    in
    {
      devShells = forEachSupportedSystem ({ pkgs }: {
        default = pkgs.mkShell {
          packages = with pkgs; [ jdk scala sbt coursier scalafmt scalafix node2nix nodejs pnpm yarn ];
        };
      });
    };
}
