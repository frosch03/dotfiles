{
  description = "frosch03 nixos configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = { self, nixpkgs }: {
    nixosConfigurations.v2309.modules = [ ./configuration.nix ];
  };
}
