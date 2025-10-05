{
  description = "frosch03 nixos configuration";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    unstablepkgs.url = "nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs, unstablepkgs }: {
    nixosConfigurations.v2309 = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ 
        ./configuration.nix
        { _module.args = {
            unstable = import unstablepkgs {
              system = "x86_64-linux";
              config.allowUnfree = true;
            };
          };
        }
      ];
    };
  };
}
