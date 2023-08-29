{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation rec {
  name = "dotfrog";

  buildInputs = [];

  src = ./.;

  installPhase = with pkgs; ''
    echo ">>> installing dotfrog <<<"
    echo "From: $src Into: $out"

    mkdir -p $out/dotfrog $out/dotfrog/bin

    echo "copying executables"
    find $src/bin/ -not -iname "*~" -type f -exec cp "{}" $out/dotfrog/bin/ \;

    substituteInPlace $out/dotfrog/bin/installDot.sh \
      --replace "~/localStorage/dotfiles" $src/ \
      --replace test success

    echo "copying dotfiles"
    find $src/ -maxdepth 1 -iname ".*" -type f -exec cp "{}" $out/dotfrog \;

    echo "copying dotfolders"
    find $src/ -maxdepth 1 -iname ".*" -not -iname ".git" -not -iname "." -type d -exec cp -r "{}" $out/dotfrog \;

    echo "create symlinks by running $out/dotfrog/bin/installDot.sh"
    # $out/dotfrog/bin/installDot.sh

    echo "done..."
  '';
}

