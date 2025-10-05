# According to: https://www.reddit.com/r/NixOS/comments/oep4zb/how_to_set_filesystems_in_a_function/

# Note: Passwords are configured within /etc/nixos/smb-secrets
let
  mounts = [
    { local = "data"; remote = "data"; }
    { local = "Pictures"; remote = "Pictures"; }
    { local = "Tausch"; remote = "tausch"; }
    { local = "Home"; remote = "homes/frosch03"; }
    { local = "Container"; remote = "Container"; }
  ];
  mount = { local, remote }: {
    fileSystems."/mnt/frogNAS/${local}" = {
      device = "//192.168.178.22/${remote}";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts},user,_netdev,uid=1000,gid=100,credentials=/etc/nixos/smb-secrets"];
    };
  };
  box-mounts = [
    { local = "data"; remote = "data"; }
    { local = "share"; remote = "share"; }
    { local = "home"; remote = "frosch03"; }
  ];
  box-mount = { local, remote }: {
    fileSystems."/mnt/the-box/${local}" = {
      device = "//192.168.178.181/${remote}";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts},user,_netdev,dir_mode=0755,file_mode=0644,uid=1000,gid=100,credentials=/etc/nixos/smb-box-secrets"];
    };
  };
  
in {
  imports = (map mount mounts) ++ (map box-mount box-mounts); 
}   
