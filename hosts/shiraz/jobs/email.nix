{ pkgs, lib, ... }: {
  systemd.user.timers.email = {
    Unit.description = "timer for syncing emails";
    Install.WantedBy = [ "timers.target" ];
    Timer = {
      OnBootSec = "5m";
      OnUnitActiveSec = "5m";
      Unit = "email.service";
    };
  };
  systemd.user.services.email = {
    Unit.description = "email sync service";
    Install.WantedBy = [ "default.target" ];
    Service = {
      type = "oneshot";
      ExecStart = toString (
        pkgs.writeShellScript "email-sync-script " ''
          set -eu
          PATH=$PATH:${lib.makeBinPath [ pkgs.isync pkgs.pass  ]}
          ${pkgs.mutt-wizard}/bin/mailsync
        ''
      );
    };
  };
}
