{ pkgs
, ... }:
let

  ejabberdInit = pkgs.runCommand "pg.new.sql-src" {} ''
    tar xf "${pkgs.ejabberd.src}" ejabberd-*/sql/pg.new.sql
    cp ejabberd-*/sql/pg.new.sql $out
  '';
  initSql = pkgs.writeText "init.sql" ''
    CREATE USER "test-ejabberd" WITH SUPERUSER PASSWORD 'safe';
    CREATE DATABASE ejabberd OWNER "test-ejabberd";

    \c ejabberd riskbook-ejabberd
    \i ${ejabberdInit}
  '';
in

{
    virtualisation = {
      diskSize = 2048;
      writableStoreUseTmpfs = true;
      graphics = false;
      memorySize = 4096;
    } ;

    environment = {
        etc."ejabberd.yml" = {
            user = "ejabberd";
            mode = "0600";
            text = builtins.readFile ./ejabberd.yml;
        };
    };

    services = {
        ejabberd = {
            enable = true;
            configFile = "/etc/ejabberd.yml";
            package = pkgs.ejabberd.override {
                withPgsql = true;
            };
        };
        # we use postgres to get DIGEST-MD5 auth method.
        # It'd be better if we could drop postgres for CI tests.
        # (less complexity).
        postgresql = {
            enable = true;
            port = 5432;
            enableTCPIP = true;
            initialScript = initSql;
            package = pkgs.postgresql_9_6;
            authentication = pkgs.lib.mkOverride 10 ''
            host all all 127.0.0.1/8 trust
            local all all trust
            host ejabberd test-ejabberd ::1/128 trust
            '';
        };
    };
    systemd.services.ejabberd.after = [ "postgresql.service" ];
    systemd.services.postgresql = {
        requiredBy = [ "ejabberd.service" ];
    };
}
