{ pkgs, nixpkgs, nixpkgs-unstable, ... }:

{
  environment.systemPackages = (with pkgs; [
    k9s
    kubectl
    kubectx
    nodejs_20
    nodejs_20.pkgs.pnpm
    nodejs_20.pkgs.typescript
    nodejs_20.pkgs.typescript-language-server
    postgresql

    (pkgs.writeScriptBin "ibazel" ''
      #!${pkgs.stdenv.shell}
      exec bazelisk "$@"
    '')
  ]) ++ (with nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
    lieer
    notmuch
    google-cloud-sql-proxy
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
  ]);

  homebrew.enable = true;
  homebrew.brews = [
    "bazelisk"
  ];
  homebrew.casks = [
    "dbeaver-community"
    "figma"
    "linear-linear"
    "mitmproxy"
    "notion"
  ];
  homebrew.masApps = {
    "1password" = 1333542190;
    "golinks" = 1478821913;
    "okta verify" = 490179405;
  };
}
