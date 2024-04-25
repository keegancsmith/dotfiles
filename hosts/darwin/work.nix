{ pkgs, nixpkgs, nixpkgs-unstable, ... }:

{
  environment.systemPackages = (with pkgs; [
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
    k9s
    kubectl
    kubectx
    nodejs_20
    nodejs_20.pkgs.typescript
    nodejs_20.pkgs.typescript-language-server

    myNodePackages.pnpm
  ]) ++ (with nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
    lieer
    notmuch
  ]);

  homebrew.enable = true;
  homebrew.brews = [
    "bazelisk"
    "figma"
    "notion"
  ];
  homebrew.masApps = {
    "1password" = 1333542190;
    "golinks" = 1478821913;
    "okta verify" = 490179405;
  };
}
