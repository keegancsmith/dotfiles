{ pkgs, nixpkgs, nixpkgs-unstable, ... }:

{
  environment.systemPackages = (with pkgs; [
    biome
    k9s
    kubectl
    kubectx
    nodejs_22
    nodejs_22.pkgs.pnpm
    nodejs_22.pkgs.typescript
    nodejs_22.pkgs.typescript-language-server
    postgresql
  ]) ++ (with nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}; [
    lieer
    notmuch
    google-cloud-sql-proxy
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
  ]);

}
