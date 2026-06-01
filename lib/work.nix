{ pkgs, unstablePkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    biome
    k9s
    kubectl
    kubectx
    nodejs_22
    pnpm
    typescript
    typescript-language-server
    postgresql
  ]) ++ (with unstablePkgs; [
    lieer
    notmuch
    google-cloud-sql-proxy
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud_sql_proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
  ]);

}
