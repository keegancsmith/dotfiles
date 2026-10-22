{ pkgs, unstablePkgs, ... }:

{
  environment.systemPackages = (with pkgs; [
    k9s
    kubectl
    kubectx
    nodejs_24
    pnpm
    typescript
    postgresql
  ]) ++ (with unstablePkgs; [
    lieer
    notmuch
    google-cloud-sql-proxy
    (google-cloud-sdk.withExtraComponents [
      google-cloud-sdk.components.cloud-sql-proxy
      google-cloud-sdk.components.gke-gcloud-auth-plugin
    ])
  ]);

}
