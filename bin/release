#!/usr/bin/env bash
set -eou pipefail

update_charts() {
  RELEASE_VERSION=$(echo "${GITHUB_REF}" | sed -e "s/refs\/tags\///g")
  echo "Release: ${RELEASE_VERSION}"
  sed -e "s/9.9.9/${RELEASE_VERSION}/g" < charts/arbeitsinspektor/Chart.yaml > charts/arbeitsinspektor/Chart.new.yaml
  mv charts/arbeitsinspektor/Chart.new.yaml charts/arbeitsinspektor/Chart.yaml
  echo "Charts updated"
}

helm_prepare() {
  cd charts
  helm lint arbeitsinspektor/
  helm package arbeitsinspektor/
}

update_charts
helm_prepare
