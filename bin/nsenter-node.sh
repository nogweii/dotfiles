#!/bin/bash

node="${1}"

nodeName="$(kubectl get node "${node}" -o template --template='{{index .metadata.labels "kubernetes.io/hostname"}}')"
nodeSelector='"nodeSelector": { "kubernetes.io/hostname": "'${nodeName:?}'" },'

read -r -d '' overrides <<EOJSON
{
  "spec": {
    "hostPID": true,
    "hostNetwork": true,
    ${nodeSelector?}
    "tolerations": [{
        "operator": "Exists"
    }],
    "containers": [
      {
        "name": "nsenter",
        "image": "alexeiled/nsenter:2.34",
        "command": [
          "/nsenter", "--all", "--target=1", "--", "su", "-"
        ],
        "stdin": true,
        "tty": true,
        "securityContext": {
          "privileged": true
        }
      }
    ]
  }
}
EOJSON


podName=${USER}-nsenter-${node%%.*}
kubectl run ${podName:?} --restart=Never -it --rm --image overridden --overrides "${overrides}" --attach "$@"
