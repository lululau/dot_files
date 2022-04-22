#!/bin/bash

context=$1
namespace=$2
deployment=$3
class_file=$4
class_file_name=$(basename "$class_file")
class_name=${class_file#*/target/classes/}
class_name=${class_name%.class}
class_name=${class_name//\//.}

if ! kubectl --context="$context" -n "$namespace" exec deploy/"$deployment" -- sh -c 'type as.sh &> /dev/null'; then
  echo "Arthas not installed in deployment"
  exit 1
fi

class_loader=$(kubectl --context="$context" -n "$namespace" exec deploy/"$deployment" -- sh -c "as.sh -c 'sc -d ${class_name}' 1 | sed 's/\\r/\\n/' | grep classLoaderHash | head -1")
class_loader=$(echo "$class_loader" | head -1 | gawk '{print $2}')
if [ -n "$class_loader" ]; then
  pod_selector=$(kubectl get --raw "/apis/apps/v1/namespaces/${namespace}/deployments/${deployment}/scale" | jq -r .status.selector)
  pods=($(kubectl --context="$context" -n "$namespace" get pods -l "$pod_selector" -o jsonpath='{.items[*].metadata.name}'))
  for pod in "${pods[@]}"; do
    kubectl --context="$context" -n "$namespace" exec "$pod" -i -- sh -c 'mkdir -p /tmp/ascreloader-cache; cat > /tmp/ascreloader-cache/'"$class_file_name" < "$class_file"
    kubectl --context="$context" -n "$namespace" exec "$pod" -- sh -c "as.sh -c 'redefine -c ${class_loader} /tmp/ascreloader-cache/$class_file_name' 1" | sed 's/\r/\n/'
  done
else
  echo "No class loader found for $class_file"
fi

