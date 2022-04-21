#!/bin/bash

context=$1
container=$2
class_file=$3
class_file_name=$(basename "$class_file")
bare_class_name=${class_file_name%.*}

docker --context="$context" exec -i "$container" sh -c 'mkdir -p /tmp/ascreloader-cache; cat > /tmp/ascreloader-cache/'"$class_file_name" < "$class_file"
class_loader=$(docker --context="$context" exec "$container" sh -c "as.sh -c 'sc -d *${bare_class_name}' 1 | sed 's/\\r/\\n/' | grep classLoaderHash | head -1")
class_loader=$(echo "$class_loader" | head -1 | gawk '{print $2}')

docker --context="$context" exec "$container" sh -c "as.sh -c 'redefine -c ${class_loader} /tmp/ascreloader-cache/$class_file_name' 1" | sed 's/\r/\n/'
