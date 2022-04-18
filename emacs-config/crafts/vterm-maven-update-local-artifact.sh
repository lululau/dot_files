#!/usr/bin/env zsh

if ! type fzf > /dev/null
then
  echo "fzf is required, see installation instructions for fzf: https://github.com/junegunn/fzf#using-homebrew-or-linuxbrew"
  exit 1
fi

repo_root=$HOME/.m2/repository

artifacts=$(find "$repo_root" -name '*.pom' -type f | perl -ne "
     s#$repo_root/##;
     my @fs = split '/';
     my \$group_id = join '.', @{fs[0..@fs-4]};
     my \$artifact_id = \${fs[@fs-3]};
     my \$version = \${fs[@fs-2]};
     next unless \${fs[@fs-1]} eq ( \$artifact_id . '-' . \$version . \".pom\n\");
     printf \"%s:%s:%s\n\", \$group_id, \$artifact_id, \$version;
" | fzf)

for artifact (${(f)artifacts}); do
  echo "Artifact: $artifact\n"
  if [[ "$artifact" =~ '.+:.+:.+' ]]; then

    group_id=${artifact%%:*}
    artifact_id=${${artifact%:*}#*:}
    version=${artifact##*:}
    dir=${group_id//./\/}/$artifact_id/$version

    echo rm -rf $repo_root/"$dir"
    rm -rf $repo_root/"$dir"
    echo mvn dependency:get -Dartifact=$artifact -Dtransitive=true
    mvn dependency:get -Dartifact=$artifact -Dtransitive=true
    echo
  fi
done
