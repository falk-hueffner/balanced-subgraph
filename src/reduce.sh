rm -f bug.graph
for i in test/*.graph; do
  ./delta.sh < $i;
  if [ ! -e bug.graph ] || ([ -e min.graph ] && [ $(wc -l min.graph) -lt $(wc -l bug.graph) ]); then
    cp min.graph bug.graph
  fi
done
