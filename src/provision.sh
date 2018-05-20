titles=("ratings" "principals" "episode" "crew" "basics" "akas")

fetch_datasets() {
  for i in "${titles[@]}"
  do
    wget -q https://datasets.imdbws.com/title."$i".tsv.gz && \
    gunzip title."$i".tsv.gz && \
    tsv-to-json2 title."$i".tsv > title."$i".json || exit 1
  done
}


mongo_import() {
  sleep 20 # wait for mongodb to become ready
  for i in "${titles[@]}"
  do
    mongoimport --host=mongo --db local --collection title."$i" --type json --file title."$i".json --jsonArray || exit 1
  done
}

"$@"
