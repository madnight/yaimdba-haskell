titles=("ratings" "principals" "episode" "crew" "basics" "akas")

sleep 20 # wait for mongodb to become ready
for i in "${titles[@]}"
do
  mongo local --eval "printjson(db.serverStatus())"
  mongoimport --host=mongo --db local --upsertFields tconst --collection title."$i" --type json --file title."$i".json --jsonArray || exit 1
  mongo local --eval "db.getCollection('title."$i"').createIndex({tconst: 1})"
done
