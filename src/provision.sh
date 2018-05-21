titles=("ratings" "episode" "principals"  "crew" "basics" "akas")

sleep 10 # wait for mongodb to become ready
mongo local --host mongo --eval "printjson(db.serverStatus())"
mongo local --host mongo --eval "db.dropDatabase()"
for i in "${titles[@]}"
do
  mongoimport --host=mongo --db local --collection title."$i" --type json --file title."$i".json --jsonArray || exit 1
  mongo local --host mongo --eval "db.getCollection('title."$i"').createIndex({tconst: 1})"
done
