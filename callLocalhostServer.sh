curl -X GET http://localhost:8081/tokipona/informu

echo ""

curl -X POST -H "Content-Type: application/json" \
    --data '{ "tekstoj" : ["mi olin e jan pona sina"] }' http://localhost:8081/tokipona/traduku

echo ""

curl -X POST -H "Content-Type: application/json" \
    --data '{
        "tekstoj" : ["mi olin e sina", "mi wile moku e kasi kule"]
}' http://localhost:8081/tokipona/traduku?lingvo=fro 

echo ""
