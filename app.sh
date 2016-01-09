#!/usr/bin/env bash

command=$1

id=`docker-compose ps -q app`
ip=`docker inspect --format '{{ .NetworkSettings.IPAddress }}' ${id}`

case "$command" in
    "logfile")
        dockerStorage=$(docker info 2>/dev/null | grep "Root Dir:" | sed "s/.*: \(.*\)/\1/g")
        container=$1
        echo ${dockerStorage}/diff/${id}/bin/soccer-rest/log/app.log
    ;;
    "log") less `./app.sh logfile ${1}`
    ;;
    "initdb")
        dbid=`docker-compose ps -q db`
        docker exec -i -t $dbid sh -c "mongoimport --db soccer-rest --collection leagueInfo --drop --file /root/soccer-rest/data/leagueInfo.out.json"
        docker exec -i -t $dbid sh -c "mongoimport --db soccer-rest --collection teamInfo --drop --file /root/soccer-rest/data/teamInfo.out.json"
    ;;
esac