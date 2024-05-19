TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

for file in ./good/*.fox ./bad/*.fox
do
    echo $file
    ./interpreter $file > $TMP_OUT 2> $TMP_ERR
    actual_exit_code=$?

    if diff ${file%fox}out $TMP_OUT>/dev/null
    then echo -e "OK";
    else echo -e "ERROR IN OUTPUT"
    fi

    if diff ${file%fox}err $TMP_ERR>/dev/null
    then echo -e "OK";
    else echo -e "ERROR IN ERROR"
    fi

    exit_file="${file%.fox}.exit"
    expected_exit_code=$(cat "$exit_file")
    if [ "$actual_exit_code" -eq "$expected_exit_code" ]; then
        echo -e "OK"
    else
        echo -e "ERROR IN EXIT"
    fi
done
