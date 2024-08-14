for file in ./MonSample/*.aps
do
    echo "******************************************"
    echo $file ": " 
    type=$(./prologTerm $file | swipl -s prog.pl -g main 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "Type Checking OK."
    else
        echo "Type Checking error!"
    fi
    # res=$(./eval $file)
    # if [[ $res = "42" ]]; then
    #     echo "Eval Checking OK."    
    # else
    #     echo "Eval Checking error! Result: $res " 
    # fi
done