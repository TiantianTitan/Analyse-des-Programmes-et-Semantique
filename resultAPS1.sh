for file in ./APS1/MonSample/*.aps;
do
    echo "******************************************"
    echo $file ": " 
    type=$(./APS1/prologTerm $file | swipl -s ./APS1/prog.pl -g main 2>&1)
    if [[ $type = *"void"* ]]; then
        echo "Type Checking OK."
    else
        echo "Type Checking error!"
    fi
    res=$(./APS1/eval $file)
    if [[ $res = "42" ]]; then
        echo "Eval Checking OK."    
    else
        echo "Eval Checking error! Result: $res " 
    fi
done